module PhysicsTests exposing (suite)

{-| Properties of the pure geometry the physics is built on (r\_main.c, p\_maputl.c).
-}

import Doom.Angle as Angle
import Doom.Fixed as Fixed
import Doom.Geometry as Geometry
import Doom.Int32 as Int32
import Doom.Level as Level
import Doom.Play.MapUtil as MapUtil
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "physics geometry"
        [ aproxDistanceTests
        , sideTests
        , angleTests
        , interceptTests
        ]



-- FUZZERS


{-| Any int32.
-}
int32 : Fuzzer Int
int32 =
    Fuzz.intRange Fixed.minInt Fixed.maxInt


{-| A map coordinate: up to 4096 units either way, fixed-point.
-}
coord : Fuzzer Int
coord =
    Fuzz.intRange (-4096 * Fixed.fracunit) (4096 * Fixed.fracunit)


{-| A whole number of map units up to 2048, fixed-point (map lines are made of these).
-}
unitLength : Fuzzer Int
unitLength =
    Fuzz.map (\n -> n * Fixed.fracunit) (Fuzz.intRange -2048 2048)


type alias Case =
    { x1 : Int, y1 : Int, dx : Int, dy : Int, px : Int, py : Int }


lineCase : Fuzzer Case
lineCase =
    Fuzz.map6 Case coord coord unitLength unitLength coord coord


{-| A linedef as P\_LoadLineDefs sets one up.
-}
makeLine : Int -> Int -> Int -> Int -> Level.Line
makeLine x1 y1 dx dy =
    let
        slopetype =
            if dx == 0 then
                Level.Vertical

            else if dy == 0 then
                Level.Horizontal

            else if Fixed.div dy dx > 0 then
                Level.Positive

            else
                Level.Negative

        x2 =
            x1 + dx

        y2 =
            y1 + dy
    in
    { v1 = { x = x1, y = y1 }
    , v2 = { x = x2, y = y2 }
    , dx = dx
    , dy = dy
    , flags = 0
    , tag = 0
    , frontSide = 0
    , backSide = Nothing
    , bbox = { top = max y1 y2, bottom = min y1 y2, left = min x1 x2, right = max x1 x2 }
    , slopetype = slopetype
    , frontsector = 0
    , backsector = Nothing
    }


{-| The cross product (line direction x offset to the point), in fixed-point squared,
as a float: negative on the front (right) side.
-}
cross : Case -> Float
cross c =
    toFloat c.dx * toFloat (c.py - c.y1) - toFloat c.dy * toFloat (c.px - c.x1)


{-| The side the exact cross product gives, when it is far enough from 0 for the
fixed-point rounding not to matter (Nothing otherwise).
-}
exactSide : Case -> Maybe Int
exactSide c =
    let
        k =
            cross c

        margin =
            -- the products are rounded to whole fixed-point units after scaling down
            -- by up to 2^16 (and 2^8 twice for divlines)
            toFloat (2 ^ 24) * toFloat Fixed.fracunit
    in
    if c.dx == 0 || c.dy == 0 then
        Nothing

    else if k < -margin then
        Just 0

    else if k > margin then
        Just 1

    else
        Nothing


expectSide : (Case -> Int) -> Case -> Expect.Expectation
expectSide side c =
    case exactSide c of
        Just s ->
            side c |> Expect.equal s

        Nothing ->
            Expect.pass



-- TESTS


aproxDistanceTests : Test
aproxDistanceTests =
    describe "aproxDistance"
        [ fuzz2 int32 int32 "is symmetric" <|
            \a b -> MapUtil.aproxDistance a b |> Expect.equal (MapUtil.aproxDistance b a)
        , fuzz2 int32 int32 "ignores signs" <|
            \a b ->
                MapUtil.aproxDistance (Int32.neg a) (Int32.neg b)
                    |> Expect.equal (MapUtil.aproxDistance a b)
        , fuzz2 coord coord "lies between the longer side and the sum of the sides" <|
            \a b ->
                let
                    d =
                        MapUtil.aproxDistance a b
                in
                Expect.all
                    [ \_ -> d |> Expect.atLeast (max (abs a) (abs b))
                    , \_ -> d |> Expect.atMost (abs a + abs b)
                    ]
                    ()
        , test "is exact on an axis" <|
            \_ -> MapUtil.aproxDistance (-300 * Fixed.fracunit) 0 |> Expect.equal (300 * Fixed.fracunit)
        ]


sideTests : Test
sideTests =
    describe "which side of a line"
        [ fuzz lineCase "pointOnLineSide agrees with the cross product's sign" <|
            expectSide (\c -> MapUtil.pointOnLineSide c.px c.py (makeLine c.x1 c.y1 c.dx c.dy))
        , fuzz lineCase "pointOnDivlineSide agrees with the cross product's sign" <|
            expectSide (\c -> MapUtil.pointOnDivlineSide c.px c.py { x = c.x1, y = c.y1, dx = c.dx, dy = c.dy })
        , fuzz lineCase "Geometry.pointOnSide agrees with the cross product's sign" <|
            expectSide
                (\c ->
                    Geometry.pointOnSide c.px
                        c.py
                        { x = c.x1, y = c.y1, dx = c.dx, dy = c.dy, bbox = ( emptyBox, emptyBox ), children = ( Level.ChildSubsector 0, Level.ChildSubsector 0 ) }
                )
        , fuzz lineCase "boxOnLineSide agrees with the corners of a small box around the point" <|
            \c ->
                let
                    line =
                        makeLine c.x1 c.y1 c.dx c.dy

                    side =
                        MapUtil.pointOnLineSide c.px c.py line

                    box =
                        { top = c.py + Fixed.fracunit, bottom = c.py - Fixed.fracunit, left = c.px - Fixed.fracunit, right = c.px + Fixed.fracunit }

                    corners =
                        [ ( box.left, box.top ), ( box.right, box.top ), ( box.left, box.bottom ), ( box.right, box.bottom ) ]
                            |> List.map (\( x, y ) -> MapUtil.pointOnLineSide x y line)
                in
                if line.slopetype /= Level.Positive && line.slopetype /= Level.Negative then
                    Expect.pass

                else if List.all ((==) side) corners then
                    MapUtil.boxOnLineSide box line |> Expect.equal side

                else
                    Expect.pass
        , test "a box across a line crosses it" <|
            \_ ->
                MapUtil.boxOnLineSide
                    { top = 10 * Fixed.fracunit, bottom = -10 * Fixed.fracunit, left = -10 * Fixed.fracunit, right = 10 * Fixed.fracunit }
                    (makeLine 0 0 (64 * Fixed.fracunit) (64 * Fixed.fracunit))
                    |> Expect.equal -1
        ]


angleTests : Test
angleTests =
    describe "pointToAngle2"
        [ fuzz2 (Fuzz.intRange -(2 ^ 28) (2 ^ 28)) (Fuzz.intRange -(2 ^ 28) (2 ^ 28)) "is close to atan2" <|
            \dx dy ->
                if max (abs dx) (abs dy) < 2 ^ 24 then
                    Expect.pass

                else
                    let
                        a =
                            Geometry.pointToAngle2 0 0 dx dy

                        expected =
                            atan2 (toFloat dy) (toFloat dx) / (2 * pi) * 4294967296

                        diff =
                            abs (toFloat a - expected)

                        circular =
                            min diff (4294967296 - diff)
                    in
                    circular |> Expect.atMost (2 ^ 19)
        , fuzz2 int32 int32 "is an unsigned angle" <|
            \x y ->
                Geometry.pointToAngle2 0 0 x y
                    |> Expect.all [ Expect.atLeast 0, Expect.atMost Angle.angMax ]
        , fuzz2 coord coord "only depends on the offset" <|
            \x y ->
                Geometry.pointToAngle2 x y (x + 100 * Fixed.fracunit) (y - 30 * Fixed.fracunit)
                    |> Expect.equal (Geometry.pointToAngle2 0 0 (100 * Fixed.fracunit) (-30 * Fixed.fracunit))
        , test "the axes" <|
            \_ ->
                [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]
                    |> List.map (\( x, y ) -> Geometry.pointToAngle2 0 0 (x * Fixed.fracunit) (y * Fixed.fracunit))
                    |> Expect.equal [ 0, Angle.ang90 - 1, Angle.ang180 - 1, Angle.ang270 ]
        , fuzz2 (Fuzz.intRange 1 4096) (Fuzz.intRange 1 4096) "pointToDist is close to the distance" <|
            \x y ->
                let
                    d =
                        Geometry.pointToDist 0 0 (x * Fixed.fracunit) (y * Fixed.fracunit)

                    exact =
                        sqrt (toFloat (x * x + y * y)) * toFloat Fixed.fracunit
                in
                abs (toFloat d - exact) / exact |> Expect.atMost 0.01
        ]


interceptTests : Test
interceptTests =
    describe "interceptVector"
        [ fuzz2 (Fuzz.intRange 1 2047) (Fuzz.intRange -1000 1000) "finds where a trace crosses a wall" <|
            \a y ->
                let
                    trace =
                        { x = 0, y = 0, dx = 2048 * Fixed.fracunit, dy = 0 }

                    wall =
                        { x = a * Fixed.fracunit, y = (y - 2000) * Fixed.fracunit, dx = 0, dy = 4000 * Fixed.fracunit }

                    expected =
                        toFloat a / 2048 * toFloat Fixed.fracunit
                in
                abs (toFloat (MapUtil.interceptVector trace wall) - expected) |> Expect.atMost 64
        , test "parallel lines give 0" <|
            \_ ->
                MapUtil.interceptVector { x = 0, y = 0, dx = Fixed.fracunit, dy = 0 } { x = 5, y = 7, dx = 3 * Fixed.fracunit, dy = 0 }
                    |> Expect.equal 0
        ]


emptyBox : Level.Box
emptyBox =
    { top = 0, bottom = 0, left = 0, right = 0 }
