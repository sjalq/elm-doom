module GoldenTests exposing (suite)

{-| The pure geometry gives exactly what linuxdoom-1.10's C code gives, on the inputs in
`GoldenVectors` (full-range int32s included, where C's arithmetic wraps).
-}

import Doom.Geometry as Geometry
import Doom.Level as Level
import Doom.Play.MapUtil as MapUtil
import Expect
import GoldenVectors
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "matches the C source"
        [ golden "R_PointToAngle2" GoldenVectors.angles (two (\x y -> Geometry.pointToAngle2 0 0 x y))
        , golden "P_AproxDistance" GoldenVectors.distances (two MapUtil.aproxDistance)
        , golden "P_PointOnLineSide"
            GoldenVectors.lineSides
            (six (\x y lx ly dx dy -> MapUtil.pointOnLineSide x y (line lx ly dx dy)))
        , golden "P_PointOnDivlineSide"
            GoldenVectors.divlineSides
            (six (\x y lx ly dx dy -> MapUtil.pointOnDivlineSide x y { x = lx, y = ly, dx = dx, dy = dy }))
        , golden "R_PointOnSide"
            GoldenVectors.nodeSides
            (six (\x y lx ly dx dy -> Geometry.pointOnSide x y (node lx ly dx dy)))
        , golden "P_InterceptVector"
            GoldenVectors.intercepts
            (\input ->
                case input of
                    [ ax, ay, adx, ady, bx, by, bdx, bdy ] ->
                        Just (MapUtil.interceptVector { x = ax, y = ay, dx = adx, dy = ady } { x = bx, y = by, dx = bdx, dy = bdy })

                    _ ->
                        Nothing
            )
        ]


two : (Int -> Int -> Int) -> List Int -> Maybe Int
two f input =
    case input of
        [ a, b ] ->
            Just (f a b)

        _ ->
            Nothing


six : (Int -> Int -> Int -> Int -> Int -> Int -> Int) -> List Int -> Maybe Int
six f input =
    case input of
        [ a, b, c, d, e, g ] ->
            Just (f a b c d e g)

        _ ->
            Nothing


golden : String -> List ( List Int, Int ) -> (List Int -> Maybe Int) -> Test
golden name vectors f =
    test name <|
        \_ ->
            vectors
                |> List.filterMap
                    (\( input, expected ) ->
                        let
                            actual =
                                f input
                        in
                        if actual == Just expected then
                            Nothing

                        else
                            Just ( input, expected, actual )
                    )
                |> List.take 5
                |> Expect.equal []


line : Int -> Int -> Int -> Int -> Level.Line
line x y dx dy =
    { v1 = { x = x, y = y }
    , v2 = { x = x + dx, y = y + dy }
    , dx = dx
    , dy = dy
    , flags = 0
    , tag = 0
    , frontSide = 0
    , backSide = Nothing
    , bbox = { top = 0, bottom = 0, left = 0, right = 0 }
    , slopetype = Level.Positive
    , frontsector = 0
    , backsector = Nothing
    }


node : Int -> Int -> Int -> Int -> Level.Node
node x y dx dy =
    { x = x
    , y = y
    , dx = dx
    , dy = dy
    , bbox = ( emptyBox, emptyBox )
    , children = ( Level.ChildSubsector 0, Level.ChildSubsector 0 )
    }


emptyBox : Level.Box
emptyBox =
    { top = 0, bottom = 0, left = 0, right = 0 }
