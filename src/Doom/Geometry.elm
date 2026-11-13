module Doom.Geometry exposing (pointToAngle2, pointToDist, pointOnSide, pointInSubsector)

{-| The renderer's geometry that the game also uses (r\_main.c): angles between points,
which side of a BSP partition a point is on, and which subsector a point is in.

@docs pointToAngle2, pointToDist, pointOnSide, pointInSubsector

-}

import Array
import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Int32 as Int32
import Doom.Level as Level exposing (Level, Node, SubsectorId)
import Doom.Tables as Tables


{-| R\_PointToAngle2 x1 y1 x2 y2: the angle from (x1, y1) to (x2, y2).

The C source sets the renderer's `viewx`/`viewy` to (x1, y1) and calls R\_PointToAngle,
which folds the offset into the first octant, looks the slope up in `tantoangle`, and
unfolds the angle again. The octants are kept exactly, off-by-ones included.

-}
pointToAngle2 : Fixed -> Fixed -> Fixed -> Fixed -> Angle
pointToAngle2 x1 y1 x2 y2 =
    pointToAngle (Int32.sub x2 x1) (Int32.sub y2 y1)


{-| R\_PointToAngle, on the offset (x, y) from the viewpoint.
-}
pointToAngle : Fixed -> Fixed -> Angle
pointToAngle x y =
    let
        tan num den =
            Tables.tantoangle (Angle.slopeDiv num den)
    in
    if x == 0 && y == 0 then
        0

    else if x >= 0 then
        if y >= 0 then
            if x > y then
                -- octant 0
                tan y x

            else
                -- octant 1
                Angle.sub (Angle.ang90 - 1) (tan x y)

        else
            let
                ny =
                    Int32.neg y
            in
            if x > ny then
                -- octant 8
                Angle.neg (tan ny x)

            else
                -- octant 7
                Angle.add Angle.ang270 (tan x ny)

    else
        let
            nx =
                Int32.neg x
        in
        if y >= 0 then
            if nx > y then
                -- octant 3
                Angle.sub (Angle.ang180 - 1) (tan y nx)

            else
                -- octant 2
                Angle.add Angle.ang90 (tan nx y)

        else
            let
                ny =
                    Int32.neg y
            in
            if nx > ny then
                -- octant 4
                Angle.add Angle.ang180 (tan ny nx)

            else
                -- octant 5
                Angle.sub (Angle.ang270 - 1) (tan nx ny)


{-| R\_PointToDist, from viewpoint (viewx, viewy) to (x, y): `pointToDist viewx viewy x y`.

The longer side divided by the cosine of the angle the slope makes. Where both offsets
are 0 the C source reads far past `tantoangle`; `Doom.Tables` gives 0 there.

-}
pointToDist : Fixed -> Fixed -> Fixed -> Fixed -> Fixed
pointToDist viewx viewy x y =
    let
        adx =
            Int32.abs (Int32.sub x viewx)

        ady =
            Int32.abs (Int32.sub y viewy)

        ( dx, dy ) =
            if ady > adx then
                ( ady, adx )

            else
                ( adx, ady )

        angle =
            Angle.toFine
                (Angle.add (Tables.tantoangle (Int32.shiftRight Angle.dbits (Fixed.div dy dx))) Angle.ang90)
    in
    Fixed.div dx (Tables.finesine angle)


{-| R\_PointOnSide: 0 for the node's front (right) side, 1 for its back.
-}
pointOnSide : Fixed -> Fixed -> Node -> Int
pointOnSide x y node =
    if node.dx == 0 then
        if x <= node.x then
            boolToInt (node.dy > 0)

        else
            boolToInt (node.dy < 0)

    else if node.dy == 0 then
        if y <= node.y then
            boolToInt (node.dx < 0)

        else
            boolToInt (node.dx > 0)

    else
        let
            dx =
                Int32.sub x node.x

            dy =
                Int32.sub y node.y
        in
        -- Try to decide quickly by the sign bits.
        if xor4 node.dy node.dx dx dy < 0 then
            if Bitwise.xor node.dy dx < 0 then
                -- left is negative
                1

            else
                0

        else
            let
                left =
                    Fixed.mul (Fixed.toInt node.dy) dx

                right =
                    Fixed.mul dy (Fixed.toInt node.dx)
            in
            if right < left then
                0

            else
                1


{-| R\_PointInSubsector: walk the BSP from the head node (the last one) down to a leaf.
A map without nodes is one subsector.
-}
pointInSubsector : Fixed -> Fixed -> Level -> SubsectorId
pointInSubsector x y level =
    let
        numnodes =
            Array.length level.nodes

        walk nodenum =
            let
                node =
                    Level.node nodenum level

                child =
                    if pointOnSide x y node == 0 then
                        Tuple.first node.children

                    else
                        Tuple.second node.children
            in
            case child of
                Level.ChildNode n ->
                    walk n

                Level.ChildSubsector s ->
                    s
    in
    if numnodes == 0 then
        0

    else
        walk (numnodes - 1)


xor4 : Int -> Int -> Int -> Int -> Int
xor4 a b c d =
    Bitwise.xor (Bitwise.xor a b) (Bitwise.xor c d)


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0
