module Doom.Play.Sight exposing (checkSight)

{-| Line of sight (p\_sight.c), through the BSP and the REJECT table.

@docs checkSight

-}

import Array
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Int32 as Int32
import Doom.Level as Level exposing (SubsectorId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.World as World exposing (Divline, World)


{-| What the C source keeps in p\_sight.c's globals while it looks: the eye height,
the slopes to the top and bottom of the target (narrowed by each opening the sight
line passes), the sight line and its end.
-}
type alias Look =
    { sightzstart : Fixed
    , topslope : Fixed
    , bottomslope : Fixed
    , strace : Divline
    , t2x : Fixed
    , t2y : Fixed
    }


{-| P\_CheckSight t1 t2: whether t1 can see t2. Changes the world (validcount, scratch).

First the REJECT table: if it says nothing in t1's sector can see into t2's, that's
it. Otherwise the line from t1's eyes (three quarters of its height up) to t2 is
followed through the BSP, front to back, narrowing the window of slopes to t2's top
and bottom at each two-sided line it crosses.

-}
checkSight : Env -> ThinkerId -> ThinkerId -> World -> ( Bool, World )
checkSight env id1 id2 world =
    case ( World.mobj id1 world, World.mobj id2 world ) of
        ( Just t1, Just t2 ) ->
            let
                s1 =
                    World.mobjSector env.level t1

                s2 =
                    World.mobjSector env.level t2
            in
            if Level.rejected s1 s2 env.level then
                -- can't possibly be connected
                ( False, world )

            else
                let
                    ( _, counted ) =
                        World.nextValidcount world

                    sightzstart =
                        Int32.sub (Int32.add t1.z t1.height) (Int32.shiftRight 2 t1.height)

                    look =
                        { sightzstart = sightzstart
                        , topslope = Int32.sub (Int32.add t2.z t2.height) sightzstart
                        , bottomslope = Int32.sub t2.z sightzstart
                        , strace =
                            { x = t1.x
                            , y = t1.y
                            , dx = Int32.sub t2.x t1.x
                            , dy = Int32.sub t2.y t1.y
                            }
                        , t2x = t2.x
                        , t2y = t2.y
                        }

                    numnodes =
                        Array.length env.level.nodes

                    -- the head node is the last node output
                    ( seen, ( lookAfter, worldAfter ) ) =
                        if numnodes == 0 then
                            crossSubsector env 0 ( look, counted )

                        else
                            crossBspNode env (Level.ChildNode (numnodes - 1)) ( look, counted )
                in
                ( seen
                , World.setScratch
                    (\s ->
                        { s
                            | sightzstart = lookAfter.sightzstart
                            , sightTopslope = lookAfter.topslope
                            , sightBottomslope = lookAfter.bottomslope
                            , strace = lookAfter.strace
                            , t2x = lookAfter.t2x
                            , t2y = lookAfter.t2y
                        }
                    )
                    worldAfter
                )

        _ ->
            ( False, world )


{-| P\_DivlineSide: 0 front, 1 back, 2 on the line.

The horizontal case compares `x` with the line's `y`, as the C source does.

-}
divlineSide : Fixed -> Fixed -> Divline -> Int
divlineSide x y node =
    if node.dx == 0 then
        if x == node.x then
            2

        else if x <= node.x then
            boolToInt (node.dy > 0)

        else
            boolToInt (node.dy < 0)

    else if node.dy == 0 then
        if x == node.y then
            2

        else if y <= node.y then
            boolToInt (node.dx < 0)

        else
            boolToInt (node.dx > 0)

    else
        let
            dx =
                Int32.sub x node.x

            dy =
                Int32.sub y node.y

            left =
                Int32.mul (Fixed.toInt node.dy) (Fixed.toInt dx)

            right =
                Int32.mul (Fixed.toInt dy) (Fixed.toInt node.dx)
        in
        if right < left then
            0

        else if left == right then
            2

        else
            1


{-| P\_InterceptVector2 v2 v1: the fraction along v2 where it crosses v1.
-}
interceptVector2 : Divline -> Divline -> Fixed
interceptVector2 v2 v1 =
    let
        den =
            Int32.sub
                (Fixed.mul (Int32.shiftRight 8 v1.dy) v2.dx)
                (Fixed.mul (Int32.shiftRight 8 v1.dx) v2.dy)
    in
    if den == 0 then
        0

    else
        Fixed.div
            (Int32.add
                (Fixed.mul (Int32.shiftRight 8 (Int32.sub v1.x v2.x)) v1.dy)
                (Fixed.mul (Int32.shiftRight 8 (Int32.sub v2.y v1.y)) v1.dx)
            )
            den


{-| P\_CrossBSPNode: whether the sight line gets through this node's subtree. It crosses
the side the eye is on first, then the other side if the line reaches it.
-}
crossBspNode : Env -> Level.Child -> ( Look, World ) -> ( Bool, ( Look, World ) )
crossBspNode env child acc =
    case child of
        Level.ChildSubsector ss ->
            crossSubsector env ss acc

        Level.ChildNode n ->
            let
                bsp =
                    Level.node n env.level

                partition =
                    { x = bsp.x, y = bsp.y, dx = bsp.dx, dy = bsp.dy }

                look =
                    Tuple.first acc

                side =
                    case divlineSide look.strace.x look.strace.y partition of
                        2 ->
                            -- an "on" should cross both sides
                            0

                        s ->
                            s

                ( front, back ) =
                    if side == 0 then
                        bsp.children

                    else
                        ( Tuple.second bsp.children, Tuple.first bsp.children )

                ( ok, acc1 ) =
                    crossBspNode env front acc
            in
            if not ok then
                ( False, acc1 )

            else if side == divlineSide look.t2x look.t2y partition then
                -- the line doesn't touch the other side
                ( True, acc1 )

            else
                crossBspNode env back acc1


{-| P\_CrossSubsector: whether the sight line gets through the subsector's segs. Each
line is checked once per look (validcount).
-}
crossSubsector : Env -> SubsectorId -> ( Look, World ) -> ( Bool, ( Look, World ) )
crossSubsector env num acc0 =
    let
        sub =
            Level.subsector num env.level

        go segnum count ( look, world ) =
            if count <= 0 then
                -- passed the subsector ok
                ( True, ( look, world ) )

            else
                let
                    seg =
                        Level.seg segnum env.level

                    lineId =
                        seg.linedef
                in
                if (World.lineState lineId world).validcount == world.validcount then
                    -- already checked the other side
                    go (segnum + 1) (count - 1) ( look, world )

                else
                    let
                        marked =
                            World.updateLine lineId (\l -> { l | validcount = world.validcount }) world
                    in
                    case crossLine env seg look marked of
                        Continue look1 ->
                            go (segnum + 1) (count - 1) ( look1, marked )

                        Blocked look1 ->
                            ( False, ( look1, marked ) )
    in
    go sub.firstline sub.numlines acc0


{-| What a seg does to the sight line.
-}
type Crossing
    = Continue Look
    | Blocked Look


{-| The body of P\_CrossSubsector's loop, for one seg whose line hasn't been checked.
-}
crossLine : Env -> Level.Seg -> Look -> World -> Crossing
crossLine env seg look world =
    let
        line =
            Level.line seg.linedef env.level

        v1 =
            line.v1

        v2 =
            line.v2

        divl =
            { x = v1.x, y = v1.y, dx = Int32.sub v2.x v1.x, dy = Int32.sub v2.y v1.y }
    in
    if divlineSide v1.x v1.y look.strace == divlineSide v2.x v2.y look.strace then
        -- the line isn't crossed
        Continue look

    else if divlineSide look.strace.x look.strace.y divl == divlineSide look.t2x look.t2y divl then
        Continue look

    else if not (Flags.has Flags.mlTwoSided line.flags) then
        -- stop because it is not two sided anyway
        Blocked look

    else
        case seg.backsector of
            Nothing ->
                -- A two-sided line without a back sector: the C source would read
                -- through a null pointer. Valid maps don't have them; take it as a wall.
                Blocked look

            Just backId ->
                crossOpening (World.sector seg.frontsector world) (World.sector backId world) divl look


{-| The rest of the loop body: a two-sided line, which narrows the window of slopes
where its floors or ceilings differ.
-}
crossOpening : World.SectorState -> World.SectorState -> Divline -> Look -> Crossing
crossOpening front back divl look =
    if front.floorheight == back.floorheight && front.ceilingheight == back.ceilingheight then
        -- no wall to block sight with
        Continue look

    else
        let
            opentop =
                min front.ceilingheight back.ceilingheight

            openbottom =
                max front.floorheight back.floorheight
        in
        if openbottom >= opentop then
            -- quick test for totally closed doors
            Blocked look

        else
            let
                frac =
                    interceptVector2 look.strace divl

                bottomslope =
                    if front.floorheight /= back.floorheight then
                        max look.bottomslope (Fixed.div (Int32.sub openbottom look.sightzstart) frac)

                    else
                        look.bottomslope

                topslope =
                    if front.ceilingheight /= back.ceilingheight then
                        min look.topslope (Fixed.div (Int32.sub opentop look.sightzstart) frac)

                    else
                        look.topslope

                narrowed =
                    { look | topslope = topslope, bottomslope = bottomslope }
            in
            if topslope <= bottomslope then
                Blocked narrowed

            else
                Continue narrowed


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0
