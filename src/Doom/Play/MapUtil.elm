module Doom.Play.MapUtil exposing (Intercept, InterceptTarget(..), ptAddLines, ptAddThings, ptEarlyOut, aproxDistance, pointOnLineSide, boxOnLineSide, pointOnDivlineSide, makeDivline, interceptVector, lineOpening, unsetThingPosition, setThingPosition, blockLinesIterator, blockThingsIterator, pathTraverse)

{-| Movement and collision helpers (p\_maputl.c): distances, which side of a line a point
or box is on, line openings, linking things into the blockmap and their sector, and the
blockmap iterators and path traversal the game's collision and hitscan code is built
on.

The C source's iterators take a function pointer and stop when it returns false; here
the function also gets and gives back the `World`.

@docs Intercept, InterceptTarget, ptAddLines, ptAddThings, ptEarlyOut, aproxDistance, pointOnLineSide, boxOnLineSide, pointOnDivlineSide, makeDivline, interceptVector, lineOpening, unsetThingPosition, setThingPosition, blockLinesIterator, blockThingsIterator, pathTraverse

-}

import Array exposing (Array)
import Bitwise
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Int32 as Int32
import Doom.Level as Level exposing (LineId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.World as World exposing (Divline, World)


{-| intercept\_t: something a traced path crosses, `frac` of the way along it.
-}
type alias Intercept =
    { frac : Fixed
    , target : InterceptTarget
    }


{-| A line or a thing.
-}
type InterceptTarget
    = InterceptLine LineId
    | InterceptThing ThinkerId


{-| PT\_ADDLINES.
-}
ptAddLines : Int
ptAddLines =
    1


{-| PT\_ADDTHINGS.
-}
ptAddThings : Int
ptAddThings =
    2


{-| PT\_EARLYOUT.
-}
ptEarlyOut : Int
ptEarlyOut =
    4



-- DISTANCES AND SIDES


{-| P\_AproxDistance dx dy: the longer side plus half the shorter, a cheap estimate of
the distance.
-}
aproxDistance : Fixed -> Fixed -> Fixed
aproxDistance dx dy =
    let
        ax =
            Int32.abs dx

        ay =
            Int32.abs dy
    in
    if ax < ay then
        Int32.sub (Int32.add ax ay) (Int32.shiftRight 1 ax)

    else
        Int32.sub (Int32.add ax ay) (Int32.shiftRight 1 ay)


{-| P\_PointOnLineSide: 0 front, 1 back.
-}
pointOnLineSide : Fixed -> Fixed -> Level.Line -> Int
pointOnLineSide x y line =
    if line.dx == 0 then
        if x <= line.v1.x then
            boolToInt (line.dy > 0)

        else
            boolToInt (line.dy < 0)

    else if line.dy == 0 then
        if y <= line.v1.y then
            boolToInt (line.dx < 0)

        else
            boolToInt (line.dx > 0)

    else
        let
            left =
                Fixed.mul (Fixed.toInt line.dy) (Int32.sub x line.v1.x)

            right =
                Fixed.mul (Int32.sub y line.v1.y) (Fixed.toInt line.dx)
        in
        if right < left then
            0

        else
            1


{-| P\_BoxOnLineSide: 0 front, 1 back, -1 crossing. The line is taken as infinite.
-}
boxOnLineSide : Level.Box -> Level.Line -> Int
boxOnLineSide box ld =
    let
        flipIf cond p =
            if cond then
                1 - p

            else
                p

        ( p1, p2 ) =
            case ld.slopetype of
                Level.Horizontal ->
                    ( flipIf (ld.dx < 0) (boolToInt (box.top > ld.v1.y))
                    , flipIf (ld.dx < 0) (boolToInt (box.bottom > ld.v1.y))
                    )

                Level.Vertical ->
                    ( flipIf (ld.dy < 0) (boolToInt (box.right < ld.v1.x))
                    , flipIf (ld.dy < 0) (boolToInt (box.left < ld.v1.x))
                    )

                Level.Positive ->
                    ( pointOnLineSide box.left box.top ld
                    , pointOnLineSide box.right box.bottom ld
                    )

                Level.Negative ->
                    ( pointOnLineSide box.right box.top ld
                    , pointOnLineSide box.left box.bottom ld
                    )
    in
    if p1 == p2 then
        p1

    else
        -1


{-| P\_PointOnDivlineSide: 0 front, 1 back. Decides by sign bits where it can, and
otherwise compares the cross product at 8 bits less precision.
-}
pointOnDivlineSide : Fixed -> Fixed -> Divline -> Int
pointOnDivlineSide x y line =
    if line.dx == 0 then
        if x <= line.x then
            boolToInt (line.dy > 0)

        else
            boolToInt (line.dy < 0)

    else if line.dy == 0 then
        if y <= line.y then
            boolToInt (line.dx < 0)

        else
            boolToInt (line.dx > 0)

    else
        let
            dx =
                Int32.sub x line.x

            dy =
                Int32.sub y line.y
        in
        if Bitwise.xor (Bitwise.xor line.dy line.dx) (Bitwise.xor dx dy) < 0 then
            if Bitwise.xor line.dy dx < 0 then
                -- left is negative
                1

            else
                0

        else
            let
                left =
                    Fixed.mul (Int32.shiftRight 8 line.dy) (Int32.shiftRight 8 dx)

                right =
                    Fixed.mul (Int32.shiftRight 8 dy) (Int32.shiftRight 8 line.dx)
            in
            if right < left then
                0

            else
                1


{-| P\_MakeDivline.
-}
makeDivline : Level.Line -> Divline
makeDivline li =
    { x = li.v1.x, y = li.v1.y, dx = li.dx, dy = li.dy }


{-| P\_InterceptVector v2 v1: the fraction along v2 where it crosses v1 (0 for parallel
lines, where the C source once stopped the game).
-}
interceptVector : Divline -> Divline -> Fixed
interceptVector v2 v1 =
    let
        den =
            Int32.sub
                (Fixed.mul (Int32.shiftRight 8 v1.dy) v2.dx)
                (Fixed.mul (Int32.shiftRight 8 v1.dx) v2.dy)
    in
    if den == 0 then
        0

    else
        let
            num =
                Int32.add
                    (Fixed.mul (Int32.shiftRight 8 (Int32.sub v1.x v2.x)) v1.dy)
                    (Fixed.mul (Int32.shiftRight 8 (Int32.sub v2.y v1.y)) v1.dx)
        in
        Fixed.div num den


{-| P\_LineOpening: sets `opentop`, `openbottom`, `openrange` and `lowfloor` in the scratch.
For a one-sided line only `openrange` is set (to 0), as in the C source.
-}
lineOpening : Env -> LineId -> World -> World
lineOpening env lineId world =
    let
        line =
            Level.line lineId env.level
    in
    case line.backSide of
        Nothing ->
            World.setScratch (\s -> { s | openrange = 0 }) world

        Just _ ->
            let
                front =
                    World.sector line.frontsector world

                back =
                    World.sector (Maybe.withDefault line.frontsector line.backsector) world

                opentop =
                    min front.ceilingheight back.ceilingheight

                ( openbottom, lowfloor ) =
                    if front.floorheight > back.floorheight then
                        ( front.floorheight, back.floorheight )

                    else
                        ( back.floorheight, front.floorheight )
            in
            World.setScratch
                (\s ->
                    { s
                        | opentop = opentop
                        , openbottom = openbottom
                        , lowfloor = lowfloor
                        , openrange = Int32.sub opentop openbottom
                    }
                )
                world



-- THING POSITIONS


{-| The blockmap cell index a point is in, if it is on the map.
-}
blockIndex : Env -> Fixed -> Fixed -> Maybe Int
blockIndex env x y =
    let
        bm =
            env.level.blockmap

        bx =
            Int32.shiftRight Defs.mapBlockShift (Int32.sub x bm.orgx)

        by =
            Int32.shiftRight Defs.mapBlockShift (Int32.sub y bm.orgy)
    in
    if bx >= 0 && bx < bm.width && by >= 0 && by < bm.height then
        Just (by * bm.width + bx)

    else
        Nothing


{-| P\_UnsetThingPosition: unlink the thing from its sector's list and its blockmap cell.

The C source unlinks through the thing's own `sprev`/`bprev` pointers; a thing in the
blockmap is always linked into the cell its `x`/`y` are in (things that move without
relinking, such as missiles, have MF\_NOBLOCKMAP), so removing it from that cell's list
is the same.

-}
unsetThingPosition : Env -> ThinkerId -> World -> World
unsetThingPosition env id world =
    case World.mobj id world of
        Nothing ->
            world

        Just thing ->
            let
                withoutSector =
                    if Flags.has Flags.mfNoSector thing.flags then
                        world

                    else
                        { world
                            | sectorThings =
                                updateArray (World.mobjSector env.level thing)
                                    (List.filter ((/=) id))
                                    world.sectorThings
                        }
            in
            if Flags.has Flags.mfNoBlockmap thing.flags then
                withoutSector

            else
                case blockIndex env thing.x thing.y of
                    Just cell ->
                        { withoutSector
                            | blocklinks = updateArray cell (List.filter ((/=) id)) withoutSector.blocklinks
                        }

                    Nothing ->
                        withoutSector


{-| P\_SetThingPosition: find the thing's subsector and link it into its sector's list and
blockmap cell (at the front of each). A thing off the blockmap isn't linked into any
cell.
-}
setThingPosition : Env -> ThinkerId -> World -> World
setThingPosition env id world =
    case World.mobj id world of
        Nothing ->
            world

        Just thing0 ->
            let
                thing =
                    { thing0 | subsector = Geometry.pointInSubsector thing0.x thing0.y env.level }

                placed =
                    World.setMobj thing world

                inSector =
                    if Flags.has Flags.mfNoSector thing.flags then
                        placed

                    else
                        { placed
                            | sectorThings =
                                updateArray (World.mobjSector env.level thing) ((::) id) placed.sectorThings
                        }
            in
            if Flags.has Flags.mfNoBlockmap thing.flags then
                inSector

            else
                case blockIndex env thing.x thing.y of
                    Just cell ->
                        { inSector | blocklinks = updateArray cell ((::) id) inSector.blocklinks }

                    Nothing ->
                        inSector



-- BLOCKMAP ITERATORS


{-| P\_BlockLinesIterator x y func: each line in blockmap cell (x, y) not yet checked this validcount, until func gives False.

Lines are marked with the current `validcount` as they are visited, so a caller who
bumps `validcount` once and walks several cells sees each line once. The cell's lists
include the lump's leading line 0, as the original game's do.

-}
blockLinesIterator : Env -> Int -> Int -> (LineId -> World -> ( Bool, World )) -> World -> ( Bool, World )
blockLinesIterator env x y func world =
    let
        ( ok, ( (), w ) ) =
            blockLinesFold env x y (\ld ( (), w0 ) -> Tuple.mapSecond (Tuple.pair ()) (func ld w0)) ( (), world )
    in
    ( ok, w )


{-| P\_BlockLinesIterator with an extra value threaded through the calls.
-}
blockLinesFold : Env -> Int -> Int -> (LineId -> ( a, World ) -> ( Bool, ( a, World ) )) -> ( a, World ) -> ( Bool, ( a, World ) )
blockLinesFold env x y func acc =
    let
        bm =
            env.level.blockmap

        go lines ( a, world ) =
            case lines of
                [] ->
                    ( True, ( a, world ) )

                ld :: rest ->
                    if (World.lineState ld world).validcount == world.validcount then
                        go rest ( a, world )

                    else
                        let
                            marked =
                                World.updateLine ld (\l -> { l | validcount = world.validcount }) world

                            ( ok, next ) =
                                func ld ( a, marked )
                        in
                        if ok then
                            go rest next

                        else
                            ( False, next )
    in
    if x < 0 || y < 0 || x >= bm.width || y >= bm.height then
        ( True, acc )

    else
        go (Level.blockmapLines x y bm) acc


{-| P\_BlockThingsIterator x y func: each thing linked into blockmap cell (x, y), most
recently linked first, until func gives False.

The C source follows each thing's `bnext` pointer after calling func on it, so the walk
sees what func changed: a thing after the current one that func unlinked is skipped,
and a thing func unlinked goes on from where it was (its `bnext` isn't cleared). Here,
after each call, the walk goes on from the thing's place in the cell's list as it now
is, or, if it is no longer linked, from where it was before the call.

-}
blockThingsIterator : Env -> Int -> Int -> (ThinkerId -> World -> ( Bool, World )) -> World -> ( Bool, World )
blockThingsIterator env x y func world =
    let
        ( ok, ( (), w ) ) =
            blockThingsFold env x y (\id ( (), w0 ) -> Tuple.mapSecond (Tuple.pair ()) (func id w0)) ( (), world )
    in
    ( ok, w )


{-| P\_BlockThingsIterator with an extra value threaded through the calls.
-}
blockThingsFold : Env -> Int -> Int -> (ThinkerId -> ( a, World ) -> ( Bool, ( a, World ) )) -> ( a, World ) -> ( Bool, ( a, World ) )
blockThingsFold env x y func ( a0, world0 ) =
    let
        bm =
            env.level.blockmap

        go cell things ( a, world ) =
            case things of
                [] ->
                    ( True, ( a, world ) )

                id :: before ->
                    let
                        ( ok, ( a1, world1 ) ) =
                            func id ( a, world )
                    in
                    if ok then
                        let
                            ( nextCell, nextThings ) =
                                successors env cell id before world1
                        in
                        go nextCell nextThings ( a1, world1 )

                    else
                        ( False, ( a1, world1 ) )
    in
    if x < 0 || y < 0 || x >= bm.width || y >= bm.height then
        ( True, ( a0, world0 ) )

    else
        let
            cell =
                y * bm.width + x
        in
        go cell (cellThings cell world0) ( a0, world0 )


{-| Where `mobj->bnext` leads after the thing has been visited: on from it in the list
it is linked into now (the cell being walked, or the cell it is in), or, if it isn't
linked any more, on through the list as it was before the visit.
-}
successors : Env -> Int -> ThinkerId -> List ThinkerId -> World -> ( Int, List ThinkerId )
successors env cell id before world =
    case after id (cellThings cell world) of
        Just rest ->
            ( cell, rest )

        Nothing ->
            let
                elsewhere =
                    World.mobj id world
                        |> Maybe.andThen (\m -> blockIndex env m.x m.y)
                        |> Maybe.andThen (\c -> after id (cellThings c world) |> Maybe.map (Tuple.pair c))
            in
            Maybe.withDefault ( cell, before ) elsewhere


cellThings : Int -> World -> List ThinkerId
cellThings cell world =
    Array.get cell world.blocklinks |> Maybe.withDefault []


{-| The part of the list after the first `x`, if `x` is in it.
-}
after : a -> List a -> Maybe (List a)
after x list =
    case list of
        [] ->
            Nothing

        y :: rest ->
            if x == y then
                Just rest

            else
                after x rest



-- PATH TRAVERSAL


{-| PIT\_AddLineIntercepts: add the line if the trace crosses it. Stops the search (an
early out) at a one-sided line closer than the trace's end when `earlyout` is set.
-}
addLineIntercept : Env -> LineId -> ( List Intercept, World ) -> ( Bool, ( List Intercept, World ) )
addLineIntercept env lineId ( intercepts, world ) =
    let
        ld =
            Level.line lineId env.level

        trace =
            world.scratch.trace

        ( s1, s2 ) =
            -- avoid precision problems with two routines
            if
                (trace.dx > Fixed.fracunit * 16)
                    || (trace.dy > Fixed.fracunit * 16)
                    || (trace.dx < -Fixed.fracunit * 16)
                    || (trace.dy < -Fixed.fracunit * 16)
            then
                ( pointOnDivlineSide ld.v1.x ld.v1.y trace
                , pointOnDivlineSide ld.v2.x ld.v2.y trace
                )

            else
                ( pointOnLineSide trace.x trace.y ld
                , pointOnLineSide (Int32.add trace.x trace.dx) (Int32.add trace.y trace.dy) ld
                )
    in
    if s1 == s2 then
        -- the line isn't crossed
        ( True, ( intercepts, world ) )

    else
        let
            frac =
                interceptVector trace (makeDivline ld)
        in
        if frac < 0 then
            -- behind the source
            ( True, ( intercepts, world ) )

        else if world.scratch.earlyout && frac < Fixed.fracunit && ld.backsector == Nothing then
            ( False, ( intercepts, world ) )

        else
            ( True, ( { frac = frac, target = InterceptLine lineId } :: intercepts, world ) )


{-| PIT\_AddThingIntercepts: add the thing if the trace crosses its corner-to-corner
diagonal (the one facing the trace).
-}
addThingIntercept : ThinkerId -> ( List Intercept, World ) -> ( Bool, ( List Intercept, World ) )
addThingIntercept id ( intercepts, world ) =
    case World.mobj id world of
        Nothing ->
            ( True, ( intercepts, world ) )

        Just thing ->
            let
                trace =
                    world.scratch.trace

                tracepositive =
                    Bitwise.xor trace.dx trace.dy > 0

                x1 =
                    Int32.sub thing.x thing.radius

                x2 =
                    Int32.add thing.x thing.radius

                ( y1, y2 ) =
                    if tracepositive then
                        ( Int32.add thing.y thing.radius, Int32.sub thing.y thing.radius )

                    else
                        ( Int32.sub thing.y thing.radius, Int32.add thing.y thing.radius )

                s1 =
                    pointOnDivlineSide x1 y1 trace

                s2 =
                    pointOnDivlineSide x2 y2 trace
            in
            if s1 == s2 then
                ( True, ( intercepts, world ) )

            else
                let
                    frac =
                        interceptVector trace
                            { x = x1, y = y1, dx = Int32.sub x2 x1, dy = Int32.sub y2 y1 }
                in
                if frac < 0 then
                    ( True, ( intercepts, world ) )

                else
                    ( True, ( { frac = frac, target = InterceptThing id } :: intercepts, world ) )


{-| P\_TraverseIntercepts: call func on the intercepts nearest first, up to `maxfrac`.

As in the C source, each round picks the first intercept with the smallest `frac`
(ties go to the one added first), and marks it done by setting its `frac` to MAXINT.

-}
traverseIntercepts : Array Intercept -> (Intercept -> World -> ( Bool, World )) -> Fixed -> World -> ( Bool, World )
traverseIntercepts intercepts func maxfrac world =
    let
        count =
            Array.length intercepts

        nearest arr =
            Array.foldl
                (\inter ( i, ( best, dist ) ) ->
                    if inter.frac < dist then
                        ( i + 1, ( i, inter.frac ) )

                    else
                        ( i + 1, ( best, dist ) )
                )
                ( 0, ( -1, Fixed.maxInt ) )
                arr
                |> Tuple.second

        loop left arr w =
            if left <= 0 then
                ( True, w )

            else
                let
                    ( index, dist ) =
                        nearest arr
                in
                if dist > maxfrac then
                    -- checked everything in range
                    ( True, w )

                else
                    case Array.get index arr of
                        Nothing ->
                            ( True, w )

                        Just inter ->
                            let
                                ( ok, w1 ) =
                                    func inter w
                            in
                            if ok then
                                loop (left - 1) (Array.set index { inter | frac = Fixed.maxInt } arr) w1

                            else
                                ( False, w1 )
    in
    loop count intercepts world


{-| P\_PathTraverse x1 y1 x2 y2 flags trav: the lines and things crossed going from (x1, y1) to (x2, y2), nearest first. Sets `trace` in the scratch.

The C source keeps at most MAXINTERCEPTS (128) intercepts and writes past its array
beyond that; here there is no limit.

-}
pathTraverse : Env -> Fixed -> Fixed -> Fixed -> Fixed -> Int -> (Intercept -> World -> ( Bool, World )) -> World -> ( Bool, World )
pathTraverse env x1in y1in x2in y2in flags trav world0 =
    let
        bm =
            env.level.blockmap

        offBlockEdge v org =
            -- don't side exactly on a line
            if Bitwise.and (Int32.sub v org) (Defs.mapBlockSize - 1) == 0 then
                Int32.add v Fixed.fracunit

            else
                v

        x1t =
            offBlockEdge x1in bm.orgx

        y1t =
            offBlockEdge y1in bm.orgy

        trace =
            { x = x1t, y = y1t, dx = Int32.sub x2in x1t, dy = Int32.sub y2in y1t }

        ( _, world1 ) =
            World.nextValidcount world0

        world =
            World.setScratch
                (\s -> { s | earlyout = Flags.has ptEarlyOut flags, trace = trace, ptflags = flags })
                world1

        x1 =
            Int32.sub x1t bm.orgx

        y1 =
            Int32.sub y1t bm.orgy

        x2 =
            Int32.sub x2in bm.orgx

        y2 =
            Int32.sub y2in bm.orgy

        xt1 =
            Int32.shiftRight Defs.mapBlockShift x1

        yt1 =
            Int32.shiftRight Defs.mapBlockShift y1

        xt2 =
            Int32.shiftRight Defs.mapBlockShift x2

        yt2 =
            Int32.shiftRight Defs.mapBlockShift y2

        fracPart v =
            Bitwise.and (Int32.shiftRight Defs.mapBtoFrac v) (Fixed.fracunit - 1)

        ( mapxstep, xpartial, ystep ) =
            if xt2 > xt1 then
                ( 1, Fixed.fracunit - fracPart x1, Fixed.div (Int32.sub y2 y1) (Int32.abs (Int32.sub x2 x1)) )

            else if xt2 < xt1 then
                ( -1, fracPart x1, Fixed.div (Int32.sub y2 y1) (Int32.abs (Int32.sub x2 x1)) )

            else
                ( 0, Fixed.fracunit, 256 * Fixed.fracunit )

        yintercept0 =
            Int32.add (Int32.shiftRight Defs.mapBtoFrac y1) (Fixed.mul xpartial ystep)

        ( mapystep, ypartial, xstep ) =
            if yt2 > yt1 then
                ( 1, Fixed.fracunit - fracPart y1, Fixed.div (Int32.sub x2 x1) (Int32.abs (Int32.sub y2 y1)) )

            else if yt2 < yt1 then
                ( -1, fracPart y1, Fixed.div (Int32.sub x2 x1) (Int32.abs (Int32.sub y2 y1)) )

            else
                ( 0, Fixed.fracunit, 256 * Fixed.fracunit )

        xintercept0 =
            Int32.add (Int32.shiftRight Defs.mapBtoFrac x1) (Fixed.mul ypartial xstep)

        addLines mapx mapy acc =
            if Flags.has ptAddLines flags then
                blockLinesFold env mapx mapy (addLineIntercept env) acc

            else
                ( True, acc )

        addThings mapx mapy acc =
            if Flags.has ptAddThings flags then
                blockThingsFold env mapx mapy addThingIntercept acc

            else
                ( True, acc )

        -- Step through the map blocks; the count stops a rounding error from
        -- skipping the end.
        step count mapx mapy xintercept yintercept acc =
            if count >= 64 then
                ( True, acc )

            else
                let
                    ( linesOk, acc1 ) =
                        addLines mapx mapy acc
                in
                if not linesOk then
                    ( False, acc1 )

                else
                    let
                        ( thingsOk, acc2 ) =
                            addThings mapx mapy acc1
                    in
                    if not thingsOk then
                        ( False, acc2 )

                    else if mapx == xt2 && mapy == yt2 then
                        ( True, acc2 )

                    else if Int32.shiftRight Fixed.fracbits yintercept == mapy then
                        step (count + 1) (mapx + mapxstep) mapy xintercept (Int32.add yintercept ystep) acc2

                    else if Int32.shiftRight Fixed.fracbits xintercept == mapx then
                        step (count + 1) mapx (mapy + mapystep) (Int32.add xintercept xstep) yintercept acc2

                    else
                        step (count + 1) mapx mapy xintercept yintercept acc2

        ( collected, ( intercepts, worldAfter ) ) =
            step 0 xt1 yt1 xintercept0 yintercept0 ( [], world )
    in
    if collected then
        traverseIntercepts (Array.fromList (List.reverse intercepts)) trav Fixed.fracunit worldAfter

    else
        ( False, worldAfter )



-- HELPERS


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray i f arr =
    case Array.get i arr of
        Just v ->
            Array.set i (f v) arr

        Nothing ->
            arr


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0
