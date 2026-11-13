module Doom.Play.Floor exposing (think, doFloor, doFloorByTag, buildStairs, doDonut)

{-| Moving floors and stairs (p\_floor.c, and EV\_DoDonut from p\_spec.c).

Fields C leaves unset start at 0 here: `newspecial` (and `texture`) of most floors,
and every field EV\_BuildStairs doesn't set (a stair step's `crush` is False and its
type `RaiseFloor`, which behaves like any type but the two that change textures).

@docs think, doFloor, doFloorByTag, buildStairs, doDonut

-}

import Array
import Bitwise
import Doom.Data as Data
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.Plane as Plane exposing (PlaneResult(..))
import Doom.Play.SpecUtil as SpecUtil exposing (sectorSound)
import Doom.Thinker as Thinker exposing (FloorMove, FloorType(..), StairType(..), Thinker(..))
import Doom.World as World exposing (World)


{-| T\_MoveFloor: one tic of a moving floor. When it gets there, the sector is free
again, and a donut's slime or a LowerAndChange floor takes on its new flat and
special.
-}
think : Env -> ThinkerId -> FloorMove -> World -> World
think env id floor world =
    let
        ( res, moved ) =
            Plane.movePlane env floor.sector floor.speed floor.floordestheight floor.crush 0 floor.direction world

        sounded =
            if Bitwise.and moved.leveltime 7 == 0 then
                sectorSound floor.sector Sfx.Stnmov moved

            else
                moved
    in
    if res == PlanePastDest then
        let
            changes =
                (floor.direction == 1 && floor.type_ == DonutRaise)
                    || (floor.direction == -1 && floor.type_ == LowerAndChange)
        in
        sounded
            |> World.updateSector floor.sector
                (\s ->
                    if changes then
                        { s | specialdata = Nothing, special = floor.newspecial, floorpic = floor.texture }

                    else
                        { s | specialdata = Nothing }
                )
            |> World.removeThinker id
            |> sectorSound floor.sector Sfx.Pstop

    else
        sounded


{-| A floor thinker with nothing set yet.
-}
blankFloor : FloorType -> SectorId -> FloorMove
blankFloor type_ sec =
    { type_ = type_
    , crush = False
    , sector = sec
    , direction = 0
    , newspecial = 0
    , texture = 0
    , floordestheight = 0
    , speed = 0
    }


{-| P\_AddThinker and `sec->specialdata = floor`.
-}
addFloor : FloorMove -> World -> World
addFloor floor world =
    let
        ( id, added ) =
            World.addThinker (FloorThinker floor) world
    in
    World.updateSector floor.sector (\s -> { s | specialdata = Just id }) added


{-| EV\_DoFloor: start a floor moving in every sector with the line's tag that has
nothing moving yet. True if one started.
-}
doFloor : Env -> LineId -> FloorType -> World -> ( Bool, World )
doFloor env lineId type_ world =
    let
        line =
            Level.line lineId env.level
    in
    doFloorFor env line.tag line.frontsector type_ world


{-| EV\_DoFloor for a tag rather than a line: what A\_BossDeath does with its made-up
line that has only a tag. (Only RaiseFloor24AndChange looks at the line's front
sector; here that is sector 0.)
-}
doFloorByTag : Env -> Int -> FloorType -> World -> ( Bool, World )
doFloorByTag env tag type_ world =
    doFloorFor env tag 0 type_ world


doFloorFor : Env -> Int -> SectorId -> FloorType -> World -> ( Bool, World )
doFloorFor env tag frontsector type_ world =
    SpecUtil.foldTaggedSectors tag
        (\sec ( rtn, w ) ->
            if (World.sector sec w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( True, startFloor env frontsector type_ sec w )
        )
        world


startFloor : Env -> SectorId -> FloorType -> SectorId -> World -> World
startFloor env frontsector type_ sec world =
    let
        s =
            World.sector sec world

        blank =
            blankFloor type_ sec

        down speed dest =
            { blank | direction = -1, speed = speed, floordestheight = dest }

        up speed dest =
            { blank | direction = 1, speed = speed, floordestheight = dest }

        toCeiling crush =
            let
                dest =
                    min s.ceilingheight (SpecUtil.findLowestCeilingSurrounding env sec world)

                floor =
                    up Thinker.floorSpeed
                        (if crush then
                            Fixed.sub dest (8 * Fixed.fracunit)

                         else
                            dest
                        )
            in
            { floor | crush = crush }
    in
    case type_ of
        LowerFloor ->
            addFloor (down Thinker.floorSpeed (SpecUtil.findHighestFloorSurrounding env sec world)) world

        LowerFloorToLowest ->
            addFloor (down Thinker.floorSpeed (SpecUtil.findLowestFloorSurrounding env sec world)) world

        TurboLower ->
            let
                highest =
                    SpecUtil.findHighestFloorSurrounding env sec world

                dest =
                    if highest /= s.floorheight then
                        Fixed.add highest (8 * Fixed.fracunit)

                    else
                        highest
            in
            addFloor (down (Thinker.floorSpeed * 4) dest) world

        RaiseFloorCrush ->
            addFloor (toCeiling True) world

        RaiseFloor ->
            addFloor (toCeiling False) world

        RaiseFloorTurbo ->
            addFloor (up (Thinker.floorSpeed * 4) (SpecUtil.findNextHighestFloor env sec s.floorheight world)) world

        RaiseFloorToNearest ->
            addFloor (up Thinker.floorSpeed (SpecUtil.findNextHighestFloor env sec s.floorheight world)) world

        RaiseFloor24 ->
            addFloor (up Thinker.floorSpeed (Fixed.add s.floorheight (24 * Fixed.fracunit))) world

        RaiseFloor512 ->
            addFloor (up Thinker.floorSpeed (Fixed.add s.floorheight (512 * Fixed.fracunit))) world

        RaiseFloor24AndChange ->
            let
                front =
                    World.sector frontsector world
            in
            world
                |> addFloor (up Thinker.floorSpeed (Fixed.add s.floorheight (24 * Fixed.fracunit)))
                |> World.updateSector sec (\st -> { st | floorpic = front.floorpic, special = front.special })

        RaiseToTexture ->
            addFloor (up Thinker.floorSpeed (Fixed.add s.floorheight (shortestLowerTexture env sec world))) world

        LowerAndChange ->
            addFloor (lowerAndChange env sec world (down Thinker.floorSpeed (SpecUtil.findLowestFloorSurrounding env sec world))) world

        DonutRaise ->
            addFloor blank world


{-| raiseToTexture's `minsize`: the height of the shortest lower texture on either
side of the sector's two-sided lines (MAXINT if it has none, which the C source then
adds to the floor height, wrapping).
-}
shortestLowerTexture : Env -> SectorId -> World -> Fixed
shortestLowerTexture env sec world =
    let
        count =
            Array.length (Level.sector sec env.level).lines

        sideHeight i side =
            case SpecUtil.getSide env.level sec i side of
                Just sideId ->
                    Data.textureHeight (World.sideState sideId world).bottomtexture env.data

                Nothing ->
                    Fixed.maxInt
    in
    List.range 0 (count - 1)
        |> List.filter (SpecUtil.twoSided env.level sec)
        |> List.foldl (\i minsize -> min minsize (min (sideHeight i 0) (sideHeight i 1))) Fixed.maxInt


{-| lowerAndChange: take the flat and special of the first neighbour (across a
two-sided line) whose floor is at the destination height; the sector's own flat if
there is none.
-}
lowerAndChange : Env -> SectorId -> World -> FloorMove -> FloorMove
lowerAndChange env sec world floor =
    let
        count =
            Array.length (Level.sector sec env.level).lines

        otherSide i =
            if SpecUtil.getSector env.level sec i 0 == Just sec then
                SpecUtil.getSector env.level sec i 1

            else
                SpecUtil.getSector env.level sec i 0

        search i =
            if i >= count then
                { floor | texture = (World.sector sec world).floorpic }

            else if SpecUtil.twoSided env.level sec i then
                case otherSide i |> Maybe.map (\o -> World.sector o world) of
                    Just other ->
                        if other.floorheight == floor.floordestheight then
                            { floor | texture = other.floorpic, newspecial = other.special }

                        else
                            search (i + 1)

                    Nothing ->
                        search (i + 1)

            else
                search (i + 1)
    in
    search 0


{-| EV\_BuildStairs: from each sector with the line's tag, raise a staircase: each
step is the sector behind a two-sided line whose front is the step before, with the
same flat, one stair size higher.

As in the C source, the search for the next tagged sector carries on from the last
step built, and a step already moving still counts for the height of the next.

-}
buildStairs : Env -> LineId -> StairType -> World -> ( Bool, World )
buildStairs env lineId type_ world =
    let
        tag =
            (Level.line lineId env.level).tag

        ( speed, stairsize ) =
            case type_ of
                Build8 ->
                    ( Thinker.floorSpeed // 4, 8 * Fixed.fracunit )

                Turbo16 ->
                    ( Thinker.floorSpeed * 4, 16 * Fixed.fracunit )

        step sec height =
            let
                blank =
                    blankFloor RaiseFloor sec
            in
            { blank | direction = 1, speed = speed, floordestheight = height }

        loop secnum rtn w =
            let
                next =
                    SpecUtil.findSectorFromTag tag secnum w
            in
            if next < 0 then
                ( rtn, w )

            else
                let
                    s =
                        World.sector next w
                in
                if s.specialdata /= Nothing then
                    loop next rtn w

                else
                    let
                        height =
                            Fixed.add s.floorheight stairsize

                        ( last, built ) =
                            climb env stairsize s.floorpic step next height (addFloor (step next height) w)
                    in
                    loop last True built
    in
    loop -1 False world


{-| The do-while loop of EV\_BuildStairs: find the next step from `sec`, and go on from
it; the last step's sector when there is none.
-}
climb : Env -> Fixed -> Int -> (SectorId -> Fixed -> FloorMove) -> SectorId -> Fixed -> World -> ( SectorId, World )
climb env stairsize texture step sec height world =
    let
        tryLines lines h =
            case lines of
                [] ->
                    ( sec, world )

                lineId :: rest ->
                    let
                        l =
                            Level.line lineId env.level
                    in
                    if not (Flags.has Flags.mlTwoSided l.flags) || l.frontsector /= sec then
                        tryLines rest h

                    else
                        case l.backsector of
                            Nothing ->
                                -- C would read through a null backsector here.
                                tryLines rest h

                            Just tsec ->
                                let
                                    t =
                                        World.sector tsec world
                                in
                                if t.floorpic /= texture then
                                    tryLines rest h

                                else
                                    let
                                        h2 =
                                            Fixed.add h stairsize
                                    in
                                    if t.specialdata /= Nothing then
                                        tryLines rest h2

                                    else
                                        climb env stairsize texture step tsec h2 (addFloor (step tsec h2) world)
    in
    tryLines (SpecUtil.sectorLines env.level sec) height


{-| EV\_DoDonut: for each sector with the line's tag (the hole) that has nothing
moving, the sector around it (across its first line) rises as slime to the floor of
the sector beyond, taking that sector's flat, and the hole lowers to the same height.

The C source's test `(!flags & ML_TWOSIDED)` is always false, so only lines whose back
is the hole are skipped. Where C would read through a null sector (the hole's first
line or the ring's line being one-sided), this port skips the line.

-}
doDonut : Env -> LineId -> World -> ( Bool, World )
doDonut env lineId world =
    SpecUtil.foldTaggedSectors (Level.line lineId env.level).tag
        (\s1 ( rtn, w ) ->
            if (World.sector s1 w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( True, donut env s1 w )
        )
        world


donut : Env -> SectorId -> World -> World
donut env s1 world =
    let
        firstLine =
            Array.get 0 (Level.sector s1 env.level).lines
    in
    case firstLine |> Maybe.andThen (\l -> SpecUtil.getNextSector env.level l s1) of
        Nothing ->
            world

        Just s2 ->
            let
                ring =
                    SpecUtil.sectorLines env.level s2
                        |> List.filterMap (\l -> (Level.line l env.level).backsector)
                        |> List.filter (\back -> back /= s1)
                        |> List.head
            in
            case ring of
                Just s3 ->
                    let
                        outer =
                            World.sector s3 world

                        slime =
                            blankFloor DonutRaise s2

                        hole =
                            blankFloor LowerFloor s1
                    in
                    world
                        |> addFloor
                            { slime
                                | direction = 1
                                , speed = Thinker.floorSpeed // 2
                                , texture = outer.floorpic
                                , newspecial = 0
                                , floordestheight = outer.floorheight
                            }
                        |> addFloor
                            { hole
                                | direction = -1
                                , speed = Thinker.floorSpeed // 2
                                , floordestheight = outer.floorheight
                            }

                Nothing ->
                    world
