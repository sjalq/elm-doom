module Doom.Play.Ceiling exposing (think, doCeiling, activateInStasisCeiling, ceilingCrushStop)

{-| Moving ceilings and crushers (p\_ceilng.c).

Every ceiling is also in `World.activeCeilings` (C's `activeceilings[]`), which is how
a stop line finds the crushers to stop and a start line the stopped ones to restart.
A ceiling's `topheight` or `bottomheight` that its type doesn't use, and its
`olddirection` before it is first stopped, start at 0 (C leaves them unset).

@docs think, doCeiling, activateInStasisCeiling, ceilingCrushStop

-}

import Array
import Bitwise
import Doom.Fixed as Fixed
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.Plane as Plane exposing (PlaneResult(..))
import Doom.Play.SpecUtil as SpecUtil exposing (sectorSound)
import Doom.Thinker as Thinker exposing (Ceiling, CeilingType(..), Thinker(..))
import Doom.World as World exposing (World)


{-| T\_MoveCeiling: one tic of a ceiling. `direction` 0 is stopped (in stasis).
-}
think : Env -> ThinkerId -> Ceiling -> World -> World
think env id ceiling world =
    if ceiling.direction == 1 then
        moveUp env id ceiling world

    else if ceiling.direction == -1 then
        moveDown env id ceiling world

    else
        world


save : ThinkerId -> Ceiling -> World -> World
save id ceiling =
    World.setThinker id (CeilingThinker ceiling)


{-| The grinding noise every 8 tics, except for silent crushers.
-}
movingSound : Ceiling -> World -> World
movingSound ceiling world =
    if Bitwise.and world.leveltime 7 == 0 && ceiling.type_ /= SilentCrushAndRaise then
        sectorSound ceiling.sector Sfx.Stnmov world

    else
        world


moveUp : Env -> ThinkerId -> Ceiling -> World -> World
moveUp env id ceiling world =
    let
        ( res, moved ) =
            Plane.movePlane env ceiling.sector ceiling.speed ceiling.topheight False 1 ceiling.direction world

        sounded =
            movingSound ceiling moved
    in
    if res == PlanePastDest then
        case ceiling.type_ of
            RaiseToHighest ->
                removeActiveCeiling id ceiling.sector sounded

            SilentCrushAndRaise ->
                sounded
                    |> sectorSound ceiling.sector Sfx.Pstop
                    |> save id { ceiling | direction = -1 }

            FastCrushAndRaise ->
                save id { ceiling | direction = -1 } sounded

            CrushAndRaise ->
                save id { ceiling | direction = -1 } sounded

            _ ->
                sounded

    else
        sounded


moveDown : Env -> ThinkerId -> Ceiling -> World -> World
moveDown env id ceiling world =
    let
        ( res, moved ) =
            Plane.movePlane env ceiling.sector ceiling.speed ceiling.bottomheight ceiling.crush 1 ceiling.direction world

        sounded =
            movingSound ceiling moved
    in
    case res of
        PlanePastDest ->
            case ceiling.type_ of
                SilentCrushAndRaise ->
                    sounded
                        |> sectorSound ceiling.sector Sfx.Pstop
                        |> save id { ceiling | speed = Thinker.ceilSpeed, direction = 1 }

                CrushAndRaise ->
                    save id { ceiling | speed = Thinker.ceilSpeed, direction = 1 } sounded

                FastCrushAndRaise ->
                    save id { ceiling | direction = 1 } sounded

                LowerAndCrush ->
                    removeActiveCeiling id ceiling.sector sounded

                LowerToFloor ->
                    removeActiveCeiling id ceiling.sector sounded

                RaiseToHighest ->
                    sounded

        PlaneCrushed ->
            case ceiling.type_ of
                SilentCrushAndRaise ->
                    save id { ceiling | speed = Thinker.ceilSpeed // 8 } sounded

                CrushAndRaise ->
                    save id { ceiling | speed = Thinker.ceilSpeed // 8 } sounded

                LowerAndCrush ->
                    save id { ceiling | speed = Thinker.ceilSpeed // 8 } sounded

                _ ->
                    sounded

        PlaneOk ->
            sounded


{-| EV\_DoCeiling: start a ceiling moving in every sector with the line's tag that has
nothing moving yet; crusher lines first restart stopped crushers with their tag. True
if one started.
-}
doCeiling : Env -> LineId -> CeilingType -> World -> ( Bool, World )
doCeiling env lineId type_ world =
    let
        restarted =
            case type_ of
                FastCrushAndRaise ->
                    activateInStasisCeiling env lineId world

                SilentCrushAndRaise ->
                    activateInStasisCeiling env lineId world

                CrushAndRaise ->
                    activateInStasisCeiling env lineId world

                _ ->
                    world
    in
    SpecUtil.foldTaggedSectors (Level.line lineId env.level).tag
        (\sec ( rtn, w ) ->
            if (World.sector sec w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( True, startCeiling env type_ sec w )
        )
        restarted


startCeiling : Env -> CeilingType -> SectorId -> World -> World
startCeiling env type_ sec world =
    let
        s =
            World.sector sec world

        base =
            { type_ = type_
            , sector = sec
            , bottomheight = 0
            , topheight = 0
            , speed = 0
            , crush = False
            , direction = 0
            , tag = s.tag
            , olddirection = 0
            }

        down =
            { base | bottomheight = Fixed.add s.floorheight (8 * Fixed.fracunit), direction = -1, speed = Thinker.ceilSpeed }

        ceiling =
            case type_ of
                FastCrushAndRaise ->
                    { down | crush = True, topheight = s.ceilingheight, speed = Thinker.ceilSpeed * 2 }

                SilentCrushAndRaise ->
                    { down | crush = True, topheight = s.ceilingheight }

                CrushAndRaise ->
                    { down | crush = True, topheight = s.ceilingheight }

                LowerAndCrush ->
                    down

                LowerToFloor ->
                    { down | bottomheight = s.floorheight }

                RaiseToHighest ->
                    { base | topheight = SpecUtil.findHighestCeilingSurrounding env sec world, direction = 1, speed = Thinker.ceilSpeed }

        ( id, added ) =
            World.addThinker (CeilingThinker ceiling) world
    in
    added
        |> World.updateSector sec (\st -> { st | specialdata = Just id })
        |> addActiveCeiling id


{-| P\_AddActiveCeiling: into the first free slot (none when all MAXCEILINGS are taken,
as in C).
-}
addActiveCeiling : ThinkerId -> World -> World
addActiveCeiling id world =
    let
        search i =
            case Array.get i world.activeCeilings of
                Nothing ->
                    world

                Just Nothing ->
                    { world | activeCeilings = Array.set i (Just id) world.activeCeilings }

                Just (Just _) ->
                    search (i + 1)
    in
    search 0


{-| P\_RemoveActiveCeiling: the ceiling is done; its sector is free and its thinker
goes. Nothing happens for a ceiling that isn't listed, as in C.
-}
removeActiveCeiling : ThinkerId -> SectorId -> World -> World
removeActiveCeiling id sec world =
    let
        search i =
            case Array.get i world.activeCeilings of
                Nothing ->
                    world

                Just slot ->
                    if slot == Just id then
                        { world | activeCeilings = Array.set i Nothing world.activeCeilings }
                            |> World.updateSector sec (\s -> { s | specialdata = Nothing })
                            |> World.removeThinker id

                    else
                        search (i + 1)
    in
    search 0


{-| P\_ActivateInStasisCeiling: restart the stopped ceilings with the line's tag.
-}
activateInStasisCeiling : Env -> LineId -> World -> World
activateInStasisCeiling env lineId world =
    let
        tag =
            (Level.line lineId env.level).tag
    in
    Tuple.second
        (forActiveCeilings
            (\c ->
                if c.tag == tag && c.direction == 0 then
                    Just { c | direction = c.olddirection }

                else
                    Nothing
            )
            world
        )


{-| EV\_CeilingCrushStop: stop the moving ceilings with the line's tag. True if one
stopped.
-}
ceilingCrushStop : Env -> LineId -> World -> ( Bool, World )
ceilingCrushStop env lineId world =
    let
        tag =
            (Level.line lineId env.level).tag
    in
    forActiveCeilings
        (\c ->
            if c.tag == tag && c.direction /= 0 then
                Just { c | olddirection = c.direction, direction = 0 }

            else
                Nothing
        )
        world


{-| Go through `activeCeilings` in order, changing the ceilings `change` gives a new
state for; True if it changed any.
-}
forActiveCeilings : (Ceiling -> Maybe Ceiling) -> World -> ( Bool, World )
forActiveCeilings change world =
    Array.foldl
        (\slot ( any, w ) ->
            case slot |> Maybe.andThen (\id -> World.thinker id w |> Maybe.map (Tuple.pair id)) of
                Just ( id, CeilingThinker c ) ->
                    case change c of
                        Just c2 ->
                            ( True, save id c2 w )

                        Nothing ->
                            ( any, w )

                _ ->
                    ( any, w )
        )
        ( False, world )
        world.activeCeilings
