module Doom.Play.Plat exposing (think, doPlat, activateInStasis, stopPlat)

{-| Lifts and moving platforms (p\_plats.c).

Every plat is also in `World.activePlats` (C's `activeplats[]`), which is how a stop
line finds the perpetual lifts to stop and a start line the stopped ones to restart.
Fields C leaves unset for some types (`low` of a raising plat, `count` before the
first stop, `oldstatus` before the first stop line) start at 0 and `PlatUp` here.

@docs think, doPlat, activateInStasis, stopPlat

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
import Doom.Thinker as Thinker exposing (Plat, PlatStatus(..), PlatType(..), Thinker(..))
import Doom.World as World exposing (World)


{-| T\_PlatRaise: one tic of a plat.
-}
think : Env -> ThinkerId -> Plat -> World -> World
think env id plat world =
    case plat.status of
        PlatUp ->
            moveUp env id plat world

        PlatDown ->
            let
                ( res, moved ) =
                    Plane.movePlane env plat.sector plat.speed plat.low False 0 -1 world
            in
            if res == PlanePastDest then
                moved
                    |> save id { plat | count = plat.wait, status = PlatWaiting }
                    |> sectorSound plat.sector Sfx.Pstop

            else
                moved

        PlatWaiting ->
            let
                counted =
                    { plat | count = plat.count - 1 }
            in
            if counted.count /= 0 then
                save id counted world

            else
                let
                    status =
                        if (World.sector plat.sector world).floorheight == plat.low then
                            PlatUp

                        else
                            PlatDown
                in
                world
                    |> save id { counted | status = status }
                    |> sectorSound plat.sector Sfx.Pstart

        PlatInStasis ->
            world


save : ThinkerId -> Plat -> World -> World
save id plat =
    World.setThinker id (PlatThinker plat)


moveUp : Env -> ThinkerId -> Plat -> World -> World
moveUp env id plat world =
    let
        ( res, moved ) =
            Plane.movePlane env plat.sector plat.speed plat.high plat.crush 0 1 world

        sounded =
            if (plat.type_ == RaiseAndChange || plat.type_ == RaiseToNearestAndChange) && Bitwise.and moved.leveltime 7 == 0 then
                sectorSound plat.sector Sfx.Stnmov moved

            else
                moved
    in
    if res == PlaneCrushed && not plat.crush then
        sounded
            |> save id { plat | count = plat.wait, status = PlatDown }
            |> sectorSound plat.sector Sfx.Pstart

    else if res == PlanePastDest then
        let
            stopped =
                sounded
                    |> save id { plat | count = plat.wait, status = PlatWaiting }
                    |> sectorSound plat.sector Sfx.Pstop
        in
        case plat.type_ of
            PerpetualRaise ->
                stopped

            _ ->
                removeActivePlat id plat.sector stopped

    else
        sounded


{-| P\_AddActivePlat: into the first free slot of `activePlats`. (C stops the game when
there are more than MAXPLATS; then the plat is just not listed.)
-}
addActivePlat : ThinkerId -> World -> World
addActivePlat id world =
    case firstFree world.activePlats of
        Just i ->
            { world | activePlats = Array.set i (Just id) world.activePlats }

        Nothing ->
            world


firstFree : Array.Array (Maybe a) -> Maybe Int
firstFree slots =
    let
        search i =
            case Array.get i slots of
                Nothing ->
                    Nothing

                Just Nothing ->
                    Just i

                Just (Just _) ->
                    search (i + 1)
    in
    search 0


{-| P\_RemoveActivePlat: the plat is done; its sector is free and its thinker goes.
(C stops the game for a plat that isn't listed; then nothing happens.)
-}
removeActivePlat : ThinkerId -> SectorId -> World -> World
removeActivePlat id sec world =
    case findSlot id world.activePlats of
        Just i ->
            { world | activePlats = Array.set i Nothing world.activePlats }
                |> World.updateSector sec (\s -> { s | specialdata = Nothing })
                |> World.removeThinker id

        Nothing ->
            world


findSlot : ThinkerId -> Array.Array (Maybe ThinkerId) -> Maybe Int
findSlot id slots =
    let
        search i =
            case Array.get i slots of
                Nothing ->
                    Nothing

                Just slot ->
                    if slot == Just id then
                        Just i

                    else
                        search (i + 1)
    in
    search 0


{-| EV\_DoPlat line type amount: start a plat in every sector with the line's tag that
has nothing moving yet (`amount` is how far RaiseAndChange raises). A perpetual lift
line first restarts the stopped ones with its tag. True if one started.
-}
doPlat : Env -> LineId -> PlatType -> Int -> World -> ( Bool, World )
doPlat env lineId type_ amount world =
    let
        line =
            Level.line lineId env.level

        restarted =
            if type_ == PerpetualRaise then
                activateInStasis env line.tag world

            else
                world
    in
    SpecUtil.foldTaggedSectors line.tag
        (\sec ( rtn, w ) ->
            if (World.sector sec w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( True, startPlat env line type_ amount sec w )
        )
        restarted


startPlat : Env -> Level.Line -> PlatType -> Int -> SectorId -> World -> World
startPlat env line type_ amount sec world =
    let
        ( id, added ) =
            World.addThinker Removed world

        withData =
            World.updateSector sec (\s -> { s | specialdata = Just id }) added

        floor =
            (World.sector sec withData).floorheight

        base =
            { sector = sec
            , speed = 0
            , low = 0
            , high = 0
            , wait = 0
            , count = 0
            , status = PlatUp
            , oldstatus = PlatUp
            , crush = False
            , tag = line.tag
            , type_ = type_
            }

        frontFloorpic =
            (World.sector line.frontsector withData).floorpic

        lowest w =
            min floor (SpecUtil.findLowestFloorSurrounding env sec w)

        downWaitUp speed w =
            ( { base | speed = speed, low = lowest w, high = floor, wait = 35 * Thinker.platWait, status = PlatDown }
            , sectorSound sec Sfx.Pstart w
            )

        ( plat, w1 ) =
            case type_ of
                RaiseToNearestAndChange ->
                    let
                        changed =
                            World.updateSector sec (\s -> { s | floorpic = frontFloorpic }) withData
                    in
                    ( { base | speed = Thinker.platSpeed // 2, high = SpecUtil.findNextHighestFloor env sec floor changed, wait = 0, status = PlatUp }
                    , changed
                        |> World.updateSector sec (\s -> { s | special = 0 })
                        |> sectorSound sec Sfx.Stnmov
                    )

                RaiseAndChange ->
                    ( { base | speed = Thinker.platSpeed // 2, high = Fixed.add floor (amount * Fixed.fracunit), wait = 0, status = PlatUp }
                    , withData
                        |> World.updateSector sec (\s -> { s | floorpic = frontFloorpic })
                        |> sectorSound sec Sfx.Stnmov
                    )

                DownWaitUpStay ->
                    downWaitUp (Thinker.platSpeed * 4) withData

                BlazeDWUS ->
                    downWaitUp (Thinker.platSpeed * 8) withData

                PerpetualRaise ->
                    let
                        ( r, rolled ) =
                            World.pRandom withData

                        status =
                            if Bitwise.and r 1 == 0 then
                                PlatUp

                            else
                                PlatDown
                    in
                    ( { base
                        | speed = Thinker.platSpeed
                        , low = lowest withData
                        , high = max floor (SpecUtil.findHighestFloorSurrounding env sec withData)
                        , wait = 35 * Thinker.platWait
                        , status = status
                      }
                    , sectorSound sec Sfx.Pstart rolled
                    )
    in
    w1
        |> save id plat
        |> addActivePlat id


{-| P\_ActivateInStasis tag: restart the stopped plats with this tag.
-}
activateInStasis : Env -> Int -> World -> World
activateInStasis _ tag world =
    forActivePlats
        (\p ->
            if p.tag == tag && p.status == PlatInStasis then
                Just { p | status = p.oldstatus }

            else
                Nothing
        )
        world


{-| EV\_StopPlat: stop the moving plats with the line's tag (they remember what they
were doing).
-}
stopPlat : Env -> LineId -> World -> World
stopPlat env lineId world =
    let
        tag =
            (Level.line lineId env.level).tag
    in
    forActivePlats
        (\p ->
            if p.status /= PlatInStasis && p.tag == tag then
                Just { p | oldstatus = p.status, status = PlatInStasis }

            else
                Nothing
        )
        world


{-| Go through `activePlats` in order, changing the plats `change` gives a new state
for.
-}
forActivePlats : (Plat -> Maybe Plat) -> World -> World
forActivePlats change world =
    Array.foldl
        (\slot w ->
            case slot of
                Just id ->
                    case World.thinker id w of
                        Just (PlatThinker p) ->
                            case change p of
                                Just p2 ->
                                    save id p2 w

                                Nothing ->
                                    w

                        _ ->
                            w

                Nothing ->
                    w
        )
        world
        world.activePlats
