module Doom.Play.Setup exposing
    ( setupLevel, mapName, newWorld
    , BodyQue, emptyBodyQue, checkSpot, deathMatchSpawnPlayer, playerStart
    )

{-| Starting a level (p\_setup.c's P\_SetupLevel) and putting players at their starts
(g\_game.c's G\_CheckSpot and G\_DeathMatchSpawnPlayer, which P\_SetupLevel and
G\_DoReborn both use).

    Setup.setupLevel wad data params bodyque world
        |> Result.map (\( env, newWorld, _ ) -> StateHash.line newWorld)

@docs setupLevel, mapName, newWorld
@docs BodyQue, emptyBodyQue, checkSpot, deathMatchSpawnPlayer, playerStart

-}

import Array exposing (Array)
import Dict
import Doom.Angle as Angle
import Doom.Data exposing (Data)
import Doom.Defs as Defs exposing (GameMode(..))
import Doom.Fixed as Fixed
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx
import Doom.Int32 as Int32
import Doom.Level as Level exposing (MapThing)
import Doom.Level.Load as Load
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Action as Action
import Doom.Play.Env exposing (Env, Params)
import Doom.Play.Map as Map
import Doom.Play.Mobj as PMobj
import Doom.Play.Spawn as Spawn
import Doom.Play.Spec as Spec
import Doom.Player as Player exposing (Player)
import Doom.Random as Random
import Doom.Sound as Sound
import Doom.Tables as Tables
import Doom.Thinker as Thinker
import Doom.Wad exposing (Wad)
import Doom.World as World exposing (World)


{-| g\_game.c's `bodyque`: the last 32 bodies players left behind when they respawned in
a net game; the oldest is removed to make room. `slot` counts bodies queued since the
level started.
-}
type alias BodyQue =
    { bodies : Array (Maybe ThinkerId)
    , slot : Int
    }


{-| An empty queue.
-}
emptyBodyQue : BodyQue
emptyBodyQue =
    { bodies = Array.repeat bodyQueSize Nothing, slot = 0 }


bodyQueSize : Int
bodyQueSize =
    32


{-| The map's marker lump: MAPxx for Doom II, ExMy otherwise.
-}
mapName : GameMode -> Int -> Int -> String
mapName mode episode map =
    case mode of
        Commercial ->
            "MAP" ++ String.padLeft 2 '0' (String.fromInt map)

        _ ->
            "E" ++ String.fromInt episode ++ "M" ++ String.fromInt map


{-| An empty world for a game about to start: no level, the players as `Player.new`
leaves them, the random numbers at the start of their table. G\_InitNew's reborn
flags and P\_SetupLevel do the rest.
-}
newWorld : Array Bool -> World
newWorld playeringame =
    { leveltime = 0
    , rng = Random.init
    , validcount = 0
    , thinkers = Dict.empty
    , mobjs = Dict.empty
    , nextThinkerId = 0
    , sectors = Array.empty
    , lines = Array.empty
    , sides = Array.empty
    , blocklinks = Array.empty
    , sectorThings = Array.empty
    , players = Array.repeat Defs.maxPlayers Player.new
    , playeringame = playeringame
    , playerStarts = Array.repeat Defs.maxPlayers Nothing
    , deathmatchStarts = []
    , activePlats = Array.repeat Thinker.maxPlats Nothing
    , activeCeilings = Array.repeat Thinker.maxCeilings Nothing
    , buttons = Array.repeat World.maxButtons Nothing
    , itemRespawn =
        { things = Array.repeat World.itemQueSize { x = 0, y = 0, angle = 0, type_ = 0, options = 0 }
        , times = Array.repeat World.itemQueSize 0
        , head = 0
        , tail = 0
        }
    , brainTargets = Array.empty
    , brainTargetOn = 0
    , brainEasy = False
    , lineSpecialList = []
    , totalkills = 0
    , totalitems = 0
    , totalsecret = 0
    , exit = Nothing
    , sounds = []
    , scratch = World.emptyScratch
    , gametic = 0
    }


{-| P\_SetupLevel for the episode, map and skill in `params`: clear the level's state,
load the map, spawn its things in THINGS order (stopping, as linuxdoom does, at the
first Doom II monster if the game isn't Doom II), spawn deathmatch players at random
deathmatch starts, and set up the specials.

What outlives a level is kept from the world given: the players (but their kill, item
and secret counts are zeroed), the random numbers, `validcount`, `gametic`, the
player starts (C's `playerstarts` is only overwritten by the new map's), the item
respawn queue's contents, A\_BrainSpit's `easy`, and the scratch variables (C's
statics). Thinker ids keep counting up, so a stale id never names a new thing.

Also gives back the body queue with its slot reset (`bodyqueslot = 0`).

-}
setupLevel : Wad -> Data -> Params -> BodyQue -> World -> Result String ( Env, World, BodyQue )
setupLevel wad data params bodyque old =
    Load.load data (mapName params.gameMode params.episode params.map) wad
        |> Result.map
            (\loaded ->
                let
                    env =
                        { level = loaded.level
                        , data = data
                        , params = params
                        , hooks = Action.hooks
                        }

                    players =
                        old.players
                            |> Array.map (\p -> { p | killcount = 0, secretcount = 0, itemcount = 0 })
                            |> Array.indexedMap
                                (\i p ->
                                    if i == params.consoleplayer then
                                        { p | viewz = 1 }

                                    else
                                        p
                                )

                    bm =
                        loaded.level.blockmap

                    itemRespawn =
                        old.itemRespawn

                    fresh =
                        { old
                            | leveltime = 0
                            , thinkers = Dict.empty
                            , mobjs = Dict.empty
                            , sectors = loaded.sectors
                            , lines = loaded.lines
                            , sides = loaded.sides
                            , blocklinks = Array.repeat (bm.width * bm.height) []
                            , sectorThings = Array.repeat (Array.length loaded.sectors) []
                            , players = players
                            , deathmatchStarts = []
                            , activePlats = Array.repeat Thinker.maxPlats Nothing
                            , activeCeilings = Array.repeat Thinker.maxCeilings Nothing
                            , buttons = Array.repeat World.maxButtons Nothing
                            , itemRespawn = { itemRespawn | head = 0, tail = 0 }
                            , brainTargets = Array.empty
                            , brainTargetOn = 0
                            , lineSpecialList = []
                            , totalkills = 0
                            , totalitems = 0
                            , totalsecret = 0
                            , exit = Nothing
                            , sounds = []
                        }

                    withThings =
                        loadThings env loaded.level.things fresh

                    ( withPlayers, que ) =
                        if params.deathmatch /= 0 then
                            List.range 0 (Defs.maxPlayers - 1)
                                |> List.foldl
                                    (\i ( w, q ) ->
                                        if inGame i w then
                                            deathMatchSpawnPlayer env i ( q, World.updatePlayer i (\p -> { p | mo = Nothing }) w )
                                                |> swap

                                        else
                                            ( w, q )
                                    )
                                    ( withThings, { bodyque | slot = 0 } )

                        else
                            ( withThings, { bodyque | slot = 0 } )
                in
                ( env, Spec.spawnSpecials env withPlayers, que )
            )


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


inGame : Int -> World -> Bool
inGame i world =
    Array.get i world.playeringame |> Maybe.withDefault False


{-| P\_LoadThings: spawn each thing; if the game isn't Doom II, the first Doom II
monster stops the loading (a `break` in linuxdoom, so nothing after it spawns).
-}
loadThings : Env -> List MapThing -> World -> World
loadThings env things world =
    case things of
        [] ->
            world

        mt :: rest ->
            if env.params.gameMode /= Commercial && List.member mt.type_ doom2Monsters then
                world

            else
                loadThings env rest (Spawn.spawnMapThing env mt world)


doom2Monsters : List Int
doom2Monsters =
    [ 68, 64, 88, 89, 69, 67, 71, 65, 66, 84 ]


{-| G\_CheckSpot: whether player n can spawn at this start. At the start of a level
(the player has no body yet) only another player's body on the same spot blocks it.
Otherwise the player's old body must fit there (P\_CheckPosition); then that body goes
on the body queue (removing the oldest if it is full) and teleport fog appears, with
its sound unless this is the level's first tic.
-}
checkSpot : Env -> Int -> MapThing -> ( BodyQue, World ) -> ( Bool, ( BodyQue, World ) )
checkSpot env playernum mthing ( que, world ) =
    let
        x =
            Fixed.fromInt mthing.x

        y =
            Fixed.fromInt mthing.y
    in
    case World.player playernum world |> Maybe.andThen .mo of
        Nothing ->
            let
                occupied =
                    List.range 0 (playernum - 1)
                        |> List.any
                            (\i ->
                                case World.playerMobj i world of
                                    Just m ->
                                        m.x == x && m.y == y

                                    Nothing ->
                                        False
                            )
            in
            ( not occupied, ( que, world ) )

        Just body ->
            let
                ( fits, checked ) =
                    Map.checkPosition env body x y world
            in
            if not fits then
                ( False, ( que, checked ) )

            else
                let
                    flushed =
                        if que.slot >= bodyQueSize then
                            case Array.get (modBy bodyQueSize que.slot) que.bodies |> Maybe.andThen identity of
                                Just old ->
                                    PMobj.removeMobj env old checked

                                Nothing ->
                                    checked

                        else
                            checked

                    newQue =
                        { bodies = Array.set (modBy bodyQueSize que.slot) (Just body) que.bodies
                        , slot = que.slot + 1
                        }

                    ss =
                        Geometry.pointInSubsector x y env.level

                    an =
                        Angle.toFine (Angle.wrap (Int32.mul Angle.ang45 (mthing.angle // 45)))

                    floorheight =
                        (World.sector (Level.subsector ss env.level).sector flushed).floorheight

                    ( fog, withFog ) =
                        PMobj.spawnMobj env
                            (Int32.add x (Int32.mul 20 (Tables.finecosine an)))
                            (Int32.add y (Int32.mul 20 (Tables.finesine an)))
                            floorheight
                            MobjType.Tfog
                            flushed

                    firstTic =
                        (World.player env.params.consoleplayer withFog |> Maybe.map .viewz) == Just 1
                in
                ( True
                , ( newQue
                  , if firstTic then
                        withFog

                    else
                        World.startSound (Sound.FromMobj fog) Sfx.Telept withFog
                  )
                )


{-| G\_DeathMatchSpawnPlayer: try 20 random deathmatch starts; the first free one
becomes this player's (its type is changed to the player's, as in the C source) and
the player spawns there. If none is free, the player spawns at their own start.
-}
deathMatchSpawnPlayer : Env -> Int -> ( BodyQue, World ) -> ( BodyQue, World )
deathMatchSpawnPlayer env playernum state =
    let
        try j ( que, world ) =
            let
                starts =
                    Array.fromList world.deathmatchStarts

                selections =
                    Array.length starts
            in
            if j >= 20 || selections == 0 then
                ( que, Spawn.spawnPlayer env (playerStart playernum world) world )

            else
                let
                    ( r, w1 ) =
                        World.pRandom world

                    i =
                        remainderBy selections r

                    start =
                        Array.get i starts |> Maybe.withDefault (playerStart playernum w1)

                    ( ok, ( q2, w2 ) ) =
                        checkSpot env playernum start ( que, w1 )
                in
                if ok then
                    let
                        chosen =
                            { start | type_ = playernum + 1 }

                        w3 =
                            { w2 | deathmatchStarts = Array.toList (Array.set i chosen starts) }
                    in
                    ( q2, Spawn.spawnPlayer env chosen w3 )

                else
                    try (j + 1) ( q2, w2 )
    in
    try 0 state


{-| `playerstarts[n]`; a start the map never set is all zeroes, as C's static array.
-}
playerStart : Int -> World -> MapThing
playerStart n world =
    Array.get n world.playerStarts
        |> Maybe.andThen identity
        |> Maybe.withDefault { x = 0, y = 0, angle = 0, type_ = 0, options = 0 }
