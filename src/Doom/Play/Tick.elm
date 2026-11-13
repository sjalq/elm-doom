module Doom.Play.Tick exposing (runThinkers, ticker)

{-| One game tic of a level (p\_tick.c): players think, then every thinker runs, then
specials update.

@docs runThinkers, ticker

-}

import Array
import Dict
import Doom.Defs as Defs
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Ceiling as Ceiling
import Doom.Play.Door as Door
import Doom.Play.Env exposing (Env)
import Doom.Play.Floor as Floor
import Doom.Play.Light as Light
import Doom.Play.Mobj as PMobj
import Doom.Play.Movement as Movement
import Doom.Play.Plat as Plat
import Doom.Play.Spec as Spec
import Doom.Play.User as User
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)


{-| P\_RunThinkers: run every thinker once, in list order (ascending id), including
those added while the list is being run (they go on the end, as in the C source). A
thinker marked `Removed` is dropped from the list when the run gets to it.
-}
runThinkers : Env -> World -> World
runThinkers env world =
    runFrom env -1 world


{-| Run the thinkers after `lastId`, then any added meanwhile.
-}
runFrom : Env -> ThinkerId -> World -> World
runFrom env lastId world =
    case List.filter (\id -> id > lastId) (Dict.keys world.thinkers) of
        [] ->
            world

        ids ->
            let
                ran =
                    List.foldl (runOne env) world ids
            in
            runFrom env (List.foldl max lastId ids) ran


runOne : Env -> ThinkerId -> World -> World
runOne env id world =
    case World.thinker id world of
        Just Removed ->
            -- time to remove it
            { world | thinkers = Dict.remove id world.thinkers }

        Just MobjThinker ->
            Movement.mobjThinker env id world

        Just (DoorThinker door) ->
            Door.think env id door world

        Just (PlatThinker plat) ->
            Plat.think env id plat world

        Just (CeilingThinker ceiling) ->
            Ceiling.think env id ceiling world

        Just (FloorThinker floor) ->
            Floor.think env id floor world

        Just (FireFlickerThinker flicker) ->
            Light.thinkFireFlicker env id flicker world

        Just (LightFlashThinker flash) ->
            Light.thinkLightFlash env id flash world

        Just (StrobeThinker strobe) ->
            Light.thinkStrobe env id strobe world

        Just (GlowThinker glow) ->
            Light.thinkGlow env id glow world

        Nothing ->
            world


{-| P\_Ticker, for a tic that runs: P\_PlayerThink for each player in the game,
P\_RunThinkers, P\_UpdateSpecials, P\_RespawnSpecials, then `leveltime++`.

The C source first returns without doing anything when the game is paused, or when
the menu is up in a single-player game that isn't a demo (once a tic has run); that
is left to the caller, which simply doesn't call this for those tics.

-}
ticker : Env -> World -> World
ticker env world =
    let
        thought =
            List.foldl
                (\n w ->
                    if Array.get n w.playeringame == Just True then
                        User.playerThink env n w

                    else
                        w
                )
                world
                (List.range 0 (Defs.maxPlayers - 1))

        ran =
            thought
                |> runThinkers env
                |> Spec.updateSpecials env
                |> PMobj.respawnSpecials env
    in
    { ran | leveltime = ran.leveltime + 1 }
