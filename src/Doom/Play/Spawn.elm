module Doom.Play.Spawn exposing (spawnPlayer, spawnMapThing)

{-| Spawning players and the map's things (P\_SpawnPlayer and P\_SpawnMapThing from
p\_mobj.c).

@docs spawnPlayer, spawnMapThing

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| P\_SpawnPlayer.
-}
spawnPlayer : Env -> Level.MapThing -> World -> World
spawnPlayer =
    Debug.todo "Doom.Play.Spawn.spawnPlayer"


{-| P\_SpawnMapThing.
-}
spawnMapThing : Env -> Level.MapThing -> World -> World
spawnMapThing =
    Debug.todo "Doom.Play.Spawn.spawnMapThing"
