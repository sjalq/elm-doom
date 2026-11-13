module Doom.Play.Movement exposing (mobjThinker, explodeMissile, checkMissileSpawn, spawnMissile, spawnPlayerMissile)

{-| Things moving and thinking (the upper half of p\_mobj.c): momentum, gravity,
missiles, nightmare respawns, and P\_MobjThinker.

@docs mobjThinker, explodeMissile, checkMissileSpawn, spawnMissile, spawnPlayerMissile

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


{-| P\_MobjThinker.
-}
mobjThinker : Env -> ThinkerId -> World -> World
mobjThinker =
    Debug.todo "Doom.Play.Movement.mobjThinker"


{-| P\_ExplodeMissile.
-}
explodeMissile : Env -> ThinkerId -> World -> World
explodeMissile =
    Debug.todo "Doom.Play.Movement.explodeMissile"


{-| P\_CheckMissileSpawn.
-}
checkMissileSpawn : Env -> ThinkerId -> World -> World
checkMissileSpawn =
    Debug.todo "Doom.Play.Movement.checkMissileSpawn"


{-| P\_SpawnMissile source dest type.
-}
spawnMissile : Env -> ThinkerId -> ThinkerId -> MobjType -> World -> ( ThinkerId, World )
spawnMissile =
    Debug.todo "Doom.Play.Movement.spawnMissile"


{-| P\_SpawnPlayerMissile source type.
-}
spawnPlayerMissile : Env -> ThinkerId -> MobjType -> World -> World
spawnPlayerMissile =
    Debug.todo "Doom.Play.Movement.spawnPlayerMissile"
