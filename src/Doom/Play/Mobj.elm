module Doom.Play.Mobj exposing (spawnMobj, removeMobj, respawnSpecials, spawnPuff, spawnBlood)

{-| Creating and removing things (the lower half of p\_mobj.c), and the deathmatch item
respawn queue. Moving them is `Doom.Play.Movement`; spawning players and map things is
`Doom.Play.Spawn`.

@docs spawnMobj, removeMobj, respawnSpecials, spawnPuff, spawnBlood

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


{-| P\_SpawnMobj x y z type (z may be `Defs.onFloorZ` or `Defs.onCeilingZ`).
-}
spawnMobj : Env -> Fixed -> Fixed -> Fixed -> MobjType -> World -> ( ThinkerId, World )
spawnMobj =
    Debug.todo "Doom.Play.Mobj.spawnMobj"


{-| P\_RemoveMobj.
-}
removeMobj : Env -> ThinkerId -> World -> World
removeMobj =
    Debug.todo "Doom.Play.Mobj.removeMobj"


{-| P\_RespawnSpecials: deathmatch item respawning.
-}
respawnSpecials : Env -> World -> World
respawnSpecials =
    Debug.todo "Doom.Play.Mobj.respawnSpecials"


{-| P\_SpawnPuff x y z.
-}
spawnPuff : Env -> Fixed -> Fixed -> Fixed -> World -> World
spawnPuff =
    Debug.todo "Doom.Play.Mobj.spawnPuff"


{-| P\_SpawnBlood x y z damage.
-}
spawnBlood : Env -> Fixed -> Fixed -> Fixed -> Int -> World -> World
spawnBlood =
    Debug.todo "Doom.Play.Mobj.spawnBlood"
