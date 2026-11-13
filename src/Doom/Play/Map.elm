module Doom.Play.Map exposing (teleportMove, checkPosition, tryMove, slideMove, aimLineAttack, lineAttack, useLines, radiusAttack, changeSector)

{-| Moving things and what they hit (p\_map.c): collision checks, moves, sliding,
hitscan aiming and attacks, using lines, explosions, and crushing when floors and
ceilings move. Results the C source leaves in globals (`tmfloorz`, `floatok`,
`linetarget`...) are in `World.scratch`.

@docs teleportMove, checkPosition, tryMove, slideMove, aimLineAttack, lineAttack, useLines, radiusAttack, changeSector

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


{-| P\_TeleportMove thing x y: move there, killing whatever is in the way.
-}
teleportMove : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
teleportMove =
    Debug.todo "Doom.Play.Map.teleportMove"


{-| P\_CheckPosition thing x y.
-}
checkPosition : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
checkPosition =
    Debug.todo "Doom.Play.Map.checkPosition"


{-| P\_TryMove thing x y.
-}
tryMove : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
tryMove =
    Debug.todo "Doom.Play.Map.tryMove"


{-| P\_SlideMove.
-}
slideMove : Env -> ThinkerId -> World -> World
slideMove =
    Debug.todo "Doom.Play.Map.slideMove"


{-| P\_AimLineAttack t1 angle distance: the slope to aim at; `linetarget` in the scratch.
-}
aimLineAttack : Env -> ThinkerId -> Angle -> Fixed -> World -> ( Fixed, World )
aimLineAttack =
    Debug.todo "Doom.Play.Map.aimLineAttack"


{-| P\_LineAttack t1 angle distance slope damage.
-}
lineAttack : Env -> ThinkerId -> Angle -> Fixed -> Fixed -> Int -> World -> World
lineAttack =
    Debug.todo "Doom.Play.Map.lineAttack"


{-| P\_UseLines player.
-}
useLines : Env -> Int -> World -> World
useLines =
    Debug.todo "Doom.Play.Map.useLines"


{-| P\_RadiusAttack spot source damage.
-}
radiusAttack : Env -> ThinkerId -> Maybe ThinkerId -> Int -> World -> World
radiusAttack =
    Debug.todo "Doom.Play.Map.radiusAttack"


{-| P\_ChangeSector sector crunch: fit things to the sector's new heights; True if something didn't fit.
-}
changeSector : Env -> SectorId -> Bool -> World -> ( Bool, World )
changeSector =
    Debug.todo "Doom.Play.Map.changeSector"
