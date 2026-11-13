module Doom.Play.Sight exposing (checkSight)

{-| Line of sight (p\_sight.c), through the BSP and the REJECT table.

@docs checkSight

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


{-| P\_CheckSight t1 t2: whether t1 can see t2. Changes the world (validcount, scratch).
-}
checkSight : Env -> ThinkerId -> ThinkerId -> World -> ( Bool, World )
checkSight =
    Debug.todo "Doom.Play.Sight.checkSight"
