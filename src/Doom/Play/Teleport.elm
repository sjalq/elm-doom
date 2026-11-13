module Doom.Play.Teleport exposing (teleport)

{-| Teleporters (p\_telept.c).

@docs teleport

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


{-| EV\_Teleport line side thing.
-}
teleport : Env -> LineId -> Int -> ThinkerId -> World -> ( Bool, World )
teleport =
    Debug.todo "Doom.Play.Teleport.teleport"
