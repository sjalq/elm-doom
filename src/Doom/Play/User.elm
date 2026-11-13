module Doom.Play.User exposing (playerThink)

{-| The player thinking each tic (p\_user.c): moving, the view bobbing, dying, using,
changing weapons, powers running out.

@docs playerThink

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


{-| P\_PlayerThink player.
-}
playerThink : Env -> Int -> World -> World
playerThink =
    Debug.todo "Doom.Play.User.playerThink"
