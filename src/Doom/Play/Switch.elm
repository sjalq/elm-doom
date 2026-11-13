module Doom.Play.Switch exposing (changeSwitchTexture, useSpecialLine)

{-| Switches and buttons, and what using a line does (p\_switch.c).

@docs changeSwitchTexture, useSpecialLine

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


{-| P\_ChangeSwitchTexture line useAgain.
-}
changeSwitchTexture : Env -> LineId -> Bool -> World -> World
changeSwitchTexture =
    Debug.todo "Doom.Play.Switch.changeSwitchTexture"


{-| P\_UseSpecialLine thing line side.
-}
useSpecialLine : Env -> ThinkerId -> LineId -> Int -> World -> ( Bool, World )
useSpecialLine =
    Debug.todo "Doom.Play.Switch.useSpecialLine"
