module Doom.Play.Plat exposing (think, doPlat, activateInStasis, stopPlat)

{-| Lifts and moving platforms (p\_plats.c).

@docs think, doPlat, activateInStasis, stopPlat

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Thinker as Thinker exposing (Thinker)
import Doom.World as World exposing (World)


{-| T\_PlatRaise.
-}
think : Env -> ThinkerId -> Thinker.Plat -> World -> World
think =
    Debug.todo "Doom.Play.Plat.think"


{-| EV\_DoPlat line type amount.
-}
doPlat : Env -> LineId -> Thinker.PlatType -> Int -> World -> ( Bool, World )
doPlat =
    Debug.todo "Doom.Play.Plat.doPlat"


{-| P\_ActivateInStasis tag.
-}
activateInStasis : Env -> Int -> World -> World
activateInStasis =
    Debug.todo "Doom.Play.Plat.activateInStasis"


{-| EV\_StopPlat.
-}
stopPlat : Env -> LineId -> World -> World
stopPlat =
    Debug.todo "Doom.Play.Plat.stopPlat"
