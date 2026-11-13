module Doom.Play.Ceiling exposing (think, doCeiling, activateInStasisCeiling, ceilingCrushStop)

{-| Moving ceilings and crushers (p\_ceilng.c).

@docs think, doCeiling, activateInStasisCeiling, ceilingCrushStop

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


{-| T\_MoveCeiling.
-}
think : Env -> ThinkerId -> Thinker.Ceiling -> World -> World
think =
    Debug.todo "Doom.Play.Ceiling.think"


{-| EV\_DoCeiling.
-}
doCeiling : Env -> LineId -> Thinker.CeilingType -> World -> ( Bool, World )
doCeiling =
    Debug.todo "Doom.Play.Ceiling.doCeiling"


{-| P\_ActivateInStasisCeiling.
-}
activateInStasisCeiling : Env -> LineId -> World -> World
activateInStasisCeiling =
    Debug.todo "Doom.Play.Ceiling.activateInStasisCeiling"


{-| EV\_CeilingCrushStop.
-}
ceilingCrushStop : Env -> LineId -> World -> ( Bool, World )
ceilingCrushStop =
    Debug.todo "Doom.Play.Ceiling.ceilingCrushStop"
