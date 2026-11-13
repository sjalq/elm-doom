module Doom.Play.Action exposing (hooks, setMobjState, setPsprite)

{-| Running states' actions: P\_SetMobjState and P\_SetPsprite, the dispatch from each
`Action` to its function, and `hooks`, which ties them into `Env`.

@docs hooks, setMobjState, setPsprite

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (Psp)
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| The real functions behind `Env`'s hooks.
-}
hooks : Env.Hooks
hooks =
    Debug.todo "Doom.Play.Action.hooks"


{-| P\_SetMobjState.
-}
setMobjState : Env -> ThinkerId -> State -> World -> ( Bool, World )
setMobjState =
    Debug.todo "Doom.Play.Action.setMobjState"


{-| P\_SetPsprite.
-}
setPsprite : Env -> Int -> Psp -> State -> World -> World
setPsprite =
    Debug.todo "Doom.Play.Action.setPsprite"
