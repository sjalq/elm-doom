module Doom.Play.Tick exposing (runThinkers, ticker)

{-| One game tic of a level (p\_tick.c): players think, then every thinker runs, then
specials update.

@docs runThinkers, ticker

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


{-| P\_RunThinkers.
-}
runThinkers : Env -> World -> World
runThinkers =
    Debug.todo "Doom.Play.Tick.runThinkers"


{-| P\_Ticker (unpaused): P\_PlayerThink for each player in the game, P\_RunThinkers, P\_UpdateSpecials, P\_RespawnSpecials, leveltime++.
-}
ticker : Env -> World -> World
ticker =
    Debug.todo "Doom.Play.Tick.ticker"
