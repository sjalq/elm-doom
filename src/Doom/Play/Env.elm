module Doom.Play.Env exposing
    ( Env, Params, Hooks(..), HookFunctions
    , stateDef, mobjInfo
    , setMobjState, setPsprite, crossSpecialLine, shootSpecialLine, useSpecialLine
    )

{-| What every function of the game's play code (the C source's p\_\*.c) is given
besides the `World`: the map, the WAD's tables, the game's settings, and hooks.


# Hooks

In the C source, p\_mobj.c's P\_SetMobjState calls the monsters' and weapons' action
functions, which call p\_map.c, which calls P\_SetMobjState again; p\_map.c's moves
cross line specials, which move floors, which call p\_map.c. Elm modules can't import
each other in a circle, so the few functions that close those circles are reached
through `Hooks`, which `Doom.Play.Action` fills in with the real ones:

  - `setMobjState`: P\_SetMobjState (runs the state's action).
  - `setPsprite`: P\_SetPsprite (runs the weapon state's action).
  - `crossSpecialLine`, `shootSpecialLine`: p\_spec.c's.
  - `useSpecialLine`: p\_switch.c's P\_UseSpecialLine.

Call them through the functions of the same name here.

@docs Env, Params, Hooks, HookFunctions
@docs stateDef, mobjInfo
@docs setMobjState, setPsprite, crossSpecialLine, shootSpecialLine, useSpecialLine

-}

import Bitwise
import Doom.Data exposing (Data)
import Doom.Defs exposing (GameMode, Psp, Skill)
import Doom.Fixed as Fixed
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level exposing (Level, LineId)
import Doom.Mobj exposing (ThinkerId)
import Doom.World exposing (World)


{-| The context of a level being played.
-}
type alias Env =
    { level : Level
    , data : Data
    , params : Params
    , hooks : Hooks
    }


{-| The game's settings for this level (g\_game.c's globals the play code reads).

  - `fast`: monsters and their shots are faster (-fast, and Nightmare).
  - `deathmatch`: 0 for none, 1 for deathmatch, 2 for altdeath.
  - `consoleplayer`: the player whose view is shown and who hears sounds.

-}
type alias Params =
    { gameMode : GameMode
    , skill : Skill
    , episode : Int
    , map : Int
    , deathmatch : Int
    , netgame : Bool
    , respawnmonsters : Bool
    , fast : Bool
    , nomonsters : Bool
    , consoleplayer : Int
    }


{-| The functions that close the C source's circles of calls (see above).
-}
type Hooks
    = Hooks HookFunctions


{-| The hooks' functions.
-}
type alias HookFunctions =
    { setMobjState : Env -> ThinkerId -> State -> World -> ( Bool, World )
    , setPsprite : Env -> Int -> Psp -> State -> World -> World
    , crossSpecialLine : Env -> LineId -> Int -> ThinkerId -> World -> World
    , shootSpecialLine : Env -> ThinkerId -> LineId -> World -> World
    , useSpecialLine : Env -> ThinkerId -> LineId -> Int -> World -> ( Bool, World )
    }


{-| `states[s]`, with the changes G\_InitNew makes for fast monsters (the demon's
states last half as long). Use this rather than `Doom.Info.State.def`.
-}
stateDef : Env -> State -> State.Def
stateDef env s =
    let
        d =
            State.def s
    in
    if env.params.fast then
        let
            i =
                State.index s
        in
        if i >= State.index State.SargRun1 && i <= State.index State.SargPain2 then
            { d | tics = Bitwise.shiftRightBy 1 d.tics }

        else
            d

    else
        d


{-| `mobjinfo[t]`, with the changes G\_InitNew makes for fast monsters (faster imp,
cacodemon and baron shots). Use this rather than `Doom.Info.MobjType.info`.
-}
mobjInfo : Env -> MobjType -> MobjType.Info
mobjInfo env t =
    let
        i =
            MobjType.info t
    in
    if env.params.fast then
        case t of
            MobjType.Bruisershot ->
                { i | speed = 20 * Fixed.fracunit }

            MobjType.Headshot ->
                { i | speed = 20 * Fixed.fracunit }

            MobjType.Troopshot ->
                { i | speed = 20 * Fixed.fracunit }

            _ ->
                i

    else
        i


hooks : Env -> HookFunctions
hooks env =
    case env.hooks of
        Hooks h ->
            h


{-| P\_SetMobjState: put the thing in a state and run the state's action, going on to
the next state while the new one lasts 0 tics. False if the thing was removed (the
state was S\_NULL).
-}
setMobjState : Env -> ThinkerId -> State -> World -> ( Bool, World )
setMobjState env =
    (hooks env).setMobjState env


{-| P\_SetPsprite: put player n's weapon sprite in a state and run its action.
-}
setPsprite : Env -> Int -> Psp -> State -> World -> World
setPsprite env =
    (hooks env).setPsprite env


{-| P\_CrossSpecialLine: a thing crossed the line from this side (0 front, 1 back).
-}
crossSpecialLine : Env -> LineId -> Int -> ThinkerId -> World -> World
crossSpecialLine env =
    (hooks env).crossSpecialLine env


{-| P\_ShootSpecialLine: a hitscan attack hit the line.
-}
shootSpecialLine : Env -> ThinkerId -> LineId -> World -> World
shootSpecialLine env =
    (hooks env).shootSpecialLine env


{-| P\_UseSpecialLine: a thing pressed use on the line from this side. False if the line
does nothing when used.
-}
useSpecialLine : Env -> ThinkerId -> LineId -> Int -> World -> ( Bool, World )
useSpecialLine env =
    (hooks env).useSpecialLine env
