module Doom exposing
    ( Model, init, Start(..), start, fromWad
    , Msg(..), Key(..), update, msPerTic
    , game, wad, playing, state, error
    )

{-| Doom (linuxdoom-1.10) as an Elm program: give it a WAD's bytes, feed it time and
key presses, and read the game back out to draw it and play its sounds.

    import Doom

    type alias Model =
        Result String Doom.Model

    init : Bytes -> Model
    init wadBytes =
        Doom.init wadBytes

    -- in update, with Browser.Events.onAnimationFrameDelta Frame:
    --     Frame ms -> Doom.update (Doom.Tick ms) doom
    --     KeyPressed k -> Doom.update (Doom.KeyDown k) doom
    -- update gives back the new model and the sounds to play.

The game runs at 35 tics a second, whatever the frame rate: `Tick` adds up the time
that has passed and runs a tic for every 1/35 of a second, catching up at most
`maxCatchUp` tics at once (after a long pause the game doesn't race ahead).


# Starting

@docs Model, init, Start, start, fromWad


# Running

@docs Msg, Key, update, msPerTic


# Reading the game

@docs game, wad, playing, state, error

-}

import Array
import Bytes exposing (Bytes)
import Doom.Data exposing (Data)
import Doom.Data.Load as DataLoad
import Doom.Defs exposing (Skill(..))
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game, GameState)
import Doom.Input as Input exposing (Input)
import Doom.Play.Env exposing (Env)
import Doom.Sound as Sound
import Doom.Ticcmd as Ticcmd exposing (Ticcmd)
import Doom.Wad as Wad exposing (Wad)
import Doom.World exposing (World)


{-| A running game and the controls being held.
-}
type Model
    = Model
        { game : Game
        , held : List Key
        , mouseX : Int
        , turnheld : Input.Turning
        , sendpause : Bool
        , pendingMs : Float
        }


{-| What to start with: a new game at a skill, episode and map, or a demo (the name
of a demo lump in the WAD, such as "DEMO1").
-}
type Start
    = NewGame Skill Int Int
    | Demo String


{-| A new game on Medium ("Hurt me plenty"), at the first map (E1M1, or MAP01 in
Doom II).
-}
init : Bytes -> Result String Model
init =
    start (NewGame Medium 1 1)


{-| Start the WAD's game some other way:

    Doom.start (Doom.Demo "DEMO1") wadBytes

    Doom.start (Doom.NewGame Defs.Hard 2 3) wadBytes

-}
start : Start -> Bytes -> Result String Model
start what bytes =
    Wad.fromBytes bytes
        |> Result.andThen (\w -> DataLoad.fromWad w |> Result.andThen (fromWad what w))


{-| Start a game from a WAD already read, and its data.
-}
fromWad : Start -> Wad -> Data -> Result String Model
fromWad what w data =
    let
        action =
            case what of
                NewGame skill episode map ->
                    Ok (Game.newGame skill episode map)

                Demo name ->
                    case Wad.lump name w of
                        Just bytes ->
                            Demo.fromBytes bytes |> Result.map Game.playDemo

                        Nothing ->
                            Err ("the WAD has no demo " ++ name)
    in
    action
        |> Result.map
            (\a ->
                Model
                    { game = Game.new w data a
                    , held = []
                    , mouseX = 0
                    , turnheld = Input.notTurning
                    , sendpause = False
                    , pendingMs = 0
                    }
            )


{-| What happens to the game.

  - `Tick ms`: this many milliseconds have passed.
  - `KeyDown`, `KeyUp`: a game control was pressed or let go.
  - `MouseMoved dx`: the mouse moved sideways (positive is right), turning (or
    strafing while `Strafe` is held).

-}
type Msg
    = Tick Float
    | KeyDown Key
    | KeyUp Key
    | MouseMoved Int


{-| The game's controls. `WeaponKey n` is the number key n (1 to 8). `Strafe` turns
the turning keys into strafing; `Run` is the speed key; `Pause` pauses and unpauses.
-}
type Key
    = Forward
    | Backward
    | TurnLeft
    | TurnRight
    | StrafeLeft
    | StrafeRight
    | Strafe
    | Run
    | Fire
    | Use
    | WeaponKey Int
    | Pause


{-| How long a tic lasts: 1000 / 35 ms.
-}
msPerTic : Float
msPerTic =
    1000 / 35


maxCatchUp : Int
maxCatchUp =
    4


{-| Advance the game. Gives back the sounds the tics run asked for, in order.
-}
update : Msg -> Model -> ( Model, List Sound.Event )
update msg (Model m) =
    case msg of
        Tick ms ->
            let
                pending =
                    m.pendingMs + max 0 ms

                due =
                    floor (pending / msPerTic)

                runs =
                    min maxCatchUp due

                left =
                    if due > maxCatchUp then
                        0

                    else
                        pending - toFloat due * msPerTic
            in
            runTics runs ( Model { m | pendingMs = left }, [] )

        KeyDown Pause ->
            ( Model { m | sendpause = True }, [] )

        KeyDown key ->
            ( Model
                { m
                    | held =
                        if List.member key m.held then
                            m.held

                        else
                            key :: m.held
                }
            , []
            )

        KeyUp key ->
            ( Model { m | held = List.filter ((/=) key) m.held }, [] )

        MouseMoved dx ->
            ( Model { m | mouseX = m.mouseX + dx }, [] )


runTics : Int -> ( Model, List Sound.Event ) -> ( Model, List Sound.Event )
runTics n ( Model m, sounds ) =
    if n <= 0 then
        ( Model m, sounds )

    else
        let
            ( cmd, turnheld ) =
                Input.buildTiccmd (input m.held m.mouseX) m.turnheld

            withPause =
                if m.sendpause then
                    { cmd | buttons = Ticcmd.btSpecial + Ticcmd.btsPause }

                else
                    cmd

            ( g, tic ) =
                Game.tick (netcmds (Game.consolePlayer m.game) withPause) m.game
        in
        runTics (n - 1)
            ( Model { m | game = g, mouseX = 0, turnheld = turnheld, sendpause = False }
            , sounds ++ tic.sounds
            )


netcmds : Int -> Ticcmd -> Array.Array Ticcmd
netcmds console cmd =
    Array.initialize 4
        (\i ->
            if i == console then
                cmd

            else
                Ticcmd.empty
        )


input : List Key -> Int -> Input
input held mouseX =
    let
        is k =
            List.member k held
    in
    { forward = is Forward
    , backward = is Backward
    , turnLeft = is TurnLeft
    , turnRight = is TurnRight
    , strafeLeft = is StrafeLeft
    , strafeRight = is StrafeRight
    , strafe = is Strafe
    , run = is Run
    , fire = is Fire
    , use = is Use
    , weapon =
        -- the lowest weapon key held wins, as G_BuildTiccmd checks '1' first
        List.range 1 8 |> List.filter (\n -> is (WeaponKey n)) |> List.head
    , mouseX = mouseX
    }


{-| The whole game, for anything the other accessors don't give.
-}
game : Model -> Game
game (Model m) =
    m.game


{-| The WAD the game runs from (for the renderer's graphics).
-}
wad : Model -> Wad
wad (Model m) =
    m.game.wad


{-| The level being played and its world (with `Doom.Game.consolePlayer`'s view), if
the game is in a level.
-}
playing : Model -> Maybe ( Env, World )
playing (Model m) =
    Game.playing m.game


{-| Whether the game is in a level, the intermission (with its counters), the finale,
or not running.
-}
state : Model -> GameState
state (Model m) =
    m.game.state


{-| Why the game stopped, if a level couldn't be loaded.
-}
error : Model -> Maybe String
error (Model m) =
    m.game.error
