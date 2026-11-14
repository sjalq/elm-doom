module Doom exposing
    ( Model, init, Options, defaultOptions, start
    , Msg(..), update, msPerTic, maxCatchUp
    , view, frames, settings, savedGames, saveCount, finished
    , game, wad, playing, state, error
    , menuActive, bindings, setBindings
    )

{-| Doom (linuxdoom-1.10) as an Elm program, frame for frame the original: give it a
WAD's bytes, feed it time and the player's keys and mouse, and show the screen it
gives back and play the sounds it asks for.

    import Doom
    import Doom.Event as Event

    type alias Model =
        Result String Doom.Model

    init : Bytes -> ( Model, List Doom.Audio.Command )
    init wadBytes =
        case Doom.init wadBytes of
            Ok ( doom, sounds ) ->
                ( Ok doom, sounds )

            Err problem ->
                ( Err problem, [] )

    -- with Browser.Events.onAnimationFrameDelta Frame, onKeyDown, onKeyUp:
    --     Frame ms ->    Doom.update (Doom.Tick ms) doom
    --     KeyDown code -> Doom.update (Doom.Event (Event.KeyDown code)) doom
    --     KeyUp code ->   Doom.update (Doom.Event (Event.KeyUp code)) doom
    --     MouseMoved buttons dx dy -> Doom.update (Doom.Event (Event.Mouse buttons dx dy)) doom
    -- then draw `Doom.view doom` (palette indices, the palette and the gamma level),
    -- play the `Doom.Audio.Command`s update gave back, and keep `Doom.savedGames doom`
    -- and `Doom.settings doom` for next time (hand them back in `start`'s options).

The game loop runs at 35 passes a second, whatever the frame rate: `Tick` adds up
the time that has passed and runs a pass of the original's D\_DoomLoop for every
1/35 of a second (at most `maxCatchUp` at once: after a long pause the game doesn't
race ahead; and the time a pass that starts a melt takes, loading a level, isn't
made up for). Each pass shows one frame. While the screen melts from one game state
to the next, each 1/35 of a second shows the melt's next frame instead (the original
spends its D\_Display wipe loop's frames the same way), and events wait for the
next pass. The same events at the same passes give the same frames.

The menu's Quit goes back to the title (a page can't exit); `Options.quitToTitle`
False makes it end the game (`finished`) as I\_Quit does.


# Starting

@docs Model, init, Options, defaultOptions, start


# Running

@docs Msg, update, msPerTic, maxCatchUp


# Showing it

@docs view, frames, settings, savedGames, saveCount, finished


# Reading the game

@docs game, wad, playing, state, error


# Controls

@docs menuActive, bindings, setBindings

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Doom.Audio exposing (Command)
import Doom.Data.Load as DataLoad
import Doom.Event exposing (Event)
import Doom.Game as Game exposing (Game, GameState)
import Doom.Input exposing (Bindings)
import Doom.Menu exposing (Settings)
import Doom.Play.Env exposing (Env)
import Doom.Screen exposing (Screen)
import Doom.Shell as Shell exposing (Shell)
import Doom.Wad as Wad exposing (Wad)
import Doom.World exposing (World)


{-| The running program, the events waiting for the next pass, the time that
hasn't made a whole tic yet, how many frames have been shown, and whether the last
pass started a melt (`meltStarted`: the time until the next `Tick` was spent
drawing the new screen, which the melt's clock doesn't count).
-}
type Model
    = Model
        { shell : Shell
        , events : List Event
        , pendingMs : Float
        , frames : Int
        , meltStarted : Bool
        }


{-| How to start (D\_DoomMain's command line, the configuration, the saved games):
see `Doom.Shell.Options`.
-}
type alias Options =
    Shell.Options


{-| The title and demo loop, with m\_misc.c's default settings and key bindings.
-}
defaultOptions : Options
defaultOptions =
    Shell.defaultOptions


{-| Start the WAD's game at the title, as the original does with no options. Gives
the first sound commands (the volumes, the title music comes with the first pass).
-}
init : Bytes -> Result String ( Model, List Command )
init =
    start defaultOptions


{-| Start some other way:

    Doom.start { defaultOptions | warp = Just ( 1, 3 ), skill = Just Defs.Hard } wadBytes

-}
start : Options -> Bytes -> Result String ( Model, List Command )
start options bytes =
    Wad.fromBytes bytes
        |> Result.andThen
            (\w ->
                DataLoad.fromWad w
                    |> Result.map
                        (\data ->
                            let
                                ( shell, out ) =
                                    Shell.init options w data
                            in
                            ( Model { shell = shell, events = [], pendingMs = 0, frames = 0, meltStarted = False }, out.audio )
                        )
            )


{-| What happens.

  - `Tick ms`: this many milliseconds have passed.
  - `Event`: a key went down or up, or the mouse moved or clicked (Doom key codes,
    see `Doom.Event`).
  - `SavedGames`: the host's saved game slots (0 to 5), if it has them later than
    `start`.

-}
type Msg
    = Tick Float
    | Event Event
    | SavedGames (Dict Int Bytes)


{-| How long a tic lasts: 1000 / 35 ms.
-}
msPerTic : Float
msPerTic =
    1000 / 35


{-| The most passes (or melt frames) one `Tick` runs.
-}
maxCatchUp : Int
maxCatchUp =
    4


{-| Advance the program. Gives back the sound and music commands, in order.
-}
update : Msg -> Model -> ( Model, List Command )
update msg (Model m) =
    case msg of
        Tick ms ->
            let
                -- the melt's first step comes a tic after it started, however
                -- long the pass that started it took (D_Display starts the wipe's
                -- clock once the new screen is drawn, after a level has loaded)
                pending =
                    if m.meltStarted then
                        msPerTic

                    else
                        m.pendingMs + max 0 ms

                due =
                    floor (pending / msPerTic)

                left =
                    if due > maxCatchUp then
                        0

                    else
                        pending - toFloat due * msPerTic
            in
            step (min maxCatchUp due) ( Model { m | pendingMs = left, meltStarted = False }, [] )

        Event ev ->
            ( Model { m | events = m.events ++ [ ev ] }, [] )

        SavedGames slots ->
            ( Model { m | shell = Shell.setSavedGames slots m.shell }, [] )


step : Int -> ( Model, List Command ) -> ( Model, List Command )
step n ( Model m, audio ) =
    if n <= 0 || Shell.finished m.shell then
        ( Model m, audio )

    else if Shell.wiping m.shell then
        let
            ( shell, out ) =
                Shell.wipeStep m.shell
        in
        step (n - 1) ( Model { m | shell = shell, frames = m.frames + List.length out.frames }, audio ++ out.audio )

    else
        let
            ( shell, out ) =
                Shell.iteration m.events m.shell

            next =
                { m | shell = shell, events = [], frames = m.frames + List.length out.frames }
        in
        if Shell.wiping shell then
            -- a melt started: its steps wait for the next Tick
            ( Model { next | meltStarted = True, pendingMs = 0 }, audio ++ out.audio )

        else
            step (n - 1) ( Model next, audio ++ out.audio )


{-| The screen to show now.
-}
view : Model -> Screen
view (Model m) =
    Shell.screen m.shell


{-| How many frames the program has shown so far (a pass of the loop or a step of
the melt each shows one): a host only has to draw `view` again when it changes.
-}
frames : Model -> Int
frames (Model m) =
    m.frames


{-| The settings (screen size, volumes, mouse sensitivity...) as the menu left them.
-}
settings : Model -> Settings
settings (Model m) =
    Shell.menuSettings m.shell


{-| The saved game slots, with any the player saved.
-}
savedGames : Model -> Dict Int Bytes
savedGames (Model m) =
    Shell.savedGames m.shell


{-| How many games the player has saved: store `savedGames` again when it changes
(Elm can't compare `Bytes` for equality).
-}
saveCount : Model -> Int
saveCount (Model m) =
    Shell.saveCount m.shell


{-| Whether the program has ended (a -playdemo demo is over, or Quit without
`quitToTitle`).
-}
finished : Model -> Bool
finished (Model m) =
    Shell.finished m.shell


{-| The whole game.
-}
game : Model -> Game
game (Model m) =
    Shell.game m.shell


{-| The WAD.
-}
wad : Model -> Wad
wad model =
    (game model).wad


{-| The level being played and its world, if the game is in a level.
-}
playing : Model -> Maybe ( Env, World )
playing model =
    Game.playing (game model)


{-| The game's state: a level, the intermission, the finale, or the title pages.
-}
state : Model -> GameState
state model =
    (game model).state


{-| Why the game stopped, if a level couldn't be loaded.
-}
error : Model -> Maybe String
error model =
    (game model).error


{-| Whether the menu, or one of its messages ("are you sure?"), is on the screen: a
host that captured the mouse for the game can let it go meanwhile.
-}
menuActive : Model -> Bool
menuActive (Model m) =
    Shell.menuActive m.shell


{-| The key and mouse button bindings the game reads the player's controls with
(`Doom.Input.Bindings`; `Options.bindings` at the start).
-}
bindings : Model -> Bindings
bindings (Model m) =
    Shell.bindings m.shell


{-| Change the bindings, from the next tic on, as DOS's setup program did between
games. The same bindings give the same frames.
-}
setBindings : Bindings -> Model -> Model
setBindings b (Model m) =
    Model { m | shell = Shell.setBindings b m.shell }
