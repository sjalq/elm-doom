module SoundLog exposing (main)

{-| A demo's sound log, to compare with the C oracle's `-soundlog` (doom/oracle,
compare\_sounds.py): plays a demo through `Doom.Game` one tic per iteration, as the
oracle's `-full -playdemo` loop does with singletics, runs `Doom.Audio.Sound` on
every tic's sounds and once per iteration after the tic, and prints the commands in
the oracle's format:

    soundlog <wad> <demo lump>

    -1 sfxvolume 64
    0 music D_E1M6 1
    35 start 0 DSPOSIT3 6 201 124 64
    35 update 0 6 202 128
    40 stop 0

The first number is the iteration (-1 for S\_Init, before the loop). Until the
whole game (`Doom`) runs the menus, status bar and screen wipes, this stands in for
their draws from M\_Random: one a tic for ST\_Ticker while a level runs, and 320 for
each screen wipe (when the game state changes, and on every level load). A demo's
G\_InitNew clears M\_Random before anything has drawn from it, so it starts at 0.

-}

import Array
import Bytes exposing (Bytes)
import Doom.Audio exposing (Command(..))
import Doom.Audio.Sound as Sound
import Doom.Data.Load as DataLoad
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game, GameState(..))
import Doom.Random as Random exposing (Rng)
import Doom.Ticcmd as Ticcmd
import Doom.Wad as Wad
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


run : System.Environment -> Task Never ()
run env =
    case List.drop 1 env.args of
        [ wadPath, demo ] ->
            FileSystem.readFile (Path.fromPosixString wadPath)
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen (\bytes -> Task.succeed (soundLog demo bytes))
                |> Task.andThen
                    (\result ->
                        case result of
                            Ok lines ->
                                Stream.logLine (String.join "\n" lines) env.stdout

                            Err e ->
                                Task.fail e
                    )
                |> Task.onError (\e -> Stream.logLine ("soundlog: " ++ e) env.stderr)

        _ ->
            Stream.logLine "usage: soundlog <wad> <demo lump>" env.stderr


{-| The whole log for a demo.
-}
soundLog : String -> Bytes -> Result String (List String)
soundLog demoName bytes =
    Wad.fromBytes bytes
        |> Result.andThen
            (\wad ->
                DataLoad.fromWad wad
                    |> Result.andThen
                        (\data ->
                            case Wad.lump demoName wad of
                                Just demoBytes ->
                                    Demo.fromBytes demoBytes
                                        |> Result.map
                                            (\demo ->
                                                let
                                                    ( sound, initCommands ) =
                                                        Sound.init wad Sound.defaults
                                                in
                                                loop
                                                    { game = Game.new wad data (Game.playDemo demo)
                                                    , sound = sound
                                                    , rng = Random.init
                                                    , wipeState = Nothing
                                                    , iteration = 0
                                                    , lines = List.reverse (List.map (line -1) initCommands)
                                                    }
                                            )

                                Nothing ->
                                    Err ("the WAD has no " ++ demoName)
                        )
            )


type alias State =
    { game : Game
    , sound : Sound.Model
    , rng : Rng
    , wipeState : Maybe String
    , iteration : Int
    , lines : List String
    }


{-| One D\_DoomLoop iteration: G\_Ticker (with S\_Start if it loaded a level, and the
tic's sounds), S\_UpdateSounds, then D\_Display's wipe.
-}
loop : State -> List String
loop s =
    let
        tic =
            s.iteration

        ( game, ticResult ) =
            Game.tick (Array.repeat 4 Ticcmd.empty) s.game
    in
    if game.state == DemoScreen && tic > 0 then
        -- G_CheckDemoStatus: -playdemo quits here
        List.reverse s.lines

    else
        let
            loaded =
                game.levelstarttic == tic && game.state == Level && (s.game.state /= Level || s.game.levelstarttic /= tic)

            ( sound1, startCommands ) =
                case ( loaded, game.env ) of
                    ( True, Just env ) ->
                        Sound.startLevel tic env.params s.sound

                    _ ->
                        ( s.sound, [] )

            ( sound2, rng1, playCommands ) =
                case game.env of
                    Just env ->
                        Sound.play tic env game.world ticResult.sounds s.rng sound1

                    Nothing ->
                        ( sound1, s.rng, [] )

            -- ST_Ticker's M_Random
            rng2 =
                if game.state == Level then
                    Tuple.second (Random.mRandom rng1)

                else
                    rng1

            ( sound3, updateCommands ) =
                Sound.updateSounds (tic + 1) (Maybe.map (\env -> ( env, game.world )) game.env) sound2

            stateName =
                Just (stateKey game.state)

            -- D_Display: a wipe when the state changed, or a level was loaded
            wipe =
                stateName /= s.wipeState || loaded

            rng3 =
                if wipe then
                    List.foldl (\_ r -> Tuple.second (Random.mRandom r)) rng2 (List.range 1 320)

                else
                    rng2

            commands =
                startCommands ++ playCommands ++ updateCommands
        in
        loop
            { game = game
            , sound = sound3
            , rng = rng3
            , wipeState = stateName
            , iteration = tic + 1
            , lines = List.foldl (\c acc -> line tic c :: acc) s.lines commands
            }


stateKey : GameState -> String
stateKey state =
    case state of
        Level ->
            "level"

        Intermission _ ->
            "intermission"

        Finale _ ->
            "finale"

        DemoScreen ->
            "demoscreen"


line : Int -> Command -> String
line iteration command =
    String.fromInt iteration
        ++ " "
        ++ (case command of
                StartSound c ->
                    String.join " " [ "start", String.fromInt c.channel, c.lump, String.fromInt c.volume, String.fromInt c.separation, String.fromInt c.pitch, String.fromInt c.priority ]

                UpdateSound c ->
                    String.join " " [ "update", String.fromInt c.channel, String.fromInt c.volume, String.fromInt c.separation, String.fromInt c.pitch ]

                StopSound c ->
                    "stop " ++ String.fromInt c

                PlayMusic m ->
                    "music "
                        ++ m.lump
                        ++ (if m.looping then
                                " 1"

                            else
                                " 0"
                           )

                StopMusic ->
                    "musicstop"

                PauseMusic ->
                    "musicpause"

                ResumeMusic ->
                    "musicresume"

                MusicVolume v ->
                    "musicvolume " ++ String.fromInt v

                SfxVolume v ->
                    "sfxvolume " ++ String.fromInt v
           )
