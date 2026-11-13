module HudCheck exposing (main)

{-| Plays a demo through `Doom.Game` and, after every tic, runs the status bar and
the heads-up messages as G\_Ticker and D\_Display do (with a script of cheat keys,
full-screen and automap stretches), printing per frame

    gametic palette statusbarHash messagesHash

to compare with the C harness built from doom/oracle's sources (see
doom/tools/hud-check/harness.c, run by check.py): the status bar is drawn onto a screen kept from
frame to frame, the messages onto a cleared screen. Every 500th frame (or every
frame with --dump-all) is written as 64000 raw bytes to OUTDIR/eNNNNN.raw.

    hud-check WAD DEMO OUTDIR [--dump-all]

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Doom.Audio.Sound as Sound
import Doom.Data.Load as DataLoad
import Doom.Demo as Demo
import Doom.Event as Event
import Doom.Game as Game exposing (Game)
import Doom.Hud.Messages as Messages
import Doom.Hud.StatusBar as StatusBar
import Doom.Int32 as Int32
import Doom.Sound
import Doom.Ticcmd as Ticcmd
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World
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
        wadPath :: demo :: out :: rest ->
            FileSystem.readFile (Path.fromPosixString wadPath)
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen
                    (\bytes ->
                        case Wad.fromBytes bytes of
                            Ok wad ->
                                demoBytes wad demo
                                    |> Task.andThen
                                        (\demoData ->
                                            case ( DataLoad.fromWad wad, Demo.fromBytes demoData ) of
                                                ( Ok data, Ok d ) ->
                                                    loop env out (List.member "--dump-all" rest) (initial (List.member "--clev" rest) wad (Game.new wad data (Game.playDemo d)))

                                                ( Err e, _ ) ->
                                                    Task.fail e

                                                ( _, Err e ) ->
                                                    Task.fail e
                                        )

                            Err e ->
                                Task.fail e
                    )
                |> Task.onError (\e -> Stream.logLine ("hud-check: " ++ e) env.stderr)

        _ ->
            Stream.logLine "usage: hud-check WAD DEMO OUTDIR [--dump-all]" env.stderr


{-| A demo lump of the WAD, or else a demo file.
-}
demoBytes : Wad -> String -> Task String Bytes
demoBytes wad name =
    case Wad.lump name wad of
        Just bytes ->
            Task.succeed bytes

        Nothing ->
            FileSystem.readFile (Path.fromPosixString name)
                |> Task.mapError FileSystem.errorToString


type alias Run =
    { game : Game
    , st : StatusBar.Model
    , hu : Messages.Model
    , screen : Framebuffer
    , wasLevel : Bool
    , lastAutomap : Bool
    , clev : Bool
    , sound : Sound.Model
    }


{-| The sound settings that make `Doom.Audio.Sound` hear what the oracle's s\_sound.c
hears: the oracle runs linuxdoom's S\_Init with snd\_SfxVolume 8 unscaled, where
`Sound` scales the menu's volume by 8 as DOS does, so menu volume 1 here. Which
sounds are audible decides which draw a pitch from M\_Random, and so the face.
-}
oracleSound : Sound.Settings
oracleSound =
    { channels = 3, sfxVolume = 1, musicVolume = 8 }


initial : Bool -> Wad -> Game -> Run
initial clev wad game =
    { game = game
    , st = StatusBar.init wad
    , hu = Messages.init wad
    , screen = Video.blank 0
    , wasLevel = False
    , lastAutomap = False
    , clev = clev
    , sound = Tuple.first (Sound.init wad oracleSound)
    }


{-| The cheat keys typed at the start of these tics (as harness.c's script).
-}
script : List ( Int, String )
script =
    [ ( 60, "iddqd" )
    , ( 200, "iddqd" )
    , ( 300, "idbeholds" )
    , ( 700, "idkfa" )
    , ( 900, "idbeholdr" )
    , ( 1300, "idbeholdv" )
    , ( 1400, "idchoppers" )
    , ( 1500, "idmypos" )
    , ( 1600, "idmus12" )
    , ( 1700, "idbehold" )
    , ( 1800, "iiddqd" )
    , ( 1900, "idspispopd" )
    , ( 2000, "idclip" )
    , ( 2100, "idbeholdl" )
    , ( 2200, "idbeholda" )
    , ( 2300, "idbeholdi" )
    , ( 2400, "idmus00" )
    , ( 2500, "idfa" )
    ]


{-| With --clev (harness.c's -clev): an invalid level warp, then a valid one.
-}
clevScript : Bool -> List ( Int, String )
clevScript on =
    if on then
        [ ( 2850, "idclev51" ), ( 2900, "idclev13" ) ]

    else
        []


fullscreenAt : Int -> Bool
fullscreenAt t =
    t >= 1000 && t < 1100


automapAt : Int -> Bool
automapAt t =
    (t >= 1050 && t < 1150) || (t >= 2600 && t < 2700)


finished : Run -> Bool
finished r =
    (r.game.demo == Nothing && r.game.state == Game.DemoScreen && r.game.action == Game.NoAction)
        || (r.clev && r.game.world.gametic >= 3400)


loop : System.Environment -> String -> Bool -> Run -> Task String ()
loop env out dumpAll r =
    let
        go n acc lines dumps =
            if n <= 0 || finished acc then
                ( acc, lines, dumps )

            else
                let
                    ( next, line, dump ) =
                        step dumpAll acc
                in
                go (n - 1) next (line ++ lines) (dump ++ dumps)

        ( after, outLines, frames ) =
            go 350 r [] []

        writes =
            frames
                |> List.map
                    (\( t, fb ) ->
                        FileSystem.writeFile (Video.toBytes fb)
                            (Path.fromPosixString (out ++ "/e" ++ String.padLeft 5 '0' (String.fromInt t) ++ ".raw"))
                            |> Task.mapError FileSystem.errorToString
                    )
    in
    Task.sequence writes
        |> Task.andThen
            (\_ ->
                if List.isEmpty outLines then
                    Task.succeed ()

                else
                    Stream.logLine (String.join "\n" (List.reverse outLines)) env.stdout
            )
        |> Task.andThen
            (\_ ->
                if finished after then
                    Task.succeed ()

                else
                    loop env out dumpAll after
            )


{-| One tic: the keys, G\_Ticker (with ST\_Start/HU\_Start when the level loads,
ST\_Ticker and HU\_Ticker after the level ran), then the frame.
-}
step : Bool -> Run -> ( Run, List String, List ( Int, Framebuffer ) )
step dumpAll r0 =
    let
        pre =
            r0.game.world.gametic

        ( r1, keyLines ) =
            if r0.game.state == Game.Level && not r0.game.params.netgame then
                (script ++ clevScript r0.clev)
                    |> List.filter (\( t, _ ) -> t == pre)
                    |> List.concatMap (\( _, keys ) -> String.toList keys |> List.map Char.toCode)
                    |> (\keys ->
                            if pre == 1650 then
                                keys ++ [ Event.keyEnter ]

                            else
                                keys
                       )
                    |> List.foldl key ( r0, [] )

            else
                ( r0, [] )

        ( ticked, tic ) =
            Game.tick (Array.repeat 4 Ticcmd.empty) r1.game

        -- the tic's sounds draw their pitch from M_Random before ST_Ticker does
        ( sound, game1 ) =
            ticSounds pre tic.sounds r1.sound ticked

        body g =
            World.player (Game.consolePlayer g) g.world |> Maybe.andThen .mo

        -- P_SpawnPlayer ran for the console player: a level load, or a netgame
        -- respawn (a new body)
        loaded =
            game1.state
                == Game.Level
                && (game1.levelstarttic == game1.world.gametic - 1 || body game1 /= body r1.game)

        r2 =
            case ( loaded, Game.playing game1 ) of
                ( True, Just ( env, world ) ) ->
                    { r1
                        | game = game1
                        , sound = sound
                        , st = StatusBar.start env world (Game.consolePlayer game1) r1.st
                        , hu = Messages.start env world r1.hu
                    }

                _ ->
                    { r1 | game = game1, sound = sound }

        r3 =
            case Game.playing r2.game of
                Just ( env, world ) ->
                    let
                        ( st, w1 ) =
                            StatusBar.ticker env world (Game.consolePlayer r2.game) r2.st

                        ( hu, w2 ) =
                            Messages.ticker { showMessages = True } w1 (Game.consolePlayer r2.game) r2.hu

                        g =
                            r2.game
                    in
                    { r2 | st = st, hu = hu, game = { g | world = w2 } }

                Nothing ->
                    r2
    in
    frame dumpAll keyLines r3


{-| S\_StartSound for the tic's sounds (in a level from where they are, otherwise from
nowhere), for the M\_Random draws they make.
-}
ticSounds : Int -> List Doom.Sound.Event -> Sound.Model -> Game -> ( Sound.Model, Game )
ticSounds gametic sounds sound game =
    let
        world =
            game.world

        ( sound1, rng ) =
            case Game.playing game of
                Just ( env, _ ) ->
                    Sound.play gametic env world sounds world.rng sound
                        |> (\( s, r, _ ) -> ( s, r ))

                Nothing ->
                    List.foldl
                        (\ev ( s, r ) ->
                            case ev of
                                Doom.Sound.Start _ sfx ->
                                    Sound.startSound gametic sfx r s |> (\( s2, r2, _ ) -> ( s2, r2 ))

                                Doom.Sound.StartAt _ sfx _ ->
                                    Sound.startSound gametic sfx r s |> (\( s2, r2, _ ) -> ( s2, r2 ))

                                _ ->
                                    ( s, r )
                        )
                        ( sound, world.rng )
                        sounds
    in
    ( sound1, { game | world = { world | rng = rng } } )


key : Int -> ( Run, List String ) -> ( Run, List String )
key k ( r, lines ) =
    case Messages.responder (Event.KeyDown k) r.hu of
        Just hu ->
            ( { r | hu = hu }, lines )

        Nothing ->
            case Game.playing r.game of
                Just ( env, world ) ->
                    let
                        res =
                            StatusBar.responder (Event.KeyDown k) env world (Game.consolePlayer r.game) r.st

                        g =
                            r.game

                        ( game, more ) =
                            List.foldl
                                (\req ( gm, ls ) ->
                                    case req of
                                        StatusBar.Warp skill e m ->
                                            ( { gm | action = Game.NewGame skill e m }, ls )

                                        StatusBar.ChangeMusic name ->
                                            ( gm, ("music " ++ String.toLower name) :: ls )

                                        StatusBar.BadMusic n ->
                                            ( gm, ("music bad " ++ String.fromInt n) :: ls )
                                )
                                ( { g | world = res.world }, [] )
                                res.requests
                    in
                    ( { r | st = res.model, game = game }, more ++ lines )

                Nothing ->
                    ( r, lines )


frame : Bool -> List String -> Run -> ( Run, List String, List ( Int, Framebuffer ) )
frame dumpAll keyLines r =
    let
        isLevel =
            r.game.state == Game.Level

        wipe =
            isLevel /= r.wasLevel

        t =
            r.game.world.gametic
    in
    case ( isLevel && t /= 0, Game.playing r.game ) of
        ( True, Just ( env, world ) ) ->
            let
                am =
                    automapAt t

                st0 =
                    if am && not r.lastAutomap then
                        StatusBar.automapEntered r.st

                    else
                        r.st

                ( st, screen ) =
                    StatusBar.drawer env world (Game.consolePlayer r.game) { fullscreen = fullscreenAt t, refresh = wipe, automap = am } st0 r.screen

                huScreen =
                    Messages.drawer env am r.hu (Video.blank 0)

                hs =
                    hashRows 168 200 screen

                hh =
                    hashRows 0 168 huScreen

                line =
                    String.join " "
                        [ String.fromInt t
                        , String.fromInt (StatusBar.palette world (Game.consolePlayer r.game))
                        , String.fromInt hs
                        , String.fromInt hh
                        , "r" ++ String.fromInt world.rng.rndindex
                        ]

                dumps =
                    if dumpAll || modBy 500 t == 0 then
                        [ ( t, Video.copyRect 0 168 320 32 screen 0 168 huScreen ) ]

                    else
                        []
            in
            ( { r | st = st, screen = screen, wasLevel = isLevel, lastAutomap = am }, line :: keyLines, dumps )

        _ ->
            ( { r | wasLevel = isLevel }, keyLines, [] )


hashRows : Int -> Int -> Framebuffer -> Int
hashRows from to fb =
    List.foldl
        (\y h ->
            List.foldl (\x acc -> Int32.add (Int32.mul acc 31) (Video.pixel x y fb)) h (List.range 0 319)
        )
        0
        (List.range from (to - 1))
        |> Int32.toUnsigned
