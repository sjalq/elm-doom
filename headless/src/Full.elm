module Full exposing (run)

{-| The whole program, as the C oracle's full-game mode runs it: D\_DoomLoop pass by
pass, with the events of a file posted before their pass, every frame written to a
file.

    doom-headless -full -iwad <wad> [-frames FILE] [-framesfrom N] [-framesto N]
        [-events FILE] [-iterations N] [-warp E M | -warp M] [-skill N] [-episode N]
        [-playdemo LUMP|FILE] [-nomonsters] [-respawn] [-fast] [-set NAME VALUE]...

  - `-frames FILE`: for every frame (each I\_FinishUpdate, the wipe's included) a record
    of a little-endian u32 frame number (counting from 0), a byte for the palette, a
    byte for the gamma level and the 64000 bytes of the screen. `-framesfrom` and
    `-framesto` (inclusive) write only those frames (the numbering still counts all).
  - `-events FILE`: lines `<pass> down <code>`, `<pass> up <code>` and
    `<pass> mouse <buttons> <dx> <dy>` (Doom key codes), posted before that pass (0 is
    the first) in the file's order.
  - `-iterations N`: stop after N passes; otherwise run until the game ends.
  - `-set NAME VALUE`: a configuration setting (m\_misc.c's names: screenblocks,
    detaillevel, usegamma, show\_messages, mouse\_sensitivity, sfx\_volume,
    music\_volume, snd\_channels, key\_right ... key\_speed, mouseb\_fire,
    mouseb\_strafe, mouseb\_forward).

The menu's Quit ends the run, as I\_Quit does.

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode as E
import Dict exposing (Dict)
import Doom.Audio exposing (Command(..))
import Doom.Data.Load as DataLoad
import Doom.Defs as Defs
import Doom.Event exposing (Event(..))
import Doom.Screen exposing (Screen)
import Doom.Shell as Shell exposing (Options, Shell)
import Doom.Video as Video
import Doom.Wad as Wad
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)


type alias Args =
    { iwad : Maybe String
    , frames : Maybe String
    , framesFrom : Int
    , framesTo : Int
    , events : Maybe String
    , savedir : Maybe String
    , soundlog : Maybe String
    , iterations : Maybe Int
    , playdemo : Maybe String
    , sets : List ( String, String )
    , options : Options
    }


parse : List String -> Args -> Result String Args
parse args a =
    let
        o =
            a.options

        int name s next =
            case String.toInt s of
                Just n ->
                    next n

                Nothing ->
                    Err (name ++ " needs a number, not " ++ s)
    in
    case args of
        [] ->
            Ok a

        "-full" :: rest ->
            parse rest a

        "-iwad" :: path :: rest ->
            parse rest { a | iwad = Just path }

        "-frames" :: path :: rest ->
            parse rest { a | frames = Just path }

        "-framesfrom" :: n :: rest ->
            int "-framesfrom" n (\v -> parse rest { a | framesFrom = v })

        "-framesto" :: n :: rest ->
            int "-framesto" n (\v -> parse rest { a | framesTo = v })

        "-events" :: path :: rest ->
            parse rest { a | events = Just path }

        "-savedir" :: path :: rest ->
            parse rest { a | savedir = Just path }

        "-soundlog" :: path :: rest ->
            parse rest { a | soundlog = Just path }

        "-iterations" :: n :: rest ->
            int "-iterations" n (\v -> parse rest { a | iterations = Just v })

        "-skill" :: n :: rest ->
            -- startskill = myargv[p+1][0]-'1'
            case String.uncons n of
                Just ( c, _ ) ->
                    parse rest
                        { a
                            | options =
                                { o | skill = Just (Defs.skillFromIndex (Char.toCode c - Char.toCode '1') |> Maybe.withDefault Defs.Nightmare) }
                        }

                Nothing ->
                    Err "-skill needs a number"

        "-episode" :: n :: rest ->
            int "-episode" (String.left 1 n) (\v -> parse rest { a | options = { o | episode = Just v } })

        "-warp" :: e :: m :: rest ->
            case ( String.toInt (String.left 1 e), String.toInt (String.left 1 m) ) of
                ( Just ev, Just mv ) ->
                    parse rest { a | options = { o | warp = Just ( ev, mv ) } }

                _ ->
                    -- Doom II's -warp MAP: one number
                    int "-warp" e (\v -> parse (m :: rest) { a | options = { o | warp = Just ( 1, v ) } })

        "-warp" :: m :: rest ->
            int "-warp" m (\v -> parse rest { a | options = { o | warp = Just ( 1, v ) } })

        "-playdemo" :: name :: rest ->
            parse rest { a | playdemo = Just name }

        "-nomonsters" :: rest ->
            parse rest { a | options = { o | nomonsters = True } }

        "-respawn" :: rest ->
            parse rest { a | options = { o | respawn = True } }

        "-fast" :: rest ->
            parse rest { a | options = { o | fast = True } }

        "-set" :: name :: value :: rest ->
            int ("-set " ++ name) value (\v -> setting name v o |> Result.andThen (\o2 -> parse rest { a | options = o2 }))

        other :: _ ->
            Err ("unknown option " ++ other)


setting : String -> Int -> Options -> Result String Options
setting name v o =
    let
        s =
            o.settings

        b =
            o.bindings
    in
    case name of
        "screenblocks" ->
            Ok { o | settings = { s | screenblocks = v } }

        "detaillevel" ->
            Ok { o | settings = { s | detailLevel = v } }

        "usegamma" ->
            Ok { o | settings = { s | usegamma = v } }

        "show_messages" ->
            Ok { o | settings = { s | showMessages = v /= 0 } }

        "mouse_sensitivity" ->
            Ok { o | settings = { s | mouseSensitivity = v } }

        "sfx_volume" ->
            Ok { o | settings = { s | sfxVolume = v } }

        "music_volume" ->
            Ok { o | settings = { s | musicVolume = v } }

        "snd_channels" ->
            Ok { o | soundChannels = v }

        "key_right" ->
            Ok { o | bindings = { b | right = v } }

        "key_left" ->
            Ok { o | bindings = { b | left = v } }

        "key_up" ->
            Ok { o | bindings = { b | up = v } }

        "key_down" ->
            Ok { o | bindings = { b | down = v } }

        "key_strafeleft" ->
            Ok { o | bindings = { b | strafeLeft = v } }

        "key_straferight" ->
            Ok { o | bindings = { b | strafeRight = v } }

        "key_fire" ->
            Ok { o | bindings = { b | fire = v } }

        "key_use" ->
            Ok { o | bindings = { b | use = v } }

        "key_strafe" ->
            Ok { o | bindings = { b | strafe = v } }

        "key_speed" ->
            Ok { o | bindings = { b | speed = v } }

        "mouseb_fire" ->
            Ok { o | bindings = { b | mouseFire = v } }

        "mouseb_strafe" ->
            Ok { o | bindings = { b | mouseStrafe = v } }

        "mouseb_forward" ->
            Ok { o | bindings = { b | mouseForward = v } }

        "use_mouse" ->
            Ok o

        "use_joystick" ->
            Ok o

        _ ->
            Err ("unknown setting " ++ name)


{-| Parse the events file.
-}
parseEvents : String -> Result String (Dict Int (List Event))
parseEvents text =
    String.lines text
        |> List.filter (\l -> String.trim l /= "")
        |> List.map String.words
        |> List.filter (not << List.isEmpty)
        |> List.foldl
            (\ws acc ->
                acc
                    |> Result.andThen
                        (\d ->
                            let
                                nums =
                                    List.map String.toInt ws

                                add k ev =
                                    Ok (Dict.update k (\l -> Just (Maybe.withDefault [] l ++ [ ev ])) d)
                            in
                            case ( ws, nums ) of
                                ( [ _, "down", _ ], [ Just k, _, Just c ] ) ->
                                    add k (KeyDown c)

                                ( [ _, "up", _ ], [ Just k, _, Just c ] ) ->
                                    add k (KeyUp c)

                                ( [ _, "mouse", _, _, _ ], [ Just k, _, Just bt, Just dx, Just dy ] ) ->
                                    add k (Mouse bt dx dy)

                                _ ->
                                    Err ("bad event line: " ++ String.join " " ws)
                        )
            )
            (Ok Dict.empty)


{-| The full-game mode.
-}
run : System.Environment -> List String -> Task Never ()
run env args =
    let
        initial =
            { iwad = Nothing
            , frames = Nothing
            , framesFrom = 0
            , framesTo = 0x7FFFFFFF
            , events = Nothing
            , savedir = Nothing
            , soundlog = Nothing
            , iterations = Nothing
            , playdemo = Nothing
            , sets = []
            , options = Shell.defaultOptions
            }
    in
    (case parse args initial of
        Err e ->
            Task.fail e

        Ok a ->
            case a.iwad of
                Nothing ->
                    Task.fail "-iwad <wad> is needed"

                Just wadPath ->
                    readFile wadPath
                        |> Task.andThen
                            (\bytes ->
                                case Wad.fromBytes bytes |> Result.andThen (\w -> DataLoad.fromWad w |> Result.map (Tuple.pair w)) of
                                    Ok ( wad, data ) ->
                                        Task.map3 (\evs demo slots -> ( evs, demo, slots ))
                                            (case a.events of
                                                Just path ->
                                                    readFile path
                                                        |> Task.andThen
                                                            (\eb ->
                                                                case decodeText eb |> Result.fromMaybe (path ++ ": not text") |> Result.andThen parseEvents of
                                                                    Ok d ->
                                                                        Task.succeed d

                                                                    Err e ->
                                                                        Task.fail e
                                                            )

                                                Nothing ->
                                                    Task.succeed Dict.empty
                                            )
                                            (case a.playdemo of
                                                Just name ->
                                                    case Wad.lump name wad of
                                                        Just db ->
                                                            Task.succeed (Just db)

                                                        Nothing ->
                                                            readFile name |> Task.map Just

                                                Nothing ->
                                                    Task.succeed Nothing
                                            )
                                            (readSlots a)
                                            |> Task.andThen
                                                (\( evs, demo, slots ) ->
                                                    let
                                                        ( shell, initOut ) =
                                                            Shell.init { options | playdemo = demo, quitToTitle = False, savedGames = slots } wad data

                                                        options =
                                                            a.options
                                                    in
                                                    startFrames a
                                                        |> Task.andThen (\_ -> startSoundLog a initOut.audio)
                                                        |> Task.andThen (\_ -> loop env a evs 0 0 shell)
                                                )

                                    Err problem ->
                                        Task.fail (wadPath ++ ": " ++ problem)
                            )
    )
        |> Task.onError
            (\e ->
                Stream.logLine e env.stderr
                    |> Task.andThen (\_ -> System.setExitCode 1)
            )


decodeText : Bytes -> Maybe String
decodeText b =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width b)) b


startFrames : Args -> Task String ()
startFrames a =
    case a.frames of
        Just path ->
            FileSystem.writeFile (E.encode (E.sequence [])) (Path.fromPosixString path)
                |> Task.map (\_ -> ())
                |> Task.mapError FileSystem.errorToString

        Nothing ->
            Task.succeed ()


{-| How many passes to run between writes.
-}
batch : Int
batch =
    35


loop : System.Environment -> Args -> Dict Int (List Event) -> Int -> Int -> Shell -> Task String ()
loop env a events pass frameNo shell =
    let
        done p s =
            Shell.finished s || (Maybe.map (\n -> p >= n) a.iterations |> Maybe.withDefault False)

        go n p f s acc saves sounds =
            if n <= 0 || done p s then
                ( ( p, f, s ), acc, ( saves, sounds ) )

            else
                let
                    ( s1, out ) =
                        Shell.iteration (Dict.get p events |> Maybe.withDefault []) s

                    ( s2, frames ) =
                        drainWipe s1 out.frames

                    ( f2, records ) =
                        List.foldl
                            (\screen ( fn, rs ) ->
                                ( fn + 1
                                , if fn >= a.framesFrom && fn <= a.framesTo then
                                    record fn screen :: rs

                                  else
                                    rs
                                )
                            )
                            ( f, acc )
                            frames
                in
                go (n - 1) (p + 1) f2 s2 records (saves ++ out.saved) (List.reverse (List.map (soundLine p) out.audio) ++ sounds)

        ( ( pass2, frame2, shell2 ), recs, ( saved, soundLines ) ) =
            go batch pass frameNo shell [] [] []
    in
    writeRecords a (List.reverse recs)
        |> Task.andThen (\_ -> writeSlots a saved)
        |> Task.andThen (\_ -> appendSoundLog a (List.reverse soundLines))
        |> Task.andThen
            (\_ ->
                if done pass2 shell2 then
                    Stream.logLine ("ran " ++ String.fromInt pass2 ++ " passes, " ++ String.fromInt frame2 ++ " frames") env.stderr

                else
                    loop env a events pass2 frame2 shell2
            )


drainWipe : Shell -> List Screen -> ( Shell, List Screen )
drainWipe s acc =
    if Shell.wiping s then
        let
            ( s2, out ) =
                Shell.wipeStep s
        in
        drainWipe s2 (acc ++ out.frames)

    else
        ( s, acc )


record : Int -> Screen -> Bytes
record n screen =
    E.encode
        (E.sequence
            [ E.unsignedInt32 Bytes.LE n
            , E.unsignedInt8 screen.palette
            , E.unsignedInt8 screen.gamma
            , E.bytes (Video.toBytes screen.pixels)
            ]
        )


writeRecords : Args -> List Bytes -> Task String ()
writeRecords a recs =
    case ( a.frames, recs ) of
        ( Just path, _ :: _ ) ->
            FileSystem.appendToFile (E.encode (E.sequence (List.map E.bytes recs))) (Path.fromPosixString path)
                |> Task.map (\_ -> ())
                |> Task.mapError FileSystem.errorToString

        _ ->
            Task.succeed ()


readFile : String -> Task String Bytes
readFile path =
    FileSystem.readFile (Path.fromPosixString path)
        |> Task.mapError (\err -> path ++ ": " ++ FileSystem.errorToString err)


slotPath : String -> Int -> Path.Path
slotPath dir slot =
    Path.fromPosixString (dir ++ "/doomsav" ++ String.fromInt slot ++ ".dsg")


{-| The saved games in -savedir (doomsav0.dsg to doomsav5.dsg), those there are.
-}
readSlots : Args -> Task String (Dict Int Bytes)
readSlots a =
    case a.savedir of
        Just dir ->
            List.range 0 5
                |> List.map
                    (\slot ->
                        FileSystem.readFile (slotPath dir slot)
                            |> Task.map (\b -> Just ( slot, b ))
                            |> Task.onError (\_ -> Task.succeed Nothing)
                    )
                |> Task.sequence
                |> Task.map (List.filterMap identity >> Dict.fromList)

        Nothing ->
            Task.succeed Dict.empty


writeSlots : Args -> List ( Int, Bytes ) -> Task String ()
writeSlots a saved =
    case a.savedir of
        Just dir ->
            saved
                |> List.map (\( slot, b ) -> FileSystem.writeFile b (slotPath dir slot) |> Task.map (\_ -> ()))
                |> Task.sequence
                |> Task.map (\_ -> ())
                |> Task.mapError FileSystem.errorToString

        Nothing ->
            Task.succeed ()


{-| A line of the sound log (doom/oracle/compare\_sounds.py's format): the pass (-1
before the loop) and the command.
-}
soundLine : Int -> Command -> String
soundLine pass cmd =
    let
        n =
            String.fromInt

        line =
            case cmd of
                StartSound c ->
                    String.join " " [ "start", n c.channel, c.lump, n c.volume, n c.separation, n c.pitch ]

                UpdateSound c ->
                    String.join " " [ "update", n c.channel, n c.volume, n c.separation, n c.pitch ]

                StopSound c ->
                    "stop " ++ n c

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
                    "musicvolume " ++ n v

                SfxVolume v ->
                    "sfxvolume " ++ n v
    in
    n pass ++ " " ++ line


startSoundLog : Args -> List Command -> Task String ()
startSoundLog a cmds =
    case a.soundlog of
        Just path ->
            FileSystem.writeFile (E.encode (E.string (String.concat (List.map (\c -> soundLine -1 c ++ "\n") cmds)))) (Path.fromPosixString path)
                |> Task.map (\_ -> ())
                |> Task.mapError FileSystem.errorToString

        Nothing ->
            Task.succeed ()


appendSoundLog : Args -> List String -> Task String ()
appendSoundLog a lines =
    case ( a.soundlog, lines ) of
        ( Just path, _ :: _ ) ->
            FileSystem.appendToFile (E.encode (E.string (String.concat (List.map (\l -> l ++ "\n") lines)))) (Path.fromPosixString path)
                |> Task.map (\_ -> ())
                |> Task.mapError FileSystem.errorToString

        _ ->
            Task.succeed ()
