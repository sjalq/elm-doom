module ScreenCheck exposing (main)

{-| The Elm side of screen-check: runs `Doom.Intermission` or `Doom.Finale` on its own,
with the same arguments as harness.c, and writes the screen after every tic and the
sounds and music it started, in the harness's formats.

    screen-check -iwad WAD -out PREFIX -tics N [-press T,T..] [-key T,T..]
        ( -wi EPSD LAST NEXT DIDSECRET MAXK MAXI MAXS PARTIME PNUM
          [-net] [-dm] [-plyr I K IT S TIME F0 F1 F2 F3]...
        | -finale EPISODE MAP )

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Encode as E
import Doom.Defs as Defs exposing (GameMode(..))
import Doom.Event as Event
import Doom.Finale as Finale
import Doom.Info.Sfx as Sfx
import Doom.Intermission as Intermission exposing (Intermission, WbPlayer, WbStart)
import Doom.Intermission.Draw as Draw
import Doom.Play.Setup as Setup
import Doom.Sound as Sound
import Doom.Ticcmd as Ticcmd
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (World)
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


type Scenario
    = Wi WbStart
    | Fin Int Int


type alias Options =
    { wad : String
    , out : String
    , tics : Int
    , press : List Int
    , keys : List Int
    , net : Bool
    , dm : Bool
    , scenario : Maybe Scenario
    , players : List ( Int, WbPlayer )
    }


parse : List String -> Options -> Options
parse args o =
    let
        int s =
            String.toInt s |> Maybe.withDefault 0

        list s =
            String.split "," s |> List.filterMap String.toInt
    in
    case args of
        "-iwad" :: v :: rest ->
            parse rest { o | wad = v }

        "-out" :: v :: rest ->
            parse rest { o | out = v }

        "-tics" :: v :: rest ->
            parse rest { o | tics = int v }

        "-press" :: v :: rest ->
            parse rest { o | press = list v }

        "-key" :: v :: rest ->
            parse rest { o | keys = list v }

        "-net" :: rest ->
            parse rest { o | net = True }

        "-dm" :: rest ->
            parse rest { o | dm = True }

        "-wi" :: epsd :: last :: next :: didsecret :: mk :: mi :: ms :: par :: pnum :: rest ->
            parse rest
                { o
                    | scenario =
                        Just
                            (Wi
                                { epsd = int epsd
                                , didsecret = int didsecret /= 0
                                , last = int last
                                , next = int next
                                , maxkills = int mk
                                , maxitems = int mi
                                , maxsecret = int ms
                                , maxfrags = 0
                                , partime = int par
                                , pnum = int pnum
                                , plyr = Array.empty
                                }
                            )
                }

        "-plyr" :: n :: k :: it :: s :: t :: f0 :: f1 :: f2 :: f3 :: rest ->
            parse rest
                { o
                    | players =
                        o.players
                            ++ [ ( int n
                                 , { in_ = True
                                   , skills = int k
                                   , sitems = int it
                                   , ssecret = int s
                                   , stime = int t
                                   , frags = Array.fromList (List.map int [ f0, f1, f2, f3 ])
                                   }
                                 )
                               ]
                }

        "-finale" :: e :: m :: rest ->
            parse rest { o | scenario = Just (Fin (int e) (int m)) }

        _ :: rest ->
            parse rest o

        [] ->
            o


run : System.Environment -> Task Never ()
run env =
    let
        o =
            parse (List.drop 1 env.args)
                { wad = "", out = "out", tics = 100, press = [], keys = [], net = False, dm = False, scenario = Nothing, players = [] }
    in
    FileSystem.readFile (Path.fromPosixString o.wad)
        |> Task.mapError FileSystem.errorToString
        |> Task.andThen (\bytes -> Wad.fromBytes bytes |> resultToTask)
        |> Task.andThen
            (\wad ->
                case o.scenario of
                    Just scenario ->
                        let
                            ( frames, sounds ) =
                                simulate wad o scenario
                        in
                        FileSystem.writeFile (E.encode (E.sequence (List.map E.bytes frames))) (Path.fromPosixString (o.out ++ ".frames"))
                            |> Task.andThen (\_ -> FileSystem.writeFile (E.encode (E.string sounds)) (Path.fromPosixString (o.out ++ ".sounds")))
                            |> Task.map (\_ -> ())
                            |> Task.mapError FileSystem.errorToString

                    Nothing ->
                        Task.fail "no -wi or -finale"
            )
        |> Task.onError (\e -> Stream.logLine ("screen-check: " ++ e) env.stdout)


resultToTask : Result String a -> Task String a
resultToTask r =
    case r of
        Ok a ->
            Task.succeed a

        Err e ->
            Task.fail e


gameMode : Wad -> GameMode
gameMode wad =
    if Wad.checkNumForName "MAP01" wad /= Nothing then
        Commercial

    else if Wad.checkNumForName "E4M1" wad /= Nothing then
        Retail

    else if Wad.checkNumForName "E3M1" wad /= Nothing then
        Registered

    else
        Shareware


type alias Log =
    { frames : List Bytes
    , lines : List String
    , music : Maybe String
    }


logTic : Int -> Framebuffer -> List Sound.Event -> Maybe String -> Log -> Log
logTic tic fb sounds music log =
    let
        sfxLines =
            List.filterMap
                (\s ->
                    case s of
                        Sound.Start _ sfx ->
                            Just (String.fromInt tic ++ " sfx " ++ (Sfx.info sfx).name)

                        Sound.Stop _ ->
                            Nothing
                )
                sounds

        musicLines =
            if music /= log.music then
                [ String.fromInt tic ++ " music " ++ Maybe.withDefault "" music ]

            else
                []
    in
    { frames = Video.toBytes fb :: log.frames
    , lines = List.reverse (musicLines ++ sfxLines) ++ log.lines
    , music = music
    }


simulate : Wad -> Options -> Scenario -> ( List Bytes, String )
simulate wad o scenario =
    let
        mode =
            gameMode wad

        playeringame =
            Array.initialize Defs.maxPlayers (\i -> List.any (\( n, _ ) -> n == i) o.players)

        buttonsAt tic world =
            World.updatePlayer 0
                (\p ->
                    let
                        cmd =
                            p.cmd
                    in
                    { p
                        | cmd =
                            { cmd
                                | buttons =
                                    if List.member tic o.press then
                                        Ticcmd.btUse

                                    else
                                        0
                            }
                    }
                )
                world

        finish log =
            ( List.reverse log.frames, String.concat (List.map (\l -> l ++ "\n") (List.reverse log.lines)) )

        empty =
            { frames = [], lines = [], music = Nothing }
    in
    case scenario of
        Wi wbs0 ->
            let
                wbs =
                    { wbs0
                        | plyr =
                            Array.initialize Defs.maxPlayers
                                (\i ->
                                    List.filter (\( n, _ ) -> n == i) o.players
                                        |> List.head
                                        |> Maybe.map Tuple.second
                                        |> Maybe.withDefault { in_ = False, skills = 0, sitems = 0, ssecret = 0, stime = 0, frags = Array.repeat 4 0 }
                                )
                    }

                ctx =
                    { gameMode = mode, deathmatch = o.dm, netgame = o.net, playeringame = playeringame }

                newWorld =
                    Setup.newWorld playeringame

                ( wi0, rng0 ) =
                    Intermission.start ctx Intermission.initialStatics wbs newWorld.rng

                world0 =
                    { newWorld | rng = rng0 }

                loop tic wi world fb log =
                    if tic >= o.tics then
                        finish log

                    else
                        let
                            ( wi2, world2, outcome ) =
                                Intermission.ticker ctx (buttonsAt tic { world | sounds = [] }) wi

                            fb2 =
                                Draw.drawer wad wi2 fb

                            log2 =
                                logTic tic fb2 (List.reverse world2.sounds) (Intermission.music wi2 |> Maybe.map (\l -> l ++ " 1")) log
                        in
                        case outcome of
                            Intermission.WorldDone ->
                                finish log2

                            Intermission.Continue ->
                                loop (tic + 1) wi2 world2 fb2 log2
            in
            loop 0 wi0 world0 (Video.blank 0) empty

        Fin episode map ->
            let
                loop tic model fb log =
                    if tic >= o.tics then
                        finish log

                    else
                        let
                            ( afterKey, keySounds ) =
                                if List.member tic o.keys then
                                    Finale.responder (Event.KeyDown 32) model |> Maybe.withDefault ( model, [] )

                                else
                                    ( model, [] )

                            t =
                                Finale.ticker (List.member tic o.press) afterKey

                            fb2 =
                                Finale.drawer wad t.model fb

                            m =
                                Finale.music t.model

                            log2 =
                                logTic tic fb2 (keySounds ++ t.sounds) (Just (m.lump ++ " " ++ boolInt m.looping)) log
                        in
                        loop (tic + 1) t.model fb2 log2
            in
            loop 0 (Finale.start mode episode map) (Video.blank 0) empty


boolInt : Bool -> String
boolInt b =
    if b then
        "1"

    else
        "0"
