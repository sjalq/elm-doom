module DemoFrames exposing (main)

{-| Frames from the real game: plays a demo through `Doom.Game`, draws the console
player's view after every level tic with `Doom.Render.View` (as the original does
with -singletics), and writes the frames asked for.

    ./demo-frames WAD DEMO OUTDIR [options] TIC...

writes OUTDIR/ticNNNNN.png for each TIC (counted as the C source's `gametic`: 1
after the first tic). Options:

  - `--blocks N`, `--detail D`: screenblocks (default 9) and detail (0 high, 1 low).
  - `--raw FILE`: also write every frame from `--first` on, every `--every`th, as
    records of a little-endian int32 gametic, int32 leveltime and the 64000 bytes of
    the screen, the format of the C frame dumper the renderer is compared with.
  - `--last N`: stop after tic N.
  - `--border`: draw the view border (R\_FillBackScreen, R\_DrawViewBorder) once.

It prints how long the frames took to draw.

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as E
import Doom.Data.Load as DataLoad
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game)
import Doom.Render.Data as RenderData exposing (RenderData)
import Doom.Render.Png as Png
import Doom.Render.View as View
import Doom.Ticcmd as Ticcmd
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)
import Time


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


type alias Opts =
    { blocks : Int
    , detail : Int
    , raw : Maybe String
    , first : Int
    , every : Int
    , last : Int
    , border : Bool
    , tics : List Int
    }


parse : List String -> Opts -> Opts
parse args opts =
    let
        int s d =
            String.toInt s |> Maybe.withDefault d
    in
    case args of
        "--blocks" :: n :: rest ->
            parse rest { opts | blocks = int n 9 }

        "--detail" :: n :: rest ->
            parse rest { opts | detail = int n 0 }

        "--raw" :: f :: rest ->
            parse rest { opts | raw = Just f }

        "--first" :: n :: rest ->
            parse rest { opts | first = int n 0 }

        "--every" :: n :: rest ->
            parse rest { opts | every = max 1 (int n 1) }

        "--last" :: n :: rest ->
            parse rest { opts | last = int n 1000000000 }

        "--border" :: rest ->
            parse rest { opts | border = True }

        t :: rest ->
            parse rest { opts | tics = List.filterMap String.toInt [ t ] ++ opts.tics }

        [] ->
            { opts | tics = List.sort opts.tics }


run : System.Environment -> Task Never ()
run env =
    case List.drop 1 env.args of
        wadPath :: demo :: out :: rest ->
            let
                opts =
                    parse rest { blocks = 9, detail = 0, raw = Nothing, first = 0, every = 1, last = 1000000000, border = False, tics = [] }
            in
            FileSystem.readFile (Path.fromPosixString wadPath)
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen
                    (\bytes ->
                        case Wad.fromBytes bytes |> Result.andThen (\wad -> DataLoad.fromWad wad |> Result.map (Tuple.pair wad)) of
                            Ok ( wad, data ) ->
                                case Wad.lump demo wad |> Result.fromMaybe ("no lump " ++ demo) |> Result.andThen Demo.fromBytes of
                                    Ok d ->
                                        let
                                            last =
                                                min opts.last
                                                    (Maybe.withDefault opts.last
                                                        (List.maximum opts.tics
                                                            |> Maybe.map
                                                                (\m ->
                                                                    if opts.raw == Nothing then
                                                                        m

                                                                    else
                                                                        opts.last
                                                                )
                                                        )
                                                    )
                                        in
                                        Time.now
                                            |> Task.andThen
                                                (\t0 ->
                                                    loop env out opts { opts | last = last } (Game.new wad data (Game.playDemo d)) (RenderData.load wad data) (Video.blank 0) 0 [] 0
                                                        |> Task.andThen
                                                            (\( frames, drawMs ) ->
                                                                Time.now
                                                                    |> Task.andThen
                                                                        (\t1 ->
                                                                            say env
                                                                                (String.fromInt frames
                                                                                    ++ " frames drawn in "
                                                                                    ++ String.fromInt drawMs
                                                                                    ++ " ms ("
                                                                                    ++ fps frames drawMs
                                                                                    ++ " frames/s); whole run "
                                                                                    ++ String.fromInt (Time.posixToMillis t1 - Time.posixToMillis t0)
                                                                                    ++ " ms"
                                                                                )
                                                                        )
                                                            )
                                                )

                                    Err e ->
                                        Task.fail e

                            Err e ->
                                Task.fail e
                    )
                |> Task.onError (\e -> say env ("demo-frames: " ++ e))

        _ ->
            say env "usage: demo-frames WAD DEMO OUTDIR [--blocks N] [--detail D] [--raw FILE [--first N] [--every K]] [--last N] [--border] TIC..."


fps : Int -> Int -> String
fps frames ms =
    if ms <= 0 then
        "?"

    else
        String.fromInt (frames * 1000 // ms)


say : System.Environment -> String -> Task x ()
say env msg =
    Stream.logLine msg env.stdout


noCmds : Array Ticcmd.Ticcmd
noCmds =
    Array.repeat 4 Ticcmd.empty


{-| Run tics, drawing after each one in a level; `raw` collects the frame records
(newest first) until they are written out.
-}
loop : System.Environment -> String -> Opts -> Opts -> Game -> RenderData -> Framebuffer -> Int -> List Bytes -> Int -> Task String ( Int, Int )
loop env out opts0 opts game rd fb tic raw drawMs =
    if tic >= opts.last || game.error /= Nothing || (game.demo == Nothing && game.state == Game.DemoScreen && game.action == Game.NoAction) then
        writeRaw opts raw |> Task.map (\_ -> ( List.length raw, drawMs ))

    else
        let
            ( next, _ ) =
                Game.tick noCmds game

            gametic =
                tic + 1
        in
        case Game.playing next of
            Nothing ->
                loop env out opts0 opts next rd fb gametic raw drawMs

            Just ( gameEnv, world ) ->
                Time.now
                    |> Task.andThen
                        (\t0 ->
                            let
                                bordered =
                                    if opts.border && tic == 0 then
                                        View.drawBorder rd opts.blocks fb

                                    else
                                        fb

                                ( fb2, _, rd2 ) =
                                    View.render rd gameEnv world { screenblocks = opts.blocks, detailshift = opts.detail, player = Game.consolePlayer next } bordered

                                screen =
                                    Video.toBytes fb2
                            in
                            Time.now
                                |> Task.andThen
                                    (\t1 ->
                                        let
                                            ms =
                                                drawMs + Time.posixToMillis t1 - Time.posixToMillis t0

                                            raw2 =
                                                if opts.raw /= Nothing && gametic >= opts.first && modBy opts.every (gametic - opts.first) == 0 then
                                                    E.encode (E.sequence [ E.signedInt32 LE gametic, E.signedInt32 LE world.leveltime, E.bytes screen ]) :: raw

                                                else
                                                    raw

                                            png =
                                                if List.member gametic opts.tics then
                                                    let
                                                        palette =
                                                            Wad.lump "PLAYPAL" game.wad |> Maybe.map Png.playpal |> Maybe.andThen (Array.get 0) |> Maybe.withDefault Array.empty

                                                        path =
                                                            out ++ "/tic" ++ String.padLeft 5 '0' (String.fromInt gametic) ++ ".png"
                                                    in
                                                    FileSystem.writeFile (Png.encode palette { width = 320, height = 200, pixels = pixels fb2 }) (Path.fromPosixString path)
                                                        |> Task.mapError FileSystem.errorToString
                                                        |> Task.andThen (\_ -> say env ("wrote " ++ path))

                                                else
                                                    Task.succeed ()
                                        in
                                        png |> Task.andThen (\_ -> loop env out opts0 opts next rd2 fb2 gametic raw2 ms)
                                    )
                        )


pixels : Framebuffer -> Array Int
pixels fb =
    Array.initialize (320 * 200) (\i -> Video.pixel (modBy 320 i) (i // 320) fb)


writeRaw : Opts -> List Bytes -> Task String ()
writeRaw opts raw =
    case opts.raw of
        Just file ->
            FileSystem.writeFile (E.encode (E.sequence (List.map E.bytes (List.reverse raw)))) (Path.fromPosixString file)
                |> Task.mapError FileSystem.errorToString
                |> Task.map (\_ -> ())

        Nothing ->
            Task.succeed ()
