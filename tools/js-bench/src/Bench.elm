port module Bench exposing (main)

{-| How fast the view draws compiled to JavaScript: plays the first tics of a demo
twice, once only running the game and once also drawing every frame (and turning
it into bytes, as the browser host does), and reports the difference per frame.
Run with `node run.js WAD DEMO TICS` (run.js supplies the file to elm/http).
-}

import Array
import Bytes exposing (Bytes)
import Doom.Data.Load as DataLoad
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game)
import Doom.Render.Data as RenderData exposing (RenderData)
import Doom.Render.View as View
import Doom.Ticcmd as Ticcmd
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad
import Http
import Task
import Time


port report : String -> Cmd msg


type alias Flags =
    { wad : String, demo : String, tics : Int }


type Msg
    = GotWad (Result Http.Error Bytes)
    | Timed String (Time.Posix -> ( Time.Posix, Time.Posix ) -> String) Time.Posix


main : Program Flags Flags Msg
main =
    Platform.worker
        { init = \flags -> ( flags, Http.get { url = flags.wad, expect = Http.expectBytesResponse GotWad toResult } )
        , update = update
        , subscriptions = \_ -> Sub.none
        }


toResult : Http.Response Bytes -> Result Http.Error Bytes
toResult response =
    case response of
        Http.GoodStatus_ _ body ->
            Ok body

        _ ->
            Err (Http.BadBody "no WAD")


update : Msg -> Flags -> ( Flags, Cmd Msg )
update msg flags =
    case msg of
        GotWad (Ok bytes) ->
            case Wad.fromBytes bytes |> Result.andThen (\wad -> DataLoad.fromWad wad |> Result.map (Tuple.pair wad)) of
                Ok ( wad, data ) ->
                    case Wad.lump flags.demo wad |> Result.fromMaybe "no demo" |> Result.andThen Demo.fromBytes of
                        Ok demo ->
                            let
                                game =
                                    Game.new wad data (Game.playDemo demo)
                            in
                            ( flags
                            , Time.now
                                |> Task.andThen
                                    (\t0 ->
                                        Task.succeed (run False flags.tics game (RenderData.load wad data))
                                            |> Task.andThen (\_ -> Time.now)
                                            |> Task.andThen
                                                (\t1 ->
                                                    Task.succeed (run True flags.tics game (RenderData.load wad data))
                                                        |> Task.andThen (\frames -> Time.now |> Task.map (\t2 -> ( frames, ( t0, t1 ), t2 )))
                                                )
                                    )
                                |> Task.perform
                                    (\( frames, ( t0, t1 ), t2 ) ->
                                        let
                                            sim =
                                                Time.posixToMillis t1 - Time.posixToMillis t0

                                            both =
                                                Time.posixToMillis t2 - Time.posixToMillis t1

                                            drawMs =
                                                toFloat (both - sim) / toFloat (max 1 frames)
                                        in
                                        Timed
                                            ("tics "
                                                ++ String.fromInt flags.tics
                                                ++ ", frames "
                                                ++ String.fromInt frames
                                                ++ ": game only "
                                                ++ String.fromInt sim
                                                ++ " ms, game and view "
                                                ++ String.fromInt both
                                                ++ " ms; "
                                                ++ String.fromFloat (toFloat (round (drawMs * 100)) / 100)
                                                ++ " ms a frame ("
                                                ++ String.fromInt (round (1000 / max 0.001 drawMs))
                                                ++ " frames/s)"
                                            )
                                            (\_ _ -> "")
                                            t2
                                    )
                            )

                        Err e ->
                            ( flags, report e )

                Err e ->
                    ( flags, report e )

        GotWad (Err _) ->
            ( flags, report "could not read the WAD" )

        Timed text _ _ ->
            ( flags, report text )


{-| Run tics, drawing after each level tic when asked; gives the frames drawn.
-}
run : Bool -> Int -> Game -> RenderData -> Int
run draw tics game0 rd0 =
    let
        go n game rd fb frames =
            if n <= 0 then
                frames + Video.pixel 0 0 fb * 0

            else
                let
                    ( next, _ ) =
                        Game.tick (Array.repeat 4 Ticcmd.empty) game
                in
                case ( draw, Game.playing next ) of
                    ( True, Just ( env, world ) ) ->
                        let
                            ( fb2, _, rd2 ) =
                                View.render rd env world { screenblocks = 9, detailshift = 0, player = Game.consolePlayer next } fb

                            sum =
                                Bytes.width (Video.toBytes fb2)
                        in
                        go (n - 1) next rd2 fb2 (frames + 1 + sum * 0)

                    _ ->
                        go (n - 1) next rd fb frames
    in
    go tics game0 rd0 (Video.blank 0) 0
