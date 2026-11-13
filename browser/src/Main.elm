port module Main exposing (main)

{-| Doom in the browser: runs the `Doom` program on the WAD `host.js` hands over,
a pass of the game loop for every 1/35 of a second of animation frames, and sends
`host.js` each frame, the sounds and music, and the settings and saved games to keep
in the browser's storage (handed back in the flags next time).

Flags: everything stored under "doom:" in localStorage, as strings: "settings"
(JSON) and "save0" to "save5" (the saved games, base 64).

-}

import Base64
import Browser
import Browser.Events
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Doom
import Doom.Audio exposing (Command)
import Doom.Menu exposing (Settings)
import File
import Host
import Html
import Json.Decode as D
import Json.Encode as E
import Task


port wad : (D.Value -> msg) -> Sub msg


port event : (D.Value -> msg) -> Sub msg


port frame : E.Value -> Cmd msg


port audio : E.Value -> Cmd msg


port status : String -> Cmd msg


port store : { key : String, value : Maybe String } -> Cmd msg


type Model
    = Waiting Stored
    | Running Game
    | Failed


type alias Stored =
    { settings : Maybe Settings
    , savedGames : Dict Int Bytes
    }


type alias Game =
    { doom : Doom.Model
    , settings : Settings
    , savedGames : Dict Int Bytes
    }


type Msg
    = GotWad D.Value
    | GotBytes Bytes
    | Input D.Value
    | Frame Float


main : Program D.Value Model Msg
main =
    Browser.element
        { init = \flags -> ( Waiting (fromFlags flags), Cmd.none )
        , update = update
        , view = \_ -> Html.text ""
        , subscriptions =
            \model ->
                case model of
                    Running _ ->
                        Sub.batch [ event Input, Browser.Events.onAnimationFrameDelta Frame ]

                    _ ->
                        wad GotWad
        }


fromFlags : D.Value -> Stored
fromFlags flags =
    let
        stored =
            D.decodeValue (D.dict D.string) flags |> Result.withDefault Dict.empty

        saves =
            List.range 0 5
                |> List.filterMap
                    (\slot ->
                        Dict.get ("save" ++ String.fromInt slot) stored
                            |> Maybe.andThen Base64.toBytes
                            |> Maybe.map (Tuple.pair slot)
                    )
                |> Dict.fromList
    in
    { settings = Dict.get "settings" stored |> Maybe.andThen (D.decodeString settingsDecoder >> Result.toMaybe)
    , savedGames = saves
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotWad value, Waiting _ ) ->
            case D.decodeValue File.decoder value of
                Ok file ->
                    ( model, Task.perform GotBytes (File.toBytes file) )

                Err _ ->
                    ( Failed, status "That isn't a file." )

        ( GotBytes bytes, Waiting stored ) ->
            let
                options =
                    Doom.defaultOptions

                withSettings =
                    case stored.settings of
                        Just s ->
                            { options | settings = s, savedGames = stored.savedGames }

                        Nothing ->
                            { options | savedGames = stored.savedGames }
            in
            case Doom.start withSettings bytes of
                Ok ( doom, commands ) ->
                    ( Running { doom = doom, settings = Doom.settings doom, savedGames = Doom.savedGames doom }
                    , Cmd.batch [ sounds doom commands, status help ]
                    )

                Err problem ->
                    ( Failed, status ("Couldn't start: " ++ problem) )

        ( Input value, Running game ) ->
            case Host.decodeEvent value of
                Just e ->
                    step (Doom.Event e) game

                Nothing ->
                    ( model, Cmd.none )

        ( Frame ms, Running game ) ->
            let
                ( next, cmd ) =
                    step (Doom.Tick ms) game
            in
            case next of
                Running g ->
                    ( next, Cmd.batch [ cmd, frame (Host.encodeScreen (Doom.view g.doom)) ] )

                _ ->
                    ( next, cmd )

        _ ->
            ( model, Cmd.none )


help : String
help =
    "Arrows move, Ctrl fires, Space opens, Shift runs, Alt strafes, 1-7 weapons, Tab map, Esc menu. Click for the mouse."


step : Doom.Msg -> Game -> ( Model, Cmd Msg )
step msg game =
    let
        ( doom, commands ) =
            Doom.update msg game.doom

        settings =
            Doom.settings doom

        savedGames =
            Doom.savedGames doom

        storeSettings =
            if settings /= game.settings then
                [ store { key = "settings", value = Just (E.encode 0 (encodeSettings settings)) } ]

            else
                []

        storeGames =
            if savedGames /= game.savedGames then
                List.range 0 5
                    |> List.filterMap
                        (\slot ->
                            let
                                now =
                                    Dict.get slot savedGames
                            in
                            if now /= Dict.get slot game.savedGames then
                                Just (store { key = "save" ++ String.fromInt slot, value = Maybe.map Base64.fromBytes now })

                            else
                                Nothing
                        )

            else
                []
    in
    ( Running { doom = doom, settings = settings, savedGames = savedGames }
    , Cmd.batch (sounds doom commands :: storeSettings ++ storeGames)
    )


sounds : Doom.Model -> List Command -> Cmd msg
sounds doom commands =
    if List.isEmpty commands then
        Cmd.none

    else
        audio (Host.encodeCommands (Doom.wad doom) commands)


encodeSettings : Settings -> E.Value
encodeSettings s =
    E.object
        [ ( "mouseSensitivity", E.int s.mouseSensitivity )
        , ( "showMessages", E.bool s.showMessages )
        , ( "detailLevel", E.int s.detailLevel )
        , ( "screenblocks", E.int s.screenblocks )
        , ( "sfxVolume", E.int s.sfxVolume )
        , ( "musicVolume", E.int s.musicVolume )
        , ( "usegamma", E.int s.usegamma )
        ]


settingsDecoder : D.Decoder Settings
settingsDecoder =
    D.map7 Settings
        (D.field "mouseSensitivity" D.int)
        (D.field "showMessages" D.bool)
        (D.field "detailLevel" D.int)
        (D.field "screenblocks" D.int)
        (D.field "sfxVolume" D.int)
        (D.field "musicVolume" D.int)
        (D.field "usegamma" D.int)
