port module SoundTest exposing (main)

{-| A test page for the browser host without the game: the WAD's title picture, its
sound effects and its music, played through `Doom.Audio.Sound` and the host's
WebAudio player and OPL emulator.

  - S: the next sound effect (with s\_sound.c's pitch variation), from the middle.
  - Left/right arrows: the same sound, panned hard left or right.
  - M: the next song (D\_E1M1 first); Space stops the music; P pauses and resumes.
  - G: the next gamma level; the number keys 0 to 9 show palettes 0 to 9.
  - B: a benchmark frame (`host.js` times how long one round trip takes).

-}

import Array
import Browser
import Browser.Events
import Bytes exposing (Bytes)
import Doom.Audio exposing (Command(..))
import Doom.Audio.Sound as Sound
import Doom.Event as Event
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Random as Random exposing (Rng)
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import File exposing (File)
import Host
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Task


port wad : (D.Value -> msg) -> Sub msg


port event : (D.Value -> msg) -> Sub msg


port frame : E.Value -> Cmd msg


port audio : E.Value -> Cmd msg


port status : String -> Cmd msg


type alias Model =
    { loaded : Maybe Loaded
    , tic : Float
    }


type alias Loaded =
    { wad : Wad
    , title : Framebuffer
    , sound : Sound.Model
    , rng : Rng
    , sfx : Int
    , song : Int
    , paused : Bool
    , palette : Int
    , gamma : Int
    , dirty : Bool
    }


type Msg
    = GotWad D.Value
    | GotBytes Bytes
    | Input D.Value
    | Frame Float


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { loaded = Nothing, tic = 0 }, Cmd.none )
        , update = update
        , view = \_ -> Html.text ""
        , subscriptions =
            \m ->
                Sub.batch
                    [ wad GotWad
                    , event Input
                    , if m.loaded == Nothing then
                        Sub.none

                      else
                        Browser.Events.onAnimationFrameDelta Frame
                    ]
        }


songs : List String
songs =
    [ "e1m1", "e1m2", "e1m3", "e1m4", "e1m5", "e1m6", "e1m7", "e1m8", "e1m9", "inter", "intro", "victor", "bunny" ]


sounds : List Sfx
sounds =
    Array.toList Sfx.all |> List.drop 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWad value ->
            case D.decodeValue (D.field "iwad" File.decoder) value of
                Ok file ->
                    ( model, Task.perform GotBytes (File.toBytes file) )

                Err _ ->
                    ( model, status "that isn't a file" )

        GotBytes bytes ->
            case Wad.fromBytes bytes of
                Ok w ->
                    let
                        title =
                            Wad.lump "TITLEPIC" w
                                |> Maybe.andThen Video.patch
                                |> Maybe.map (\p -> Video.drawPatch 0 0 p (Video.blank 0))
                                |> Maybe.withDefault (Video.blank 0)

                        ( sound, cmds ) =
                            Sound.init w { channels = 8, sfxVolume = 15, musicVolume = 12 }
                    in
                    ( { model | loaded = Just { wad = w, title = title, sound = sound, rng = Random.init, sfx = 0, song = 0, paused = False, palette = 0, gamma = 0, dirty = True } }
                    , Cmd.batch [ audio (Host.encodeCommands w cmds), status "S sound, arrows pan, M music, Space stop, P pause, G gamma, 0-9 palette" ]
                    )

                Err e ->
                    ( model, status ("couldn't read the WAD: " ++ e) )

        Input value ->
            case ( model.loaded, Host.decodeEvent value ) of
                ( Just l, Just (Event.KeyDown k) ) ->
                    key (floor model.tic) k l |> Tuple.mapFirst (\l2 -> { model | loaded = Just l2 })

                ( Just l, Nothing ) ->
                    -- the benchmark: draw a frame now
                    ( model, frame (Host.encodeFrame l.title l.palette l.gamma) )

                _ ->
                    ( model, Cmd.none )

        Frame ms ->
            case model.loaded of
                Just l ->
                    let
                        tic =
                            model.tic + ms * 35 / 1000

                        ( sound, cmds ) =
                            Sound.updateSounds (floor tic) Nothing l.sound
                    in
                    ( { model | tic = tic, loaded = Just { l | sound = sound, dirty = False } }
                    , Cmd.batch
                        [ if l.dirty then
                            frame (Host.encodeFrame l.title l.palette l.gamma)

                          else
                            Cmd.none
                        , if List.isEmpty cmds then
                            Cmd.none

                          else
                            audio (Host.encodeCommands l.wad cmds)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )


key : Int -> Int -> Loaded -> ( Loaded, Cmd Msg )
key tic k l =
    let
        play sep =
            let
                sfx =
                    List.drop (modBy (List.length sounds) l.sfx) sounds |> List.head |> Maybe.withDefault Sfx.Pistol

                ( sound, rng, cmds ) =
                    Sound.startSound tic sfx l.rng l.sound

                panned =
                    List.map
                        (\c ->
                            case ( c, sep ) of
                                ( StartSound s, Just p ) ->
                                    StartSound { s | separation = p }

                                _ ->
                                    c
                        )
                        cmds
            in
            ( { l | sound = sound, rng = rng }
            , Cmd.batch
                [ audio (Host.encodeCommands l.wad panned)
                , status ("sound " ++ (Sfx.info sfx).name ++ " (" ++ Sound.lumpName sfx sound ++ ", " ++ String.fromInt (Sound.soundTics sfx sound) ++ " tics)")
                ]
            )
    in
    if k == Char.toCode 's' then
        play Nothing |> Tuple.mapFirst (\x -> { x | sfx = x.sfx + 1 })

    else if k == Event.keyLeftArrow then
        play (Just 0)

    else if k == Event.keyRightArrow then
        play (Just 255)

    else if k == Char.toCode 'm' then
        let
            name =
                List.drop (modBy (List.length songs) l.song) songs |> List.head |> Maybe.withDefault "e1m1"

            ( sound, cmds ) =
                Sound.changeMusic name True l.sound
        in
        ( { l | sound = sound, song = l.song + 1, paused = False }
        , Cmd.batch [ audio (Host.encodeCommands l.wad cmds), status ("music D_" ++ String.toUpper name) ]
        )

    else if k == 32 then
        let
            ( sound, cmds ) =
                Sound.stopMusic l.sound
        in
        ( { l | sound = sound }, audio (Host.encodeCommands l.wad cmds) )

    else if k == Char.toCode 'p' then
        let
            ( sound, cmds ) =
                if l.paused then
                    Sound.resumeSound l.sound

                else
                    Sound.pauseSound l.sound
        in
        ( { l | sound = sound, paused = not l.paused }, audio (Host.encodeCommands l.wad cmds) )

    else if k == Char.toCode 'g' then
        ( { l | gamma = modBy 5 (l.gamma + 1), dirty = True }, status ("gamma " ++ String.fromInt (modBy 5 (l.gamma + 1))) )

    else if k >= Char.toCode '0' && k <= Char.toCode '9' then
        ( { l | palette = k - Char.toCode '0', dirty = True }, status ("palette " ++ String.fromInt (k - Char.toCode '0')) )

    else
        ( l, Cmd.none )
