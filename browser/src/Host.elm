module Host exposing (encodeFrame, encodeScreen, encodeCommands, decodeEvent)

{-| What the browser programs send to and take from `host.js`: frames, audio
commands (with the music already decoded from MUS), and the player's input.

@docs encodeFrame, encodeScreen, encodeCommands, decodeEvent

-}

import Bytes
import Doom.Audio exposing (Command(..))
import Doom.Audio.Midi as Midi
import Doom.Audio.Mus as Mus
import Doom.Event as Event
import Doom.Screen exposing (Screen)
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import Json.Decode
import Json.Encode as E


{-| A frame for `host.js`'s `frame` port: the 64000 palette indices as a JavaScript
array (`Doom.Video.toArray`, encoded element by element: about a millisecond, where
going through `Doom.Video.toBytes` took six).
-}
encodeFrame : Framebuffer -> Int -> Int -> E.Value
encodeFrame pixels palette gamma =
    E.object
        [ ( "pixels", packed pixels )
        , ( "palette", E.int palette )
        , ( "gamma", E.int gamma )
        ]


{-| `encodeFrame` for a `Doom.Screen.Screen`.
-}
encodeScreen : Screen -> E.Value
encodeScreen screen =
    encodeFrame screen.pixels screen.palette screen.gamma


packed : Framebuffer -> E.Value
packed pixels =
    E.array E.int (Video.toArray pixels)


{-| Audio commands for the `audio` port. `PlayMusic` carries the song, decoded
from the WAD's MUS (or MIDI) lump (`Doom.Audio.Mus.toInts`), for the OPL player.
-}
encodeCommands : Wad -> List Command -> E.Value
encodeCommands wad commands =
    E.list (encodeCommand wad) commands


encodeCommand : Wad -> Command -> E.Value
encodeCommand wad command =
    case command of
        StartSound c ->
            E.object
                [ ( "cmd", E.string "start" )
                , ( "channel", E.int c.channel )
                , ( "lump", E.string c.lump )
                , ( "volume", E.int c.volume )
                , ( "separation", E.int c.separation )
                , ( "pitch", E.int c.pitch )
                ]

        UpdateSound c ->
            E.object
                [ ( "cmd", E.string "update" )
                , ( "channel", E.int c.channel )
                , ( "volume", E.int c.volume )
                , ( "separation", E.int c.separation )
                , ( "pitch", E.int c.pitch )
                ]

        StopSound channel ->
            E.object [ ( "cmd", E.string "stop" ), ( "channel", E.int channel ) ]

        PlayMusic m ->
            E.object
                [ ( "cmd", E.string "music" )
                , ( "lump", E.string m.lump )
                , ( "looping", E.bool m.looping )
                , ( "events"
                  , Wad.lump m.lump wad
                        |> Maybe.andThen song
                        |> Maybe.map Mus.toInts
                        |> Maybe.withDefault []
                        |> E.list E.int
                  )
                ]

        StopMusic ->
            E.object [ ( "cmd", E.string "musicstop" ) ]

        PauseMusic ->
            E.object [ ( "cmd", E.string "musicpause" ) ]

        ResumeMusic ->
            E.object [ ( "cmd", E.string "musicresume" ) ]

        MusicVolume v ->
            E.object [ ( "cmd", E.string "musicvolume" ), ( "value", E.int v ) ]

        SfxVolume v ->
            E.object [ ( "cmd", E.string "sfxvolume" ), ( "value", E.int v ) ]


{-| A music lump: MUS, or a MIDI file (as some WADs, Freedoom's, have).
-}
song : Bytes.Bytes -> Maybe Mus.Song
song bytes =
    if Midi.isMidi bytes then
        Midi.decode bytes

    else
        Mus.decode bytes


{-| One of `host.js`'s input events: `{kind, data1, data2, data3}` with `kind` one
of keydown, keyup, mouse (event\_t's fields).
-}
decodeEvent : Json.Decode.Value -> Maybe Event.Event
decodeEvent value =
    let
        field name =
            Json.Decode.field name Json.Decode.int

        decoder =
            Json.Decode.map4
                (\kind d1 d2 d3 ->
                    case kind of
                        "keydown" ->
                            Just (Event.KeyDown d1)

                        "keyup" ->
                            Just (Event.KeyUp d1)

                        "mouse" ->
                            Just (Event.Mouse d1 d2 d3)

                        _ ->
                            Nothing
                )
                (Json.Decode.field "kind" Json.Decode.string)
                (field "data1")
                (field "data2")
                (field "data3")
    in
    Json.Decode.decodeValue decoder value
        |> Result.toMaybe
        |> Maybe.andThen identity
