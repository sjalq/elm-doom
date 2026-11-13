module Host exposing (encodeFrame, encodeScreen, encodeCommands, decodeEvent)

{-| What the browser programs send to and take from `host.js`: frames, audio
commands (with the music already decoded from MUS), and the player's input.

@docs encodeFrame, encodeScreen, encodeCommands, decodeEvent

-}

import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Doom.Audio exposing (Command(..))
import Doom.Audio.Midi as Midi
import Doom.Audio.Mus as Mus
import Doom.Event as Event
import Doom.Screen exposing (Screen)
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import Json.Decode
import Json.Encode as E


{-| A frame for `host.js`'s `frame` port: the pixels as 16000 numbers of four
palette indices each (the first in the low byte), which crosses the port much faster
than 64000 separate numbers would.
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
    Video.toBytes pixels
        |> D.decode (D.loop ( 16000, [] ) word)
        |> Maybe.withDefault []
        |> E.list E.int


word : ( Int, List Int ) -> D.Decoder (D.Step ( Int, List Int ) (List Int))
word ( left, acc ) =
    if left <= 0 then
        D.succeed (D.Done (List.reverse acc))

    else
        D.map (\w -> D.Loop ( left - 1, w :: acc )) (D.unsignedInt32 LE)


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
