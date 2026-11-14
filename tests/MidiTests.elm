module MidiTests exposing (suite)

{-| Doom.Audio.Midi on small hand-made MUS songs. The expected files were checked
against Chocolate Doom's mus2mid (by way of hosts/browser/oplmusic.js);
tools/midi-check compares every song in a WAD.
-}

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Doom.Audio.Midi as Midi
import Expect
import Test exposing (Test, describe, test)


bytes : List Int -> Bytes
bytes list =
    E.encode (E.sequence (List.map E.unsignedInt8 list))


toList : Bytes -> List Int
toList b =
    D.decode
        (D.loop ( Bytes.width b, [] )
            (\( left, acc ) ->
                if left <= 0 then
                    D.succeed (D.Done (List.reverse acc))

                else
                    D.map (\x -> D.Loop ( left - 1, x :: acc )) D.unsignedInt8
            )
        )
        b
        |> Maybe.withDefault []


{-| A MUS lump: the 16-byte header (no instruments), then the score.
-}
mus : List Int -> Bytes
mus score =
    bytes ([ 0x4D, 0x55, 0x53, 0x1A, List.length score, 0, 16, 0, 1, 0, 0, 0, 0, 0, 0, 0 ] ++ score)


{-| A MIDI file of one track holding `track`.
-}
midi : List Int -> List Int
midi track =
    [ 0x4D, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 0, 0, 1, 0, 0x46, 0x4D, 0x54, 0x72, 0x6B, 0, 0, 0, List.length track ]
        ++ track


song : Bytes
song =
    mus
        [ 0x10, 0x80 + 60, 100 -- press key 60 on channel 0, volume 100
        , 0x4F, 0, 30 -- percussion channel: instrument 30
        , 0x91, 62 -- press key 62 on channel 1, its last volume; then a delay
        , 0x81, 0x00 -- of 128 ticks
        , 0x00, 60 -- release key 60 on channel 0
        , 0x22, 0x80 -- pitch wheel centred on channel 2
        , 0x33, 11 -- all notes off on channel 3
        , 0x44, 3, 200 -- channel 4 volume 200 (over 127)
        , 0x90, 64 -- press key 64 on channel 0, its last volume (100); then
        , 0x05 -- 5 ticks
        , 0x60 -- the end
        ]


expected : List Int
expected =
    midi
        [ 0, 0xB0, 0x7B, 0 -- channel 0's first use: all notes off
        , 0, 0x90, 60, 100
        , 0, 0xC9, 30 -- MUS 15 is MIDI 9
        , 0, 0xB1, 0x7B, 0
        , 0, 0x91, 62, 127
        , 0x81, 0x00, 0x80, 60, 0
        , 0, 0xB2, 0x7B, 0
        , 0, 0xE2, 0, 64
        , 0, 0xB3, 0x7B, 0
        , 0, 0xB3, 0x7B, 0
        , 0, 0xB4, 0x7B, 0
        , 0, 0xB4, 0x07, 127
        , 0, 0x90, 64, 100
        , 5, 0xFF, 0x2F, 0
        ]


suite : Test
suite =
    describe "Doom.Audio.Midi"
        [ test "a song" <|
            \_ -> Midi.fromMus song |> Maybe.map toList |> Expect.equal (Just expected)
        , test "channel 9 is skipped: the tenth MUS channel used is MIDI's 10" <|
            \_ ->
                Midi.fromMus (mus (List.concatMap (\c -> [ c, 60 ]) (List.range 0 9) ++ [ 0x60 ]))
                    |> Maybe.map (toList >> List.drop 22 >> List.drop (9 * 8) >> List.take 8)
                    |> Expect.equal (Just [ 0, 0xBA, 0x7B, 0, 0, 0x8A, 60, 0 ])
        , test "note volumes carry over to the next song, as mus2mid.c's do" <|
            \_ ->
                let
                    ( _, after ) =
                        Midi.convert Midi.fresh song

                    ( second, _ ) =
                        Midi.convert after (mus [ 0x10, 61, 0x60 ])
                in
                second
                    |> Maybe.map toList
                    |> Expect.equal (Just (midi [ 0, 0xB0, 0x7B, 0, 0, 0x90, 61, 100, 0, 0xFF, 0x2F, 0 ]))
        , test "a fresh converter: volume 127" <|
            \_ ->
                Midi.fromMus (mus [ 0x10, 61, 0x60 ])
                    |> Maybe.map toList
                    |> Expect.equal (Just (midi [ 0, 0xB0, 0x7B, 0, 0, 0x90, 61, 127, 0, 0xFF, 0x2F, 0 ]))
        , test "long delays are variable-length quantities" <|
            \_ ->
                Midi.fromMus (mus [ 0x90, 61, 0x83, 0xFF, 0x7F, 0x60 ])
                    |> Maybe.map (toList >> List.drop 30)
                    |> Expect.equal (Just [ 0x83, 0xFF, 0x7F, 0xFF, 0x2F, 0 ])
        , test "what mus2mid.c refuses" <|
            \_ ->
                List.map Midi.fromMus
                    [ mus [ 0x50, 0x60 ] -- measure end
                    , mus [ 0x70, 0x60 ] -- the unused event type
                    , mus [ 0x30, 9, 0x60 ] -- system event 9
                    , mus [ 0x40, 10, 0, 0x60 ] -- controller 10
                    , mus [ 0x10, 60 ] -- no end
                    , mus [ 0x10 ] -- cut short
                    , bytes [ 0x4D, 0x55, 0x53, 0x1A, 0, 0, 16, 0 ] -- header cut short
                    ]
                    |> Expect.equal (List.repeat 7 Nothing)
        ]
