module AudioTests exposing (suite)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as E
import Doom.Audio.Midi as Midi
import Doom.Audio.Mus as Mus exposing (Controller(..), Event(..))
import Doom.Audio.Sound as Sound
import Doom.Defs exposing (GameMode(..))
import Doom.Info.Sfx as Sfx
import Doom.Sound
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "audio"
        [ musTests
        , midiTests
        , musicChoice
        , test "pitch draws: every sound but itemup and tink" <|
            \_ ->
                List.map Doom.Sound.drawsPitch [ Sfx.Pistol, Sfx.Itemup, Sfx.Tink, Sfx.Sawup ]
                    |> Expect.equal [ True, False, False, True ]
        ]


bytes : List Int -> Bytes
bytes list =
    E.encode (E.sequence (List.map E.unsignedInt8 list))


u16 : Int -> List Int
u16 n =
    [ modBy 256 n, n // 256 ]


{-| A MUS lump: header with one instrument, then the score.
-}
mus : List Int -> Bytes
mus score =
    let
        start =
            16 + 2
    in
    bytes
        ([ 0x4D, 0x55, 0x53, 0x1A ]
            ++ u16 (List.length score)
            ++ u16 start
            ++ u16 1
            ++ u16 0
            ++ u16 1
            ++ u16 0
            ++ u16 30
            ++ score
        )


musTests : Test
musTests =
    describe "MUS"
        [ test "header and events, with delays" <|
            \_ ->
                mus
                    [ 0x40
                    , 0x00
                    , 0x1E -- controller 0 (instrument) on channel 0: 30
                    , 0x90
                    , 0xBC
                    , 0x64
                    , 0x81
                    , 0x00 -- play note 60 at 100, last: delay 128
                    , 0x10
                    , 0x3C -- play note 60, last volume
                    , 0x8F
                    , 0x23
                    , 0x05 -- release note 35 on channel 15, delay 5
                    , 0x60 -- score end
                    ]
                    |> Mus.decode
                    |> Expect.equal
                        (Just
                            { primaryChannels = 1
                            , secondaryChannels = 0
                            , instruments = [ 30 ]
                            , events =
                                [ { event = Controller 0 ChangeInstrument 30, delay = 0 }
                                , { event = PlayNote 0 60 (Just 100), delay = 128 }
                                , { event = PlayNote 0 60 Nothing, delay = 0 }
                                , { event = ReleaseNote 15 35, delay = 5 }
                                , { event = ScoreEnd, delay = 0 }
                                ]
                            }
                        )
        , test "not MUS" <|
            \_ ->
                Mus.decode (bytes [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ])
                    |> Expect.equal Nothing
        , test "flattened for the host" <|
            \_ ->
                mus [ 0x20, 0x80, 0x60 ]
                    |> Mus.decode
                    |> Maybe.map Mus.toInts
                    |> Expect.equal (Just [ 2, 0, 128, 0, 0, 6, 0, 0, 0, 0 ])
        ]


be32 : Int -> List Int
be32 n =
    [ n // 16777216, modBy 256 (n // 65536), modBy 256 (n // 256), modBy 256 n ]


midiTests : Test
midiTests =
    describe "MIDI"
        [ test "a format-0 file: tempo, running status, percussion channel, times at 140 Hz" <|
            \_ ->
                let
                    -- 96 ticks a quarter; tempo 500000 us a quarter (120 bpm)
                    track =
                        [ 0x00
                        , 0xFF
                        , 0x51
                        , 0x03
                        , 0x07
                        , 0xA1
                        , 0x20 -- tempo
                        , 0x00
                        , 0x99
                        , 0x24
                        , 0x7F -- note on, MIDI channel 10 (percussion)
                        , 0x60
                        , 0x24
                        , 0x00 -- a quarter later (0.5 s), running status, velocity 0: off
                        , 0x00
                        , 0xB0
                        , 0x07
                        , 0x50 -- channel 1 volume 80
                        , 0x00
                        , 0xFF
                        , 0x2F
                        , 0x00
                        ]

                    file =
                        [ 0x4D, 0x54, 0x68, 0x64 ]
                            ++ be32 6
                            ++ [ 0, 0, 0, 1, 0, 96 ]
                            ++ [ 0x4D, 0x54, 0x72, 0x6B ]
                            ++ be32 (List.length track)
                            ++ track
                in
                bytes file
                    |> Midi.decode
                    |> Maybe.map .events
                    |> Expect.equal
                        (Just
                            [ { event = PlayNote 15 36 (Just 127), delay = 70 }
                            , { event = ReleaseNote 15 36, delay = 0 }
                            , { event = Controller 0 Volume 80, delay = 0 }
                            , { event = ScoreEnd, delay = 0 }
                            ]
                        )
        , test "recognised by its header" <|
            \_ ->
                ( Midi.isMidi (bytes [ 0x4D, 0x54, 0x68, 0x64, 0 ]), Midi.isMidi (bytes [ 0x4D, 0x55, 0x53, 0x1A ]) )
                    |> Expect.equal ( True, False )
        ]


musicChoice : Test
musicChoice =
    describe "S_Start's music"
        [ test "Doom episodes" <|
            \_ ->
                [ Sound.levelMusic Retail 1 1, Sound.levelMusic Registered 3 9, Sound.levelMusic Shareware 2 4 ]
                    |> Expect.equal [ Just "e1m1", Just "e3m9", Just "e2m4" ]
        , test "The Ultimate Doom's fourth episode borrows" <|
            \_ ->
                [ Sound.levelMusic Retail 4 1, Sound.levelMusic Retail 4 9 ]
                    |> Expect.equal [ Just "e3m4", Just "e1m9" ]
        , test "Doom II from RUNNIN on" <|
            \_ ->
                [ Sound.levelMusic Commercial 1 1, Sound.levelMusic Commercial 1 30, Sound.levelMusic Commercial 1 32 ]
                    |> Expect.equal [ Just "runnin", Just "openin", Just "ultima" ]
        ]
