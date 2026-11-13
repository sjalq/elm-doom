module Doom.Audio.Mus exposing
    ( Song, Event(..), Timed, Controller(..), SystemEvent(..)
    , decode, toInts, ticksPerSecond
    )

{-| The music format (MUS, the D\_xxxx lumps): a header, and a stream of events on
16 channels, each followed by an optional delay, played at 140 ticks a second.
Channel 15 is percussion: its notes pick a drum, not a pitch.

    Header:  "MUS" 0x1A, then little-endian u16s: score length, score start,
             primary channels, secondary channels, instrument count, a reserved
             word, then the instrument numbers used (u16 each).

    Event:   one byte: last (bit 7), type (bits 4 to 6), channel (bits 0 to 3),
             then the type's data, then, if `last` is set, a delay in ticks
             (7 bits a byte, most significant first, bit 7 set on all but the last).

@docs Song, Event, Timed, Controller, SystemEvent
@docs decode, toInts, ticksPerSecond

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder)


{-| A decoded song: how many channels it says it uses, the instruments it lists
(GENMIDI numbers; 135 to 181 for percussion), and its events in order.
-}
type alias Song =
    { primaryChannels : Int
    , secondaryChannels : Int
    , instruments : List Int
    , events : List Timed
    }


{-| An event and how many ticks to wait after it before the next one.
-}
type alias Timed =
    { event : Event
    , delay : Int
    }


{-| What a MUS event does, on channel `channel` (0 to 15).

  - `ReleaseNote channel note`
  - `PlayNote channel note volume`: `volume` 0 to 127 if the event gives one;
    otherwise the channel's last note volume is meant.
  - `PitchBend channel bend`: 0 to 255, 128 is no bend, 0 and 255 a whole tone down
    and up (about).
  - `System channel event`, `Controller channel controller value`.
  - `MeasureEnd`: a bar line (nothing to do).
  - `ScoreEnd`: the song is over (or starts again, if it loops).

-}
type Event
    = ReleaseNote Int Int
    | PlayNote Int Int (Maybe Int)
    | PitchBend Int Int
    | System Int SystemEvent
    | Controller Int Controller Int
    | MeasureEnd
    | ScoreEnd


{-| The controllers (event type 4), with their number in the format.
-}
type Controller
    = ChangeInstrument
    | BankSelect
    | Modulation
    | Volume
    | Pan
    | Expression
    | Reverb
    | Chorus
    | Sustain
    | SoftPedal
    | UnknownController Int


{-| The system events (event type 3).
-}
type SystemEvent
    = AllSoundsOff
    | AllNotesOff
    | Mono
    | Poly
    | ResetAllControllers
    | UnknownSystem Int


{-| 140: MUS ticks per second (DMX plays Doom's music at this rate).
-}
ticksPerSecond : Int
ticksPerSecond =
    140


{-| Decode a MUS lump. Fails on a lump that isn't MUS; stops at the score's end
event, or where the lump ends (an event cut short there is dropped).
-}
decode : Bytes -> Maybe Song
decode bytes =
    D.decode header bytes
        |> Maybe.andThen
            (\h ->
                let
                    scoreBytes =
                        D.decode
                            (D.map2 (\_ b -> b)
                                (D.bytes h.scoreStart)
                                (list (Bytes.width bytes - h.scoreStart) D.unsignedInt8)
                            )
                            bytes
                in
                Maybe.map
                    (\score ->
                        { primaryChannels = h.primary
                        , secondaryChannels = h.secondary
                        , instruments = h.instruments
                        , events = parse score []
                        }
                    )
                    scoreBytes
            )


type alias Header =
    { scoreStart : Int
    , primary : Int
    , secondary : Int
    , instruments : List Int
    }


header : Decoder Header
header =
    D.unsignedInt32 LE
        |> D.andThen
            (\magic ->
                if magic /= 0x1A53554D then
                    D.fail

                else
                    D.map5
                        (\_ start primary secondary count -> { start = start, primary = primary, secondary = secondary, count = count })
                        u16
                        u16
                        u16
                        u16
                        u16
                        |> D.andThen
                            (\r ->
                                D.map2 (\_ instruments -> Header r.start r.primary r.secondary instruments)
                                    u16
                                    (list r.count u16)
                            )
            )


u16 : Decoder Int
u16 =
    D.unsignedInt16 LE


list : Int -> Decoder a -> Decoder (List a)
list n item =
    D.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\x -> D.Loop ( left - 1, x :: acc )) item
        )


{-| The score's events, from its bytes.
-}
parse : List Int -> List Timed -> List Timed
parse score acc =
    case score of
        [] ->
            List.reverse acc

        desc :: rest ->
            let
                channel =
                    Bitwise.and desc 0x0F

                last =
                    Bitwise.and desc 0x80 /= 0
            in
            case eventBody (Bitwise.and (Bitwise.shiftRightBy 4 desc) 7) channel rest of
                Nothing ->
                    List.reverse acc

                Just ( event, afterBody ) ->
                    let
                        ( d, afterDelay ) =
                            if last then
                                readDelay 0 afterBody

                            else
                                ( 0, afterBody )

                        timed =
                            { event = event, delay = d } :: acc
                    in
                    if event == ScoreEnd then
                        List.reverse timed

                    else
                        parse afterDelay timed


eventBody : Int -> Int -> List Int -> Maybe ( Event, List Int )
eventBody kind channel bytes =
    case ( kind, bytes ) of
        ( 0, n :: rest ) ->
            Just ( ReleaseNote channel (Bitwise.and n 0x7F), rest )

        ( 1, n :: rest ) ->
            if Bitwise.and n 0x80 /= 0 then
                case rest of
                    v :: rest2 ->
                        Just ( PlayNote channel (Bitwise.and n 0x7F) (Just (Bitwise.and v 0x7F)), rest2 )

                    [] ->
                        Nothing

            else
                Just ( PlayNote channel n Nothing, rest )

        ( 2, b :: rest ) ->
            Just ( PitchBend channel b, rest )

        ( 3, n :: rest ) ->
            Just ( System channel (systemEvent (Bitwise.and n 0x7F)), rest )

        ( 4, c :: v :: rest ) ->
            Just ( Controller channel (controller (Bitwise.and c 0x7F)) (Bitwise.and v 0x7F), rest )

        ( 5, _ ) ->
            Just ( MeasureEnd, bytes )

        ( 6, _ ) ->
            Just ( ScoreEnd, bytes )

        ( 7, _ ) ->
            -- unused; DMX reads no data for it
            Just ( MeasureEnd, bytes )

        _ ->
            Nothing


{-| A delay: 7 bits a byte, most significant first, bit 7 set on all but the last.
-}
readDelay : Int -> List Int -> ( Int, List Int )
readDelay acc bytes =
    case bytes of
        b :: rest ->
            let
                total =
                    acc * 128 + Bitwise.and b 0x7F
            in
            if Bitwise.and b 0x80 /= 0 then
                readDelay total rest

            else
                ( total, rest )

        [] ->
            ( acc, [] )


controller : Int -> Controller
controller n =
    case n of
        0 ->
            ChangeInstrument

        1 ->
            BankSelect

        2 ->
            Modulation

        3 ->
            Volume

        4 ->
            Pan

        5 ->
            Expression

        6 ->
            Reverb

        7 ->
            Chorus

        8 ->
            Sustain

        9 ->
            SoftPedal

        _ ->
            UnknownController n


systemEvent : Int -> SystemEvent
systemEvent n =
    case n of
        10 ->
            AllSoundsOff

        11 ->
            AllNotesOff

        12 ->
            Mono

        13 ->
            Poly

        14 ->
            ResetAllControllers

        _ ->
            UnknownSystem n


{-| The events flattened for a host (the browser's OPL player): five numbers per
event, `[type, channel, a, b, delay]`, with the MUS type numbers (0 release note
`a`; 1 play note `a` at volume `b`, or -1 for the channel's last; 2 pitch bend `a`;
3 system event `a`; 4 controller `a` set to `b`; 5 measure end; 6 score end).
-}
toInts : Song -> List Int
toInts song =
    List.concatMap
        (\{ event, delay } ->
            case event of
                ReleaseNote c n ->
                    [ 0, c, n, 0, delay ]

                PlayNote c n v ->
                    [ 1, c, n, Maybe.withDefault -1 v, delay ]

                PitchBend c b ->
                    [ 2, c, b, 0, delay ]

                System c s ->
                    [ 3, c, systemNumber s, 0, delay ]

                Controller c k v ->
                    [ 4, c, controllerNumber k, v, delay ]

                MeasureEnd ->
                    [ 5, 0, 0, 0, delay ]

                ScoreEnd ->
                    [ 6, 0, 0, 0, delay ]
        )
        song.events


controllerNumber : Controller -> Int
controllerNumber k =
    case k of
        ChangeInstrument ->
            0

        BankSelect ->
            1

        Modulation ->
            2

        Volume ->
            3

        Pan ->
            4

        Expression ->
            5

        Reverb ->
            6

        Chorus ->
            7

        Sustain ->
            8

        SoftPedal ->
            9

        UnknownController n ->
            n


systemNumber : SystemEvent -> Int
systemNumber s =
    case s of
        AllSoundsOff ->
            10

        AllNotesOff ->
            11

        Mono ->
            12

        Poly ->
            13

        ResetAllControllers ->
            14

        UnknownSystem n ->
            n
