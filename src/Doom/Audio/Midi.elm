module Doom.Audio.Midi exposing
    ( Converter, fresh, convert
    , fromMus
    )

{-| Doom's music (MUS lumps, see `Doom.Audio.Mus`) as Standard MIDI Files, converted
byte for byte as Chocolate Doom's mus2mid.c converts them, so a MIDI synthesizer that
plays like Chocolate's OPL music driver (Elm OS's host/synth) plays Doom's music as
Chocolate Doom does.

The file is format 0 with one track at 70 ticks a beat; at MIDI's default 120 beats a
minute that is MUS's 140 ticks a second, and there are no tempo events. MUS channels
get MIDI channels in the order they are first used, skipping 9 (MIDI's percussion
channel, which MUS channel 15 becomes), each with an "all notes off" when it is first
used. A note without a volume has its channel's last one; controller values over 127
become 127.

mus2mid.c keeps two things in file-level variables that carry over from one song to
the next: each MIDI channel's last note volume (127 to begin with), and the time not
yet written out (if a conversion fails part way). Chocolate converts a song each time
it is played, so a song's MIDI can depend on the songs converted before it. A
`Converter` is those two things: to be identical to Chocolate over a whole game, keep
one and convert every song with it; `fromMus` converts as the first song of a game.

@docs Converter, fresh, convert
@docs fromMus

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E


{-| mus2mid.c's state between conversions.
-}
type Converter
    = Converter
        { velocities : Array Int
        , queuedTime : Int
        }


{-| The state before the first conversion: every channel's note volume 127.
-}
fresh : Converter
fresh =
    Converter { velocities = Array.repeat 16 127, queuedTime = 0 }


{-| Convert a MUS lump. `Nothing` where mus2mid.c fails (the lump is too short or
cut short, or has an event it doesn't know: a measure end, an unused event type,
a system event outside 10 to 14 or a controller outside 0 to 9). The header's
"MUS" signature isn't checked, as Chocolate doesn't check it. The converter comes
back changed even when the conversion fails, as mus2mid.c's variables do.
-}
convert : Converter -> Bytes -> ( Maybe Bytes, Converter )
convert (Converter state) lump =
    let
        bytes =
            toArray lump

        scoreStart =
            byteAt 6 bytes + 256 * byteAt 7 bytes

        start =
            { pos = scoreStart
            , out = []
            , trackSize = 0
            , channelMap = Array.repeat 16 -1
            , velocities = state.velocities
            , queuedTime = state.queuedTime
            }

        after s =
            Converter { velocities = s.velocities, queuedTime = s.queuedTime }
    in
    if Array.length bytes < 14 || scoreStart >= Array.length bytes then
        ( Nothing, Converter state )

    else
        case events bytes start of
            Ok s ->
                ( Just (file s), after s )

            Err s ->
                ( Nothing, after s )


{-| Convert a MUS lump with a `fresh` converter.
-}
fromMus : Bytes -> Maybe Bytes
fromMus lump =
    Tuple.first (convert fresh lump)


type alias State =
    { pos : Int
    , out : List Int -- the track's bytes, last first
    , trackSize : Int
    , channelMap : Array Int
    , velocities : Array Int
    , queuedTime : Int
    }


toArray : Bytes -> Array Int
toArray bytes =
    D.decode
        (D.loop ( Bytes.width bytes, [] )
            (\( left, acc ) ->
                if left <= 0 then
                    D.succeed (D.Done (Array.fromList (List.reverse acc)))

                else
                    D.map (\b -> D.Loop ( left - 1, b :: acc )) D.unsignedInt8
            )
        )
        bytes
        |> Maybe.withDefault Array.empty


byteAt : Int -> Array Int -> Int
byteAt i bytes =
    Array.get i bytes |> Maybe.withDefault 0


{-| The next byte of the lump, if there is one.
-}
read : Array Int -> State -> Maybe ( Int, State )
read bytes s =
    Array.get s.pos bytes |> Maybe.map (\b -> ( b, { s | pos = s.pos + 1 } ))


{-| The events, up to the score's end, and the end of the track.
-}
events : Array Int -> State -> Result State State
events bytes s0 =
    case read bytes s0 of
        Nothing ->
            Err s0

        Just ( descriptor, s1 ) ->
            let
                ( channel, s2 ) =
                    midiChannel (Bitwise.and descriptor 0x0F) s1
            in
            if Bitwise.and descriptor 0x70 == 0x60 then
                -- score end: the end of the track, at the time still queued
                Ok (writeEvent [ 0xFF, 0x2F, 0x00 ] s2)

            else
                case event bytes (Bitwise.and descriptor 0x70) channel s2 of
                    Err s3 ->
                        Err s3

                    Ok s3 ->
                        if Bitwise.and descriptor 0x80 /= 0 then
                            case delay bytes 0 s3 of
                                Err s4 ->
                                    Err s4

                                Ok s4 ->
                                    events bytes s4

                        else
                            events bytes s3


{-| The time after the last event of a group: 7 bits a byte, most significant first,
in a 32-bit unsigned int as the C has it.
-}
delay : Array Int -> Int -> State -> Result State State
delay bytes acc s =
    case read bytes s of
        Nothing ->
            Err s

        Just ( b, s1 ) ->
            let
                total =
                    modBy 0x0000000100000000 (acc * 128 + Bitwise.and b 0x7F)
            in
            if Bitwise.and b 0x80 /= 0 then
                delay bytes total s1

            else
                Ok { s1 | queuedTime = modBy 0x0000000100000000 (s1.queuedTime + total) }


{-| One MUS event (not a score end) on a MIDI channel.
-}
event : Array Int -> Int -> Int -> State -> Result State State
event bytes kind channel s =
    case kind of
        0x00 ->
            -- release key
            readThen bytes s (\key s1 -> Ok (writeEvent [ 0x80 + channel, Bitwise.and key 0x7F, 0 ] s1))

        0x10 ->
            -- press key, with a volume if bit 7 of the key is set
            readThen bytes
                s
                (\key s1 ->
                    let
                        press s2 =
                            Ok
                                (writeEvent
                                    [ 0x90 + channel
                                    , Bitwise.and key 0x7F
                                    , Bitwise.and (byteAt channel s2.velocities) 0x7F
                                    ]
                                    s2
                                )
                    in
                    if Bitwise.and key 0x80 /= 0 then
                        readThen bytes
                            s1
                            (\v s2 -> press { s2 | velocities = Array.set channel (Bitwise.and v 0x7F) s2.velocities })

                    else
                        press s1
                )

        0x20 ->
            -- pitch wheel: MUS's 0 to 255 is MIDI's 14 bits over 64
            readThen bytes
                s
                (\key s1 ->
                    let
                        wheel =
                            key * 64
                    in
                    Ok (writeEvent [ 0xE0 + channel, Bitwise.and wheel 0x7F, Bitwise.and (Bitwise.shiftRightBy 7 wheel) 0x7F ] s1)
                )

        0x30 ->
            -- system event: a valueless controller
            readThen bytes
                s
                (\number s1 ->
                    if number < 10 || number > 14 then
                        Err s1

                    else
                        Ok (controller channel (controllerNumber number) 0 s1)
                )

        0x40 ->
            -- change controller; controller 0 is the instrument
            readThen bytes
                s
                (\number s1 ->
                    readThen bytes
                        s1
                        (\value s2 ->
                            if number == 0 then
                                Ok (writeEvent [ 0xC0 + channel, Bitwise.and value 0x7F ] s2)

                            else if number < 1 || number > 9 then
                                Err s2

                            else
                                Ok (controller channel (controllerNumber number) value s2)
                        )
                )

        _ ->
            Err s


readThen : Array Int -> State -> (Int -> State -> Result State State) -> Result State State
readThen bytes s f =
    case read bytes s of
        Nothing ->
            Err s

        Just ( b, s1 ) ->
            f b s1


{-| mus2mid.c's controller\_map: MUS controller and system event numbers to MIDI's.
-}
controllerNumber : Int -> Int
controllerNumber n =
    case n of
        1 ->
            0x20

        2 ->
            0x01

        3 ->
            0x07

        4 ->
            0x0A

        5 ->
            0x0B

        6 ->
            0x5B

        7 ->
            0x5D

        8 ->
            0x40

        9 ->
            0x43

        10 ->
            0x78

        11 ->
            0x7B

        12 ->
            0x7E

        13 ->
            0x7F

        14 ->
            0x79

        _ ->
            0x00


{-| A controller change; a value over 127 (MUS values are bytes) becomes 127.
-}
controller : Int -> Int -> Int -> State -> State
controller channel number value s =
    writeEvent
        [ 0xB0 + channel
        , Bitwise.and number 0x7F
        , if Bitwise.and value 0x80 /= 0 then
            0x7F

          else
            value
        ]
        s


{-| The MIDI channel for a MUS channel, allocating the next one (not 9) the first
time a channel is used, with an "all notes off" on it.
-}
midiChannel : Int -> State -> ( Int, State )
midiChannel musChannel s =
    if musChannel == 15 then
        ( 9, s )

    else
        let
            mapped =
                byteAt musChannel s.channelMap
        in
        if mapped /= -1 then
            ( mapped, s )

        else
            let
                next =
                    Array.foldl max -1 s.channelMap + 1

                allocated =
                    if next == 9 then
                        10

                    else
                        next
            in
            ( allocated
            , controller allocated 0x7B 0 { s | channelMap = Array.set musChannel allocated s.channelMap }
            )


{-| An event at the time queued so far, which is then written out.
-}
writeEvent : List Int -> State -> State
writeEvent eventBytes s =
    let
        time =
            timeBytes s.queuedTime
    in
    { s
        | out = List.reverse eventBytes ++ List.reverse time ++ s.out
        , trackSize = s.trackSize + List.length time + List.length eventBytes
        , queuedTime = 0
    }


{-| WriteTime: a variable-length quantity, built in a 32-bit unsigned int as the C
builds it (so a time of 2^28 ticks or more loses its top bits, as there).
-}
timeBytes : Int -> List Int
timeBytes time =
    let
        build t buffer =
            if t == 0 then
                buffer

            else
                build (Bitwise.shiftRightZfBy 7 t)
                    (Bitwise.shiftRightZfBy 0
                        (Bitwise.or (Bitwise.shiftLeftBy 8 buffer) (Bitwise.or (Bitwise.and t 0x7F) 0x80))
                    )

        emit buffer acc =
            if Bitwise.and buffer 0x80 /= 0 then
                emit (Bitwise.shiftRightZfBy 8 buffer) (Bitwise.and buffer 0xFF :: acc)

            else
                List.reverse (Bitwise.and buffer 0xFF :: acc)
    in
    emit (build (Bitwise.shiftRightZfBy 7 time) (Bitwise.and time 0x7F)) []


{-| The MIDI file: header, track header with the track's length, and the track.
-}
file : State -> Bytes
file s =
    let
        size =
            s.trackSize
    in
    E.encode
        (E.sequence
            (List.map E.unsignedInt8
                ([ 0x4D, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 0, 0, 1, 0, 0x46, 0x4D, 0x54, 0x72, 0x6B ]
                    ++ [ Bitwise.and (Bitwise.shiftRightZfBy 24 size) 0xFF
                       , Bitwise.and (Bitwise.shiftRightZfBy 16 size) 0xFF
                       , Bitwise.and (Bitwise.shiftRightZfBy 8 size) 0xFF
                       , Bitwise.and size 0xFF
                       ]
                    ++ List.reverse s.out
                )
            )
        )
