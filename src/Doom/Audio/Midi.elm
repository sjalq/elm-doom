module Doom.Audio.Midi exposing (decode, isMidi)

{-| Standard MIDI files as MUS songs. Doom's own music lumps are MUS, but some WADs
(Freedoom's among them) carry MIDI files instead, which Doom's later ports play
through the same OPL driver. This turns one into a `Doom.Audio.Mus.Song`: every
track merged in time order, tempo changes applied, times rounded to MUS's 140 ticks
a second (without drift: each event's time is rounded from the start, not from the
last event), MIDI's percussion channel 10 swapped with MUS's channel 15, and the
controllers DMX knows mapped to their MUS numbers.

@docs decode, isMidi

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as D
import Doom.Audio.Mus as Mus exposing (Controller(..), Event(..), SystemEvent(..))


{-| Whether a lump is a MIDI file (starts with "MThd").
-}
isMidi : Bytes -> Bool
isMidi bytes =
    D.decode (D.unsignedInt32 Bytes.BE) bytes == Just 0x4D546864


{-| Decode a MIDI file (format 0 or 1).
-}
decode : Bytes -> Maybe Mus.Song
decode bytes =
    D.decode (list (Bytes.width bytes) D.unsignedInt8) bytes
        |> Maybe.andThen fromList


list : Int -> D.Decoder a -> D.Decoder (List a)
list n item =
    D.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\x -> D.Loop ( left - 1, x :: acc )) item
        )


fromList : List Int -> Maybe Mus.Song
fromList all =
    case all of
        0x4D :: 0x54 :: 0x68 :: 0x64 :: l1 :: l2 :: l3 :: l4 :: rest ->
            let
                headerLength =
                    be [ l1, l2, l3, l4 ]

                headerBytes =
                    List.take headerLength rest

                division =
                    be (List.take 2 (List.drop 4 headerBytes))

                tracks =
                    readTracks (List.drop headerLength rest) 0 []

                timed =
                    tracks
                        |> List.concat
                        |> List.sortBy (\e -> ( e.tick, e.track, e.order ))
                        |> toMicroseconds division
            in
            if Bitwise.and division 0x8000 /= 0 && smpteTicksPerSecond division <= 0 then
                Nothing

            else
                Just
                    { primaryChannels = 16
                    , secondaryChannels = 0
                    , instruments = []
                    , events = toMus timed
                    }

        _ ->
            Nothing


be : List Int -> Int
be =
    List.foldl (\b acc -> acc * 256 + b) 0


smpteTicksPerSecond : Int -> Int
smpteTicksPerSecond division =
    let
        fps =
            256 - Bitwise.shiftRightBy 8 division

        perFrame =
            Bitwise.and division 0xFF
    in
    (if fps == 29 then
        30

     else
        fps
    )
        * perFrame



-- TRACKS


{-| A track event: when (in MIDI ticks), which track, its place in the track, and
what it is (a channel message, or a tempo in microseconds a quarter note).
-}
type alias Raw =
    { tick : Int
    , track : Int
    , order : Int
    , what : What
    }


type What
    = Channel Int Int Int Int
    | Tempo Int


readTracks : List Int -> Int -> List (List Raw) -> List (List Raw)
readTracks bytes n acc =
    case bytes of
        0x4D :: 0x54 :: 0x72 :: 0x6B :: l1 :: l2 :: l3 :: l4 :: rest ->
            let
                len =
                    be [ l1, l2, l3, l4 ]
            in
            readTracks (List.drop len rest) (n + 1) (readTrack n (List.take len rest) :: acc)

        _ :: _ :: _ :: _ :: l1 :: l2 :: l3 :: l4 :: rest ->
            -- an unknown chunk: skip it
            readTracks (List.drop (be [ l1, l2, l3, l4 ]) rest) n acc

        _ ->
            List.reverse acc


readTrack : Int -> List Int -> List Raw
readTrack track bytes =
    trackLoop track bytes 0 0 0 []


trackLoop : Int -> List Int -> Int -> Int -> Int -> List Raw -> List Raw
trackLoop track bytes tick running order acc =
    case trackEvent bytes running of
        Next delta what rest status ->
            let
                now =
                    tick + delta
            in
            case what of
                Just w ->
                    trackLoop track rest now status (order + 1) ({ tick = now, track = track, order = order, what = w } :: acc)

                Nothing ->
                    trackLoop track rest now status order acc

        End ->
            List.reverse acc


{-| One step through a track: the delta time, the event if it is one we keep, what
follows, and the running status.
-}
type Step
    = Next Int (Maybe What) (List Int) Int
    | End


trackEvent : List Int -> Int -> Step
trackEvent bytes running =
    let
        ( delta, afterDelta ) =
            vlq 0 bytes
    in
    case afterDelta of
        [] ->
            End

        first :: afterFirst ->
            let
                ( status, data ) =
                    if first >= 0x80 then
                        ( first, afterFirst )

                    else
                        ( running, afterDelta )

                kind =
                    Bitwise.and status 0xF0

                channel =
                    Bitwise.and status 0x0F
            in
            if status == 0xFF then
                case data of
                    metaType :: afterType ->
                        let
                            ( len, afterLen ) =
                                vlq 0 afterType

                            rest =
                                List.drop len afterLen
                        in
                        if metaType == 0x2F then
                            End

                        else if metaType == 0x51 && len == 3 then
                            Next delta (Just (Tempo (be (List.take len afterLen)))) rest running

                        else
                            Next delta Nothing rest running

                    [] ->
                        End

            else if status == 0xF0 || status == 0xF7 then
                let
                    ( len, afterLen ) =
                        vlq 0 data
                in
                Next delta Nothing (List.drop len afterLen) running

            else if kind == 0xC0 || kind == 0xD0 then
                case data of
                    a :: rest ->
                        Next delta (Just (Channel kind channel a 0)) rest status

                    [] ->
                        End

            else if kind >= 0x80 then
                case data of
                    a :: b :: rest ->
                        Next delta (Just (Channel kind channel a b)) rest status

                    _ ->
                        End

            else
                -- data with no status to run on: the file is broken here
                End


vlq : Int -> List Int -> ( Int, List Int )
vlq acc bytes =
    case bytes of
        b :: rest ->
            if Bitwise.and b 0x80 /= 0 then
                vlq (acc * 128 + Bitwise.and b 0x7F) rest

            else
                ( acc * 128 + b, rest )

        [] ->
            ( acc, [] )



-- TIME


{-| Each event's time from the start in microseconds, following the tempo changes
(120 beats a minute until the first).
-}
toMicroseconds : Int -> List Raw -> List ( Float, What )
toMicroseconds division events =
    let
        ticksPerQuarter =
            toFloat (max 1 division)

        step e ( lastTick, lastUs, tempo ) =
            let
                us =
                    if Bitwise.and division 0x8000 /= 0 then
                        toFloat e.tick * 1000000 / toFloat (smpteTicksPerSecond division)

                    else
                        lastUs + toFloat (e.tick - lastTick) * toFloat tempo / ticksPerQuarter
            in
            ( ( us, e.what )
            , ( e.tick
              , us
              , case e.what of
                    Tempo t ->
                        t

                    _ ->
                        tempo
              )
            )
    in
    events
        |> List.foldl
            (\e ( acc, state ) ->
                let
                    ( out, next ) =
                        step e state
                in
                ( out :: acc, next )
            )
            ( [], ( 0, 0, 500000 ) )
        |> Tuple.first
        |> List.reverse


toMus : List ( Float, What ) -> List Mus.Timed
toMus events =
    let
        musTick us =
            round (us * toFloat Mus.ticksPerSecond / 1000000)

        withTimes =
            List.filterMap (\( us, what ) -> musEvent what |> Maybe.map (\e -> ( musTick us, e ))) events
                ++ [ ( List.foldl (\( us, _ ) m -> max m (musTick us)) 0 events, ScoreEnd ) ]
    in
    delays withTimes []


{-| MUS events carry the delay after them; the first event's time is dropped (a song
starts with its first note).
-}
delays : List ( Int, Event ) -> List Mus.Timed -> List Mus.Timed
delays events acc =
    case events of
        ( t, e ) :: ((( next, _ ) :: _) as rest) ->
            delays rest ({ event = e, delay = max 0 (next - t) } :: acc)

        [ ( _, e ) ] ->
            List.reverse ({ event = e, delay = 0 } :: acc)

        [] ->
            List.reverse acc


musChannel : Int -> Int
musChannel c =
    if c == 9 then
        15

    else if c == 15 then
        9

    else
        c


musEvent : What -> Maybe Event
musEvent what =
    case what of
        Tempo _ ->
            Nothing

        Channel kind c a b ->
            let
                ch =
                    musChannel c
            in
            case kind of
                0x80 ->
                    Just (ReleaseNote ch a)

                0x90 ->
                    if b == 0 then
                        Just (ReleaseNote ch a)

                    else
                        Just (PlayNote ch a (Just b))

                0xB0 ->
                    controllerEvent ch a b

                0xC0 ->
                    Just (Mus.Controller ch ChangeInstrument a)

                0xE0 ->
                    Just (PitchBend ch (Bitwise.shiftRightBy 6 (b * 128 + a)))

                _ ->
                    Nothing


controllerEvent : Int -> Int -> Int -> Maybe Event
controllerEvent ch number value =
    let
        set k =
            Just (Mus.Controller ch k value)
    in
    case number of
        0 ->
            set BankSelect

        1 ->
            set Modulation

        7 ->
            set Volume

        10 ->
            set Pan

        11 ->
            set Expression

        91 ->
            set Reverb

        93 ->
            set Chorus

        64 ->
            set Sustain

        67 ->
            set SoftPedal

        120 ->
            Just (System ch AllSoundsOff)

        123 ->
            Just (System ch AllNotesOff)

        126 ->
            Just (System ch Mono)

        127 ->
            Just (System ch Poly)

        121 ->
            Just (System ch ResetAllControllers)

        _ ->
            Nothing
