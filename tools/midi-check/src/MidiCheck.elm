port module MidiCheck exposing (main)

{-| Doom.Audio.Midi on a list of MUS lumps, for check.mjs to compare with
Chocolate Doom's mus2mid (by way of hosts/browser/oplmusic.js). The flags are the
lumps' bytes; the answer is each lump converted with a fresh converter, and each
converted in turn with one converter (as a game converts songs one after another).
-}

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Doom.Audio.Midi as Midi


port result : { fresh : List (Maybe (List Int)), sequential : List (Maybe (List Int)) } -> Cmd msg


main : Program (List (List Int)) () ()
main =
    Platform.worker
        { init = \lumps -> ( (), result (check (List.map toBytes lumps)) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


check : List Bytes -> { fresh : List (Maybe (List Int)), sequential : List (Maybe (List Int)) }
check lumps =
    { fresh = List.map (Midi.fromMus >> Maybe.map toList) lumps
    , sequential =
        List.foldl
            (\lump ( converter, acc ) ->
                let
                    ( midi, next ) =
                        Midi.convert converter lump
                in
                ( next, Maybe.map toList midi :: acc )
            )
            ( Midi.fresh, [] )
            lumps
            |> Tuple.second
            |> List.reverse
    }


toBytes : List Int -> Bytes
toBytes list =
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
