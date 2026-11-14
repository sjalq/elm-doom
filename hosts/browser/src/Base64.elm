module Base64 exposing (fromBytes, toBytes)

{-| Base 64 (RFC 4648, with padding), for keeping saved games in the browser's
storage, which holds only strings.

@docs fromBytes, toBytes

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Dict exposing (Dict)


alphabet : Array Char
alphabet =
    Array.fromList (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")


values : Dict Char Int
values =
    Array.toIndexedList alphabet |> List.map (\( i, c ) -> ( c, i )) |> Dict.fromList


{-| Bytes as base 64.
-}
fromBytes : Bytes -> String
fromBytes bytes =
    let
        list =
            D.decode (byteList (Bytes.width bytes)) bytes |> Maybe.withDefault []
    in
    String.fromList (encodeLoop list [])


byteList : Int -> D.Decoder (List Int)
byteList n =
    D.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\b -> D.Loop ( left - 1, b :: acc )) D.unsignedInt8
        )


char : Int -> Char
char i =
    Array.get (Bitwise.and i 63) alphabet |> Maybe.withDefault 'A'


encodeLoop : List Int -> List Char -> List Char
encodeLoop bytes acc =
    case bytes of
        a :: b :: c :: rest ->
            let
                n =
                    a * 65536 + b * 256 + c
            in
            encodeLoop rest (char n :: char (n // 64) :: char (n // 4096) :: char (n // 262144) :: acc)

        [ a, b ] ->
            let
                n =
                    a * 65536 + b * 256
            in
            List.reverse ('=' :: char (n // 64) :: char (n // 4096) :: char (n // 262144) :: acc)

        [ a ] ->
            let
                n =
                    a * 65536
            in
            List.reverse ('=' :: '=' :: char (n // 4096) :: char (n // 262144) :: acc)

        [] ->
            List.reverse acc


{-| Base 64 as bytes, if it is base 64.
-}
toBytes : String -> Maybe Bytes
toBytes text =
    let
        chars =
            String.toList text |> List.filter (\c -> c /= '=' && c /= '\n' && c /= '\u{000D}')

        digits =
            List.map (\c -> Dict.get c values) chars
    in
    if List.any ((==) Nothing) digits then
        Nothing

    else
        Just (E.encode (E.sequence (List.map E.unsignedInt8 (decodeLoop (List.filterMap identity digits) []))))


decodeLoop : List Int -> List Int -> List Int
decodeLoop digits acc =
    case digits of
        a :: b :: c :: d :: rest ->
            let
                n =
                    ((a * 64 + b) * 64 + c) * 64 + d
            in
            decodeLoop rest (Bitwise.and n 255 :: Bitwise.and (n // 256) 255 :: (n // 65536) :: acc)

        [ a, b, c ] ->
            let
                n =
                    ((a * 64 + b) * 64 + c) * 64
            in
            List.reverse (Bitwise.and (n // 256) 255 :: (n // 65536) :: acc)

        [ a, b ] ->
            let
                n =
                    (a * 64 + b) * 4096
            in
            List.reverse ((n // 65536) :: acc)

        _ ->
            List.reverse acc
