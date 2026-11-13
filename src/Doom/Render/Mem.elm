module Doom.Render.Mem exposing (Mem, byte, fromBytes, fromList, s16, size, u32)

{-| A block of bytes the renderer reads from as the C source reads memory: a patch
lump, a composed texture. The original's column drawers read wherever their
pointers lead, past the end of a column and into whatever follows it (the next
post's header, the next column of a texture), so the renderer keeps lumps whole and
reads them by offset.

Two bytes are packed into each `Int` (the low byte first), which keeps the arrays
small and every element a small integer under every backend. Reading outside the
block gives 0 (the original would read whatever memory lay there).

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D


{-| Packed bytes: element i holds bytes 2i (low) and 2i + 1 (high).
-}
type alias Mem =
    Array Int


{-| A lump's bytes.
-}
fromBytes : Bytes -> Mem
fromBytes bytes =
    let
        n =
            Bytes.width bytes

        pairs =
            n // 2

        decoder =
            D.loop ( pairs, [] )
                (\( left, acc ) ->
                    if left <= 0 then
                        if n - pairs * 2 == 1 then
                            D.map (\b -> D.Done (List.reverse (b :: acc))) D.unsignedInt8

                        else
                            D.succeed (D.Done (List.reverse acc))

                    else
                        D.map (\w -> D.Loop ( left - 1, w :: acc )) (D.unsignedInt16 LE)
                )
    in
    D.decode decoder bytes
        |> Maybe.withDefault []
        |> Array.fromList


{-| Bytes from a list (each taken modulo 256).
-}
fromList : List Int -> Mem
fromList list =
    let
        go rest acc =
            case rest of
                a :: b :: more ->
                    go more ((modBy 256 a + 256 * modBy 256 b) :: acc)

                [ a ] ->
                    List.reverse (modBy 256 a :: acc)

                [] ->
                    List.reverse acc
    in
    Array.fromList (go list [])


{-| The byte at an offset, 0 outside the block.
-}
byte : Int -> Mem -> Int
byte i mem =
    if i < 0 then
        0

    else
        case Array.get (Bitwise.shiftRightBy 1 i) mem of
            Just w ->
                if Bitwise.and 1 i == 0 then
                    Bitwise.and 255 w

                else
                    Bitwise.shiftRightBy 8 w

            Nothing ->
                0


{-| A little-endian unsigned 32-bit value at an offset (C's `LONG` of an int read as
an offset).
-}
u32 : Int -> Mem -> Int
u32 i mem =
    byte i mem + 256 * (byte (i + 1) mem + 256 * (byte (i + 2) mem + 256 * byte (i + 3) mem))


{-| A little-endian signed 16-bit value at an offset (C's `SHORT`).
-}
s16 : Int -> Mem -> Int
s16 i mem =
    let
        u =
            byte i mem + 256 * byte (i + 1) mem
    in
    if u >= 32768 then
        u - 65536

    else
        u


{-| The number of bytes the block can hold (rounded up to an even number).
-}
size : Mem -> Int
size mem =
    2 * Array.length mem
