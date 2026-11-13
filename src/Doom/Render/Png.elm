module Doom.Render.Png exposing (encode, crc32, adler32)

{-| Write a palette-indexed picture as a PNG file, with nothing but `elm/bytes`: an
8-bit indexed image (colour type 3) with its palette, the pixels stored in
uncompressed deflate blocks.

@docs encode, crc32, adler32

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode
import Doom.Render.Picture exposing (Palette)


{-| The PNG file for a `width` × `height` picture of palette indices (row by row;
anything outside 0 to 255 is drawn as index 0) in this palette.
-}
encode : Palette -> { a | width : Int, height : Int, pixels : Array Int } -> Bytes
encode palette pic =
    let
        rows =
            List.range 0 (pic.height - 1)
                |> List.concatMap
                    (\y ->
                        0
                            :: (Array.slice (y * pic.width) ((y + 1) * pic.width) pic.pixels
                                    |> Array.toList
                                    |> List.map
                                        (\p ->
                                            if p < 0 || p > 255 then
                                                0

                                            else
                                                p
                                        )
                               )
                    )

        ihdr =
            List.concat
                [ u32 pic.width
                , u32 pic.height
                , [ 8 -- bit depth
                  , 3 -- colour type: indexed
                  , 0 -- deflate
                  , 0 -- adaptive filtering
                  , 0 -- no interlace
                  ]
                ]

        plte =
            List.range 0 255
                |> List.concatMap
                    (\i ->
                        case Array.get i palette of
                            Just c ->
                                [ c.r, c.g, c.b ]

                            Nothing ->
                                [ 0, 0, 0 ]
                    )
    in
    Encode.encode
        (Encode.sequence
            (List.map Encode.unsignedInt8 [ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A ]
                ++ chunk "IHDR" ihdr
                ++ chunk "PLTE" plte
                ++ chunk "IDAT" (zlibStored rows)
                ++ chunk "IEND" []
            )
        )


u32 : Int -> List Int
u32 n =
    [ Bitwise.and 0xFF (Bitwise.shiftRightZfBy 24 n)
    , Bitwise.and 0xFF (Bitwise.shiftRightZfBy 16 n)
    , Bitwise.and 0xFF (Bitwise.shiftRightZfBy 8 n)
    , Bitwise.and 0xFF n
    ]


chunk : String -> List Int -> List Encode.Encoder
chunk name data =
    let
        typeBytes =
            String.toList name |> List.map Char.toCode

        body =
            typeBytes ++ data
    in
    List.map Encode.unsignedInt8 (u32 (List.length data) ++ body ++ u32 (crc32 body))


{-| A zlib stream holding `data` in stored (uncompressed) deflate blocks of at most
65535 bytes, then the Adler-32 of the data.
-}
zlibStored : List Int -> List Int
zlibStored data =
    let
        blocks l acc =
            let
                block =
                    List.take 65535 l

                rest =
                    List.drop 65535 l

                len =
                    List.length block

                final =
                    if List.isEmpty rest then
                        1

                    else
                        0

                header =
                    [ final
                    , Bitwise.and 0xFF len
                    , Bitwise.shiftRightBy 8 len
                    , Bitwise.and 0xFF (Bitwise.complement len)
                    , Bitwise.and 0xFF (Bitwise.shiftRightBy 8 (Bitwise.and 0xFFFF (Bitwise.complement len)))
                    ]
            in
            if List.isEmpty rest then
                List.reverse ((header ++ block) :: acc)

            else
                blocks rest ((header ++ block) :: acc)
    in
    -- 0x78 0x01: deflate, 32K window, no preset dictionary, fastest.
    [ 0x78, 0x01 ] ++ List.concat (blocks data []) ++ u32 (adler32 data)


{-| Adler-32 of a list of bytes.
-}
adler32 : List Int -> Int
adler32 data =
    let
        ( a, b ) =
            List.foldl
                (\byte ( s1, s2 ) ->
                    let
                        n1 =
                            modBy 65521 (s1 + byte)
                    in
                    ( n1, modBy 65521 (s2 + n1) )
                )
                ( 1, 0 )
                data
    in
    b * 65536 + a


{-| CRC-32 (the one PNG and zlib use) of a list of bytes.
-}
crc32 : List Int -> Int
crc32 data =
    let
        c =
            List.foldl
                (\byte acc ->
                    let
                        idx =
                            Bitwise.and 0xFF (Bitwise.xor acc byte)
                    in
                    Bitwise.xor (Array.get idx crcTable |> Maybe.withDefault 0) (Bitwise.shiftRightZfBy 8 acc)
                )
                0xFFFFFFFF
                data
    in
    Bitwise.shiftRightZfBy 0 (Bitwise.xor c 0xFFFFFFFF)


crcTable : Array Int
crcTable =
    Array.initialize 256
        (\n ->
            List.foldl
                (\_ c ->
                    if Bitwise.and c 1 /= 0 then
                        Bitwise.xor 0xEDB88320 (Bitwise.shiftRightZfBy 1 c)

                    else
                        Bitwise.shiftRightZfBy 1 c
                )
                n
                (List.range 1 8)
        )
