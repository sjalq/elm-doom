module RenderTest exposing (suite)

import Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Doom.Render.Compose as Compose exposing (Op(..))
import Doom.Render.Internal as Internal
import Doom.Render.Mem as Mem
import Doom.Render.Png as Png
import Doom.Render.View as View
import Doom.Video as Video
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Doom.Render"
        [ pngTests
        , memTests
        , videoTests
        , viewSizeTests
        , composeTests
        ]



-- PNG


u32 : Int -> Decode.Decoder Int
u32 _ =
    Decode.unsignedInt32 BE


pngTests : Test
pngTests =
    let
        palette =
            Array.initialize 256 (\i -> { r = i, g = 255 - i, b = 7 })

        pic =
            { width = 3, height = 2, pixels = Array.fromList [ 1, 2, 3, 4, 5, 300 ] }

        file =
            Png.encode palette pic

        bytesAt ofs n =
            Decode.decode (Decode.map2 (\_ b -> b) (Decode.bytes ofs) (Decode.bytes n)) file

        list b =
            Decode.decode
                (Decode.loop ( Bytes.width b, [] )
                    (\( left, acc ) ->
                        if left == 0 then
                            Decode.succeed (Decode.Done (List.reverse acc))

                        else
                            Decode.map (\x -> Decode.Loop ( left - 1, x :: acc )) Decode.unsignedInt8
                    )
                )
                b
                |> Maybe.withDefault []

        -- Chunks after the signature: (type, data).
        chunks =
            let
                go ofs acc =
                    case ( bytesAt ofs 4 |> Maybe.andThen (Decode.decode (u32 0)), bytesAt (ofs + 4) 4 ) of
                        ( Just len, Just typ ) ->
                            let
                                name =
                                    list typ |> List.map Char.fromCode |> String.fromList

                                data =
                                    bytesAt (ofs + 8) len |> Maybe.map list |> Maybe.withDefault []

                                crc =
                                    bytesAt (ofs + 8 + len) 4 |> Maybe.andThen (Decode.decode (u32 0)) |> Maybe.withDefault -1
                            in
                            go (ofs + 12 + len) (( name, data, crc ) :: acc)

                        _ ->
                            List.reverse acc
            in
            go 8 []
    in
    describe "PNG"
        [ test "starts with the PNG signature" <|
            \_ ->
                bytesAt 0 8 |> Maybe.map list |> Expect.equal (Just [ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A ])
        , test "has IHDR, PLTE, IDAT, IEND in order" <|
            \_ -> List.map (\( n, _, _ ) -> n) chunks |> Expect.equal [ "IHDR", "PLTE", "IDAT", "IEND" ]
        , test "IHDR: 3 × 2, 8-bit indexed" <|
            \_ ->
                List.head chunks
                    |> Maybe.map (\( _, d, _ ) -> d)
                    |> Expect.equal (Just [ 0, 0, 0, 3, 0, 0, 0, 2, 8, 3, 0, 0, 0 ])
        , test "every chunk's CRC is right" <|
            \_ ->
                chunks
                    |> List.all (\( n, d, crc ) -> Png.crc32 (List.map Char.toCode (String.toList n) ++ d) == crc)
                    |> Expect.equal True
        , test "IEND's CRC is the well-known AE426082" <|
            \_ -> Png.crc32 (List.map Char.toCode (String.toList "IEND")) |> Expect.equal 0xAE426082
        , test "Adler-32 of Wikipedia" <|
            \_ -> Png.adler32 (List.map Char.toCode (String.toList "Wikipedia")) |> Expect.equal 0x11E60398
        , test "PLTE holds the palette" <|
            \_ ->
                List.drop 1 chunks
                    |> List.head
                    |> Maybe.map (\( _, d, _ ) -> List.take 6 d)
                    |> Expect.equal (Just [ 0, 255, 7, 1, 254, 7 ])
        , test "IDAT round trip: a stored zlib stream of the filtered rows" <|
            \_ ->
                List.drop 2 chunks
                    |> List.head
                    |> Maybe.map (\( _, d, _ ) -> d)
                    |> Expect.equal
                        (Just
                            ([ 0x78, 0x01, 1, 8, 0, 0xF7, 0xFF ]
                                ++ [ 0, 1, 2, 3, 0, 4, 5, 0 ]
                                ++ [ 0x00, 0x40, 0x00, 0x10 ]
                            )
                        )
        ]



-- MEMORY


encodeBytes : List Int -> Bytes
encodeBytes l =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 l))


memTests : Test
memTests =
    let
        mem =
            Mem.fromBytes (encodeBytes [ 1, 2, 3, 0xFE, 0xFF ])
    in
    describe "Mem"
        [ test "reads every byte back, 0 outside" <|
            \_ -> List.map (\i -> Mem.byte i mem) (List.range -1 6) |> Expect.equal [ 0, 1, 2, 3, 0xFE, 0xFF, 0, 0 ]
        , test "fromList packs the same way" <|
            \_ -> Mem.fromList [ 1, 2, 3, 0xFE, 0xFF ] |> Expect.equal mem
        , test "SHORT and LONG are little-endian, SHORT signed" <|
            \_ -> ( Mem.s16 3 mem, Mem.u32 0 mem ) |> Expect.equal ( -2, 0xFE030201 )
        ]



-- VIDEO


{-| A 2 × 3 patch, offsets (1, -2): column 0 a post of 2 pixels at the top, column 1
a post of 2 pixels one row down.
-}
tinyPatch : List Int
tinyPatch =
    [ 2, 0, 3, 0, 1, 0, 0xFE, 0xFF ]
        ++ [ 16, 0, 0, 0, 23, 0, 0, 0 ]
        -- column 0, at 16: topdelta 0, length 2, pad, 10, 11, pad, end
        ++ [ 0, 2, 0, 10, 11, 0, 0xFF ]
        -- column 1, at 23: topdelta 1, length 2, pad, 20, 21, pad, end
        ++ [ 1, 2, 0, 20, 21, 0, 0xFF ]


videoTests : Test
videoTests =
    let
        fb =
            case Video.patch (encodeBytes tinyPatch) of
                Just p ->
                    Video.drawPatch 10 20 p (Video.blank 7)

                Nothing ->
                    Video.blank 0
    in
    describe "Video"
        [ test "drawPatch applies the offsets and posts" <|
            \_ ->
                [ Video.pixel 9 22 fb, Video.pixel 9 23 fb, Video.pixel 9 24 fb, Video.pixel 10 23 fb, Video.pixel 10 24 fb, Video.pixel 10 22 fb ]
                    |> Expect.equal [ 10, 11, 7, 20, 21, 7 ]
        , test "toBytes is row by row and fromBytes reads it back" <|
            \_ ->
                Video.fromBytes (Video.toBytes fb)
                    |> Maybe.map (\g -> List.map (\( x, y ) -> Video.pixel x y g) [ ( 9, 22 ), ( 10, 24 ), ( 319, 199 ) ])
                    |> Expect.equal (Just [ 10, 21, 7 ])
        , test "toBytes puts (x, y) at y * 320 + x" <|
            \_ ->
                Decode.decode (Decode.map2 (\_ b -> b) (Decode.bytes (24 * 320 + 10)) Decode.unsignedInt8) (Video.toBytes fb)
                    |> Expect.equal (Just 21)
        ]



-- VIEW SIZE


viewSizeTests : Test
viewSizeTests =
    let
        full =
            Internal.viewSize 11 0

        low =
            Internal.viewSize 9 1
    in
    describe "R_ExecuteSetViewSize"
        [ test "windows: 3 to 11 blocks" <|
            \_ ->
                List.map View.window [ 3, 9, 10, 11 ]
                    |> Expect.equal
                        [ { x = 112, y = 60, width = 96, height = 48 }
                        , { x = 16, y = 12, width = 288, height = 144 }
                        , { x = 0, y = 0, width = 320, height = 168 }
                        , { x = 0, y = 0, width = 320, height = 200 }
                        ]
        , test "low detail halves the view's width, not the window's" <|
            \_ -> ( low.viewwidth, low.scaledviewwidth, low.centerx ) |> Expect.equal ( 144, 288, 72 )
        , test "the view's edge angles are the clip angle either way" <|
            \_ ->
                ( Array.get 0 full.xtoviewangle, Array.get 320 full.xtoviewangle |> Maybe.map (\a -> 4294967296 - a) )
                    |> Expect.equal ( Just full.clipangle, Just full.clipangle )
        , test "xtoviewangle falls from left to right" <|
            \_ ->
                Array.toList full.xtoviewangle
                    |> List.map (\a -> modBy 4294967296 (a + 0x40000000))
                    |> (\l -> List.map2 (>=) l (List.drop 1 l))
                    |> List.all identity
                    |> Expect.equal True
        , test "scalelight: bright and near is table 0, dark and far table 31" <|
            \_ ->
                ( Array.get 15 full.scalelight |> Maybe.andThen (Array.get 47), Array.get 0 full.scalelight |> Maybe.andThen (Array.get 0) )
                    |> Expect.equal ( Just 0, Just 31 )
        ]



-- COMPOSING COLUMNS


composeTests : Test
composeTests =
    let
        fuzzmap =
            Array.initialize 256 (\c -> c + 100)

        cols ops masked =
            { solid = Compose.push 0 ops Compose.emptyColumns.solid, masked = Compose.push 0 masked Compose.emptyColumns.masked }

        pixelsOf fb =
            List.map (\y -> Video.pixel 0 y fb) (List.range 0 5)
    in
    describe "Compose"
        [ test "runs land on their rows, the rest keeps the old screen" <|
            \_ ->
                Compose.apply fuzzmap { solid = Compose.push 0 (Run 3 [ 5, 6 ]) (Compose.push 0 (Run 0 [ 1 ]) Compose.emptyColumns.solid), masked = Compose.emptyColumns.masked } (Video.blank 9)
                    |> pixelsOf
                    |> Expect.equal [ 1, 9, 9, 5, 6, 9 ]
        , test "overlapping runs: the later one wins" <|
            \_ ->
                Compose.apply fuzzmap { solid = Compose.push 0 (Run 1 [ 2, 2 ]) (Compose.push 0 (Run 0 [ 1, 1, 1 ]) Compose.emptyColumns.solid), masked = Compose.emptyColumns.masked } (Video.blank 9)
                    |> pixelsOf
                    |> Expect.equal [ 1, 2, 2, 9, 9, 9 ]
        , test "fuzz reads the pixel below or above as it was at that moment" <|
            \_ ->
                -- fuzzoffset starts +1, -1, +1: row 1 reads row 2, row 2 reads the
                -- new row 1, row 3 reads row 4
                Compose.apply fuzzmap (cols (Run 0 [ 0, 1, 2, 3, 4, 5 ]) (Fuzz 1 3 0)) (Video.blank 9)
                    |> pixelsOf
                    |> Expect.equal [ 0, 102, 202, 104, 4, 5 ]
        ]
