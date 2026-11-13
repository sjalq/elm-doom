module RenderTest exposing (suite)

import Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Doom.Fixed as Fixed
import Doom.Level exposing (Child(..), Level)
import Doom.Render.Build as Build
import Doom.Render.Picture as Picture
import Doom.Render.Png as Png
import Doom.Render.Scene as Scene
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Doom.Render"
        [ subsectorTests
        , pngTests
        , patchTests
        , sceneTests
        ]



-- SUBSECTORS


{-| A 256 × 256 room cut in two by one BSP node along x = 128: subsector 0 east of
it, subsector 1 west. Every line is one-sided, going clockwise round the room so the
room is on its right.
-}
tinyLevel : Level
tinyLevel =
    let
        v x y =
            { x = Fixed.fromInt x, y = Fixed.fromInt y }

        seg a b =
            { v1 = a, v2 = b, offset = 0, angle = 0, sidedef = 0, linedef = 0, side = 0, frontsector = 0, backsector = Nothing }

        box =
            { top = 0, bottom = 0, left = 0, right = 0 }
    in
    { name = "TINY"
    , vertexes = Array.fromList [ v 0 0, v 128 0, v 256 0, v 256 256, v 128 256, v 0 256 ]
    , sectors = Array.fromList [ { lines = Array.empty, blockbox = box, soundorgX = 0, soundorgY = 0 } ]
    , sides = Array.fromList [ { sector = 0 } ]
    , lines = Array.empty
    , segs =
        Array.fromList
            [ seg (v 128 256) (v 256 256)
            , seg (v 256 256) (v 256 0)
            , seg (v 256 0) (v 128 0)
            , seg (v 128 0) (v 0 0)
            , seg (v 0 0) (v 0 256)
            , seg (v 0 256) (v 128 256)
            ]
    , subsectors =
        Array.fromList
            [ { sector = 0, numlines = 3, firstline = 0 }
            , { sector = 0, numlines = 3, firstline = 3 }
            ]
    , nodes =
        Array.fromList
            [ { x = Fixed.fromInt 128
              , y = 0
              , dx = 0
              , dy = Fixed.fromInt 256
              , bbox = ( box, box )
              , children = ( ChildSubsector 0, ChildSubsector 1 )
              }
            ]
    , blockmap = { orgx = 0, orgy = 0, width = 0, height = 0, cells = Array.empty }
    , reject = Array.empty
    , things = []
    }


signedArea : List Scene.Point -> Float
signedArea pts =
    case pts of
        first :: _ ->
            List.map2 (\a b -> a.x * b.y - b.x * a.y) pts (List.drop 1 pts ++ [ first ])
                |> List.sum
                |> (\s -> s / 2)

        [] ->
            0


{-| Every turn of the polygon goes left (anticlockwise), allowing for collinear points.
-}
convex : List Scene.Point -> Bool
convex pts =
    case pts of
        a :: b :: _ ->
            let
                ring =
                    pts ++ [ a, b ]

                triples =
                    List.map3 (\p q r -> ( p, q, r )) ring (List.drop 1 ring) (List.drop 2 ring)
            in
            List.all (\( p, q, r ) -> (q.x - p.x) * (r.y - q.y) - (q.y - p.y) * (r.x - q.x) >= -1.0e-6) triples

        _ ->
            False


subsectorTests : Test
subsectorTests =
    let
        mesh =
            Build.levelMesh tinyLevel

        polygons =
            Array.toList mesh.subsectors |> List.map .polygon
    in
    describe "subsector polygons"
        [ test "there is one per subsector" <|
            \_ -> List.length polygons |> Expect.equal 2
        , test "they are convex and anticlockwise" <|
            \_ -> List.all (\p -> convex p && signedArea p > 0) polygons |> Expect.equal True
        , test "their areas sum to the room's" <|
            \_ -> List.map signedArea polygons |> List.sum |> Expect.within (Expect.Absolute 0.01) (256 * 256)
        , test "each is half the room" <|
            \_ -> List.map signedArea polygons |> Expect.equal [ 128 * 256, 128 * 256 ]
        , test "the east one is east of the partition" <|
            \_ ->
                List.head polygons
                    |> Maybe.map (List.all (\p -> p.x >= 128))
                    |> Expect.equal (Just True)
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



-- PATCHES


encodeBytes : List Int -> Bytes
encodeBytes l =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 l))


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


patchTests : Test
patchTests =
    let
        t =
            Picture.transparent

        lookup n =
            case n of
                "A" ->
                    Just (encodeBytes tinyPatch)

                _ ->
                    Nothing
    in
    describe "patches"
        [ test "decodes size, offsets and posts" <|
            \_ ->
                Picture.decodePatch (encodeBytes tinyPatch)
                    |> Maybe.map (\p -> ( ( p.width, p.height ), ( p.leftOffset, p.topOffset ), Array.toList p.pixels ))
                    |> Expect.equal (Just ( ( 2, 3 ), ( 1, -2 ), [ 10, t, 11, 20, t, 21 ] ))
        , test "rejects a lump too short to be a patch" <|
            \_ -> Picture.decodePatch (encodeBytes [ 2, 0, 3 ]) |> Expect.equal Nothing
        , test "a texture: one-patch columns ignore originY, composited ones don't, later patches win" <|
            \_ ->
                -- Patch A at x 0 and again at x 1 two rows down: column 0 has only
                -- the first, column 1 both, column 2 only the second.
                Picture.composeTexture lookup
                    { name = "T"
                    , width = 3
                    , height = 4
                    , patches =
                        [ { originX = 0, originY = 0, patch = "A" }
                        , { originX = 1, originY = 2, patch = "A" }
                        ]
                    }
                    |> .pixels
                    |> Array.toList
                    |> Expect.equal
                        [ 10
                        , t
                        , t
                        , 11
                        , 20
                        , 20
                        , t
                        , 10
                        , 21
                        , t
                        , 11
                        , t
                        ]
        , test "a solid texture reads one-patch columns straight from the lump" <|
            \_ ->
                Picture.composeSolidTexture lookup
                    { name = "T", width = 1, height = 4, patches = [ { originX = 0, originY = 0, patch = "A" } ] }
                    |> .pixels
                    |> Array.toList
                    -- 10, 11, then the pad byte and the 0xFF end marker.
                    |> Expect.equal [ 10, 11, 0, 0xFF ]
        ]



-- SCENE


sceneTests : Test
sceneTests =
    describe "light and palette"
        [ test "a bright wall right in front is table 0" <|
            \_ -> Scene.wallColormap 15 10 |> Expect.equal 0
        , test "a dark far wall is table 31" <|
            \_ -> Scene.wallColormap 0 5000 |> Expect.equal 31
        , test "light 8 at depth 160: startmap 28 - 16/2" <|
            \_ -> Scene.wallColormap 8 160 |> Expect.equal 20
        , test "plane light 8 at depth 160: j 10, 655360/11/4096 = 14, 28 - 7" <|
            \_ -> Scene.planeColormap 8 160 |> Expect.equal 21
        , test "damage picks a red palette" <|
            \_ -> Scene.paletteFor { damagecount = 20, bonuscount = 0, strength = 0, ironfeet = 0 } |> Expect.equal 4
        , test "lots of damage tops out at palette 8" <|
            \_ -> Scene.paletteFor { damagecount = 100, bonuscount = 5, strength = 0, ironfeet = 0 } |> Expect.equal 8
        , test "a pickup picks gold" <|
            \_ -> Scene.paletteFor { damagecount = 0, bonuscount = 6, strength = 0, ironfeet = 0 } |> Expect.equal 10
        , test "the radiation suit is green, blinking at the end" <|
            \_ ->
                List.map (\t -> Scene.paletteFor { damagecount = 0, bonuscount = 0, strength = 0, ironfeet = t }) [ 1000, 100, 104 ]
                    |> Expect.equal [ 13, 0, 13 ]
        , test "berserk fades in red" <|
            \_ -> Scene.paletteFor { damagecount = 0, bonuscount = 0, strength = 1, ironfeet = 0 } |> Expect.equal 3
        ]
