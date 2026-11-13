module Doom.Render.Picture exposing
    ( Picture, transparent, pixel, empty
    , decodePatch, decodeFlat
    , Palette, Rgb, decodePlaypal, decodeColormaps, Colormaps, colormapRow, numColormaps, inverseColormap
    , composeTexture, composeSolidTexture
    , translationTable
    , toIndexBytes, toAlphaBytes
    , bytesToArray
    )

{-| The WAD's graphics, decoded (r\_data.c, v\_video.c, w\_wad.h's `patch_t`).

Every pixel stays a **palette index** (0 to 255): the palette (PLAYPAL) and the light
tables (COLORMAP) are applied only when a frame is drawn, as the game does. Pixels a
patch leaves out (between its column posts) are `transparent` (-1).

Lumps are passed in as `Bytes`; textures take a lookup function from a lump's name to
its bytes, so this module doesn't depend on how the WAD is read.


# Pictures

@docs Picture, transparent, pixel, empty


# Patches and flats

@docs decodePatch, decodeFlat


# Palettes and light tables

@docs Palette, Rgb, decodePlaypal, decodeColormaps, Colormaps, colormapRow, numColormaps, inverseColormap


# Wall textures

@docs composeTexture, composeSolidTexture


# Player colours

@docs translationTable


# For the GPU

@docs toIndexBytes, toAlphaBytes


# Bytes

@docs bytesToArray

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Doom.Data as Data



-- PICTURES


{-| A decoded picture: `width` by `height` palette indices, row by row (`pixels` has
`width * height` entries, the pixel at (x, y) is at `y * width + x`). Holes are
`transparent`.

`leftOffset` and `topOffset` are a patch's offsets (`patch_t.leftoffset`/`topoffset`):
for a sprite, how far its origin (the thing's position, on the floor) is from the
picture's left edge and top edge; for a menu or status-bar graphic, how far to move it
left and up from where it's drawn. Flats and textures have 0.

-}
type alias Picture =
    { width : Int
    , height : Int
    , leftOffset : Int
    , topOffset : Int
    , pixels : Array Int
    }


{-| The value of a pixel nothing was drawn at.
-}
transparent : Int
transparent =
    -1


{-| A picture of nothing, 0 by 0.
-}
empty : Picture
empty =
    { width = 0, height = 0, leftOffset = 0, topOffset = 0, pixels = Array.empty }


{-| The pixel at (x, y), `transparent` outside the picture.
-}
pixel : Int -> Int -> Picture -> Int
pixel x y pic =
    if x < 0 || y < 0 || x >= pic.width || y >= pic.height then
        transparent

    else
        Array.get (y * pic.width + x) pic.pixels |> Maybe.withDefault transparent



-- BYTES


{-| A lump's bytes as an array of 0 to 255, for reading at any offset.
-}
bytesToArray : Bytes -> Array Int
bytesToArray bytes =
    let
        n =
            Bytes.width bytes

        step ( left, acc ) =
            if left <= 0 then
                Decode.succeed (Decode.Done (Array.fromList (List.reverse acc)))

            else
                Decode.map (\b -> Decode.Loop ( left - 1, b :: acc )) Decode.unsignedInt8
    in
    Decode.decode (Decode.loop ( n, [] ) step) bytes
        |> Maybe.withDefault Array.empty


byteAt : Array Int -> Int -> Int
byteAt arr i =
    Array.get i arr |> Maybe.withDefault 0


short : Array Int -> Int -> Int
short arr i =
    let
        u =
            byteAt arr i + 256 * byteAt arr (i + 1)
    in
    if u >= 32768 then
        u - 65536

    else
        u


long : Array Int -> Int -> Int
long arr i =
    byteAt arr i
        + 256
        * byteAt arr (i + 1)
        + 65536
        * byteAt arr (i + 2)
        + 16777216
        * byteAt arr (i + 3)



-- PATCHES


{-| One column of a patch: its posts, each a run of pixels starting `topdelta` rows
down (`post_t`), and the column's offset in the lump (`columnofs[x]`).
-}
type alias Column =
    { offset : Int
    , posts : List Post
    }


type alias Post =
    { topdelta : Int
    , pixels : List Int
    }


type alias RawPatch =
    { width : Int
    , height : Int
    , leftOffset : Int
    , topOffset : Int
    , columns : Array Column
    , bytes : Array Int
    }


{-| A patch lump (`patch_t`: a header, then column posts). Nothing if it isn't one
(too short, or its column offsets point outside the lump).
-}
decodePatch : Bytes -> Maybe Picture
decodePatch bytes =
    rawPatch (bytesToArray bytes) |> Maybe.map patchPicture


rawPatch : Array Int -> Maybe RawPatch
rawPatch arr =
    let
        size =
            Array.length arr

        width =
            short arr 0

        height =
            short arr 2
    in
    if size < 8 || width <= 0 || height <= 0 || width > 4096 || height > 4096 || size < 8 + 4 * width then
        Nothing

    else
        let
            columns =
                List.range 0 (width - 1)
                    |> List.map (\x -> long arr (8 + 4 * x))
                    |> List.map (\ofs -> { offset = ofs, posts = posts arr ofs })
        in
        if List.any (\c -> c.offset >= size) columns then
            Nothing

        else
            Just
                { width = width
                , height = height
                , leftOffset = short arr 4
                , topOffset = short arr 6
                , columns = Array.fromList columns
                , bytes = arr
                }


{-| The posts of the column at this offset: `topdelta`, `length`, a pad byte, the
pixels and another pad byte, until a `topdelta` of 0xFF.
-}
posts : Array Int -> Int -> List Post
posts arr start =
    let
        go ofs acc guard =
            let
                topdelta =
                    byteAt arr ofs
            in
            if topdelta == 0xFF || ofs >= Array.length arr || guard > 256 then
                List.reverse acc

            else
                let
                    len =
                        byteAt arr (ofs + 1)

                    px =
                        Array.slice (ofs + 3) (ofs + 3 + len) arr |> Array.toList
                in
                go (ofs + len + 4) ({ topdelta = topdelta, pixels = px } :: acc) (guard + 1)
    in
    go start [] 0


patchPicture : RawPatch -> Picture
patchPicture raw =
    { width = raw.width
    , height = raw.height
    , leftOffset = raw.leftOffset
    , topOffset = raw.topOffset
    , pixels =
        fromColumns raw.width raw.height (Array.map (\c -> postsColumn raw.height 0 c.posts) raw.columns)
    }


{-| A column `height` pixels tall with the posts drawn into it, `originy` rows down
(R\_DrawColumnInCache: clipped to the column, later posts over earlier ones).
-}
postsColumn : Int -> Int -> List Post -> Array Int
postsColumn height originy ps =
    drawPosts height originy ps (Array.repeat height transparent)


drawPosts : Int -> Int -> List Post -> Array Int -> Array Int
drawPosts height originy ps column =
    List.foldl
        (\post col ->
            List.foldl
                (\p ( y, c ) ->
                    if y >= 0 && y < height then
                        ( y + 1, Array.set y p c )

                    else
                        ( y + 1, c )
                )
                ( originy + post.topdelta, col )
                post.pixels
                |> Tuple.second
        )
        column
        ps


{-| A picture from its columns (each `height` long).
-}
fromColumns : Int -> Int -> Array (Array Int) -> Array Int
fromColumns width height columns =
    Array.initialize (width * height)
        (\i ->
            Array.get (modBy width i) columns
                |> Maybe.andThen (Array.get (i // width))
                |> Maybe.withDefault transparent
        )



-- FLATS


{-| A flat lump: 64 by 64 raw palette indices, row by row (4096 bytes). Nothing if the
lump is shorter.
-}
decodeFlat : Bytes -> Maybe Picture
decodeFlat bytes =
    let
        arr =
            bytesToArray bytes
    in
    if Array.length arr < 4096 then
        Nothing

    else
        Just { width = 64, height = 64, leftOffset = 0, topOffset = 0, pixels = Array.slice 0 4096 arr }



-- PALETTES


{-| A colour.
-}
type alias Rgb =
    { r : Int, g : Int, b : Int }


{-| 256 colours.
-}
type alias Palette =
    Array Rgb


{-| PLAYPAL: its 14 palettes of 256 colours (768 bytes each). Palette 0 is normal,
1 to 8 the damage reds, 9 to 12 the item-pickup golds, 13 the radiation suit green.
-}
decodePlaypal : Bytes -> Array Palette
decodePlaypal bytes =
    let
        arr =
            bytesToArray bytes
    in
    Array.initialize (Array.length arr // 768)
        (\p ->
            Array.initialize 256
                (\i ->
                    let
                        o =
                            p * 768 + i * 3
                    in
                    { r = byteAt arr o, g = byteAt arr (o + 1), b = byteAt arr (o + 2) }
                )
        )


{-| COLORMAP: 34 light tables of 256 entries, each mapping a palette index to the index
it becomes. `get (m * 256 + i)` is index i under table m.

  - 0 to 31: full brightness to darkest.
  - 32: the invulnerability inverse greyscale (`inverseColormap`).
  - 33: all black (unused).

-}
type alias Colormaps =
    Array Int


{-| NUMCOLORMAPS: the number of light tables from bright to dark (32).
-}
numColormaps : Int
numColormaps =
    32


{-| INVERSECOLORMAP: the invulnerability table.
-}
inverseColormap : Int
inverseColormap =
    32


{-| The COLORMAP lump.
-}
decodeColormaps : Bytes -> Colormaps
decodeColormaps =
    bytesToArray


{-| Palette index `i` under light table `m`.
-}
colormapRow : Colormaps -> Int -> Int -> Int
colormapRow maps m i =
    Array.get (m * 256 + i) maps |> Maybe.withDefault i



-- TEXTURES


{-| A wall texture put together from its patches the way R\_GenerateLookup and
R\_GenerateComposite do it, as the game draws it where the wall can be seen through
(a two-sided line's middle texture, R\_RenderMaskedSegRange):

  - Each patch is drawn in order, later ones over earlier ones, starting at its
    `originX`/`originY`, clipped to the texture's width and height.
  - A column covered by **one** patch is that patch's column as it is in the lump:
    the patch's own posts, starting at the top of the texture whatever the patch's
    `originY` (the game draws such columns straight from the patch, so the vertical
    offset is lost).
  - A column covered by several patches is composited, `originY` and all.
  - A column covered by no patch is left transparent (the game prints a warning and
    draws garbage).

-}
composeTexture : (String -> Maybe Bytes) -> Data.Texture -> Picture
composeTexture lookup texture =
    compose False lookup texture


{-| The same texture as the game draws it on a solid wall (R\_DrawColumn): where
`composeTexture` has a hole, a solid wall still shows something.

  - A one-patch column is the lump's bytes from the first post's pixels on, read
    straight down for the texture's height, whatever is there (post headers
    included: the "tutti-frutti" of textures whose patches have holes).
  - Holes in composited columns are palette index 0, as the composite memory
    starts out (in practice: whatever was there before).

-}
composeSolidTexture : (String -> Maybe Bytes) -> Data.Texture -> Picture
composeSolidTexture lookup texture =
    compose True lookup texture


type alias PlacedPatch =
    { originX : Int
    , originY : Int
    , raw : RawPatch
    }


compose : Bool -> (String -> Maybe Bytes) -> Data.Texture -> Picture
compose solid lookup texture =
    let
        width =
            max 0 texture.width

        height =
            max 0 texture.height

        placed =
            texture.patches
                |> List.filterMap
                    (\tp ->
                        lookup tp.patch
                            |> Maybe.map bytesToArray
                            |> Maybe.andThen rawPatch
                            |> Maybe.map (\raw -> { originX = tp.originX, originY = tp.originY, raw = raw })
                    )

        -- For each texture column: the patches covering it, in order.
        covering x =
            List.filter (\p -> x >= p.originX && x < p.originX + p.raw.width) placed

        column x =
            case covering x of
                [] ->
                    Array.repeat height transparent

                [ p ] ->
                    if solid then
                        rawColumn height p.raw (x - p.originX)

                    else
                        Array.get (x - p.originX) p.raw.columns
                            |> Maybe.map (\c -> postsColumn height 0 c.posts)
                            |> Maybe.withDefault (Array.repeat height transparent)

                many ->
                    let
                        start =
                            Array.repeat height
                                (if solid then
                                    0

                                 else
                                    transparent
                                )
                    in
                    List.foldl
                        (\p col ->
                            case Array.get (x - p.originX) p.raw.columns of
                                Just c ->
                                    drawPosts height p.originY c.posts col

                                Nothing ->
                                    col
                        )
                        start
                        many
    in
    { width = width
    , height = height
    , leftOffset = 0
    , topOffset = 0
    , pixels = fromColumns width height (Array.initialize width column)
    }


{-| `height` bytes of the lump from the column's first pixel (columnofs + 3) on.
-}
rawColumn : Int -> RawPatch -> Int -> Array Int
rawColumn height raw x =
    case Array.get x raw.columns of
        Just c ->
            Array.initialize height (\y -> byteAt raw.bytes (c.offset + 3 + y))

        Nothing ->
            Array.repeat height 0



-- TRANSLATIONS


{-| R\_InitTranslationTables: the colour remaps for players 2 to 4 (a thing's
`(flags & MF_TRANSLATION) >> MF_TRANSSHIFT`, 1 to 3), as 256 entries. The green ramp
(0x70 to 0x7f) becomes grey, brown or red; 0 is no remap.
-}
translationTable : Int -> Array Int
translationTable n =
    Array.initialize 256
        (\i ->
            if i >= 0x70 && i <= 0x7F then
                case n of
                    1 ->
                        0x60 + Bitwise.and i 0x0F

                    2 ->
                        0x40 + Bitwise.and i 0x0F

                    3 ->
                        0x20 + Bitwise.and i 0x0F

                    _ ->
                        i

            else
                i
        )



-- GPU


{-| The picture's palette indices, one byte a pixel, row by row (transparent pixels
are 0): an 8-bit LUMINANCE (or R8) texture.
-}
toIndexBytes : Picture -> Bytes
toIndexBytes pic =
    pic.pixels
        |> Array.toList
        |> List.map (\p -> Encode.unsignedInt8 (max 0 p))
        |> Encode.sequence
        |> Encode.encode


{-| One byte a pixel: 255 where the picture has a pixel, 0 where it is transparent.
With `toIndexBytes`, a LUMINANCE\_ALPHA texture.
-}
toAlphaBytes : Picture -> Bytes
toAlphaBytes pic =
    pic.pixels
        |> Array.toList
        |> List.map
            (\p ->
                Encode.unsignedInt8
                    (if p < 0 then
                        0

                     else
                        255
                    )
            )
        |> Encode.sequence
        |> Encode.encode
