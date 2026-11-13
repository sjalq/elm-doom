module Doom.Video exposing
    ( Framebuffer, Patch, width, height
    , blank, patch, patchSize
    , drawPatch, drawPatchFlipped, drawPatchTranslated, tileFlat, fill, plot, copyRect
    , pixel, toBytes, fromBytes
    )

{-| The screen and drawing on it (v\_video.c): a 320 × 200 framebuffer of palette
indices, patches (the WAD's picture format), and the primitives everything 2D is drawn
with: patches, tiled flats, rectangles, single pixels. The 3D view is drawn into the
same framebuffer by `Doom.Render.View`.

Drawing clips to the screen where the original would stop with "Bad V\_DrawPatch"
(it never does in the game itself).

@docs Framebuffer, Patch, width, height
@docs blank, patch, patchSize
@docs drawPatch, drawPatchFlipped, drawPatchTranslated, tileFlat, fill, plot, copyRect
@docs pixel, toBytes, fromBytes

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E


{-| A 320 × 200 screen of palette indices, row by row from the top left.
-}
type Framebuffer
    = Framebuffer (Array Int)


{-| A decoded patch (patch\_t): its header and, for each column, its posts (a run of
pixels starting `top` rows down).
-}
type Patch
    = Patch
        { width : Int
        , height : Int
        , leftoffset : Int
        , topoffset : Int
        , columns : Array (List Post)
        }


type alias Post =
    { top : Int
    , pixels : List Int
    }


{-| SCREENWIDTH.
-}
width : Int
width =
    320


{-| SCREENHEIGHT.
-}
height : Int
height =
    200


{-| A screen filled with one palette index.
-}
blank : Int -> Framebuffer
blank color =
    Framebuffer (Array.repeat (width * height) color)



-- PATCHES


{-| Decode a patch lump: a header (width, height, left and top offsets as shorts), a
table of column offsets, and each column's posts (topdelta, length, a padding byte,
the pixels, another padding byte), ended by a topdelta of 255.
-}
patch : Bytes -> Maybe Patch
patch bytes =
    let
        size =
            Bytes.width bytes

        headerDecoder =
            D.map4 (\w h l t -> { width = w, height = h, leftoffset = l, topoffset = t })
                (D.signedInt16 LE)
                (D.signedInt16 LE)
                (D.signedInt16 LE)
                (D.signedInt16 LE)
    in
    D.decode headerDecoder bytes
        |> Maybe.andThen
            (\h ->
                if h.width <= 0 || h.height <= 0 || 8 + 4 * h.width > size then
                    Nothing

                else
                    D.decode (D.map2 (\_ offs -> offs) (D.bytes 8) (list h.width (D.unsignedInt32 LE))) bytes
                        |> Maybe.andThen
                            (\offsets ->
                                offsets
                                    |> List.map (column bytes size)
                                    |> combine
                                    |> Maybe.map
                                        (\cols ->
                                            Patch
                                                { width = h.width
                                                , height = h.height
                                                , leftoffset = h.leftoffset
                                                , topoffset = h.topoffset
                                                , columns = Array.fromList cols
                                                }
                                        )
                            )
            )


column : Bytes -> Int -> Int -> Maybe (List Post)
column bytes size offset =
    if offset < 0 || offset >= size then
        Nothing

    else
        D.decode (D.map2 (\_ ps -> ps) (D.bytes offset) posts) bytes


posts : D.Decoder (List Post)
posts =
    D.loop []
        (\acc ->
            D.unsignedInt8
                |> D.andThen
                    (\top ->
                        if top == 255 then
                            D.succeed (D.Done (List.reverse acc))

                        else
                            D.unsignedInt8
                                |> D.andThen
                                    (\len ->
                                        D.map3 (\_ px _ -> D.Loop ({ top = top, pixels = px } :: acc))
                                            D.unsignedInt8
                                            (list len D.unsignedInt8)
                                            D.unsignedInt8
                                    )
                    )
        )


list : Int -> D.Decoder a -> D.Decoder (List a)
list n decoder =
    D.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\x -> D.Loop ( left - 1, x :: acc )) decoder
        )


combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldr (\m acc -> Maybe.map2 (::) m acc) (Just [])


{-| A patch's header.
-}
patchSize : Patch -> { width : Int, height : Int, leftoffset : Int, topoffset : Int }
patchSize (Patch p) =
    { width = p.width, height = p.height, leftoffset = p.leftoffset, topoffset = p.topoffset }



-- DRAWING


{-| V\_DrawPatch x y: the patch with its offsets applied (its origin at x, y).
-}
drawPatch : Int -> Int -> Patch -> Framebuffer -> Framebuffer
drawPatch x y p fb =
    drawPatchWith False x y identity p fb


{-| V\_DrawPatchFlipped: mirrored left to right.
-}
drawPatchFlipped : Int -> Int -> Patch -> Framebuffer -> Framebuffer
drawPatchFlipped x y p fb =
    drawPatchWith True x y identity p fb


{-| V\_DrawPatch with every pixel's index mapped through `translate` (a colour
translation or colormap).
-}
drawPatchTranslated : Int -> Int -> (Int -> Int) -> Patch -> Framebuffer -> Framebuffer
drawPatchTranslated x y translate p fb =
    drawPatchWith False x y translate p fb


drawPatchWith : Bool -> Int -> Int -> (Int -> Int) -> Patch -> Framebuffer -> Framebuffer
drawPatchWith flipped x0 y0 translate (Patch p) (Framebuffer px) =
    let
        left =
            x0 - p.leftoffset

        top =
            y0 - p.topoffset

        drawColumn col acc =
            let
                sx =
                    left + col

                source =
                    if flipped then
                        p.width - 1 - col

                    else
                        col
            in
            if sx < 0 || sx >= width then
                acc

            else
                case Array.get source p.columns of
                    Just colPosts ->
                        List.foldl (drawPost sx) acc colPosts

                    Nothing ->
                        acc

        drawPost sx post acc =
            List.foldl
                (\pixelIndex ( row, a ) ->
                    let
                        sy =
                            top + post.top + row
                    in
                    if sy < 0 || sy >= height then
                        ( row + 1, a )

                    else
                        ( row + 1, Array.set (sy * width + sx) (translate pixelIndex) a )
                )
                ( 0, acc )
                post.pixels
                |> Tuple.second
    in
    Framebuffer (List.foldl drawColumn px (List.range 0 (p.width - 1)))


{-| tileFlat x y w h flat: the 64 × 64 flat (4096 bytes) tiled over the rectangle,
aligned to the screen's 64-pixel grid, as F\_TextWrite and R\_FillBackScreen tile them.
-}
tileFlat : Int -> Int -> Int -> Int -> Bytes -> Framebuffer -> Framebuffer
tileFlat x y w h flat (Framebuffer px) =
    case D.decode (list 4096 D.unsignedInt8) flat of
        Just texels ->
            let
                tex =
                    Array.fromList texels

                setRow sy acc =
                    List.foldl
                        (\sx a ->
                            Array.set (sy * width + sx)
                                (Array.get (modBy 64 sy * 64 + modBy 64 sx) tex |> Maybe.withDefault 0)
                                a
                        )
                        acc
                        (List.range (max 0 x) (min width (x + w) - 1))
            in
            Framebuffer (List.foldl setRow px (List.range (max 0 y) (min height (y + h) - 1)))

        Nothing ->
            Framebuffer px


{-| fill x y w h color: a rectangle of one palette index.
-}
fill : Int -> Int -> Int -> Int -> Int -> Framebuffer -> Framebuffer
fill x y w h color (Framebuffer px) =
    let
        setRow sy acc =
            List.foldl (\sx a -> Array.set (sy * width + sx) color a)
                acc
                (List.range (max 0 x) (min width (x + w) - 1))
    in
    Framebuffer (List.foldl setRow px (List.range (max 0 y) (min height (y + h) - 1)))


{-| Set pixels (x, y, color), in order, skipping those off the screen.
-}
plot : List ( Int, Int, Int ) -> Framebuffer -> Framebuffer
plot points (Framebuffer px) =
    Framebuffer
        (List.foldl
            (\( x, y, c ) a ->
                if x < 0 || x >= width || y < 0 || y >= height then
                    a

                else
                    Array.set (y * width + x) c a
            )
            px
            points
        )


{-| V\_CopyRect srcx srcy w h src destx desty dest.
-}
copyRect : Int -> Int -> Int -> Int -> Framebuffer -> Int -> Int -> Framebuffer -> Framebuffer
copyRect sx sy w h src dx dy dest =
    let
        points =
            List.range 0 (h - 1)
                |> List.concatMap
                    (\row ->
                        List.range 0 (w - 1)
                            |> List.map (\col -> ( dx + col, dy + row, pixel (sx + col) (sy + row) src ))
                    )
    in
    plot points dest


{-| The index at (x, y), 0 off the screen.
-}
pixel : Int -> Int -> Framebuffer -> Int
pixel x y (Framebuffer px) =
    if x < 0 || x >= width || y < 0 || y >= height then
        0

    else
        Array.get (y * width + x) px |> Maybe.withDefault 0


{-| The screen as 64000 bytes, row by row from the top left.
-}
toBytes : Framebuffer -> Bytes
toBytes (Framebuffer px) =
    E.encode (E.sequence (Array.foldr (\c acc -> E.unsignedInt8 c :: acc) [] px))


{-| A screen from 64000 bytes.
-}
fromBytes : Bytes -> Maybe Framebuffer
fromBytes bytes =
    if Bytes.width bytes /= width * height then
        Nothing

    else
        D.decode (list (width * height) D.unsignedInt8) bytes
            |> Maybe.map (Array.fromList >> Framebuffer)
