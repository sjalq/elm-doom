module Doom.Video exposing (Framebuffer, Patch, width, height, blank, patch, patchSize, drawPatch, drawPatchFlipped, drawPatchTranslated, tileFlat, fill, plot, pixel, copyRect, toBytes, fromBytes)

{-| The screen and drawing on it (v\_video.c): a 320 × 200 framebuffer of palette
indices, patches (the WAD's picture format), and the primitives everything 2D is drawn
with: patches, tiled flats, rectangles, single pixels. The 3D view is drawn into the
same framebuffer by `Doom.Render.View`.

@docs Framebuffer, Patch, width, height, blank, patch, patchSize, drawPatch, drawPatchFlipped, drawPatchTranslated, tileFlat, fill, plot, pixel, copyRect, toBytes, fromBytes

-}

import Bytes exposing (Bytes)


{-| A 320 × 200 screen of palette indices.
-}
type Framebuffer
    = Framebuffer


{-| A decoded patch (patch\_t): width, height, offsets and its columns of posts.
-}
type Patch
    = Patch


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
blank a0 =
    Debug.todo "Doom.Video.blank"


{-| Decode a patch lump.
-}
patch : Bytes -> Maybe Patch
patch a0 =
    Debug.todo "Doom.Video.patch"


{-| A patch's header.
-}
patchSize : Patch -> { width : Int, height : Int, leftoffset : Int, topoffset : Int }
patchSize a0 =
    Debug.todo "Doom.Video.patchSize"


{-| V\_DrawPatch x y: the patch with its offsets applied, clipped to the screen.
-}
drawPatch : Int -> Int -> Patch -> Framebuffer -> Framebuffer
drawPatch a0 a1 a2 a3 =
    Debug.todo "Doom.Video.drawPatch"


{-| V\_DrawPatchFlipped.
-}
drawPatchFlipped : Int -> Int -> Patch -> Framebuffer -> Framebuffer
drawPatchFlipped a0 a1 a2 a3 =
    Debug.todo "Doom.Video.drawPatchFlipped"


{-| V\_DrawPatch with every pixel's index mapped (a colour translation).
-}
drawPatchTranslated : Int -> Int -> (Int -> Int) -> Patch -> Framebuffer -> Framebuffer
drawPatchTranslated a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Video.drawPatchTranslated"


{-| tileFlat x y w h flat: the 64 × 64 flat tiled over the rectangle, aligned to the screen's 64-pixel grid (F\_TextWrite, R\_FillBackScreen).
-}
tileFlat : Int -> Int -> Int -> Int -> Bytes -> Framebuffer -> Framebuffer
tileFlat a0 a1 a2 a3 a4 a5 =
    Debug.todo "Doom.Video.tileFlat"


{-| fill x y w h color (V\_FillRect-like).
-}
fill : Int -> Int -> Int -> Int -> Int -> Framebuffer -> Framebuffer
fill a0 a1 a2 a3 a4 a5 =
    Debug.todo "Doom.Video.fill"


{-| Set pixels (x, y, color), in order, skipping those off the screen.
-}
plot : List ( Int, Int, Int ) -> Framebuffer -> Framebuffer
plot a0 a1 =
    Debug.todo "Doom.Video.plot"


{-| The index at (x, y).
-}
pixel : Int -> Int -> Framebuffer -> Int
pixel a0 a1 a2 =
    Debug.todo "Doom.Video.pixel"


{-| V\_CopyRect srcx srcy w h src destx desty dest.
-}
copyRect : Int -> Int -> Int -> Int -> Framebuffer -> Int -> Int -> Framebuffer -> Framebuffer
copyRect a0 a1 a2 a3 a4 a5 a6 a7 =
    Debug.todo "Doom.Video.copyRect"


{-| The screen as 64000 bytes, row by row from the top left.
-}
toBytes : Framebuffer -> Bytes
toBytes a0 =
    Debug.todo "Doom.Video.toBytes"


{-| A screen from 64000 bytes.
-}
fromBytes : Bytes -> Maybe Framebuffer
fromBytes a0 =
    Debug.todo "Doom.Video.fromBytes"
