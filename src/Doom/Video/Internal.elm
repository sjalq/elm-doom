module Doom.Video.Internal exposing (Framebuffer(..), columnCount, rowCount)

{-| The framebuffer's representation, shared by `Doom.Video` (2D drawing) and
`Doom.Render.View` (the 3D view): the 320 × 200 screen as 320 columns of 200 palette
indices each, top to bottom. The original draws walls, sprites and patches column by
column, so columns are what both mostly change; `Doom.Video.toBytes` turns them into
rows.
-}

import Array exposing (Array)


{-| 320 columns, left to right, each 200 palette indices from the top.
-}
type Framebuffer
    = Framebuffer (Array (Array Int))


{-| SCREENWIDTH.
-}
columnCount : Int
columnCount =
    320


{-| SCREENHEIGHT.
-}
rowCount : Int
rowCount =
    200
