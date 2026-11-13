module Doom.Video.Internal exposing (Framebuffer(..), columnCount, rowCount, splice)

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


{-| Rows `top` to `bottom - 1` of a whole (200-row) column replaced by `rows`, which
has that many elements: the column cut around them and put back together, which
for more than a few rows is quicker than setting them one at a time.
-}
splice : Int -> Int -> Array Int -> Array Int -> Array Int
splice top bottom rows col =
    if top == 0 && bottom == rowCount then
        rows

    else if top == 0 then
        Array.append rows (Array.slice bottom rowCount col)

    else if bottom == rowCount then
        Array.append (Array.slice 0 top col) rows

    else
        Array.append (Array.append (Array.slice 0 top col) rows) (Array.slice bottom rowCount col)
