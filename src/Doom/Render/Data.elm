module Doom.Render.Data exposing (RenderData, load)

{-| What the 3D view is drawn from (r\_data.c, r\_sky.c, r\_things.c's R\_InitSprites, r\_draw.c's translation tables): composed wall textures and their column lookups, flats, COLORMAP, sprite definitions with their rotations, the fuzz table. Built once per WAD.

@docs RenderData, load

-}

import Doom.Data exposing (Data)
import Doom.Wad exposing (Wad)


{-| The renderer's tables.
-}
type RenderData
    = RenderData


{-| R\_InitData, R\_InitSprites, R\_InitTranslationTables and friends.
-}
load : Wad -> Data -> RenderData
load a0 a1 =
    Debug.todo "Doom.Render.Data.load"
