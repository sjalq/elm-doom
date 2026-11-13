module Doom.Render.View exposing (Options, render, window, drawBorder)

{-| The 3D view (r\_main.c's R\_RenderPlayerView with r\_bsp.c, r\_segs.c, r\_plane.c,
r\_things.c, r\_draw.c, r\_sky.c), drawn into the framebuffer exactly as the original
draws it, and the view window and its border (R\_SetViewSize, R\_FillBackScreen,
R\_DrawViewBorder).

@docs Options, render, window, drawBorder

-}

import Doom.Level exposing (LineId)
import Doom.Play.Env exposing (Env)
import Doom.Render.Data exposing (RenderData)
import Doom.Video exposing (Framebuffer)
import Doom.World exposing (World)


{-| The view settings: `screenblocks` 3 to 11 (10: full width above the status bar,
11: the whole screen), `detailshift` 0 (high) or 1 (low detail), and the player whose
eyes it is.
-}
type alias Options =
    { screenblocks : Int
    , detailshift : Int
    , player : Int
    }


{-| R\_RenderPlayerView: draw the view into its window; also the lines it drew walls for (for ML\_MAPPED).
-}
render : RenderData -> Env -> World -> Options -> Framebuffer -> ( Framebuffer, List LineId, RenderData )
render a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Render.View.render"


{-| The view window for a screenblocks setting.
-}
window : Int -> { x : Int, y : Int, width : Int, height : Int }
window a0 =
    Debug.todo "Doom.Render.View.window"


{-| R\_FillBackScreen and R\_DrawViewBorder for a reduced view: the flat and bevel patches around the window.
-}
drawBorder : RenderData -> Int -> Framebuffer -> Framebuffer
drawBorder a0 a1 a2 =
    Debug.todo "Doom.Render.View.drawBorder"
