module Doom.Render.View exposing (Options, render, window, drawBorder)

{-| The 3D view (r\_main.c's R\_RenderPlayerView with r\_bsp.c, r\_segs.c, r\_plane.c,
r\_things.c, r\_draw.c, r\_sky.c), drawn into the framebuffer exactly as the original
draws it, and the view window and its border (R\_SetViewSize, R\_FillBackScreen,
R\_DrawViewBorder).

The original writes straight into the screen as it walks the BSP tree. Here the walk
(`Doom.Render.Walk`) collects, for every screen column, what is written to it, in the
order the original writes it; the columns are then put together and the framebuffer
gets them in one go. Pixels the view doesn't cover keep what the framebuffer had,
as they would in the original's `screens[0]` (the "hall of mirrors").

@docs Options, render, window, drawBorder

-}

import Array exposing (Array)
import Doom.Level exposing (LineId)
import Doom.Play.Env exposing (Env)
import Doom.Render.Compose as Compose
import Doom.Render.Data exposing (RenderData)
import Doom.Render.Internal as Internal exposing (RenderData(..))
import Doom.Render.Walk as Walk
import Doom.Video as Video exposing (Framebuffer)
import Doom.Video.Internal as VI
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


{-| R\_RenderPlayerView: draw the view into its window. Also gives the lines it drew
walls for (R\_StoreWallRange sets their ML\_MAPPED, for the automap), and the
`RenderData` to draw the next frame with: it has the lumps this frame read, the
view-size tables, and what the original keeps between frames (`fuzzpos`, the plane
row caches).

Draws nothing if the player has no body.

-}
render : RenderData -> Env -> World -> Options -> Framebuffer -> ( Framebuffer, List LineId, RenderData )
render renderData env world options fb =
    case Walk.walk renderData env world options of
        Nothing ->
            ( fb, [], renderData )

        Just frame ->
            ( Compose.apply frame.fuzzmap frame.columns fb, frame.mapped, frame.renderData )


{-| The view window for a screenblocks setting (R\_ExecuteSetViewSize and
R\_InitBuffer): where it is on the screen and how big. Low detail doesn't change it.
-}
window : Int -> { x : Int, y : Int, width : Int, height : Int }
window blocks =
    let
        ( w, h ) =
            if blocks >= 11 then
                ( 320, 200 )

            else
                ( blocks * 32, (blocks * 168 // 10) // 8 * 8 )
    in
    { x = (320 - w) // 2
    , y =
        if w == 320 then
            0

        else
            (200 - 32 - h) // 2
    , width = w
    , height = h
    }


{-| R\_FillBackScreen and R\_DrawViewBorder for a reduced view: the flat and bevel patches around the window.

R\_FillBackScreen tiles the flat (FLOOR7\_2, GRNROCK in Doom II) over the 168 rows
above the status bar of `screens[1]` and draws the bevel around the window there;
R\_DrawViewBorder copies everything of it outside the window to the screen. Nothing
happens for a full-width view.

-}
drawBorder : RenderData -> Int -> Framebuffer -> Framebuffer
drawBorder (RenderData static _) blocks fb =
    let
        win =
            window blocks
    in
    if win.width == 320 then
        fb

    else
        let
            lump name =
                static.borderPatches
                    |> List.filter (\( n, _ ) -> n == name)
                    |> List.head
                    |> Maybe.andThen Tuple.second
                    |> Maybe.andThen (\l -> Internal.lumpBytes static l)

            patch name =
                lump name |> Maybe.andThen Video.patch

            flat =
                static.backFlat
                    |> Maybe.andThen (Internal.lumpBytes static)

            back0 =
                case flat of
                    Just f ->
                        Video.tileFlat 0 0 320 168 f (Video.blank 0)

                    Nothing ->
                        Video.blank 0

            draw name xs ys b =
                case patch name of
                    Just p ->
                        List.foldl (\( x, y ) acc -> Video.drawPatch x y p acc) b (List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs)

                    Nothing ->
                        b

            steps from to =
                List.range 0 ((to - from - 1) // 8) |> List.map (\i -> from + 8 * i)

            back =
                back0
                    |> draw "BRDR_T" (steps 0 win.width |> List.map ((+) win.x)) [ win.y - 8 ]
                    |> draw "BRDR_B" (steps 0 win.width |> List.map ((+) win.x)) [ win.y + win.height ]
                    |> draw "BRDR_L" [ win.x - 8 ] (steps 0 win.height |> List.map ((+) win.y))
                    |> draw "BRDR_R" [ win.x + win.width ] (steps 0 win.height |> List.map ((+) win.y))
                    |> draw "BRDR_TL" [ win.x - 8 ] [ win.y - 8 ]
                    |> draw "BRDR_TR" [ win.x + win.width ] [ win.y - 8 ]
                    |> draw "BRDR_BL" [ win.x - 8 ] [ win.y + win.height ]
                    |> draw "BRDR_BR" [ win.x + win.width ] [ win.y + win.height ]

            (VI.Framebuffer backCols) =
                back

            (VI.Framebuffer cols) =
                fb

            -- every pixel of the 168 rows above the status bar outside the window
            -- (R_DrawViewBorder's R_VideoErase calls cover exactly those)
            outside x y =
                y < 168 && (y < win.y || y >= win.y + win.height || x < win.x || x >= win.x + win.width)
        in
        VI.Framebuffer
            (Array.indexedMap
                (\x col ->
                    case Array.get x backCols of
                        Just backCol ->
                            Array.indexedMap
                                (\y c ->
                                    if outside x y then
                                        Array.get y backCol |> Maybe.withDefault c

                                    else
                                        c
                                )
                                col

                        Nothing ->
                            col
                )
                cols
            )
