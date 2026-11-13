module Doom.Render.Raster exposing
    ( Framebuffer, Images
    , render
    , blankFramebuffer, drawPicture
    )

{-| A reference software rasterizer: draws a `Doom.Render.Scene` frame into a
palette-indexed framebuffer, with a depth buffer, perspective-correct texturing and
Doom's light (COLORMAP by light level and distance). It is what a GPU drawing the
scene should match, for tests and screenshots; it is not fast.

Every surface is a convex polygon: it is moved into view space, cut at the near
plane, projected, and filled row by row. A pixel is drawn where its centre is inside
(left and top edges in, right and bottom edges out, so neighbouring polygons neither
overlap nor leave gaps), nearer than what's there, and not transparent.

@docs Framebuffer, Images
@docs render
@docs blankFramebuffer, drawPicture

-}

import Array exposing (Array)
import Doom.Render.Picture as Picture exposing (Colormaps, Picture)
import Doom.Render.Scene as Scene exposing (Frame, LevelMesh, Light(..), LightKind(..), PlaneLook(..), WallLook(..))



-- TYPES


{-| A picture of palette indices, row by row (as `Doom.Render.Picture`'s, with no
transparent pixels).
-}
type alias Framebuffer =
    { width : Int
    , height : Int
    , pixels : Array Int
    }


{-| The pictures a frame refers to, by number, and COLORMAP.

  - `texture`: a wall texture with holes (`Picture.composeTexture`), for see-through
    middles.
  - `solidTexture`: a wall texture as solid walls show it
    (`Picture.composeSolidTexture`), and the sky.
  - `flat`, `sprite`: by flat number and sprite lump number.

-}
type alias Images =
    { texture : Int -> Maybe Picture
    , solidTexture : Int -> Maybe Picture
    , flat : Int -> Maybe Picture
    , sprite : Int -> Maybe Picture
    , colormaps : Colormaps
    }


{-| A framebuffer of index 0 (black).
-}
blankFramebuffer : Int -> Int -> Framebuffer
blankFramebuffer w h =
    { width = w, height = h, pixels = Array.repeat (w * h) 0 }


{-| Draw a picture at (x, y), one pixel a pixel, skipping its transparent pixels
(V\_DrawPatch, without the patch's offsets).
-}
drawPicture : Int -> Int -> Picture -> Framebuffer -> Framebuffer
drawPicture x0 y0 pic fb =
    let
        rows =
            List.range 0 (pic.height - 1)

        cols =
            List.range 0 (pic.width - 1)
    in
    { fb
        | pixels =
            List.foldl
                (\py acc ->
                    List.foldl
                        (\px a ->
                            let
                                p =
                                    Picture.pixel px py pic

                                x =
                                    x0 + px

                                y =
                                    y0 + py
                            in
                            if p >= 0 && x >= 0 && y >= 0 && x < fb.width && y < fb.height then
                                Array.set (y * fb.width + x) p a

                            else
                                a
                        )
                        acc
                        cols
                )
                fb.pixels
                rows
    }



-- PRIMITIVES


{-| A vertex in view space: `right`, `up` and `depth` from the eye, and texture
coordinates in texels.
-}
type alias Vertex =
    { right : Float
    , up : Float
    , depth : Float
    , u : Float
    , v : Float
    }


type Shade
    = Textured
        { pic : Picture
        , wrapU : Bool
        , wrapV : Bool
        , light : Light
        , kind : LightKind
        , translation : Maybe (Array Int)
        }
    | SkyShade Picture


type alias Prim =
    { verts : List Vertex
    , shade : Shade
    }


type alias View =
    { camera : Scene.Camera
    , cos : Float
    , sin : Float
    }


toView : View -> Float -> Float -> Float -> Float -> Float -> Vertex
toView view x y z u v =
    let
        dx =
            x - view.camera.x

        dy =
            y - view.camera.y
    in
    { right = dx * view.sin - dy * view.cos
    , up = z - view.camera.z
    , depth = dx * view.cos + dy * view.sin
    , u = u
    , v = v
    }



-- RENDER


{-| Draw a frame at `width` × `height` (320 × 200 is Doom's own size; others are
scaled to it): walls, floors, ceilings, sky and sprites with the depth buffer, then
shadow (fuzz) sprites over them, then the weapon.
-}
render : Int -> Int -> Images -> LevelMesh -> Frame -> Framebuffer
render width height images mesh frame =
    let
        view =
            { camera = frame.camera, cos = cos frame.camera.angle, sin = sin frame.camera.angle }

        skyPic =
            images.solidTexture frame.skyTexture |> Maybe.withDefault Picture.empty

        prims =
            wallPrims images mesh frame view skyPic
                ++ planePrims images mesh frame view skyPic
                ++ spritePrims images frame view False

        shadows =
            spritePrims images frame view True

        target =
            { width = width
            , height = height
            , color = Array.repeat (width * height) 0
            , depth = Array.repeat (width * height) 1.0e30
            }

        ctx =
            { width = width
            , height = height
            , sx = toFloat width / 320
            , sy = toFloat height / 200
            , frame = frame
            , colormaps = images.colormaps
            }

        drawn =
            List.foldl (rasterize ctx) target prims

        fuzzed =
            List.foldl (rasterize ctx) drawn shadows

        withWeapon =
            List.foldl (drawWeapon ctx images) fuzzed.color frame.weapon
    in
    { width = width, height = height, pixels = withWeapon }


wallPrims : Images -> LevelMesh -> Frame -> View -> Picture -> List Prim
wallPrims images mesh frame view skyPic =
    List.filterMap
        (\w ->
            Array.get w.seg mesh.segs
                |> Maybe.andThen
                    (\s ->
                        let
                            -- Seen from the front (right) side only.
                            facing =
                                (s.x2 - s.x1) * (view.camera.y - s.y1) - (s.y2 - s.y1) * (view.camera.x - s.x1) < 0

                            corners u0 v0 =
                                let
                                    h =
                                        w.top - w.bottom
                                in
                                [ toView view s.x1 s.y1 w.bottom u0 (v0 + h)
                                , toView view s.x2 s.y2 w.bottom (u0 + s.length) (v0 + h)
                                , toView view s.x2 s.y2 w.top (u0 + s.length) v0
                                , toView view s.x1 s.y1 w.top u0 v0
                                ]
                        in
                        if not facing then
                            Nothing

                        else
                            case w.look of
                                SkyWall ->
                                    Just { verts = corners 0 0, shade = SkyShade skyPic }

                                Scene.Textured t ->
                                    let
                                        pic =
                                            if t.masked then
                                                images.texture t.texture

                                            else
                                                images.solidTexture t.texture
                                    in
                                    pic
                                        |> Maybe.map
                                            (\p ->
                                                { verts = corners t.u t.v
                                                , shade =
                                                    Textured
                                                        { pic = p
                                                        , wrapU = True
                                                        , wrapV = not t.masked
                                                        , light = Lit w.light
                                                        , kind = WallLight
                                                        , translation = Nothing
                                                        }
                                                }
                                            )
                    )
        )
        frame.walls


planePrims : Images -> LevelMesh -> Frame -> View -> Picture -> List Prim
planePrims images mesh frame view skyPic =
    mesh.subsectors
        |> Array.toList
        |> List.concatMap
            (\ss ->
                case Array.get ss.sector frame.sectors of
                    Nothing ->
                        []

                    Just sec ->
                        let
                            plane z look reverse =
                                let
                                    pts =
                                        List.map (\p -> toView view p.x p.y z p.x -p.y) ss.polygon

                                    verts =
                                        if reverse then
                                            List.reverse pts

                                        else
                                            pts
                                in
                                case look of
                                    Sky ->
                                        [ { verts = verts, shade = SkyShade skyPic } ]

                                    Flat n ->
                                        case images.flat n of
                                            Just pic ->
                                                [ { verts = verts
                                                  , shade =
                                                        Textured
                                                            { pic = pic
                                                            , wrapU = True
                                                            , wrapV = True
                                                            , light = Lit sec.light
                                                            , kind = PlaneLight
                                                            , translation = Nothing
                                                            }
                                                  }
                                                ]

                                            Nothing ->
                                                []

                            floor_ =
                                if sec.floor < frame.camera.z then
                                    plane sec.floor sec.floorPic False

                                else
                                    []

                            ceiling =
                                if sec.ceiling > frame.camera.z then
                                    plane sec.ceiling sec.ceilingPic True

                                else
                                    []
                        in
                        floor_ ++ ceiling
            )


spritePrims : Images -> Frame -> View -> Bool -> List Prim
spritePrims images frame view shadows =
    frame.sprites
        |> List.filter (\s -> (s.light == Shadow) == shadows)
        |> List.filterMap
            (\s ->
                images.sprite s.lump
                    |> Maybe.andThen
                        (\pic ->
                            let
                                -- The camera's right-hand direction.
                                rx =
                                    view.sin

                                ry =
                                    -view.cos

                                w =
                                    toFloat pic.width

                                h =
                                    toFloat pic.height

                                ( uLeft, uRight ) =
                                    if s.flip then
                                        ( w, 0 )

                                    else
                                        ( 0, w )

                                corner lateral z u v =
                                    toView view (s.x + rx * lateral) (s.y + ry * lateral) z u v

                                centre =
                                    toView view s.x s.y 0 0 0
                            in
                            if centre.depth < Scene.minZ then
                                Nothing

                            else
                                Just
                                    { verts =
                                        [ corner s.left s.bottom uLeft h
                                        , corner s.right s.bottom uRight h
                                        , corner s.right s.top uRight 0
                                        , corner s.left s.top uLeft 0
                                        ]
                                    , shade =
                                        Textured
                                            { pic = pic
                                            , wrapU = False
                                            , wrapV = False
                                            , light = s.light
                                            , kind = WallLight
                                            , translation =
                                                if s.translation > 0 then
                                                    Just (Picture.translationTable s.translation)

                                                else
                                                    Nothing
                                            }
                                    }
                        )
            )



-- RASTERIZING


type alias Target =
    { width : Int
    , height : Int
    , color : Array Int
    , depth : Array Float
    }


type alias Context =
    { width : Int
    , height : Int
    , sx : Float
    , sy : Float
    , frame : Frame
    , colormaps : Colormaps
    }


nearPlane : Float
nearPlane =
    1


{-| Sutherland-Hodgman against depth >= nearPlane.
-}
clipNear : List Vertex -> List Vertex
clipNear verts =
    case verts of
        [] ->
            []

        first :: _ ->
            List.map2 Tuple.pair verts (List.drop 1 verts ++ [ first ])
                |> List.concatMap
                    (\( a, b ) ->
                        let
                            ina =
                                a.depth >= nearPlane

                            inb =
                                b.depth >= nearPlane

                            cut () =
                                let
                                    t =
                                        (nearPlane - a.depth) / (b.depth - a.depth)

                                    lerp f =
                                        f a + (f b - f a) * t
                                in
                                { right = lerp .right, up = lerp .up, depth = nearPlane, u = lerp .u, v = lerp .v }
                        in
                        case ( ina, inb ) of
                            ( True, True ) ->
                                [ a ]

                            ( True, False ) ->
                                [ a, cut () ]

                            ( False, True ) ->
                                [ cut () ]

                            ( False, False ) ->
                                []
                    )


{-| A projected vertex: screen position, and 1/depth, u/depth, v/depth (which are
affine on the screen).
-}
type alias Screen =
    { x : Float, y : Float, iz : Float, uz : Float, vz : Float }


project : Context -> Vertex -> Screen
project ctx v =
    let
        iz =
            1 / v.depth
    in
    { x = ctx.sx * (Scene.centerX + Scene.projection * v.right * iz)
    , y = ctx.sy * (Scene.centerY - Scene.projection * v.up * iz)
    , iz = iz
    , uz = v.u * iz
    , vz = v.v * iz
    }


type alias Gradient =
    { x0 : Float, y0 : Float, a0 : Float, dx : Float, dy : Float }


{-| The screen-affine attribute through three projected vertices.
-}
gradient : Screen -> Screen -> Screen -> (Screen -> Float) -> Gradient
gradient p0 p1 p2 f =
    let
        det =
            (p1.x - p0.x) * (p2.y - p0.y) - (p2.x - p0.x) * (p1.y - p0.y)

        a0 =
            f p0

        a1 =
            f p1 - a0

        a2 =
            f p2 - a0
    in
    { x0 = p0.x
    , y0 = p0.y
    , a0 = a0
    , dx = (a1 * (p2.y - p0.y) - a2 * (p1.y - p0.y)) / det
    , dy = (a2 * (p1.x - p0.x) - a1 * (p2.x - p0.x)) / det
    }


at : Gradient -> Float -> Float -> Float
at g x y =
    g.a0 + g.dx * (x - g.x0) + g.dy * (y - g.y0)


{-| The three vertices spanning the largest triangle (for well-conditioned
gradients).
-}
bestTriangle : List Screen -> Maybe ( Screen, Screen, Screen )
bestTriangle pts =
    case pts of
        p0 :: rest ->
            let
                area a b =
                    abs ((a.x - p0.x) * (b.y - p0.y) - (b.x - p0.x) * (a.y - p0.y))

                candidates =
                    List.concatMap (\a -> List.map (\b -> ( area a b, a, b )) rest) rest

                best =
                    List.foldl
                        (\( ar, a, b ) acc ->
                            case acc of
                                Just ( bestArea, _, _ ) ->
                                    if ar > bestArea then
                                        Just ( ar, a, b )

                                    else
                                        acc

                                Nothing ->
                                    Just ( ar, a, b )
                        )
                        Nothing
                        candidates
            in
            case best of
                Just ( ar, a, b ) ->
                    if ar > 1.0e-9 then
                        Just ( p0, a, b )

                    else
                        Nothing

                Nothing ->
                    Nothing

        [] ->
            Nothing


rasterize : Context -> Prim -> Target -> Target
rasterize ctx prim target =
    let
        pts =
            List.map (project ctx) (clipNear prim.verts)
    in
    case bestTriangle pts of
        Nothing ->
            target

        Just ( p0, p1, p2 ) ->
            let
                giz =
                    gradient p0 p1 p2 .iz

                guz =
                    gradient p0 p1 p2 .uz

                gvz =
                    gradient p0 p1 p2 .vz

                ys =
                    List.map .y pts

                ymin =
                    max 0 (ceiling ((List.minimum ys |> Maybe.withDefault 0) - 0.5))

                ymax =
                    min (ctx.height - 1) (ceiling ((List.maximum ys |> Maybe.withDefault 0) - 0.5) - 1)

                edges =
                    case pts of
                        first :: _ ->
                            List.map2 Tuple.pair pts (List.drop 1 pts ++ [ first ])

                        [] ->
                            []

                shadePixel =
                    shader ctx prim.shade

                row y t =
                    let
                        yc =
                            toFloat y + 0.5

                        xs =
                            List.filterMap
                                (\( a, b ) ->
                                    if (a.y <= yc && yc < b.y) || (b.y <= yc && yc < a.y) then
                                        Just (a.x + (yc - a.y) / (b.y - a.y) * (b.x - a.x))

                                    else
                                        Nothing
                                )
                                edges
                    in
                    case ( List.minimum xs, List.maximum xs ) of
                        ( Just xl, Just xr ) ->
                            let
                                x0 =
                                    max 0 (ceiling (xl - 0.5))

                                x1 =
                                    min (ctx.width - 1) (ceiling (xr - 0.5) - 1)
                            in
                            span ctx giz guz gvz shadePixel y yc x0 x1 t

                        _ ->
                            t
            in
            List.foldl row target (List.range ymin ymax)


span :
    Context
    -> Gradient
    -> Gradient
    -> Gradient
    -> (Int -> Int -> Float -> Float -> Float -> Int -> Maybe Int)
    -> Int
    -> Float
    -> Int
    -> Int
    -> Target
    -> Target
span ctx giz guz gvz shadePixel y yc x x1 t =
    if x > x1 then
        t

    else
        let
            xc =
                toFloat x + 0.5

            iz =
                at giz xc yc

            depth =
                1 / iz

            i =
                y * ctx.width + x

            behind =
                case Array.get i t.depth of
                    Just d ->
                        depth >= d

                    Nothing ->
                        True

            next =
                if behind || iz <= 0 then
                    t

                else
                    let
                        u =
                            at guz xc yc * depth

                        v =
                            at gvz xc yc * depth

                        current =
                            Array.get i t.color |> Maybe.withDefault 0
                    in
                    case shadePixel x y u v depth current of
                        Just c ->
                            { t | color = Array.set i c t.color, depth = Array.set i depth t.depth }

                        Nothing ->
                            t
        in
        span ctx giz guz gvz shadePixel y yc (x + 1) x1 next


{-| The colour of a pixel of this surface, given the pixel, its texture coordinates,
depth and the colour already there; Nothing where the surface is transparent.
-}
shader : Context -> Shade -> Int -> Int -> Float -> Float -> Float -> Int -> Maybe Int
shader ctx shade =
    case shade of
        SkyShade pic ->
            \x y _ _ _ _ ->
                let
                    -- The view ray's angle, in Doom's 320-column screen.
                    sx =
                        (toFloat x + 0.5) / ctx.sx

                    angle =
                        ctx.frame.camera.angle + atan ((Scene.centerX - sx) / Scene.projection)

                    col =
                        Scene.skyColumn angle

                    row =
                        Scene.skyRow ((toFloat y + 0.5) / ctx.sy)

                    p =
                        if pic.width > 0 && pic.height > 0 then
                            Picture.pixel (modBy pic.width col) (modBy pic.height row) pic

                        else
                            0
                in
                Just (Picture.colormapRow ctx.colormaps 0 (max 0 p))

        Textured t ->
            let
                w =
                    t.pic.width

                h =
                    t.pic.height
            in
            \x y u v depth current ->
                let
                    ui =
                        floor u

                    vi =
                        floor v

                    uu =
                        if t.wrapU then
                            modBy (max 1 w) ui

                        else
                            ui

                    vv =
                        if t.wrapV then
                            modBy (max 1 h) vi

                        else
                            vi

                    p =
                        Picture.pixel uu vv t.pic
                in
                if p < 0 then
                    Nothing

                else
                    case t.light of
                        Shadow ->
                            -- Fuzz: what is behind, darkened (Doom also jitters it by a row).
                            Just (Picture.colormapRow ctx.colormaps 6 current)

                        _ ->
                            let
                                translated =
                                    case t.translation of
                                        Just table ->
                                            Array.get p table |> Maybe.withDefault p

                                        Nothing ->
                                            p
                            in
                            case Scene.colormapFor ctx.frame.fixedColormap t.kind t.light depth of
                                Just m ->
                                    Just (Picture.colormapRow ctx.colormaps m translated)

                                Nothing ->
                                    Just translated


{-| R\_DrawPSprite: a weapon sprite over the view, no depth test.
-}
drawWeapon : Context -> Images -> Scene.WeaponSprite -> Array Int -> Array Int
drawWeapon ctx images ws pixels =
    case images.sprite ws.lump of
        Nothing ->
            pixels

        Just pic ->
            let
                left =
                    ws.x

                top =
                    Scene.centerY + ws.y

                x0 =
                    max 0 (ceiling (left * ctx.sx - 0.5))

                x1 =
                    min (ctx.width - 1) (ceiling ((left + toFloat pic.width) * ctx.sx - 0.5) - 1)

                y0 =
                    max 0 (ceiling (top * ctx.sy - 0.5))

                y1 =
                    min (ctx.height - 1) (ceiling ((top + toFloat pic.height) * ctx.sy - 0.5) - 1)

                colormap =
                    Scene.colormapFor ctx.frame.fixedColormap WallLight ws.light 0
            in
            List.foldl
                (\y acc ->
                    List.foldl
                        (\x a ->
                            let
                                u0 =
                                    floor ((toFloat x + 0.5) / ctx.sx - left)

                                u =
                                    if ws.flip then
                                        pic.width - 1 - u0

                                    else
                                        u0

                                v =
                                    floor ((toFloat y + 0.5) / ctx.sy - top)

                                p =
                                    Picture.pixel u v pic

                                i =
                                    y * ctx.width + x
                            in
                            if p < 0 then
                                a

                            else
                                case colormap of
                                    Just m ->
                                        Array.set i (Picture.colormapRow ctx.colormaps m p) a

                                    Nothing ->
                                        Array.set i (Picture.colormapRow ctx.colormaps 6 (Array.get i a |> Maybe.withDefault 0)) a
                        )
                        acc
                        (List.range x0 x1)
                )
                pixels
                (List.range y0 y1)
