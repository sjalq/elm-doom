module Doom.Render.Scene exposing
    ( LevelMesh, SegMesh, SubsectorMesh, Point
    , Frame, Camera, SectorLook, PlaneLook(..), Wall, WallPart(..), WallLook(..), Sprite, WeaponSprite, Light(..)
    , projection, centerX, centerY, viewWidth, viewHeight, minZ
    , wallColormap, planeColormap, colormapFor, LightKind(..)
    , skyColumn, skyRow
    , paletteFor
    )

{-| What the renderer hands to whatever draws the picture: a GPU, or
`Doom.Render.Raster`.

A picture is two things:

  - a **`LevelMesh`**, made once per level (`Doom.Render.Build.levelMesh`): the map's
    walls (segs) as 2D line segments and its floors and ceilings (subsectors) as 2D
    convex polygons. None of this ever changes during the level.
  - a **`Frame`**, made every frame (`Doom.Render.Build.frame`): the camera, each
    sector's floor and ceiling height, flats and light, which wall pieces show which
    texture between which heights, the things' sprites and the player's weapon.

Everything is in **map units** (`Float`), Doom's own: x east, y north, z up. A wall
texture or flat texel is one map unit.

All pixels are **palette indices**. A frame is lit the way Doom lights it: a surface
has a light number (0 dark to 15 bright), which with the distance from the viewer
picks one of COLORMAP's 32 tables (`wallColormap`, `planeColormap`); the table turns
the texel's index into the index to show, and the frame's PLAYPAL palette (`palette`)
turns that into a colour. A GPU does that per fragment: upload each picture as an
8-bit index texture plus a transparency bit (`Doom.Render.Picture.toIndexBytes`,
`toAlphaBytes`), COLORMAP as a 256 × 34 texture and the 14 palettes as a 256 × 14
texture, then in the fragment shader:

```glsl
vec2 texel = texture2D(page, uv).ra;          // index / 255, opaque
if (texel.y < 0.5) discard;
float map = colormapFor(fixedColormap, kind, light, depth);  // 0..33
float shaded = texture2D(colormap, vec2(texel.x, map / 34.0)).r;
gl_FragColor = texture2D(playpal, vec2(shaded, palette / 14.0));
```

(sampling at texel centres), with `depth` the fragment's distance along the view
direction (view-space z, map units), and `nearest` filtering everywhere: no
smoothing, no mipmaps, so it looks like 1993.

Where a depth-buffered drawing of this can differ from the original's:

  - Doom wraps wall textures at the power of two at or below their width, and every
    wall texture at 128 rows, so oddly sized textures tile differently there; here
    they repeat at their own size.
  - Doom draws sprites over the floor they stand in; a depth test can cut off a
    sprite's lowest rows where its picture reaches below the floor.
  - Holes in a see-through texture made of several patches show garbage in Doom
    ("Medusa"); here they are clean holes.
  - Fuzz (`Shadow`) here darkens what's behind without Doom's jitter.


# The level

@docs LevelMesh, SegMesh, SubsectorMesh, Point


# A frame

@docs Frame, Camera, SectorLook, PlaneLook, Wall, WallPart, WallLook, Sprite, WeaponSprite, Light


# The view

@docs projection, centerX, centerY, viewWidth, viewHeight, minZ


# Light

@docs wallColormap, planeColormap, colormapFor, LightKind


# The sky

@docs skyColumn, skyRow


# The palette

@docs paletteFor

-}

import Array exposing (Array)



-- THE LEVEL


{-| The map's geometry, made once per level. `segs` and `subsectors` are indexed as
`Doom.Level`'s are (a `Wall`'s `seg`, a subsector's position).
-}
type alias LevelMesh =
    { segs : Array SegMesh
    , subsectors : Array SubsectorMesh
    }


{-| A point on the map.
-}
type alias Point =
    { x : Float
    , y : Float
    }


{-| A seg: a piece of a line's side, from (`x1`, `y1`) to (`x2`, `y2`), seen from its
right (the side its sector is on). `length` is its length.

Wall pieces on it are quads with corners, in this order (anticlockwise seen from the
front, so a GPU can cull back faces):

    (x1, y1, bottom), (x2, y2, bottom), (x2, y2, top), (x1, y1, top)

-}
type alias SegMesh =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , length : Float
    }


{-| A subsector (a BSP leaf): a convex polygon, anticlockwise seen from above, in
`sector`. Its floor is the polygon at the sector's floor height, seen from above
(anticlockwise); its ceiling the same polygon at the ceiling height, seen from below
(so a GPU culling back faces draws the ceiling's vertices in reverse order).
-}
type alias SubsectorMesh =
    { sector : Int
    , polygon : List Point
    }



-- A FRAME


{-| One frame.

  - `camera`: where it's seen from.
  - `palette`: which of PLAYPAL's 14 palettes to show it in (`paletteFor`).
  - `fixedColormap`: when set, every surface and sprite uses this COLORMAP table
    whatever its light and distance (1 for the light amplification visor, 32 for
    invulnerability), except the sky and shadow (fuzz) sprites.
  - `skyTexture`: the wall texture the sky shows (`SKY1`...).
  - `sectors`: each sector's floor and ceiling, indexed by sector number; subsectors
    take their heights, flats and light from here.
  - `walls`: the wall pieces to draw (a seg can have an upper, middle and lower piece).
  - `sprites`: the things.
  - `weapon`: the player's weapon and its muzzle flash, drawn over everything.

-}
type alias Frame =
    { camera : Camera
    , palette : Int
    , fixedColormap : Maybe Int
    , skyTexture : Int
    , sectors : Array SectorLook
    , walls : List Wall
    , sprites : List Sprite
    , weapon : List WeaponSprite
    }


{-| The viewpoint: the eye at (`x`, `y`, `z`) looking along `angle` (radians, 0 is
east, anticlockwise), level: Doom doesn't look up or down.

The projection is Doom's (r\_main.c, full screen at 320 × 200): a point at view-space
(`right`, `up`, `depth`) lands on screen pixel

    x =
        centerX + projection * right / depth

    y =
        centerY - projection * up / depth

with `projection` 160 for both axes, so the horizontal field of view is 90 degrees,
and the 320 × 200 picture is shown stretched to 4:3 (each pixel 1.2 times as tall as
it is wide), as on the original screen. Scale everything for other sizes.

-}
type alias Camera =
    { x : Float
    , y : Float
    , z : Float
    , angle : Float
    }


{-| A sector's floor and ceiling this frame: heights, what they show, and their
light number (0 to 15, with the player's `extralight` already added).
-}
type alias SectorLook =
    { floor : Float
    , ceiling : Float
    , floorPic : PlaneLook
    , ceilingPic : PlaneLook
    , light : Int
    }


{-| What a floor or ceiling shows.

  - `Flat n`: flat number n (`Doom.Data.flats`, animation already applied). Flats are
    64 × 64 and fixed to the map: the texel at map point (x, y) is column `x mod 64`,
    row `(-y) mod 64`. Lit with `planeColormap`.
  - `Sky`: the sky (see `skyColumn`), normally only on ceilings (F\_SKY1).

-}
type PlaneLook
    = Flat Int
    | Sky


{-| A wall piece: on seg `seg`, from height `bottom` to `top`, showing `look`, with
light number `light` (0 to 15: the sector's light, `extralight`, and Doom's "fake
contrast": one darker on walls running east-west, one brighter on walls running
north-south).
-}
type alias Wall =
    { seg : Int
    , part : WallPart
    , bottom : Float
    , top : Float
    , look : WallLook
    , light : Int
    }


{-| Which piece of the seg a `Wall` is: on a one-sided line, `Middle`; on a two-sided
line, `Upper` (between the two ceilings), `Lower` (between the two floors), and
`Middle` for a see-through middle texture (railings, grates).
-}
type WallPart
    = Upper
    | Middle
    | Lower


{-| What a wall piece shows.

  - `Textured`: wall texture `texture` (`Doom.Data.textures`, animation and switches
    already applied). The texel at a point `d` map units along the seg from (x1, y1)
    and at height `z` is

        column =
            (u + d) mod width

        row =
            (v + top - z) mod height

    `masked` pieces (a two-sided line's middle texture) have holes: draw them with
    transparency (`Doom.Render.Picture.composeTexture`), and they don't repeat up and
    down (`bottom` and `top` already span just the one copy). The others are solid
    (`composeSolidTexture`). Lit with `wallColormap`.

  - `SkyWall`: the sky. Doom shows the sky above the lower of two sky ceilings next
    to each other; this is that gap.

-}
type WallLook
    = Textured { texture : Int, u : Float, v : Float, masked : Bool }
    | SkyWall


{-| A thing's sprite: sprite lump number `lump` (its position among the sprite lumps,
S\_START to S\_END), mirrored if `flip`. It's a flat picture standing at (`x`, `y`)
facing the viewer square on: perpendicular to the camera's view direction, not
turned towards the eye. Its quad spans `left` to `right` map units along the
camera's right-hand direction (`(sin angle, -cos angle)`) from (`x`, `y`), and
`bottom` to `top` in height:

  - picture column 0 is at `left` (`right` if `flip`), row 0 at `top`.
  - all its pixels are at the same depth, the distance of (`x`, `y`) along the view
    direction (as Doom draws it).

`translation` is the player colour remap (0 none, 1 to 3:
`Doom.Render.Picture.translationTable`), applied to the texel before the light.

-}
type alias Sprite =
    { lump : Int
    , flip : Bool
    , x : Float
    , y : Float
    , left : Float
    , right : Float
    , bottom : Float
    , top : Float
    , light : Light
    , translation : Int
    }


{-| A weapon sprite, drawn on the screen over the 3D view, with no depth: sprite lump
`lump` with its top-left corner at (`x`, `centerY + y`) in 320 × 200 screen pixels,
one sprite pixel a screen pixel, mirrored if `flip`.
-}
type alias WeaponSprite =
    { lump : Int
    , flip : Bool
    , x : Float
    , y : Float
    , light : Light
    }


{-| How a sprite is lit.

  - `Lit n`: light number n (0 to 15), with `wallColormap` by its depth. A weapon
    sprite's light is already at depth 0's brightest table: draw it with
    `wallColormap n 0`.
  - `FullBright`: COLORMAP table 0 (frames marked bright: fireballs, lamps), unless
    the frame's `fixedColormap` is set.
  - `Shadow`: the spectre and invisible players: Doom's fuzz, which shows the picture
    behind darkened (COLORMAP table 6) and jittered, wherever the sprite has pixels.

-}
type Light
    = Lit Int
    | FullBright
    | Shadow



-- THE VIEW


{-| How many screen pixels one map unit is at a depth of one map unit (160).
-}
projection : Float
projection =
    160


{-| The screen column the view direction lands on (160).
-}
centerX : Float
centerX =
    160


{-| The screen row the eye's height lands on (100, full screen; 84 with the status bar
showing, as the 3D view is then 168 rows).
-}
centerY : Float
centerY =
    100


{-| The 3D view's size: 320 × 200.
-}
viewWidth : Int
viewWidth =
    320


{-| See `viewWidth`.
-}
viewHeight : Int
viewHeight =
    200


{-| The nearest depth anything is drawn at (Doom's MINZ is 4 map units, for sprites).
-}
minZ : Float
minZ =
    4



-- LIGHT


{-| Which of Doom's two light tables a surface uses: walls and sprites (`scalelight`,
by their scale on the screen) or floors and ceilings (`zlight`, by distance).
-}
type LightKind
    = WallLight
    | PlaneLight


{-| The COLORMAP table (0 brightest to 31 darkest) for a wall or sprite with light
number `light` at view depth `depth` (R\_ExecuteSetViewSize's `scalelight`, full
screen at 320 wide):

    startmap =
        (15 - light) * 4

    scale =
        min 47 (floor (2560 / depth))

    table =
        clamp 0 31 (startmap - scale / 2)

-}
wallColormap : Int -> Float -> Int
wallColormap light depth =
    let
        startmap =
            (15 - clamp 0 15 light) * 4

        scale =
            if depth <= 0 then
                47

            else
                min 47 (floor (2560 / depth))
    in
    clamp 0 31 (startmap - scale // 2)


{-| The COLORMAP table for a floor or ceiling with light number `light` at view depth
`depth` (R\_InitLightTables' `zlight`):

    startmap =
        (15 - light) * 4

    j =
        min 127 (floor (depth / 16))

    table =
        clamp 0 31 (startmap - floor (655360 / (j + 1) / 4096) / 2)

-}
planeColormap : Int -> Float -> Int
planeColormap light depth =
    let
        startmap =
            (15 - clamp 0 15 light) * 4

        j =
            clamp 0 127 (floor (depth / 16))

        scale =
            (655360 // (j + 1)) // 4096
    in
    clamp 0 31 (startmap - scale // 2)


{-| The COLORMAP table for a surface, everything considered: the frame's fixed
colormap first, then full-bright, then the light by depth. Nothing for `Shadow` (fuzz).
-}
colormapFor : Maybe Int -> LightKind -> Light -> Float -> Maybe Int
colormapFor fixed kind light depth =
    case light of
        Shadow ->
            Nothing

        _ ->
            case fixed of
                Just m ->
                    Just m

                Nothing ->
                    case light of
                        FullBright ->
                            Just 0

                        Lit n ->
                            case kind of
                                WallLight ->
                                    Just (wallColormap n depth)

                                PlaneLight ->
                                    Just (planeColormap n depth)

                        Shadow ->
                            Nothing



-- THE SKY


{-| The sky texture's column for a view ray at `angle` radians (anticlockwise from
east): Doom wraps the 256-column sky four times round the horizon
(`angle >> ANGLETOSKYSHIFT`, 1024 columns a turn), so it doesn't move with
perspective, only turning.

    column =
        floor (angle / (2 * pi) * 1024) mod 256

The sky is drawn full bright (COLORMAP table 0), even under a fixed colormap.

-}
skyColumn : Float -> Int
skyColumn angle =
    modBy 256 (floor (angle / (2 * pi) * 1024))


{-| The sky texture's row for screen row `y` (of 200, counted from the top of the
screen, full screen): the sky's row 100 is at the screen's centre, one texel a
screen row, whatever the distance (`skytexturemid` = 100, `pspriteiscale` = 1).

    row =
        (y - centerY + 100) mod 128

-}
skyRow : Float -> Int
skyRow y =
    modBy 128 (floor (y - centerY + 100))



-- THE PALETTE


{-| ST\_doPaletteStuff: which PLAYPAL palette the frame is shown in, from the player's
`damagecount`, `bonuscount`, berserk (`powers[pw_strength]`) and radiation suit
(`powers[pw_ironfeet]`) counters:

  - hurt (or berserk fading): red, 1 to 8, redder the more damage.
  - picked something up: gold, 9 to 12.
  - radiation suit (blinking when it's about to run out): green, 13.
  - otherwise 0.

-}
paletteFor : { damagecount : Int, bonuscount : Int, strength : Int, ironfeet : Int } -> Int
paletteFor p =
    let
        berserk =
            if p.strength /= 0 then
                max p.damagecount (12 - (p.strength // 64))

            else
                p.damagecount
    in
    if berserk /= 0 then
        1 + min 7 ((berserk + 7) // 8)

    else if p.bonuscount /= 0 then
        9 + min 3 ((p.bonuscount + 7) // 8)

    else if p.ironfeet > 4 * 32 || modBy 16 p.ironfeet >= 8 then
        13

    else
        0
