module Doom.Render.Build exposing
    ( levelMesh, subsectorPolygon
    , Input, Translations, noTranslations, specTranslations
    , SpriteInfo, spriteInfo
    , Viewpoint, viewpoint, frame
    , skyTextureName
    )

{-| Make the scene (`Doom.Render.Scene`) from the game: once per level the
`LevelMesh`, every frame a `Frame`.

This is the job of r\_bsp.c, r\_segs.c, r\_plane.c and r\_things.c, without the
drawing: which wall pieces show which texture between which heights, how the
textures are pegged, how bright everything is, which sprite rotation each thing
shows, where the weapon is, and which palette the frame is in. Hidden surfaces are
left to the depth buffer.


# The level

@docs levelMesh, subsectorPolygon


# A frame

@docs Input, Translations, noTranslations, specTranslations
@docs SpriteInfo, spriteInfo
@docs Viewpoint, viewpoint, frame
@docs skyTextureName

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Doom.Angle exposing (Angle)
import Doom.Data as Data exposing (Data)
import Doom.Defs as Defs exposing (GameMode(..), Power(..))
import Doom.Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Info.State as State
import Doom.Level as Level exposing (Child(..), Level)
import Doom.Mobj as Mobj exposing (Mobj)
import Doom.Play.Env exposing (Env)
import Doom.Play.Spec as Spec
import Doom.Player as Player exposing (Player, PspDef)
import Doom.Render.Scene as Scene exposing (LevelMesh, Light(..), PlaneLook(..), Point, SubsectorMesh, Wall, WallLook(..), WallPart(..))
import Doom.Render.SpriteFrames as SpriteFrames exposing (SpriteFrames)
import Doom.World as World exposing (SectorState, World)



-- NUMBERS


fixed : Fixed -> Float
fixed f =
    toFloat f / 65536


{-| A binary angle in radians.
-}
radians : Angle -> Float
radians a =
    toFloat a / 4294967296 * 2 * pi


{-| Radians as a binary angle.
-}
binaryAngle : Float -> Angle
binaryAngle r =
    let
        turns =
            r / (2 * pi)
    in
    modBy 4294967296 (floor ((turns - toFloat (floor turns)) * 4294967296))



-- THE LEVEL


{-| The level's static geometry: every seg as a line segment, every subsector as its
convex polygon.
-}
levelMesh : Level -> LevelMesh
levelMesh level =
    let
        polygons =
            subsectorPolygons level
    in
    { segs =
        Array.map
            (\s ->
                let
                    x1 =
                        fixed s.v1.x

                    y1 =
                        fixed s.v1.y

                    x2 =
                        fixed s.v2.x

                    y2 =
                        fixed s.v2.y
                in
                { x1 = x1, y1 = y1, x2 = x2, y2 = y2, length = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) }
            )
            level.segs
    , subsectors =
        Array.indexedMap
            (\i ss ->
                { sector = ss.sector
                , polygon = Array.get i polygons |> Maybe.withDefault []
                }
            )
            level.subsectors
    }


{-| A half-plane: the points on the right of the line through (`x`, `y`) going
(`dx`, `dy`), or on its left.
-}
type alias HalfPlane =
    { x : Float, y : Float, dx : Float, dy : Float, right : Bool }


{-| Every subsector's polygon: a box round the whole map, cut by each BSP partition
line on the way down from the root to the subsector (keeping the side the subsector
is on: a node's first child is on the partition line's right), then by the
subsector's own segs (it is on their right).
-}
subsectorPolygons : Level -> Array (List Point)
subsectorPolygons level =
    let
        box =
            mapBox level

        leaves =
            if Array.isEmpty level.nodes then
                [ ( 0, [] ) ]

            else
                descend level (Array.length level.nodes - 1) [] []

        byIndex =
            List.foldl (\( ss, planes ) acc -> Array.set ss (Just planes) acc)
                (Array.repeat (Array.length level.subsectors) Nothing)
                leaves
    in
    Array.indexedMap
        (\i planes ->
            subsectorPolygon level i (Maybe.withDefault [] planes) box
        )
        byIndex


{-| The polygon of subsector `i`: `box` cut by these half-planes, then by the
subsector's segs.
-}
subsectorPolygon : Level -> Int -> List HalfPlane -> List Point -> List Point
subsectorPolygon level i planes box =
    let
        ss =
            Level.subsector i level

        segPlanes =
            List.range ss.firstline (ss.firstline + ss.numlines - 1)
                |> List.map (\s -> Level.seg s level)
                |> List.map
                    (\s ->
                        { x = fixed s.v1.x
                        , y = fixed s.v1.y
                        , dx = fixed (s.v2.x - s.v1.x)
                        , dy = fixed (s.v2.y - s.v1.y)
                        , right = True
                        }
                    )
                |> List.filter (\h -> h.dx /= 0 || h.dy /= 0)
    in
    List.foldl clip box (planes ++ segPlanes)


descend : Level -> Int -> List HalfPlane -> List ( Int, List HalfPlane ) -> List ( Int, List HalfPlane )
descend level n planes acc =
    let
        node =
            Level.node n level

        line right =
            { x = fixed node.x, y = fixed node.y, dx = fixed node.dx, dy = fixed node.dy, right = right }

        ( front, back ) =
            node.children

        visit child half a =
            case child of
                ChildNode m ->
                    descend level m (half :: planes) a

                ChildSubsector s ->
                    ( s, List.reverse (half :: planes) ) :: a
    in
    acc
        |> visit front (line True)
        |> visit back (line False)


mapBox : Level -> List Point
mapBox level =
    let
        xs =
            Array.toList level.vertexes |> List.map (.x >> fixed)

        ys =
            Array.toList level.vertexes |> List.map (.y >> fixed)

        lo l =
            (List.minimum l |> Maybe.withDefault 0) - 64

        hi l =
            (List.maximum l |> Maybe.withDefault 0) + 64
    in
    [ { x = lo xs, y = lo ys }, { x = hi xs, y = lo ys }, { x = hi xs, y = hi ys }, { x = lo xs, y = hi ys } ]


{-| Sutherland-Hodgman: the part of the convex polygon on the half-plane's side.
-}
clip : HalfPlane -> List Point -> List Point
clip h poly =
    let
        -- > 0: the point is on the side kept.
        side p =
            let
                c =
                    h.dx * (p.y - h.y) - h.dy * (p.x - h.x)
            in
            if h.right then
                -c

            else
                c

        eps =
            1.0e-6

        cut a b =
            let
                sa =
                    side a

                sb =
                    side b

                t =
                    sa / (sa - sb)
            in
            { x = a.x + (b.x - a.x) * t, y = a.y + (b.y - a.y) * t }

        edges =
            case poly of
                first :: _ ->
                    List.map2 Tuple.pair poly (List.drop 1 poly ++ [ first ])

                [] ->
                    []
    in
    List.concatMap
        (\( a, b ) ->
            let
                ina =
                    side a >= -eps

                inb =
                    side b >= -eps
            in
            case ( ina, inb ) of
                ( True, True ) ->
                    [ a ]

                ( True, False ) ->
                    [ a, cut a b ]

                ( False, True ) ->
                    [ cut a b ]

                ( False, False ) ->
                    []
        )
        edges
        |> dedupe


dedupe : List Point -> List Point
dedupe pts =
    let
        close a b =
            abs (a.x - b.x) < 1.0e-6 && abs (a.y - b.y) < 1.0e-6

        go l acc =
            case l of
                p :: rest ->
                    case acc of
                        q :: _ ->
                            if close p q then
                                go rest acc

                            else
                                go rest (p :: acc)

                        [] ->
                            go rest [ p ]

                [] ->
                    acc
    in
    case List.reverse (go pts []) of
        first :: rest ->
            case List.reverse rest of
                last :: _ ->
                    if close first last then
                        first :: List.reverse (List.drop 1 (List.reverse rest))

                    else
                        first :: rest

                [] ->
                    [ first ]

        [] ->
            []



-- A FRAME


{-| Animated textures and flats: `texturetranslation[]` and `flattranslation[]` this
tic.
-}
type alias Translations =
    { texture : Int -> Int
    , flat : Int -> Int
    }


{-| No animation: every texture and flat as it is.
-}
noTranslations : Translations
noTranslations =
    { texture = identity, flat = identity }


{-| The game's animations at this `leveltime` (`Doom.Play.Spec.textureTranslation`
and `flatTranslation`).
-}
specTranslations : Env -> Int -> Translations
specTranslations env leveltime =
    { texture = Spec.textureTranslation env leveltime
    , flat = Spec.flatTranslation env leveltime
    }


{-| A sprite lump's size and offsets (`spritewidth`, `spriteoffset`,
`spritetopoffset`, R\_InitSpriteLumps), by sprite lump number.
-}
type alias SpriteInfo =
    { width : Int
    , height : Int
    , leftOffset : Int
    , topOffset : Int
    }


{-| Read the sizes of the sprite lumps (their patch headers), given the lump lookup
and the sprite lumps' names in order. A lump that can't be read gets size 0.
-}
spriteInfo : (String -> Maybe Bytes) -> List String -> Array SpriteInfo
spriteInfo lookup names =
    let
        s16 =
            Decode.signedInt16 Bytes.LE

        header =
            Decode.map4 SpriteInfo s16 s16 s16 s16
    in
    names
        |> List.map
            (\n ->
                lookup n
                    |> Maybe.andThen (Decode.decode header)
                    |> Maybe.withDefault { width = 0, height = 0, leftOffset = 0, topOffset = 0 }
            )
        |> Array.fromList


{-| What a frame is made from.

  - `mesh`: `levelMesh level`.
  - `spriteFrames`, `spriteInfo`: from the sprite lumps (`Doom.Render.SpriteFrames.build`,
    `spriteInfo`).
  - `translations`: animated textures and flats (`specTranslations`).
  - `skyTexture`: the sky's texture number (`skyTextureName`).
  - `player`: whose view it is (`consoleplayer`): their body isn't drawn, and their
    `extralight`, `fixedcolormap`, palette counters and weapon are used.

-}
type alias Input =
    { level : Level
    , data : Data
    , world : World
    , mesh : LevelMesh
    , spriteFrames : SpriteFrames
    , spriteInfo : Array SpriteInfo
    , translations : Translations
    , skyTexture : Int
    , player : Int
    }


{-| Where the view is from: the eye at (`x`, `y`, `z`) looking along `angle`.
-}
type alias Viewpoint =
    { x : Fixed
    , y : Fixed
    , z : Fixed
    , angle : Angle
    }


{-| R\_SetupFrame's viewpoint for player n: their body's x, y and angle, and their
`viewz`.
-}
viewpoint : Int -> World -> Maybe Viewpoint
viewpoint n world =
    Maybe.map2 (\p mo -> { x = mo.x, y = mo.y, z = p.viewz, angle = mo.angle })
        (World.player n world)
        (World.playerMobj n world)


{-| G\_InitNew's sky: SKY1 to SKY4 by episode, or, in Doom II, by map (SKY1 below
map 12, SKY2 below 21, then SKY3).
-}
skyTextureName : GameMode -> Int -> Int -> String
skyTextureName mode episode map =
    case mode of
        Commercial ->
            if map < 12 then
                "SKY1"

            else if map < 21 then
                "SKY2"

            else
                "SKY3"

        _ ->
            case episode of
                2 ->
                    "SKY2"

                3 ->
                    "SKY3"

                4 ->
                    "SKY4"

                _ ->
                    "SKY1"


{-| The frame seen from this viewpoint.
-}
frame : Input -> Viewpoint -> Scene.Frame
frame input view =
    let
        player =
            World.player input.player input.world |> Maybe.withDefault Player.new

        extralight =
            player.extralight

        camera =
            { x = fixed view.x, y = fixed view.y, z = fixed view.z, angle = radians view.angle }
    in
    { camera = camera
    , palette =
        Scene.paletteFor
            { damagecount = player.damagecount
            , bonuscount = player.bonuscount
            , strength = Player.power Strength player
            , ironfeet = Player.power IronFeet player
            }
    , fixedColormap =
        if player.fixedcolormap /= 0 then
            Just player.fixedcolormap

        else
            Nothing
    , skyTexture = input.skyTexture
    , sectors = Array.map (sectorLook input extralight) input.world.sectors
    , walls =
        List.range 0 (Array.length input.level.segs - 1)
            |> List.concatMap (segWalls input extralight)
    , sprites = sprites input camera extralight player
    , weapon = weaponSprites input extralight player
    }


lightNum : Int -> Int -> Int
lightNum level extralight =
    clamp 0 15 (Bitwise.shiftRightBy 4 level + extralight)


sectorLook : Input -> Int -> SectorState -> Scene.SectorLook
sectorLook input extralight s =
    let
        look pic =
            if pic == input.data.skyFlatNum then
                Sky

            else
                Flat (input.translations.flat pic)
    in
    { floor = fixed s.floorheight
    , ceiling = fixed s.ceilingheight
    , floorPic = look s.floorpic
    , ceilingPic = look s.ceilingpic
    , light = lightNum s.lightlevel extralight
    }



-- WALLS


{-| R\_StoreWallRange's wall pieces for seg `i`, and R\_RenderMaskedSegRange's
see-through middle.
-}
segWalls : Input -> Int -> Int -> List Wall
segWalls input extralight i =
    let
        level =
            input.level

        seg =
            Level.seg i level

        line =
            Level.line seg.linedef level

        flags =
            line.flags

        side =
            World.sideState seg.sidedef input.world

        front =
            World.sector seg.frontsector input.world

        -- Fake contrast: walls running east-west a step darker, north-south a step
        -- brighter.
        contrast =
            if seg.v1.y == seg.v2.y then
                -1

            else if seg.v1.x == seg.v2.x then
                1

            else
                0

        light =
            clamp 0 15 (Bitwise.shiftRightBy 4 front.lightlevel + extralight + contrast)

        u =
            fixed (side.textureoffset + seg.offset)

        rowoffset =
            fixed side.rowoffset

        pegTop =
            Flags.has Flags.mlDontPegTop flags

        pegBottom =
            Flags.has Flags.mlDontPegBottom flags

        tex n =
            input.translations.texture n

        texHeight n =
            fixed (Data.textureHeight n input.data)

        ff =
            fixed front.floorheight

        fc =
            fixed front.ceilingheight

        -- A solid piece from `bottom` to `top`, whose texture's row 0 is at height
        -- `ref` (before the side's row offset).
        piece part texture bottom top ref =
            if texture == 0 || top <= bottom then
                []

            else
                [ { seg = i
                  , part = part
                  , bottom = bottom
                  , top = top
                  , look = Textured { texture = texture, u = u, v = ref + rowoffset - top, masked = False }
                  , light = light
                  }
                ]
    in
    case seg.backsector of
        Nothing ->
            let
                mid =
                    tex side.midtexture
            in
            piece Middle
                mid
                ff
                fc
                (if pegBottom then
                    ff + texHeight mid

                 else
                    fc
                )

        Just backId ->
            let
                back =
                    World.sector backId input.world

                bf =
                    fixed back.floorheight

                bc =
                    fixed back.ceilingheight

                sky =
                    input.data.skyFlatNum

                bothSky =
                    front.ceilingpic == sky && back.ceilingpic == sky

                upper =
                    if bc < fc then
                        if bothSky then
                            [ { seg = i, part = Upper, bottom = max bc ff, top = fc, look = SkyWall, light = light } ]

                        else
                            let
                                t =
                                    tex side.toptexture
                            in
                            piece Upper
                                t
                                (max bc ff)
                                fc
                                (if pegTop then
                                    fc

                                 else
                                    bc + texHeight t
                                )

                    else
                        []

                lower =
                    if bf > ff then
                        piece Lower
                            (tex side.bottomtexture)
                            ff
                            (min bf fc)
                            (if pegBottom then
                                fc

                             else
                                bf
                            )

                    else
                        []

                middle =
                    let
                        m =
                            tex side.midtexture

                        h =
                            texHeight m

                        -- The texture's top edge: drawn once, not repeated.
                        texTop =
                            (if pegBottom then
                                max ff bf + h

                             else
                                min fc bc
                            )
                                + rowoffset

                        top =
                            min texTop (min fc bc)

                        bottom =
                            max (texTop - h) (max ff bf)
                    in
                    if m == 0 || top <= bottom then
                        []

                    else
                        [ { seg = i
                          , part = Middle
                          , bottom = bottom
                          , top = top
                          , look = Textured { texture = m, u = u, v = texTop - top, masked = True }
                          , light = light
                          }
                        ]
            in
            upper ++ lower ++ middle



-- SPRITES


{-| R\_AddSprites and R\_ProjectSprite for every thing in a sector (things with
MF\_NOSECTOR aren't in one, and aren't drawn), except the viewer's own body.
-}
sprites : Input -> Scene.Camera -> Int -> Player -> List Scene.Sprite
sprites input camera extralight player =
    input.world.sectorThings
        |> Array.toList
        |> List.indexedMap
            (\sectorId ids ->
                let
                    light =
                        lightNum (World.sector sectorId input.world).lightlevel extralight
                in
                List.filterMap
                    (\id ->
                        if Just id == player.mo then
                            Nothing

                        else
                            World.mobj id input.world |> Maybe.andThen (thingSprite input camera light)
                    )
                    ids
            )
        |> List.concat


thingSprite : Input -> Scene.Camera -> Int -> Mobj -> Maybe Scene.Sprite
thingSprite input camera light mo =
    let
        x =
            fixed mo.x

        y =
            fixed mo.y

        toThing =
            binaryAngle (atan2 (y - camera.y) (x - camera.x))
    in
    SpriteFrames.frame mo.sprite (Mobj.frameNumber mo) input.spriteFrames
        |> Maybe.map (SpriteFrames.rotationFor toThing mo.angle)
        |> Maybe.andThen
            (\rot ->
                Array.get rot.lump input.spriteInfo
                    |> Maybe.map
                        (\info ->
                            let
                                top =
                                    fixed mo.z + toFloat info.topOffset
                            in
                            { lump = rot.lump
                            , flip = rot.flip
                            , x = x
                            , y = y
                            , left = toFloat -info.leftOffset
                            , right = toFloat (info.width - info.leftOffset)
                            , bottom = top - toFloat info.height
                            , top = top
                            , light =
                                if Flags.has Flags.mfShadow mo.flags then
                                    Shadow

                                else if Mobj.fullbright mo then
                                    FullBright

                                else
                                    Lit light
                            , translation = Bitwise.shiftRightBy Flags.mfTransShift (Bitwise.and mo.flags Flags.mfTranslation)
                            }
                        )
            )


{-| R\_DrawPlayerSprites: the weapon, then its flash.
-}
weaponSprites : Input -> Int -> Player -> List Scene.WeaponSprite
weaponSprites input extralight player =
    let
        sectorLight =
            World.playerMobj input.player input.world
                |> Maybe.map (\mo -> (World.sector (World.mobjSector input.level mo) input.world).lightlevel)
                |> Maybe.withDefault 0

        invisible =
            let
                t =
                    Player.power Invisibility player
            in
            t > 4 * 32 || Bitwise.and t 8 /= 0
    in
    [ player.weaponSprite, player.flashSprite ]
        |> List.filterMap (weaponSprite input (lightNum sectorLight extralight) invisible)


weaponSprite : Input -> Int -> Bool -> PspDef -> Maybe Scene.WeaponSprite
weaponSprite input light invisible psp =
    psp.state
        |> Maybe.andThen
            (\state ->
                let
                    def =
                        State.def state
                in
                SpriteFrames.frame def.sprite (Bitwise.and def.frame Defs.ffFrameMask) input.spriteFrames
                    |> Maybe.andThen (\fr -> Array.get 0 fr.rotations)
                    |> Maybe.andThen
                        (\rot ->
                            Array.get rot.lump input.spriteInfo
                                |> Maybe.map
                                    (\info ->
                                        { lump = rot.lump
                                        , flip = rot.flip
                                        , x = fixed psp.sx - toFloat info.leftOffset
                                        , y = fixed psp.sy - toFloat info.topOffset - 100.5
                                        , light =
                                            if invisible then
                                                Shadow

                                            else if Bitwise.and def.frame Defs.ffFullbright /= 0 then
                                                FullBright

                                            else
                                                Lit light
                                        }
                                    )
                        )
            )
