module Doom.Render.Data exposing
    ( RenderData, load, prepareView
    , Precache, precacheLevel, precacheWeight, precache
    )

{-| What the 3D view is drawn from (r\_data.c, r\_sky.c, r\_things.c's R\_InitSprites, r\_draw.c's translation tables): composed wall textures and their column lookups, flats, COLORMAP, sprite definitions with their rotations, the fuzz table. Built once per WAD.

Like the original, the renderer reads a lump, and composes a texture made of several
patches, the first time a frame needs it; a `RenderData` carries what it has read so
far from frame to frame (`Doom.Render.View.render` gives back the new one), with the
little the original keeps between frames (`fuzzpos`, the plane row caches).

@docs RenderData, load, prepareView


# Reading ahead

@docs Precache, precacheLevel, precacheWeight, precache

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as D
import Dict
import Doom.Data exposing (Data)
import Doom.Defs exposing (GameMode(..))
import Doom.Fixed as Fixed
import Doom.Info.Sprite as Sprite
import Doom.Render.Internal as Internal exposing (SpriteFrame, SpriteLump)
import Doom.Render.Mem as Mem
import Doom.Wad as Wad exposing (Wad)
import Doom.World exposing (World)
import Set


{-| The renderer's tables.
-}
type alias RenderData =
    Internal.RenderData


{-| R\_InitData, R\_InitSprites, R\_InitTranslationTables and friends.
-}
load : Wad -> Data -> RenderData
load wad data =
    let
        colormapMem =
            Wad.lump "COLORMAP" wad |> Maybe.map Mem.fromBytes |> Maybe.withDefault Array.empty

        firstSprite =
            Wad.checkNumForName "S_START" wad |> Maybe.map ((+) 1) |> Maybe.withDefault 0

        lastSprite =
            Wad.checkNumForName "S_END" wad |> Maybe.map (\n -> n - 1) |> Maybe.withDefault -1

        spriteLumps =
            Array.initialize (max 0 (lastSprite - firstSprite + 1)) (\i -> spriteLump wad (firstSprite + i))

        spriteNames =
            Array.initialize (Array.length spriteLumps)
                (\i -> Wad.lumpAt (firstSprite + i) wad |> Maybe.map (.name >> String.toUpper) |> Maybe.withDefault "")

        lumpNum name =
            Wad.checkNumForName name wad
    in
    Internal.RenderData
        { wad = wad
        , gameMode = data.gameMode
        , colormaps =
            Array.initialize 34 (\m -> Array.initialize 256 (\c -> Mem.byte (m * 256 + c) colormapMem))
        , textures =
            Array.map
                (\t ->
                    { width = t.width
                    , height = t.height
                    , widthMask = widthMask t.width
                    , patches = List.map (\p -> { originX = p.originX, originY = p.originY, lump = lumpNum p.patch }) t.patches
                    }
                )
                data.textures
        , firstFlat = lumpNum "F_START" |> Maybe.map ((+) 1) |> Maybe.withDefault 0
        , spriteLumps = spriteLumps
        , sprites = Array.map (\s -> spriteDef (Sprite.name s) spriteNames) Sprite.all
        , zlight = zlight
        , backFlat =
            lumpNum
                (if data.gameMode == Commercial then
                    "GRNROCK"

                 else
                    "FLOOR7_2"
                )
        , borderPatches =
            List.map (\n -> ( n, lumpNum n ))
                [ "BRDR_T", "BRDR_B", "BRDR_L", "BRDR_R", "BRDR_TL", "BRDR_TR", "BRDR_BL", "BRDR_BR" ]
        }
        { lumps = Dict.empty
        , textures = Dict.empty
        , flats = Dict.empty
        , fuzzpos = 0
        , cachedDistance = Array.repeat 200 0
        , cachedXstep = Array.repeat 200 0
        , cachedYstep = Array.repeat 200 0
        , view = Nothing
        }


{-| R\_ExecuteSetViewSize ahead of time: the view-size tables for a screen size (3
to 11) and detail (0 high, 1 low), which the first frame drawn at that size would
otherwise have to make.
-}
prepareView : Int -> Int -> RenderData -> RenderData
prepareView blocks detail (Internal.RenderData static cache) =
    Internal.RenderData static { cache | view = Just (Internal.viewSize (clamp 3 11 blocks) (clamp 0 1 detail)) }


{-| texturewidthmask: the biggest power of two no wider than the texture, less one.
-}
widthMask : Int -> Int
widthMask w =
    let
        go j =
            if j * 2 <= w then
                go (j * 2)

            else
                j
    in
    go 1 - 1


{-| R\_InitSpriteLumps: a sprite patch's width and offsets.
-}
spriteLump : Wad -> Int -> SpriteLump
spriteLump wad lump =
    let
        header =
            Wad.lumpAt lump wad
                |> Maybe.andThen
                    (\entry ->
                        D.decode
                            (D.map4 (\pw _ pl pt -> ( pw, pl, pt ))
                                (D.signedInt16 LE)
                                (D.signedInt16 LE)
                                (D.signedInt16 LE)
                                (D.signedInt16 LE)
                            )
                            entry.bytes
                    )
                |> Maybe.withDefault ( 0, 0, 0 )

        ( w, l, t ) =
            header
    in
    { lump = lump
    , width = Fixed.fromInt w
    , offset = Fixed.fromInt l
    , topoffset = Fixed.fromInt t
    }


{-| R\_InitSpriteDefs for one sprite: every lump between S\_START and S\_END whose name
starts with the sprite's four letters gives a frame (5th letter) and rotation (6th
digit, 0 for all of them), and, if the name goes on, a second frame and rotation
drawn mirrored. Frames run to the last one found.
-}
spriteDef : String -> Array String -> Array SpriteFrame
spriteDef name lumpNames =
    let
        unset =
            { rotate = Nothing, lumps = Array.repeat 8 -1, flips = Array.repeat 8 False }

        charAt i s =
            String.slice i (i + 1) s |> String.toList |> List.head |> Maybe.map Char.toCode |> Maybe.withDefault 0

        install lump frame rotation flipped frames =
            case Array.get frame frames of
                Nothing ->
                    frames

                Just f ->
                    if rotation == 0 then
                        Array.set frame { rotate = Just False, lumps = Array.repeat 8 lump, flips = Array.repeat 8 flipped } frames

                    else if rotation >= 1 && rotation <= 8 then
                        Array.set frame
                            { rotate = Just True
                            , lumps = Array.set (rotation - 1) lump f.lumps
                            , flips = Array.set (rotation - 1) flipped f.flips
                            }
                            frames

                    else
                        frames

        ( temp, maxframe ) =
            Array.foldl
                (\lumpName ( ( i, frames ), mx ) ->
                    if String.left 4 lumpName == name then
                        let
                            frame =
                                charAt 4 lumpName - Char.toCode 'A'

                            withFirst =
                                install i frame (charAt 5 lumpName - Char.toCode '0') False frames

                            ( withSecond, mx2 ) =
                                if String.length lumpName > 6 then
                                    let
                                        frame2 =
                                            charAt 6 lumpName - Char.toCode 'A'
                                    in
                                    ( install i frame2 (charAt 7 lumpName - Char.toCode '0') True withFirst, max frame frame2 )

                                else
                                    ( withFirst, frame )
                        in
                        ( ( i + 1, withSecond ), max mx mx2 )

                    else
                        ( ( i + 1, frames ), mx )
                )
                ( ( 0, Array.repeat 29 unset ), -1 )
                lumpNames
                |> Tuple.mapFirst Tuple.second
    in
    Array.slice 0 (maxframe + 1) temp
        |> Array.map (\f -> { rotate = f.rotate == Just True, lumps = f.lumps, flips = f.flips })


{-| R\_InitLightTables: zlight[16][128], as colormap numbers.
-}
zlight : Array (Array Int)
zlight =
    Array.initialize Internal.lightLevels
        (\i ->
            let
                startmap =
                    ((Internal.lightLevels - 1 - i) * 2) * Internal.numColormaps // Internal.lightLevels
            in
            Array.initialize Internal.maxLightZ
                (\j ->
                    let
                        scale =
                            Fixed.div (160 * Fixed.fracunit) ((j + 1) * 1048576) // 4096

                        level =
                            startmap - scale // 2
                    in
                    clamp 0 (Internal.numColormaps - 1) level
                )
        )



-- PRECACHING


{-| Something to read ahead of the frames that need it, and roughly how much work
that is (a few thousand bytes read or composed is 1).
-}
type Precache
    = Precache Item Int


{-| A flat (by lump number), a wall texture (composed), or a sprite lump.
-}
type Item
    = Flat Int
    | Texture Int
    | Lump Int


{-| What a level's frames will read, as R\_PrecacheLevel loads it when the level
starts, less what has been read already: the textures its sides show and the
flats its sectors show (with every frame of their animations, and both faces of
their switches), the sky, and every sprite lump (R\_PrecacheLevel takes the
sprites of the things present; their deaths, shots and the weapons need the rest
soon enough).

Reading it all in the first frame would make that frame long, so a host spreads it
over the melt (`precache`); what is left unread is read when a frame first needs
it, as before. Either way the frames are the same.

-}
precacheLevel : Data -> World -> Int -> RenderData -> List Precache
precacheLevel data world skytexture (Internal.RenderData static cache) =
    let
        animated istexture pic =
            pic
                :: List.concatMap
                    (\a ->
                        if a.istexture == istexture && pic >= a.basepic && pic < a.basepic + a.numpics then
                            List.range a.basepic (a.basepic + a.numpics - 1)

                        else
                            []
                    )
                    data.anims

        switched tex =
            Array.toIndexedList data.switchList
                |> List.filter (\( _, t ) -> t == tex)
                |> List.filterMap (\( i, _ ) -> Array.get (Bitwise.xor i 1) data.switchList)

        textures =
            Array.foldl (\side acc -> side.toptexture :: side.midtexture :: side.bottomtexture :: acc) [ skytexture ] world.sides
                |> Set.fromList
                |> Set.toList
                |> List.concatMap (\t -> animated True t ++ switched t)
                |> Set.fromList
                |> Set.filter (\t -> not (Dict.member t cache.textures))
                |> Set.toList
                |> List.map
                    (\t ->
                        Precache (Texture t)
                            (Array.get t static.textures
                                |> Maybe.map (\tex -> 1 + tex.width * tex.height // 2048)
                                |> Maybe.withDefault 1
                            )
                    )

        flats =
            Array.foldl (\sector acc -> sector.floorpic :: sector.ceilingpic :: acc) [] world.sectors
                |> List.filter (\f -> f /= data.skyFlatNum)
                |> Set.fromList
                |> Set.toList
                |> List.concatMap (animated False)
                |> List.map (\f -> static.firstFlat + f)
                |> Set.fromList
                |> Set.filter (\lump -> not (Dict.member lump cache.flats))
                |> Set.toList
                |> List.map (\lump -> Precache (Flat lump) 2)

        sprites =
            Array.toList static.spriteLumps
                |> List.map .lump
                |> List.filter (\lump -> not (Dict.member lump cache.lumps))
                |> List.map
                    (\lump ->
                        Precache (Lump lump)
                            (1 + (Wad.lumpAt lump static.wad |> Maybe.map (.bytes >> Bytes.width) |> Maybe.withDefault 0) // 4096)
                    )
    in
    textures ++ flats ++ sprites


{-| How much reading the list is, in the same units.
-}
precacheWeight : List Precache -> Int
precacheWeight =
    List.foldl (\(Precache _ w) acc -> acc + w) 0


{-| Read from the front of the list until `budget` units of work are done; gives
what is left.
-}
precache : Int -> List Precache -> RenderData -> ( List Precache, RenderData )
precache budget items (Internal.RenderData static cache) =
    let
        go left rest c =
            case rest of
                (Precache item w) :: more ->
                    if left <= 0 then
                        ( rest, c )

                    else
                        go (left - w) more (read item c)

                [] ->
                    ( [], c )

        read item c =
            case item of
                Texture t ->
                    Internal.texture static t c |> Tuple.second

                Flat lump ->
                    Internal.flat static lump c |> Tuple.second

                Lump lump ->
                    Internal.lumpMem static lump c |> Tuple.second

        ( left_, cache2 ) =
            go budget items cache
    in
    ( left_, Internal.RenderData static cache2 )
