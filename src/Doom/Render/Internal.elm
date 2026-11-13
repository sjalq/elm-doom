module Doom.Render.Internal exposing
    ( Cache
    , RenderData(..)
    , Source
    , SpriteFrame
    , SpriteLump
    , Static
    , TexInfo
    , TexPatch
    , ViewSize
    , flat
    , lightLevels
    , lumpBytes
    , lumpMem
    , maxLightScale
    , maxLightZ
    , numColormaps
    , texture
    , viewSize
    )

{-| What `Doom.Render.Data` builds and `Doom.Render.View` draws from, shared by the
two: the WAD's tables (r\_data.c, r\_things.c), the view-size tables (r\_main.c's
R\_ExecuteSetViewSize), and what the original keeps from one frame to the next.

The original reads lumps and builds composite textures the first time it needs them
(W\_CacheLumpNum, R\_GenerateComposite); so does the renderer here, keeping them in
`Cache`, which every frame hands on to the next.

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Doom.Defs exposing (GameMode)
import Doom.Fixed as Fixed
import Doom.Int32 as Int32
import Doom.Render.Mem as Mem exposing (Mem)
import Doom.Tables as Tables
import Doom.Wad as Wad exposing (Wad)


{-| The renderer's tables and its cache.
-}
type RenderData
    = RenderData Static Cache


{-| What never changes for a WAD.

  - `colormaps`: COLORMAP's 34 tables of 256 (32 light levels, invulnerability, black).
  - `textures`: by texture number.
  - `firstFlat`: the lump number of flat 0.
  - `spriteLumps`: by sprite lump number (counting from S\_START + 1).
  - `sprites`: by `Doom.Info.Sprite` index, each sprite's frames.
  - `zlight`: zlight[16][128], as colormap numbers.
  - `border`: the flat R\_FillBackScreen tiles, then the bevel patches brdr\_t, b, l,
    r, tl, tr, bl, br (as lump numbers).

-}
type alias Static =
    { wad : Wad
    , gameMode : GameMode
    , colormaps : Array (Array Int)
    , textures : Array TexInfo
    , firstFlat : Int
    , spriteLumps : Array SpriteLump
    , sprites : Array (Array SpriteFrame)
    , zlight : Array (Array Int)
    , backFlat : Maybe Int
    , borderPatches : List ( String, Maybe Int )
    }


{-| A wall texture (texture\_t with r\_data.c's per-texture tables).
-}
type alias TexInfo =
    { width : Int
    , height : Int
    , widthMask : Int
    , patches : List TexPatch
    }


{-| texpatch\_t: a patch of a texture, by lump number (Nothing when PNAMES names a lump
the WAD doesn't have).
-}
type alias TexPatch =
    { originX : Int
    , originY : Int
    , lump : Maybe Int
    }


{-| A sprite lump: its lump number, and spritewidth, spriteoffset and
spritetopoffset (fixed-point).
-}
type alias SpriteLump =
    { lump : Int
    , width : Int
    , offset : Int
    , topoffset : Int
    }


{-| spriteframe\_t: one frame of a sprite, as eight rotations (all the same when
`rotate` is False): the sprite lump number of each, and whether it is drawn mirrored.
-}
type alias SpriteFrame =
    { rotate : Bool
    , lumps : Array Int
    , flips : Array Bool
    }


{-| What a frame leaves for the next one, and the memory read so far.

  - `lumps`: lumps read, by lump number.
  - `textures`: textures R\_GetColumn has been asked for: for every column it can
    return (`col & texturewidthmask`), the memory and offset it returns.
  - `flats`: flats, by lump number, 4096 bytes each.
  - `fuzzpos`: r\_draw.c's `fuzzpos`, where the next fuzz pixel reads its offset.
  - `cachedDistance`, `cachedXstep`, `cachedYstep`: r\_plane.c's row caches. R\_ClearPlanes
    only clears `cachedheight`, so a plane at the viewer's own height reuses what an
    earlier frame left in them.
  - `view`: the tables for the view size last drawn.
  - `columnofs`: r\_draw.c's `columnofs[MAXWIDTH]` (low detail reads past the
    view's width).

-}
type alias Cache =
    { lumps : Dict Int Mem
    , textures : Dict Int (Array Source)
    , flats : Dict Int (Array Int)
    , fuzzpos : Int
    , cachedDistance : Array Int
    , cachedXstep : Array Int
    , cachedYstep : Array Int
    , view : Maybe ViewSize
    , columnofs : Array Int
    }


{-| Where a column's bytes are: R\_GetColumn's pointer, as a block and an offset.
-}
type alias Source =
    { mem : Mem
    , offset : Int
    }


{-| NUMCOLORMAPS.
-}
numColormaps : Int
numColormaps =
    32


{-| LIGHTLEVELS.
-}
lightLevels : Int
lightLevels =
    16


{-| MAXLIGHTSCALE.
-}
maxLightScale : Int
maxLightScale =
    48


{-| MAXLIGHTZ.
-}
maxLightZ : Int
maxLightZ =
    128



-- VIEW SIZE


{-| The globals R\_ExecuteSetViewSize, R\_InitBuffer and R\_InitTextureMapping set.
`scalelight` holds colormap numbers.
-}
type alias ViewSize =
    { blocks : Int
    , detailshift : Int
    , scaledviewwidth : Int
    , viewwidth : Int
    , viewheight : Int
    , centerx : Int
    , centery : Int
    , centerxfrac : Int
    , centeryfrac : Int
    , projection : Int
    , viewwindowx : Int
    , viewwindowy : Int
    , viewangletox : Array Int
    , xtoviewangle : Array Int
    , clipangle : Int
    , pspritescale : Int
    , pspriteiscale : Int
    , yslope : Array Int
    , distscale : Array Int
    , scalelight : Array (Array Int)
    }


{-| R\_ExecuteSetViewSize for screenblocks 3 to 11 and detail 0 (high) or 1 (low).
-}
viewSize : Int -> Int -> ViewSize
viewSize blocks detailshift =
    let
        ( scaledviewwidth, viewheight ) =
            if blocks == 11 then
                ( 320, 200 )

            else
                ( blocks * 32, (blocks * 168 // 10) // 8 * 8 )

        viewwidth =
            Int32.shiftRight detailshift scaledviewwidth

        centerx =
            viewwidth // 2

        centery =
            viewheight // 2

        centerxfrac =
            centerx * Fixed.fracunit

        -- R_InitTextureMapping
        focallength =
            Fixed.div centerxfrac (Tables.finetangent (2048 + 1024))

        rawAngleToX =
            Array.initialize 4096
                (\i ->
                    let
                        tan =
                            Tables.finetangent i
                    in
                    if tan > 2 * Fixed.fracunit then
                        -1

                    else if tan < -2 * Fixed.fracunit then
                        viewwidth + 1

                    else
                        let
                            t =
                                Int32.shiftRight 16 (Int32.add (Int32.sub centerxfrac (Fixed.mul tan focallength)) (Fixed.fracunit - 1))
                        in
                        if t < -1 then
                            -1

                        else if t > viewwidth + 1 then
                            viewwidth + 1

                        else
                            t
                )

        firstAtOrLeft x i =
            if (Array.get i rawAngleToX |> Maybe.withDefault -1) > x then
                firstAtOrLeft x (i + 1)

            else
                i

        xtoviewangle =
            Array.initialize (viewwidth + 1)
                (\x -> Int32.toUnsigned (firstAtOrLeft x 0 * 524288 - 0x40000000))

        viewangletox =
            Array.map
                (\t ->
                    if t == -1 then
                        0

                    else if t == viewwidth + 1 then
                        viewwidth

                    else
                        t
                )
                rawAngleToX

        startmap i =
            ((lightLevels - 1 - i) * 2) * numColormaps // lightLevels

        clampLevel level =
            if level < 0 then
                0

            else if level >= numColormaps then
                numColormaps - 1

            else
                level
    in
    { blocks = blocks
    , detailshift = detailshift
    , scaledviewwidth = scaledviewwidth
    , viewwidth = viewwidth
    , viewheight = viewheight
    , centerx = centerx
    , centery = centery
    , centerxfrac = centerxfrac
    , centeryfrac = centery * Fixed.fracunit
    , projection = centerxfrac
    , viewwindowx = (320 - scaledviewwidth) // 2
    , viewwindowy =
        if scaledviewwidth == 320 then
            0

        else
            (200 - 32 - viewheight) // 2
    , viewangletox = viewangletox
    , xtoviewangle = xtoviewangle
    , clipangle = Array.get 0 xtoviewangle |> Maybe.withDefault 0
    , pspritescale = Fixed.fracunit * viewwidth // 320
    , pspriteiscale = Fixed.fracunit * 320 // viewwidth
    , yslope =
        Array.initialize viewheight
            (\i ->
                Fixed.div (Int32.shiftLeft detailshift viewwidth // 2 * Fixed.fracunit)
                    (abs ((i - viewheight // 2) * Fixed.fracunit + Fixed.fracunit // 2))
            )
    , distscale =
        Array.initialize viewwidth
            (\i ->
                let
                    angle =
                        Array.get i xtoviewangle |> Maybe.withDefault 0
                in
                Fixed.div Fixed.fracunit (Int32.abs (Tables.finecosine (Int32.shiftRightUnsigned 19 angle)))
            )
    , scalelight =
        Array.initialize lightLevels
            (\i ->
                Array.initialize maxLightScale
                    (\j -> clampLevel (startmap i - j * 320 // Int32.shiftLeft detailshift viewwidth // 2))
            )
    }



-- LAZY MEMORY


{-| A lump's bytes as the WAD has them.
-}
lumpBytes : Static -> Int -> Maybe Bytes
lumpBytes static lump =
    Wad.lumpAt lump static.wad |> Maybe.map .bytes


{-| W\_CacheLumpNum: a lump's bytes.
-}
lumpMem : Static -> Int -> Cache -> ( Mem, Cache )
lumpMem static lump cache =
    case Dict.get lump cache.lumps of
        Just mem ->
            ( mem, cache )

        Nothing ->
            let
                mem =
                    Wad.lumpAt lump static.wad
                        |> Maybe.map (.bytes >> Mem.fromBytes)
                        |> Maybe.withDefault Array.empty
            in
            ( mem, { cache | lumps = Dict.insert lump mem cache.lumps } )


{-| A flat's 4096 bytes (by lump number).
-}
flat : Static -> Int -> Cache -> ( Array Int, Cache )
flat static lump cache =
    case Dict.get lump cache.flats of
        Just f ->
            ( f, cache )

        Nothing ->
            let
                ( mem, c1 ) =
                    lumpMem static lump cache

                f =
                    Array.initialize 4096 (\i -> Mem.byte i mem)
            in
            ( f, { c1 | flats = Dict.insert lump f c1.flats } )


{-| What R\_GetColumn returns for each column of a texture (`col & texturewidthmask`
picks the column), after R\_GenerateLookup and, when it has columns made of more than
one patch, R\_GenerateComposite.
-}
texture : Static -> Int -> Cache -> ( Array Source, Cache )
texture static texnum cache =
    case Dict.get texnum cache.textures of
        Just t ->
            ( t, cache )

        Nothing ->
            case Array.get texnum static.textures of
                Nothing ->
                    ( Array.empty, cache )

                Just tex ->
                    let
                        ( built, c1 ) =
                            buildTexture static tex cache
                    in
                    ( built, { c1 | textures = Dict.insert texnum built c1.textures } )


type ColRef
    = PatchCol Int Int
    | CompositeCol Int


type alias LoadedPatch =
    { originX : Int
    , originY : Int
    , lump : Int
    , mem : Mem
    , width : Int
    }


buildTexture : Static -> TexInfo -> Cache -> ( Array Source, Cache )
buildTexture static tex cache =
    let
        ( patches, c1 ) =
            List.foldl
                (\p ( acc, c ) ->
                    case p.lump of
                        Just lump ->
                            let
                                ( mem, c2 ) =
                                    lumpMem static lump c
                            in
                            ( { originX = p.originX, originY = p.originY, lump = lump, mem = mem, width = Mem.s16 0 mem } :: acc, c2 )

                        Nothing ->
                            -- R_InitTextures stops the game for a missing patch
                            ( acc, c )
                )
                ( [], cache )
                tex.patches
                |> Tuple.mapFirst List.reverse

        -- R_GenerateLookup, first loop: for each column, how many patches cover it,
        -- and the last one's lump and column offset (an unsigned short)
        covered =
            List.foldl
                (\p acc ->
                    List.foldl
                        (\x a ->
                            case Array.get x a of
                                Just ( n, _ ) ->
                                    Array.set x
                                        ( n + 1
                                        , PatchCol p.lump (modBy 65536 (Mem.u32 (8 + 4 * (x - p.originX)) p.mem + 3))
                                        )
                                        a

                                Nothing ->
                                    a
                        )
                        acc
                        (List.range (max 0 p.originX) (min tex.width (p.originX + p.width) - 1))
                )
                (Array.repeat tex.width ( 0, CompositeCol 0 ))
                patches

        -- second loop: columns of more than one patch go in the composite, in
        -- order; a column no patch covers ends the loop (the C source returns)
        lookup x ( refs, compositeSize, stopped ) =
            if stopped then
                ( refs, compositeSize, True )

            else
                case Array.get x covered of
                    Just ( 0, _ ) ->
                        ( refs, compositeSize, True )

                    Just ( n, ref ) ->
                        if n > 1 then
                            ( Array.push (CompositeCol compositeSize) refs, compositeSize + tex.height, False )

                        else
                            ( Array.push ref refs, compositeSize, False )

                    Nothing ->
                        ( refs, compositeSize, True )

        ( lookedUp, _, _ ) =
            List.foldl lookup ( Array.empty, 0, False ) (List.range 0 (tex.width - 1))

        columns =
            Array.append lookedUp (Array.slice (Array.length lookedUp) tex.width (Array.map Tuple.second covered))

        composite =
            if List.any isComposite (Array.toList lookedUp) then
                generateComposite tex patches lookedUp

            else
                Array.empty

        source ref =
            case ref of
                PatchCol lump ofs ->
                    case List.filter (\p -> p.lump == lump) patches |> List.head of
                        Just p ->
                            { mem = p.mem, offset = ofs }

                        Nothing ->
                            { mem = Array.empty, offset = ofs }

                CompositeCol ofs ->
                    { mem = composite, offset = ofs }
    in
    ( Array.initialize (tex.widthMask + 1)
        (\c -> Array.get c columns |> Maybe.withDefault (CompositeCol 0) |> source)
    , c1
    )


isComposite : ColRef -> Bool
isComposite ref =
    case ref of
        CompositeCol _ ->
            True

        PatchCol _ _ ->
            False


{-| R\_GenerateComposite: the columns made of several patches, one after another, each
`height` bytes, with every patch's posts copied in by R\_DrawColumnInCache. Bytes no
patch covers are left as zero (the original leaves what Z\_Malloc's memory held).
-}
generateComposite : TexInfo -> List LoadedPatch -> Array ColRef -> Mem
generateComposite tex patches columns =
    let
        compositeColumns =
            Array.toIndexedList columns
                |> List.filter (\( _, ref ) -> isComposite ref)
                |> List.map Tuple.first

        column x =
            List.foldl
                (\p col ->
                    if x >= max 0 p.originX && x < min tex.width (p.originX + p.width) then
                        drawColumnInCache p.mem (Mem.u32 (8 + 4 * (x - p.originX)) p.mem) p.originY tex.height col

                    else
                        col
                )
                (Array.repeat tex.height 0)
                patches
    in
    compositeColumns
        |> List.concatMap (column >> Array.toList)
        |> Mem.fromList


{-| R\_DrawColumnInCache: copy a patch column's posts into a column of the composite.
A post cut off at the top is copied from its first byte, as in the C source.
-}
drawColumnInCache : Mem -> Int -> Int -> Int -> Array Int -> Array Int
drawColumnInCache mem ofs originy cacheheight col =
    let
        topdelta =
            Mem.byte ofs mem
    in
    if topdelta == 0xFF || ofs >= Mem.size mem then
        col

    else
        let
            length =
                Mem.byte (ofs + 1) mem

            position0 =
                originy + topdelta

            ( count0, position ) =
                if position0 < 0 then
                    ( length + position0, 0 )

                else
                    ( length, position0 )

            count =
                if position + count0 > cacheheight then
                    cacheheight - position

                else
                    count0

            copied =
                if count > 0 then
                    List.foldl (\i c -> Array.set (position + i) (Mem.byte (ofs + 3 + i) mem) c) col (List.range 0 (count - 1))

                else
                    col
        in
        drawColumnInCache mem (ofs + length + 4) originy cacheheight copied
