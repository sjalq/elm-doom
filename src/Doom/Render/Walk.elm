module Doom.Render.Walk exposing (Frame, walk)

{-| R\_RenderPlayerView, as far as what gets written where: the BSP walk (r\_bsp.c),
the walls (r\_segs.c), the floors, ceilings and sky (r\_plane.c, r\_sky.c), the things
and the weapon (r\_things.c), with r\_draw.c's column and span drawers computing
every pixel. Everything is done in the original's order and with its arithmetic;
what it would write to the screen is collected per screen column
(`Doom.Render.Compose`).

The C source's file-level variables are fields of `Ctx` (fixed for the frame) and
`St` (changing as the walk goes); its pointers into arrays are indices.

-}

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Doom.Angle as Angle
import Doom.Defs exposing (Power(..))
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.Sprite as Sprite
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Level as Level exposing (Child(..), Level, LineId, Seg)
import Doom.Mobj exposing (Mobj)
import Doom.Play.Env exposing (Env)
import Doom.Play.Spec as Spec
import Doom.Player as Player exposing (Player, PspDef)
import Doom.Render.Compose as Compose exposing (Columns, Op(..))
import Doom.Render.Internal as Internal exposing (Cache, RenderData(..), Source, Static, ViewSize)
import Doom.Render.Mem as Mem exposing (Mem)
import Doom.Tables as Tables
import Doom.World as World exposing (SectorState, World)
import Set exposing (Set)


{-| What a frame writes, the lines it drew walls for, and the renderer's data for
the next frame. `fuzzmap` is colormap 6 (for R\_DrawFuzzColumn).
-}
type alias Frame =
    { columns : Columns
    , fuzzmap : Array Int
    , mapped : List LineId
    , renderData : RenderData
    }


{-| The frame's constants (R\_SetupFrame, R\_ExecuteSetViewSize, R\_ClearPlanes).
`fixedcolormap` is the colormap the player's powers fix, if any.
-}
type alias Ctx =
    { vs : ViewSize
    , static : Static
    , env : Env
    , level : Level
    , world : World
    , viewx : Int
    , viewy : Int
    , viewz : Int
    , viewangle : Int
    , extralight : Int
    , fixedcolormap : Maybe (Array Int)
    , skyflatnum : Int
    , skytexture : Int
    , leveltime : Int
    , low : Bool
    , wx : Int
    , wy : Int
    , basexscale : Int
    , baseyscale : Int
    }


{-| visplane\_t: a floor or ceiling area to draw, with its extent in each column
(`top` 0xff where it has none).
-}
type alias Plane =
    { height : Int
    , picnum : Int
    , lightlevel : Int
    , minx : Int
    , maxx : Int
    , top : Array Int
    , bottom : Array Int
    }


{-| Where a drawseg keeps a sprite's clip: nothing, screenheightarray, negonearray, or
its own copy of the clip array (indexed from `x1`).
-}
type Clip
    = NoClip
    | AllHeight
    | AllNegOne
    | Clipped (Array Int)


{-| drawseg\_t. `maskedtexturecol` is indexed from `x1`.
-}
type alias Drawseg =
    { x1 : Int
    , x2 : Int
    , seg : Seg
    , scale1 : Int
    , scale2 : Int
    , scalestep : Int
    , silhouette : Int
    , bsilheight : Int
    , tsilheight : Int
    , sprtopclip : Clip
    , sprbottomclip : Clip
    , maskedtexturecol : Maybe (Array Int)
    }


{-| vissprite\_t. `colormap` Nothing is a shadow (drawn with fuzz); `patch` is the
sprite lump number.
-}
type alias Vissprite =
    { x1 : Int
    , x2 : Int
    , gx : Int
    , gy : Int
    , gz : Int
    , gzt : Int
    , startfrac : Int
    , scale : Int
    , xiscale : Int
    , texturemid : Int
    , patch : Int
    , colormap : Maybe (Array Int)
    , mobjflags : Int
    }


{-| What the walk changes. `pixhigh`... are r\_segs.c's globals that a wall leaves
for the next when it doesn't set them.
-}
type alias St =
    { solidsegs : List ( Int, Int )
    , ceilingclip : Array Int
    , floorclip : Array Int
    , planes : Array Plane
    , planeIndex : Dict ( Int, Int, Int ) Int
    , floorplane : Int
    , ceilingplane : Int
    , drawsegs : List Drawseg
    , numDrawsegs : Int
    , vissprites : List Vissprite
    , numVissprites : Int
    , seen : Set Int
    , ops : Array (List Op)
    , mapped : List LineId
    , cache : Cache
    , pixhigh : Int
    , pixhighstep : Int
    , pixlow : Int
    , pixlowstep : Int
    }


ang90 : Int
ang90 =
    0x40000000


ang180 : Int
ang180 =
    0x80000000


maxInt : Int
maxInt =
    2147483647


minInt : Int
minInt =
    -2147483648


silBottom : Int
silBottom =
    1


silTop : Int
silTop =
    2


{-| R\_RenderPlayerView for the options' player.
-}
walk : RenderData -> Env -> World -> { screenblocks : Int, detailshift : Int, player : Int, skytexture : Int } -> Maybe Frame
walk (RenderData static cache0) env world options =
    case ( World.player options.player world, World.playerMobj options.player world ) of
        ( Just player, Just mo ) ->
            let
                blocks =
                    clamp 3 11 options.screenblocks

                detail =
                    clamp 0 1 options.detailshift

                vs =
                    case cache0.view of
                        Just v ->
                            if v.blocks == blocks && v.detailshift == detail then
                                v

                            else
                                Internal.viewSize blocks detail

                        Nothing ->
                            Internal.viewSize blocks detail

                cache1 =
                    { cache0 | view = Just vs }

                viewangle =
                    mo.angle

                fine =
                    Angle.toFine (Angle.sub viewangle ang90)

                ctx =
                    { vs = vs
                    , static = static
                    , env = env
                    , level = env.level
                    , world = world
                    , viewx = mo.x
                    , viewy = mo.y
                    , viewz = player.viewz
                    , viewangle = viewangle
                    , extralight = player.extralight
                    , fixedcolormap =
                        if player.fixedcolormap /= 0 then
                            Just (colormap static player.fixedcolormap)

                        else
                            Nothing
                    , skyflatnum = env.data.skyFlatNum
                    , skytexture = options.skytexture
                    , leveltime = world.leveltime
                    , low = detail == 1
                    , wx = vs.viewwindowx
                    , wy = vs.viewwindowy
                    , basexscale = Fixed.div (Tables.finecosine fine) vs.centerxfrac
                    , baseyscale = Int32.neg (Fixed.div (Tables.finesine fine) vs.centerxfrac)
                    }

                st0 =
                    { solidsegs = [ ( -0x7FFFFFFF, -1 ), ( vs.viewwidth, 0x7FFFFFFF ) ]
                    , ceilingclip = Array.repeat vs.viewwidth -1
                    , floorclip = Array.repeat vs.viewwidth vs.viewheight
                    , planes = Array.empty
                    , planeIndex = Dict.empty
                    , floorplane = -1
                    , ceilingplane = -1
                    , drawsegs = []
                    , numDrawsegs = 0
                    , vissprites = []
                    , numVissprites = 0
                    , seen = Set.empty
                    , ops = Compose.emptyColumns.solid
                    , mapped = []
                    , cache = cache1
                    , pixhigh = 0
                    , pixhighstep = 0
                    , pixlow = 0
                    , pixlowstep = 0
                    }

                numnodes =
                    Array.length ctx.level.nodes

                st1 =
                    if numnodes == 0 then
                        subsector ctx 0 st0

                    else
                        renderNode ctx (numnodes - 1) st0

                ( solidOps, cache2 ) =
                    drawPlanes ctx st1.planes st1.ops st1.cache

                masked =
                    drawMasked ctx player mo st1 cache2
            in
            Just
                { columns = { solid = solidOps, masked = masked.ops }
                , fuzzmap = colormap static 6
                , mapped = st1.mapped
                , renderData = RenderData static { cache2 | fuzzpos = masked.fuzzpos, lumps = masked.cache.lumps, textures = masked.cache.textures }
                }

        _ ->
            Nothing


colormap : Static -> Int -> Array Int
colormap static n =
    Array.get n static.colormaps |> Maybe.withDefault Array.empty


{-| texturetranslation[]: P\_UpdateSpecials sets it in the tic just drawn, before that
tic's `leveltime++`.
-}
textureTranslation : Ctx -> Int -> Int
textureTranslation ctx tex =
    if ctx.leveltime <= 0 then
        tex

    else
        Spec.textureTranslation ctx.env (ctx.leveltime - 1) tex


flatTranslation : Ctx -> Int -> Int
flatTranslation ctx flat =
    if ctx.leveltime <= 0 then
        flat

    else
        Spec.flatTranslation ctx.env (ctx.leveltime - 1) flat


textureHeight : Ctx -> Int -> Int
textureHeight ctx tex =
    Array.get tex ctx.static.textures |> Maybe.map (\t -> t.height * Fixed.fracunit) |> Maybe.withDefault 0


pointToAngle : Ctx -> Int -> Int -> Int
pointToAngle ctx x y =
    Geometry.pointToAngle2 ctx.viewx ctx.viewy x y


at : Int -> Array Int -> Int
at i a =
    Array.get i a |> Maybe.withDefault 0


{-| `finesine[i]` for an index that went negative through a signed shift: the C
source reads the table before it (finetangent).
-}
finesineAny : Int -> Int
finesineAny i =
    if i >= 0 then
        Tables.finesine i

    else
        Tables.finetangent (4096 + i)


{-| `finetangent[i]` for an index past its 4096 entries: the C source reads on into
finesine.
-}
finetangentAny : Int -> Int
finetangentAny i =
    if i < 4096 then
        Tables.finetangent i

    else
        Tables.finesine (i - 4096)


{-| `0xffffffffu / (unsigned)scale`, as an int.
-}
invScale : Int -> Int
invScale scale =
    let
        u =
            Int32.toUnsigned scale
    in
    if u == 0 then
        0

    else
        Int32.wrap (4294967295 // u)


lightRow : Ctx -> Int -> Array Int
lightRow ctx lightnum =
    Array.get (clamp 0 (Internal.lightLevels - 1) lightnum) ctx.vs.scalelight |> Maybe.withDefault Array.empty


{-| A light table entry for an unsigned index computed by a signed shift.
-}
lightIndex : Int -> Int -> Int
lightIndex limit index =
    if index < 0 || index >= limit then
        limit - 1

    else
        index



-- BSP


renderNode : Ctx -> Int -> St -> St
renderNode ctx n st =
    let
        node =
            Level.node n ctx.level

        side =
            Geometry.pointOnSide ctx.viewx ctx.viewy node

        ( front, back ) =
            if side == 0 then
                node.children

            else
                ( Tuple.second node.children, Tuple.first node.children )

        backBox =
            if side == 0 then
                Tuple.second node.bbox

            else
                Tuple.first node.bbox

        st1 =
            renderChild ctx front st
    in
    if checkBBox ctx backBox st1.solidsegs then
        renderChild ctx back st1

    else
        st1


renderChild : Ctx -> Child -> St -> St
renderChild ctx child st =
    case child of
        ChildNode n ->
            renderNode ctx n st

        ChildSubsector s ->
            subsector ctx s st


{-| checkcoord[boxpos]: which box sides (BOXTOP, BOXBOTTOM, BOXLEFT, BOXRIGHT) make
the corners to check.
-}
checkcoord : Array ( ( Int, Int ), ( Int, Int ) )
checkcoord =
    Array.fromList
        [ ( ( 3, 0 ), ( 2, 1 ) )
        , ( ( 3, 0 ), ( 2, 0 ) )
        , ( ( 3, 1 ), ( 2, 0 ) )
        , ( ( 0, 0 ), ( 0, 0 ) )
        , ( ( 2, 0 ), ( 2, 1 ) )
        , ( ( 0, 0 ), ( 0, 0 ) )
        , ( ( 3, 1 ), ( 3, 0 ) )
        , ( ( 0, 0 ), ( 0, 0 ) )
        , ( ( 2, 0 ), ( 3, 1 ) )
        , ( ( 2, 1 ), ( 3, 1 ) )
        , ( ( 2, 1 ), ( 3, 0 ) )
        ]


{-| R\_CheckBBox: whether any of the box might be visible.
-}
checkBBox : Ctx -> Level.Box -> List ( Int, Int ) -> Bool
checkBBox ctx box solidsegs =
    let
        boxx =
            if ctx.viewx <= box.left then
                0

            else if ctx.viewx < box.right then
                1

            else
                2

        boxy =
            if ctx.viewy >= box.top then
                0

            else if ctx.viewy > box.bottom then
                1

            else
                2

        boxpos =
            boxy * 4 + boxx
    in
    if boxpos == 5 then
        True

    else
        let
            ( ( c0, c1 ), ( c2, c3 ) ) =
                Array.get boxpos checkcoord |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )

            coord c =
                case c of
                    0 ->
                        box.top

                    1 ->
                        box.bottom

                    2 ->
                        box.left

                    _ ->
                        box.right

            angle1 =
                Angle.sub (pointToAngle ctx (coord c0) (coord c1)) ctx.viewangle

            angle2 =
                Angle.sub (pointToAngle ctx (coord c2) (coord c3)) ctx.viewangle
        in
        case clipAngles ctx angle1 angle2 of
            Nothing ->
                -- span >= ANG180: the viewer is in the box's angle; or off the view
                Angle.sub angle1 angle2 >= ang180

            Just ( a1, a2 ) ->
                let
                    sx1 =
                        at (Angle.toFine (Angle.add a1 ang90)) ctx.vs.viewangletox

                    sx2 =
                        at (Angle.toFine (Angle.add a2 ang90)) ctx.vs.viewangletox - 1
                in
                if sx1 == sx2 + 1 then
                    False

                else
                    case List.filter (\( _, last ) -> last >= sx2) solidsegs |> List.head of
                        Just ( first, last ) ->
                            not (sx1 >= first && sx2 <= last)

                        Nothing ->
                            True


{-| The clipping of angles (relative to the view) to the field of view, common to
R\_AddLine and R\_CheckBBox: Nothing when the span is ANG180 or more, or entirely
outside the view.
-}
clipAngles : Ctx -> Int -> Int -> Maybe ( Int, Int )
clipAngles ctx angle1 angle2 =
    let
        span =
            Angle.sub angle1 angle2

        clipangle =
            ctx.vs.clipangle

        tspan1 =
            Angle.add angle1 clipangle
    in
    if span >= ang180 then
        Nothing

    else if tspan1 > 2 * clipangle && Angle.sub tspan1 (2 * clipangle) >= span then
        Nothing

    else
        let
            a1 =
                if tspan1 > 2 * clipangle then
                    clipangle

                else
                    angle1

            tspan2 =
                Angle.sub clipangle angle2
        in
        if tspan2 > 2 * clipangle && Angle.sub tspan2 (2 * clipangle) >= span then
            Nothing

        else if tspan2 > 2 * clipangle then
            Just ( a1, Angle.neg clipangle )

        else
            Just ( a1, angle2 )


{-| R\_Subsector: find its floor and ceiling planes, project its sector's things, and
add its segs.
-}
subsector : Ctx -> Int -> St -> St
subsector ctx num st =
    let
        sub =
            Level.subsector num ctx.level

        sec =
            World.sector sub.sector ctx.world

        st1 =
            if sec.floorheight < ctx.viewz then
                findPlane ctx sec.floorheight sec.floorpic sec.lightlevel st
                    |> (\( i, s ) -> { s | floorplane = i })

            else
                { st | floorplane = -1 }

        st2 =
            if sec.ceilingheight > ctx.viewz || sec.ceilingpic == ctx.skyflatnum then
                findPlane ctx sec.ceilingheight sec.ceilingpic sec.lightlevel st1
                    |> (\( i, s ) -> { s | ceilingplane = i })

            else
                { st1 | ceilingplane = -1 }

        st3 =
            addSprites ctx sub.sector sec st2
    in
    List.foldl (\i s -> addLine ctx sec i s) st3 (List.range sub.firstline (sub.firstline + sub.numlines - 1))


{-| R\_AddLine: clip a seg to the view and to the walls already drawn.
-}
addLine : Ctx -> SectorState -> Int -> St -> St
addLine ctx front segIndex st =
    let
        seg =
            Level.seg segIndex ctx.level

        angle1 =
            pointToAngle ctx seg.v1.x seg.v1.y

        angle2 =
            pointToAngle ctx seg.v2.x seg.v2.y
    in
    case clipAngles ctx (Angle.sub angle1 ctx.viewangle) (Angle.sub angle2 ctx.viewangle) of
        Nothing ->
            st

        Just ( a1, a2 ) ->
            let
                x1 =
                    at (Angle.toFine (Angle.add a1 ang90)) ctx.vs.viewangletox

                x2 =
                    at (Angle.toFine (Angle.add a2 ang90)) ctx.vs.viewangletox

                wall =
                    { seg = seg, front = front, back = Maybe.map (\b -> World.sector b ctx.world) seg.backsector, angle1 = angle1 }
            in
            if x1 == x2 then
                st

            else
                case wall.back of
                    Nothing ->
                        clipSolid ctx wall x1 (x2 - 1) st

                    Just back ->
                        if back.ceilingheight <= front.floorheight || back.floorheight >= front.ceilingheight then
                            clipSolid ctx wall x1 (x2 - 1) st

                        else if back.ceilingheight /= front.ceilingheight || back.floorheight /= front.floorheight then
                            clipPass ctx wall x1 (x2 - 1) st

                        else if
                            back.ceilingpic
                                == front.ceilingpic
                                && back.floorpic
                                == front.floorpic
                                && back.lightlevel
                                == front.lightlevel
                                && (World.sideState seg.sidedef ctx.world).midtexture
                                == 0
                        then
                            st

                        else
                            clipPass ctx wall x1 (x2 - 1) st


type alias WallSeg =
    { seg : Seg
    , front : SectorState
    , back : Maybe SectorState
    , angle1 : Int
    }


{-| R\_ClipSolidWallSegment: draw the parts of [first, last] not yet covered, and mark
it all covered.
-}
clipSolid : Ctx -> WallSeg -> Int -> Int -> St -> St
clipSolid ctx wall first last st =
    let
        ( newSegs, ranges ) =
            solidRanges first last st.solidsegs
    in
    List.foldl (\( a, b ) s -> storeWallRange ctx wall a b s) { st | solidsegs = newSegs } ranges


{-| R\_ClipPassWallSegment: draw the parts of [first, last] not yet covered.
-}
clipPass : Ctx -> WallSeg -> Int -> Int -> St -> St
clipPass ctx wall first last st =
    List.foldl (\( a, b ) s -> storeWallRange ctx wall a b s) st (passRanges first last st.solidsegs)


{-| The solidsegs list after adding [first, last], and the ranges R\_ClipSolidWallSegment
draws, in order.
-}
solidRanges : Int -> Int -> List ( Int, Int ) -> ( List ( Int, Int ), List ( Int, Int ) )
solidRanges first last segs =
    let
        go before rest =
            case rest of
                ( sf, sl ) :: after ->
                    if sl < first - 1 then
                        go (( sf, sl ) :: before) after

                    else if first < sf && last < sf - 1 then
                        ( List.reverse before ++ (( first, last ) :: rest), [ ( first, last ) ] )

                    else
                        let
                            ( startFirst, stores ) =
                                if first < sf then
                                    ( first, [ ( first, sf - 1 ) ] )

                                else
                                    ( sf, [] )
                        in
                        if last <= sl then
                            ( List.reverse before ++ (( startFirst, sl ) :: after), stores )

                        else
                            merge before startFirst sl after (List.reverse stores)

                [] ->
                    ( List.reverse before, [] )

        merge before startFirst nextLast after storesRev =
            case after of
                ( nf, nl ) :: after2 ->
                    if last >= nf - 1 then
                        let
                            storesRev2 =
                                ( nextLast + 1, nf - 1 ) :: storesRev
                        in
                        if last <= nl then
                            ( List.reverse before ++ (( startFirst, nl ) :: after2), List.reverse storesRev2 )

                        else
                            merge before startFirst nl after2 storesRev2

                    else
                        ( List.reverse before ++ (( startFirst, last ) :: after), List.reverse (( nextLast + 1, last ) :: storesRev) )

                [] ->
                    ( List.reverse before ++ [ ( startFirst, last ) ], List.reverse (( nextLast + 1, last ) :: storesRev) )
    in
    go [] segs


{-| The ranges R\_ClipPassWallSegment draws.
-}
passRanges : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
passRanges first last segs =
    let
        go rest =
            case rest of
                ( sf, sl ) :: after ->
                    if sl < first - 1 then
                        go after

                    else if first < sf && last < sf - 1 then
                        [ ( first, last ) ]

                    else
                        let
                            stores =
                                if first < sf then
                                    [ ( first, sf - 1 ) ]

                                else
                                    []
                        in
                        if last <= sl then
                            stores

                        else
                            stores ++ walkOn sl after []

                [] ->
                    []

        walkOn startLast after acc =
            case after of
                ( nf, nl ) :: after2 ->
                    if last >= nf - 1 then
                        if last <= nl then
                            List.reverse (( startLast + 1, nf - 1 ) :: acc)

                        else
                            walkOn nl after2 (( startLast + 1, nf - 1 ) :: acc)

                    else
                        List.reverse (( startLast + 1, last ) :: acc)

                [] ->
                    List.reverse (( startLast + 1, last ) :: acc)
    in
    go segs



-- PLANES (finding them)


{-| R\_FindPlane: the first visplane with this height, flat and light (all skies are
one), or a new one.
-}
findPlane : Ctx -> Int -> Int -> Int -> St -> ( Int, St )
findPlane ctx height0 picnum lightlevel0 st =
    let
        ( height, lightlevel ) =
            if picnum == ctx.skyflatnum then
                ( 0, 0 )

            else
                ( height0, lightlevel0 )

        key =
            ( height, picnum, lightlevel )
    in
    case Dict.get key st.planeIndex of
        Just i ->
            ( i, st )

        Nothing ->
            let
                i =
                    Array.length st.planes
            in
            ( i
            , { st
                | planes = Array.push (newPlane height picnum lightlevel 320 -1) st.planes
                , planeIndex = Dict.insert key i st.planeIndex
              }
            )


newPlane : Int -> Int -> Int -> Int -> Int -> Plane
newPlane height picnum lightlevel minx maxx =
    { height = height
    , picnum = picnum
    , lightlevel = lightlevel
    , minx = minx
    , maxx = maxx
    , top = Array.repeat 320 0xFF
    , bottom = Array.repeat 320 0
    }


{-| R\_CheckPlane: the plane grown to cover [start, stop], or a copy of it for those
columns if it already has some of them.
-}
checkPlane : Int -> Int -> Int -> Array Plane -> ( Int, Array Plane )
checkPlane index start stop planes =
    case Array.get index planes of
        Nothing ->
            ( index, planes )

        Just pl ->
            let
                ( intrl, unionl ) =
                    if start < pl.minx then
                        ( pl.minx, start )

                    else
                        ( start, pl.minx )

                ( intrh, unionh ) =
                    if stop > pl.maxx then
                        ( pl.maxx, stop )

                    else
                        ( stop, pl.maxx )

                free x =
                    x > intrh || (at x pl.top == 0xFF && free (x + 1))
            in
            if free intrl then
                ( index, Array.set index { pl | minx = unionl, maxx = unionh } planes )

            else
                ( Array.length planes, Array.push (newPlane pl.height pl.picnum pl.lightlevel start stop) planes )


setPlaneColumn : Int -> Int -> Int -> Int -> Array Plane -> Array Plane
setPlaneColumn index x top bottom planes =
    case Array.get index planes of
        Just pl ->
            Array.set index { pl | top = Array.set x top pl.top, bottom = Array.set x bottom pl.bottom } planes

        Nothing ->
            planes



-- WALLS


type alias Tex =
    { columns : Array Source
    , mask : Int
    }


{-| R\_GetColumn's texture, ready to read columns from.
-}
getTexture : Ctx -> Int -> Cache -> ( Tex, Cache )
getTexture ctx texnum cache =
    let
        ( cols, c1 ) =
            Internal.texture ctx.static texnum cache
    in
    ( { columns = cols, mask = Array.length cols - 1 }, c1 )


column : Tex -> Int -> Source
column tex col =
    Array.get (Bitwise.and col tex.mask) tex.columns |> Maybe.withDefault { mem = Array.empty, offset = 0 }


{-| What R\_StoreWallRange works out for R\_RenderSegLoop.
-}
type alias Wall =
    { stop : Int
    , scalestep : Int
    , topstep : Int
    , bottomstep : Int
    , markceiling : Bool
    , markfloor : Bool
    , mid : Maybe Tex
    , top : Maybe Tex
    , bottom : Maybe Tex
    , masked : Bool
    , segtextured : Bool
    , centerangle : Int
    , offset : Int
    , distance : Int
    , walllights : Array (Array Int)
    , midtexturemid : Int
    , toptexturemid : Int
    , bottomtexturemid : Int
    , ceilingplane : Int
    , floorplane : Int
    , pixhighstep : Int
    , pixlowstep : Int
    }


{-| R\_StoreWallRange: draw the seg's columns start to stop and record a drawseg.
-}
storeWallRange : Ctx -> WallSeg -> Int -> Int -> St -> St
storeWallRange ctx wall start stop st =
    if st.numDrawsegs == 256 then
        st

    else
        let
            vs =
                ctx.vs

            seg =
                wall.seg

            front =
                wall.front

            side =
                World.sideState seg.sidedef ctx.world

            line =
                Level.line seg.linedef ctx.level

            normalangle =
                Angle.add seg.angle ang90

            offsetangle0 =
                Int32.toUnsigned (Int32.abs (Int32.wrap (normalangle - wall.angle1)))

            distangle =
                ang90 - min offsetangle0 ang90

            hyp =
                Geometry.pointToDist ctx.viewx ctx.viewy seg.v1.x seg.v1.y

            distance =
                Fixed.mul hyp (Tables.finesine (Angle.toFine distangle))

            scale1 =
                scaleFromGlobalAngle ctx normalangle distance (Angle.add ctx.viewangle (at start vs.xtoviewangle))

            ( scale2, scalestep ) =
                if stop > start then
                    let
                        s2 =
                            scaleFromGlobalAngle ctx normalangle distance (Angle.add ctx.viewangle (at stop vs.xtoviewangle))
                    in
                    ( s2, (s2 - scale1) // (stop - start) )

                else
                    ( scale1, 0 )

            worldtop0 =
                Int32.sub front.ceilingheight ctx.viewz

            worldbottom =
                Int32.sub front.floorheight ctx.viewz

            dontPegBottom =
                Bitwise.and line.flags Flags.mlDontPegBottom /= 0

            dontPegTop =
                Bitwise.and line.flags Flags.mlDontPegTop /= 0

            sides =
                case wall.back of
                    Nothing ->
                        { midtexture = textureTranslation ctx side.midtexture
                        , toptexture = 0
                        , bottomtexture = 0
                        , masked = False
                        , markfloor = True
                        , markceiling = True
                        , midtexturemid =
                            Int32.add side.rowoffset
                                (if dontPegBottom then
                                    Int32.sub (Int32.add front.floorheight (textureHeight ctx side.midtexture)) ctx.viewz

                                 else
                                    worldtop0
                                )
                        , toptexturemid = 0
                        , bottomtexturemid = 0
                        , silhouette = 3
                        , sprtopclip = AllHeight
                        , sprbottomclip = AllNegOne
                        , bsilheight = maxInt
                        , tsilheight = minInt
                        , worldtop = worldtop0
                        , worldhigh = 0
                        , worldlow = 0
                        }

                    Just back ->
                        let
                            ( sil1, bsil1 ) =
                                if front.floorheight > back.floorheight then
                                    ( silBottom, front.floorheight )

                                else if back.floorheight > ctx.viewz then
                                    ( silBottom, maxInt )

                                else
                                    ( 0, 0 )

                            ( sil2, tsil2 ) =
                                if front.ceilingheight < back.ceilingheight then
                                    ( Bitwise.or sil1 silTop, front.ceilingheight )

                                else if back.ceilingheight < ctx.viewz then
                                    ( Bitwise.or sil1 silTop, minInt )

                                else
                                    ( sil1, 0 )

                            closedBelow =
                                back.ceilingheight <= front.floorheight

                            closedAbove =
                                back.floorheight >= front.ceilingheight

                            ( sil3, bsil3, sprbottom ) =
                                if closedBelow then
                                    ( Bitwise.or sil2 silBottom, maxInt, AllNegOne )

                                else
                                    ( sil2, bsil1, NoClip )

                            ( sil4, tsil4, sprtop ) =
                                if closedAbove then
                                    ( Bitwise.or sil3 silTop, minInt, AllHeight )

                                else
                                    ( sil3, tsil2, NoClip )

                            high =
                                Int32.sub back.ceilingheight ctx.viewz

                            low =
                                Int32.sub back.floorheight ctx.viewz

                            top =
                                if front.ceilingpic == ctx.skyflatnum && back.ceilingpic == ctx.skyflatnum then
                                    high

                                else
                                    worldtop0

                            closed =
                                closedBelow || closedAbove
                        in
                        { midtexture = 0
                        , toptexture =
                            if high < top then
                                textureTranslation ctx side.toptexture

                            else
                                0
                        , bottomtexture =
                            if low > worldbottom then
                                textureTranslation ctx side.bottomtexture

                            else
                                0
                        , masked = side.midtexture /= 0
                        , markfloor =
                            closed || low /= worldbottom || back.floorpic /= front.floorpic || back.lightlevel /= front.lightlevel
                        , markceiling =
                            closed || high /= top || back.ceilingpic /= front.ceilingpic || back.lightlevel /= front.lightlevel
                        , midtexturemid = 0
                        , toptexturemid =
                            Int32.add side.rowoffset
                                (if dontPegTop then
                                    top

                                 else
                                    Int32.sub (Int32.add back.ceilingheight (textureHeight ctx side.toptexture)) ctx.viewz
                                )
                        , bottomtexturemid =
                            Int32.add side.rowoffset
                                (if dontPegBottom then
                                    top

                                 else
                                    low
                                )
                        , silhouette = sil4
                        , sprtopclip = sprtop
                        , sprbottomclip = sprbottom
                        , bsilheight = bsil3
                        , tsilheight = tsil4
                        , worldtop = top
                        , worldhigh = high
                        , worldlow = low
                        }

            segtextured =
                sides.midtexture /= 0 || sides.toptexture /= 0 || sides.bottomtexture /= 0 || sides.masked

            rwOffset =
                if segtextured then
                    let
                        oa0 =
                            Angle.sub normalangle wall.angle1

                        oa1 =
                            if oa0 > ang180 then
                                Angle.neg oa0

                            else
                                oa0

                        oa =
                            min oa1 ang90

                        o =
                            Fixed.mul hyp (Tables.finesine (Angle.toFine oa))

                        signed =
                            if Angle.sub normalangle wall.angle1 < ang180 then
                                Int32.neg o

                            else
                                o
                    in
                    Int32.add (Int32.add signed side.textureoffset) seg.offset

                else
                    0

            walllights =
                case ctx.fixedcolormap of
                    Just fixed ->
                        Array.repeat Internal.maxLightScale fixed

                    Nothing ->
                        let
                            lightnum0 =
                                Int32.shiftRight 4 front.lightlevel + ctx.extralight

                            lightnum =
                                if seg.v1.y == seg.v2.y then
                                    lightnum0 - 1

                                else if seg.v1.x == seg.v2.x then
                                    lightnum0 + 1

                                else
                                    lightnum0
                        in
                        Array.map (colormap ctx.static) (lightRow ctx lightnum)

            markfloor =
                sides.markfloor && front.floorheight < ctx.viewz

            markceiling =
                sides.markceiling && not (front.ceilingheight <= ctx.viewz && front.ceilingpic /= ctx.skyflatnum)

            worldtop =
                Int32.shiftRight 4 sides.worldtop

            worldbottom4 =
                Int32.shiftRight 4 worldbottom

            centery4 =
                Int32.shiftRight 4 vs.centeryfrac

            worldhigh =
                Int32.shiftRight 4 sides.worldhigh

            worldlow =
                Int32.shiftRight 4 sides.worldlow

            hasBack =
                wall.back /= Nothing

            ( pixhigh, pixhighstep ) =
                if hasBack && worldhigh < worldtop then
                    ( Int32.sub centery4 (Fixed.mul worldhigh scale1), Int32.neg (Fixed.mul scalestep worldhigh) )

                else
                    ( st.pixhigh, st.pixhighstep )

            ( pixlow, pixlowstep ) =
                if hasBack && worldlow > worldbottom4 then
                    ( Int32.sub centery4 (Fixed.mul worldlow scale1), Int32.neg (Fixed.mul scalestep worldlow) )

                else
                    ( st.pixlow, st.pixlowstep )

            ( ceilingplane, planes1 ) =
                if markceiling then
                    checkPlane st.ceilingplane start stop st.planes

                else
                    ( st.ceilingplane, st.planes )

            ( floorplane, planes2 ) =
                if markfloor then
                    checkPlane st.floorplane start stop planes1

                else
                    ( st.floorplane, planes1 )

            maybeTex texnum cache =
                if texnum == 0 then
                    ( Nothing, cache )

                else
                    getTexture ctx texnum cache |> Tuple.mapFirst Just

            ( midTex, c1 ) =
                maybeTex sides.midtexture st.cache

            ( topTex, c2 ) =
                maybeTex sides.toptexture c1

            ( bottomTex, c3 ) =
                maybeTex sides.bottomtexture c2

            w =
                { stop = stop
                , scalestep = scalestep
                , topstep = Int32.neg (Fixed.mul scalestep worldtop)
                , bottomstep = Int32.neg (Fixed.mul scalestep worldbottom4)
                , markceiling = markceiling
                , markfloor = markfloor
                , mid = midTex
                , top = topTex
                , bottom = bottomTex
                , masked = sides.masked
                , segtextured = segtextured
                , centerangle = Angle.sub (Angle.add ang90 ctx.viewangle) normalangle
                , offset = rwOffset
                , distance = distance
                , walllights = walllights
                , midtexturemid = sides.midtexturemid
                , toptexturemid = sides.toptexturemid
                , bottomtexturemid = sides.bottomtexturemid
                , ceilingplane = ceilingplane
                , floorplane = floorplane
                , pixhighstep = pixhighstep
                , pixlowstep = pixlowstep
                }

            done =
                segLoop ctx
                    w
                    start
                    scale1
                    (Int32.sub centery4 (Fixed.mul worldtop scale1))
                    (Int32.sub centery4 (Fixed.mul worldbottom4 scale1))
                    pixhigh
                    pixlow
                    { cc = st.ceilingclip, fc = st.floorclip, planes = planes2, ops = st.ops, maskedcols = [] }

            sprtopclip =
                if (Bitwise.and sides.silhouette silTop /= 0 || sides.masked) && sides.sprtopclip == NoClip then
                    Clipped (Array.slice start (stop + 1) done.acc.cc)

                else
                    sides.sprtopclip

            sprbottomclip =
                if (Bitwise.and sides.silhouette silBottom /= 0 || sides.masked) && sides.sprbottomclip == NoClip then
                    Clipped (Array.slice start (stop + 1) done.acc.fc)

                else
                    sides.sprbottomclip

            ( sil5, tsil5 ) =
                if sides.masked && Bitwise.and sides.silhouette silTop == 0 then
                    ( Bitwise.or sides.silhouette silTop, minInt )

                else
                    ( sides.silhouette, sides.tsilheight )

            ( sil6, bsil6 ) =
                if sides.masked && Bitwise.and sil5 silBottom == 0 then
                    ( Bitwise.or sil5 silBottom, maxInt )

                else
                    ( sil5, sides.bsilheight )

            drawseg =
                { x1 = start
                , x2 = stop
                , seg = seg
                , scale1 = scale1
                , scale2 = scale2
                , scalestep = scalestep
                , silhouette = sil6
                , bsilheight = bsil6
                , tsilheight = tsil5
                , sprtopclip = sprtopclip
                , sprbottomclip = sprbottomclip
                , maskedtexturecol =
                    if sides.masked then
                        Just (Array.fromList (List.reverse done.acc.maskedcols))

                    else
                        Nothing
                }
        in
        { st
            | ceilingclip = done.acc.cc
            , floorclip = done.acc.fc
            , planes = done.acc.planes
            , ceilingplane = ceilingplane
            , floorplane = floorplane
            , ops = done.acc.ops
            , drawsegs = drawseg :: st.drawsegs
            , numDrawsegs = st.numDrawsegs + 1
            , mapped = seg.linedef :: st.mapped
            , cache = c3
            , pixhigh = done.pixhigh
            , pixhighstep = pixhighstep
            , pixlow = done.pixlow
            , pixlowstep = pixlowstep
        }


{-| R\_ScaleFromGlobalAngle: the wall's scale at a view angle.
-}
scaleFromGlobalAngle : Ctx -> Int -> Int -> Int -> Int
scaleFromGlobalAngle ctx normalangle distance visangle =
    let
        anglea =
            Int32.wrap (ang90 + Angle.sub visangle ctx.viewangle)

        angleb =
            Int32.wrap (ang90 + Angle.sub visangle normalangle)

        sinea =
            finesineAny (Int32.shiftRight 19 anglea)

        sineb =
            finesineAny (Int32.shiftRight 19 angleb)

        num =
            Int32.shiftLeft ctx.vs.detailshift (Fixed.mul ctx.vs.projection sineb)

        den =
            Fixed.mul distance sinea
    in
    if den > Int32.shiftRight 16 num then
        let
            scale =
                Fixed.div num den
        in
        if scale > 64 * Fixed.fracunit then
            64 * Fixed.fracunit

        else if scale < 256 then
            256

        else
            scale

    else
        64 * Fixed.fracunit


type alias SegAcc =
    { cc : Array Int
    , fc : Array Int
    , planes : Array Plane
    , ops : Array (List Op)
    , maskedcols : List Int
    }


{-| R\_RenderSegLoop: the seg's columns, left to right.
-}
segLoop : Ctx -> Wall -> Int -> Int -> Int -> Int -> Int -> Int -> SegAcc -> { acc : SegAcc, pixhigh : Int, pixlow : Int }
segLoop ctx w x scale topfrac bottomfrac pixhigh pixlow acc =
    if x > w.stop then
        { acc = acc, pixhigh = pixhigh, pixlow = pixlow }

    else
        let
            cc =
                at x acc.cc

            fc =
                at x acc.fc

            yl0 =
                Int32.shiftRight 12 (Int32.add topfrac 4095)

            yl =
                if yl0 < cc + 1 then
                    cc + 1

                else
                    yl0

            planes1 =
                if w.markceiling then
                    let
                        bottom =
                            if yl - 1 >= fc then
                                fc - 1

                            else
                                yl - 1
                    in
                    if cc + 1 <= bottom then
                        setPlaneColumn w.ceilingplane x (cc + 1) bottom acc.planes

                    else
                        acc.planes

                else
                    acc.planes

            yh0 =
                Int32.shiftRight 12 bottomfrac

            yh =
                if yh0 >= fc then
                    fc - 1

                else
                    yh0

            planes2 =
                if w.markfloor then
                    let
                        top =
                            if yh + 1 <= cc then
                                cc + 1

                            else
                                yh + 1
                    in
                    if top <= fc - 1 then
                        setPlaneColumn w.floorplane x top (fc - 1) planes1

                    else
                        planes1

                else
                    planes1

            ( texturecolumn, cmap, iscale ) =
                if w.segtextured then
                    let
                        angle =
                            Angle.toFine (Angle.add w.centerangle (at x ctx.vs.xtoviewangle))
                    in
                    ( Int32.shiftRight 16 (Int32.sub w.offset (Fixed.mul (finetangentAny angle) w.distance))
                    , Array.get (lightIndex Internal.maxLightScale (Int32.shiftRight 12 scale)) w.walllights |> Maybe.withDefault Array.empty
                    , invScale scale
                    )

                else
                    ( 0, Array.empty, 0 )

            next a ph pl =
                segLoop ctx
                    w
                    (x + 1)
                    (Int32.add scale w.scalestep)
                    (Int32.add topfrac w.topstep)
                    (Int32.add bottomfrac w.bottomstep)
                    ph
                    pl
                    a
        in
        case w.mid of
            Just tex ->
                next
                    { acc
                        | cc = Array.set x ctx.vs.viewheight acc.cc
                        , fc = Array.set x -1 acc.fc
                        , planes = planes2
                        , ops = drawColumn ctx x yl yh w.midtexturemid iscale (column tex texturecolumn) cmap acc.ops
                    }
                    pixhigh
                    pixlow

            Nothing ->
                let
                    ( cc2, ops1, pixhigh2 ) =
                        case w.top of
                            Just tex ->
                                let
                                    mid0 =
                                        Int32.shiftRight 12 pixhigh

                                    mid =
                                        if mid0 >= fc then
                                            fc - 1

                                        else
                                            mid0
                                in
                                if mid >= yl then
                                    ( mid, drawColumn ctx x yl mid w.toptexturemid iscale (column tex texturecolumn) cmap acc.ops, Int32.add pixhigh w.pixhighstep )

                                else
                                    ( yl - 1, acc.ops, Int32.add pixhigh w.pixhighstep )

                            Nothing ->
                                if w.markceiling then
                                    ( yl - 1, acc.ops, pixhigh )

                                else
                                    ( cc, acc.ops, pixhigh )

                    ( fc2, ops2, pixlow2 ) =
                        case w.bottom of
                            Just tex ->
                                let
                                    mid0 =
                                        Int32.shiftRight 12 (Int32.add pixlow 4095)

                                    mid =
                                        if mid0 <= cc2 then
                                            cc2 + 1

                                        else
                                            mid0
                                in
                                if mid <= yh then
                                    ( mid, drawColumn ctx x mid yh w.bottomtexturemid iscale (column tex texturecolumn) cmap ops1, Int32.add pixlow w.pixlowstep )

                                else
                                    ( yh + 1, ops1, Int32.add pixlow w.pixlowstep )

                            Nothing ->
                                if w.markfloor then
                                    ( yh + 1, ops1, pixlow )

                                else
                                    ( fc, ops1, pixlow )
                in
                next
                    { cc = Array.set x cc2 acc.cc
                    , fc = Array.set x fc2 acc.fc
                    , planes = planes2
                    , ops = ops2
                    , maskedcols =
                        if w.masked then
                            Int32.toShort texturecolumn :: acc.maskedcols

                        else
                            acc.maskedcols
                    }
                    pixhigh2
                    pixlow2



-- COLUMN DRAWERS


{-| R\_DrawColumn (or R\_DrawColumnLow, two screen columns per view column) from row
yl to yh of view column x.
-}
drawColumn : Ctx -> Int -> Int -> Int -> Int -> Int -> Source -> Array Int -> Array (List Op) -> Array (List Op)
drawColumn ctx x yl yh texturemid iscale source cmap ops =
    let
        count =
            yh - yl
    in
    if count < 0 then
        ops

    else
        let
            frac0 =
                Int32.add texturemid (Int32.mul (yl - ctx.vs.centery) iscale)

            op =
                Run (ctx.wy + yl) (columnRun source.mem source.offset cmap frac0 iscale count [])
        in
        pushView ctx x op ops


{-| Add a write to view column x's screen column, or in low detail to both of its
screen columns (R\_DrawColumnLow, R\_DrawFuzzColumnLow, R\_DrawTranslatedColumnLow
and R\_DrawSpanLow all write `x << 1` and the column after it).
-}
pushView : Ctx -> Int -> Op -> Array (List Op) -> Array (List Op)
pushView ctx x op ops =
    if ctx.low then
        Compose.push (ctx.wx + 2 * x) op (Compose.push (ctx.wx + 2 * x + 1) op ops)

    else
        Compose.push (ctx.wx + x) op ops


{-| The pixels `dc_colormap[dc_source[(frac>>FRACBITS)&127]]` for k = 0 to count, frac
being frac0 + k \* step. Only bits 16 to 22 of frac are used, which the sum has
right without wrapping it to 32 bits.
-}
columnRun : Mem -> Int -> Array Int -> Int -> Int -> Int -> List Int -> List Int
columnRun mem ofs cmap frac0 step k acc =
    if k < 0 then
        acc

    else
        let
            -- Mem.byte and the colormap lookup, written out: this is the
            -- innermost loop of every wall and sprite
            i =
                ofs + Bitwise.and 127 (Bitwise.shiftRightBy 16 (frac0 + k * step))

            b =
                case Array.get (Bitwise.shiftRightBy 1 i) mem of
                    Just w ->
                        if Bitwise.and 1 i == 0 then
                            Bitwise.and 255 w

                        else
                            Bitwise.shiftRightBy 8 w

                    Nothing ->
                        0

            c =
                case Array.get b cmap of
                    Just v ->
                        v

                    Nothing ->
                        0
        in
        columnRun mem ofs cmap frac0 step (k - 1) (c :: acc)


{-| R\_DrawTranslatedColumn: `dc_colormap[dc_translation[dc_source[frac>>FRACBITS]]]`
(no wrapping of the row).
-}
translatedRun : Mem -> Int -> Array Int -> Int -> Int -> Int -> Int -> List Int -> List Int
translatedRun mem ofs cmap table frac0 step k acc =
    if k < 0 then
        acc

    else
        let
            b =
                Mem.byte (ofs + Int32.shiftRight 16 (Int32.wrap (frac0 + k * step))) mem

            t =
                if b >= 0x70 && b <= 0x7F then
                    table + Bitwise.and b 15

                else
                    b
        in
        translatedRun mem ofs cmap table frac0 step (k - 1) ((Array.get t cmap |> Maybe.withDefault 0) :: acc)


{-| How a masked column is drawn: colfunc, R\_DrawTranslatedColumn with a translation
(its first index, 0x60, 0x40 or 0x20), or R\_DrawFuzzColumn.
-}
type ColFunc
    = Base (Array Int)
    | Translated (Array Int) Int
    | Shadow


type alias MaskedSt =
    { ops : Array (List Op)
    , fuzzpos : Int
    }


{-| colfunc for a masked column: R\_DrawColumn's kinds, writing into `ops`.
-}
drawMaskedRun : Ctx -> ColFunc -> Int -> Int -> Int -> Int -> Int -> Source -> MaskedSt -> MaskedSt
drawMaskedRun ctx colfunc x yl yh texturemid iscale source ms =
    case colfunc of
        Base cmap ->
            { ms | ops = drawColumn ctx x yl yh texturemid iscale source cmap ms.ops }

        Translated cmap table ->
            let
                count =
                    yh - yl
            in
            if count < 0 then
                ms

            else
                let
                    frac0 =
                        Int32.add texturemid (Int32.mul (yl - ctx.vs.centery) iscale)
                in
                { ms | ops = pushView ctx x (Run (ctx.wy + yl) (translatedRun source.mem source.offset cmap table frac0 iscale count [])) ms.ops }

        Shadow ->
            let
                yl2 =
                    if yl == 0 then
                        1

                    else
                        yl

                yh2 =
                    if yh == ctx.vs.viewheight - 1 then
                        ctx.vs.viewheight - 2

                    else
                        yh

                count =
                    yh2 - yl2
            in
            if count < 0 then
                ms

            else
                { ops = pushView ctx x (Fuzz (ctx.wy + yl2) (ctx.wy + yh2) ms.fuzzpos) ms.ops
                , fuzzpos = modBy 50 (ms.fuzzpos + count + 1)
                }


{-| R\_DrawMaskedColumn: the posts of a column (starting at `ofs` in `mem`), clipped
to the column's floor and ceiling clips.
-}
drawMaskedColumn : Ctx -> ColFunc -> Int -> Mem -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> MaskedSt -> MaskedSt
drawMaskedColumn ctx colfunc x mem ofs texturemid sprtopscreen spryscale iscale floorclip ceilingclip ms =
    let
        topdelta =
            Mem.byte ofs mem
    in
    if topdelta == 0xFF || ofs < 0 || ofs >= Mem.size mem then
        ms

    else
        let
            length =
                Mem.byte (ofs + 1) mem

            topscreen =
                Int32.add sprtopscreen (Int32.mul spryscale topdelta)

            bottomscreen =
                Int32.add topscreen (Int32.mul spryscale length)

            yl0 =
                Int32.shiftRight 16 (Int32.add topscreen (Fixed.fracunit - 1))

            yh0 =
                Int32.shiftRight 16 (Int32.sub bottomscreen 1)

            yh =
                if yh0 >= floorclip then
                    floorclip - 1

                else
                    yh0

            yl =
                if yl0 <= ceilingclip then
                    ceilingclip + 1

                else
                    yl0

            ms2 =
                if yl <= yh then
                    drawMaskedRun ctx colfunc x yl yh (Int32.sub texturemid (topdelta * Fixed.fracunit)) iscale { mem = mem, offset = ofs + 3 } ms

                else
                    ms
        in
        drawMaskedColumn ctx colfunc x mem (ofs + length + 4) texturemid sprtopscreen spryscale iscale floorclip ceilingclip ms2



-- PLANES (drawing them)


{-| A span as R\_MapPlane sets it up, its fractions packed as the DOS R\_DrawSpan
packs them (see `packSpan`).
-}
type alias SpanStart =
    { x1 : Int
    , position : Int
    , step : Int
    , cmap : Array Int
    }


type alias PlaneCache =
    { height : Array Int
    , distance : Array Int
    , xstep : Array Int
    , ystep : Array Int
    }


{-| R\_DrawPlanes: every visplane in order, skies as columns of the sky texture,
flats as spans.
-}
drawPlanes : Ctx -> Array Plane -> Array (List Op) -> Cache -> ( Array (List Op), Cache )
drawPlanes ctx planes ops0 cache0 =
    let
        pc0 =
            { height = Array.repeat 200 0
            , distance = cache0.cachedDistance
            , xstep = cache0.cachedXstep
            , ystep = cache0.cachedYstep
            }

        ( ops, pc, cache ) =
            Array.foldl
                (\pl ( ops1, pc1, c1 ) ->
                    if pl.minx > pl.maxx then
                        ( ops1, pc1, c1 )

                    else if pl.picnum == ctx.skyflatnum then
                        let
                            ( sky, c2 ) =
                                getTexture ctx ctx.skytexture c1
                        in
                        ( drawSky ctx sky pl ops1, pc1, c2 )

                    else
                        let
                            lump =
                                ctx.static.firstFlat + flatTranslation ctx pl.picnum

                            ( flat, c2 ) =
                                Internal.flat ctx.static lump c1

                            ( ops2, pc2 ) =
                                drawFlat ctx flat pl ops1 pc1
                        in
                        ( ops2, pc2, c2 )
                )
                ( ops0, pc0, cache0 )
                planes
    in
    ( ops, { cache | cachedDistance = pc.distance, cachedXstep = pc.xstep, cachedYstep = pc.ystep } )


drawSky : Ctx -> Tex -> Plane -> Array (List Op) -> Array (List Op)
drawSky ctx sky pl ops =
    let
        iscale =
            Int32.shiftRight ctx.vs.detailshift ctx.vs.pspriteiscale

        cmap =
            colormap ctx.static 0

        go x acc =
            if x > pl.maxx then
                acc

            else
                let
                    yl =
                        at x pl.top

                    yh =
                        at x pl.bottom
                in
                if yl <= yh then
                    let
                        angle =
                            Int32.shiftRightUnsigned 22 (Angle.add ctx.viewangle (at x ctx.vs.xtoviewangle))
                    in
                    go (x + 1) (drawColumn ctx x yl yh (100 * Fixed.fracunit) iscale (column sky angle) cmap acc)

                else
                    go (x + 1) acc
    in
    go pl.minx ops


{-| A flat plane: R\_MakeSpans over its columns, each span mapped by R\_MapPlane and
drawn by R\_DrawSpan. In high detail a pixel only depends on its row and where its
span starts, so the plane is laid out column by column; low detail's R\_DrawSpanLow
writes twice the span's length and is replayed span by span.
-}
drawFlat : Ctx -> Array Int -> Plane -> Array (List Op) -> PlaneCache -> ( Array (List Op), PlaneCache )
drawFlat ctx flat pl ops pc =
    let
        planeheight =
            Int32.abs (Int32.sub pl.height ctx.viewz)

        light =
            clamp 0 (Internal.lightLevels - 1) (Int32.shiftRight 4 pl.lightlevel + ctx.extralight)

        planezlight =
            Array.get light ctx.static.zlight |> Maybe.withDefault Array.empty

        topAt x =
            if x < pl.minx || x > pl.maxx then
                0xFF

            else
                at x pl.top

        bottomAt x =
            if x < pl.minx || x > pl.maxx then
                0

            else
                at x pl.bottom

        -- R_MapPlane's mapping of a span on row y starting at x1
        mapPlane y x1 cache =
            let
                ( ( distance, xstep, ystep ), cache2 ) =
                    if planeheight /= at y cache.height then
                        let
                            d =
                                Fixed.mul planeheight (at y ctx.vs.yslope)

                            xs =
                                Fixed.mul d ctx.basexscale

                            ys =
                                Fixed.mul d ctx.baseyscale
                        in
                        ( ( d, xs, ys )
                        , { height = Array.set y planeheight cache.height
                          , distance = Array.set y d cache.distance
                          , xstep = Array.set y xs cache.xstep
                          , ystep = Array.set y ys cache.ystep
                          }
                        )

                    else
                        ( ( at y cache.distance, at y cache.xstep, at y cache.ystep ), cache )

                length =
                    Fixed.mul distance (at x1 ctx.vs.distscale)

                angle =
                    Angle.toFine (Angle.add ctx.viewangle (at x1 ctx.vs.xtoviewangle))

                cmap =
                    case ctx.fixedcolormap of
                        Just fixed ->
                            fixed

                        Nothing ->
                            colormap ctx.static (at (lightIndex Internal.maxLightZ (Int32.shiftRight 20 distance)) planezlight)
            in
            ( { x1 = x1
              , position =
                    packSpan
                        (Int32.add ctx.viewx (Fixed.mul (Tables.finecosine angle) length))
                        (Int32.sub (Int32.neg ctx.viewy) (Fixed.mul (Tables.finesine angle) length))
              , step = packSpan xstep ystep
              , cmap = cmap
              }
            , cache2
            )
    in
    let
        noSpan =
            { x1 = 0, position = 0, step = 0, cmap = Array.empty }

        open from to x ( starts, cache ) =
            if from > to then
                ( starts, cache )

            else
                let
                    ( s, c2 ) =
                        mapPlane from x cache
                in
                open (from + 1) to x ( Array.set from s starts, c2 )

        sweep x ( opsAcc, starts, cache ) =
            if x > pl.maxx + 1 then
                ( opsAcc, cache )

            else
                let
                    t1 =
                        topAt (x - 1)

                    b1 =
                        bottomAt (x - 1)

                    t2 =
                        topAt x

                    b2 =
                        bottomAt x

                    -- R_MakeSpans' loops: rows leaving close their spans (drawn
                    -- column by column below), rows entering start new ones
                    t1b =
                        if t1 < t2 && t1 <= b1 then
                            max t1 (min t2 (b1 + 1))

                        else
                            t1

                    b1b =
                        if b1 > b2 && b1 >= t1b then
                            max b2 (t1b - 1)

                        else
                            b1

                    ( t2b, firstOpen ) =
                        if t2 < t1b && t2 <= b2 then
                            ( max t2 (min t1b (b2 + 1)), Just ( t2, min (t1b - 1) b2 ) )

                        else
                            ( t2, Nothing )

                    secondOpen =
                        if b2 > b1b && b2 >= t2b then
                            Just ( max (b1b + 1) t2b, b2 )

                        else
                            Nothing

                    opened =
                        [ firstOpen, secondOpen ]
                            |> List.filterMap identity
                            |> List.foldl (\( a, b ) acc -> open a b x acc) ( starts, cache )

                    ( starts2, cache2 ) =
                        opened

                    opsAcc2 =
                        if x <= pl.maxx && t2 <= b2 then
                            pushView ctx x (Run (ctx.wy + t2) (spanColumn flat starts2 x t2 b2 [])) opsAcc

                        else
                            opsAcc
                in
                sweep (x + 1) ( opsAcc2, starts2, cache2 )
    in
    sweep pl.minx ( ops, Array.repeat 200 noSpan, pc )


{-| The pixels of rows `top` to `y` of column x of a flat, each from the span its
row is in: R\_DrawSpan's `ds_colormap[ds_source[spot]]` at x - x1 steps along it.
-}
spanColumn : Array Int -> Array SpanStart -> Int -> Int -> Int -> List Int -> List Int
spanColumn flat starts x top y acc =
    if y < top then
        acc

    else
        case Array.get y starts of
            Just s ->
                let
                    k =
                        x - s.x1
                in
                spanColumn flat starts x top (y - 1) (spanPixel flat s.cmap (s.position + k * s.step) :: acc)

            Nothing ->
                spanColumn flat starts x top (y - 1) (0 :: acc)


{-| The DOS R\_DrawSpan's packing of a pair of fractions into one 32-bit word: x's
bits 6 to 21 on top, y's bits 6 to 21 below, `((x << 10) & 0xffff0000) | ((y >> 6) &
0xffff)`. Stepping adds packed words, so a carry out of y's half spills into x's.
-}
packSpan : Int -> Int -> Int
packSpan x y =
    modBy 65536 (Int32.floorDiv x 64) * 65536 + modBy 65536 (Int32.floorDiv y 64)


{-| `ds_colormap[ds_source[(position >> 26) | ((position >> 4) & 0x0fc0)]]` for the
packed position (any multiple of 2^32 off).
-}
spanPixel : Array Int -> Array Int -> Int -> Int
spanPixel flat cmap unwrapped =
    let
        position =
            modBy 4294967296 unwrapped

        spot =
            position // 67108864 + modBy 64 (position // 1024) * 64
    in
    case Array.get spot flat of
        Just t ->
            case Array.get t cmap of
                Just c ->
                    c

                Nothing ->
                    0

        Nothing ->
            0



-- THINGS


{-| R\_AddSprites: project the things in a sector, the first time the walk enters it
this frame.
-}
addSprites : Ctx -> Int -> SectorState -> St -> St
addSprites ctx secId sec st =
    if Set.member secId st.seen then
        st

    else
        let
            spritelights =
                lightRow ctx (Int32.shiftRight 4 sec.lightlevel + ctx.extralight)

            things =
                Array.get secId ctx.world.sectorThings |> Maybe.withDefault []
        in
        List.foldl
            (\id s ->
                case World.mobj id ctx.world of
                    Just thing ->
                        projectSprite ctx spritelights thing s

                    Nothing ->
                        s
            )
            { st | seen = Set.insert secId st.seen }
            things


spriteFrame : Static -> Int -> Int -> Maybe Internal.SpriteFrame
spriteFrame static sprite frame =
    Array.get sprite static.sprites |> Maybe.andThen (Array.get (Bitwise.and frame 0x7FFF))


spriteLump : Static -> Int -> Internal.SpriteLump
spriteLump static lump =
    Array.get lump static.spriteLumps |> Maybe.withDefault { lump = 0, width = 0, offset = 0, topoffset = 0 }


{-| R\_ProjectSprite: a thing as a vissprite, if it is in front and in view.
-}
projectSprite : Ctx -> Array Int -> Mobj -> St -> St
projectSprite ctx spritelights thing st =
    let
        vs =
            ctx.vs

        viewsin =
            Angle.sine ctx.viewangle

        viewcos =
            Angle.cosine ctx.viewangle

        trx =
            Int32.sub thing.x ctx.viewx

        try =
            Int32.sub thing.y ctx.viewy

        tz =
            Int32.sub (Fixed.mul trx viewcos) (Int32.neg (Fixed.mul try viewsin))
    in
    if tz < 4 * Fixed.fracunit then
        st

    else
        let
            xscale =
                Fixed.div vs.projection tz

            tx0 =
                Int32.neg (Int32.add (Fixed.mul try viewcos) (Int32.neg (Fixed.mul trx viewsin)))
        in
        if Int32.abs tx0 > Int32.shiftLeft 2 tz then
            st

        else
            case spriteFrame ctx.static (Sprite.index thing.sprite) thing.frame of
                Nothing ->
                    st

                Just sprframe ->
                    let
                        rot =
                            if sprframe.rotate then
                                Int32.shiftRightUnsigned 29 (Angle.wrap (pointToAngle ctx thing.x thing.y - thing.angle + 0x10000000 * 9))

                            else
                                0

                        lumpNum =
                            Array.get rot sprframe.lumps |> Maybe.withDefault 0

                        flip =
                            Array.get rot sprframe.flips |> Maybe.withDefault False

                        lump =
                            spriteLump ctx.static lumpNum

                        tx1 =
                            Int32.sub tx0 lump.offset

                        x1 =
                            Int32.shiftRight 16 (Int32.add vs.centerxfrac (Fixed.mul tx1 xscale))

                        tx2 =
                            Int32.add tx1 lump.width

                        x2 =
                            Int32.shiftRight 16 (Int32.add vs.centerxfrac (Fixed.mul tx2 xscale)) - 1
                    in
                    if x1 > vs.viewwidth || x2 < 0 then
                        st

                    else if st.numVissprites >= 128 then
                        -- R_NewVisSprite hands out overflowsprite, which is never drawn
                        st

                    else
                        let
                            iscale =
                                Fixed.div Fixed.fracunit xscale

                            ( startfrac0, xiscale ) =
                                if flip then
                                    ( lump.width - 1, Int32.neg iscale )

                                else
                                    ( 0, iscale )

                            vx1 =
                                max 0 x1

                            gzt =
                                Int32.add thing.z lump.topoffset

                            vis =
                                { x1 = vx1
                                , x2 = min (vs.viewwidth - 1) x2
                                , gx = thing.x
                                , gy = thing.y
                                , gz = thing.z
                                , gzt = gzt
                                , startfrac =
                                    if vx1 > x1 then
                                        Int32.add startfrac0 (Int32.mul xiscale (vx1 - x1))

                                    else
                                        startfrac0
                                , scale = Int32.shiftLeft vs.detailshift xscale
                                , xiscale = xiscale
                                , texturemid = Int32.sub gzt ctx.viewz
                                , patch = lumpNum
                                , colormap =
                                    if Bitwise.and thing.flags Flags.mfShadow /= 0 then
                                        Nothing

                                    else
                                        case ctx.fixedcolormap of
                                            Just fixed ->
                                                Just fixed

                                            Nothing ->
                                                if Bitwise.and thing.frame 0x8000 /= 0 then
                                                    Just (colormap ctx.static 0)

                                                else
                                                    Just
                                                        (colormap ctx.static
                                                            (at (lightIndex Internal.maxLightScale (Int32.shiftRight (12 - vs.detailshift) xscale)) spritelights)
                                                        )
                                , mobjflags = thing.flags
                                }
                        in
                        { st | vissprites = vis :: st.vissprites, numVissprites = st.numVissprites + 1 }



-- MASKED


type alias MaskedPhase =
    { ops : Array (List Op)
    , fuzzpos : Int
    , drawsegs : Array Drawseg
    , cache : Cache
    }


{-| R\_DrawMasked: the vissprites from back to front, each with the masked walls
behind it; the rest of the masked walls; then the weapon.
-}
drawMasked : Ctx -> Player -> Mobj -> St -> Cache -> MaskedPhase
drawMasked ctx player mo st cache =
    let
        sorted =
            List.reverse st.vissprites
                |> List.indexedMap Tuple.pair
                |> List.sortBy (\( i, v ) -> ( v.scale, i ))
                |> List.map Tuple.second

        phase0 =
            { ops = Compose.emptyColumns.masked
            , fuzzpos = cache.fuzzpos
            , drawsegs = Array.fromList (List.reverse st.drawsegs)
            , cache = cache
            }

        phase1 =
            List.foldl (drawSprite ctx) phase0 sorted

        n =
            Array.length phase1.drawsegs

        phase2 =
            List.foldl
                (\i ph ->
                    case Array.get i ph.drawsegs of
                        Just ds ->
                            if ds.maskedtexturecol /= Nothing then
                                renderMaskedSegRange ctx i ds.x1 ds.x2 ph

                            else
                                ph

                        Nothing ->
                            ph
                )
                phase1
                (List.range 0 (n - 1) |> List.reverse)
    in
    drawPlayerSprites ctx player mo phase2


clipAt : Ctx -> Clip -> Int -> Int -> Int
clipAt ctx clip x1 x =
    case clip of
        NoClip ->
            0

        AllHeight ->
            ctx.vs.viewheight

        AllNegOne ->
            -1

        Clipped a ->
            at (x - x1) a


{-| R\_RenderMaskedSegRange: a two-sided wall's middle texture from x1 to x2, column
by column, each drawn only once.
-}
renderMaskedSegRange : Ctx -> Int -> Int -> Int -> MaskedPhase -> MaskedPhase
renderMaskedSegRange ctx dsIndex x1 x2 phase =
    case Array.get dsIndex phase.drawsegs of
        Nothing ->
            phase

        Just ds ->
            case ds.maskedtexturecol of
                Nothing ->
                    phase

                Just cols ->
                    let
                        seg =
                            ds.seg

                        front =
                            World.sector seg.frontsector ctx.world

                        back =
                            World.sector (Maybe.withDefault seg.frontsector seg.backsector) ctx.world

                        side =
                            World.sideState seg.sidedef ctx.world

                        texnum =
                            textureTranslation ctx side.midtexture

                        ( tex, cache1 ) =
                            getTexture ctx texnum phase.cache

                        lightnum0 =
                            Int32.shiftRight 4 front.lightlevel + ctx.extralight

                        lightnum =
                            if seg.v1.y == seg.v2.y then
                                lightnum0 - 1

                            else if seg.v1.x == seg.v2.x then
                                lightnum0 + 1

                            else
                                lightnum0

                        walllights =
                            lightRow ctx lightnum

                        line =
                            Level.line seg.linedef ctx.level

                        texturemid =
                            Int32.add side.rowoffset
                                (if Bitwise.and line.flags Flags.mlDontPegBottom /= 0 then
                                    Int32.sub (Int32.add (max front.floorheight back.floorheight) (textureHeight ctx texnum)) ctx.viewz

                                 else
                                    Int32.sub (min front.ceilingheight back.ceilingheight) ctx.viewz
                                )

                        go x spryscale ( colsAcc, ms ) =
                            if x > x2 then
                                ( colsAcc, ms )

                            else
                                let
                                    col =
                                        at (x - ds.x1) colsAcc
                                in
                                if col /= 32767 then
                                    let
                                        cmap =
                                            case ctx.fixedcolormap of
                                                Just fixed ->
                                                    fixed

                                                Nothing ->
                                                    colormap ctx.static (at (lightIndex Internal.maxLightScale (Int32.shiftRight 12 spryscale)) walllights)

                                        sprtopscreen =
                                            Int32.sub ctx.vs.centeryfrac (Fixed.mul texturemid spryscale)

                                        source =
                                            column tex col

                                        ms2 =
                                            drawMaskedColumn ctx
                                                (Base cmap)
                                                x
                                                source.mem
                                                (source.offset - 3)
                                                texturemid
                                                sprtopscreen
                                                spryscale
                                                (invScale spryscale)
                                                (clipAt ctx ds.sprbottomclip ds.x1 x)
                                                (clipAt ctx ds.sprtopclip ds.x1 x)
                                                ms
                                    in
                                    go (x + 1) (Int32.add spryscale ds.scalestep) ( Array.set (x - ds.x1) 32767 colsAcc, ms2 )

                                else
                                    go (x + 1) (Int32.add spryscale ds.scalestep) ( colsAcc, ms )

                        ( newCols, msDone ) =
                            go x1 (Int32.add ds.scale1 (Int32.mul (x1 - ds.x1) ds.scalestep)) ( cols, { ops = phase.ops, fuzzpos = phase.fuzzpos } )
                    in
                    { phase
                        | ops = msDone.ops
                        , fuzzpos = msDone.fuzzpos
                        , drawsegs = Array.set dsIndex { ds | maskedtexturecol = Just newCols } phase.drawsegs
                        , cache = cache1
                    }


{-| R\_DrawSprite: clip a vissprite against the walls in front of it (drawing the
masked walls behind it first), then draw it.
-}
drawSprite : Ctx -> Vissprite -> MaskedPhase -> MaskedPhase
drawSprite ctx spr phase0 =
    let
        width =
            spr.x2 - spr.x1 + 1

        fillClip r1 r2 clip dsx1 arr =
            List.foldl
                (\x a ->
                    if at (x - spr.x1) a == -2 then
                        Array.set (x - spr.x1) (clipAt ctx clip dsx1 x) a

                    else
                        a
                )
                arr
                (List.range r1 r2)

        step i ( bots, tops, phase ) =
            case Array.get i phase.drawsegs of
                Nothing ->
                    ( bots, tops, phase )

                Just ds ->
                    if ds.x1 > spr.x2 || ds.x2 < spr.x1 || (ds.silhouette == 0 && ds.maskedtexturecol == Nothing) then
                        ( bots, tops, phase )

                    else
                        let
                            r1 =
                                max ds.x1 spr.x1

                            r2 =
                                min ds.x2 spr.x2

                            ( lowscale, scale ) =
                                if ds.scale1 > ds.scale2 then
                                    ( ds.scale2, ds.scale1 )

                                else
                                    ( ds.scale1, ds.scale2 )
                        in
                        if scale < spr.scale || (lowscale < spr.scale && pointOnSegSide spr.gx spr.gy ds.seg == 0) then
                            if ds.maskedtexturecol /= Nothing then
                                ( bots, tops, renderMaskedSegRange ctx i r1 r2 phase )

                            else
                                ( bots, tops, phase )

                        else
                            let
                                sil1 =
                                    if spr.gz >= ds.bsilheight then
                                        Bitwise.and ds.silhouette (Bitwise.complement silBottom)

                                    else
                                        ds.silhouette

                                sil =
                                    if spr.gzt <= ds.tsilheight then
                                        Bitwise.and sil1 (Bitwise.complement silTop)

                                    else
                                        sil1
                            in
                            case sil of
                                1 ->
                                    ( fillClip r1 r2 ds.sprbottomclip ds.x1 bots, tops, phase )

                                2 ->
                                    ( bots, fillClip r1 r2 ds.sprtopclip ds.x1 tops, phase )

                                3 ->
                                    ( fillClip r1 r2 ds.sprbottomclip ds.x1 bots, fillClip r1 r2 ds.sprtopclip ds.x1 tops, phase )

                                _ ->
                                    ( bots, tops, phase )

        n =
            Array.length phase0.drawsegs

        ( bot, top, phase1 ) =
            List.foldl step ( Array.repeat width -2, Array.repeat width -2, phase0 ) (List.range 0 (n - 1) |> List.reverse)

        clipbot =
            Array.map
                (\c ->
                    if c == -2 then
                        ctx.vs.viewheight

                    else
                        c
                )
                bot

        cliptop =
            Array.map
                (\c ->
                    if c == -2 then
                        -1

                    else
                        c
                )
                top
    in
    drawVisSprite ctx spr (\x -> at (x - spr.x1) clipbot) (\x -> at (x - spr.x1) cliptop) phase1


{-| R\_PointOnSegSide.
-}
pointOnSegSide : Int -> Int -> Seg -> Int
pointOnSegSide x y seg =
    Geometry.pointOnSide x
        y
        { x = seg.v1.x
        , y = seg.v1.y
        , dx = Int32.sub seg.v2.x seg.v1.x
        , dy = Int32.sub seg.v2.y seg.v1.y
        , bbox = ( emptyBox, emptyBox )
        , children = ( ChildSubsector 0, ChildSubsector 0 )
        }


emptyBox : Level.Box
emptyBox =
    { top = 0, bottom = 0, left = 0, right = 0 }


{-| R\_DrawVisSprite: the sprite's columns, x1 to x2.
-}
drawVisSprite : Ctx -> Vissprite -> (Int -> Int) -> (Int -> Int) -> MaskedPhase -> MaskedPhase
drawVisSprite ctx vis floorclip ceilingclip phase =
    let
        lump =
            spriteLump ctx.static vis.patch

        ( mem, cache1 ) =
            Internal.lumpMem ctx.static lump.lump phase.cache

        colfunc =
            case vis.colormap of
                Nothing ->
                    Shadow

                Just cmap ->
                    if Bitwise.and vis.mobjflags Flags.mfTranslation /= 0 then
                        Translated cmap
                            (case Bitwise.shiftRightBy Flags.mfTransShift (Bitwise.and vis.mobjflags Flags.mfTranslation) of
                                1 ->
                                    0x60

                                2 ->
                                    0x40

                                _ ->
                                    0x20
                            )

                    else
                        Base cmap

        iscale =
            Int32.shiftRight ctx.vs.detailshift (Int32.abs vis.xiscale)

        sprtopscreen =
            Int32.sub ctx.vs.centeryfrac (Fixed.mul vis.texturemid vis.scale)

        go x frac ms =
            if x > vis.x2 then
                ms

            else
                let
                    texturecolumn =
                        Int32.shiftRight 16 frac

                    ofs =
                        Mem.u32 (8 + 4 * texturecolumn) mem
                in
                go (x + 1)
                    (Int32.add frac vis.xiscale)
                    (drawMaskedColumn ctx colfunc x mem ofs vis.texturemid sprtopscreen vis.scale iscale (floorclip x) (ceilingclip x) ms)

        done =
            go vis.x1 vis.startfrac { ops = phase.ops, fuzzpos = phase.fuzzpos }
    in
    { phase | ops = done.ops, fuzzpos = done.fuzzpos, cache = cache1 }


{-| R\_DrawPlayerSprites: the weapon and its flash, lit by the player's sector.
-}
drawPlayerSprites : Ctx -> Player -> Mobj -> MaskedPhase -> MaskedPhase
drawPlayerSprites ctx player mo phase =
    let
        sec =
            World.sector (World.mobjSector ctx.level mo) ctx.world

        spritelights =
            lightRow ctx (Int32.shiftRight 4 sec.lightlevel + ctx.extralight)
    in
    List.foldl (drawPSprite ctx player spritelights) phase [ player.weaponSprite, player.flashSprite ]


{-| R\_DrawPSprite.
-}
drawPSprite : Ctx -> Player -> Array Int -> PspDef -> MaskedPhase -> MaskedPhase
drawPSprite ctx player spritelights psp phase =
    case psp.state of
        Nothing ->
            phase

        Just state ->
            let
                def =
                    State.def state

                vs =
                    ctx.vs
            in
            case spriteFrame ctx.static (Sprite.index def.sprite) def.frame of
                Nothing ->
                    phase

                Just sprframe ->
                    let
                        lumpNum =
                            Array.get 0 sprframe.lumps |> Maybe.withDefault 0

                        flip =
                            Array.get 0 sprframe.flips |> Maybe.withDefault False

                        lump =
                            spriteLump ctx.static lumpNum

                        tx1 =
                            Int32.sub (Int32.sub psp.sx (160 * Fixed.fracunit)) lump.offset

                        x1 =
                            Int32.shiftRight 16 (Int32.add vs.centerxfrac (Fixed.mul tx1 vs.pspritescale))

                        tx2 =
                            Int32.add tx1 lump.width

                        x2 =
                            Int32.shiftRight 16 (Int32.add vs.centerxfrac (Fixed.mul tx2 vs.pspritescale)) - 1
                    in
                    if x1 > vs.viewwidth || x2 < 0 then
                        phase

                    else
                        let
                            ( startfrac0, xiscale ) =
                                if flip then
                                    ( lump.width - 1, Int32.neg vs.pspriteiscale )

                                else
                                    ( 0, vs.pspriteiscale )

                            vx1 =
                                max 0 x1

                            invisibility =
                                Player.power Invisibility player

                            vis =
                                { x1 = vx1
                                , x2 = min (vs.viewwidth - 1) x2
                                , gx = 0
                                , gy = 0
                                , gz = 0
                                , gzt = 0
                                , startfrac =
                                    if vx1 > x1 then
                                        Int32.add startfrac0 (Int32.mul xiscale (vx1 - x1))

                                    else
                                        startfrac0
                                , scale = Int32.shiftLeft vs.detailshift vs.pspritescale
                                , xiscale = xiscale
                                , texturemid =
                                    Int32.sub (100 * Fixed.fracunit + Fixed.fracunit // 2) (Int32.sub psp.sy lump.topoffset)
                                , patch = lumpNum
                                , colormap =
                                    if invisibility > 4 * 32 || Bitwise.and invisibility 8 /= 0 then
                                        Nothing

                                    else
                                        case ctx.fixedcolormap of
                                            Just fixed ->
                                                Just fixed

                                            Nothing ->
                                                if Bitwise.and def.frame 0x8000 /= 0 then
                                                    Just (colormap ctx.static 0)

                                                else
                                                    Just (colormap ctx.static (at (Internal.maxLightScale - 1) spritelights))
                                , mobjflags = 0
                                }
                        in
                        drawVisSprite ctx vis (\_ -> vs.viewheight) (\_ -> -1) phase
