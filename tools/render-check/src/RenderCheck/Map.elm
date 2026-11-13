module RenderCheck.Map exposing (Loaded, floorAt, load, skySpots)

{-| A minimal map loader, private to this tool (the game's is P\_SetupLevel): enough
of `Doom.Level` and `Doom.World` to draw a map from its player 1 start, with every
thing standing in its spawn state.
-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Dict
import Doom.Angle as Angle
import Doom.Data as Data exposing (Data)
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State
import Doom.Level as Level exposing (Child(..), Level, MapThing)
import Doom.Mobj exposing (Mobj)
import Doom.Player as Player
import Doom.Random as Random
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)
import RenderCheck.Wad as Wad exposing (Wad)


type alias Loaded =
    { level : Level
    , world : World
    }


s16 : Decoder Int
s16 =
    Decode.signedInt16 LE


u16 : Decoder Int
u16 =
    Decode.unsignedInt16 LE


name8 : Decoder String
name8 =
    Decode.string 8
        |> Decode.map (String.toList >> List.filter (\c -> c /= '\u{0000}') >> String.fromList >> String.toUpper)


records : Int -> Decoder a -> Maybe Bytes -> Array a
records size item bytes =
    case bytes of
        Just b ->
            let
                n =
                    Bytes.width b // size
            in
            Decode.decode
                (Decode.loop ( n, [] )
                    (\( left, acc ) ->
                        if left <= 0 then
                            Decode.succeed (Decode.Done (Array.fromList (List.reverse acc)))

                        else
                            Decode.map (\x -> Decode.Loop ( left - 1, x :: acc )) item
                    )
                )
                b
                |> Maybe.withDefault Array.empty

        Nothing ->
            Array.empty


map5 : (a -> b -> c -> d -> e -> r) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder r
map5 =
    Decode.map5


{-| Load map `name` (E1M1...).
-}
load : Wad -> Data -> String -> Maybe Loaded
load wad data mapName =
    let
        lumps =
            Wad.afterMarker wad mapName 10
                |> List.map (\l -> ( l.name, l.bytes ))
                |> Dict.fromList

        lump n =
            Dict.get n lumps

        vertexes =
            records 4 (Decode.map2 (\x y -> { x = Fixed.fromInt x, y = Fixed.fromInt y }) s16 s16) (lump "VERTEXES")

        rawSides =
            records 30
                (Decode.map2 (\a b -> a b)
                    (map5 (\xo yo top bot mid -> \sec -> { textureoffset = xo, rowoffset = yo, top = top, bottom = bot, mid = mid, sector = sec })
                        s16
                        s16
                        name8
                        name8
                        name8
                    )
                    s16
                )
                (lump "SIDEDEFS")

        rawSectors =
            records 26
                (Decode.map2 (\a b -> a b)
                    (map5 (\f c fp cp l -> \( sp, tag ) -> { floor = f, ceiling = c, floorpic = fp, ceilingpic = cp, light = l, special = sp, tag = tag })
                        s16
                        s16
                        name8
                        name8
                        s16
                    )
                    (Decode.map2 Tuple.pair s16 s16)
                )
                (lump "SECTORS")

        sideSector i =
            Array.get i rawSides |> Maybe.map .sector

        vertex i =
            Array.get i vertexes |> Maybe.withDefault { x = 0, y = 0 }

        rawLines =
            records 14
                (Decode.map2 (\a b -> a b)
                    (map5 (\v1 v2 fl sp tag -> \( s0, s1 ) -> { v1 = v1, v2 = v2, flags = fl, special = sp, tag = tag, s0 = s0, s1 = s1 })
                        u16
                        u16
                        s16
                        s16
                        s16
                    )
                    (Decode.map2 Tuple.pair u16 u16)
                )
                (lump "LINEDEFS")

        lines =
            Array.map
                (\l ->
                    let
                        a =
                            vertex l.v1

                        b =
                            vertex l.v2

                        back =
                            if l.s1 == 0xFFFF then
                                Nothing

                            else
                                Just l.s1
                    in
                    { v1 = a
                    , v2 = b
                    , dx = b.x - a.x
                    , dy = b.y - a.y
                    , flags = l.flags
                    , tag = l.tag
                    , frontSide = l.s0
                    , backSide = back
                    , bbox =
                        { top = max a.y b.y, bottom = min a.y b.y, left = min a.x b.x, right = max a.x b.x }
                    , slopetype =
                        if a.x == b.x then
                            Level.Vertical

                        else if a.y == b.y then
                            Level.Horizontal

                        else if (b.y - a.y > 0) == (b.x - a.x > 0) then
                            Level.Positive

                        else
                            Level.Negative
                    , frontsector = sideSector l.s0 |> Maybe.withDefault 0
                    , backsector = back |> Maybe.andThen sideSector
                    }
                )
                rawLines

        segs =
            records 12
                (Decode.map2 (\a b -> a b)
                    (map5 (\v1 v2 ang ld sd -> \ofs -> { v1 = v1, v2 = v2, angle = ang, linedef = ld, side = sd, offset = ofs })
                        u16
                        u16
                        u16
                        u16
                        s16
                    )
                    s16
                )
                (lump "SEGS")
                |> Array.map
                    (\s ->
                        let
                            line =
                                Array.get s.linedef rawLines |> Maybe.withDefault { v1 = 0, v2 = 0, flags = 0, special = 0, tag = 0, s0 = 0, s1 = 0xFFFF }

                            ( mine, other ) =
                                if s.side == 0 then
                                    ( line.s0, line.s1 )

                                else
                                    ( line.s1, line.s0 )

                            twoSided =
                                Bitwise.and line.flags Flags.mlTwoSided /= 0
                        in
                        { v1 = vertex s.v1
                        , v2 = vertex s.v2
                        , offset = Fixed.fromInt s.offset
                        , angle = Angle.wrap (s.angle * 65536)
                        , sidedef = mine
                        , linedef = s.linedef
                        , side = s.side
                        , frontsector = sideSector mine |> Maybe.withDefault 0
                        , backsector =
                            if twoSided then
                                sideSector other

                            else
                                Nothing
                        }
                    )

        subsectors =
            records 4 (Decode.map2 (\n first -> { numlines = n, firstline = first }) u16 u16) (lump "SSECTORS")
                |> Array.map
                    (\ss ->
                        { sector = Array.get ss.firstline segs |> Maybe.map .frontsector |> Maybe.withDefault 0
                        , numlines = ss.numlines
                        , firstline = ss.firstline
                        }
                    )

        box =
            Decode.map4 (\t b l r -> { top = Fixed.fromInt t, bottom = Fixed.fromInt b, left = Fixed.fromInt l, right = Fixed.fromInt r }) s16 s16 s16 s16

        child c =
            if Bitwise.and c 0x8000 /= 0 then
                ChildSubsector (Bitwise.and c 0x7FFF)

            else
                ChildNode c

        nodes =
            records 28
                (Decode.map4 (\( x, y ) ( dx, dy ) bb ( c0, c1 ) -> { x = Fixed.fromInt x, y = Fixed.fromInt y, dx = Fixed.fromInt dx, dy = Fixed.fromInt dy, bbox = bb, children = ( child c0, child c1 ) })
                    (Decode.map2 Tuple.pair s16 s16)
                    (Decode.map2 Tuple.pair s16 s16)
                    (Decode.map2 Tuple.pair box box)
                    (Decode.map2 Tuple.pair u16 u16)
                )
                (lump "NODES")

        things =
            records 10 (map5 MapThing s16 s16 s16 s16 s16) (lump "THINGS") |> Array.toList

        level =
            { name = mapName
            , vertexes = vertexes
            , sectors = Array.map (\_ -> { lines = Array.empty, blockbox = { top = 0, bottom = 0, left = 0, right = 0 }, soundorgX = 0, soundorgY = 0 }) rawSectors
            , sides = Array.map (\s -> { sector = s.sector }) rawSides
            , lines = lines
            , segs = segs
            , subsectors = subsectors
            , nodes = nodes
            , blockmap = { orgx = 0, orgy = 0, width = 0, height = 0, cells = Array.empty }
            , reject = Array.empty
            , things = things
            }

        sectors =
            Array.map
                (\s ->
                    { floorheight = Fixed.fromInt s.floor
                    , ceilingheight = Fixed.fromInt s.ceiling
                    , floorpic = Data.flatNumForName s.floorpic data
                    , ceilingpic = Data.flatNumForName s.ceilingpic data
                    , lightlevel = s.light
                    , special = s.special
                    , tag = s.tag
                    , soundtraversed = 0
                    , soundtarget = Nothing
                    , validcount = 0
                    , specialdata = Nothing
                    }
                )
                rawSectors

        sides =
            Array.map
                (\s ->
                    { textureoffset = Fixed.fromInt s.textureoffset
                    , rowoffset = Fixed.fromInt s.rowoffset
                    , toptexture = Data.textureNumForName s.top data
                    , bottomtexture = Data.textureNumForName s.bottom data
                    , midtexture = Data.textureNumForName s.mid data
                    }
                )
                rawSides
    in
    if Array.isEmpty vertexes then
        Nothing

    else
        Just { level = level, world = spawnAll level sectors sides things }


{-| Which subsector a point is in (R\_PointInSubsector, by the sign of the cross
product).
-}
pointInSubsector : Int -> Int -> Level -> Int
pointInSubsector x y level =
    let
        go n =
            let
                node =
                    Level.node n level

                dx =
                    toFloat (x - node.x)

                dy =
                    toFloat (y - node.y)

                left =
                    toFloat node.dy * dx

                right =
                    dy * toFloat node.dx

                ( front, back ) =
                    node.children
            in
            case
                if right < left then
                    front

                else
                    back
            of
                ChildNode m ->
                    go m

                ChildSubsector s ->
                    s
    in
    if Array.isEmpty level.nodes then
        0

    else
        go (Array.length level.nodes - 1)


{-| Every thing on Ultra-Violence, single player, standing in its spawn state;
player 1 at its start.
-}
spawnAll : Level -> Array World.SectorState -> Array World.SideState -> List MapThing -> World
spawnAll level sectors sides things =
    let
        spawnable =
            things
                |> List.filter (\t -> t.type_ == 1 || (Bitwise.and t.options Level.mtfHard /= 0 && Bitwise.and t.options 16 == 0))
                |> List.filterMap (\t -> typeFor t.type_ |> Maybe.map (\ty -> ( t, ty )))

        mobjs =
            List.indexedMap (\i ( t, ty ) -> spawn level sectors (i + 1) t ty) spawnable

        playerMo =
            List.filter (\m -> m.type_ == MobjType.Player) mobjs |> List.head

        sectorThings =
            List.foldl
                (\m acc ->
                    let
                        s =
                            (Level.subsector m.subsector level).sector
                    in
                    Array.set s (m.id :: (Array.get s acc |> Maybe.withDefault [])) acc
                )
                (Array.repeat (Array.length sectors) [])
                mobjs

        player =
            Player.new
                |> (\p ->
                        { p
                            | mo = Maybe.map .id playerMo
                            , viewz = Maybe.map (\m -> m.z + Fixed.fromInt 41) playerMo |> Maybe.withDefault 0
                            , viewheight = Fixed.fromInt 41
                            , health = 100
                            , weaponSprite = { state = Just State.Pistol, tics = 1, sx = Fixed.fromInt 1, sy = Fixed.fromInt 32 }
                        }
                   )
    in
    { leveltime = 0
    , rng = Random.init
    , validcount = 0
    , thinkers = mobjs |> List.map (\m -> ( m.id, MobjThinker )) |> Dict.fromList
    , mobjs = mobjs |> List.map (\m -> ( m.id, m )) |> Dict.fromList
    , nextThinkerId = List.length mobjs + 1
    , sectors = sectors
    , lines = Array.map (\l -> { flags = l.flags, special = 0, validcount = 0 }) level.lines
    , sides = sides
    , blocklinks = Array.empty
    , sectorThings = sectorThings
    , players = Array.fromList [ player ]
    , playeringame = Array.fromList [ True ]
    , playerStarts = Array.empty
    , deathmatchStarts = []
    , activePlats = Array.empty
    , activeCeilings = Array.empty
    , buttons = Array.empty
    , itemRespawn = { things = Array.empty, times = Array.empty, head = 0, tail = 0 }
    , brainTargets = Array.empty
    , brainTargetOn = 0
    , brainEasy = False
    , lineSpecialList = []
    , totalkills = 0
    , totalitems = 0
    , totalsecret = 0
    , exit = Nothing
    , sounds = []
    , scratch = World.emptyScratch
    }


typeFor : Int -> Maybe MobjType
typeFor doomednum =
    if doomednum == 1 then
        Just MobjType.Player

    else
        MobjType.all
            |> Array.toList
            |> List.filter (\t -> (MobjType.info t).doomednum == doomednum)
            |> List.head


spawn : Level -> Array World.SectorState -> Int -> MapThing -> MobjType -> Mobj
spawn level sectors id t ty =
    let
        info =
            MobjType.info ty

        x =
            Fixed.fromInt t.x

        y =
            Fixed.fromInt t.y

        ss =
            pointInSubsector x y level

        sector =
            Array.get (Level.subsector ss level).sector sectors

        floorz =
            sector |> Maybe.map .floorheight |> Maybe.withDefault 0

        ceilingz =
            sector |> Maybe.map .ceilingheight |> Maybe.withDefault 0

        def =
            State.def info.spawnstate
    in
    { id = id
    , x = x
    , y = y
    , z =
        if Bitwise.and info.flags Flags.mfSpawnCeiling /= 0 then
            ceilingz - info.height

        else
            floorz
    , angle = Angle.wrap (Angle.ang45 * (t.angle // 45))
    , sprite = def.sprite
    , frame = def.frame
    , subsector = ss
    , floorz = floorz
    , ceilingz = ceilingz
    , radius = info.radius
    , height = info.height
    , momx = 0
    , momy = 0
    , momz = 0
    , validcount = 0
    , type_ = ty
    , tics = def.tics
    , state = info.spawnstate
    , flags = info.flags
    , health = info.spawnhealth
    , movedir = 0
    , movecount = 0
    , target = Nothing
    , reactiontime = info.reactiontime
    , threshold = 0
    , player =
        if ty == MobjType.Player then
            Just 0

        else
            Nothing
    , lastlook = 0
    , spawnpoint = t
    , tracer = Nothing
    }


{-| The floor height at (x, y).
-}
floorAt : Loaded -> Int -> Int -> Int
floorAt loaded x y =
    (World.sector (Level.subsector (pointInSubsector x y loaded.level) loaded.level).sector loaded.world).floorheight


{-| Sectors open to the sky, each with the middle of one of its lines (map units), to
pick viewpoints from.
-}
skySpots : Data -> Loaded -> List ( Int, ( Int, Int ) )
skySpots data loaded =
    loaded.level.lines
        |> Array.toList
        |> List.filterMap
            (\l ->
                if (World.sector l.frontsector loaded.world).ceilingpic == data.skyFlatNum then
                    Just ( l.frontsector, ( (l.v1.x + l.v2.x) // 131072, (l.v1.y + l.v2.y) // 131072 ) )

                else
                    Nothing
            )
        |> Dict.fromList
        |> Dict.toList
