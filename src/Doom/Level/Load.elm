module Doom.Level.Load exposing (Loaded, load, fromLumps)

{-| Loading a map (p\_setup.c): P\_LoadVertexes, P\_LoadSectors, P\_LoadSideDefs,
P\_LoadLineDefs, P\_LoadSubsectors, P\_LoadNodes, P\_LoadSegs, P\_LoadBlockMap, the
REJECT lump and P\_GroupLines, computing every derived value as the C source does.
Things are read but not spawned: that is `Doom.Play.Setup`.

    Load.load data "E1M1" wad
        |> Result.map (\loaded -> Array.length loaded.level.lines)

@docs Loaded, load, fromLumps

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Doom.Angle as Angle
import Doom.Data as Data exposing (Data)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Int32 as Int32
import Doom.Level as Level exposing (Blockmap, Box, Level, Line, MapThing, Node, Seg, Side, SlopeType(..), Subsector, Vertex)
import Doom.Wad as Wad exposing (MapLumps, Wad)
import Doom.World exposing (LineState, SectorState, SideState)


{-| A loaded map: what never changes (`level`) and the starting values of what does,
for `World.sectors`, `World.lines` and `World.sides`.
-}
type alias Loaded =
    { level : Level
    , sectors : Array SectorState
    , lines : Array LineState
    , sides : Array SideState
    }


{-| Load the map with this marker name (E1M1, MAP01...) from the WAD.
-}
load : Data -> String -> Wad -> Result String Loaded
load data name wad =
    case Wad.mapLumps name wad of
        Just lumps ->
            fromLumps data name lumps

        Nothing ->
            Err ("the WAD has no map " ++ name)


{-| Load a map from its lumps.
-}
fromLumps : Data -> String -> MapLumps -> Result String Loaded
fromLumps data name lumps =
    let
        decodeAll what size decoder bytes =
            Decode.decode (records (Bytes.width bytes // size) decoder) bytes
                |> Result.fromMaybe ("can't read the " ++ what ++ " of " ++ name)
    in
    Result.map5
        (\blockmap vertexes mapSectors mapSides mapLines ->
            Result.map4
                (\mapSubsectors nodes mapSegs things ->
                    build data
                        name
                        { blockmap = blockmap
                        , vertexes = vertexes
                        , sectors = mapSectors
                        , sides = mapSides
                        , lines = mapLines
                        , subsectors = mapSubsectors
                        , nodes = nodes
                        , segs = mapSegs
                        , things = things
                        , reject = bytesToArray lumps.reject
                        }
                )
                (decodeAll "subsectors" 4 subsectorDecoder lumps.ssectors)
                (decodeAll "nodes" 28 nodeDecoder lumps.nodes)
                (decodeAll "segs" 12 segDecoder lumps.segs)
                (decodeAll "things" 10 thingDecoder lumps.things)
        )
        (readBlockmap lumps.blockmap |> Result.fromMaybe ("can't read the blockmap of " ++ name))
        (decodeAll "vertexes" 4 vertexDecoder lumps.vertexes)
        (decodeAll "sectors" 26 sectorDecoder lumps.sectors)
        (decodeAll "sidedefs" 30 sideDecoder lumps.sidedefs)
        (decodeAll "linedefs" 14 lineDecoder lumps.linedefs)
        |> Result.andThen identity



-- THE LUMPS' RECORDS


type alias MapSector =
    { floorheight : Int
    , ceilingheight : Int
    , floorpic : String
    , ceilingpic : String
    , lightlevel : Int
    , special : Int
    , tag : Int
    }


type alias MapSide =
    { textureoffset : Int
    , rowoffset : Int
    , toptexture : String
    , bottomtexture : String
    , midtexture : String
    , sector : Int
    }


type alias MapLine =
    { v1 : Int
    , v2 : Int
    , flags : Int
    , special : Int
    , tag : Int
    , sidenum0 : Int
    , sidenum1 : Int
    }


type alias MapSeg =
    { v1 : Int
    , v2 : Int
    , angle : Int
    , linedef : Int
    , side : Int
    , offset : Int
    }


type alias Lumps =
    { blockmap : Blockmap
    , vertexes : List Vertex
    , sectors : List MapSector
    , sides : List MapSide
    , lines : List MapLine
    , subsectors : List ( Int, Int )
    , nodes : List Node
    , segs : List MapSeg
    , things : List MapThing
    , reject : Array Int
    }


short : Decoder Int
short =
    Decode.signedInt16 LE


{-| An index stored in a short: read unsigned, so that maps with more than 32767 of
something still load (the C source would read it as negative and fail).
-}
index : Decoder Int
index =
    Decode.unsignedInt16 LE


vertexDecoder : Decoder Vertex
vertexDecoder =
    Decode.map2 (\x y -> { x = Fixed.fromInt x, y = Fixed.fromInt y }) short short


sectorDecoder : Decoder MapSector
sectorDecoder =
    Decode.succeed MapSector |> keep short |> keep short |> keep Wad.name8 |> keep Wad.name8 |> keep short |> keep short |> keep short


sideDecoder : Decoder MapSide
sideDecoder =
    Decode.succeed MapSide |> keep short |> keep short |> keep Wad.name8 |> keep Wad.name8 |> keep Wad.name8 |> keep index


lineDecoder : Decoder MapLine
lineDecoder =
    Decode.succeed MapLine |> keep index |> keep index |> keep short |> keep short |> keep short |> keep short |> keep short


subsectorDecoder : Decoder ( Int, Int )
subsectorDecoder =
    Decode.map2 Tuple.pair short short


segDecoder : Decoder MapSeg
segDecoder =
    Decode.succeed MapSeg |> keep index |> keep index |> keep short |> keep index |> keep short |> keep short


thingDecoder : Decoder MapThing
thingDecoder =
    Decode.map5 MapThing short short short short short


{-| mapnode\_t: the partition line, the two children's boxes (top, bottom, left,
right), and the children (unsigned shorts, NF\_SUBSECTOR marking a subsector).
-}
nodeDecoder : Decoder Node
nodeDecoder =
    let
        box =
            Decode.map4 (\t b l r -> { top = Fixed.fromInt t, bottom = Fixed.fromInt b, left = Fixed.fromInt l, right = Fixed.fromInt r })
                short
                short
                short
                short

        child =
            Decode.unsignedInt16 LE
                |> Decode.map
                    (\c ->
                        if Bitwise.and c 0x8000 /= 0 then
                            Level.ChildSubsector (Bitwise.and c 0x7FFF)

                        else
                            Level.ChildNode c
                    )
    in
    Decode.map4 (\x y dx dy -> { x = Fixed.fromInt x, y = Fixed.fromInt y, dx = Fixed.fromInt dx, dy = Fixed.fromInt dy })
        short
        short
        short
        short
        |> Decode.andThen
            (\partition ->
                Decode.map4
                    (\b0 b1 c0 c1 ->
                        { x = partition.x
                        , y = partition.y
                        , dx = partition.dx
                        , dy = partition.dy
                        , bbox = ( b0, b1 )
                        , children = ( c0, c1 )
                        }
                    )
                    box
                    box
                    child
                    child
            )


keep : Decoder a -> Decoder (a -> b) -> Decoder b
keep value function =
    Decode.map2 (\f v -> f v) function value


records : Int -> Decoder a -> Decoder (List a)
records n item =
    Decode.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\x -> Decode.Loop ( left - 1, x :: acc )) item
        )


bytesToArray : Bytes -> Array Int
bytesToArray bytes =
    Decode.decode (records (Bytes.width bytes) Decode.unsignedInt8) bytes
        |> Maybe.withDefault []
        |> Array.fromList


{-| P\_LoadBlockMap: the lump as signed shorts: origin x, origin y, width, height, then
an offset per cell to its list of lines, each list ending with -1. Offsets are read
unsigned (see `index`).
-}
readBlockmap : Bytes -> Maybe Blockmap
readBlockmap bytes =
    Decode.decode (records (Bytes.width bytes // 2) short) bytes
        |> Maybe.andThen
            (\shortList ->
                let
                    lump =
                        Array.fromList shortList

                    at i =
                        Array.get i lump
                in
                Maybe.map4
                    (\orgx orgy width height ->
                        { orgx = Fixed.fromInt orgx
                        , orgy = Fixed.fromInt orgy
                        , width = width
                        , height = height
                        , cells =
                            Array.initialize (max 0 (width * height))
                                (\cell ->
                                    case at (4 + cell) of
                                        Just offset ->
                                            cellLines lump (Int32.toUnsignedShort offset) []

                                        Nothing ->
                                            []
                                )
                        }
                    )
                    (at 0)
                    (at 1)
                    (at 2)
                    (at 3)
            )


cellLines : Array Int -> Int -> List Int -> List Int
cellLines lump i acc =
    case Array.get i lump of
        Just l ->
            if l == -1 then
                List.reverse acc

            else
                cellLines lump (i + 1) (l :: acc)

        Nothing ->
            List.reverse acc



-- BUILDING THE LEVEL


build : Data -> String -> Lumps -> Loaded
build data name lumps =
    let
        vertexes =
            Array.fromList lumps.vertexes

        vertexAt i =
            Array.get i vertexes |> Maybe.withDefault { x = 0, y = 0 }

        -- P_LoadSideDefs
        sides : Array Side
        sides =
            lumps.sides |> List.map (\s -> { sector = s.sector }) |> Array.fromList

        sideSector i =
            Array.get i sides |> Maybe.map .sector

        -- P_LoadLineDefs
        lines : Array Line
        lines =
            lumps.lines
                |> List.map
                    (\ml ->
                        let
                            v1 =
                                vertexAt ml.v1

                            v2 =
                                vertexAt ml.v2

                            dx =
                                Int32.sub v2.x v1.x

                            dy =
                                Int32.sub v2.y v1.y
                        in
                        { v1 = v1
                        , v2 = v2
                        , dx = dx
                        , dy = dy
                        , flags = ml.flags
                        , tag = ml.tag
                        , frontSide = ml.sidenum0
                        , backSide =
                            if ml.sidenum1 == -1 then
                                Nothing

                            else
                                Just ml.sidenum1
                        , bbox = lineBox v1 v2
                        , slopetype = slopeType dx dy
                        , frontsector =
                            if ml.sidenum0 == -1 then
                                0

                            else
                                sideSector ml.sidenum0 |> Maybe.withDefault 0
                        , backsector =
                            if ml.sidenum1 == -1 then
                                Nothing

                            else
                                sideSector ml.sidenum1
                        }
                    )
                |> Array.fromList

        lineAt i =
            Array.get i lines

        -- P_LoadSegs
        segs : Array Seg
        segs =
            lumps.segs
                |> List.map
                    (\ms ->
                        let
                            ldef =
                                lineAt ms.linedef

                            sidenum s =
                                case ( ldef, s ) of
                                    ( Just l, 0 ) ->
                                        l.frontSide

                                    ( Just l, _ ) ->
                                        l.backSide |> Maybe.withDefault -1

                                    ( Nothing, _ ) ->
                                        -1

                            sidedef =
                                sidenum ms.side

                            twoSided =
                                ldef |> Maybe.map (\l -> Flags.has Flags.mlTwoSided l.flags) |> Maybe.withDefault False
                        in
                        { v1 = vertexAt ms.v1
                        , v2 = vertexAt ms.v2
                        , offset = Int32.shiftLeft 16 ms.offset
                        , angle = Angle.wrap (Int32.shiftLeft 16 ms.angle)
                        , sidedef = sidedef
                        , linedef = ms.linedef
                        , side = ms.side
                        , frontsector = sideSector sidedef |> Maybe.withDefault 0
                        , backsector =
                            if twoSided then
                                sideSector (sidenum (Bitwise.xor ms.side 1))

                            else
                                Nothing
                        }
                    )
                |> Array.fromList

        -- P_LoadSubsectors, and P_GroupLines' sector for each subsector
        subsectors : Array Subsector
        subsectors =
            lumps.subsectors
                |> List.map
                    (\( numsegs, firstseg ) ->
                        { sector =
                            Array.get firstseg segs
                                |> Maybe.andThen (\s -> sideSector s.sidedef)
                                |> Maybe.withDefault 0
                        , numlines = numsegs
                        , firstline = firstseg
                        }
                    )
                |> Array.fromList

        numsectors =
            List.length lumps.sectors

        bm =
            lumps.blockmap
    in
    { level =
        { name = name
        , vertexes = vertexes
        , sectors = groupLines numsectors bm lines
        , sides = sides
        , lines = lines
        , segs = segs
        , subsectors = subsectors
        , nodes = Array.fromList lumps.nodes
        , blockmap = bm
        , reject = padReject numsectors lines lumps.reject
        , things = lumps.things
        }
    , sectors =
        lumps.sectors
            |> List.map
                (\ms ->
                    { floorheight = Fixed.fromInt ms.floorheight
                    , ceilingheight = Fixed.fromInt ms.ceilingheight
                    , floorpic = Data.flatNumForName ms.floorpic data
                    , ceilingpic = Data.flatNumForName ms.ceilingpic data
                    , lightlevel = ms.lightlevel
                    , special = ms.special
                    , tag = ms.tag
                    , soundtraversed = 0
                    , soundtarget = Nothing
                    , validcount = 0
                    , specialdata = Nothing
                    }
                )
            |> Array.fromList
    , lines =
        lumps.lines
            |> List.map (\ml -> { flags = ml.flags, special = ml.special, validcount = 0 })
            |> Array.fromList
    , sides =
        lumps.sides
            |> List.map
                (\ms ->
                    { textureoffset = Fixed.fromInt ms.textureoffset
                    , rowoffset = Fixed.fromInt ms.rowoffset
                    , toptexture = Data.textureNumForName ms.toptexture data
                    , bottomtexture = Data.textureNumForName ms.bottomtexture data
                    , midtexture = Data.textureNumForName ms.midtexture data
                    }
                )
            |> Array.fromList
    }


lineBox : Vertex -> Vertex -> Box
lineBox v1 v2 =
    { left = min v1.x v2.x
    , right = max v1.x v2.x
    , bottom = min v1.y v2.y
    , top = max v1.y v2.y
    }


slopeType : Fixed -> Fixed -> SlopeType
slopeType dx dy =
    if dx == 0 then
        Vertical

    else if dy == 0 then
        Horizontal

    else if Fixed.div dy dx > 0 then
        Positive

    else
        Negative


{-| P\_LoadReject as the DOS executable behaves (chocolate-doom p\_setup.c:P\_LoadReject,
p\_rejectpad.c:PadRejectArray): a REJECT lump shorter than numsectors² bits is read
past its end, into the zone memory after it, which in DOS starts with the next block's
header: its size (`((totallines * 4 + 3) & ~3) + 24`, totallines being P\_GroupLines'
count of sector-line pairs), 0, PU\_LEVEL (50) and the zone id 0x1d4a11, as 32-bit
little-endian words; anything after those 16 bytes is 0.
-}
padReject : Int -> Array Line -> Array Int -> Array Int
padReject numsectors lines reject =
    let
        minlength =
            (numsectors * numsectors + 7) // 8

        missing =
            minlength - Array.length reject

        totallines =
            Array.foldl
                (\l n ->
                    case l.backsector of
                        Just b ->
                            if b /= l.frontsector then
                                n + 2

                            else
                                n + 1

                        Nothing ->
                            n + 1
                )
                0
                lines

        word w =
            List.map (\k -> Bitwise.and 0xFF (Bitwise.shiftRightZfBy (8 * k) w)) [ 0, 1, 2, 3 ]

        pad =
            List.concatMap word [ Bitwise.and (totallines * 4 + 3) (Bitwise.complement 3) + 24, 0, 50, 0x001D4A11 ]
                ++ List.repeat (max 0 (missing - 16)) 0
                |> List.take missing
    in
    if missing <= 0 then
        reject

    else
        Array.append reject (Array.fromList pad)


{-| P\_GroupLines' sector part: each sector's lines (those with it on either side, in
line order), the middle of their bounding box (where the sector's sounds come from),
and that box in blockmap cells, grown by MAXRADIUS and clamped to the blockmap.
-}
groupLines : Int -> Blockmap -> Array Line -> Array Level.Sector
groupLines numsectors bm lines =
    let
        add s i acc =
            case Array.get s acc of
                Just ls ->
                    Array.set s (i :: ls) acc

                Nothing ->
                    acc

        sectorLines =
            Array.foldr
                (\l ( i, acc ) ->
                    let
                        withFront =
                            add l.frontsector i acc

                        withBoth =
                            case l.backsector of
                                Just b ->
                                    if b /= l.frontsector then
                                        add b i withFront

                                    else
                                        withFront

                                Nothing ->
                                    withFront
                    in
                    ( i - 1, withBoth )
                )
                ( Array.length lines - 1, Array.repeat numsectors [] )
                lines
                |> Tuple.second
    in
    Array.map
        (\ids ->
            let
                bbox =
                    List.foldl
                        (\i box ->
                            case Array.get i lines of
                                Just l ->
                                    box |> addToBox l.v1.x l.v1.y |> addToBox l.v2.x l.v2.y

                                Nothing ->
                                    box
                        )
                        { top = Fixed.minInt, right = Fixed.minInt, bottom = Fixed.maxInt, left = Fixed.maxInt }
                        ids

                cell a =
                    Int32.shiftRight Defs.mapBlockShift a
            in
            { lines = Array.fromList ids
            , soundorgX = Int32.add bbox.right bbox.left // 2
            , soundorgY = Int32.add bbox.top bbox.bottom // 2
            , blockbox =
                { top = min (bm.height - 1) (cell (Int32.add (Int32.sub bbox.top bm.orgy) Defs.maxRadius))
                , bottom = max 0 (cell (Int32.sub (Int32.sub bbox.bottom bm.orgy) Defs.maxRadius))
                , right = min (bm.width - 1) (cell (Int32.add (Int32.sub bbox.right bm.orgx) Defs.maxRadius))
                , left = max 0 (cell (Int32.sub (Int32.sub bbox.left bm.orgx) Defs.maxRadius))
                }
            }
        )
        sectorLines


{-| M\_AddToBox (note the `else`s: a point only moves one side of each pair).
-}
addToBox : Fixed -> Fixed -> Box -> Box
addToBox x y box =
    let
        withX =
            if x < box.left then
                { box | left = x }

            else if x > box.right then
                { box | right = x }

            else
                box
    in
    if y < withX.bottom then
        { withX | bottom = y }

    else if y > withX.top then
        { withX | top = y }

    else
        withX
