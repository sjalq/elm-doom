module Doom.Level exposing
    ( Level, LineId, SectorId, SideId, SubsectorId
    , Vertex, Sector, Side, Line, SlopeType(..), Seg, Subsector, Node, Child(..), Box
    , boxTop, boxBottom, boxLeft, boxRight
    , Blockmap, MapThing
    , line, sector, side, seg, subsector, node, vertex
    , blockmapLines, rejected
    , mtfEasy, mtfNormal, mtfHard, mtfAmbush
    )

{-| A map as the WAD describes it (p\_setup.c, r\_defs.h): everything about it that
never changes while it is played. What does change (floor and ceiling heights, light,
line specials, textures switches flip) is in `Doom.World`, indexed the same way.

Things refer to each other by index (`LineId`, `SectorId`, ...), where the C source
uses pointers.

@docs Level, LineId, SectorId, SideId, SubsectorId
@docs Vertex, Sector, Side, Line, SlopeType, Seg, Subsector, Node, Child, Box
@docs boxTop, boxBottom, boxLeft, boxRight
@docs Blockmap, MapThing
@docs line, sector, side, seg, subsector, node, vertex
@docs blockmapLines, rejected
@docs mtfEasy, mtfNormal, mtfHard, mtfAmbush

-}

import Array exposing (Array)
import Bitwise
import Doom.Angle exposing (Angle)
import Doom.Fixed exposing (Fixed)


{-| An index into `Level.lines`.
-}
type alias LineId =
    Int


{-| An index into `Level.sectors` (and `World.sectors`).
-}
type alias SectorId =
    Int


{-| An index into `Level.sides` (and `World.sides`).
-}
type alias SideId =
    Int


{-| An index into `Level.subsectors`.
-}
type alias SubsectorId =
    Int


{-| A loaded map.

  - `things`: the map's THINGS lump, in order (P\_LoadThings spawns them from it).
  - `blockmap`, `reject`: the collision grid and the sector-to-sector sight table.

-}
type alias Level =
    { name : String
    , vertexes : Array Vertex
    , sectors : Array Sector
    , sides : Array Side
    , lines : Array Line
    , segs : Array Seg
    , subsectors : Array Subsector
    , nodes : Array Node
    , blockmap : Blockmap
    , reject : Array Int
    , things : List MapThing
    }


{-| vertex\_t: a point, fixed-point.
-}
type alias Vertex =
    { x : Fixed
    , y : Fixed
    }


{-| The parts of sector\_t that never change. `lines` are the lines with this sector on
either side, in the order P\_GroupLines found them. `soundorgX`/`soundorgY` is the
middle of `blockbox`, where sounds the sector makes come from. `blockbox` is in
blockmap cells.
-}
type alias Sector =
    { lines : Array LineId
    , blockbox : Box
    , soundorgX : Fixed
    , soundorgY : Fixed
    }


{-| The parts of side\_t that never change: which sector the side faces.
-}
type alias Side =
    { sector : SectorId
    }


{-| The parts of line\_t that never change. `frontSide`/`backSide` are sidenum[0] and
sidenum[1] (-1 for a one-sided line is `Nothing`), `frontsector`/`backsector` the
sectors those sides face. `flags` here are the map's; the ones that change
(ML\_MAPPED) are in `World.lines`.
-}
type alias Line =
    { v1 : Vertex
    , v2 : Vertex
    , dx : Fixed
    , dy : Fixed
    , flags : Int
    , tag : Int
    , frontSide : SideId
    , backSide : Maybe SideId
    , bbox : Box
    , slopetype : SlopeType
    , frontsector : SectorId
    , backsector : Maybe SectorId
    }


{-| slopetype\_t.
-}
type SlopeType
    = Horizontal
    | Vertical
    | Positive
    | Negative


{-| seg\_t: part of a line, as the BSP split it.
-}
type alias Seg =
    { v1 : Vertex
    , v2 : Vertex
    , offset : Fixed
    , angle : Angle
    , sidedef : SideId
    , linedef : LineId
    , side : Int
    , frontsector : SectorId
    , backsector : Maybe SectorId
    }


{-| subsector\_t: a convex piece of a sector, the BSP's leaf.
-}
type alias Subsector =
    { sector : SectorId
    , numlines : Int
    , firstline : Int
    }


{-| node\_t: a BSP split, `x`/`y`/`dx`/`dy` its partition line, `bbox` and `children`
for its right (0) and left (1) sides.
-}
type alias Node =
    { x : Fixed
    , y : Fixed
    , dx : Fixed
    , dy : Fixed
    , bbox : ( Box, Box )
    , children : ( Child, Child )
    }


{-| A node's child: another node, or (NF\_SUBSECTOR set) a subsector.
-}
type Child
    = ChildNode Int
    | ChildSubsector SubsectorId


{-| A bounding box, `[BOXTOP, BOXBOTTOM, BOXLEFT, BOXRIGHT]` in the C source.
-}
type alias Box =
    { top : Int
    , bottom : Int
    , left : Int
    , right : Int
    }


{-| `box[BOXTOP]`.
-}
boxTop : Box -> Int
boxTop =
    .top


{-| `box[BOXBOTTOM]`.
-}
boxBottom : Box -> Int
boxBottom =
    .bottom


{-| `box[BOXLEFT]`.
-}
boxLeft : Box -> Int
boxLeft =
    .left


{-| `box[BOXRIGHT]`.
-}
boxRight : Box -> Int
boxRight =
    .right


{-| The blockmap: the map cut into 128-unit cells, each with the lines that cross it.

`cells` holds, for each cell (x + y \* width), the lines exactly as the BLOCKMAP lump
lists them, including the line 0 that every list in the lump starts with, which the
original game also checks in every cell.

-}
type alias Blockmap =
    { orgx : Fixed
    , orgy : Fixed
    , width : Int
    , height : Int
    , cells : Array (List LineId)
    }


{-| mapthing\_t: a thing as the map places it. `angle` is in degrees.
-}
type alias MapThing =
    { x : Int
    , y : Int
    , angle : Int
    , type_ : Int
    , options : Int
    }


{-| MTF\_EASY: the thing appears on the easy skills.
-}
mtfEasy : Int
mtfEasy =
    1


{-| MTF\_NORMAL.
-}
mtfNormal : Int
mtfNormal =
    2


{-| MTF\_HARD.
-}
mtfHard : Int
mtfHard =
    4


{-| MTF\_AMBUSH: deaf.
-}
mtfAmbush : Int
mtfAmbush =
    8



-- LOOKUPS


{-| `lines[i]`. Out of range gives a harmless empty line, never a crash: the game only
asks for indexes the map itself contains.
-}
line : LineId -> Level -> Line
line i level =
    Array.get i level.lines |> Maybe.withDefault emptyLine


{-| `sectors[i]`.
-}
sector : SectorId -> Level -> Sector
sector i level =
    Array.get i level.sectors |> Maybe.withDefault emptySector


{-| `sides[i]`.
-}
side : SideId -> Level -> Side
side i level =
    Array.get i level.sides |> Maybe.withDefault { sector = 0 }


{-| `segs[i]`.
-}
seg : Int -> Level -> Seg
seg i level =
    Array.get i level.segs
        |> Maybe.withDefault
            { v1 = origin, v2 = origin, offset = 0, angle = 0, sidedef = 0, linedef = 0, side = 0, frontsector = 0, backsector = Nothing }


{-| `subsectors[i]`.
-}
subsector : SubsectorId -> Level -> Subsector
subsector i level =
    Array.get i level.subsectors |> Maybe.withDefault { sector = 0, numlines = 0, firstline = 0 }


{-| `nodes[i]`.
-}
node : Int -> Level -> Node
node i level =
    Array.get i level.nodes
        |> Maybe.withDefault
            { x = 0, y = 0, dx = 0, dy = 0, bbox = ( emptyBox, emptyBox ), children = ( ChildSubsector 0, ChildSubsector 0 ) }


{-| `vertexes[i]`.
-}
vertex : Int -> Level -> Vertex
vertex i level =
    Array.get i level.vertexes |> Maybe.withDefault origin


{-| The lines listed in blockmap cell (x, y), in the lump's order.
-}
blockmapLines : Int -> Int -> Blockmap -> List LineId
blockmapLines x y bm =
    Array.get (x + y * bm.width) bm.cells |> Maybe.withDefault []


{-| Whether the REJECT table says nothing in sector `s1` can see into sector `s2`
(`rejectmatrix[pnum>>3] & (1 << (pnum&7))` with pnum = s1 \* numsectors + s2).
-}
rejected : SectorId -> SectorId -> Level -> Bool
rejected s1 s2 level =
    let
        pnum =
            s1 * Array.length level.sectors + s2
    in
    case Array.get (pnum // 8) level.reject of
        Just byte ->
            Bitwise.and byte (Bitwise.shiftLeftBy (modBy 8 pnum) 1) /= 0

        Nothing ->
            False


origin : Vertex
origin =
    { x = 0, y = 0 }


emptyBox : Box
emptyBox =
    { top = 0, bottom = 0, left = 0, right = 0 }


emptyLine : Line
emptyLine =
    { v1 = origin
    , v2 = origin
    , dx = 0
    , dy = 0
    , flags = 0
    , tag = 0
    , frontSide = 0
    , backSide = Nothing
    , bbox = emptyBox
    , slopetype = Horizontal
    , frontsector = 0
    , backsector = Nothing
    }


emptySector : Sector
emptySector =
    { lines = Array.empty, blockbox = emptyBox, soundorgX = 0, soundorgY = 0 }
