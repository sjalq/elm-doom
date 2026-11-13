module Doom.Play.MapUtil exposing (Intercept, InterceptTarget(..), ptAddLines, ptAddThings, ptEarlyOut, aproxDistance, pointOnLineSide, boxOnLineSide, pointOnDivlineSide, makeDivline, interceptVector, lineOpening, unsetThingPosition, setThingPosition, blockLinesIterator, blockThingsIterator, pathTraverse)

{-| Movement and collision helpers (p\_maputl.c): distances, which side of a line a point
or box is on, line openings, linking things into the blockmap and their sector, and the
blockmap iterators and path traversal the game's collision and hitscan code is built
on.

The C source's iterators take a function pointer and stop when it returns false; here
the function also gets and gives back the `World`.

@docs Intercept, InterceptTarget, ptAddLines, ptAddThings, ptEarlyOut, aproxDistance, pointOnLineSide, boxOnLineSide, pointOnDivlineSide, makeDivline, interceptVector, lineOpening, unsetThingPosition, setThingPosition, blockLinesIterator, blockThingsIterator, pathTraverse

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (Divline, World)


{-| intercept\_t: something a traced path crosses, `frac` of the way along it.
-}
type alias Intercept =
    { frac : Fixed
    , target : InterceptTarget
    }


{-| A line or a thing.
-}
type InterceptTarget
    = InterceptLine LineId
    | InterceptThing ThinkerId


{-| PT\_ADDLINES.
-}
ptAddLines : Int
ptAddLines =
    1


{-| PT\_ADDTHINGS.
-}
ptAddThings : Int
ptAddThings =
    2


{-| PT\_EARLYOUT.
-}
ptEarlyOut : Int
ptEarlyOut =
    4


{-| P\_AproxDistance dx dy.
-}
aproxDistance : Fixed -> Fixed -> Fixed
aproxDistance =
    Debug.todo "Doom.Play.MapUtil.aproxDistance"


{-| P\_PointOnLineSide: 0 front, 1 back.
-}
pointOnLineSide : Fixed -> Fixed -> Level.Line -> Int
pointOnLineSide =
    Debug.todo "Doom.Play.MapUtil.pointOnLineSide"


{-| P\_BoxOnLineSide: 0 front, 1 back, -1 crossing.
-}
boxOnLineSide : Level.Box -> Level.Line -> Int
boxOnLineSide =
    Debug.todo "Doom.Play.MapUtil.boxOnLineSide"


{-| P\_PointOnDivlineSide.
-}
pointOnDivlineSide : Fixed -> Fixed -> Divline -> Int
pointOnDivlineSide =
    Debug.todo "Doom.Play.MapUtil.pointOnDivlineSide"


{-| P\_MakeDivline.
-}
makeDivline : Level.Line -> Divline
makeDivline =
    Debug.todo "Doom.Play.MapUtil.makeDivline"


{-| P\_InterceptVector v2 v1: the fraction along v2 where it crosses v1.
-}
interceptVector : Divline -> Divline -> Fixed
interceptVector =
    Debug.todo "Doom.Play.MapUtil.interceptVector"


{-| P\_LineOpening: sets `opentop`, `openbottom`, `openrange` and `lowfloor` in the scratch.
-}
lineOpening : Env -> LineId -> World -> World
lineOpening =
    Debug.todo "Doom.Play.MapUtil.lineOpening"


{-| P\_UnsetThingPosition: unlink the thing from its sector's list and its blockmap cell.
-}
unsetThingPosition : Env -> ThinkerId -> World -> World
unsetThingPosition =
    Debug.todo "Doom.Play.MapUtil.unsetThingPosition"


{-| P\_SetThingPosition: find the thing's subsector and link it into its sector's list and blockmap cell.
-}
setThingPosition : Env -> ThinkerId -> World -> World
setThingPosition =
    Debug.todo "Doom.Play.MapUtil.setThingPosition"


{-| P\_BlockLinesIterator x y func: each line in blockmap cell (x, y) not yet checked this validcount, until func gives False.
-}
blockLinesIterator : Env -> Int -> Int -> (LineId -> World -> ( Bool, World )) -> World -> ( Bool, World )
blockLinesIterator =
    Debug.todo "Doom.Play.MapUtil.blockLinesIterator"


{-| P\_BlockThingsIterator x y func.
-}
blockThingsIterator : Env -> Int -> Int -> (ThinkerId -> World -> ( Bool, World )) -> World -> ( Bool, World )
blockThingsIterator =
    Debug.todo "Doom.Play.MapUtil.blockThingsIterator"


{-| P\_PathTraverse x1 y1 x2 y2 flags trav: the lines and things crossed going from (x1, y1) to (x2, y2), nearest first. Sets `trace` in the scratch.
-}
pathTraverse : Env -> Fixed -> Fixed -> Fixed -> Fixed -> Int -> (Intercept -> World -> ( Bool, World )) -> World -> ( Bool, World )
pathTraverse =
    Debug.todo "Doom.Play.MapUtil.pathTraverse"
