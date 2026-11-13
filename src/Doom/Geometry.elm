module Doom.Geometry exposing (pointToAngle2, pointToDist, pointOnSide, pointInSubsector)

{-| The renderer's geometry that the game also uses (r\_main.c): angles between points,
which side of a BSP partition a point is on, and which subsector a point is in.

@docs pointToAngle2, pointToDist, pointOnSide, pointInSubsector

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Level as Level exposing (Level, Node, SubsectorId)


{-| R\_PointToAngle2 x1 y1 x2 y2: the angle from (x1, y1) to (x2, y2).
-}
pointToAngle2 : Fixed -> Fixed -> Fixed -> Fixed -> Angle
pointToAngle2 =
    Debug.todo "Doom.Geometry.pointToAngle2"


{-| R\_PointToDist, from viewpoint (viewx, viewy) to (x, y): `pointToDist viewx viewy x y`.
-}
pointToDist : Fixed -> Fixed -> Fixed -> Fixed -> Fixed
pointToDist =
    Debug.todo "Doom.Geometry.pointToDist"


{-| R\_PointOnSide: 0 for the node's front (right) side, 1 for its back.
-}
pointOnSide : Fixed -> Fixed -> Node -> Int
pointOnSide =
    Debug.todo "Doom.Geometry.pointOnSide"


{-| R\_PointInSubsector.
-}
pointInSubsector : Fixed -> Fixed -> Level -> SubsectorId
pointInSubsector =
    Debug.todo "Doom.Geometry.pointInSubsector"
