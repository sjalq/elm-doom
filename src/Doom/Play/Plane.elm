module Doom.Play.Plane exposing (PlaneResult(..), movePlane)

{-| Moving a sector's floor or ceiling (T\_MovePlane, p\_floor.c), which every moving
floor, ceiling, door and lift uses.

@docs PlaneResult, movePlane

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| result\_e.
-}
type PlaneResult
    = PlaneOk
    | PlaneCrushed
    | PlanePastDest


{-| T\_MovePlane sector speed dest crush floorOrCeiling direction (floorOrCeiling 0 floor, 1 ceiling).
-}
movePlane : Env -> SectorId -> Fixed -> Fixed -> Bool -> Int -> Int -> World -> ( PlaneResult, World )
movePlane =
    Debug.todo "Doom.Play.Plane.movePlane"
