module Doom.Play.SpecUtil exposing (getSide, getSector, twoSided, getNextSector, findLowestFloorSurrounding, findHighestFloorSurrounding, findNextHighestFloor, findLowestCeilingSurrounding, findHighestCeilingSurrounding, findSectorFromLineTag, findMinSurroundingLight)

{-| The sector and line lookups p\_spec.c gives the moving floors, doors, lifts and
lights.

@docs getSide, getSector, twoSided, getNextSector, findLowestFloorSurrounding, findHighestFloorSurrounding, findNextHighestFloor, findLowestCeilingSurrounding, findHighestCeilingSurrounding, findSectorFromLineTag, findMinSurroundingLight

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


{-| getSide currentSector line side: side `side` of the sector's `line`th line.
-}
getSide : Level -> SectorId -> Int -> Int -> SideId
getSide =
    Debug.todo "Doom.Play.SpecUtil.getSide"


{-| getSector currentSector line side.
-}
getSector : Level -> SectorId -> Int -> Int -> SectorId
getSector =
    Debug.todo "Doom.Play.SpecUtil.getSector"


{-| twoSided sector line.
-}
twoSided : Level -> SectorId -> Int -> Bool
twoSided =
    Debug.todo "Doom.Play.SpecUtil.twoSided"


{-| getNextSector line sec: the sector on the line's other side from sec.
-}
getNextSector : Level -> LineId -> SectorId -> Maybe SectorId
getNextSector =
    Debug.todo "Doom.Play.SpecUtil.getNextSector"


{-| P\_FindLowestFloorSurrounding.
-}
findLowestFloorSurrounding : Env -> SectorId -> World -> Fixed
findLowestFloorSurrounding =
    Debug.todo "Doom.Play.SpecUtil.findLowestFloorSurrounding"


{-| P\_FindHighestFloorSurrounding.
-}
findHighestFloorSurrounding : Env -> SectorId -> World -> Fixed
findHighestFloorSurrounding =
    Debug.todo "Doom.Play.SpecUtil.findHighestFloorSurrounding"


{-| P\_FindNextHighestFloor sec currentheight.
-}
findNextHighestFloor : Env -> SectorId -> Fixed -> World -> Fixed
findNextHighestFloor =
    Debug.todo "Doom.Play.SpecUtil.findNextHighestFloor"


{-| P\_FindLowestCeilingSurrounding.
-}
findLowestCeilingSurrounding : Env -> SectorId -> World -> Fixed
findLowestCeilingSurrounding =
    Debug.todo "Doom.Play.SpecUtil.findLowestCeilingSurrounding"


{-| P\_FindHighestCeilingSurrounding.
-}
findHighestCeilingSurrounding : Env -> SectorId -> World -> Fixed
findHighestCeilingSurrounding =
    Debug.todo "Doom.Play.SpecUtil.findHighestCeilingSurrounding"


{-| P\_FindSectorFromLineTag line start: the next sector after `start` with the line's tag, -1 if none.
-}
findSectorFromLineTag : Env -> LineId -> Int -> World -> Int
findSectorFromLineTag =
    Debug.todo "Doom.Play.SpecUtil.findSectorFromLineTag"


{-| P\_FindMinSurroundingLight sector max.
-}
findMinSurroundingLight : Env -> SectorId -> Int -> World -> Int
findMinSurroundingLight =
    Debug.todo "Doom.Play.SpecUtil.findMinSurroundingLight"
