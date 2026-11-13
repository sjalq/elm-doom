module Doom.Play.Door exposing (think, doLockedDoor, doDoor, verticalDoor, spawnDoorCloseIn30, spawnDoorRaiseIn5Mins)

{-| Doors (p\_doors.c).

@docs think, doLockedDoor, doDoor, verticalDoor, spawnDoorCloseIn30, spawnDoorRaiseIn5Mins

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Thinker as Thinker exposing (Thinker)
import Doom.World as World exposing (World)


{-| T\_VerticalDoor.
-}
think : Env -> ThinkerId -> Thinker.Door -> World -> World
think =
    Debug.todo "Doom.Play.Door.think"


{-| EV\_DoLockedDoor line type thing.
-}
doLockedDoor : Env -> LineId -> Thinker.DoorType -> ThinkerId -> World -> ( Bool, World )
doLockedDoor =
    Debug.todo "Doom.Play.Door.doLockedDoor"


{-| EV\_DoDoor.
-}
doDoor : Env -> LineId -> Thinker.DoorType -> World -> ( Bool, World )
doDoor =
    Debug.todo "Doom.Play.Door.doDoor"


{-| EV\_VerticalDoor line thing.
-}
verticalDoor : Env -> LineId -> ThinkerId -> World -> World
verticalDoor =
    Debug.todo "Doom.Play.Door.verticalDoor"


{-| P\_SpawnDoorCloseIn30.
-}
spawnDoorCloseIn30 : Env -> SectorId -> World -> World
spawnDoorCloseIn30 =
    Debug.todo "Doom.Play.Door.spawnDoorCloseIn30"


{-| P\_SpawnDoorRaiseIn5Mins.
-}
spawnDoorRaiseIn5Mins : Env -> SectorId -> World -> World
spawnDoorRaiseIn5Mins =
    Debug.todo "Doom.Play.Door.spawnDoorRaiseIn5Mins"
