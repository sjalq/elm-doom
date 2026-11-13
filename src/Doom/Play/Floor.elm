module Doom.Play.Floor exposing (think, doFloorByTag, doFloor, buildStairs, doDonut)

{-| Moving floors and stairs (p\_floor.c, and EV\_DoDonut from p\_spec.c).

@docs think, doFloorByTag, doFloor, buildStairs, doDonut

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


{-| T\_MoveFloor.
-}
think : Env -> ThinkerId -> Thinker.FloorMove -> World -> World
think =
    Debug.todo "Doom.Play.Floor.think"


{-| EV\_DoFloor.
-}
doFloor : Env -> LineId -> Thinker.FloorType -> World -> ( Bool, World )
doFloor =
    Debug.todo "Doom.Play.Floor.doFloor"


{-| EV\_BuildStairs.
-}
buildStairs : Env -> LineId -> Thinker.StairType -> World -> ( Bool, World )
buildStairs =
    Debug.todo "Doom.Play.Floor.buildStairs"


{-| EV\_DoDonut.
-}
doDonut : Env -> LineId -> World -> ( Bool, World )
doDonut =
    Debug.todo "Doom.Play.Floor.doDonut"


{-| EV\_DoFloor on every sector with this tag (a line with only a tag, as A\_BossDeath's `junk` line). Stub: doom-specials implements it.
-}
doFloorByTag : Env -> Int -> Thinker.FloorType -> World -> ( Bool, World )
doFloorByTag =
    Debug.todo "Doom.Play.Floor.doFloorByTag"
