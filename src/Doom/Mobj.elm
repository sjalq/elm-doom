module Doom.Mobj exposing (Mobj, ThinkerId, frameNumber, fullbright)

{-| A thing in the world (mobj\_t, p\_mobj.h): a monster, the player's body, a
projectile, an item, a decoration.

@docs Mobj, ThinkerId, frameNumber, fullbright

-}

import Bitwise
import Doom.Angle exposing (Angle)
import Doom.Fixed exposing (Fixed)
import Doom.Info.MobjType exposing (MobjType)
import Doom.Info.Sprite exposing (Sprite)
import Doom.Info.State exposing (State)
import Doom.Level exposing (MapThing, SubsectorId)


{-| A thinker's id: its key in `World.thinkers`. Ids are handed out in increasing
order, so they also give the order the C source keeps its thinker list in.
-}
type alias ThinkerId =
    Int


{-| mobj\_t. Where the C source has pointers:

  - `id`: the thing's own id (its thinker).
  - `subsector`: the subsector its centre is in (`mobj->subsector`); its sector is
    that subsector's.
  - `target`, `tracer`: other things, by id. A thing that has been removed may still
    be found by id, as a freed mobj could still be read in the C source.
  - `player`: the number of the player this is the body of.
  - `type_` gives `info` (`Doom.Play.Env.mobjInfo`); `state` is the current state.

The thinker-list and blockmap/sector links (`snext`, `bnext`...) are in
`World.blocklinks` and `World.sectorThings`.

-}
type alias Mobj =
    { id : ThinkerId
    , x : Fixed
    , y : Fixed
    , z : Fixed
    , angle : Angle
    , sprite : Sprite
    , frame : Int
    , subsector : SubsectorId
    , floorz : Fixed
    , ceilingz : Fixed
    , radius : Fixed
    , height : Fixed
    , momx : Fixed
    , momy : Fixed
    , momz : Fixed
    , validcount : Int
    , type_ : MobjType
    , tics : Int
    , state : State
    , flags : Int
    , health : Int
    , movedir : Int
    , movecount : Int
    , target : Maybe ThinkerId
    , reactiontime : Int
    , threshold : Int
    , player : Maybe Int
    , lastlook : Int
    , spawnpoint : MapThing
    , tracer : Maybe ThinkerId
    }


{-| The frame letter's number (`frame & FF_FRAMEMASK`): 0 is A.
-}
frameNumber : Mobj -> Int
frameNumber mobj =
    Bitwise.and mobj.frame 0x7FFF


{-| Whether the frame is drawn at full brightness (FF\_FULLBRIGHT).
-}
fullbright : Mobj -> Bool
fullbright mobj =
    Bitwise.and mobj.frame 0x8000 /= 0
