module Doom.Thinker exposing
    ( Thinker(..)
    , Door, DoorType(..)
    , Plat, PlatStatus(..), PlatType(..)
    , Ceiling, CeilingType(..)
    , FloorMove, FloorType(..), StairType(..)
    , FireFlicker, LightFlash, Strobe, Glow
    , glowSpeed, strobeBright, fastDark, slowDark
    , vdoorSpeed, vdoorWait, platWait, platSpeed, ceilSpeed, ceilWait, floorSpeed
    , maxPlats, maxCeilings
    )

{-| Thinkers (d\_think.h, p\_spec.h): everything that acts once a tic. Things act through
`MobjThinker`, whose data is the `Mobj` with the same id in `World.mobjs`; moving
floors, doors, lifts and flickering lights carry their state here.

`Removed` is C's `function == -1`: P\_RemoveThinker marks a thinker so, and
P\_RunThinkers drops it when it gets to it. A plat or ceiling "in stasis" (C sets its
function to NULL) keeps its variant: its status says it is in stasis, and its think
function then does nothing, as in the C source.

@docs Thinker
@docs Door, DoorType
@docs Plat, PlatStatus, PlatType
@docs Ceiling, CeilingType
@docs FloorMove, FloorType, StairType
@docs FireFlicker, LightFlash, Strobe, Glow
@docs glowSpeed, strobeBright, fastDark, slowDark
@docs vdoorSpeed, vdoorWait, platWait, platSpeed, ceilSpeed, ceilWait, floorSpeed
@docs maxPlats, maxCeilings

-}

import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Level exposing (SectorId)


{-| A thinker.
-}
type Thinker
    = MobjThinker
    | DoorThinker Door
    | PlatThinker Plat
    | CeilingThinker Ceiling
    | FloorThinker FloorMove
    | FireFlickerThinker FireFlicker
    | LightFlashThinker LightFlash
    | StrobeThinker Strobe
    | GlowThinker Glow
    | Removed



-- DOORS


{-| vldoor\_t. `direction`: 1 opening, 0 waiting at the top, -1 closing, 2 waiting to
open (raiseIn5Mins).
-}
type alias Door =
    { type_ : DoorType
    , sector : SectorId
    , topheight : Fixed
    , speed : Fixed
    , direction : Int
    , topwait : Int
    , topcountdown : Int
    }


{-| vldoor\_e.
-}
type DoorType
    = NormalDoor
    | Close30ThenOpen
    | CloseDoor
    | OpenDoor
    | RaiseIn5Mins
    | BlazeRaise
    | BlazeOpen
    | BlazeClose



-- PLATS


{-| plat\_t.
-}
type alias Plat =
    { sector : SectorId
    , speed : Fixed
    , low : Fixed
    , high : Fixed
    , wait : Int
    , count : Int
    , status : PlatStatus
    , oldstatus : PlatStatus
    , crush : Bool
    , tag : Int
    , type_ : PlatType
    }


{-| plat\_e.
-}
type PlatStatus
    = PlatUp
    | PlatDown
    | PlatWaiting
    | PlatInStasis


{-| plattype\_e.
-}
type PlatType
    = PerpetualRaise
    | DownWaitUpStay
    | RaiseAndChange
    | RaiseToNearestAndChange
    | BlazeDWUS



-- CEILINGS


{-| ceiling\_t. `direction`: 1 up, -1 down, 0 in stasis.
-}
type alias Ceiling =
    { type_ : CeilingType
    , sector : SectorId
    , bottomheight : Fixed
    , topheight : Fixed
    , speed : Fixed
    , crush : Bool
    , direction : Int
    , tag : Int
    , olddirection : Int
    }


{-| ceiling\_e.
-}
type CeilingType
    = LowerToFloor
    | RaiseToHighest
    | LowerAndCrush
    | CrushAndRaise
    | FastCrushAndRaise
    | SilentCrushAndRaise



-- FLOORS


{-| floormove\_t. `texture` is a flat number.
-}
type alias FloorMove =
    { type_ : FloorType
    , crush : Bool
    , sector : SectorId
    , direction : Int
    , newspecial : Int
    , texture : Int
    , floordestheight : Fixed
    , speed : Fixed
    }


{-| floor\_e.
-}
type FloorType
    = LowerFloor
    | LowerFloorToLowest
    | TurboLower
    | RaiseFloor
    | RaiseFloorToNearest
    | RaiseToTexture
    | LowerAndChange
    | RaiseFloor24
    | RaiseFloor24AndChange
    | RaiseFloorCrush
    | RaiseFloorTurbo
    | DonutRaise
    | RaiseFloor512


{-| stair\_e.
-}
type StairType
    = Build8
    | Turbo16



-- LIGHTS


{-| fireflicker\_t.
-}
type alias FireFlicker =
    { sector : SectorId
    , count : Int
    , maxlight : Int
    , minlight : Int
    }


{-| lightflash\_t.
-}
type alias LightFlash =
    { sector : SectorId
    , count : Int
    , maxlight : Int
    , minlight : Int
    , maxtime : Int
    , mintime : Int
    }


{-| strobe\_t.
-}
type alias Strobe =
    { sector : SectorId
    , count : Int
    , minlight : Int
    , maxlight : Int
    , darktime : Int
    , brighttime : Int
    }


{-| glow\_t.
-}
type alias Glow =
    { sector : SectorId
    , minlight : Int
    , maxlight : Int
    , direction : Int
    }



-- CONSTANTS


{-| GLOWSPEED.
-}
glowSpeed : Int
glowSpeed =
    8


{-| STROBEBRIGHT.
-}
strobeBright : Int
strobeBright =
    5


{-| FASTDARK.
-}
fastDark : Int
fastDark =
    15


{-| SLOWDARK.
-}
slowDark : Int
slowDark =
    35


{-| VDOORSPEED.
-}
vdoorSpeed : Fixed
vdoorSpeed =
    Fixed.fracunit * 2


{-| VDOORWAIT.
-}
vdoorWait : Int
vdoorWait =
    150


{-| PLATWAIT (seconds).
-}
platWait : Int
platWait =
    3


{-| PLATSPEED.
-}
platSpeed : Fixed
platSpeed =
    Fixed.fracunit


{-| CEILSPEED.
-}
ceilSpeed : Fixed
ceilSpeed =
    Fixed.fracunit


{-| CEILWAIT.
-}
ceilWait : Int
ceilWait =
    150


{-| FLOORSPEED.
-}
floorSpeed : Fixed
floorSpeed =
    Fixed.fracunit


{-| MAXPLATS: the size of `World.activePlats`.
-}
maxPlats : Int
maxPlats =
    30


{-| MAXCEILINGS: the size of `World.activeCeilings`.
-}
maxCeilings : Int
maxCeilings =
    30
