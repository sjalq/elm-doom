module Doom.Sound exposing (Event(..), Origin(..), Moment, Spot, drawsPitch)

{-| The sounds the game asks for (S\_StartSound and S\_StopSound calls in the C source).
The game doesn't play them: it lists them in `World.sounds`, and whoever runs the game
plays them, working out volume and stereo from the origin and the listener, as
s\_sound.c does (`Doom.Audio.Sound`).

@docs Event, Origin, Moment, Spot, drawsPitch

-}

import Doom.Angle exposing (Angle)
import Doom.Fixed exposing (Fixed)
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Level exposing (SectorId)
import Doom.Mobj exposing (ThinkerId)


{-| Start a sound, or stop the one a thing is making (S\_StopSound).

  - `Start`: a sound asked for with no level around it (menus, the intermission,
    the finale), or by code that doesn't know where things are.
  - `StartAt`: a sound the play code asked for, with where everything was at that
    moment (`World.startSound` fills it in). The C source works out a sound's volume
    and stereo the moment it starts, halfway through a tic, so a thing that moves
    later in the same tic (a missile, just after it is fired) must be heard from
    where it was, not from where the tic leaves it.
  - `StartDrawn`: a sound from nowhere in particular (`Ambient`) whose pitch
    variation byte the caller has already drawn from M\_Random (`drawsPitch`), at
    the moment S\_StartSound would have, between its own draws (the intermission's
    animations). Only for `Ambient`: a sound from somewhere draws only if it is near
    enough to hear, which only `Doom.Audio.Sound` knows.

-}
type Event
    = Start Origin Sfx
    | StartAt Origin Sfx Moment
    | StartDrawn Origin Sfx Int
    | Stop ThinkerId


{-| Where a sound comes from: a thing, the middle of a sector (doors, lifts,
switches), or nowhere in particular, heard at full volume (C passes NULL).

`FromButton i` is the click of the switch in `buttonlist[i]` flipping back. The C
source passes the address of that slot's `soundorg` field (`&buttonlist[i].soundorg`)
where it means the field's value, so s\_sound.c reads a "thing" laid over the button
list: on DOS's 32-bit layout its x and y are the `btexture` and `btimer` of the next
slot, `buttonlist[i + 1]` (0 and 0 when that slot is free or past the end), a point
by the map's origin, wherever the switch is.

-}
type Origin
    = FromMobj ThinkerId
    | FromSector SectorId
    | FromButton Int
    | Ambient


{-| The moment a sound started: the tic (`gametic`), where its origin was (for a
`FromMobj` origin that still exists, and a `FromButton`), and where each player's body was (by player
number, `Nothing` for a player without one), so that the console player's ears can
be found.
-}
type alias Moment =
    { gametic : Int
    , origin : Maybe Spot
    , players : List (Maybe Spot)
    }


{-| A thing's position and facing, and which thing it is.
-}
type alias Spot =
    { mo : ThinkerId
    , x : Fixed
    , y : Fixed
    , angle : Angle
    }


{-| Whether S\_StartSound draws a pitch variation from M\_Random for this sound when
it is heard: every sound but `Itemup` and `Tink` (`Sawup` to `Sawhit` use the low 4
bits, the rest the low 5).
-}
drawsPitch : Sfx -> Bool
drawsPitch sfx =
    sfx /= Sfx.Itemup && sfx /= Sfx.Tink
