module Doom.Sound exposing (Event(..), Origin(..), Moment, Spot)

{-| The sounds the game asks for (S\_StartSound and S\_StopSound calls in the C source).
The game doesn't play them: it lists them in `World.sounds`, and whoever runs the game
plays them, working out volume and stereo from the origin and the listener, as
s\_sound.c does (`Doom.Audio.Sound`).

@docs Event, Origin, Moment, Spot

-}

import Doom.Angle exposing (Angle)
import Doom.Fixed exposing (Fixed)
import Doom.Info.Sfx exposing (Sfx)
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

-}
type Event
    = Start Origin Sfx
    | StartAt Origin Sfx Moment
    | Stop ThinkerId


{-| Where a sound comes from: a thing, the middle of a sector (doors, lifts,
switches), or nowhere in particular, heard at full volume (C passes NULL).
-}
type Origin
    = FromMobj ThinkerId
    | FromSector SectorId
    | Ambient


{-| The moment a sound started: the tic (`gametic`), where its thing was (for a
`FromMobj` origin that still exists), and where each player's body was (by player
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
