module Doom.Sound exposing (Event(..), Origin(..))

{-| The sounds the game asks for (S\_StartSound and S\_StopSound calls in the C source).
The game doesn't play them: it lists them in `World.sounds`, and whoever runs the game
plays them, working out volume and stereo from the origin and the listener, as
s\_sound.c does.

@docs Event, Origin

-}

import Doom.Info.Sfx exposing (Sfx)
import Doom.Level exposing (SectorId)
import Doom.Mobj exposing (ThinkerId)


{-| Start a sound, or stop the one a thing is making (S\_StopSound).
-}
type Event
    = Start Origin Sfx
    | Stop ThinkerId


{-| Where a sound comes from: a thing, the middle of a sector (doors, lifts,
switches), or nowhere in particular, heard at full volume (C passes NULL).
-}
type Origin
    = FromMobj ThinkerId
    | FromSector SectorId
    | Ambient
