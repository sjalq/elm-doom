module Doom.Play.Light exposing (thinkFireFlicker, thinkLightFlash, thinkStrobe, thinkGlow, spawnFireFlicker, spawnLightFlash, spawnStrobeFlash, spawnGlowingLight, startLightStrobing, turnTagLightsOff, lightTurnOn)

{-| Flickering, flashing, strobing and glowing lights, and switching lights
(p\_lights.c).

@docs thinkFireFlicker, thinkLightFlash, thinkStrobe, thinkGlow, spawnFireFlicker, spawnLightFlash, spawnStrobeFlash, spawnGlowingLight, startLightStrobing, turnTagLightsOff, lightTurnOn

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


{-| T\_FireFlicker.
-}
thinkFireFlicker : Env -> ThinkerId -> Thinker.FireFlicker -> World -> World
thinkFireFlicker =
    Debug.todo "Doom.Play.Light.thinkFireFlicker"


{-| T\_LightFlash.
-}
thinkLightFlash : Env -> ThinkerId -> Thinker.LightFlash -> World -> World
thinkLightFlash =
    Debug.todo "Doom.Play.Light.thinkLightFlash"


{-| T\_StrobeFlash.
-}
thinkStrobe : Env -> ThinkerId -> Thinker.Strobe -> World -> World
thinkStrobe =
    Debug.todo "Doom.Play.Light.thinkStrobe"


{-| T\_Glow.
-}
thinkGlow : Env -> ThinkerId -> Thinker.Glow -> World -> World
thinkGlow =
    Debug.todo "Doom.Play.Light.thinkGlow"


{-| P\_SpawnFireFlicker.
-}
spawnFireFlicker : Env -> SectorId -> World -> World
spawnFireFlicker =
    Debug.todo "Doom.Play.Light.spawnFireFlicker"


{-| P\_SpawnLightFlash.
-}
spawnLightFlash : Env -> SectorId -> World -> World
spawnLightFlash =
    Debug.todo "Doom.Play.Light.spawnLightFlash"


{-| P\_SpawnStrobeFlash sector fastOrSlow inSync.
-}
spawnStrobeFlash : Env -> SectorId -> Int -> Bool -> World -> World
spawnStrobeFlash =
    Debug.todo "Doom.Play.Light.spawnStrobeFlash"


{-| P\_SpawnGlowingLight.
-}
spawnGlowingLight : Env -> SectorId -> World -> World
spawnGlowingLight =
    Debug.todo "Doom.Play.Light.spawnGlowingLight"


{-| EV\_StartLightStrobing.
-}
startLightStrobing : Env -> LineId -> World -> World
startLightStrobing =
    Debug.todo "Doom.Play.Light.startLightStrobing"


{-| EV\_TurnTagLightsOff.
-}
turnTagLightsOff : Env -> LineId -> World -> World
turnTagLightsOff =
    Debug.todo "Doom.Play.Light.turnTagLightsOff"


{-| EV\_LightTurnOn line bright.
-}
lightTurnOn : Env -> LineId -> Int -> World -> World
lightTurnOn =
    Debug.todo "Doom.Play.Light.lightTurnOn"
