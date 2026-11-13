module Doom.Play.Spec exposing (crossSpecialLine, shootSpecialLine, playerInSpecialSector, updateSpecials, spawnSpecials, textureTranslation, flatTranslation)

{-| Line and sector specials (p\_spec.c): what crossing or shooting a line does, damaging
and secret sectors, scrolling walls, switches flipping back, and setting up a level's
specials. Animated textures and flats only change what is drawn: see
`textureTranslation` and `flatTranslation`.

@docs crossSpecialLine, shootSpecialLine, playerInSpecialSector, updateSpecials, spawnSpecials, textureTranslation, flatTranslation

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


{-| P\_CrossSpecialLine linenum side thing.
-}
crossSpecialLine : Env -> LineId -> Int -> ThinkerId -> World -> World
crossSpecialLine =
    Debug.todo "Doom.Play.Spec.crossSpecialLine"


{-| P\_ShootSpecialLine thing line.
-}
shootSpecialLine : Env -> ThinkerId -> LineId -> World -> World
shootSpecialLine =
    Debug.todo "Doom.Play.Spec.shootSpecialLine"


{-| P\_PlayerInSpecialSector player.
-}
playerInSpecialSector : Env -> Int -> World -> World
playerInSpecialSector =
    Debug.todo "Doom.Play.Spec.playerInSpecialSector"


{-| P\_UpdateSpecials: scrolling walls and buttons (texture animation is `textureTranslation`).
-}
updateSpecials : Env -> World -> World
updateSpecials =
    Debug.todo "Doom.Play.Spec.updateSpecials"


{-| P\_SpawnSpecials.
-}
spawnSpecials : Env -> World -> World
spawnSpecials =
    Debug.todo "Doom.Play.Spec.spawnSpecials"


{-| texturetranslation[texture] at leveltime: `textureTranslation env leveltime texture`.
-}
textureTranslation : Env -> Int -> Int -> Int
textureTranslation =
    Debug.todo "Doom.Play.Spec.textureTranslation"


{-| flattranslation[flat] at leveltime: `flatTranslation env leveltime flat`.
-}
flatTranslation : Env -> Int -> Int -> Int
flatTranslation =
    Debug.todo "Doom.Play.Spec.flatTranslation"
