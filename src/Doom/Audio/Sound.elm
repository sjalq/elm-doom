module Doom.Audio.Sound exposing (Model, init, startLevel, update, menuSound, startMusic, setVolumes)

{-| Sound (s\_sound.c): choosing channels, priorities, volume and stereo from where a sound is and where the listener is, and which music a level has.

@docs Model, init, startLevel, update, menuSound, startMusic, setVolumes

-}

import Doom.Audio exposing (Command)
import Doom.Play.Env exposing (Env)
import Doom.Sound as Sound
import Doom.World exposing (World)


{-| The 8 channels and what is on them, the volumes, the music playing.
-}
type Model
    = Model


{-| S\_Init sfxVolume musicVolume (0 to 15).
-}
init : Int -> Int -> Model
init a0 a1 =
    Debug.todo "Doom.Audio.Sound.init"


{-| S\_Start: stop the level's sounds, start its music.
-}
startLevel : Env -> Model -> ( Model, List Command )
startLevel a0 a1 =
    Debug.todo "Doom.Audio.Sound.startLevel"


{-| S\_StartSound/S\_StopSound for this tic's events, then S\_UpdateSounds for listener player n.
-}
update : Env -> World -> Int -> List Sound.Event -> Model -> ( Model, List Command )
update a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Audio.Sound.update"


{-| A sound with no game around it (menus, intermission, finale).
-}
menuSound : Sound.Event -> Model -> ( Model, List Command )
menuSound a0 a1 =
    Debug.todo "Doom.Audio.Sound.menuSound"


{-| S\_ChangeMusic by lump name.
-}
startMusic : String -> Bool -> Model -> ( Model, List Command )
startMusic a0 a1 a2 =
    Debug.todo "Doom.Audio.Sound.startMusic"


{-| S\_SetSfxVolume and S\_SetMusicVolume.
-}
setVolumes : Int -> Int -> Model -> ( Model, List Command )
setVolumes a0 a1 a2 =
    Debug.todo "Doom.Audio.Sound.setVolumes"
