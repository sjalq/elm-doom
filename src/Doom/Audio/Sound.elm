module Doom.Audio.Sound exposing
    ( Model, Settings, defaults, init
    , startLevel, play, startSound, updateSounds
    , changeMusic, startMusic, stopMusic, levelMusic, pauseSound, resumeSound
    , setSfxVolume, setMusicVolume, sfxVolume, musicVolume
    , mRandom, randomIndex, setRandomIndex, clearRandom
    , soundTics, lumpName
    )

{-| Sound (s\_sound.c). Stub with doom-audio's announced API (their module replaces it).

@docs Model, Settings, defaults, init
@docs startLevel, play, startSound, updateSounds
@docs changeMusic, startMusic, stopMusic, levelMusic, pauseSound, resumeSound
@docs setSfxVolume, setMusicVolume, sfxVolume, musicVolume
@docs mRandom, randomIndex, setRandomIndex, clearRandom
@docs soundTics, lumpName

-}

import Doom.Audio exposing (Command)
import Doom.Defs exposing (GameMode)
import Doom.Info.Sfx exposing (Sfx)
import Doom.Play.Env exposing (Env, Params)
import Doom.Sound as Sound
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The channels.
-}
type Model
    = Model


{-| snd\_channels and the volumes.
-}
type alias Settings =
    { channels : Int
    , sfxVolume : Int
    , musicVolume : Int
    }


{-| linuxdoom's defaults.
-}
defaults : Settings
defaults =
    { channels = 3, sfxVolume = 8, musicVolume = 8 }


{-| S\_Init.
-}
init : Wad -> Settings -> ( Model, List Command )
init _ _ =
    Debug.todo "Doom.Audio.Sound.init"


{-| S\_Start.
-}
startLevel : Int -> Params -> Model -> ( Model, List Command )
startLevel _ _ _ =
    Debug.todo "Doom.Audio.Sound.startLevel"


{-| A tic's sounds.
-}
play : Int -> Env -> World -> List Sound.Event -> Model -> ( Model, List Command )
play _ _ _ _ _ =
    Debug.todo "Doom.Audio.Sound.play"


{-| S\_StartSound(NULL, sfx).
-}
startSound : Int -> Sfx -> Model -> ( Model, List Command )
startSound _ _ _ =
    Debug.todo "Doom.Audio.Sound.startSound"


{-| S\_UpdateSounds.
-}
updateSounds : Int -> Maybe ( Env, World ) -> Model -> ( Model, List Command )
updateSounds _ _ _ =
    Debug.todo "Doom.Audio.Sound.updateSounds"


{-| S\_ChangeMusic.
-}
changeMusic : String -> Bool -> Model -> ( Model, List Command )
changeMusic _ _ _ =
    Debug.todo "Doom.Audio.Sound.changeMusic"


{-| S\_StartMusic.
-}
startMusic : String -> Model -> ( Model, List Command )
startMusic _ _ =
    Debug.todo "Doom.Audio.Sound.startMusic"


{-| S\_StopMusic.
-}
stopMusic : Model -> ( Model, List Command )
stopMusic _ =
    Debug.todo "Doom.Audio.Sound.stopMusic"


{-| A level's music.
-}
levelMusic : GameMode -> Int -> Int -> Maybe String
levelMusic _ _ _ =
    Debug.todo "Doom.Audio.Sound.levelMusic"


{-| S\_PauseSound.
-}
pauseSound : Model -> ( Model, List Command )
pauseSound _ =
    Debug.todo "Doom.Audio.Sound.pauseSound"


{-| S\_ResumeSound.
-}
resumeSound : Model -> ( Model, List Command )
resumeSound _ =
    Debug.todo "Doom.Audio.Sound.resumeSound"


{-| S\_SetSfxVolume.
-}
setSfxVolume : Int -> Model -> ( Model, List Command )
setSfxVolume _ _ =
    Debug.todo "Doom.Audio.Sound.setSfxVolume"


{-| S\_SetMusicVolume.
-}
setMusicVolume : Int -> Model -> ( Model, List Command )
setMusicVolume _ _ =
    Debug.todo "Doom.Audio.Sound.setMusicVolume"


{-| snd\_SfxVolume.
-}
sfxVolume : Model -> Int
sfxVolume _ =
    Debug.todo "Doom.Audio.Sound.sfxVolume"


{-| snd\_MusicVolume.
-}
musicVolume : Model -> Int
musicVolume _ =
    Debug.todo "Doom.Audio.Sound.musicVolume"


{-| M\_Random.
-}
mRandom : Model -> ( Int, Model )
mRandom _ =
    Debug.todo "Doom.Audio.Sound.mRandom"


{-| rndindex.
-}
randomIndex : Model -> Int
randomIndex _ =
    Debug.todo "Doom.Audio.Sound.randomIndex"


{-| Set rndindex.
-}
setRandomIndex : Int -> Model -> Model
setRandomIndex _ _ =
    Debug.todo "Doom.Audio.Sound.setRandomIndex"


{-| M\_ClearRandom's half.
-}
clearRandom : Model -> Model
clearRandom _ =
    Debug.todo "Doom.Audio.Sound.clearRandom"


{-| A sound's length in tics.
-}
soundTics : Sfx -> Model -> Int
soundTics _ _ =
    Debug.todo "Doom.Audio.Sound.soundTics"


{-| A sound's lump.
-}
lumpName : Sfx -> Model -> String
lumpName _ _ =
    Debug.todo "Doom.Audio.Sound.lumpName"
