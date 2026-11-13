module Doom.Audio exposing (Command(..))

{-| What the game asks its host to play (i\_sound.c's interface, after s\_sound.c's
channel and volume work): sound effects on numbered channels, and music.

@docs Command

-}


{-| One instruction to the host.

  - `StartSound`: play the sound lump (DSxxxx, raw DMX format) on `channel` (C's
    channel number, 0 to 7), replacing what was on it. `volume` is 0 to 127,
    `separation` 0 (left) to 255 (right), 128 the middle; `pitch` 128 is normal (Doom
    varies it for some sounds: pitch 128 + n plays n/128 faster... as I\_StartSound
    does with its steptable).
  - `UpdateSound`: change a playing channel's volume, separation and pitch.
  - `StopSound`: stop the channel.
  - `PlayMusic`: play the music lump (MUS format, D\_xxxx), looping or not.
  - `StopMusic`, `PauseMusic`, `ResumeMusic`.
  - `MusicVolume`, `SfxVolume`: the settings, 0 to 15.

-}
type Command
    = StartSound { channel : Int, lump : String, volume : Int, separation : Int, pitch : Int }
    | UpdateSound { channel : Int, volume : Int, separation : Int, pitch : Int }
    | StopSound Int
    | PlayMusic { lump : String, looping : Bool }
    | StopMusic
    | PauseMusic
    | ResumeMusic
    | MusicVolume Int
    | SfxVolume Int
