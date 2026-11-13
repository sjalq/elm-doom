module Doom.Audio exposing (Command(..))

{-| What the game asks its host to play (i\_sound.c's interface, after s\_sound.c's
channel and volume work): sound effects on numbered channels, and music.

@docs Command

-}


{-| One instruction to the host.

  - `StartSound`: play the sound lump (DSxxxx, raw DMX format) on `channel` (C's
    channel number, from 0 to the number of channels less one), replacing what was
    on it. `volume` is 0 to 127 (DMX's scale; the menu's 0 to 15 times 8 at
    most); `separation` 0 (left) to 255 (right), 128 the
    middle; `pitch` 128 is normal, and plays `2 ^ ((pitch - 128) / 64)` times as
    fast (I\_SetChannels's steptable; DOS v1.9's DMX ignored it and played every
    sound at its own rate); `priority` is what I\_StartSound is given (the mixer
    ignores it).
  - `UpdateSound`: change a playing channel's volume, separation and pitch.
  - `StopSound`: stop the channel.
  - `PlayMusic`: play the music lump (MUS format, D\_xxxx), looping or not.
  - `StopMusic`, `PauseMusic`, `ResumeMusic`.
  - `MusicVolume`, `SfxVolume`: the settings, 0 to 127 (the menu's 0 to 15 times
    8; S\_SetMusicVolume sets 127 first, then the setting, so it gives two
    `MusicVolume` commands).

-}
type Command
    = StartSound { channel : Int, lump : String, volume : Int, separation : Int, pitch : Int, priority : Int }
    | UpdateSound { channel : Int, volume : Int, separation : Int, pitch : Int }
    | StopSound Int
    | PlayMusic { lump : String, looping : Bool }
    | StopMusic
    | PauseMusic
    | ResumeMusic
    | MusicVolume Int
    | SfxVolume Int
