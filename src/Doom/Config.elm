module Doom.Config exposing
    ( Settings, defaultSettings
    , Bindings, defaultBindings
    , Skill(..)
    )

{-| What DOS Doom kept in its configuration file (m\_misc.c's defaults), for a host
to store between sessions and hand back when it starts the game (`Doom.Options`):
the settings the menu changes, and the key and mouse bindings. And the skill levels,
for starting a game straight away.

@docs Settings, defaultSettings
@docs Bindings, defaultBindings
@docs Skill

-}


{-| The settings the menu changes (m\_misc.c's defaults): `screenblocks` 3 to 11,
`detailLevel` 0 high or 1 low, the volumes 0 to 15, `mouseSensitivity` 0 to 9,
`usegamma` 0 to 4.
-}
type alias Settings =
    { mouseSensitivity : Int
    , showMessages : Bool
    , detailLevel : Int
    , screenblocks : Int
    , sfxVolume : Int
    , musicVolume : Int
    , usegamma : Int
    }


{-| m\_misc.c's defaults: screenblocks 9, high detail, no gamma correction, messages
on, mouse sensitivity 5, both volumes 8.
-}
defaultSettings : Settings
defaultSettings =
    { mouseSensitivity = 5
    , showMessages = True
    , detailLevel = 0
    , screenblocks = 9
    , sfxVolume = 8
    , musicVolume = 8
    , usegamma = 0
    }


{-| The key bindings (Doom key codes, see `Doom.Event`) and mouse buttons (0 left, 1
right, 2 middle): m\_misc.c's `key_*` and `mouseb_*` settings.
-}
type alias Bindings =
    { right : Int
    , left : Int
    , up : Int
    , down : Int
    , strafeLeft : Int
    , strafeRight : Int
    , fire : Int
    , use : Int
    , strafe : Int
    , speed : Int
    , mouseFire : Int
    , mouseStrafe : Int
    , mouseForward : Int
    }


{-| The defaults: arrows to move and turn, ',' and '.' to strafe, Ctrl fires, space
uses, Alt strafes, Shift runs; the left mouse button fires, the right one strafes,
the middle one moves forward.
-}
defaultBindings : Bindings
defaultBindings =
    { right = 0xAE
    , left = 0xAC
    , up = 0xAD
    , down = 0xAF
    , strafeLeft = Char.toCode ','
    , strafeRight = Char.toCode '.'
    , fire = 157
    , use = Char.toCode ' '
    , strafe = 184
    , speed = 182
    , mouseFire = 0
    , mouseStrafe = 1
    , mouseForward = 2
    }


{-| The skill levels (skill\_t): "I'm too young to die" to "Nightmare!".
-}
type Skill
    = Baby
    | Easy
    | Medium
    | Hard
    | Nightmare
