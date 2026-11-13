module Doom.Event exposing
    ( Event(..)
    , keyRightArrow, keyLeftArrow, keyUpArrow, keyDownArrow, keyEscape, keyEnter, keyTab
    , keyF1, keyF2, keyF3, keyF4, keyF5, keyF6, keyF7, keyF8, keyF9, keyF10, keyF11, keyF12
    , keyBackspace, keyPause, keyEquals, keyMinus, keyRShift, keyRCtrl, keyRAlt
    , keyCapsLock, keyScrollLock, keyNumLock
    )

{-| What the player does (event\_t, d\_event.h), as the game's responders see it:
keys by Doom's key codes (doomdef.h), the mouse by its movement and buttons.

Key codes: printable keys are their lower-case ASCII code (`Char.toCode 'a'`, `32`
for space); the others are the constants here. Whoever hosts the game turns its own
keyboard events into these (in a browser: `KeyboardEvent.key`).

@docs Event
@docs keyRightArrow, keyLeftArrow, keyUpArrow, keyDownArrow, keyEscape, keyEnter, keyTab
@docs keyF1, keyF2, keyF3, keyF4, keyF5, keyF6, keyF7, keyF8, keyF9, keyF10, keyF11, keyF12
@docs keyBackspace, keyPause, keyEquals, keyMinus, keyRShift, keyRCtrl, keyRAlt
@docs keyCapsLock, keyScrollLock, keyNumLock

-}


{-| ev\_keydown, ev\_keyup, ev\_mouse. `Mouse buttons dx dy`: `buttons` is a bit per button
(1 left, 2 right, 4 middle), `dx`/`dy` the movement since the last mouse event, in the
host's mouse units, positive right and forwards (up), as the C source's data2/data3.
-}
type Event
    = KeyDown Int
    | KeyUp Int
    | Mouse Int Int Int


{-| KEY\_RIGHTARROW.
-}
keyRightArrow : Int
keyRightArrow =
    0xAE


{-| KEY\_LEFTARROW.
-}
keyLeftArrow : Int
keyLeftArrow =
    0xAC


{-| KEY\_UPARROW.
-}
keyUpArrow : Int
keyUpArrow =
    0xAD


{-| KEY\_DOWNARROW.
-}
keyDownArrow : Int
keyDownArrow =
    0xAF


{-| KEY\_ESCAPE.
-}
keyEscape : Int
keyEscape =
    27


{-| KEY\_ENTER.
-}
keyEnter : Int
keyEnter =
    13


{-| KEY\_TAB.
-}
keyTab : Int
keyTab =
    9


{-| KEY\_F1.
-}
keyF1 : Int
keyF1 =
    187


{-| KEY\_F2.
-}
keyF2 : Int
keyF2 =
    188


{-| KEY\_F3.
-}
keyF3 : Int
keyF3 =
    189


{-| KEY\_F4.
-}
keyF4 : Int
keyF4 =
    190


{-| KEY\_F5.
-}
keyF5 : Int
keyF5 =
    191


{-| KEY\_F6.
-}
keyF6 : Int
keyF6 =
    192


{-| KEY\_F7.
-}
keyF7 : Int
keyF7 =
    193


{-| KEY\_F8.
-}
keyF8 : Int
keyF8 =
    194


{-| KEY\_F9.
-}
keyF9 : Int
keyF9 =
    195


{-| KEY\_F10.
-}
keyF10 : Int
keyF10 =
    196


{-| KEY\_F11.
-}
keyF11 : Int
keyF11 =
    215


{-| KEY\_F12.
-}
keyF12 : Int
keyF12 =
    216


{-| KEY\\\_BACKSPACE.
-}
keyBackspace : Int
keyBackspace =
    127


{-| KEY\\\_PAUSE.
-}
keyPause : Int
keyPause =
    255


{-| KEY\\\_EQUALS.
-}
keyEquals : Int
keyEquals =
    61


{-| KEY\\\_MINUS.
-}
keyMinus : Int
keyMinus =
    45


{-| KEY\\\_RSHIFT (shift, either side).
-}
keyRShift : Int
keyRShift =
    182


{-| KEY\\\_RCTRL (control, either side).
-}
keyRCtrl : Int
keyRCtrl =
    157


{-| KEY\\_RALT (alt, either side; KEY\\_LALT is the same).
-}
keyRAlt : Int
keyRAlt =
    184


{-| KEY\_CAPSLOCK (the DOS scan code 0x3a plus 0x80, as Chocolate Doom numbers it).
-}
keyCapsLock : Int
keyCapsLock =
    0xBA


{-| KEY\_SCRLCK.
-}
keyScrollLock : Int
keyScrollLock =
    0xC6


{-| KEY\_NUMLOCK.
-}
keyNumLock : Int
keyNumLock =
    0xC5
