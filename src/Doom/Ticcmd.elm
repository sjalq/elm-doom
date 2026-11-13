module Doom.Ticcmd exposing
    ( Ticcmd, empty
    , btAttack, btUse, btSpecial, btSpecialMask, btChange, btWeaponMask, btWeaponShift
    , btsPause, btsSaveGame, btsSaveMask, btsSaveShift
    )

{-| What a player does in one tic (ticcmd\_t, d\_ticcmd.h): the input the game runs on,
recorded in demos.

@docs Ticcmd, empty
@docs btAttack, btUse, btSpecial, btSpecialMask, btChange, btWeaponMask, btWeaponShift
@docs btsPause, btsSaveGame, btsSaveMask, btsSaveShift

-}


{-| `forwardmove` and `sidemove` are signed chars (-128 to 127), `angleturn` a short
whose high byte is what turns (in BAM >> 16), `buttons` the BT\_ bits.
-}
type alias Ticcmd =
    { forwardmove : Int
    , sidemove : Int
    , angleturn : Int
    , consistancy : Int
    , chatchar : Int
    , buttons : Int
    }


{-| Doing nothing.
-}
empty : Ticcmd
empty =
    { forwardmove = 0, sidemove = 0, angleturn = 0, consistancy = 0, chatchar = 0, buttons = 0 }


{-| BT\_ATTACK.
-}
btAttack : Int
btAttack =
    1


{-| BT\_USE.
-}
btUse : Int
btUse =
    2


{-| BT\_SPECIAL: the other bits are a special command (pause, save).
-}
btSpecial : Int
btSpecial =
    128


{-| BT\_SPECIALMASK.
-}
btSpecialMask : Int
btSpecialMask =
    3


{-| BT\_CHANGE: change to the weapon in BT\_WEAPONMASK.
-}
btChange : Int
btChange =
    4


{-| BT\_WEAPONMASK.
-}
btWeaponMask : Int
btWeaponMask =
    8 + 16 + 32


{-| BT\_WEAPONSHIFT.
-}
btWeaponShift : Int
btWeaponShift =
    3


{-| BTS\_PAUSE.
-}
btsPause : Int
btsPause =
    1


{-| BTS\_SAVEGAME.
-}
btsSaveGame : Int
btsSaveGame =
    2


{-| BTS\_SAVEMASK.
-}
btsSaveMask : Int
btsSaveMask =
    4 + 8 + 16


{-| BTS\_SAVESHIFT.
-}
btsSaveShift : Int
btsSaveShift =
    2
