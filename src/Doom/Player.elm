module Doom.Player exposing
    ( Player, PspDef, new
    , power, setPower, hasCard, setCard, ownsWeapon, setWeaponOwned
    , ammo, setAmmo, maxAmmoOf, setMaxAmmo, psprite, setPsprite
    )

{-| A player (player\_t, d\_player.h). The arrays indexed by enum in the C source are
`Array`s indexed by `Doom.Defs`' `...Index` functions; use the accessors here rather
than indexing them by hand.

@docs Player, PspDef, new
@docs power, setPower, hasCard, setCard, ownsWeapon, setWeaponOwned
@docs ammo, setAmmo, maxAmmoOf, setMaxAmmo, psprite, setPsprite

-}

import Array exposing (Array)
import Doom.Defs as Defs exposing (Ammo, Card, PlayerState(..), Power, Psp(..), Weapon(..))
import Doom.Fixed exposing (Fixed)
import Doom.Info.State exposing (State)
import Doom.Mobj exposing (ThinkerId)
import Doom.Ticcmd as Ticcmd exposing (Ticcmd)


{-| player\_t. `mo` is the player's body; `pendingweapon` is `Nothing` for
wp\_nochange; `attacker` the thing that last hurt them; `message` what the status line
should show. `attackdown`/`usedown` are booleans in effect.
-}
type alias Player =
    { mo : Maybe ThinkerId
    , playerstate : PlayerState
    , cmd : Ticcmd
    , viewz : Fixed
    , viewheight : Fixed
    , deltaviewheight : Fixed
    , bob : Fixed
    , health : Int
    , armorpoints : Int
    , armortype : Int
    , powers : Array Int
    , cards : Array Bool
    , backpack : Bool
    , frags : Array Int
    , readyweapon : Weapon
    , pendingweapon : Maybe Weapon
    , weaponowned : Array Bool
    , ammo : Array Int
    , maxammo : Array Int
    , attackdown : Bool
    , usedown : Bool
    , cheats : Int
    , refire : Int
    , killcount : Int
    , itemcount : Int
    , secretcount : Int
    , message : Maybe String
    , damagecount : Int
    , bonuscount : Int
    , attacker : Maybe ThinkerId
    , extralight : Int
    , fixedcolormap : Int
    , colormap : Int
    , weaponSprite : PspDef
    , flashSprite : PspDef
    , didsecret : Bool
    }


{-| pspdef\_t: a weapon sprite. `state` is `Nothing` when it isn't shown.
-}
type alias PspDef =
    { state : Maybe State
    , tics : Int
    , sx : Fixed
    , sy : Fixed
    }


{-| A player as `memset(p, 0, sizeof(*p))` leaves one: everything zero. G\_PlayerReborn
fills in the rest.
-}
new : Player
new =
    { mo = Nothing
    , playerstate = Live
    , cmd = Ticcmd.empty
    , viewz = 0
    , viewheight = 0
    , deltaviewheight = 0
    , bob = 0
    , health = 0
    , armorpoints = 0
    , armortype = 0
    , powers = Array.repeat 6 0
    , cards = Array.repeat 6 False
    , backpack = False
    , frags = Array.repeat Defs.maxPlayers 0
    , readyweapon = Fist
    , pendingweapon = Nothing
    , weaponowned = Array.repeat 9 False
    , ammo = Array.repeat 4 0
    , maxammo = Array.repeat 4 0
    , attackdown = False
    , usedown = False
    , cheats = 0
    , refire = 0
    , killcount = 0
    , itemcount = 0
    , secretcount = 0
    , message = Nothing
    , damagecount = 0
    , bonuscount = 0
    , attacker = Nothing
    , extralight = 0
    , fixedcolormap = 0
    , colormap = 0
    , weaponSprite = { state = Nothing, tics = 0, sx = 0, sy = 0 }
    , flashSprite = { state = Nothing, tics = 0, sx = 0, sy = 0 }
    , didsecret = False
    }


{-| `powers[p]`: tics left (or, for strength, tics since picked up).
-}
power : Power -> Player -> Int
power p player =
    Array.get (Defs.powerIndex p) player.powers |> Maybe.withDefault 0


{-| Set `powers[p]`.
-}
setPower : Power -> Int -> Player -> Player
setPower p n player =
    { player | powers = Array.set (Defs.powerIndex p) n player.powers }


{-| `cards[c]`.
-}
hasCard : Card -> Player -> Bool
hasCard c player =
    Array.get (Defs.cardIndex c) player.cards |> Maybe.withDefault False


{-| Set `cards[c]`.
-}
setCard : Card -> Bool -> Player -> Player
setCard c b player =
    { player | cards = Array.set (Defs.cardIndex c) b player.cards }


{-| `weaponowned[w]`.
-}
ownsWeapon : Weapon -> Player -> Bool
ownsWeapon w player =
    Array.get (Defs.weaponIndex w) player.weaponowned |> Maybe.withDefault False


{-| Set `weaponowned[w]`.
-}
setWeaponOwned : Weapon -> Bool -> Player -> Player
setWeaponOwned w b player =
    { player | weaponowned = Array.set (Defs.weaponIndex w) b player.weaponowned }


{-| `ammo[a]`.
-}
ammo : Ammo -> Player -> Int
ammo a player =
    Array.get (Defs.ammoIndex a) player.ammo |> Maybe.withDefault 0


{-| Set `ammo[a]`.
-}
setAmmo : Ammo -> Int -> Player -> Player
setAmmo a n player =
    { player | ammo = Array.set (Defs.ammoIndex a) n player.ammo }


{-| `maxammo[a]`.
-}
maxAmmoOf : Ammo -> Player -> Int
maxAmmoOf a player =
    Array.get (Defs.ammoIndex a) player.maxammo |> Maybe.withDefault 0


{-| Set `maxammo[a]`.
-}
setMaxAmmo : Ammo -> Int -> Player -> Player
setMaxAmmo a n player =
    { player | maxammo = Array.set (Defs.ammoIndex a) n player.maxammo }


{-| `psprites[psp]`.
-}
psprite : Psp -> Player -> PspDef
psprite psp player =
    case psp of
        WeaponPsp ->
            player.weaponSprite

        FlashPsp ->
            player.flashSprite


{-| Set `psprites[psp]`.
-}
setPsprite : Psp -> PspDef -> Player -> Player
setPsprite psp def player =
    case psp of
        WeaponPsp ->
            { player | weaponSprite = def }

        FlashPsp ->
            { player | flashSprite = def }
