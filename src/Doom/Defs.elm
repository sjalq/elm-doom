module Doom.Defs exposing
    ( GameMode(..), Skill(..), skillIndex, skillFromIndex
    , Weapon(..), weaponIndex, weaponFromIndex, allWeapons, WeaponInfo, weaponInfo
    , Ammo(..), ammoIndex, allAmmo, maxAmmo, clipAmmo
    , Power(..), powerIndex, Card(..), cardIndex
    , PlayerState(..), Psp(..)
    , ticrate, maxPlayers, maxHealth, viewHeight, playerRadius, maxRadius, gravity, maxMove
    , useRange, meleeRange, missileRange, floatSpeed, baseThreshold, onFloorZ, onCeilingZ
    , mapBlockShift, mapBlockSize, mapBtoFrac, invulnTics, invisTics, infraTics, ironTics
    , cfNoclip, cfGodmode, cfNomomentum, ffFullbright, ffFrameMask
    )

{-| The game's enums and constants (doomdef.h, p\_local.h, d\_items.c, d\_player.h).

Enums the game indexes arrays by (weapons, ammo, powers, cards) have an `...Index`
function giving their position in the C enum, which is also their position in the
player's arrays.


# Game

@docs GameMode, Skill, skillIndex, skillFromIndex


# Weapons and ammo

@docs Weapon, weaponIndex, weaponFromIndex, allWeapons, WeaponInfo, weaponInfo
@docs Ammo, ammoIndex, allAmmo, maxAmmo, clipAmmo


# Players

@docs Power, powerIndex, Card, cardIndex
@docs PlayerState, Psp


# Constants

@docs ticrate, maxPlayers, maxHealth, viewHeight, playerRadius, maxRadius, gravity, maxMove
@docs useRange, meleeRange, missileRange, floatSpeed, baseThreshold, onFloorZ, onCeilingZ
@docs mapBlockShift, mapBlockSize, mapBtoFrac, invulnTics, invisTics, infraTics, ironTics
@docs cfNoclip, cfGodmode, cfNomomentum, ffFullbright, ffFrameMask

-}

import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.State as State exposing (State)



-- GAME


{-| Which game the WAD is (GameMode\_t). Freedoom: Phase 1 plays as `Retail`, Phase 2 as
`Commercial`.
-}
type GameMode
    = Shareware
    | Registered
    | Commercial
    | Retail


{-| Skill level (skill\_t).
-}
type Skill
    = Baby
    | Easy
    | Medium
    | Hard
    | Nightmare


{-| Its position in skill\_t (sk\_baby = 0).
-}
skillIndex : Skill -> Int
skillIndex skill =
    case skill of
        Baby ->
            0

        Easy ->
            1

        Medium ->
            2

        Hard ->
            3

        Nightmare ->
            4


{-| The skill at a position in skill\_t, if any.
-}
skillFromIndex : Int -> Maybe Skill
skillFromIndex i =
    case i of
        0 ->
            Just Baby

        1 ->
            Just Easy

        2 ->
            Just Medium

        3 ->
            Just Hard

        4 ->
            Just Nightmare

        _ ->
            Nothing



-- WEAPONS


{-| weapontype\_t. C's `wp_nochange` is `Nothing` where a `Maybe Weapon` is expected.
-}
type Weapon
    = Fist
    | Pistol
    | Shotgun
    | Chaingun
    | Missile
    | Plasma
    | Bfg
    | Chainsaw
    | SuperShotgun


{-| Its position in weapontype\_t.
-}
weaponIndex : Weapon -> Int
weaponIndex w =
    case w of
        Fist ->
            0

        Pistol ->
            1

        Shotgun ->
            2

        Chaingun ->
            3

        Missile ->
            4

        Plasma ->
            5

        Bfg ->
            6

        Chainsaw ->
            7

        SuperShotgun ->
            8


{-| Every weapon, in weapontype\_t order.
-}
allWeapons : List Weapon
allWeapons =
    [ Fist, Pistol, Shotgun, Chaingun, Missile, Plasma, Bfg, Chainsaw, SuperShotgun ]


{-| The weapon at a position in weapontype\_t, if any.
-}
weaponFromIndex : Int -> Maybe Weapon
weaponFromIndex i =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i allWeapons)


{-| A row of weaponinfo[] (d\_items.c): the ammo it uses (`Nothing` for am\_noammo) and
its states.
-}
type alias WeaponInfo =
    { ammo : Maybe Ammo
    , upstate : State
    , downstate : State
    , readystate : State
    , atkstate : State
    , flashstate : State
    }


{-| weaponinfo[].
-}
weaponInfo : Weapon -> WeaponInfo
weaponInfo w =
    case w of
        Fist ->
            WeaponInfo Nothing State.Punchup State.Punchdown State.Punch State.Punch1 State.Null

        Pistol ->
            WeaponInfo (Just Clip) State.Pistolup State.Pistoldown State.Pistol State.Pistol1 State.Pistolflash

        Shotgun ->
            WeaponInfo (Just Shell) State.Sgunup State.Sgundown State.Sgun State.Sgun1 State.Sgunflash1

        Chaingun ->
            WeaponInfo (Just Clip) State.Chainup State.Chaindown State.Chain State.Chain1 State.Chainflash1

        Missile ->
            WeaponInfo (Just Misl) State.Missileup State.Missiledown State.Missile State.Missile1 State.Missileflash1

        Plasma ->
            WeaponInfo (Just Cell) State.Plasmaup State.Plasmadown State.Plasma State.Plasma1 State.Plasmaflash1

        Bfg ->
            WeaponInfo (Just Cell) State.Bfgup State.Bfgdown State.Bfg State.Bfg1 State.Bfgflash1

        Chainsaw ->
            WeaponInfo Nothing State.Sawup State.Sawdown State.Saw State.Saw1 State.Null

        SuperShotgun ->
            WeaponInfo (Just Shell) State.Dsgunup State.Dsgundown State.Dsgun State.Dsgun1 State.Dsgunflash1



-- AMMO


{-| ammotype\_t. C's `am_noammo` is `Nothing` where a `Maybe Ammo` is expected.
-}
type Ammo
    = Clip
    | Shell
    | Cell
    | Misl


{-| Its position in ammotype\_t.
-}
ammoIndex : Ammo -> Int
ammoIndex a =
    case a of
        Clip ->
            0

        Shell ->
            1

        Cell ->
            2

        Misl ->
            3


{-| Every ammo type, in ammotype\_t order.
-}
allAmmo : List Ammo
allAmmo =
    [ Clip, Shell, Cell, Misl ]


{-| maxammo[] (p\_inter.c): how much a player can carry without a backpack.
-}
maxAmmo : Ammo -> Int
maxAmmo a =
    case a of
        Clip ->
            200

        Shell ->
            50

        Cell ->
            300

        Misl ->
            50


{-| clipammo[] (p\_inter.c): how much one clip of each holds.
-}
clipAmmo : Ammo -> Int
clipAmmo a =
    case a of
        Clip ->
            10

        Shell ->
            4

        Cell ->
            20

        Misl ->
            1



-- PLAYERS


{-| powertype\_t.
-}
type Power
    = Invulnerability
    | Strength
    | Invisibility
    | IronFeet
    | AllMap
    | Infrared


{-| Its position in powertype\_t.
-}
powerIndex : Power -> Int
powerIndex p =
    case p of
        Invulnerability ->
            0

        Strength ->
            1

        Invisibility ->
            2

        IronFeet ->
            3

        AllMap ->
            4

        Infrared ->
            5


{-| card\_t.
-}
type Card
    = BlueCard
    | YellowCard
    | RedCard
    | BlueSkull
    | YellowSkull
    | RedSkull


{-| Its position in card\_t.
-}
cardIndex : Card -> Int
cardIndex c =
    case c of
        BlueCard ->
            0

        YellowCard ->
            1

        RedCard ->
            2

        BlueSkull ->
            3

        YellowSkull ->
            4

        RedSkull ->
            5


{-| playerstate\_t.
-}
type PlayerState
    = Live
    | Dead
    | Reborn


{-| psprnum\_t: the player's two weapon sprites.
-}
type Psp
    = WeaponPsp
    | FlashPsp



-- CONSTANTS


{-| TICRATE: game tics per second.
-}
ticrate : Int
ticrate =
    35


{-| MAXPLAYERS.
-}
maxPlayers : Int
maxPlayers =
    4


{-| MAXHEALTH.
-}
maxHealth : Int
maxHealth =
    100


{-| VIEWHEIGHT.
-}
viewHeight : Fixed
viewHeight =
    41 * Fixed.fracunit


{-| PLAYERRADIUS.
-}
playerRadius : Fixed
playerRadius =
    16 * Fixed.fracunit


{-| MAXRADIUS: the biggest radius a thing has, for blockmap searches.
-}
maxRadius : Fixed
maxRadius =
    32 * Fixed.fracunit


{-| GRAVITY.
-}
gravity : Fixed
gravity =
    Fixed.fracunit


{-| MAXMOVE: the fastest a thing moves per tic.
-}
maxMove : Fixed
maxMove =
    30 * Fixed.fracunit


{-| USERANGE.
-}
useRange : Fixed
useRange =
    64 * Fixed.fracunit


{-| MELEERANGE.
-}
meleeRange : Fixed
meleeRange =
    64 * Fixed.fracunit


{-| MISSILERANGE.
-}
missileRange : Fixed
missileRange =
    32 * 64 * Fixed.fracunit


{-| FLOATSPEED.
-}
floatSpeed : Fixed
floatSpeed =
    4 * Fixed.fracunit


{-| BASETHRESHOLD: tics a monster keeps chasing whoever hurt it.
-}
baseThreshold : Int
baseThreshold =
    100


{-| ONFLOORZ (MININT): spawn the thing on the floor.
-}
onFloorZ : Fixed
onFloorZ =
    Fixed.minInt


{-| ONCEILINGZ (MAXINT): spawn the thing hanging from the ceiling.
-}
onCeilingZ : Fixed
onCeilingZ =
    Fixed.maxInt


{-| MAPBLOCKSHIFT: fixed-point coordinates to blockmap cells (128 units each).
-}
mapBlockShift : Int
mapBlockShift =
    Fixed.fracbits + 7


{-| MAPBLOCKSIZE.
-}
mapBlockSize : Fixed
mapBlockSize =
    128 * Fixed.fracunit


{-| MAPBTOFRAC.
-}
mapBtoFrac : Int
mapBtoFrac =
    mapBlockShift - Fixed.fracbits


{-| INVULNTICS.
-}
invulnTics : Int
invulnTics =
    30 * ticrate


{-| INVISTICS.
-}
invisTics : Int
invisTics =
    60 * ticrate


{-| INFRATICS.
-}
infraTics : Int
infraTics =
    120 * ticrate


{-| IRONTICS.
-}
ironTics : Int
ironTics =
    60 * ticrate


{-| CF\_NOCLIP: the player's `cheats` bit for walking through walls.
-}
cfNoclip : Int
cfNoclip =
    1


{-| CF\_GODMODE.
-}
cfGodmode : Int
cfGodmode =
    2


{-| CF\_NOMOMENTUM.
-}
cfNomomentum : Int
cfNomomentum =
    4


{-| FF\_FULLBRIGHT: a state frame's bit for drawing at full brightness.
-}
ffFullbright : Int
ffFullbright =
    0x8000


{-| FF\_FRAMEMASK: a state frame's frame number.
-}
ffFrameMask : Int
ffFrameMask =
    0x7FFF
