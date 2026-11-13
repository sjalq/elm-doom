module Doom.Play.WeaponChoice exposing (shotCost, hasAmmoForShot, fallbackWeapon, requestedWeapon)

{-| The rules for which weapon a player has up: whether they have the ammo to fire
the one they hold, which one they fall back to when they don't (P\_CheckAmmo's
preferences, p\_pspr.c), and which one a weapon change button asks for (P\_PlayerThink,
p\_user.c). They only read the player, so they are kept apart from the code that
changes the world.

@docs shotCost, hasAmmoForShot, fallbackWeapon, requestedWeapon

-}

import Bitwise
import Doom.Defs as Defs exposing (Ammo(..), GameMode(..), Power(..), Weapon(..))
import Doom.Player as Player exposing (Player)
import Doom.Ticcmd as Ticcmd


{-| The ammo one shot takes: 40 cells for the BFG (BFGCELLS), two shells for the
super shotgun, one of anything else.
-}
shotCost : Weapon -> Int
shotCost weapon =
    case weapon of
        Bfg ->
            40

        SuperShotgun ->
            2

        _ ->
            1


{-| Whether the player can fire the weapon they hold: weapons without ammo always
can (the first half of P\_CheckAmmo).
-}
hasAmmoForShot : Player -> Bool
hasAmmoForShot player =
    case (Defs.weaponInfo player.readyweapon).ammo of
        Nothing ->
            True

        Just ammo ->
            Player.ammo ammo player >= shotCost player.readyweapon


{-| The weapon P\_CheckAmmo switches to when the player is out of ammo, in its order
of preference: plasma rifle (not in shareware), super shotgun (only in Doom II, and
with more than two shells), chaingun, shotgun, pistol, chainsaw, rocket launcher,
BFG (not in shareware, and with more than 40 cells), and the fist when all else
fails. The player needn't own the pistol: any bullets will do.
-}
fallbackWeapon : GameMode -> Player -> Weapon
fallbackWeapon gameMode player =
    let
        owns w =
            Player.ownsWeapon w player

        ammo a =
            Player.ammo a player
    in
    if owns Plasma && ammo Cell /= 0 && gameMode /= Shareware then
        Plasma

    else if owns SuperShotgun && ammo Shell > 2 && gameMode == Commercial then
        SuperShotgun

    else if owns Chaingun && ammo Clip /= 0 then
        Chaingun

    else if owns Shotgun && ammo Shell /= 0 then
        Shotgun

    else if ammo Clip /= 0 then
        Pistol

    else if owns Chainsaw then
        Chainsaw

    else if owns Missile && ammo Misl /= 0 then
        Missile

    else if owns Bfg && ammo Cell > 40 && gameMode /= Shareware then
        Bfg

    else
        Fist


{-| The weapon the player's ticcmd asks to change to (BT\_CHANGE), as P\_PlayerThink
works it out, or `Nothing` when there is no change to make:

  - asking for the fist gives the chainsaw if they own it, unless they are holding
    the chainsaw with berserk on;
  - in Doom II, asking for the shotgun gives the super shotgun if they own it and
    aren't already holding it;
  - they must own the weapon and not be holding it already;
  - shareware never changes to the plasma rifle or BFG, even when cheated.

-}
requestedWeapon : GameMode -> Player -> Maybe Weapon
requestedWeapon gameMode player =
    let
        buttons =
            player.cmd.buttons
    in
    if Bitwise.and buttons Ticcmd.btChange == 0 then
        Nothing

    else
        Defs.weaponFromIndex
            (Bitwise.shiftRightBy Ticcmd.btWeaponShift (Bitwise.and buttons Ticcmd.btWeaponMask))
            |> Maybe.map (substitute gameMode player)
            |> Maybe.andThen (allowed gameMode player)


substitute : GameMode -> Player -> Weapon -> Weapon
substitute gameMode player asked =
    let
        fistOrSaw =
            if
                asked
                    == Fist
                    && Player.ownsWeapon Chainsaw player
                    && not (player.readyweapon == Chainsaw && Player.power Strength player /= 0)
            then
                Chainsaw

            else
                asked
    in
    if
        gameMode
            == Commercial
            && fistOrSaw
            == Shotgun
            && Player.ownsWeapon SuperShotgun player
            && player.readyweapon
            /= SuperShotgun
    then
        SuperShotgun

    else
        fistOrSaw


allowed : GameMode -> Player -> Weapon -> Maybe Weapon
allowed gameMode player weapon =
    if
        Player.ownsWeapon weapon player
            && weapon
            /= player.readyweapon
            && ((weapon /= Plasma && weapon /= Bfg) || gameMode /= Shareware)
    then
        Just weapon

    else
        Nothing
