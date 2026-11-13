module Doom.Play.Weapon exposing (setupPsprites, movePsprites, dropWeapon, bfgSpray, weaponReady, reFire, checkReload, lower, raise, gunFlash, punch, saw, fireMissile, fireBFG, firePlasma, firePistol, fireShotgun, fireShotgun2, fireCGun, light0, light1, light2, bfgSound, openShotgun2, loadShotgun2, closeShotgun2)

{-| The player's weapons (p\_pspr.c, and the super shotgun's reload actions from
p\_enemy.c): raising, lowering, firing, and the weapon action functions, which take
the player's number and which of their two weapon sprites the state is on.

@docs setupPsprites, movePsprites, dropWeapon, bfgSpray, weaponReady, reFire, checkReload, lower, raise, gunFlash, punch, saw, fireMissile, fireBFG, firePlasma, firePistol, fireShotgun, fireShotgun2, fireCGun, light0, light1, light2, bfgSound, openShotgun2, loadShotgun2, closeShotgun2

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (Psp)
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| P\_SetupPsprites player.
-}
setupPsprites : Env -> Int -> World -> World
setupPsprites =
    Debug.todo "Doom.Play.Weapon.setupPsprites"


{-| P\_MovePsprites player.
-}
movePsprites : Env -> Int -> World -> World
movePsprites =
    Debug.todo "Doom.Play.Weapon.movePsprites"


{-| P\_DropWeapon player.
-}
dropWeapon : Env -> Int -> World -> World
dropWeapon =
    Debug.todo "Doom.Play.Weapon.dropWeapon"


{-| A\_BFGSpray (an action of the BFG ball, not of a weapon sprite).
-}
bfgSpray : Env -> ThinkerId -> World -> World
bfgSpray =
    Debug.todo "Doom.Play.Weapon.bfgSpray"


{-| A\_WeaponReady.
-}
weaponReady : Env -> Int -> Psp -> World -> World
weaponReady =
    Debug.todo "Doom.Play.Weapon.weaponReady"


{-| A\_ReFire.
-}
reFire : Env -> Int -> Psp -> World -> World
reFire =
    Debug.todo "Doom.Play.Weapon.reFire"


{-| A\_CheckReload.
-}
checkReload : Env -> Int -> Psp -> World -> World
checkReload =
    Debug.todo "Doom.Play.Weapon.checkReload"


{-| A\_Lower.
-}
lower : Env -> Int -> Psp -> World -> World
lower =
    Debug.todo "Doom.Play.Weapon.lower"


{-| A\_Raise.
-}
raise : Env -> Int -> Psp -> World -> World
raise =
    Debug.todo "Doom.Play.Weapon.raise"


{-| A\_GunFlash.
-}
gunFlash : Env -> Int -> Psp -> World -> World
gunFlash =
    Debug.todo "Doom.Play.Weapon.gunFlash"


{-| A\_Punch.
-}
punch : Env -> Int -> Psp -> World -> World
punch =
    Debug.todo "Doom.Play.Weapon.punch"


{-| A\_Saw.
-}
saw : Env -> Int -> Psp -> World -> World
saw =
    Debug.todo "Doom.Play.Weapon.saw"


{-| A\_FireMissile.
-}
fireMissile : Env -> Int -> Psp -> World -> World
fireMissile =
    Debug.todo "Doom.Play.Weapon.fireMissile"


{-| A\_FireBFG.
-}
fireBFG : Env -> Int -> Psp -> World -> World
fireBFG =
    Debug.todo "Doom.Play.Weapon.fireBFG"


{-| A\_FirePlasma.
-}
firePlasma : Env -> Int -> Psp -> World -> World
firePlasma =
    Debug.todo "Doom.Play.Weapon.firePlasma"


{-| A\_FirePistol.
-}
firePistol : Env -> Int -> Psp -> World -> World
firePistol =
    Debug.todo "Doom.Play.Weapon.firePistol"


{-| A\_FireShotgun.
-}
fireShotgun : Env -> Int -> Psp -> World -> World
fireShotgun =
    Debug.todo "Doom.Play.Weapon.fireShotgun"


{-| A\_FireShotgun2.
-}
fireShotgun2 : Env -> Int -> Psp -> World -> World
fireShotgun2 =
    Debug.todo "Doom.Play.Weapon.fireShotgun2"


{-| A\_FireCGun.
-}
fireCGun : Env -> Int -> Psp -> World -> World
fireCGun =
    Debug.todo "Doom.Play.Weapon.fireCGun"


{-| A\_Light0.
-}
light0 : Env -> Int -> Psp -> World -> World
light0 =
    Debug.todo "Doom.Play.Weapon.light0"


{-| A\_Light1.
-}
light1 : Env -> Int -> Psp -> World -> World
light1 =
    Debug.todo "Doom.Play.Weapon.light1"


{-| A\_Light2.
-}
light2 : Env -> Int -> Psp -> World -> World
light2 =
    Debug.todo "Doom.Play.Weapon.light2"


{-| A\_BfgSound.
-}
bfgSound : Env -> Int -> Psp -> World -> World
bfgSound =
    Debug.todo "Doom.Play.Weapon.bfgSound"


{-| A\_OpenShotgun2.
-}
openShotgun2 : Env -> Int -> Psp -> World -> World
openShotgun2 =
    Debug.todo "Doom.Play.Weapon.openShotgun2"


{-| A\_LoadShotgun2.
-}
loadShotgun2 : Env -> Int -> Psp -> World -> World
loadShotgun2 =
    Debug.todo "Doom.Play.Weapon.loadShotgun2"


{-| A\_CloseShotgun2.
-}
closeShotgun2 : Env -> Int -> Psp -> World -> World
closeShotgun2 =
    Debug.todo "Doom.Play.Weapon.closeShotgun2"
