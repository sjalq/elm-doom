module Doom.Play.Weapon exposing (setupPsprites, movePsprites, dropWeapon, bfgSpray, weaponReady, reFire, checkReload, lower, raise, gunFlash, punch, saw, fireMissile, fireBFG, firePlasma, firePistol, fireShotgun, fireShotgun2, fireCGun, light0, light1, light2, bfgSound, openShotgun2, loadShotgun2, closeShotgun2, calcSwing)

{-| The player's weapons (p\_pspr.c, and the super shotgun's reload actions from
p\_enemy.c): raising, lowering, firing, and the weapon action functions, which take
the player's number and which of their two weapon sprites the state is on.

The weapon sprite moves between WEAPONBOTTOM (off the bottom of the screen) and
WEAPONTOP; while it is ready it bobs with the player's walk. Which weapon to fall
back on when out of ammo is `Doom.Play.WeaponChoice`.

@docs setupPsprites, movePsprites, dropWeapon, bfgSpray, weaponReady, reFire, checkReload, lower, raise, gunFlash, punch, saw, fireMissile, fireBFG, firePlasma, firePistol, fireShotgun, fireShotgun2, fireCGun, light0, light1, light2, bfgSound, openShotgun2, loadShotgun2, closeShotgun2, calcSwing

-}

import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (PlayerState(..), Power(..), Psp(..), Weapon(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Info.State as State exposing (State)
import Doom.Int32 as Int32
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Enemy as Enemy
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Inter as Inter
import Doom.Play.Map as Map
import Doom.Play.Mobj
import Doom.Play.Movement as Movement
import Doom.Play.WeaponChoice as WeaponChoice
import Doom.Player as Player exposing (Player, PspDef)
import Doom.Sound as Sound
import Doom.Tables as Tables
import Doom.Ticcmd as Ticcmd
import Doom.World as World exposing (World)



-- CONSTANTS


{-| LOWERSPEED.
-}
lowerSpeed : Fixed
lowerSpeed =
    6 * Fixed.fracunit


{-| RAISESPEED.
-}
raiseSpeed : Fixed
raiseSpeed =
    6 * Fixed.fracunit


{-| WEAPONBOTTOM.
-}
weaponBottom : Fixed
weaponBottom =
    128 * Fixed.fracunit


{-| WEAPONTOP.
-}
weaponTop : Fixed
weaponTop =
    32 * Fixed.fracunit


{-| BFGCELLS: the cells one BFG shot takes.
-}
bfgCells : Int
bfgCells =
    40


{-| How far the hitscan weapons and the BFG's tracers look for a target to aim at.
-}
aimRange : Fixed
aimRange =
    16 * 64 * Fixed.fracunit



-- SETTING UP AND MOVING THE SPRITES


{-| P\_SetupPsprites: at the start of a level, take both weapon sprites away and
bring the player's weapon up.
-}
setupPsprites : Env -> Int -> World -> World
setupPsprites env n world =
    world
        |> World.updatePlayer n
            (\p ->
                { p
                    | weaponSprite = hide p.weaponSprite
                    , flashSprite = hide p.flashSprite
                    , pendingweapon = Just p.readyweapon
                }
            )
        |> bringUpWeapon env n


hide : PspDef -> PspDef
hide psp =
    { psp | state = Nothing }


{-| P\_MovePsprites: each tic, count down each shown weapon sprite's tics (a -1 tic
count never runs out) and move it to its next state when they run out. Then the
flash follows the weapon.
-}
movePsprites : Env -> Int -> World -> World
movePsprites env n world =
    world
        |> tickPsprite env n WeaponPsp
        |> tickPsprite env n FlashPsp
        |> World.updatePlayer n
            (\p ->
                let
                    flash =
                        p.flashSprite
                in
                { p | flashSprite = { flash | sx = p.weaponSprite.sx, sy = p.weaponSprite.sy } }
            )


tickPsprite : Env -> Int -> Psp -> World -> World
tickPsprite env n which =
    onPlayer n
        (\p world ->
            let
                psp =
                    Player.psprite which p
            in
            case psp.state of
                Just state ->
                    if psp.tics /= -1 then
                        let
                            tics =
                                psp.tics - 1

                            ticked =
                                updatePsprite n which (\s -> { s | tics = tics }) world
                        in
                        if tics == 0 then
                            Env.setPsprite env n which (Env.stateDef env state).next ticked

                        else
                            ticked

                    else
                        world

                Nothing ->
                    world
        )


{-| P\_CalcSwing: the weapon's sideways and up-and-down swing for the player's bob,
left in the scratch's `swingx` and `swingy`. The C source never calls it.
-}
calcSwing : Int -> World -> World
calcSwing n =
    onPlayer n
        (\p world ->
            let
                swingx =
                    Fixed.mul p.bob (Tables.finesine (modBy Angle.fineangles (117 * world.leveltime)))

                swingy =
                    Int32.neg
                        (Fixed.mul swingx
                            (Tables.finesine (modBy Angle.fineangles (117 * world.leveltime + Angle.fineangles // 2)))
                        )
            in
            World.setScratch (\s -> { s | swingx = swingx, swingy = swingy }) world
        )



-- RAISING, LOWERING, AMMO


{-| P\_BringUpWeapon: start raising the pending weapon (or, if none, the one held)
from the bottom of the screen. The chainsaw revs as it comes up.
-}
bringUpWeapon : Env -> Int -> World -> World
bringUpWeapon env n =
    onPlayer n
        (\p world ->
            let
                pending =
                    Maybe.withDefault p.readyweapon p.pendingweapon

                revved =
                    if pending == Chainsaw then
                        playerSound n Sfx.Sawup world

                    else
                        world
            in
            revved
                |> World.updatePlayer n (\pl -> { pl | pendingweapon = Nothing })
                |> updatePsprite n WeaponPsp (\s -> { s | sy = weaponBottom })
                |> Env.setPsprite env n WeaponPsp (Defs.weaponInfo pending).upstate
        )


{-| P\_CheckAmmo: True if the player has the ammo to fire the weapon they hold. If
not, pick the weapon to change to (`WeaponChoice.fallbackWeapon`) and start lowering
the one held.
-}
checkAmmo : Env -> Int -> World -> ( Bool, World )
checkAmmo env n world =
    case World.player n world of
        Just p ->
            if WeaponChoice.hasAmmoForShot p then
                ( True, world )

            else
                ( False
                , world
                    |> World.updatePlayer n
                        (\pl -> { pl | pendingweapon = Just (WeaponChoice.fallbackWeapon env.params.gameMode pl) })
                    |> Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).downstate
                )

        Nothing ->
            ( False, world )


{-| P\_FireWeapon: if there's the ammo, the player's body goes into its attack pose,
the weapon into its attack states, and monsters hear the shot.
-}
fireWeapon : Env -> Int -> World -> World
fireWeapon env n world =
    case checkAmmo env n world of
        ( False, unfired ) ->
            unfired

        ( True, armed ) ->
            armed
                |> onPlayerMo n
                    (\_ mo w ->
                        w
                            |> setMobjState env mo.id State.PlayAtk1
                            |> onPlayer n
                                (\p -> Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).atkstate)
                            |> Enemy.noiseAlert env mo.id mo.id
                    )


{-| P\_DropWeapon: the player died, so put the weapon away.
-}
dropWeapon : Env -> Int -> World -> World
dropWeapon env n =
    onPlayer n (\p -> Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).downstate)


{-| A\_WeaponReady: the weapon is up and can be fired or changed. Ends the body's
attack pose; the chainsaw idles; a pending change (or death) lowers the weapon; the
attack button fires (the rocket launcher and BFG only once per press); otherwise the
weapon bobs with the player's walk.
-}
weaponReady : Env -> Int -> Psp -> World -> World
weaponReady env n which world =
    world
        |> onPlayerMo n
            (\_ mo w ->
                if mo.state == State.PlayAtk1 || mo.state == State.PlayAtk2 then
                    setMobjState env mo.id State.Play w

                else
                    w
            )
        |> onPlayer n
            (\p w ->
                if p.readyweapon == Chainsaw && (Player.psprite which p).state == Just State.Saw then
                    playerSound n Sfx.Sawidl w

                else
                    w
            )
        |> onPlayer n
            (\p w ->
                if p.pendingweapon /= Nothing || p.health == 0 then
                    Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).downstate w

                else if Bitwise.and p.cmd.buttons Ticcmd.btAttack /= 0 then
                    if not p.attackdown || (p.readyweapon /= Missile && p.readyweapon /= Bfg) then
                        w
                            |> World.updatePlayer n (\pl -> { pl | attackdown = True })
                            |> fireWeapon env n

                    else
                        bobWeapon n which w

                else
                    w
                        |> World.updatePlayer n (\pl -> { pl | attackdown = False })
                        |> bobWeapon n which
            )


{-| The end of A\_WeaponReady: swing the weapon sprite with the player's bob.
-}
bobWeapon : Int -> Psp -> World -> World
bobWeapon n which =
    onPlayer n
        (\p world ->
            let
                angle =
                    modBy Angle.fineangles (128 * world.leveltime)

                halfAngle =
                    Bitwise.and angle (Angle.fineangles // 2 - 1)
            in
            updatePsprite n
                which
                (\s ->
                    { s
                        | sx = Fixed.add Fixed.fracunit (Fixed.mul p.bob (Tables.finecosine angle))
                        , sy = Fixed.add weaponTop (Fixed.mul p.bob (Tables.finesine halfAngle))
                    }
                )
                world
        )


{-| A\_ReFire: fire again without lowering the weapon if the button is still held
(and no weapon change is pending and the player is alive); otherwise check the
ammo, which may start a change.
-}
reFire : Env -> Int -> Psp -> World -> World
reFire env n _ =
    onPlayer n
        (\p world ->
            if Bitwise.and p.cmd.buttons Ticcmd.btAttack /= 0 && p.pendingweapon == Nothing && p.health /= 0 then
                world
                    |> World.updatePlayer n (\pl -> { pl | refire = pl.refire + 1 })
                    |> fireWeapon env n

            else
                world
                    |> World.updatePlayer n (\pl -> { pl | refire = 0 })
                    |> checkAmmo env n
                    |> Tuple.second
        )


{-| A\_CheckReload: change weapon if out of ammo.
-}
checkReload : Env -> Int -> Psp -> World -> World
checkReload env n _ world =
    Tuple.second (checkAmmo env n world)


{-| A\_Lower: move the weapon down; at the bottom, keep it there if the player is
dead, take it away if they have no health, or else change to the pending weapon and
start raising it.
-}
lower : Env -> Int -> Psp -> World -> World
lower env n which =
    onPlayer n
        (\p world ->
            let
                sy =
                    Fixed.add (Player.psprite which p).sy lowerSpeed

                lowered =
                    updatePsprite n which (\s -> { s | sy = sy }) world
            in
            if sy < weaponBottom then
                lowered

            else if p.playerstate == Dead then
                updatePsprite n which (\s -> { s | sy = weaponBottom }) lowered

            else if p.health == 0 then
                Env.setPsprite env n WeaponPsp State.Null lowered

            else
                -- The pending weapon is always set by the time the weapon is lowered
                -- for a change; were it wp_nochange, C would index past weaponinfo[].
                lowered
                    |> World.updatePlayer n
                        (\pl -> { pl | readyweapon = Maybe.withDefault pl.readyweapon pl.pendingweapon })
                    |> bringUpWeapon env n
        )


{-| A\_Raise: move the weapon up; at the top, it is ready.
-}
raise : Env -> Int -> Psp -> World -> World
raise env n which =
    onPlayer n
        (\p world ->
            let
                sy =
                    Fixed.sub (Player.psprite which p).sy raiseSpeed
            in
            if sy > weaponTop then
                updatePsprite n which (\s -> { s | sy = sy }) world

            else
                world
                    |> updatePsprite n which (\s -> { s | sy = weaponTop })
                    |> Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).readystate
        )


{-| A\_GunFlash: the body's firing pose and the weapon's muzzle flash.
-}
gunFlash : Env -> Int -> Psp -> World -> World
gunFlash env n _ world =
    world
        |> onPlayerMo n (\_ mo -> setMobjState env mo.id State.PlayAtk2)
        |> showFlash env n



-- ATTACKS


{-| A\_Punch: a punch at whatever is in front, ten times as hard with berserk; the
player turns to face what they hit.
-}
punch : Env -> Int -> Psp -> World -> World
punch env n _ =
    onPlayerMo n
        (\p mo world ->
            let
                ( r, w1 ) =
                    World.pRandom world

                base =
                    (remainderBy 10 r + 1) * 2

                damage =
                    if Player.power Strength p /= 0 then
                        base * 10

                    else
                        base

                ( jitter, w2 ) =
                    randomSpread 18 w1

                angle =
                    Angle.add mo.angle jitter

                ( slope, w3 ) =
                    Map.aimLineAttack env mo.id angle Defs.meleeRange w2

                w4 =
                    Map.lineAttack env mo.id angle Defs.meleeRange slope damage w3
            in
            case lineTarget w4 of
                Just target ->
                    w4
                        |> playerSound n Sfx.Punch
                        |> World.updateMobj mo.id (\m -> { m | angle = Geometry.pointToAngle2 m.x m.y target.x target.y })

                Nothing ->
                    w4
        )


{-| A\_Saw: the chainsaw bites whatever is just in front (one unit further than a
punch reaches, so the puff doesn't skip the flash), and pulls the player round
towards what it hit, a little at a time.
-}
saw : Env -> Int -> Psp -> World -> World
saw env n _ =
    onPlayerMo n
        (\_ mo world ->
            let
                ( r, w1 ) =
                    World.pRandom world

                damage =
                    2 * (remainderBy 10 r + 1)

                ( jitter, w2 ) =
                    randomSpread 18 w1

                angle =
                    Angle.add mo.angle jitter

                range =
                    Defs.meleeRange + 1

                ( slope, w3 ) =
                    Map.aimLineAttack env mo.id angle range w2

                w4 =
                    Map.lineAttack env mo.id angle range slope damage w3
            in
            case lineTarget w4 of
                Nothing ->
                    playerSound n Sfx.Sawful w4

                Just target ->
                    w4
                        |> playerSound n Sfx.Sawhit
                        |> World.updateMobj mo.id
                            (\m ->
                                { m
                                    | angle = turnTowards (Geometry.pointToAngle2 m.x m.y target.x target.y) m.angle
                                    , flags = Flags.set Flags.mfJustAttacked m.flags
                                }
                            )
        )


{-| A\_Saw's turn: towards the target by ANG90/20 a tic, or to just short of it
(ANG90/21) when it is nearly in front.
-}
turnTowards : Angle -> Angle -> Angle
turnTowards target facing =
    let
        delta =
            Angle.sub target facing

        step =
            Angle.ang90 // 20

        stop =
            Angle.ang90 // 21
    in
    if delta > Angle.ang180 then
        if delta < Angle.neg step then
            Angle.add target stop

        else
            Angle.sub facing step

    else if delta > step then
        Angle.sub target stop

    else
        Angle.add facing step


{-| A\_FireMissile.
-}
fireMissile : Env -> Int -> Psp -> World -> World
fireMissile env n _ world =
    world
        |> useAmmo n 1
        |> onPlayerMo n (\_ mo -> Movement.spawnPlayerMissile env mo.id MobjType.Rocket)


{-| A\_FireBFG.
-}
fireBFG : Env -> Int -> Psp -> World -> World
fireBFG env n _ world =
    world
        |> useAmmo n bfgCells
        |> onPlayerMo n (\_ mo -> Movement.spawnPlayerMissile env mo.id MobjType.Bfg)


{-| A\_FirePlasma: one of the plasma rifle's two flashes, at random.
-}
firePlasma : Env -> Int -> Psp -> World -> World
firePlasma env n _ world =
    world
        |> useAmmo n 1
        |> onPlayer n
            (\p w ->
                let
                    ( r, w1 ) =
                        World.pRandom w
                in
                Env.setPsprite env n FlashPsp (stateOffset (Defs.weaponInfo p.readyweapon).flashstate (Bitwise.and r 1)) w1
            )
        |> onPlayerMo n (\_ mo -> Movement.spawnPlayerMissile env mo.id MobjType.Plasma)


{-| P\_BulletSlope: aim straight ahead, or failing that a little to the left, or
failing that a little to the right, so a near miss is still about the height of
the target; the slope goes in the scratch's `bulletslope`.
-}
bulletSlope : Env -> ThinkerId -> World -> World
bulletSlope env moId world =
    let
        aim angle w =
            let
                ( slope, aimed ) =
                    Map.aimLineAttack env moId angle aimRange w
            in
            World.setScratch (\s -> { s | bulletslope = slope }) aimed
    in
    case World.mobj moId world of
        Just mo ->
            let
                ahead =
                    aim mo.angle world

                left =
                    Angle.add mo.angle (2 ^ 26)
            in
            if ahead.scratch.linetarget /= Nothing then
                ahead

            else
                let
                    leftAimed =
                        aim left ahead
                in
                if leftAimed.scratch.linetarget /= Nothing then
                    leftAimed

                else
                    aim (Angle.sub left (2 ^ 27)) leftAimed

        Nothing ->
            world


{-| P\_GunShot: one bullet along `bulletslope`, spread sideways unless accurate.
-}
gunShot : Env -> ThinkerId -> Bool -> World -> World
gunShot env moId accurate world =
    let
        ( r, w1 ) =
            World.pRandom world

        damage =
            5 * (remainderBy 3 r + 1)
    in
    case World.mobj moId w1 of
        Just mo ->
            let
                ( angle, w2 ) =
                    if accurate then
                        ( mo.angle, w1 )

                    else
                        randomSpread 18 w1 |> Tuple.mapFirst (Angle.add mo.angle)
            in
            Map.lineAttack env moId angle Defs.missileRange w2.scratch.bulletslope damage w2

        Nothing ->
            w1


{-| What the pistol, shotgun and chaingun do before the bullets fly: the sound, the
body's firing pose, one round of ammo, the flash in the given state, and aiming.
-}
fireBullets : Env -> Int -> Sfx -> Int -> State -> World -> World
fireBullets env n sound ammo flashState world =
    world
        |> playerSound n sound
        |> onPlayerMo n (\_ mo -> setMobjState env mo.id State.PlayAtk2)
        |> useAmmo n ammo
        |> Env.setPsprite env n FlashPsp flashState
        |> onPlayerMo n (\_ mo -> bulletSlope env mo.id)


{-| A\_FirePistol: accurate unless the trigger is held for a refire.
-}
firePistol : Env -> Int -> Psp -> World -> World
firePistol env n _ =
    onPlayer n
        (\p world ->
            world
                |> fireBullets env n Sfx.Pistol 1 (Defs.weaponInfo p.readyweapon).flashstate
                |> onPlayerMo n (\pl mo -> gunShot env mo.id (pl.refire == 0))
        )


{-| A\_FireShotgun: seven pellets, spread.
-}
fireShotgun : Env -> Int -> Psp -> World -> World
fireShotgun env n _ =
    onPlayer n
        (\p world ->
            world
                |> fireBullets env n Sfx.Shotgn 1 (Defs.weaponInfo p.readyweapon).flashstate
                |> onPlayerMo n (\_ mo w -> repeat 7 (gunShot env mo.id False) w)
        )


{-| A\_FireShotgun2: twenty pellets, spread twice as wide and up and down too.
-}
fireShotgun2 : Env -> Int -> Psp -> World -> World
fireShotgun2 env n _ =
    onPlayer n
        (\p world ->
            world
                |> fireBullets env n Sfx.Dshtgn 2 (Defs.weaponInfo p.readyweapon).flashstate
                |> onPlayerMo n (\_ mo w -> repeat 20 (superShotgunPellet env mo.id) w)
        )


superShotgunPellet : Env -> ThinkerId -> World -> World
superShotgunPellet env moId world =
    let
        ( r, w1 ) =
            World.pRandom world

        damage =
            5 * (remainderBy 3 r + 1)

        ( spread, w2 ) =
            randomSpread 19 w1

        ( tilt, w3 ) =
            randomSpread 5 w2
    in
    case World.mobj moId w3 of
        Just mo ->
            Map.lineAttack env
                moId
                (Angle.add mo.angle spread)
                Defs.missileRange
                (Int32.add w3.scratch.bulletslope tilt)
                damage
                w3

        Nothing ->
            w3


{-| A\_FireCGun: nothing but the sound without bullets; the flash is the one
matching the chaingun's frame (its first or second firing state).
-}
fireCGun : Env -> Int -> Psp -> World -> World
fireCGun env n which world =
    world
        |> playerSound n Sfx.Pistol
        |> onPlayer n
            (\p w ->
                if readyAmmo p == 0 then
                    w

                else
                    let
                        frame =
                            (Player.psprite which p).state
                                |> Maybe.map (\s -> State.index s - State.index State.Chain1)
                                |> Maybe.withDefault 0

                        flashState =
                            stateOffset (Defs.weaponInfo p.readyweapon).flashstate frame
                    in
                    w
                        |> onPlayerMo n (\_ mo -> setMobjState env mo.id State.PlayAtk2)
                        |> useAmmo n 1
                        |> Env.setPsprite env n FlashPsp flashState
                        |> onPlayerMo n (\_ mo -> bulletSlope env mo.id)
                        |> onPlayerMo n (\pl mo -> gunShot env mo.id (pl.refire == 0))
            )


{-| A\_Light0: the muzzle flash's light is over.
-}
light0 : Env -> Int -> Psp -> World -> World
light0 _ n _ =
    setExtraLight n 0


{-| A\_Light1.
-}
light1 : Env -> Int -> Psp -> World -> World
light1 _ n _ =
    setExtraLight n 1


{-| A\_Light2.
-}
light2 : Env -> Int -> Psp -> World -> World
light2 _ n _ =
    setExtraLight n 2


setExtraLight : Int -> Int -> World -> World
setExtraLight n light =
    World.updatePlayer n (\p -> { p | extralight = light })


{-| A\_BFGSpray (an action of the BFG ball, not of a weapon sprite): forty tracers
fanned across 90 degrees of the ball's heading, aimed from whoever fired it; each
that finds a target puts a flash on it and does 15 to 120 damage.
-}
bfgSpray : Env -> ThinkerId -> World -> World
bfgSpray env moId world =
    repeatIndexed 40 (bfgTracer env moId) world


bfgTracer : Env -> ThinkerId -> Int -> World -> World
bfgTracer env moId i world =
    case World.mobj moId world of
        Just mo ->
            case mo.target of
                Just source ->
                    let
                        angle =
                            Angle.add (Angle.sub mo.angle (Angle.ang90 // 2)) ((Angle.ang90 // 40) * i)

                        ( _, aimed ) =
                            Map.aimLineAttack env source angle aimRange world
                    in
                    case aimed.scratch.linetarget of
                        Just targetId ->
                            case World.mobj targetId aimed of
                                Just target ->
                                    let
                                        ( _, flashed ) =
                                            Doom.Play.Mobj.spawnMobj env
                                                target.x
                                                target.y
                                                (Int32.add target.z (Int32.shiftRight 2 target.height))
                                                MobjType.Extrabfg
                                                aimed

                                        ( damage, rolled ) =
                                            bfgDamage flashed
                                    in
                                    Inter.damageMobj env targetId (Just source) (Just source) damage rolled

                                Nothing ->
                                    aimed

                        Nothing ->
                            aimed

                Nothing ->
                    -- C would follow a null `target` and crash; leave the world as it is.
                    world

        Nothing ->
            world


{-| The sum of fifteen rolls of 1 to 8.
-}
bfgDamage : World -> ( Int, World )
bfgDamage world =
    List.foldl
        (\_ ( total, w ) ->
            let
                ( r, next ) =
                    World.pRandom w
            in
            ( total + Bitwise.and r 7 + 1, next )
        )
        ( 0, world )
        (List.range 1 15)


{-| A\_BFGsound: the BFG charging up.
-}
bfgSound : Env -> Int -> Psp -> World -> World
bfgSound _ n _ =
    playerSound n Sfx.Bfg


{-| A\_OpenShotgun2 (p\_enemy.c).
-}
openShotgun2 : Env -> Int -> Psp -> World -> World
openShotgun2 _ n _ =
    playerSound n Sfx.Dbopn


{-| A\_LoadShotgun2 (p\_enemy.c).
-}
loadShotgun2 : Env -> Int -> Psp -> World -> World
loadShotgun2 _ n _ =
    playerSound n Sfx.Dbload


{-| A\_CloseShotgun2 (p\_enemy.c): the sound, then A\_ReFire.
-}
closeShotgun2 : Env -> Int -> Psp -> World -> World
closeShotgun2 env n which world =
    world
        |> playerSound n Sfx.Dbcls
        |> reFire env n which



-- HELPERS


{-| Carry on with player n, as they are now; nothing happens if there is no such
player.
-}
onPlayer : Int -> (Player -> World -> World) -> World -> World
onPlayer n f world =
    case World.player n world of
        Just p ->
            f p world

        Nothing ->
            world


{-| Carry on with player n and their body, as they are now; nothing happens if the
player has no body (C would crash following the null `mo`).
-}
onPlayerMo : Int -> (Player -> Mobj -> World -> World) -> World -> World
onPlayerMo n f world =
    case World.player n world of
        Just p ->
            case p.mo |> Maybe.andThen (\id -> World.mobj id world) of
                Just mo ->
                    f p mo world

                Nothing ->
                    world

        Nothing ->
            world


{-| P\_SetMobjState, when whether the thing survived doesn't matter.
-}
setMobjState : Env -> ThinkerId -> State -> World -> World
setMobjState env id state world =
    Tuple.second (Env.setMobjState env id state world)


updatePsprite : Int -> Psp -> (PspDef -> PspDef) -> World -> World
updatePsprite n which f =
    World.updatePlayer n (\p -> Player.setPsprite which (f (Player.psprite which p)) p)


{-| `S_StartSound (player->mo, sfx)`.
-}
playerSound : Int -> Sfx -> World -> World
playerSound n sfx world =
    case World.player n world |> Maybe.andThen .mo of
        Just id ->
            World.startSound (Sound.FromMobj id) sfx world

        Nothing ->
            world


{-| A\_GunFlash's second half: the flash state of the weapon held.
-}
showFlash : Env -> Int -> World -> World
showFlash env n =
    onPlayer n (\p -> Env.setPsprite env n FlashPsp (Defs.weaponInfo p.readyweapon).flashstate)


{-| `player->ammo[weaponinfo[player->readyweapon].ammo] -= count`. Only weapons that
use ammo fire, so am\_noammo never comes up.
-}
useAmmo : Int -> Int -> World -> World
useAmmo n count =
    World.updatePlayer n
        (\p ->
            case (Defs.weaponInfo p.readyweapon).ammo of
                Just ammo ->
                    Player.setAmmo ammo (Player.ammo ammo p - count) p

                Nothing ->
                    p
        )


{-| The ammo the weapon held uses, 0 for one that uses none.
-}
readyAmmo : Player -> Int
readyAmmo p =
    case (Defs.weaponInfo p.readyweapon).ammo of
        Just ammo ->
            Player.ammo ammo p

        Nothing ->
            0


{-| The state `offset` places after `state` in the state table (C's `statenum + k`).
-}
stateOffset : State -> Int -> State
stateOffset state offset =
    State.fromIndex (State.index state + offset)
        |> Maybe.withDefault State.Null


{-| `(P_Random() - P_Random()) << shift`, the left roll first.
-}
randomSpread : Int -> World -> ( Int, World )
randomSpread shift world =
    let
        ( a, w1 ) =
            World.pRandom world

        ( b, w2 ) =
            World.pRandom w1
    in
    ( Int32.shiftLeft shift (a - b), w2 )


{-| The thing `linetarget` points at, if any.
-}
lineTarget : World -> Maybe Mobj
lineTarget world =
    world.scratch.linetarget |> Maybe.andThen (\id -> World.mobj id world)


repeat : Int -> (World -> World) -> World -> World
repeat count f world =
    repeatIndexed count (\_ -> f) world


repeatIndexed : Int -> (Int -> World -> World) -> World -> World
repeatIndexed count f world =
    List.foldl f world (List.range 0 (count - 1))
