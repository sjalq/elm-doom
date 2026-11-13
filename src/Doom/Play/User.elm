module Doom.Play.User exposing (playerThink)

{-| The player thinking each tic (p\_user.c): moving, the view bobbing, dying, using,
changing weapons, powers running out.

@docs playerThink

-}

import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (PlayerState(..), Power(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Map as Map
import Doom.Play.Spec as Spec
import Doom.Play.Weapon as Weapon
import Doom.Play.WeaponChoice as WeaponChoice
import Doom.Player as Player exposing (Player)
import Doom.Tables as Tables
import Doom.Ticcmd as Ticcmd
import Doom.World as World exposing (World)


{-| MAXBOB: at most 16 pixels of bob.
-}
maxBob : Fixed
maxBob =
    0x00100000


{-| INVERSECOLORMAP: the colormap of the invulnerability effect.
-}
inverseColormap : Int
inverseColormap =
    32


{-| ANG5.
-}
ang5 : Angle
ang5 =
    Angle.ang90 // 18



-- THINKING


{-| P\_PlayerThink player: one tic of a player. The noclip cheat goes on or off; a
chainsaw hit last tic pulls the player forward this tic; a dead player only falls
and looks at their killer. Otherwise the player moves (unless just teleported),
their view height is worked out, the floor's special hurts them or counts a secret,
the weapon is changed or used, the weapon sprites run, and powers count down.
-}
playerThink : Env -> Int -> World -> World
playerThink env n =
    onPlayerMo n
        (\p mo world ->
            let
                noclipped =
                    if Bitwise.and p.cheats Defs.cfNoclip /= 0 then
                        Flags.set Flags.mfNoClip mo.flags

                    else
                        Flags.clear Flags.mfNoClip mo.flags

                pulled =
                    if Flags.has Flags.mfJustAttacked noclipped then
                        world
                            |> World.updatePlayer n
                                (\pl ->
                                    let
                                        cmd =
                                            pl.cmd
                                    in
                                    { pl | cmd = { cmd | angleturn = 0, forwardmove = 0xC800 // 512, sidemove = 0 } }
                                )
                            |> World.updateMobj mo.id (\m -> { m | flags = Flags.clear Flags.mfJustAttacked noclipped })

                    else
                        World.updateMobj mo.id (\m -> { m | flags = noclipped }) world
            in
            if p.playerstate == Dead then
                deathThink env n pulled

            else
                pulled
                    |> moveOrWait env n
                    |> calcHeight n
                    |> specialSector env n
                    |> changeWeapon env n
                    |> use env n
                    |> Weapon.movePsprites env n
                    |> countDownPowers n
        )


{-| The thing's `reactiontime` keeps it still for a while after teleporting.
-}
moveOrWait : Env -> Int -> World -> World
moveOrWait env n =
    onPlayerMo n
        (\_ mo world ->
            if mo.reactiontime /= 0 then
                World.updateMobj mo.id (\m -> { m | reactiontime = m.reactiontime - 1 }) world

            else
                movePlayer env n world
        )


specialSector : Env -> Int -> World -> World
specialSector env n =
    onPlayerMo n
        (\_ mo world ->
            if (World.sector (World.mobjSector env.level mo) world).special /= 0 then
                Spec.playerInSpecialSector env n world

            else
                world
        )


{-| The weapon change half of P\_PlayerThink: a special command (pause, save) has no
other buttons; the change itself happens when the weapon sprite gets to it.
-}
changeWeapon : Env -> Int -> World -> World
changeWeapon env n world =
    world
        |> World.updatePlayer n
            (\p ->
                if Bitwise.and p.cmd.buttons Ticcmd.btSpecial /= 0 then
                    let
                        cmd =
                            p.cmd
                    in
                    { p | cmd = { cmd | buttons = 0 } }

                else
                    p
            )
        |> World.updatePlayer n
            (\p ->
                case WeaponChoice.requestedWeapon env.params.gameMode p of
                    Just weapon ->
                        { p | pendingweapon = Just weapon }

                    Nothing ->
                        p
            )


{-| Pressing use works lines once per press.
-}
use : Env -> Int -> World -> World
use env n =
    onPlayer n
        (\p world ->
            if Bitwise.and p.cmd.buttons Ticcmd.btUse /= 0 then
                if not p.usedown then
                    world
                        |> Map.useLines env n
                        |> World.updatePlayer n (\pl -> { pl | usedown = True })

                else
                    world

            else
                World.updatePlayer n (\pl -> { pl | usedown = False }) world
        )


{-| The power counters: berserk counts up (its red fades), the others down; partial
invisibility ends when it runs out. Then the damage and pickup flashes fade, and the
view's colormap follows the powers.
-}
countDownPowers : Int -> World -> World
countDownPowers n =
    onPlayer n
        (\p world ->
            let
                step power f pl =
                    if Player.power power pl /= 0 then
                        Player.setPower power (f (Player.power power pl)) pl

                    else
                        pl

                down =
                    \x -> x - 1

                counted =
                    p
                        |> step Strength (\x -> x + 1)
                        |> step Invulnerability down
                        |> step Invisibility down
                        |> step Infrared down
                        |> step IronFeet down
                        |> (\pl -> { pl | damagecount = fade pl.damagecount, bonuscount = fade pl.bonuscount })

                withColormap =
                    { counted | fixedcolormap = fixedColormap counted }

                visible =
                    Player.power Invisibility p /= 0 && Player.power Invisibility counted == 0
            in
            world
                |> World.updatePlayer n (\_ -> withColormap)
                |> (if visible then
                        clearShadow p.mo

                    else
                        identity
                   )
        )


clearShadow : Maybe ThinkerId -> World -> World
clearShadow mo world =
    case mo of
        Just id ->
            World.updateMobj id (\m -> { m | flags = Flags.clear Flags.mfShadow m.flags }) world

        Nothing ->
            world


fade : Int -> Int
fade count =
    if count /= 0 then
        count - 1

    else
        count


{-| The colormap the whole view is drawn with for the player's powers (the end of
P\_PlayerThink): inverted while invulnerable, almost full bright with light
amplification, each flickering off in its last four seconds or so; 0 for none.
-}
fixedColormap : Player -> Int
fixedColormap p =
    let
        flickering tics on =
            if tics > 4 * 32 || Bitwise.and tics 8 /= 0 then
                on

            else
                0

        invulnerable =
            Player.power Invulnerability p

        infrared =
            Player.power Infrared p
    in
    if invulnerable /= 0 then
        flickering invulnerable inverseColormap

    else if infrared /= 0 then
        flickering infrared 1

    else
        0



-- MOVEMENT


{-| P\_Thrust: push the player's body along an angle.
-}
thrust : ThinkerId -> Angle -> Fixed -> World -> World
thrust id angle move =
    World.updateMobj id
        (\m ->
            { m
                | momx = Fixed.add m.momx (Fixed.mul move (Angle.cosine angle))
                , momy = Fixed.add m.momy (Fixed.mul move (Angle.sine angle))
            }
        )


{-| P\_MovePlayer: turn, and walk only with feet on the ground (`onground` stays in the
scratch for P\_CalcHeight). Moving from standing starts the running animation.
-}
movePlayer : Env -> Int -> World -> World
movePlayer env n =
    onPlayerMo n
        (\p mo world ->
            let
                cmd =
                    p.cmd

                angle =
                    Angle.add mo.angle (Int32.shiftLeft 16 cmd.angleturn)

                onground =
                    mo.z <= mo.floorz

                turned =
                    world
                        |> World.updateMobj mo.id (\m -> { m | angle = angle })
                        |> World.setScratch (\s -> { s | onground = onground })

                forward =
                    if cmd.forwardmove /= 0 && onground then
                        thrust mo.id angle (cmd.forwardmove * 2048) turned

                    else
                        turned

                sideways =
                    if cmd.sidemove /= 0 && onground then
                        thrust mo.id (Angle.sub angle Angle.ang90) (cmd.sidemove * 2048) forward

                    else
                        forward
            in
            if (cmd.forwardmove /= 0 || cmd.sidemove /= 0) && mo.state == State.Play then
                Tuple.second (Env.setMobjState env mo.id State.PlayRun1 sideways)

            else
                sideways
        )


{-| P\_CalcHeight: the player's bob from their speed, and the height of their view:
eye level plus the bob when walking on the ground, easing back to eye level after
landing or crouching from a fall; never closer than 4 units to the ceiling.
-}
calcHeight : Int -> World -> World
calcHeight n =
    onPlayerMo n
        (\p mo world ->
            let
                bob =
                    Int32.add (Fixed.mul mo.momx mo.momx) (Fixed.mul mo.momy mo.momy)
                        |> Int32.shiftRight 2
                        |> min maxBob
            in
            if Bitwise.and p.cheats Defs.cfNomomentum /= 0 || not world.scratch.onground then
                -- C first sets viewz to z + VIEWHEIGHT, clamped below the ceiling,
                -- then overwrites it with this.
                World.updatePlayer n
                    (\pl -> { pl | bob = bob, viewz = Fixed.add mo.z pl.viewheight })
                    world

            else
                let
                    viewBob =
                        Fixed.mul (bob // 2) (Tables.finesine (modBy Angle.fineangles (409 * world.leveltime)))

                    ( viewheight, deltaviewheight ) =
                        if p.playerstate == Live then
                            settleViewHeight p.viewheight p.deltaviewheight

                        else
                            ( p.viewheight, p.deltaviewheight )

                    viewz =
                        Fixed.add (Fixed.add mo.z viewheight) viewBob
                            |> min (Fixed.sub mo.ceilingz (4 * Fixed.fracunit))
                in
                World.updatePlayer n
                    (\pl ->
                        { pl
                            | bob = bob
                            , viewheight = viewheight
                            , deltaviewheight = deltaviewheight
                            , viewz = viewz
                        }
                    )
                    world
        )


{-| A live player's view height moving by `deltaviewheight` (which grows by a
quarter unit a tic) back towards VIEWHEIGHT, but never below half of it.
-}
settleViewHeight : Fixed -> Fixed -> ( Fixed, Fixed )
settleViewHeight viewheight deltaviewheight =
    let
        moved =
            Fixed.add viewheight deltaviewheight

        ( capped, cappedDelta ) =
            if moved > Defs.viewHeight then
                ( Defs.viewHeight, 0 )

            else
                ( moved, deltaviewheight )

        ( floored, flooredDelta ) =
            if capped < Defs.viewHeight // 2 then
                ( Defs.viewHeight // 2
                , if cappedDelta <= 0 then
                    1

                  else
                    cappedDelta
                )

            else
                ( capped, cappedDelta )

        speeded =
            if flooredDelta /= 0 then
                case Fixed.add flooredDelta (Fixed.fracunit // 4) of
                    0 ->
                        1

                    d ->
                        d

            else
                flooredDelta
    in
    ( floored, speeded )



-- DEATH


{-| P\_DeathThink: the weapon sprites run on, the view sinks to the floor, the player
turns towards their killer (and the red fades once facing them), and pressing use
asks to be reborn.
-}
deathThink : Env -> Int -> World -> World
deathThink env n world =
    world
        |> Weapon.movePsprites env n
        |> World.updatePlayer n
            (\p ->
                let
                    lowered =
                        if p.viewheight > 6 * Fixed.fracunit then
                            Fixed.sub p.viewheight Fixed.fracunit

                        else
                            p.viewheight
                in
                { p | viewheight = max lowered (6 * Fixed.fracunit), deltaviewheight = 0 }
            )
        |> onPlayerMo n (\_ mo -> World.setScratch (\s -> { s | onground = mo.z <= mo.floorz }))
        |> calcHeight n
        |> onPlayerMo n (lookAtAttacker n)
        |> World.updatePlayer n
            (\p ->
                if Bitwise.and p.cmd.buttons Ticcmd.btUse /= 0 then
                    { p | playerstate = Reborn }

                else
                    p
            )


lookAtAttacker : Int -> Player -> Mobj -> World -> World
lookAtAttacker n p mo world =
    let
        attacker =
            p.attacker
                |> Maybe.andThen
                    (\id ->
                        if id /= mo.id then
                            World.mobj id world

                        else
                            Nothing
                    )

        fadeDamage =
            World.updatePlayer n (\pl -> { pl | damagecount = fade pl.damagecount })
    in
    case attacker of
        Just a ->
            let
                angle =
                    Geometry.pointToAngle2 mo.x mo.y a.x a.y

                delta =
                    Angle.sub angle mo.angle

                turn newAngle =
                    World.updateMobj mo.id (\m -> { m | angle = newAngle })
            in
            if delta < ang5 || delta > Angle.neg ang5 then
                world |> turn angle |> fadeDamage

            else if delta < Angle.ang180 then
                turn (Angle.add mo.angle ang5) world

            else
                turn (Angle.sub mo.angle ang5) world

        Nothing ->
            fadeDamage world



-- HELPERS


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
