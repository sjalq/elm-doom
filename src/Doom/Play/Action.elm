module Doom.Play.Action exposing (hooks, setMobjState, setPsprite, ActionKind(..), kind)

{-| Running states' actions: P\_SetMobjState and P\_SetPsprite, the dispatch from each
`Action` to its function, and `hooks`, which ties them into `Env`.

In the C source a state's action is a function pointer called with whatever the state
belongs to: a thing (`acp1(mobj)`, the monsters' actions and A\_BFGSpray) or a
player's weapon sprite (`acp2(player, psp)`, the weapons' actions). Here each `Action`
is mapped to its function, in `Doom.Play.Enemy` or `Doom.Play.Weapon`, and `kind` says
which of the two it takes.

@docs hooks, setMobjState, setPsprite, ActionKind, kind

-}

import Doom.Defs exposing (Psp)
import Doom.Fixed as Fixed
import Doom.Info.Action as A exposing (Action)
import Doom.Info.State as State exposing (State)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Enemy as Enemy
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Mobj as PMobj
import Doom.Play.Spec as Spec
import Doom.Play.Switch as Switch
import Doom.Play.Weapon as Weapon
import Doom.Player as Player
import Doom.World as World exposing (World)


{-| The real functions behind `Env`'s hooks.
-}
hooks : Env.Hooks
hooks =
    Env.Hooks
        { setMobjState = setMobjState
        , setPsprite = setPsprite
        , crossSpecialLine = Spec.crossSpecialLine
        , shootSpecialLine = Spec.shootSpecialLine
        , useSpecialLine = Switch.useSpecialLine
        }



-- P_SetMobjState


{-| P\_SetMobjState: put the thing in a state and run the state's action; while the new
state lasts 0 tics, go straight on to the next one (the state's own `nextstate`, read
from the state entered, not from wherever the action left the thing). S\_NULL removes
the thing: then the result is False.

After each action the thing is read again: the action may have changed it, even its
state.

-}
setMobjState : Env -> ThinkerId -> State -> World -> ( Bool, World )
setMobjState env id state world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just m ->
            if state == State.Null then
                ( False
                , world
                    |> World.setMobj { m | state = State.Null }
                    |> PMobj.removeMobj env id
                )

            else
                let
                    st =
                        Env.stateDef env state

                    entered =
                        World.setMobj { m | state = state, tics = st.tics, sprite = st.sprite, frame = st.frame } world

                    acted =
                        case Maybe.map dispatch st.action of
                            Just (OnMobj f) ->
                                f env id entered

                            Just (OnWeapon _) ->
                                -- No thing's state has a weapon action (the C source would
                                -- call it with the thing as the player).
                                entered

                            Nothing ->
                                entered
                in
                case World.mobj id acted of
                    Just after ->
                        if after.tics == 0 then
                            setMobjState env id st.next acted

                        else
                            ( True, acted )

                    Nothing ->
                        ( True, acted )



-- P_SetPsprite


{-| P\_SetPsprite: put one of player n's weapon sprites in a state, set its position if
the state gives one (`misc1`, `misc2`), and run the state's action; while the state
lasts 0 tics, go on to the next. The next state is that of the sprite's state after
the action (which may have changed it); if the action took the sprite away, stop.
-}
setPsprite : Env -> Int -> Psp -> State -> World -> World
setPsprite env n psp state world =
    case World.player n world of
        Nothing ->
            world

        Just p ->
            let
                sprite =
                    Player.psprite psp p
            in
            if state == State.Null then
                -- object removed itself
                World.updatePlayer n (Player.setPsprite psp { sprite | state = Nothing }) world

            else
                let
                    st =
                        Env.stateDef env state

                    placed =
                        if st.misc1 /= 0 then
                            -- coordinate set
                            { sprite | state = Just state, tics = st.tics, sx = Fixed.fromInt st.misc1, sy = Fixed.fromInt st.misc2 }

                        else
                            { sprite | state = Just state, tics = st.tics }

                    entered =
                        World.updatePlayer n (Player.setPsprite psp placed) world

                    acted =
                        case Maybe.map dispatch st.action of
                            Just (OnWeapon f) ->
                                f env n psp entered

                            Just (OnMobj _) ->
                                -- No weapon state has a thing's action (the C source would
                                -- call it with the player as the thing).
                                entered

                            Nothing ->
                                entered
                in
                case World.player n acted |> Maybe.map (Player.psprite psp) of
                    Just after ->
                        case after.state of
                            Just current ->
                                if after.tics == 0 then
                                    setPsprite env n psp (Env.stateDef env current).next acted

                                else
                                    acted

                            Nothing ->
                                acted

                    Nothing ->
                        acted



-- DISPATCH


{-| What an action is called with: a thing, or a player's weapon sprite.
-}
type ActionKind
    = MobjAction
    | WeaponAction


{-| Which of the two an action takes.
-}
kind : Action -> ActionKind
kind action =
    case dispatch action of
        OnMobj _ ->
            MobjAction

        OnWeapon _ ->
            WeaponAction


type Dispatch
    = OnMobj (Env -> ThinkerId -> World -> World)
    | OnWeapon (Env -> Int -> Psp -> World -> World)


{-| Each action's function (info.c's `{A_...}` entries).
-}
dispatch : Action -> Dispatch
dispatch action =
    case action of
        A.Light0 ->
            OnWeapon Weapon.light0

        A.WeaponReady ->
            OnWeapon Weapon.weaponReady

        A.Lower ->
            OnWeapon Weapon.lower

        A.Raise ->
            OnWeapon Weapon.raise

        A.Punch ->
            OnWeapon Weapon.punch

        A.ReFire ->
            OnWeapon Weapon.reFire

        A.FirePistol ->
            OnWeapon Weapon.firePistol

        A.Light1 ->
            OnWeapon Weapon.light1

        A.FireShotgun ->
            OnWeapon Weapon.fireShotgun

        A.Light2 ->
            OnWeapon Weapon.light2

        A.FireShotgun2 ->
            OnWeapon Weapon.fireShotgun2

        A.CheckReload ->
            OnWeapon Weapon.checkReload

        A.OpenShotgun2 ->
            OnWeapon Weapon.openShotgun2

        A.LoadShotgun2 ->
            OnWeapon Weapon.loadShotgun2

        A.CloseShotgun2 ->
            OnWeapon Weapon.closeShotgun2

        A.FireCGun ->
            OnWeapon Weapon.fireCGun

        A.GunFlash ->
            OnWeapon Weapon.gunFlash

        A.FireMissile ->
            OnWeapon Weapon.fireMissile

        A.Saw ->
            OnWeapon Weapon.saw

        A.FirePlasma ->
            OnWeapon Weapon.firePlasma

        A.BFGsound ->
            OnWeapon Weapon.bfgSound

        A.FireBFG ->
            OnWeapon Weapon.fireBFG

        A.BFGSpray ->
            OnMobj Weapon.bfgSpray

        A.Explode ->
            OnMobj Enemy.explode

        A.Pain ->
            OnMobj Enemy.pain

        A.PlayerScream ->
            OnMobj Enemy.playerScream

        A.Fall ->
            OnMobj Enemy.fall

        A.XScream ->
            OnMobj Enemy.xScream

        A.Look ->
            OnMobj Enemy.look

        A.Chase ->
            OnMobj Enemy.chase

        A.FaceTarget ->
            OnMobj Enemy.faceTarget

        A.PosAttack ->
            OnMobj Enemy.posAttack

        A.Scream ->
            OnMobj Enemy.scream

        A.SPosAttack ->
            OnMobj Enemy.sPosAttack

        A.VileChase ->
            OnMobj Enemy.vileChase

        A.VileStart ->
            OnMobj Enemy.vileStart

        A.VileTarget ->
            OnMobj Enemy.vileTarget

        A.VileAttack ->
            OnMobj Enemy.vileAttack

        A.StartFire ->
            OnMobj Enemy.startFire

        A.Fire ->
            OnMobj Enemy.fire

        A.FireCrackle ->
            OnMobj Enemy.fireCrackle

        A.Tracer ->
            OnMobj Enemy.tracer

        A.SkelWhoosh ->
            OnMobj Enemy.skelWhoosh

        A.SkelFist ->
            OnMobj Enemy.skelFist

        A.SkelMissile ->
            OnMobj Enemy.skelMissile

        A.FatRaise ->
            OnMobj Enemy.fatRaise

        A.FatAttack1 ->
            OnMobj Enemy.fatAttack1

        A.FatAttack2 ->
            OnMobj Enemy.fatAttack2

        A.FatAttack3 ->
            OnMobj Enemy.fatAttack3

        A.BossDeath ->
            OnMobj Enemy.bossDeath

        A.CPosAttack ->
            OnMobj Enemy.cPosAttack

        A.CPosRefire ->
            OnMobj Enemy.cPosRefire

        A.TroopAttack ->
            OnMobj Enemy.troopAttack

        A.SargAttack ->
            OnMobj Enemy.sargAttack

        A.HeadAttack ->
            OnMobj Enemy.headAttack

        A.BruisAttack ->
            OnMobj Enemy.bruisAttack

        A.SkullAttack ->
            OnMobj Enemy.skullAttack

        A.Metal ->
            OnMobj Enemy.metal

        A.SpidRefire ->
            OnMobj Enemy.spidRefire

        A.BabyMetal ->
            OnMobj Enemy.babyMetal

        A.BspiAttack ->
            OnMobj Enemy.bspiAttack

        A.Hoof ->
            OnMobj Enemy.hoof

        A.CyberAttack ->
            OnMobj Enemy.cyberAttack

        A.PainAttack ->
            OnMobj Enemy.painAttack

        A.PainDie ->
            OnMobj Enemy.painDie

        A.KeenDie ->
            OnMobj Enemy.keenDie

        A.BrainPain ->
            OnMobj Enemy.brainPain

        A.BrainScream ->
            OnMobj Enemy.brainScream

        A.BrainDie ->
            OnMobj Enemy.brainDie

        A.BrainAwake ->
            OnMobj Enemy.brainAwake

        A.BrainSpit ->
            OnMobj Enemy.brainSpit

        A.SpawnSound ->
            OnMobj Enemy.spawnSound

        A.SpawnFly ->
            OnMobj Enemy.spawnFly

        A.BrainExplode ->
            OnMobj Enemy.brainExplode
