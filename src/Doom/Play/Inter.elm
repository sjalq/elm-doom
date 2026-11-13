module Doom.Play.Inter exposing (giveAmmo, giveWeapon, giveBody, giveArmor, giveCard, givePower, touchSpecialThing, killMobj, damageMobj)

{-| Picking things up, getting hurt and dying (p\_inter.c).

@docs giveAmmo, giveWeapon, giveBody, giveArmor, giveCard, givePower, touchSpecialThing, killMobj, damageMobj

-}

import Array
import Bitwise
import Doom.Angle as Angle
import Doom.Defs as Defs exposing (Ammo(..), Card(..), GameMode(..), Power(..), Psp(..), Skill(..), Weapon(..))
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Info.Sprite as Sprite
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Mobj as PMobj
import Doom.Player as Player exposing (Player)
import Doom.Sound as Sound
import Doom.World as World exposing (World)


{-| BONUSADD: how much the screen flashes for a pickup.
-}
bonusAdd : Int
bonusAdd =
    6



-- GET STUFF


{-| P\_GiveAmmo player ammo num: `num` clips of the ammo (0 for half a clip), doubled on
the easiest and hardest skills, up to what the player can carry. False if they are
already full. A player who had none of it switches to a weapon that uses it.
-}
giveAmmo : Env -> Int -> Ammo -> Int -> World -> ( Bool, World )
giveAmmo env n ammo num world =
    withPlayer n world ( False, world ) <|
        \p ->
            let
                ( gave, given ) =
                    giveAmmoTo env.params.skill ammo num p
            in
            ( gave, World.updatePlayer n (always given) world )


giveAmmoTo : Skill -> Ammo -> Int -> Player -> ( Bool, Player )
giveAmmoTo skill ammo num p =
    if Player.ammo ammo p == Player.maxAmmoOf ammo p then
        ( False, p )

    else
        let
            clips =
                if num /= 0 then
                    num * Defs.clipAmmo ammo

                else
                    Defs.clipAmmo ammo // 2

            amount =
                if skill == Baby || skill == Nightmare then
                    -- double ammo in trainer mode, you'll need it in nightmare
                    clips * 2

                else
                    clips

            oldammo =
                Player.ammo ammo p

            filled =
                Player.setAmmo ammo (min (oldammo + amount) (Player.maxAmmoOf ammo p)) p
        in
        if oldammo /= 0 then
            -- the player was lower on purpose: don't change weapons
            ( True, filled )

        else
            ( True, switchForAmmo ammo filled )


{-| Down to zero before: select a weapon for the new ammo (preferences are not user
selectable).
-}
switchForAmmo : Ammo -> Player -> Player
switchForAmmo ammo p =
    let
        switchTo weapon =
            if Player.ownsWeapon weapon p then
                { p | pendingweapon = Just weapon }

            else
                p
    in
    case ammo of
        Clip ->
            if p.readyweapon == Fist then
                if Player.ownsWeapon Chaingun p then
                    { p | pendingweapon = Just Chaingun }

                else
                    { p | pendingweapon = Just Pistol }

            else
                p

        Shell ->
            if p.readyweapon == Fist || p.readyweapon == Pistol then
                switchTo Shotgun

            else
                p

        Cell ->
            if p.readyweapon == Fist || p.readyweapon == Pistol then
                switchTo Plasma

            else
                p

        Misl ->
            if p.readyweapon == Fist then
                switchTo Missile

            else
                p


{-| P\_GiveWeapon player weapon dropped: the weapon and some ammo for it (one clip if a
monster dropped it, two if it was placed). In cooperative net games placed weapons
stay for everyone: each player takes one once, with more ammo, and it is left where
it is (False).
-}
giveWeapon : Env -> Int -> Weapon -> Bool -> World -> ( Bool, World )
giveWeapon env n weapon dropped world =
    withPlayer n world ( False, world ) <|
        \p ->
            let
                ammo =
                    (Defs.weaponInfo weapon).ammo

                giveClips clips player =
                    case ammo of
                        Just a ->
                            giveAmmoTo env.params.skill a clips player

                        Nothing ->
                            ( False, player )
            in
            if env.params.netgame && env.params.deathmatch /= 2 && not dropped then
                -- leave placed weapons forever on net games
                if Player.ownsWeapon weapon p then
                    ( False, world )

                else
                    let
                        ( _, withAmmo ) =
                            { p | bonuscount = p.bonuscount + bonusAdd }
                                |> Player.setWeaponOwned weapon True
                                |> giveClips
                                    (if env.params.deathmatch /= 0 then
                                        5

                                     else
                                        2
                                    )

                        updated =
                            World.updatePlayer n (always { withAmmo | pendingweapon = Just weapon }) world
                    in
                    ( False
                    , if n == env.params.consoleplayer then
                        World.startSound Sound.Ambient Sfx.Wpnup updated

                      else
                        updated
                    )

            else
                let
                    -- one clip with a dropped weapon, two clips with a found weapon
                    ( gaveammo, p1 ) =
                        giveClips
                            (if dropped then
                                1

                             else
                                2
                            )
                            p

                    ( gaveweapon, p2 ) =
                        if Player.ownsWeapon weapon p1 then
                            ( False, p1 )

                        else
                            ( True, Player.setWeaponOwned weapon True { p1 | pendingweapon = Just weapon } )
                in
                ( gaveweapon || gaveammo, World.updatePlayer n (always p2) world )


{-| P\_GiveBody player num: up to `num` health, up to 100%. False if the player is
already at 100% or more.
-}
giveBody : Env -> Int -> Int -> World -> ( Bool, World )
giveBody _ n num world =
    withPlayer n world ( False, world ) <|
        \p ->
            if p.health >= Defs.maxHealth then
                ( False, world )

            else
                ( True
                , world
                    |> World.updatePlayer n (\q -> { q | health = min (q.health + num) Defs.maxHealth })
                    |> syncBodyHealth n
                )


{-| `player->mo->health = player->health`.
-}
syncBodyHealth : Int -> World -> World
syncBodyHealth n world =
    case World.player n world of
        Just p ->
            case p.mo of
                Just mo ->
                    World.updateMobj mo (\m -> { m | health = p.health }) world

                Nothing ->
                    world

        Nothing ->
            world


{-| P\_GiveArmor player armortype: green (1, 100%) or blue (2, 200%) armor. False if the
player already has at least that much.
-}
giveArmor : Env -> Int -> Int -> World -> ( Bool, World )
giveArmor _ n armortype world =
    withPlayer n world ( False, world ) <|
        \p ->
            let
                hits =
                    armortype * 100
            in
            if p.armorpoints >= hits then
                -- don't pick up
                ( False, world )

            else
                ( True, World.updatePlayer n (\q -> { q | armortype = armortype, armorpoints = hits }) world )


{-| P\_GiveCard.
-}
giveCard : Env -> Int -> Card -> World -> World
giveCard _ n card world =
    World.updatePlayer n
        (\p ->
            if Player.hasCard card p then
                p

            else
                Player.setCard card True { p | bonuscount = bonusAdd }
        )
        world


{-| P\_GivePower: the timed powers start (or restart) their count; berserk heals to 100%
and starts counting up; the computer map is a one-off. False only for a computer map
the player already has.
-}
givePower : Env -> Int -> Power -> World -> ( Bool, World )
givePower env n power world =
    let
        setTo tics =
            World.updatePlayer n (Player.setPower power tics)
    in
    case power of
        Invulnerability ->
            ( True, setTo Defs.invulnTics world )

        Invisibility ->
            ( True
            , world
                |> setTo Defs.invisTics
                |> updateBody n (\m -> { m | flags = Flags.set Flags.mfShadow m.flags })
            )

        Infrared ->
            ( True, setTo Defs.infraTics world )

        IronFeet ->
            ( True, setTo Defs.ironTics world )

        Strength ->
            ( True, giveBody env n 100 world |> Tuple.second |> setTo 1 )

        AllMap ->
            withPlayer n world ( False, world ) <|
                \p ->
                    if Player.power power p /= 0 then
                        -- already got it
                        ( False, world )

                    else
                        ( True, setTo 1 world )


updateBody : Int -> (Mobj -> Mobj) -> World -> World
updateBody n f world =
    case World.player n world |> Maybe.andThen .mo of
        Just mo ->
            World.updateMobj mo f world

        Nothing ->
            world


withPlayer : Int -> World -> a -> (Player -> a) -> a
withPlayer n world default f =
    case World.player n world of
        Just p ->
            f p

        Nothing ->
            default



-- P_TouchSpecialThing


{-| What touching an item did: the player took it (and hears this sound), or it stays
where it is (the world may still have changed: a key in a net game, a weapon in
cooperative).
-}
type Pickup
    = Taken Sfx World
    | Left World


{-| P\_TouchSpecialThing special toucher: a player touched an item. If they can use it
they take it, get its message, and count it; the item is removed.

Items are told apart by their sprite, as in the C source. The C source stops the game
for an item it doesn't know; this leaves it where it is.

-}
touchSpecialThing : Env -> ThinkerId -> ThinkerId -> World -> World
touchSpecialThing env specialId toucherId world =
    case ( World.mobj specialId world, World.mobj toucherId world ) of
        ( Just special, Just toucher ) ->
            let
                delta =
                    Int32.sub special.z toucher.z
            in
            if delta > toucher.height || delta < -8 * Fixed.fracunit then
                -- out of reach
                world

            else if toucher.health <= 0 then
                -- dead thing touching (can happen with a sliding player corpse)
                world

            else
                case toucher.player of
                    Just n ->
                        case pickUp env n special world of
                            Taken sound taken ->
                                taken
                                    |> (if Flags.has Flags.mfCountItem special.flags then
                                            World.updatePlayer n (\p -> { p | itemcount = p.itemcount + 1 })

                                        else
                                            identity
                                       )
                                    |> PMobj.removeMobj env specialId
                                    |> World.updatePlayer n (\p -> { p | bonuscount = p.bonuscount + bonusAdd })
                                    |> (if n == env.params.consoleplayer then
                                            World.startSound Sound.Ambient sound

                                        else
                                            identity
                                       )

                            Left left ->
                                left

                    Nothing ->
                        -- only players pick things up (the C source would crash)
                        world

        _ ->
            world


{-| The switch on the item's sprite.
-}
pickUp : Env -> Int -> Mobj -> World -> Pickup
pickUp env n special world =
    let
        dropped =
            Flags.has Flags.mfDropped special.flags

        -- if (!give) return; player->message = msg; (sound)
        whenGiven ( gave, given ) msg sound =
            if gave then
                Taken sound (setMessage n msg given)

            else
                Left given

        ammo a num msg =
            whenGiven (giveAmmo env n a num world) msg Sfx.Itemup

        weapon w isDropped msg =
            whenGiven (giveWeapon env n w isDropped world) msg Sfx.Wpnup

        power p msg =
            whenGiven (givePower env n p world) msg Sfx.Getpow

        card c msg =
            let
                hadIt =
                    Maybe.map (Player.hasCard c) (World.player n world) == Just True

                given =
                    (if hadIt then
                        world

                     else
                        setMessage n msg world
                    )
                        |> giveCard env n c
            in
            -- leave cards for everyone in net games
            if env.params.netgame then
                Left given

            else
                Taken Sfx.Itemup given

        healthBonus amount sound msg =
            -- can go over 100%
            world
                |> World.updatePlayer n (\p -> { p | health = min (p.health + amount) 200 })
                |> syncBodyHealth n
                |> setMessage n msg
                |> Taken sound
    in
    case special.sprite of
        -- armor
        Sprite.Arm1 ->
            whenGiven (giveArmor env n 1 world) "Picked up the armor." Sfx.Itemup

        Sprite.Arm2 ->
            whenGiven (giveArmor env n 2 world) "Picked up the MegaArmor!" Sfx.Itemup

        -- bonus items
        Sprite.Bon1 ->
            healthBonus 1 Sfx.Itemup "Picked up a health bonus."

        Sprite.Bon2 ->
            world
                |> World.updatePlayer n
                    (\p ->
                        { p
                            | armorpoints = min (p.armorpoints + 1) 200
                            , armortype =
                                if p.armortype == 0 then
                                    1

                                else
                                    p.armortype
                        }
                    )
                |> setMessage n "Picked up an armor bonus."
                |> Taken Sfx.Itemup

        Sprite.Soul ->
            healthBonus 100 Sfx.Getpow "Supercharge!"

        Sprite.Mega ->
            if env.params.gameMode /= Commercial then
                Left world

            else
                world
                    |> World.updatePlayer n (\p -> { p | health = 200 })
                    |> syncBodyHealth n
                    |> giveArmor env n 2
                    |> Tuple.second
                    |> setMessage n "MegaSphere!"
                    |> Taken Sfx.Getpow

        -- cards
        Sprite.Bkey ->
            card BlueCard "Picked up a blue keycard."

        Sprite.Ykey ->
            card YellowCard "Picked up a yellow keycard."

        Sprite.Rkey ->
            card RedCard "Picked up a red keycard."

        Sprite.Bsku ->
            card BlueSkull "Picked up a blue skull key."

        Sprite.Ysku ->
            card YellowSkull "Picked up a yellow skull key."

        Sprite.Rsku ->
            card RedSkull "Picked up a red skull key."

        -- medikits, heals
        Sprite.Stim ->
            whenGiven (giveBody env n 10 world) "Picked up a stimpack." Sfx.Itemup

        Sprite.Medi ->
            case giveBody env n 25 world of
                ( True, given ) ->
                    -- (checked after healing, so the first message never shows)
                    let
                        msg =
                            if Maybe.map .health (World.player n given) |> Maybe.map (\h -> h < 25) |> Maybe.withDefault False then
                                "Picked up a medikit that you REALLY need!"

                            else
                                "Picked up a medikit."
                    in
                    Taken Sfx.Itemup (setMessage n msg given)

                ( False, given ) ->
                    Left given

        -- power ups
        Sprite.Pinv ->
            power Invulnerability "Invulnerability!"

        Sprite.Pstr ->
            case givePower env n Strength world of
                ( True, given ) ->
                    given
                        |> setMessage n "Berserk!"
                        |> World.updatePlayer n
                            (\p ->
                                if p.readyweapon /= Fist then
                                    { p | pendingweapon = Just Fist }

                                else
                                    p
                            )
                        |> Taken Sfx.Getpow

                ( False, given ) ->
                    Left given

        Sprite.Pins ->
            power Invisibility "Partial Invisibility"

        Sprite.Suit ->
            power IronFeet "Radiation Shielding Suit"

        Sprite.Pmap ->
            power AllMap "Computer Area Map"

        Sprite.Pvis ->
            power Infrared "Light Amplification Visor"

        -- ammo
        Sprite.Clip ->
            ammo Clip
                (if dropped then
                    0

                 else
                    1
                )
                "Picked up a clip."

        Sprite.Ammo ->
            ammo Clip 5 "Picked up a box of bullets."

        Sprite.Rock ->
            ammo Misl 1 "Picked up a rocket."

        Sprite.Brok ->
            ammo Misl 5 "Picked up a box of rockets."

        Sprite.Cell ->
            ammo Cell 1 "Picked up an energy cell."

        Sprite.Celp ->
            ammo Cell 5 "Picked up an energy cell pack."

        Sprite.Shel ->
            ammo Shell 1 "Picked up 4 shotgun shells."

        Sprite.Sbox ->
            ammo Shell 5 "Picked up a box of shotgun shells."

        Sprite.Bpak ->
            let
                packed =
                    World.updatePlayer n
                        (\p ->
                            if p.backpack then
                                p

                            else
                                { p | maxammo = Array.map (\m -> m * 2) p.maxammo, backpack = True }
                        )
                        world
            in
            List.foldl (\a w -> Tuple.second (giveAmmo env n a 1 w)) packed Defs.allAmmo
                |> setMessage n "Picked up a backpack full of ammo!"
                |> Taken Sfx.Itemup

        -- weapons
        Sprite.Bfug ->
            weapon Bfg False "You got the BFG9000!  Oh, yes."

        Sprite.Mgun ->
            weapon Chaingun dropped "You got the chaingun!"

        Sprite.Csaw ->
            weapon Chainsaw False "A chainsaw!  Find some meat!"

        Sprite.Laun ->
            weapon Missile False "You got the rocket launcher!"

        Sprite.Plas ->
            weapon Plasma False "You got the plasma gun!"

        Sprite.Shot ->
            weapon Shotgun dropped "You got the shotgun!"

        Sprite.Sgn2 ->
            weapon SuperShotgun dropped "You got the super shotgun!"

        _ ->
            -- I_Error ("P_SpecialThing: Unknown gettable thing"): the world is left as it was.
            Left world


setMessage : Int -> String -> World -> World
setMessage n msg =
    World.updatePlayer n (\p -> { p | message = Just msg })



-- P_KillMobj


{-| P\_KillMobj source target: the target dies. It becomes a corpse a quarter its height,
counts as a kill (or a frag) for whoever killed it, goes to its death state (its
gory one when overkilled), and drops its weapon if it carried one.
-}
killMobj : Env -> Maybe ThinkerId -> ThinkerId -> World -> World
killMobj env source targetId world =
    case World.mobj targetId world of
        Nothing ->
            world

        Just target ->
            let
                flags =
                    target.flags
                        |> Flags.clear (Bitwise.or Flags.mfShootable (Bitwise.or Flags.mfFloat Flags.mfSkullFly))
                        |> (if target.type_ /= MobjType.Skull then
                                Flags.clear Flags.mfNoGravity

                            else
                                identity
                           )
                        |> Flags.set (Bitwise.or Flags.mfCorpse Flags.mfDropOff)

                corpse =
                    World.setMobj { target | flags = flags, height = Int32.shiftRight 2 target.height } world

                sourcePlayer =
                    source |> Maybe.andThen (\s -> World.mobj s world) |> Maybe.andThen .player

                counted =
                    case sourcePlayer of
                        Just sp ->
                            corpse
                                |> (if Flags.has Flags.mfCountKill flags then
                                        World.updatePlayer sp (\p -> { p | killcount = p.killcount + 1 })

                                    else
                                        identity
                                   )
                                |> (case target.player of
                                        Just tp ->
                                            addFrag sp tp

                                        Nothing ->
                                            identity
                                   )

                        Nothing ->
                            if not env.params.netgame && Flags.has Flags.mfCountKill flags then
                                -- count all monster deaths, even those caused by other monsters
                                World.updatePlayer 0 (\p -> { p | killcount = p.killcount + 1 }) corpse

                            else
                                corpse

                fallen =
                    case target.player of
                        Just tp ->
                            counted
                                |> (if source == Nothing then
                                        -- count environment kills against you
                                        addFrag tp tp

                                    else
                                        identity
                                   )
                                |> World.updateMobj targetId (\m -> { m | flags = Flags.clear Flags.mfSolid m.flags })
                                |> World.updatePlayer tp (\p -> { p | playerstate = Defs.Dead })
                                |> dropWeapon env tp

                        Nothing ->
                            counted
            in
            deathState env targetId fallen
                |> dropItem env targetId


{-| `players[n].frags[victim]++`.
-}
addFrag : Int -> Int -> World -> World
addFrag n victim =
    World.updatePlayer n
        (\p ->
            { p | frags = Array.set victim ((Array.get victim p.frags |> Maybe.withDefault 0) + 1) p.frags }
        )


{-| P\_DropWeapon: the player died, so put the weapon away.
-}
dropWeapon : Env -> Int -> World -> World
dropWeapon env n world =
    case World.player n world of
        Just p ->
            Env.setPsprite env n WeaponPsp (Defs.weaponInfo p.readyweapon).downstate world

        Nothing ->
            world


{-| The death (or extreme death) state, lasting a little less, at random.
-}
deathState : Env -> ThinkerId -> World -> World
deathState env id world =
    case World.mobj id world of
        Just target ->
            let
                info =
                    Env.mobjInfo env target.type_

                state =
                    if target.health < -info.spawnhealth && info.xdeathstate /= State.Null then
                        info.xdeathstate

                    else
                        info.deathstate

                ( _, dying ) =
                    Env.setMobjState env id state world

                ( r, rolled ) =
                    World.pRandom dying
            in
            World.updateMobj id (\m -> { m | tics = max 1 (m.tics - Bitwise.and r 3) }) rolled

        Nothing ->
            world


{-| Former humans drop their weapon (a "dropped" item: less ammo, never respawns).
-}
dropItem : Env -> ThinkerId -> World -> World
dropItem env id world =
    case World.mobj id world of
        Just target ->
            let
                item =
                    case target.type_ of
                        MobjType.Wolfss ->
                            Just MobjType.Clip

                        MobjType.Possessed ->
                            Just MobjType.Clip

                        MobjType.Shotguy ->
                            Just MobjType.Shotgun

                        MobjType.Chainguy ->
                            Just MobjType.Chaingun

                        _ ->
                            Nothing
            in
            case item of
                Just t ->
                    let
                        ( mo, spawned ) =
                            PMobj.spawnMobj env target.x target.y Defs.onFloorZ t world
                    in
                    World.updateMobj mo (\m -> { m | flags = Flags.set Flags.mfDropped m.flags }) spawned

                Nothing ->
                    world

        Nothing ->
            world



-- P_DamageMobj


{-| P\_DamageMobj target inflictor source damage: hurt a thing. `inflictor` is what did
it (a monster, a missile; Nothing for slime and the like), `source` who is to blame
and gets chased for it (Nothing for the environment and barrels). The thing is
pushed away from the inflictor (unless the source is a player with a chainsaw), a
player's armor takes some of the damage, and a monster may flinch and turn on its
attacker.
-}
damageMobj : Env -> ThinkerId -> Maybe ThinkerId -> Maybe ThinkerId -> Int -> World -> World
damageMobj env targetId inflictor source damage world =
    case World.mobj targetId world of
        Nothing ->
            world

        Just target ->
            if not (Flags.has Flags.mfShootable target.flags) then
                -- shouldn't happen...
                world

            else if target.health <= 0 then
                world

            else
                let
                    stopped =
                        if Flags.has Flags.mfSkullFly target.flags then
                            World.setMobj { target | momx = 0, momy = 0, momz = 0 } world

                        else
                            world

                    halved =
                        if target.player /= Nothing && env.params.skill == Baby then
                            -- take half damage in trainer mode
                            Int32.shiftRight 1 damage

                        else
                            damage

                    pushed =
                        thrust env targetId inflictor source halved stopped
                in
                case target.player of
                    Just n ->
                        case playerDamage env n targetId source halved pushed of
                            Just ( dealt, hurt ) ->
                                applyDamage env targetId source dealt hurt

                            Nothing ->
                                -- god mode or invulnerable
                                pushed

                    Nothing ->
                        applyDamage env targetId source halved pushed


{-| Kick the target away from the inflictor, unless the source is a player using the
chainsaw (which would push the victim out of reach). It sometimes falls forwards
instead: a low blow that kills, from well below.
-}
thrust : Env -> ThinkerId -> Maybe ThinkerId -> Maybe ThinkerId -> Int -> World -> World
thrust env targetId inflictor source damage world =
    case ( World.mobj targetId world, inflictor |> Maybe.andThen (\i -> World.mobj i world) ) of
        ( Just target, Just inf ) ->
            let
                chainsawing =
                    source
                        |> Maybe.andThen (\s -> World.mobj s world)
                        |> Maybe.andThen .player
                        |> Maybe.andThen (\n -> World.player n world)
                        |> Maybe.map (\p -> p.readyweapon == Chainsaw)
                        |> Maybe.withDefault False
            in
            if Flags.has Flags.mfNoClip target.flags || chainsawing then
                world

            else
                let
                    ang =
                        Geometry.pointToAngle2 inf.x inf.y target.x target.y

                    push =
                        Int32.mul (Int32.mul damage (Fixed.fracunit // 8)) 100 // (Env.mobjInfo env target.type_).mass

                    -- make fall forwards sometimes
                    ( ( finalAng, finalPush ), rolled ) =
                        if damage < 40 && damage > target.health && Int32.sub target.z inf.z > 64 * Fixed.fracunit then
                            let
                                ( r, w ) =
                                    World.pRandom world
                            in
                            if Bitwise.and r 1 /= 0 then
                                ( ( Angle.add ang Angle.ang180, Int32.mul push 4 ), w )

                            else
                                ( ( ang, push ), w )

                        else
                            ( ( ang, push ), world )
                in
                World.updateMobj targetId
                    (\m ->
                        { m
                            | momx = Int32.add m.momx (Fixed.mul finalPush (Angle.cosine finalAng))
                            , momy = Int32.add m.momy (Fixed.mul finalPush (Angle.sine finalAng))
                        }
                    )
                    rolled

        _ ->
            world


{-| The player's part: the end-of-game hell hack (a sector of special 11 can't kill),
god mode and invulnerability (Nothing: no damage at all, below 1000), armor, and the
damage counters. Gives the damage left for the body.
-}
playerDamage : Env -> Int -> ThinkerId -> Maybe ThinkerId -> Int -> World -> Maybe ( Int, World )
playerDamage env n targetId source damage world =
    case ( World.player n world, World.mobj targetId world ) of
        ( Just p, Just target ) ->
            let
                capped =
                    if
                        (World.sector (World.mobjSector env.level target) world).special
                            == 11
                            && damage
                            >= target.health
                    then
                        target.health - 1

                    else
                        damage
            in
            if capped < 1000 && (Flags.has Defs.cfGodmode p.cheats || Player.power Invulnerability p /= 0) then
                Nothing

            else
                let
                    ( saved, armored ) =
                        if p.armortype /= 0 then
                            let
                                absorbs =
                                    if p.armortype == 1 then
                                        capped // 3

                                    else
                                        capped // 2
                            in
                            if p.armorpoints <= absorbs then
                                -- armor is used up
                                ( p.armorpoints, { p | armorpoints = 0, armortype = 0 } )

                            else
                                ( absorbs, { p | armorpoints = p.armorpoints - absorbs } )

                        else
                            ( 0, p )

                    dealt =
                        capped - saved

                    hurt =
                        { armored
                            | health = max 0 (armored.health - dealt)
                            , attacker = source
                            , damagecount = min 100 (armored.damagecount + dealt)
                        }
                in
                Just ( dealt, World.updatePlayer n (always hurt) world )

        _ ->
            Just ( damage, world )


{-| Take the damage off the thing's health; it dies, or may flinch (pain chance), wakes
up, and turns on its attacker unless it is busy with another (`threshold`). Arch-viles
are never targeted this way, and always switch to whoever hurt them.
-}
applyDamage : Env -> ThinkerId -> Maybe ThinkerId -> Int -> World -> World
applyDamage env targetId source damage world =
    case World.mobj targetId world of
        Nothing ->
            world

        Just target ->
            let
                health =
                    Int32.sub target.health damage

                hurt =
                    World.setMobj { target | health = health } world
            in
            if health <= 0 then
                killMobj env source targetId hurt

            else
                let
                    info =
                        Env.mobjInfo env target.type_

                    ( r, rolled ) =
                        World.pRandom hurt

                    flinched =
                        if r < info.painchance && not (Flags.has Flags.mfSkullFly target.flags) then
                            -- fight back!
                            rolled
                                |> World.updateMobj targetId (\m -> { m | flags = Flags.set Flags.mfJustHit m.flags })
                                |> Env.setMobjState env targetId info.painstate
                                |> Tuple.second

                        else
                            rolled

                    -- we're awake now...
                    awake =
                        World.updateMobj targetId (\m -> { m | reactiontime = 0 }) flinched
                in
                retaliate env targetId source awake


retaliate : Env -> ThinkerId -> Maybe ThinkerId -> World -> World
retaliate env targetId source world =
    case ( World.mobj targetId world, source |> Maybe.andThen (\s -> World.mobj s world) ) of
        ( Just target, Just src ) ->
            if
                (target.threshold == 0 || target.type_ == MobjType.Vile)
                    && (src.id /= targetId)
                    && (src.type_ /= MobjType.Vile)
            then
                let
                    info =
                        Env.mobjInfo env target.type_

                    -- if not intent on another player, chase after this one
                    chasing =
                        World.setMobj { target | target = Just src.id, threshold = Defs.baseThreshold } world
                in
                if target.state == info.spawnstate && info.seestate /= State.Null then
                    Tuple.second (Env.setMobjState env targetId info.seestate chasing)

                else
                    chasing

            else
                world

        _ ->
            world
