module Doom.Play.Spawn exposing (spawnPlayer, spawnMapThing)

{-| Spawning players and the map's things (P\_SpawnPlayer and P\_SpawnMapThing from
p\_mobj.c), with g\_game.c's G\_PlayerReborn for a player starting afresh.

@docs spawnPlayer, spawnMapThing

-}

import Array exposing (Array)
import Doom.Defs as Defs exposing (Ammo(..), PlayerState(..), Skill(..), Weapon(..))
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Info.MobjType as MobjType
import Doom.Int32 as Int32
import Doom.Level as Level exposing (MapThing)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Mobj as PMobj
import Doom.Play.Weapon as Weapon
import Doom.Player as Player exposing (Player)
import Doom.World as World exposing (World)


{-| P\_SpawnPlayer: put a player in the game at a start spot (the map thing's type is
the player's number plus one). A player who died starts afresh (G\_PlayerReborn);
otherwise they keep what they had. Players other than the first get their colour
(the translation bits of their body's flags); in deathmatch everyone has every key.

The status bar and message display restarts (ST\_Start, HU\_Start) are not part of the
game's state and are left to whoever draws them.

-}
spawnPlayer : Env -> MapThing -> World -> World
spawnPlayer env mthing world =
    let
        n =
            mthing.type_ - 1
    in
    if Array.get n world.playeringame /= Just True then
        -- not playing
        world

    else
        let
            reborn =
                case World.player n world of
                    Just p ->
                        if p.playerstate == Reborn then
                            World.updatePlayer n playerReborn world

                        else
                            world

                    Nothing ->
                        world

            ( mo, spawned ) =
                PMobj.spawnMobj env (Fixed.fromInt mthing.x) (Fixed.fromInt mthing.y) Defs.onFloorZ MobjType.Player reborn

            health =
                World.player n spawned |> Maybe.map .health |> Maybe.withDefault 0

            body =
                World.updateMobj mo
                    (\m ->
                        { m
                            | flags =
                                -- colour translation for player sprites
                                if mthing.type_ > 1 then
                                    Flags.set (Int32.shiftLeft Flags.mfTransShift (mthing.type_ - 1)) m.flags

                                else
                                    m.flags
                            , angle = PMobj.spawnAngle mthing
                            , player = Just n
                            , health = health
                        }
                    )
                    spawned

            live =
                World.updatePlayer n
                    (\p ->
                        { p
                            | mo = Just mo
                            , playerstate = Live
                            , refire = 0
                            , message = Nothing
                            , damagecount = 0
                            , bonuscount = 0
                            , extralight = 0
                            , fixedcolormap = 0
                            , viewheight = Defs.viewHeight
                        }
                    )
                    body

            -- setup gun psprite
            armed =
                Weapon.setupPsprites env n live
        in
        if env.params.deathmatch /= 0 then
            -- give all cards in death match mode
            World.updatePlayer n (\p -> { p | cards = Array.repeat 6 True }) armed

        else
            armed


{-| G\_PlayerReborn: a player starting afresh: everything cleared, then 100% health and
a pistol with 50 bullets. Frags, kills, items and secrets found are kept.
-}
playerReborn : Player -> Player
playerReborn old =
    let
        cleared =
            Player.new
    in
    { cleared
        | frags = old.frags
        , killcount = old.killcount
        , itemcount = old.itemcount
        , secretcount = old.secretcount
        , usedown = True
        , attackdown = True
        , playerstate = Live
        , health = Defs.maxHealth
        , readyweapon = Pistol
        , pendingweapon = Just Pistol
        , maxammo = Array.fromList (List.map Defs.maxAmmo Defs.allAmmo)
    }
        |> Player.setWeaponOwned Fist True
        |> Player.setWeaponOwned Pistol True
        |> Player.setAmmo Clip 50


{-| P\_SpawnMapThing: spawn one of the map's things, if it belongs in this game.

  - Deathmatch starts (type 11) are kept in `World.deathmatchStarts` (the first 10).
  - Player starts (types 1 to 4) are kept in `World.playerStarts`; outside deathmatch
    the player is spawned there.
  - Other things appear only on the skills their options say, not if they are for
    multiplayer only (and this isn't a net game), not keys in deathmatch, and not
    monsters with -nomonsters. Monsters and items count towards the level's totals;
    deaf ones are flagged ambush; each starts at a random point in its first state.

The C source stops the game for a thing type it doesn't know; this skips the thing.
A player start of type 0 or below (which the C source would write before the start of
its array) is skipped too.

-}
spawnMapThing : Env -> MapThing -> World -> World
spawnMapThing env mthing world =
    if mthing.type_ == 11 then
        if List.length world.deathmatchStarts < 10 then
            { world | deathmatchStarts = world.deathmatchStarts ++ [ mthing ] }

        else
            world

    else if mthing.type_ <= 4 then
        if mthing.type_ <= 0 then
            world

        else
            let
                -- save spots for respawning in network games
                saved =
                    { world | playerStarts = setPadded (mthing.type_ - 1) (Just mthing) Nothing world.playerStarts }
            in
            if env.params.deathmatch == 0 then
                spawnPlayer env mthing saved

            else
                saved

    else if not env.params.netgame && Flags.has 16 mthing.options then
        -- multiplayer only
        world

    else if not (Flags.has (skillBit env.params.skill) mthing.options) then
        world

    else
        case PMobj.typeForDoomednum mthing.type_ of
            Nothing ->
                -- I_Error ("P_SpawnMapThing: Unknown type"): the world is left as it was.
                world

            Just type_ ->
                let
                    info =
                        Env.mobjInfo env type_
                in
                if env.params.deathmatch /= 0 && Flags.has Flags.mfNotDeathmatch info.flags then
                    -- don't spawn keycards and players in deathmatch
                    world

                else if env.params.nomonsters && (type_ == MobjType.Skull || Flags.has Flags.mfCountKill info.flags) then
                    world

                else
                    spawnThing env mthing type_ world


spawnThing : Env -> MapThing -> MobjType.MobjType -> World -> World
spawnThing env mthing type_ world =
    let
        z =
            if Flags.has Flags.mfSpawnCeiling (Env.mobjInfo env type_).flags then
                Defs.onCeilingZ

            else
                Defs.onFloorZ

        ( id, spawned ) =
            PMobj.spawnMobj env (Fixed.fromInt mthing.x) (Fixed.fromInt mthing.y) z type_ world
    in
    case World.mobj id spawned of
        Nothing ->
            spawned

        Just mobj ->
            let
                ( tics, rolled ) =
                    if mobj.tics > 0 then
                        let
                            ( r, w ) =
                                World.pRandom spawned
                        in
                        ( 1 + remainderBy mobj.tics r, w )

                    else
                        ( mobj.tics, spawned )

                counted =
                    { rolled
                        | totalkills =
                            if Flags.has Flags.mfCountKill mobj.flags then
                                rolled.totalkills + 1

                            else
                                rolled.totalkills
                        , totalitems =
                            if Flags.has Flags.mfCountItem mobj.flags then
                                rolled.totalitems + 1

                            else
                                rolled.totalitems
                    }
            in
            World.updateMobj id
                (\m ->
                    { m
                        | spawnpoint = mthing
                        , tics = tics
                        , angle = PMobj.spawnAngle mthing
                        , flags =
                            if Flags.has Level.mtfAmbush mthing.options then
                                Flags.set Flags.mfAmbush m.flags

                            else
                                m.flags
                    }
                )
                counted


{-| The options bit for the skill: easy for the two easiest, hard for the two hardest.
-}
skillBit : Skill -> Int
skillBit skill =
    case skill of
        Baby ->
            1

        Nightmare ->
            4

        _ ->
            2 ^ (Defs.skillIndex skill - 1)


{-| `array[i] = value`, growing the array (with `default`) if it is shorter.
-}
setPadded : Int -> a -> a -> Array a -> Array a
setPadded i value default array =
    let
        grown =
            if i >= Array.length array then
                Array.append array (Array.repeat (i + 1 - Array.length array) default)

            else
                array
    in
    Array.set i value grown
