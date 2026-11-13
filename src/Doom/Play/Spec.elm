module Doom.Play.Spec exposing (crossSpecialLine, shootSpecialLine, playerInSpecialSector, updateSpecials, spawnSpecials, textureTranslation, flatTranslation)

{-| Line and sector specials (p\_spec.c): what crossing or shooting a line does, damaging
and secret sectors, scrolling walls, switches flipping back, and setting up a level's
specials. Animated textures and flats only change what is drawn: see
`textureTranslation` and `flatTranslation`.

The deathmatch level timer (`-timer`, `-avg`) isn't part of this port: nothing in the
game's settings asks for it, so P\_SpawnSpecials and P\_UpdateSpecials leave it out.

@docs crossSpecialLine, shootSpecialLine, playerInSpecialSector, updateSpecials, spawnSpecials, textureTranslation, flatTranslation

-}

import Array
import Bitwise
import Doom.Data exposing (Anim)
import Doom.Defs as Defs
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Ceiling as Ceiling
import Doom.Play.Door as Door
import Doom.Play.Env exposing (Env)
import Doom.Play.Floor as Floor
import Doom.Play.Inter as Inter
import Doom.Play.Light as Light
import Doom.Play.Plat as Plat
import Doom.Play.Switch as Switch
import Doom.Play.Teleport as Teleport
import Doom.Player as Player
import Doom.Sound as Sound
import Doom.Thinker as Thinker exposing (CeilingType(..), DoorType(..), FloorType(..), PlatType(..), StairType(..))
import Doom.World as World exposing (ButtonWhere(..), Exit(..), World)



-- CROSSING LINES


{-| P\_CrossSpecialLine linenum side thing: the thing crossed the line from `side` (0
front, 1 back). Monsters (and anything else that isn't a player, but not missiles)
only set off teleporters, raise doors (4) and lifts (10, 88). One-time lines lose their
special; the exits don't.
-}
crossSpecialLine : Env -> LineId -> Int -> ThinkerId -> World -> World
crossSpecialLine env lineId side thingId world =
    case World.mobj thingId world of
        Nothing ->
            world

        Just thing ->
            let
                special =
                    (World.lineState lineId world).special

                isPlayer =
                    thing.player /= Nothing
            in
            if not isPlayer && (isMissile thing.type_ || not (List.member special monsterCrossable)) then
                world

            else
                cross env lineId side thingId isPlayer special world


isMissile : MobjType.MobjType -> Bool
isMissile t =
    List.member t [ MobjType.Rocket, MobjType.Plasma, MobjType.Bfg, MobjType.Troopshot, MobjType.Headshot, MobjType.Bruisershot ]


monsterCrossable : List Int
monsterCrossable =
    [ 39, 97, 125, 126, 4, 10, 88 ]


clearSpecial : LineId -> World -> World
clearSpecial lineId =
    World.updateLine lineId (\l -> { l | special = 0 })


cross : Env -> LineId -> Int -> ThinkerId -> Bool -> Int -> World -> World
cross env lineId side thingId isPlayer special world =
    let
        door t w =
            Door.doDoor env lineId t w |> Tuple.second

        floor t w =
            Floor.doFloor env lineId t w |> Tuple.second

        plat t w =
            Plat.doPlat env lineId t 0 w |> Tuple.second

        ceiling t w =
            Ceiling.doCeiling env lineId t w |> Tuple.second

        teleport w =
            Teleport.teleport env lineId side thingId w |> Tuple.second

        once action =
            action world |> clearSpecial lineId

        again action =
            action world
    in
    case special of
        -- TRIGGERS
        2 ->
            once (door OpenDoor)

        3 ->
            once (door CloseDoor)

        4 ->
            once (door NormalDoor)

        5 ->
            once (floor RaiseFloor)

        6 ->
            once (ceiling FastCrushAndRaise)

        8 ->
            once (\w -> Floor.buildStairs env lineId Build8 w |> Tuple.second)

        10 ->
            once (plat DownWaitUpStay)

        12 ->
            once (Light.lightTurnOn env lineId 0)

        13 ->
            once (Light.lightTurnOn env lineId 255)

        16 ->
            once (door Close30ThenOpen)

        17 ->
            once (Light.startLightStrobing env lineId)

        19 ->
            once (floor LowerFloor)

        22 ->
            once (plat RaiseToNearestAndChange)

        25 ->
            once (ceiling CrushAndRaise)

        30 ->
            once (floor RaiseToTexture)

        35 ->
            once (Light.lightTurnOn env lineId 35)

        36 ->
            once (floor TurboLower)

        37 ->
            once (floor LowerAndChange)

        38 ->
            once (floor LowerFloorToLowest)

        39 ->
            once teleport

        40 ->
            once (ceiling RaiseToHighest >> floor LowerFloorToLowest)

        44 ->
            once (ceiling LowerAndCrush)

        52 ->
            { world | exit = Just NormalExit }

        53 ->
            once (plat PerpetualRaise)

        54 ->
            once (Plat.stopPlat env lineId)

        56 ->
            once (floor RaiseFloorCrush)

        57 ->
            once (\w -> Ceiling.ceilingCrushStop env lineId w |> Tuple.second)

        58 ->
            once (floor RaiseFloor24)

        59 ->
            once (floor RaiseFloor24AndChange)

        104 ->
            once (Light.turnTagLightsOff env lineId)

        108 ->
            once (door BlazeRaise)

        109 ->
            once (door BlazeOpen)

        100 ->
            once (\w -> Floor.buildStairs env lineId Turbo16 w |> Tuple.second)

        110 ->
            once (door BlazeClose)

        119 ->
            once (floor RaiseFloorToNearest)

        121 ->
            once (plat BlazeDWUS)

        124 ->
            { world | exit = Just SecretExit }

        125 ->
            if isPlayer then
                world

            else
                once teleport

        130 ->
            once (floor RaiseFloorTurbo)

        141 ->
            once (ceiling SilentCrushAndRaise)

        -- RETRIGGERS
        72 ->
            again (ceiling LowerAndCrush)

        73 ->
            again (ceiling CrushAndRaise)

        74 ->
            again (\w -> Ceiling.ceilingCrushStop env lineId w |> Tuple.second)

        75 ->
            again (door CloseDoor)

        76 ->
            again (door Close30ThenOpen)

        77 ->
            again (ceiling FastCrushAndRaise)

        79 ->
            again (Light.lightTurnOn env lineId 35)

        80 ->
            again (Light.lightTurnOn env lineId 0)

        81 ->
            again (Light.lightTurnOn env lineId 255)

        82 ->
            again (floor LowerFloorToLowest)

        83 ->
            again (floor LowerFloor)

        84 ->
            again (floor LowerAndChange)

        86 ->
            again (door OpenDoor)

        87 ->
            again (plat PerpetualRaise)

        88 ->
            again (plat DownWaitUpStay)

        89 ->
            again (Plat.stopPlat env lineId)

        90 ->
            again (door NormalDoor)

        91 ->
            again (floor RaiseFloor)

        92 ->
            again (floor RaiseFloor24)

        93 ->
            again (floor RaiseFloor24AndChange)

        94 ->
            again (floor RaiseFloorCrush)

        95 ->
            again (plat RaiseToNearestAndChange)

        96 ->
            again (floor RaiseToTexture)

        97 ->
            again teleport

        98 ->
            again (floor TurboLower)

        105 ->
            again (door BlazeRaise)

        106 ->
            again (door BlazeOpen)

        107 ->
            again (door BlazeClose)

        120 ->
            again (plat BlazeDWUS)

        126 ->
            if isPlayer then
                world

            else
                again teleport

        128 ->
            again (floor RaiseFloorToNearest)

        129 ->
            again (floor RaiseFloorTurbo)

        _ ->
            world



-- SHOOTING LINES


{-| P\_ShootSpecialLine thing line: a hitscan attack hit the line. Monsters only open
doors this way (46).
-}
shootSpecialLine : Env -> ThinkerId -> LineId -> World -> World
shootSpecialLine env thingId lineId world =
    let
        special =
            (World.lineState lineId world).special

        isPlayer =
            (World.mobj thingId world |> Maybe.andThen .player) /= Nothing
    in
    if not isPlayer && special /= 46 then
        world

    else
        case special of
            24 ->
                Floor.doFloor env lineId RaiseFloor world
                    |> Tuple.second
                    |> Switch.changeSwitchTexture env lineId False

            46 ->
                Door.doDoor env lineId OpenDoor world
                    |> Tuple.second
                    |> Switch.changeSwitchTexture env lineId True

            47 ->
                Plat.doPlat env lineId RaiseToNearestAndChange 0 world
                    |> Tuple.second
                    |> Switch.changeSwitchTexture env lineId False

            _ ->
                world



-- SECTORS


{-| P\_PlayerInSpecialSector player: the player's body is on the floor of a sector with
a special: slime and lava hurt every 32 tics (unless the radiation suit protects),
a secret counts, and E1M8's exit sector (11) hurts, takes away god mode, and ends the
level once health is down to 10. (C stops the game for any other special; then
nothing happens.)
-}
playerInSpecialSector : Env -> Int -> World -> World
playerInSpecialSector env pnum world =
    case ( World.player pnum world, World.playerMobj pnum world ) of
        ( Just p, Just mo ) ->
            let
                secId =
                    World.mobjSector env.level mo

                sec =
                    World.sector secId world

                tick =
                    Bitwise.and world.leveltime 0x1F == 0

                ironfeet =
                    Player.power Defs.IronFeet p /= 0

                hurt damage w =
                    if tick then
                        Inter.damageMobj env mo.id Nothing Nothing damage w

                    else
                        w
            in
            if mo.z /= sec.floorheight then
                world

            else
                case sec.special of
                    5 ->
                        if ironfeet then
                            world

                        else
                            hurt 10 world

                    7 ->
                        if ironfeet then
                            world

                        else
                            hurt 5 world

                    16 ->
                        superHurt ironfeet hurt world

                    4 ->
                        superHurt ironfeet hurt world

                    9 ->
                        world
                            |> World.updatePlayer pnum (\pl -> { pl | secretcount = pl.secretcount + 1 })
                            |> World.updateSector secId (\s -> { s | special = 0 })

                    11 ->
                        let
                            hurtWorld =
                                world
                                    |> World.updatePlayer pnum (\pl -> { pl | cheats = Flags.clear Defs.cfGodmode pl.cheats })
                                    |> hurt 20

                            health =
                                World.player pnum hurtWorld |> Maybe.map .health |> Maybe.withDefault 0
                        in
                        if health <= 10 then
                            { hurtWorld | exit = Just NormalExit }

                        else
                            hurtWorld

                    _ ->
                        world

        _ ->
            world


{-| Super hellslime (16) and strobe hurt (4): even with the suit, a 5 in 256 chance of
hurting (P\_Random is only called when the suit is on).
-}
superHurt : Bool -> (Int -> World -> World) -> World -> World
superHurt ironfeet hurt world =
    if not ironfeet then
        hurt 20 world

    else
        let
            ( r, rolled ) =
                World.pRandom world
        in
        if r < 5 then
            hurt 20 rolled

        else
            rolled



-- EVERY TIC


{-| P\_UpdateSpecials: scroll the scrolling walls (line special 48) one unit, and count
down pressed buttons, flipping their texture back with a click when time is up.
Texture animation is `textureTranslation`.
-}
updateSpecials : Env -> World -> World
updateSpecials env world =
    world
        |> scrollWalls env
        |> countDownButtons env


scrollWalls : Env -> World -> World
scrollWalls env world =
    List.foldl
        (\lineId w ->
            if (World.lineState lineId w).special == 48 then
                World.updateSide (Level.line lineId env.level).frontSide
                    (\s -> { s | textureoffset = Fixed.add s.textureoffset Fixed.fracunit })
                    w

            else
                w
        )
        world
        world.lineSpecialList


{-| The click comes from `Doom.Sound.FromButton i`: C passes the address of the
button's `soundorg` field itself, not the sector origin it points to.
-}
countDownButtons : Env -> World -> World
countDownButtons env world =
    List.foldl
        (\i w ->
            case Array.get i w.buttons of
                Just (Just b) ->
                    if b.btimer == 0 then
                        w

                    else if b.btimer - 1 /= 0 then
                        { w | buttons = Array.set i (Just { b | btimer = b.btimer - 1 }) w.buttons }

                    else
                        let
                            sideId =
                                (Level.line b.line env.level).frontSide

                            restore s =
                                case b.where_ of
                                    ButtonTop ->
                                        { s | toptexture = b.btexture }

                                    ButtonMiddle ->
                                        { s | midtexture = b.btexture }

                                    ButtonBottom ->
                                        { s | bottomtexture = b.btexture }
                        in
                        w
                            |> World.updateSide sideId restore
                            |> World.startSound (Sound.FromButton i) Sfx.Swtchn
                            |> (\w2 -> { w2 | buttons = Array.set i Nothing w2.buttons })

                _ ->
                    w
        )
        world
        (List.range 0 (Array.length world.buttons - 1))



-- LEVEL START


{-| P\_SpawnSpecials: after the level is loaded, start the sector specials' thinkers
(lights, timed doors), count the secrets, list the scrolling walls, and empty the
active plat, ceiling and button lists.
-}
spawnSpecials : Env -> World -> World
spawnSpecials env world =
    let
        withSectors =
            List.foldl (spawnSectorSpecial env) world (List.range 0 (Array.length world.sectors - 1))

        scrolling =
            Array.toIndexedList withSectors.lines
                |> List.filterMap
                    (\( i, l ) ->
                        if l.special == 48 then
                            Just i

                        else
                            Nothing
                    )
    in
    { withSectors
        | lineSpecialList = scrolling
        , activeCeilings = Array.repeat Thinker.maxCeilings Nothing
        , activePlats = Array.repeat Thinker.maxPlats Nothing
        , buttons = Array.repeat World.maxButtons Nothing
    }


spawnSectorSpecial : Env -> Int -> World -> World
spawnSectorSpecial env sec world =
    case (World.sector sec world).special of
        1 ->
            Light.spawnLightFlash env sec world

        2 ->
            Light.spawnStrobeFlash env sec Thinker.fastDark False world

        3 ->
            Light.spawnStrobeFlash env sec Thinker.slowDark False world

        4 ->
            Light.spawnStrobeFlash env sec Thinker.fastDark False world
                |> World.updateSector sec (\s -> { s | special = 4 })

        8 ->
            Light.spawnGlowingLight env sec world

        9 ->
            { world | totalsecret = world.totalsecret + 1 }

        10 ->
            Door.spawnDoorCloseIn30 env sec world

        12 ->
            Light.spawnStrobeFlash env sec Thinker.slowDark True world

        13 ->
            Light.spawnStrobeFlash env sec Thinker.fastDark True world

        14 ->
            Door.spawnDoorRaiseIn5Mins env sec world

        17 ->
            Light.spawnFireFlicker env sec world

        _ ->
            world



-- ANIMATION


{-| `texturetranslation[texture]` as P\_UpdateSpecials leaves it at `leveltime`: the
texture actually drawn for a wall texture, `texture` itself unless it is part of an
animation.
-}
textureTranslation : Env -> Int -> Int -> Int
textureTranslation env leveltime tex =
    translate True env.data.anims leveltime tex


{-| `flattranslation[flat]` as P\_UpdateSpecials leaves it at `leveltime`.
-}
flatTranslation : Env -> Int -> Int -> Int
flatTranslation env leveltime flat =
    translate False env.data.anims leveltime flat


{-| For each animation of the kind (later ones overriding earlier), pic `i` in
[basepic, basepic + numpics) becomes `basepic + (leveltime / speed + i) % numpics`
(with `i` the pic's own number, as in the C source).
-}
translate : Bool -> List Anim -> Int -> Int -> Int
translate istexture anims leveltime pic =
    List.foldl
        (\anim result ->
            if anim.istexture == istexture && pic >= anim.basepic && pic < anim.basepic + anim.numpics then
                anim.basepic + remainderBy anim.numpics (leveltime // anim.speed + pic)

            else
                result
        )
        pic
        anims
