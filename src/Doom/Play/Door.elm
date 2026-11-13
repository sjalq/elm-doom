module Doom.Play.Door exposing (think, doLockedDoor, doDoor, doDoorByTag, verticalDoor, spawnDoorCloseIn30, spawnDoorRaiseIn5Mins)

{-| Doors (p\_doors.c, without its unused sliding doors).

A door moves its sector's ceiling. Its thinker is the sector's `specialdata` while it
runs, which is how a second push on a door finds the door already moving.

Fields the C source leaves as whatever Z\_Malloc's memory held (a door's
`topcountdown` before it first waits; `topheight` and `topwait` of a door that closes
in 30 seconds) start at 0 here.

@docs think, doLockedDoor, doDoor, doDoorByTag, verticalDoor, spawnDoorCloseIn30, spawnDoorRaiseIn5Mins

-}

import Doom.Defs exposing (Card(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.Plane as Plane exposing (PlaneResult(..))
import Doom.Play.SpecUtil as SpecUtil exposing (sectorSound)
import Doom.Player as Player exposing (Player)
import Doom.Sound as Sound
import Doom.Thinker as Thinker exposing (Door, DoorType(..), Thinker(..))
import Doom.World as World exposing (World)


{-| T\_VerticalDoor: one tic of a door. `direction` 0 waits at the top, 2 waits to
open, -1 closes, 1 opens.
-}
think : Env -> ThinkerId -> Door -> World -> World
think env id door world =
    if door.direction == 0 then
        waitAtTop id door world

    else if door.direction == 2 then
        waitToOpen id door world

    else if door.direction == -1 then
        moveDown env id door world

    else if door.direction == 1 then
        moveUp env id door world

    else
        world


save : ThinkerId -> Door -> World -> World
save id door =
    World.setThinker id (DoorThinker door)


{-| The door is done: `sector->specialdata = NULL; P_RemoveThinker`.
-}
finish : ThinkerId -> Door -> World -> World
finish id door world =
    world
        |> World.updateSector door.sector (\s -> { s | specialdata = Nothing })
        |> World.removeThinker id


waitAtTop : ThinkerId -> Door -> World -> World
waitAtTop id door world =
    let
        counted =
            { door | topcountdown = door.topcountdown - 1 }
    in
    if counted.topcountdown /= 0 then
        save id counted world

    else
        case door.type_ of
            BlazeRaise ->
                world
                    |> save id { counted | direction = -1 }
                    |> sectorSound door.sector Sfx.Bdcls

            NormalDoor ->
                world
                    |> save id { counted | direction = -1 }
                    |> sectorSound door.sector Sfx.Dorcls

            Close30ThenOpen ->
                world
                    |> save id { counted | direction = 1 }
                    |> sectorSound door.sector Sfx.Doropn

            _ ->
                save id counted world


waitToOpen : ThinkerId -> Door -> World -> World
waitToOpen id door world =
    let
        counted =
            { door | topcountdown = door.topcountdown - 1 }
    in
    if counted.topcountdown /= 0 then
        save id counted world

    else
        case door.type_ of
            RaiseIn5Mins ->
                world
                    |> save id { counted | direction = 1, type_ = NormalDoor }
                    |> sectorSound door.sector Sfx.Doropn

            _ ->
                save id counted world


moveDown : Env -> ThinkerId -> Door -> World -> World
moveDown env id door world =
    let
        ( res, moved ) =
            Plane.movePlane env door.sector door.speed (World.sector door.sector world).floorheight False 1 door.direction world
    in
    case res of
        PlanePastDest ->
            case door.type_ of
                BlazeRaise ->
                    moved |> finish id door |> sectorSound door.sector Sfx.Bdcls

                BlazeClose ->
                    moved |> finish id door |> sectorSound door.sector Sfx.Bdcls

                NormalDoor ->
                    finish id door moved

                CloseDoor ->
                    finish id door moved

                Close30ThenOpen ->
                    save id { door | direction = 0, topcountdown = 35 * 30 } moved

                _ ->
                    moved

        PlaneCrushed ->
            case door.type_ of
                BlazeClose ->
                    moved

                CloseDoor ->
                    -- Do not go back up!
                    moved

                _ ->
                    moved
                        |> save id { door | direction = 1 }
                        |> sectorSound door.sector Sfx.Doropn

        PlaneOk ->
            moved


moveUp : Env -> ThinkerId -> Door -> World -> World
moveUp env id door world =
    let
        ( res, moved ) =
            Plane.movePlane env door.sector door.speed door.topheight False 1 door.direction world
    in
    case res of
        PlanePastDest ->
            case door.type_ of
                BlazeRaise ->
                    save id { door | direction = 0, topcountdown = door.topwait } moved

                NormalDoor ->
                    save id { door | direction = 0, topcountdown = door.topwait } moved

                Close30ThenOpen ->
                    finish id door moved

                BlazeOpen ->
                    finish id door moved

                OpenDoor ->
                    finish id door moved

                _ ->
                    moved

        _ ->
            moved



-- STARTING DOORS


{-| A new door thinker for a sector, as EV\_DoDoor and EV\_VerticalDoor first fill it
in.
-}
newDoor : DoorType -> SectorId -> Door
newDoor type_ sec =
    { type_ = type_
    , sector = sec
    , topheight = 0
    , speed = Thinker.vdoorSpeed
    , direction = 0
    , topwait = Thinker.vdoorWait
    , topcountdown = 0
    }


{-| P\_AddThinker and `sec->specialdata = door`.
-}
addDoor : Door -> World -> World
addDoor door world =
    let
        ( id, added ) =
            World.addThinker (DoorThinker door) world
    in
    World.updateSector door.sector (\s -> { s | specialdata = Just id }) added


{-| The lowest ceiling around the sector, less 4: where doors open to.
-}
doorTop : Env -> SectorId -> World -> Fixed
doorTop env sec world =
    Fixed.sub (SpecUtil.findLowestCeilingSurrounding env sec world) (4 * Fixed.fracunit)


{-| EV\_DoLockedDoor line type thing: EV\_DoDoor, if the thing is a player holding the
key the line asks for (lines 99 and 133 blue, 134 and 135 red, 136 and 137 yellow).
Without it the player is told so and grunts.
-}
doLockedDoor : Env -> LineId -> DoorType -> ThinkerId -> World -> ( Bool, World )
doLockedDoor env lineId type_ thing world =
    case playerOf thing world of
        Nothing ->
            ( False, world )

        Just ( pnum, p ) ->
            let
                refuse message =
                    ( False, refuseLocked pnum message world )
            in
            case (World.lineState lineId world).special of
                99 ->
                    if hasKey BlueCard BlueSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a blue key to activate this object"

                133 ->
                    if hasKey BlueCard BlueSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a blue key to activate this object"

                134 ->
                    if hasKey RedCard RedSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a red key to activate this object"

                135 ->
                    if hasKey RedCard RedSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a red key to activate this object"

                136 ->
                    if hasKey YellowCard YellowSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a yellow key to activate this object"

                137 ->
                    if hasKey YellowCard YellowSkull p then
                        doDoor env lineId type_ world

                    else
                        refuse "You need a yellow key to activate this object"

                _ ->
                    doDoor env lineId type_ world


playerOf : ThinkerId -> World -> Maybe ( Int, Player )
playerOf thing world =
    World.mobj thing world
        |> Maybe.andThen .player
        |> Maybe.andThen (\n -> World.player n world |> Maybe.map (Tuple.pair n))


hasKey : Card -> Card -> Player -> Bool
hasKey card skull p =
    Player.hasCard card p || Player.hasCard skull p


{-| `p->message = ...; S_StartSound(NULL, sfx_oof)`.
-}
refuseLocked : Int -> String -> World -> World
refuseLocked pnum message world =
    world
        |> World.updatePlayer pnum (\p -> { p | message = Just message })
        |> World.startSound Sound.Ambient Sfx.Oof


{-| EV\_DoDoor: start a door in every sector with the line's tag that has nothing
moving yet. True if one started.
-}
doDoor : Env -> LineId -> DoorType -> World -> ( Bool, World )
doDoor env lineId type_ world =
    doDoorByTag env (Level.line lineId env.level).tag type_ world


{-| EV\_DoDoor for a tag rather than a line: what A\_BossDeath and A\_KeenDie do with
their made-up line that has only a tag.
-}
doDoorByTag : Env -> Int -> DoorType -> World -> ( Bool, World )
doDoorByTag env tag type_ world =
    SpecUtil.foldTaggedSectors tag
        (\sec ( rtn, w ) ->
            if (World.sector sec w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( True, startDoor env type_ sec w )
        )
        world


startDoor : Env -> DoorType -> SectorId -> World -> World
startDoor env type_ sec world =
    let
        door =
            newDoor type_ sec

        ceiling =
            (World.sector sec world).ceilingheight

        opening speed sfx =
            let
                top =
                    doorTop env sec world

                started =
                    addDoor { door | direction = 1, topheight = top, speed = speed } world
            in
            if top /= ceiling then
                sectorSound sec sfx started

            else
                started
    in
    case type_ of
        BlazeClose ->
            world
                |> addDoor { door | topheight = doorTop env sec world, direction = -1, speed = Thinker.vdoorSpeed * 4 }
                |> sectorSound sec Sfx.Bdcls

        CloseDoor ->
            world
                |> addDoor { door | topheight = doorTop env sec world, direction = -1 }
                |> sectorSound sec Sfx.Dorcls

        Close30ThenOpen ->
            world
                |> addDoor { door | topheight = ceiling, direction = -1 }
                |> sectorSound sec Sfx.Dorcls

        BlazeRaise ->
            opening (Thinker.vdoorSpeed * 4) Sfx.Bdopn

        BlazeOpen ->
            opening (Thinker.vdoorSpeed * 4) Sfx.Bdopn

        NormalDoor ->
            opening Thinker.vdoorSpeed Sfx.Doropn

        OpenDoor ->
            opening Thinker.vdoorSpeed Sfx.Doropn

        RaiseIn5Mins ->
            -- Not a type EV_DoDoor is given; C leaves direction and topheight unset.
            addDoor door world


{-| EV\_VerticalDoor line thing: a door used by hand (it opens the sector behind the
line). Locked doors (26 and 32 blue, 27 and 34 yellow, 28 and 33 red) need the key. A
raise door that is already moving turns around: a closing one opens again, an open one
closes, though only players close doors.
-}
verticalDoor : Env -> LineId -> ThinkerId -> World -> World
verticalDoor env lineId thing world =
    let
        special =
            (World.lineState lineId world).special

        player =
            playerOf thing world

        needKey card skull message =
            case player of
                Nothing ->
                    world

                Just ( pnum, p ) ->
                    if hasKey card skull p then
                        openVerticalDoor env lineId special (player /= Nothing) world

                    else
                        refuseLocked pnum message world
    in
    case special of
        26 ->
            needKey BlueCard BlueSkull "You need a blue key to open this door"

        32 ->
            needKey BlueCard BlueSkull "You need a blue key to open this door"

        27 ->
            needKey YellowCard YellowSkull "You need a yellow key to open this door"

        34 ->
            needKey YellowCard YellowSkull "You need a yellow key to open this door"

        28 ->
            needKey RedCard RedSkull "You need a red key to open this door"

        33 ->
            needKey RedCard RedSkull "You need a red key to open this door"

        _ ->
            openVerticalDoor env lineId special (player /= Nothing) world


openVerticalDoor : Env -> LineId -> Int -> Bool -> World -> World
openVerticalDoor env lineId special isPlayer world =
    case (Level.line lineId env.level).backSide of
        Nothing ->
            -- C reads sides[-1] here (a door line without a back side): undefined; do nothing.
            world

        Just backSide ->
            let
                sec =
                    (Level.side backSide env.level).sector
            in
            case ( (World.sector sec world).specialdata, isRaiseDoor special ) of
                ( Just doorId, True ) ->
                    reverseDoor doorId isPlayer world

                _ ->
                    newVerticalDoor env lineId special sec world


isRaiseDoor : Int -> Bool
isRaiseDoor special =
    List.member special [ 1, 26, 27, 28, 117 ]


{-| A raise door already moving: closing turns to opening; otherwise a player makes
it start closing.

The C source assumes `specialdata` is a door. When it is a lift, floor or ceiling
moving the same sector, `door->direction` reads and writes whatever field of that
thinker lies at the offset of a door's `direction` (28) in the DOS executable's 32-bit
layout: a lift's `wait`, a floor's `newspecial`, a ceiling's `speed` (chocolate-doom
p\_doors.c:EV\_VerticalDoor sets a lift's `wait` to -1 for this reason; linuxdoom-1.10
on 64-bit hits another field).

-}
reverseDoor : ThinkerId -> Bool -> World -> World
reverseDoor doorId isPlayer world =
    let
        reverse direction set =
            if direction == -1 then
                World.setThinker doorId (set 1) world

            else if not isPlayer then
                -- JDC: bad guys never close doors
                world

            else
                World.setThinker doorId (set -1) world
    in
    case World.thinker doorId world of
        Just (DoorThinker door) ->
            reverse door.direction (\d -> DoorThinker { door | direction = d })

        Just (PlatThinker plat) ->
            reverse plat.wait (\d -> PlatThinker { plat | wait = d })

        Just (FloorThinker floor) ->
            reverse floor.newspecial (\d -> FloorThinker { floor | newspecial = d })

        Just (CeilingThinker ceiling) ->
            reverse ceiling.speed (\d -> CeilingThinker { ceiling | speed = d })

        _ ->
            world


newVerticalDoor : Env -> LineId -> Int -> SectorId -> World -> World
newVerticalDoor env lineId special sec world =
    let
        sound =
            case special of
                117 ->
                    Sfx.Bdopn

                118 ->
                    Sfx.Bdopn

                _ ->
                    Sfx.Doropn

        base =
            newDoor NormalDoor sec

        ( door, clearSpecial ) =
            case special of
                31 ->
                    ( { base | type_ = OpenDoor }, True )

                32 ->
                    ( { base | type_ = OpenDoor }, True )

                33 ->
                    ( { base | type_ = OpenDoor }, True )

                34 ->
                    ( { base | type_ = OpenDoor }, True )

                117 ->
                    ( { base | type_ = BlazeRaise, speed = Thinker.vdoorSpeed * 4 }, False )

                118 ->
                    ( { base | type_ = BlazeOpen, speed = Thinker.vdoorSpeed * 4 }, True )

                _ ->
                    ( base, False )

        sounded =
            sectorSound sec sound world

        cleared =
            if clearSpecial then
                World.updateLine lineId (\l -> { l | special = 0 }) sounded

            else
                sounded
    in
    addDoor { door | direction = 1, topheight = doorTop env sec cleared } cleared


{-| P\_SpawnDoorCloseIn30: sector special 10, a door that closes 30 seconds into the
level (and stays closed).
-}
spawnDoorCloseIn30 : Env -> SectorId -> World -> World
spawnDoorCloseIn30 _ sec world =
    let
        door =
            newDoor NormalDoor sec
    in
    world
        |> addDoor { door | direction = 0, topwait = 0, topcountdown = 30 * 35 }
        |> World.updateSector sec (\s -> { s | special = 0 })


{-| P\_SpawnDoorRaiseIn5Mins: sector special 14, a door that opens five minutes into
the level, then works as a normal door.
-}
spawnDoorRaiseIn5Mins : Env -> SectorId -> World -> World
spawnDoorRaiseIn5Mins env sec world =
    let
        door =
            newDoor RaiseIn5Mins sec
    in
    world
        |> addDoor { door | direction = 2, topheight = doorTop env sec world, topcountdown = 5 * 60 * 35 }
        |> World.updateSector sec (\s -> { s | special = 0 })
