module Doom.Play.Mobj exposing (spawnMobj, removeMobj, respawnSpecials, spawnPuff, spawnBlood, typeForDoomednum, spawnAngle)

{-| Creating and removing things (the lower half of p\_mobj.c), and the deathmatch item
respawn queue. Moving them is `Doom.Play.Movement`; spawning players and map things is
`Doom.Play.Spawn`.

@docs spawnMobj, removeMobj, respawnSpecials, spawnPuff, spawnBlood, typeForDoomednum, spawnAngle

-}

import Array exposing (Array)
import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (Skill(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.Sfx as Sfx
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Level as Level exposing (MapThing)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.MapUtil as MapUtil
import Doom.Sound as Sound
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (ItemRespawn, World)


{-| P\_SpawnMobj x y z type: a new thing of this type, in its spawn state, linked into
the blockmap and its sector, and added at the end of the thinker list. `z` may be
`Defs.onFloorZ` or `Defs.onCeilingZ` to put it on the floor or hang it from the
ceiling of the sector it lands in.

Its state is set without running the state's action, as in the C source. The thing's id
is the id its thinker gets: nothing adds a thinker between choosing the id here and
P\_AddThinker at the end (P\_SetThingPosition only links it).

-}
spawnMobj : Env -> Fixed -> Fixed -> Fixed -> MobjType -> World -> ( ThinkerId, World )
spawnMobj env x y z type_ world =
    let
        info =
            Env.mobjInfo env type_

        st =
            Env.stateDef env info.spawnstate

        ( lastlook, world1 ) =
            World.pRandom world

        id =
            world1.nextThinkerId

        fresh : Mobj
        fresh =
            { id = id
            , x = x
            , y = y
            , z = 0
            , angle = 0
            , sprite = st.sprite
            , frame = st.frame
            , subsector = 0
            , floorz = 0
            , ceilingz = 0
            , radius = info.radius
            , height = info.height
            , momx = 0
            , momy = 0
            , momz = 0
            , validcount = 0
            , type_ = type_
            , tics = st.tics
            , state = info.spawnstate
            , flags = info.flags
            , health = info.spawnhealth
            , movedir = 0
            , movecount = 0
            , target = Nothing
            , reactiontime =
                if env.params.skill /= Nightmare then
                    info.reactiontime

                else
                    0
            , threshold = 0
            , player = Nothing
            , lastlook = remainderBy Defs.maxPlayers lastlook
            , spawnpoint = noMapThing
            , tracer = Nothing
            }

        world2 =
            world1
                |> World.setMobj fresh
                |> MapUtil.setThingPosition env id

        placed =
            World.mobj id world2 |> Maybe.withDefault fresh

        sec =
            World.sector (World.mobjSector env.level placed) world2

        newZ =
            if z == Defs.onFloorZ then
                sec.floorheight

            else if z == Defs.onCeilingZ then
                Int32.sub sec.ceilingheight info.height

            else
                z
    in
    world2
        |> World.setMobj { placed | floorz = sec.floorheight, ceilingz = sec.ceilingheight, z = newZ }
        |> World.addThinker MobjThinker


{-| P\_RemoveMobj: take the thing out of the game. A placed item (not dropped by a
monster, and not an invulnerability or invisibility sphere) goes on the item respawn
queue first, for deathmatch.

The thing's data stays in `World.mobjs`; its thinker becomes `Removed`.

-}
removeMobj : Env -> ThinkerId -> World -> World
removeMobj env id world =
    case World.mobj id world of
        Nothing ->
            world

        Just m ->
            let
                queued =
                    if
                        Flags.has Flags.mfSpecial m.flags
                            && not (Flags.has Flags.mfDropped m.flags)
                            && (m.type_ /= MobjType.Inv)
                            && (m.type_ /= MobjType.Ins)
                    then
                        { world | itemRespawn = enqueue m.spawnpoint world.leveltime world.itemRespawn }

                    else
                        world
            in
            queued
                |> MapUtil.unsetThingPosition env id
                |> World.stopSound id
                |> World.removeThinker id


{-| Add to the head of the item respawn queue, losing the oldest one off the tail when
it is full.
-}
enqueue : MapThing -> Int -> ItemRespawn -> ItemRespawn
enqueue mthing time queue =
    let
        head =
            Bitwise.and (queue.head + 1) (World.itemQueSize - 1)
    in
    { things = setPadded queue.head mthing noMapThing queue.things
    , times = setPadded queue.head time 0 queue.times
    , head = head
    , tail =
        if head == queue.tail then
            Bitwise.and (queue.tail + 1) (World.itemQueSize - 1)

        else
            queue.tail
    }


{-| `array[i] = value`, growing the array (with `default`) if it is shorter: the C
source's queue arrays always have ITEMQUESIZE entries.
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


{-| P\_RespawnSpecials: in altdeath (deathmatch 2), put back the item at the tail of the
respawn queue once it has been gone 30 seconds, with a fog where it appears.
-}
respawnSpecials : Env -> World -> World
respawnSpecials env world =
    let
        queue =
            world.itemRespawn
    in
    if env.params.deathmatch /= 2 then
        world

    else if queue.head == queue.tail then
        world

    else if world.leveltime - (Array.get queue.tail queue.times |> Maybe.withDefault 0) < 30 * 35 then
        world

    else
        let
            mthing =
                Array.get queue.tail queue.things |> Maybe.withDefault noMapThing

            x =
                Fixed.fromInt mthing.x

            y =
                Fixed.fromInt mthing.y

            ss =
                Geometry.pointInSubsector x y env.level

            floor =
                (World.sector (Level.subsector ss env.level).sector world).floorheight

            ( fog, world1 ) =
                spawnMobj env x y floor MobjType.Ifog world

            world2 =
                World.startSound (Sound.FromMobj fog) Sfx.Itmbk world1

            -- The C source looks the type up in mobjinfo[] and would read past its end
            -- for an unknown one; only things it spawned go on the queue, so it always
            -- finds one. If not, nothing more is spawned.
            world3 =
                case typeForDoomednum mthing.type_ of
                    Just type_ ->
                        let
                            z =
                                if Flags.has Flags.mfSpawnCeiling (Env.mobjInfo env type_).flags then
                                    Defs.onCeilingZ

                                else
                                    Defs.onFloorZ

                            ( mo, spawned ) =
                                spawnMobj env x y z type_ world2
                        in
                        World.updateMobj mo
                            (\m -> { m | spawnpoint = mthing, angle = spawnAngle mthing })
                            spawned

                    Nothing ->
                        world2

            queue3 =
                world3.itemRespawn
        in
        { world3 | itemRespawn = { queue3 | tail = Bitwise.and (queue3.tail + 1) (World.itemQueSize - 1) } }


{-| The first type in mobjinfo[] whose editor number is this (the C source's search
loop), if any.
-}
typeForDoomednum : Int -> Maybe MobjType
typeForDoomednum doomednum =
    Array.foldl
        (\t found ->
            case found of
                Just _ ->
                    found

                Nothing ->
                    if (MobjType.info t).doomednum == doomednum then
                        Just t

                    else
                        Nothing
        )
        Nothing
        MobjType.all


{-| `ANG45 * (mthing->angle / 45)`: a map thing's facing, to the nearest 45 degrees
below.
-}
spawnAngle : MapThing -> Angle
spawnAngle mthing =
    Angle.wrap (Angle.ang45 * (mthing.angle // 45))


{-| P\_SpawnPuff x y z: a bullet puff, a little above or below z; a punch's puff (when
`attackrange` is MELEERANGE) doesn't spark.
-}
spawnPuff : Env -> Fixed -> Fixed -> Fixed -> World -> World
spawnPuff env x y z world =
    let
        ( z1, world1 ) =
            jitterZ z world

        ( th, world2 ) =
            spawnMobj env x y z1 MobjType.Puff world1

        ( r, world3 ) =
            World.pRandom world2

        world4 =
            World.updateMobj th (\m -> { m | momz = Fixed.fracunit, tics = shortenTics r m.tics }) world3
    in
    if world4.scratch.attackrange == Defs.meleeRange then
        Tuple.second (Env.setMobjState env th State.Puff3 world4)

    else
        world4


{-| P\_SpawnBlood x y z damage: blood, smaller splats for smaller damage.
-}
spawnBlood : Env -> Fixed -> Fixed -> Fixed -> Int -> World -> World
spawnBlood env x y z damage world =
    let
        ( z1, world1 ) =
            jitterZ z world

        ( th, world2 ) =
            spawnMobj env x y z1 MobjType.Blood world1

        ( r, world3 ) =
            World.pRandom world2

        world4 =
            World.updateMobj th (\m -> { m | momz = Fixed.fracunit * 2, tics = shortenTics r m.tics }) world3
    in
    if damage <= 12 && damage >= 9 then
        Tuple.second (Env.setMobjState env th State.Blood2 world4)

    else if damage < 9 then
        Tuple.second (Env.setMobjState env th State.Blood3 world4)

    else
        world4


{-| `z += (P_Random() - P_Random()) << 10`, the left P\_Random first.
-}
jitterZ : Fixed -> World -> ( Fixed, World )
jitterZ z world =
    let
        ( r1, world1 ) =
            World.pRandom world

        ( r2, world2 ) =
            World.pRandom world1
    in
    ( Int32.add z (Int32.shiftLeft 10 (r1 - r2)), world2 )


{-| `tics -= random & 3`, at least 1.
-}
shortenTics : Int -> Int -> Int
shortenTics random tics =
    max 1 (tics - Bitwise.and random 3)


noMapThing : MapThing
noMapThing =
    { x = 0, y = 0, angle = 0, type_ = 0, options = 0 }
