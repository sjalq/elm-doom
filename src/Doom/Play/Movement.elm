module Doom.Play.Movement exposing (mobjThinker, explodeMissile, checkMissileSpawn, spawnMissile, spawnPlayerMissile)

{-| Things moving and thinking (the upper half of p\_mobj.c): momentum, gravity,
missiles, nightmare respawns, and P\_MobjThinker.

@docs mobjThinker, explodeMissile, checkMissileSpawn, spawnMissile, spawnPlayerMissile

-}

import Bitwise
import Doom.Angle as Angle
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Level as Level
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Map as Map
import Doom.Play.MapUtil as MapUtil
import Doom.Play.Mobj as PMobj
import Doom.Player exposing (Player)
import Doom.Sound as Sound
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)


{-| STOPSPEED: slower than this, a thing on the ground stops.
-}
stopSpeed : Fixed
stopSpeed =
    0x1000


{-| FRICTION: how much momentum a thing on the ground keeps each tic.
-}
friction : Fixed
friction =
    0xE800


{-| Whether the thing's thinker has been removed (C: `function.acv == -1`).
-}
isRemoved : ThinkerId -> World -> Bool
isRemoved id world =
    World.thinker id world /= Just MobjThinker


{-| Run `f` on the thing, if it is there (things stay in `World.mobjs` for the whole
level, so it always is).
-}
withMobj : ThinkerId -> World -> (Mobj -> World) -> World
withMobj id world f =
    case World.mobj id world of
        Just m ->
            f m

        Nothing ->
            world


{-| The player a thing is the body of (`mo->player`), as they are now.
-}
playerOf : Mobj -> World -> Maybe ( Int, Player )
playerOf m world =
    m.player
        |> Maybe.andThen (\n -> World.player n world |> Maybe.map (Tuple.pair n))


{-| `tics -= random & 3`, at least 1.
-}
shortenTics : Int -> Int -> Int
shortenTics random tics =
    max 1 (tics - Bitwise.and random 3)



-- P_MobjThinker


{-| P\_MobjThinker: move the thing by its momentum, then let gravity act, then count down
its state's tics, going to the next state when they run out. A monster lying dead in a
state that lasts forever may come back to life on Nightmare (`respawnmonsters`).
-}
mobjThinker : Env -> ThinkerId -> World -> World
mobjThinker env id world =
    withMobj id world <|
        \m ->
            if m.momx /= 0 || m.momy /= 0 || Flags.has Flags.mfSkullFly m.flags then
                let
                    moved =
                        xyMovement env id world
                in
                if isRemoved id moved then
                    moved

                else
                    thinkZ env id moved

            else
                thinkZ env id world


thinkZ : Env -> ThinkerId -> World -> World
thinkZ env id world =
    withMobj id world <|
        \m ->
            if m.z /= m.floorz || m.momz /= 0 then
                let
                    moved =
                        zMovement env id world
                in
                if isRemoved id moved then
                    moved

                else
                    cycleState env id moved

            else
                cycleState env id world


{-| Cycle through states, calling action functions at transitions; or, for a thing in a
state that never ends, check for a nightmare respawn.
-}
cycleState : Env -> ThinkerId -> World -> World
cycleState env id world =
    withMobj id world <|
        \m ->
            if m.tics /= -1 then
                let
                    tics =
                        m.tics - 1

                    counted =
                        World.setMobj { m | tics = tics } world
                in
                if tics == 0 then
                    Tuple.second (Env.setMobjState env id (Env.stateDef env m.state).next counted)

                else
                    counted

            else if not (Flags.has Flags.mfCountKill m.flags) then
                world

            else if not env.params.respawnmonsters then
                world

            else
                let
                    counted =
                        World.setMobj { m | movecount = m.movecount + 1 } world
                in
                if m.movecount + 1 < 12 * 35 then
                    counted

                else if Bitwise.and counted.leveltime 31 /= 0 then
                    counted

                else
                    let
                        ( r, rolled ) =
                            World.pRandom counted
                    in
                    if r > 4 then
                        rolled

                    else
                        nightmareRespawn env id rolled



-- P_XYMovement


{-| P\_XYMovement: move the thing by its momentum, at most MAXMOVE a tic, in two steps
when it is fast; then slow it down by friction if it is on the ground.
-}
xyMovement : Env -> ThinkerId -> World -> World
xyMovement env id world =
    withMobj id world <|
        \m ->
            if m.momx == 0 && m.momy == 0 then
                if Flags.has Flags.mfSkullFly m.flags then
                    -- the skull slammed into something
                    world
                        |> World.setMobj
                            { m | flags = Flags.clear Flags.mfSkullFly m.flags, momx = 0, momy = 0, momz = 0 }
                        |> Env.setMobjState env id (Env.mobjInfo env m.type_).spawnstate
                        |> Tuple.second

                else
                    world

            else
                let
                    clamped =
                        { m | momx = clampMove m.momx, momy = clampMove m.momy }

                    ( removed, moved ) =
                        moveSteps env id clamped.momx clamped.momy (World.setMobj clamped world)
                in
                if removed then
                    moved

                else
                    applyFriction env id moved


clampMove : Fixed -> Fixed
clampMove mom =
    if mom > Defs.maxMove then
        Defs.maxMove

    else if mom < -Defs.maxMove then
        -Defs.maxMove

    else
        mom


{-| The C source's do-while loop of tries: True if the thing was removed (a missile
that flew into the sky).

Only positive moves are halved (`xmove > MAXMOVE/2 || ymove > MAXMOVE/2`), as in the C
source.

-}
moveSteps : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
moveSteps env id xmove ymove world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just m ->
            let
                ( ( ptryx, ptryy ), ( xleft, yleft ) ) =
                    if xmove > Defs.maxMove // 2 || ymove > Defs.maxMove // 2 then
                        ( ( Int32.add m.x (xmove // 2), Int32.add m.y (ymove // 2) )
                        , ( Int32.shiftRight 1 xmove, Int32.shiftRight 1 ymove )
                        )

                    else
                        ( ( Int32.add m.x xmove, Int32.add m.y ymove ), ( 0, 0 ) )

                ( ok, tried ) =
                    Map.tryMove env id ptryx ptryy world

                ( removed, after ) =
                    if ok then
                        ( False, tried )

                    else
                        blockedMove env id tried
            in
            if removed then
                ( True, after )

            else if xleft /= 0 || yleft /= 0 then
                moveSteps env id xleft yleft after

            else
                ( False, after )


{-| A blocked move: a player slides along the wall, a missile explodes (or vanishes,
against the sky), anything else stops.
-}
blockedMove : Env -> ThinkerId -> World -> ( Bool, World )
blockedMove env id world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just m ->
            if m.player /= Nothing then
                ( False, Map.slideMove env id world )

            else if Flags.has Flags.mfMissile m.flags then
                if hitSky env world then
                    -- Hack to prevent missiles exploding against the sky.
                    -- Does not handle sky floors.
                    ( True, PMobj.removeMobj env id world )

                else
                    ( False, explodeMissile env id world )

            else
                ( False, World.setMobj { m | momx = 0, momy = 0 } world )


{-| Whether `ceilingline` (the line whose upper side the move hit) has sky above its
back sector.
-}
hitSky : Env -> World -> Bool
hitSky env world =
    case world.scratch.ceilingline of
        Just lineId ->
            case (Level.line lineId env.level).backsector of
                Just back ->
                    (World.sector back world).ceilingpic == env.data.skyFlatNum

                Nothing ->
                    False

        Nothing ->
            False


{-| The end of P\_XYMovement: friction for things on the ground.
-}
applyFriction : Env -> ThinkerId -> World -> World
applyFriction env id world =
    withMobj id world <|
        \m ->
            let
                player =
                    playerOf m world
            in
            if Maybe.map (\( _, p ) -> Flags.has Defs.cfNomomentum p.cheats) player == Just True then
                -- debug option for no sliding at all
                World.setMobj { m | momx = 0, momy = 0 } world

            else if Flags.has (Bitwise.or Flags.mfMissile Flags.mfSkullFly) m.flags then
                -- no friction for missiles ever
                world

            else if m.z > m.floorz then
                -- no friction when airborne
                world

            else if
                Flags.has Flags.mfCorpse m.flags
                    && (m.momx > Fixed.fracunit // 4 || m.momx < -Fixed.fracunit // 4 || m.momy > Fixed.fracunit // 4 || m.momy < -Fixed.fracunit // 4)
                    && (m.floorz /= (World.sector (World.mobjSector env.level m) world).floorheight)
            then
                -- do not stop sliding if halfway off a step with some momentum
                world

            else if
                (m.momx > -stopSpeed && m.momx < stopSpeed && m.momy > -stopSpeed && m.momy < stopSpeed)
                    && (case player of
                            Nothing ->
                                True

                            Just ( _, p ) ->
                                p.cmd.forwardmove == 0 && p.cmd.sidemove == 0
                       )
            then
                -- if in a walking frame, stop moving
                stopWalking env player world
                    |> World.updateMobj id (\n -> { n | momx = 0, momy = 0 })

            else
                World.setMobj { m | momx = Fixed.mul m.momx friction, momy = Fixed.mul m.momy friction } world


{-| `if (player && (player->mo->state - states) - S_PLAY_RUN1 < 4) P_SetMobjState
(player->mo, S_PLAY)`: the player's own body, which isn't always the thing that moved
(a "voodoo doll" from an extra player start points at the same player).
-}
stopWalking : Env -> Maybe ( Int, Player ) -> World -> World
stopWalking env player world =
    case player |> Maybe.andThen (\( _, p ) -> p.mo) of
        Just bodyId ->
            case World.mobj bodyId world of
                Just body ->
                    let
                        frame =
                            State.index body.state - State.index State.PlayRun1
                    in
                    if frame >= 0 && frame < 4 then
                        Tuple.second (Env.setMobjState env bodyId State.Play world)

                    else
                        world

                Nothing ->
                    world

        Nothing ->
            world



-- P_ZMovement


{-| P\_ZMovement: move the thing up or down by its momentum, gravity, floating towards
its target's height, and landing on floors and bumping into ceilings.
-}
zMovement : Env -> ThinkerId -> World -> World
zMovement env id world =
    withMobj id world <|
        \m ->
            let
                -- check for smooth step up
                stepped =
                    case m.player of
                        Just n ->
                            if m.z < m.floorz then
                                World.updatePlayer n
                                    (\p ->
                                        let
                                            viewheight =
                                                Int32.sub p.viewheight (Int32.sub m.floorz m.z)
                                        in
                                        { p
                                            | viewheight = viewheight
                                            , deltaviewheight = Int32.shiftRight 3 (Int32.sub Defs.viewHeight viewheight)
                                        }
                                    )
                                    world

                            else
                                world

                        Nothing ->
                            world

                z =
                    floatTowardsTarget m (Int32.add m.z m.momz) stepped
            in
            if z <= m.floorz then
                hitFloor env id { m | z = z } stepped

            else if not (Flags.has Flags.mfNoGravity m.flags) then
                let
                    momz =
                        if m.momz == 0 then
                            -Defs.gravity * 2

                        else
                            Int32.sub m.momz Defs.gravity
                in
                hitCeiling env id (World.setMobj { m | z = z, momz = momz } stepped)

            else
                hitCeiling env id (World.setMobj { m | z = z } stepped)


{-| A floating monster drifts down or up towards its target if it is too close
(steeper than 1 in 3).
-}
floatTowardsTarget : Mobj -> Fixed -> World -> Fixed
floatTowardsTarget m z world =
    if
        Flags.has Flags.mfFloat m.flags
            && not (Flags.has Flags.mfSkullFly m.flags)
            && not (Flags.has Flags.mfInFloat m.flags)
    then
        case m.target |> Maybe.andThen (\t -> World.mobj t world) of
            Just target ->
                let
                    dist =
                        MapUtil.aproxDistance (Int32.sub m.x target.x) (Int32.sub m.y target.y)

                    delta =
                        Int32.sub (Int32.add target.z (Int32.shiftRight 1 m.height)) z
                in
                if delta < 0 && dist < Int32.neg (Int32.mul delta 3) then
                    Int32.sub z Defs.floatSpeed

                else if delta > 0 && dist < Int32.mul delta 3 then
                    Int32.add z Defs.floatSpeed

                else
                    z

            Nothing ->
                z

    else
        z


{-| The thing (already moved to `m.z`) is at or below the floor.
-}
hitFloor : Env -> ThinkerId -> Mobj -> World -> World
hitFloor env id m world =
    let
        -- the skull slammed into something
        momz =
            if Flags.has Flags.mfSkullFly m.flags then
                Int32.neg m.momz

            else
                m.momz

        ( landedMomz, landed ) =
            if momz < 0 then
                case m.player of
                    Just n ->
                        if momz < -Defs.gravity * 8 then
                            -- Squat down: decrease viewheight for a moment after
                            -- hitting the ground hard, and say "oof".
                            ( 0
                            , world
                                |> World.updatePlayer n (\p -> { p | deltaviewheight = Int32.shiftRight 3 momz })
                                |> World.startSound (Sound.FromMobj id) Sfx.Oof
                            )

                        else
                            ( 0, world )

                    Nothing ->
                        ( 0, world )

            else
                ( momz, world )

        onFloor =
            World.setMobj { m | momz = landedMomz, z = m.floorz } landed
    in
    if Flags.has Flags.mfMissile m.flags && not (Flags.has Flags.mfNoClip m.flags) then
        explodeMissile env id onFloor

    else
        hitCeiling env id onFloor


hitCeiling : Env -> ThinkerId -> World -> World
hitCeiling env id world =
    withMobj id world <|
        \m ->
            if Int32.add m.z m.height > m.ceilingz then
                let
                    stopped =
                        if m.momz > 0 then
                            0

                        else
                            m.momz

                    -- the skull slammed into something
                    momz =
                        if Flags.has Flags.mfSkullFly m.flags then
                            Int32.neg stopped

                        else
                            stopped

                    bumped =
                        World.setMobj { m | momz = momz, z = Int32.sub m.ceilingz m.height } world
                in
                if Flags.has Flags.mfMissile m.flags && not (Flags.has Flags.mfNoClip m.flags) then
                    explodeMissile env id bumped

                else
                    bumped

            else
                world



-- P_NightmareRespawn


{-| P\_NightmareRespawn: if nothing is standing on the monster's spawn point, a new one
appears there (with a teleport fog where the body was and one where it appears) and
the body goes.
-}
nightmareRespawn : Env -> ThinkerId -> World -> World
nightmareRespawn env id world =
    withMobj id world <|
        \m ->
            let
                x =
                    Fixed.fromInt m.spawnpoint.x

                y =
                    Fixed.fromInt m.spawnpoint.y

                ( free, checked ) =
                    Map.checkPosition env id x y world
            in
            if not free then
                -- something is occupying its position
                checked

            else
                let
                    body =
                        World.mobj id checked |> Maybe.withDefault m

                    -- a teleport fog at the old spot, because the body is removed
                    ( oldFog, world1 ) =
                        PMobj.spawnMobj env
                            body.x
                            body.y
                            (World.sector (World.mobjSector env.level body) checked).floorheight
                            MobjType.Tfog
                            checked

                    world2 =
                        World.startSound (Sound.FromMobj oldFog) Sfx.Telept world1

                    -- a teleport fog at the new spot
                    ss =
                        Geometry.pointInSubsector x y env.level

                    ( newFog, world3 ) =
                        PMobj.spawnMobj env x y (World.sector (Level.subsector ss env.level).sector world2).floorheight MobjType.Tfog world2

                    world4 =
                        World.startSound (Sound.FromMobj newFog) Sfx.Telept world3

                    z =
                        if Flags.has Flags.mfSpawnCeiling (Env.mobjInfo env body.type_).flags then
                            Defs.onCeilingZ

                        else
                            Defs.onFloorZ

                    -- the new monster inherits the deceased one's spawn point
                    ( mo, world5 ) =
                        PMobj.spawnMobj env x y z body.type_ world4

                    world6 =
                        World.updateMobj mo
                            (\n ->
                                { n
                                    | spawnpoint = body.spawnpoint
                                    , angle = PMobj.spawnAngle body.spawnpoint
                                    , flags =
                                        if Flags.has Level.mtfAmbush body.spawnpoint.options then
                                            Flags.set Flags.mfAmbush n.flags

                                        else
                                            n.flags
                                    , reactiontime = 18
                                }
                            )
                            world5
                in
                PMobj.removeMobj env id world6



-- MISSILES


{-| P\_ExplodeMissile: stop, go to the death state (lasting a little less, at random),
stop being a missile, and make the death sound.
-}
explodeMissile : Env -> ThinkerId -> World -> World
explodeMissile env id world =
    withMobj id world <|
        \m ->
            let
                info =
                    Env.mobjInfo env m.type_

                ( _, world1 ) =
                    Env.setMobjState env id info.deathstate (World.setMobj { m | momx = 0, momy = 0, momz = 0 } world)

                ( r, world2 ) =
                    World.pRandom world1

                world3 =
                    World.updateMobj id
                        (\n -> { n | tics = shortenTics r n.tics, flags = Flags.clear Flags.mfMissile n.flags })
                        world2
            in
            if info.deathsound /= Sfx.None then
                World.startSound (Sound.FromMobj id) info.deathsound world3

            else
                world3


{-| P\_CheckMissileSpawn: move a new missile forward a little (so an angle can be worked
out if it explodes at once) and explode it if it is already in a wall.
-}
checkMissileSpawn : Env -> ThinkerId -> World -> World
checkMissileSpawn env id world =
    let
        ( r, world1 ) =
            World.pRandom world
    in
    withMobj id world1 <|
        \m ->
            let
                moved =
                    { m
                        | tics = shortenTics r m.tics
                        , x = Int32.add m.x (Int32.shiftRight 1 m.momx)
                        , y = Int32.add m.y (Int32.shiftRight 1 m.momy)
                        , z = Int32.add m.z (Int32.shiftRight 1 m.momz)
                    }

                ( ok, tried ) =
                    Map.tryMove env id moved.x moved.y (World.setMobj moved world1)
            in
            if ok then
                tried

            else
                explodeMissile env id tried


{-| P\_SpawnMissile source dest type: a monster's missile, from 32 units above its feet,
heading for `dest` (off by a random angle if `dest` is fuzzy), and climbing or
falling to arrive at `dest`'s height.
-}
spawnMissile : Env -> ThinkerId -> ThinkerId -> MobjType -> World -> ( ThinkerId, World )
spawnMissile env sourceId destId type_ world =
    case ( World.mobj sourceId world, World.mobj destId world ) of
        ( Just source, Just dest ) ->
            let
                info =
                    Env.mobjInfo env type_

                ( th, world1 ) =
                    PMobj.spawnMobj env source.x source.y (Int32.add source.z (4 * 8 * Fixed.fracunit)) type_ world

                world2 =
                    seeSound info.seesound th world1

                aim =
                    Geometry.pointToAngle2 source.x source.y dest.x dest.y

                -- fuzzy player
                ( an, world3 ) =
                    if Flags.has Flags.mfShadow dest.flags then
                        let
                            ( r1, w1 ) =
                                World.pRandom world2

                            ( r2, w2 ) =
                                World.pRandom w1
                        in
                        ( Angle.add aim (Int32.shiftLeft 20 (r1 - r2)), w2 )

                    else
                        ( aim, world2 )

                dist =
                    max 1 (MapUtil.aproxDistance (Int32.sub dest.x source.x) (Int32.sub dest.y source.y) // info.speed)

                world4 =
                    World.updateMobj th
                        (\n ->
                            { n
                                | target = Just sourceId
                                , angle = an
                                , momx = Fixed.mul info.speed (Angle.cosine an)
                                , momy = Fixed.mul info.speed (Angle.sine an)
                                , momz = Int32.sub dest.z source.z // dist
                            }
                        )
                        world3
            in
            ( th, checkMissileSpawn env th world4 )

        _ ->
            -- Never happens: things stay in World.mobjs for the whole level.
            ( -1, world )


{-| P\_SpawnPlayerMissile source type: a player's missile. It aims at a monster
straight ahead, or a little to the left, or a little to the right; failing those it
goes level.
-}
spawnPlayerMissile : Env -> ThinkerId -> MobjType -> World -> World
spawnPlayerMissile env sourceId type_ world =
    withMobj sourceId world <|
        \source ->
            let
                range =
                    16 * 64 * Fixed.fracunit

                aimAt angle w =
                    let
                        ( aimSlope, aimed ) =
                            Map.aimLineAttack env sourceId angle range w
                    in
                    ( ( angle, aimSlope ), aimed )

                hit w =
                    w.scratch.linetarget /= Nothing

                ( ( an0, slope0 ), world0 ) =
                    aimAt source.angle world

                ( ( an, slope ), world1 ) =
                    if hit world0 then
                        ( ( an0, slope0 ), world0 )

                    else
                        let
                            ( leftAim, w1 ) =
                                aimAt (Angle.add an0 (2 ^ 26)) world0

                            ( secondAim, w2 ) =
                                if hit w1 then
                                    ( leftAim, w1 )

                                else
                                    aimAt (Angle.sub (Tuple.first leftAim) (2 * 2 ^ 26)) w1
                        in
                        if hit w2 then
                            ( secondAim, w2 )

                        else
                            ( ( source.angle, 0 ), w2 )

                info =
                    Env.mobjInfo env type_

                ( th, world2 ) =
                    PMobj.spawnMobj env source.x source.y (Int32.add source.z (4 * 8 * Fixed.fracunit)) type_ world1

                world3 =
                    seeSound info.seesound th world2
                        |> World.updateMobj th
                            (\n ->
                                { n
                                    | target = Just sourceId
                                    , angle = an
                                    , momx = Fixed.mul info.speed (Angle.cosine an)
                                    , momy = Fixed.mul info.speed (Angle.sine an)
                                    , momz = Fixed.mul info.speed slope
                                }
                            )
            in
            checkMissileSpawn env th world3


seeSound : Sfx -> ThinkerId -> World -> World
seeSound sfx id world =
    if sfx /= Sfx.None then
        World.startSound (Sound.FromMobj id) sfx world

    else
        world
