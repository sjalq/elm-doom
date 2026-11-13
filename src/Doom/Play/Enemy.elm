module Doom.Play.Enemy exposing
    ( noiseAlert, keenDie, look, chase, faceTarget, posAttack, sPosAttack, cPosAttack, cPosRefire, spidRefire, bspiAttack, troopAttack, sargAttack, headAttack, cyberAttack, bruisAttack, skelMissile, tracer, skelWhoosh, skelFist, vileChase, vileStart, startFire, fireCrackle, fire, vileTarget, vileAttack, fatRaise, fatAttack1, fatAttack2, fatAttack3, skullAttack, painAttack, painDie, scream, xScream, pain, fall, explode, bossDeath, hoof, metal, babyMetal, brainAwake, brainPain, brainScream, brainExplode, brainDie, brainSpit, spawnSound, spawnFly, playerScream
    , diNoDir, opposite, diags, xspeed, yspeed
    )

{-| Monsters (p\_enemy.c): hearing, seeing, chasing and attacking, and their action
functions. The three super shotgun actions p\_enemy.c also holds are in
`Doom.Play.Weapon`.

Every action takes the acting thing's id. Where the C source would dereference a
missing thing (a NULL target, say) and crash, these leave the world as it is.

@docs noiseAlert, keenDie, look, chase, faceTarget, posAttack, sPosAttack, cPosAttack, cPosRefire, spidRefire, bspiAttack, troopAttack, sargAttack, headAttack, cyberAttack, bruisAttack, skelMissile, tracer, skelWhoosh, skelFist, vileChase, vileStart, startFire, fireCrackle, fire, vileTarget, vileAttack, fatRaise, fatAttack1, fatAttack2, fatAttack3, skullAttack, painAttack, painDie, scream, xScream, pain, fall, explode, bossDeath, hoof, metal, babyMetal, brainAwake, brainPain, brainScream, brainExplode, brainDie, brainSpit, spawnSound, spawnFly, playerScream


# Directions

A monster's `movedir` is a `dirtype_t`: 0 is east, going anticlockwise in steps of 45
degrees to 7 (south-east); 8 is "no direction".

@docs diNoDir, opposite, diags, xspeed, yspeed

-}

import Array
import Bitwise
import Dict
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (GameMode(..), Skill(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Info.State as State exposing (State)
import Doom.Int32 as Int32
import Doom.Level as Level exposing (SectorId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Door as Door
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Floor as Floor
import Doom.Play.Inter as Inter
import Doom.Play.Map as Map
import Doom.Play.MapUtil as MapUtil
import Doom.Play.Mobj as PlayMobj
import Doom.Play.Movement as Movement
import Doom.Play.Sight as Sight
import Doom.Sound as Sound
import Doom.Thinker as Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)



-- DIRECTIONS


diEast : Int
diEast =
    0


diNorth : Int
diNorth =
    2


diWest : Int
diWest =
    4


diSouth : Int
diSouth =
    6


diSouthEast : Int
diSouthEast =
    7


{-| DI\_NODIR: not moving.
-}
diNoDir : Int
diNoDir =
    8


{-| `opposite[]`: the direction facing the other way (no direction for no direction).
-}
opposite : Int -> Int
opposite dir =
    if dir >= 0 && dir < 8 then
        modBy 8 (dir + 4)

    else
        diNoDir


{-| `diags[]`, indexed by `((deltay < 0) << 1) + (deltax > 0)`: north-west, north-east,
south-west, south-east.
-}
diags : Int -> Int
diags i =
    case i of
        0 ->
            3

        1 ->
            1

        2 ->
            5

        _ ->
            7


{-| `xspeed[]`: the x part of a step in each direction, fixed-point (47000 is about
FRACUNIT / sqrt 2).
-}
xspeed : Int -> Fixed
xspeed dir =
    case dir of
        0 ->
            Fixed.fracunit

        1 ->
            47000

        2 ->
            0

        3 ->
            -47000

        4 ->
            -Fixed.fracunit

        5 ->
            -47000

        6 ->
            0

        _ ->
            47000


{-| `yspeed[]`: the y part of a step in each direction.
-}
yspeed : Int -> Fixed
yspeed dir =
    xspeed (modBy 8 (dir + 6))



-- HELPERS


{-| Run `f` on the thing with this id, or leave the world alone if there is none.
-}
withMobj : ThinkerId -> World -> (Mobj -> World) -> World
withMobj id world f =
    case World.mobj id world of
        Just m ->
            f m

        Nothing ->
            world


{-| Run `f` on the thing and its target, or leave the world alone if it has none
(the C actions' `if (!actor->target) return;`).
-}
withTarget : ThinkerId -> World -> (Mobj -> Mobj -> World) -> World
withTarget id world f =
    withMobj id world <|
        \actor ->
            case actor.target |> Maybe.andThen (\t -> World.mobj t world) of
                Just target ->
                    f actor target

                Nothing ->
                    world


{-| `actor->info`.
-}
infoOf : Env -> Mobj -> MobjType.Info
infoOf env m =
    Env.mobjInfo env m.type_


{-| S\_StartSound from a thing.
-}
soundFrom : ThinkerId -> Sfx -> World -> World
soundFrom id =
    World.startSound (Sound.FromMobj id)


{-| S\_StartSound (NULL, ...): heard everywhere at full volume.
-}
soundEverywhere : Sfx -> World -> World
soundEverywhere =
    World.startSound Sound.Ambient


{-| P\_SetMobjState, not caring whether the thing survived.
-}
setState : Env -> ThinkerId -> State -> World -> World
setState env id st world =
    Tuple.second (Env.setMobjState env id st world)


{-| `(P_Random() - P_Random()) << shift`, the left call first.
-}
randomSpread : Int -> World -> ( Int, World )
randomSpread shift world =
    let
        ( r1, w1 ) =
            World.pRandom world

        ( r2, w2 ) =
            World.pRandom w1
    in
    ( Int32.shiftLeft shift (r1 - r2), w2 )


{-| The things whose thinker is P\_MobjThinker, in thinker-list order.
-}
liveMobjs : World -> List Mobj
liveMobjs world =
    Dict.foldr
        (\id t acc ->
            case t of
                MobjThinker ->
                    case World.mobj id world of
                        Just m ->
                            m :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        []
        world.thinkers


{-| Whether another thing of this one's type is still alive (A\_KeenDie, A\_BossDeath).
-}
otherOfTypeAlive : Mobj -> World -> Bool
otherOfTypeAlive mo world =
    List.any (\m2 -> m2.id /= mo.id && m2.type_ == mo.type_ && m2.health > 0) (liveMobjs world)


{-| G\_ExitLevel.
-}
exitLevel : World -> World
exitLevel world =
    { world | exit = Just World.NormalExit }



-- HEARING


{-| P\_RecursiveSound: flood a noise through the sector and the sectors joined to it
by open two-sided lines; the second sound-blocking line crossed stops it.
-}
recursiveSound : Env -> SectorId -> Int -> World -> World
recursiveSound env sec soundblocks world =
    let
        s =
            World.sector sec world
    in
    if s.validcount == world.validcount && s.soundtraversed <= soundblocks + 1 then
        -- already flooded
        world

    else
        let
            marked =
                World.updateSector sec
                    (\st ->
                        { st
                            | validcount = world.validcount
                            , soundtraversed = soundblocks + 1
                            , soundtarget = world.scratch.soundtarget
                        }
                    )
                    world
        in
        Array.foldl (soundThroughLine env sec soundblocks) marked (Level.sector sec env.level).lines


soundThroughLine : Env -> SectorId -> Int -> Level.LineId -> World -> World
soundThroughLine env sec soundblocks lineId world =
    let
        check =
            Level.line lineId env.level
    in
    if not (Flags.has Flags.mlTwoSided check.flags) then
        world

    else
        let
            opened =
                MapUtil.lineOpening env lineId world

            other =
                if check.frontsector == sec then
                    check.backsector

                else
                    Just check.frontsector
        in
        case other of
            Just otherSec ->
                if opened.scratch.openrange <= 0 then
                    -- closed door
                    opened

                else if Flags.has Flags.mlSoundBlock check.flags then
                    if soundblocks == 0 then
                        recursiveSound env otherSec 1 opened

                    else
                        opened

                else
                    recursiveSound env otherSec soundblocks opened

            Nothing ->
                -- a two-sided line with no back: P_LineOpening gives it no opening
                opened


{-| P\_NoiseAlert target emitter: monsters that can hear the emitter (a player firing)
wake up and go for the target.
-}
noiseAlert : Env -> ThinkerId -> ThinkerId -> World -> World
noiseAlert env target emitter world =
    let
        w1 =
            World.setScratch (\s -> { s | soundtarget = Just target }) world
    in
    withMobj emitter w1 <|
        \e ->
            let
                ( _, w2 ) =
                    World.nextValidcount w1
            in
            recursiveSound env (World.mobjSector env.level e) 0 w2



-- RANGE CHECKS


{-| P\_CheckMeleeRange: whether the target is close enough to hit, and in sight.
-}
checkMeleeRange : Env -> ThinkerId -> World -> ( Bool, World )
checkMeleeRange env id world =
    case World.mobj id world of
        Just actor ->
            case actor.target |> Maybe.andThen (\t -> World.mobj t world) of
                Just pl ->
                    let
                        dist =
                            MapUtil.aproxDistance (Int32.sub pl.x actor.x) (Int32.sub pl.y actor.y)
                    in
                    if dist >= Defs.meleeRange - 20 * Fixed.fracunit + (infoOf env pl).radius then
                        ( False, world )

                    else
                        Sight.checkSight env id pl.id world

                Nothing ->
                    ( False, world )

        Nothing ->
            ( False, world )


{-| P\_CheckMissileRange: whether to fire now. The further away the target, the less
likely; a monster just hurt always fires back.
-}
checkMissileRange : Env -> ThinkerId -> World -> ( Bool, World )
checkMissileRange env id world =
    case World.mobj id world |> Maybe.andThen .target of
        Nothing ->
            ( False, world )

        Just targetId ->
            let
                ( seen, w1 ) =
                    Sight.checkSight env id targetId world
            in
            case ( World.mobj id w1, World.mobj targetId w1 ) of
                ( Just actor, Just target ) ->
                    if not seen then
                        ( False, w1 )

                    else if Flags.has Flags.mfJustHit actor.flags then
                        -- the target just hit the enemy, so fight back!
                        ( True, World.setMobj { actor | flags = Flags.clear Flags.mfJustHit actor.flags } w1 )

                    else if actor.reactiontime /= 0 then
                        -- do not attack yet
                        ( False, w1 )

                    else
                        missileOdds env actor target w1

                _ ->
                    ( False, w1 )


missileOdds : Env -> Mobj -> Mobj -> World -> ( Bool, World )
missileOdds env actor target world =
    let
        near =
            Int32.sub (MapUtil.aproxDistance (Int32.sub actor.x target.x) (Int32.sub actor.y target.y)) (64 * Fixed.fracunit)

        dist0 =
            if (infoOf env actor).meleestate == State.Null then
                -- no melee attack, so fire more
                Int32.shiftRight 16 (Int32.sub near (128 * Fixed.fracunit))

            else
                Int32.shiftRight 16 near
    in
    if actor.type_ == MobjType.Vile && dist0 > 14 * 64 then
        -- too far away
        ( False, world )

    else if actor.type_ == MobjType.Undead && dist0 < 196 then
        -- close for fist attack
        ( False, world )

    else
        let
            dist1 =
                if actor.type_ == MobjType.Undead then
                    Int32.shiftRight 1 dist0

                else
                    dist0

            dist2 =
                if actor.type_ == MobjType.Cyborg || actor.type_ == MobjType.Spider || actor.type_ == MobjType.Skull then
                    Int32.shiftRight 1 dist1

                else
                    dist1

            dist3 =
                if actor.type_ == MobjType.Cyborg then
                    min 160 dist2

                else
                    min 200 dist2

            ( r, w1 ) =
                World.pRandom world
        in
        ( not (r < dist3), w1 )



-- WALKING


{-| P\_Move: a step in the monster's direction. A floater blocked by height rises or
sinks instead; a monster blocked by special lines tries to use them (opening doors).
False if it couldn't move.
-}
move : Env -> ThinkerId -> World -> ( Bool, World )
move env id world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just actor ->
            if actor.movedir == diNoDir then
                ( False, world )

            else if actor.movedir < 0 || actor.movedir >= 8 then
                -- I_Error ("Weird actor->movedir!"): the world is left as it is
                ( False, world )

            else
                let
                    speed =
                        (infoOf env actor).speed

                    tryx =
                        Int32.add actor.x (Int32.mul speed (xspeed actor.movedir))

                    tryy =
                        Int32.add actor.y (Int32.mul speed (yspeed actor.movedir))

                    ( ok, w1 ) =
                        Map.tryMove env id tryx tryy world
                in
                case World.mobj id w1 of
                    Nothing ->
                        ( ok, w1 )

                    Just moved ->
                        if ok then
                            ( True
                            , World.setMobj
                                { moved
                                    | flags = Flags.clear Flags.mfInFloat moved.flags
                                    , z =
                                        if Flags.has Flags.mfFloat moved.flags then
                                            moved.z

                                        else
                                            moved.floorz
                                }
                                w1
                            )

                        else
                            blockedMove env moved w1


blockedMove : Env -> Mobj -> World -> ( Bool, World )
blockedMove env actor world =
    if Flags.has Flags.mfFloat actor.flags && world.scratch.floatok then
        -- must adjust height
        ( True
        , World.setMobj
            { actor
                | z =
                    if actor.z < world.scratch.tmfloorz then
                        Int32.add actor.z Defs.floatSpeed

                    else
                        Int32.sub actor.z Defs.floatSpeed
                , flags = Flags.set Flags.mfInFloat actor.flags
            }
            world
        )

    else if List.isEmpty world.scratch.spechit then
        ( False, world )

    else
        let
            -- C walks spechit from the last added down to the first, leaving
            -- numspechit used up
            lines =
                List.reverse world.scratch.spechit

            w1 =
                world
                    |> World.setMobj { actor | movedir = diNoDir }
                    |> World.setScratch (\s -> { s | spechit = [] })
        in
        List.foldl
            (\ld ( good, w ) ->
                let
                    ( used, w2 ) =
                        Env.useSpecialLine env actor.id ld 0 w
                in
                ( good || used, w2 )
            )
            ( False, w1 )
            lines


{-| P\_TryWalk: P\_Move, then wait a random number of steps before turning.
-}
tryWalk : Env -> ThinkerId -> World -> ( Bool, World )
tryWalk env id world =
    let
        ( moved, w1 ) =
            move env id world
    in
    if not moved then
        ( False, w1 )

    else
        let
            ( r, w2 ) =
                World.pRandom w1
        in
        ( True, World.updateMobj id (\m -> { m | movecount = Bitwise.and r 15 }) w2 )


setMovedir : ThinkerId -> Int -> World -> World
setMovedir id dir =
    World.updateMobj id (\m -> { m | movedir = dir })


{-| Try walking each direction in turn until one works.
-}
tryDirs : Env -> ThinkerId -> List Int -> World -> ( Bool, World )
tryDirs env id dirs world =
    case dirs of
        [] ->
            ( False, world )

        dir :: rest ->
            let
                ( ok, w1 ) =
                    tryWalk env id (setMovedir id dir world)
            in
            if ok then
                ( True, w1 )

            else
                tryDirs env id rest w1


{-| P\_NewChaseDir: pick a direction towards the target: straight at it if it can,
then along either axis, then on as before, then any way but back, then back.
-}
newChaseDir : Env -> ThinkerId -> World -> World
newChaseDir env id world =
    case World.mobj id world of
        Nothing ->
            world

        Just actor ->
            case actor.target |> Maybe.andThen (\t -> World.mobj t world) of
                Nothing ->
                    -- I_Error ("P_NewChaseDir: called with no target")
                    world

                Just target ->
                    let
                        olddir =
                            actor.movedir

                        turnaround =
                            opposite olddir

                        deltax =
                            Int32.sub target.x actor.x

                        deltay =
                            Int32.sub target.y actor.y

                        d1 =
                            if deltax > 10 * Fixed.fracunit then
                                diEast

                            else if deltax < -10 * Fixed.fracunit then
                                diWest

                            else
                                diNoDir

                        d2 =
                            if deltay < -10 * Fixed.fracunit then
                                diSouth

                            else if deltay > 10 * Fixed.fracunit then
                                diNorth

                            else
                                diNoDir

                        -- try direct route
                        ( direct, w1 ) =
                            if d1 /= diNoDir && d2 /= diNoDir then
                                let
                                    dir =
                                        diags
                                            ((if deltay < 0 then
                                                2

                                              else
                                                0
                                             )
                                                + (if deltax > 0 then
                                                    1

                                                   else
                                                    0
                                                  )
                                            )

                                    pointed =
                                        setMovedir id dir world
                                in
                                if dir /= turnaround then
                                    tryWalk env id pointed

                                else
                                    ( False, pointed )

                            else
                                ( False, world )
                    in
                    if direct then
                        w1

                    else
                        otherChaseDirs env id olddir turnaround ( d1, d2 ) ( deltax, deltay ) w1


otherChaseDirs : Env -> ThinkerId -> Int -> Int -> ( Int, Int ) -> ( Fixed, Fixed ) -> World -> World
otherChaseDirs env id olddir turnaround ( d1, d2 ) ( deltax, deltay ) world =
    let
        ( r, w1 ) =
            World.pRandom world

        ( e1, e2 ) =
            if r > 200 || Int32.abs deltay > Int32.abs deltax then
                ( d2, d1 )

            else
                ( d1, d2 )

        notBack d =
            if d == turnaround then
                diNoDir

            else
                d

        candidates =
            List.filter (\d -> d /= diNoDir) [ notBack e1, notBack e2, olddir ]

        ( found, w2 ) =
            tryDirs env id candidates w1
    in
    if found then
        w2

    else
        let
            -- randomly determine direction of search
            ( r2, w3 ) =
                World.pRandom w2

            order =
                if Bitwise.and r2 1 /= 0 then
                    List.range diEast diSouthEast

                else
                    List.reverse (List.range diEast diSouthEast)

            ( searched, w4 ) =
                tryDirs env id (List.filter (\d -> d /= turnaround) order) w3
        in
        if searched then
            w4

        else
            let
                ( back, w5 ) =
                    if turnaround /= diNoDir then
                        tryDirs env id [ turnaround ] w4

                    else
                        ( False, w4 )
            in
            if back then
                w5

            else
                -- can not move
                setMovedir id diNoDir w5



-- LOOKING


{-| P\_LookForPlayers: look at up to two of the players in the game, starting from the
one after the last looked at; True (and the player as target) if one is alive and in
sight, and, unless `allaround`, in front or very close.
-}
lookForPlayers : Env -> ThinkerId -> Bool -> World -> ( Bool, World )
lookForPlayers env id allaround world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just actor ->
            if not (List.any identity (Array.toList world.playeringame)) then
                -- C loops forever here; nobody is in the game, so nobody is seen
                ( False, world )

            else
                lookLoop env id allaround 0 (Bitwise.and (actor.lastlook - 1) 3) world


lookLoop : Env -> ThinkerId -> Bool -> Int -> Int -> World -> ( Bool, World )
lookLoop env id allaround c stop world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just actor ->
            let
                n =
                    actor.lastlook

                next w cNext =
                    lookLoop env id allaround cNext stop (World.updateMobj id (\m -> { m | lastlook = Bitwise.and (m.lastlook + 1) 3 }) w)
            in
            if not (Array.get n world.playeringame |> Maybe.withDefault False) then
                next world c

            else if c == 2 || n == stop then
                -- done looking
                ( False, world )

            else
                case World.player n world of
                    Nothing ->
                        next world (c + 1)

                    Just player ->
                        case player.mo of
                            Nothing ->
                                next world (c + 1)

                            Just mo ->
                                if player.health <= 0 then
                                    -- dead
                                    next world (c + 1)

                                else
                                    let
                                        ( seen, w1 ) =
                                            Sight.checkSight env id mo world
                                    in
                                    if not seen || not (inView allaround actor mo w1) then
                                        next w1 (c + 1)

                                    else
                                        ( True, World.updateMobj id (\m -> { m | target = Just mo }) w1 )


{-| Whether a player's body is in front of the actor, or close enough to notice anyway.
-}
inView : Bool -> Mobj -> ThinkerId -> World -> Bool
inView allaround actor mo world =
    if allaround then
        True

    else
        case World.mobj mo world of
            Nothing ->
                False

            Just pmo ->
                let
                    an =
                        Angle.sub (Geometry.pointToAngle2 actor.x actor.y pmo.x pmo.y) actor.angle
                in
                if an > Angle.ang90 && an < Angle.ang270 then
                    -- behind its back: if real close, react anyway
                    MapUtil.aproxDistance (Int32.sub pmo.x actor.x) (Int32.sub pmo.y actor.y) <= Defs.meleeRange

                else
                    True


{-| A\_KeenDie: when the last Commander Keen dies, open the doors tagged 666.
-}
keenDie : Env -> ThinkerId -> World -> World
keenDie env id world =
    let
        w1 =
            fall env id world
    in
    withMobj id w1 <|
        \mo ->
            if otherOfTypeAlive mo w1 then
                w1

            else
                Tuple.second (Door.doDoorByTag env 666 Thinker.OpenDoor w1)


{-| A\_Look: stay in this state until a player is seen or heard, then chase.
-}
look : Env -> ThinkerId -> World -> World
look env id world =
    withMobj id world <|
        \actor0 ->
            let
                actor =
                    { actor0 | threshold = 0 }

                w0 =
                    World.setMobj actor world

                heard =
                    (World.sector (World.mobjSector env.level actor) w0).soundtarget
                        |> Maybe.andThen (\t -> World.mobj t w0)
                        |> Maybe.andThen
                            (\t ->
                                if Flags.has Flags.mfShootable t.flags then
                                    Just t

                                else
                                    Nothing
                            )

                lookAround w =
                    let
                        ( found, w2 ) =
                            lookForPlayers env id False w
                    in
                    if found then
                        seeYou env id w2

                    else
                        w2
            in
            case heard of
                Just targ ->
                    let
                        w1 =
                            World.setMobj { actor | target = Just targ.id } w0
                    in
                    if Flags.has Flags.mfAmbush actor.flags then
                        let
                            ( seen, w2 ) =
                                Sight.checkSight env id targ.id w1
                        in
                        if seen then
                            seeYou env id w2

                        else
                            lookAround w2

                    else
                        seeYou env id w1

                Nothing ->
                    lookAround w0


{-| A\_Look's `seeyou`: the sight sound, then the chase.
-}
seeYou : Env -> ThinkerId -> World -> World
seeYou env id world =
    withMobj id world <|
        \actor ->
            let
                info =
                    infoOf env actor

                ( sound, w1 ) =
                    pickSound info.seesound world

                w2 =
                    if info.seesound == Sfx.None then
                        w1

                    else if actor.type_ == MobjType.Spider || actor.type_ == MobjType.Cyborg then
                        -- full volume
                        soundEverywhere sound w1

                    else
                        soundFrom id sound w1
            in
            setState env id info.seestate w2


{-| The sight sound to play: one of the possessed or imp sounds at random.
-}
pickSound : Sfx -> World -> ( Sfx, World )
pickSound sfx world =
    case sfx of
        Sfx.Posit1 ->
            randomOf [ Sfx.Posit1, Sfx.Posit2, Sfx.Posit3 ] world

        Sfx.Posit2 ->
            randomOf [ Sfx.Posit1, Sfx.Posit2, Sfx.Posit3 ] world

        Sfx.Posit3 ->
            randomOf [ Sfx.Posit1, Sfx.Posit2, Sfx.Posit3 ] world

        Sfx.Bgsit1 ->
            randomOf [ Sfx.Bgsit1, Sfx.Bgsit2 ] world

        Sfx.Bgsit2 ->
            randomOf [ Sfx.Bgsit1, Sfx.Bgsit2 ] world

        _ ->
            ( sfx, world )


{-| `first + P_Random() % count`.
-}
randomOf : List Sfx -> World -> ( Sfx, World )
randomOf sounds world =
    let
        ( r, w1 ) =
            World.pRandom world
    in
    ( List.drop (modBy (List.length sounds) r) sounds |> List.head |> Maybe.withDefault Sfx.None, w1 )



-- CHASING


{-| A\_Chase: turn towards the movement direction, attack if it can, otherwise keep
walking at the target.
-}
chase : Env -> ThinkerId -> World -> World
chase env id world =
    withMobj id world <|
        \actor0 ->
            let
                target0 =
                    actor0.target |> Maybe.andThen (\t -> World.mobj t world)

                threshold =
                    if actor0.threshold == 0 then
                        0

                    else
                        case target0 of
                            Just t ->
                                if t.health <= 0 then
                                    0

                                else
                                    actor0.threshold - 1

                            Nothing ->
                                0

                actor =
                    { actor0
                        | reactiontime =
                            if actor0.reactiontime /= 0 then
                                actor0.reactiontime - 1

                            else
                                0
                        , threshold = threshold
                        , angle = turnTowards actor0.movedir actor0.angle
                    }

                w1 =
                    World.setMobj actor world

                info =
                    infoOf env actor
            in
            case target0 of
                Just target ->
                    if not (Flags.has Flags.mfShootable target.flags) then
                        findNewTarget env id info w1

                    else if Flags.has Flags.mfJustAttacked actor.flags then
                        -- do not attack twice in a row
                        let
                            w2 =
                                World.setMobj { actor | flags = Flags.clear Flags.mfJustAttacked actor.flags } w1
                        in
                        if env.params.skill /= Nightmare && not env.params.fast then
                            newChaseDir env id w2

                        else
                            w2

                    else
                        chaseAttack env id info w1

                Nothing ->
                    findNewTarget env id info w1


{-| A\_Chase's turn: snap the angle to a multiple of 45 degrees, then turn 45 degrees
towards `movedir`.
-}
turnTowards : Int -> Angle -> Angle
turnTowards movedir angle =
    if movedir < 8 then
        let
            snapped =
                angle - modBy Angle.ang45 angle

            delta =
                Int32.wrap (snapped - movedir * Angle.ang45)
        in
        if delta > 0 then
            Angle.sub snapped Angle.ang45

        else if delta < 0 then
            Angle.add snapped Angle.ang45

        else
            snapped

    else
        angle


findNewTarget : Env -> ThinkerId -> MobjType.Info -> World -> World
findNewTarget env id info world =
    let
        ( found, w1 ) =
            lookForPlayers env id True world
    in
    if found then
        -- got a new target
        w1

    else
        setState env id info.spawnstate w1


chaseAttack : Env -> ThinkerId -> MobjType.Info -> World -> World
chaseAttack env id info world =
    let
        ( melee, w1 ) =
            if info.meleestate /= State.Null then
                checkMeleeRange env id world

            else
                ( False, world )
    in
    if melee then
        let
            w2 =
                if info.attacksound /= Sfx.None then
                    soundFrom id info.attacksound w1

                else
                    w1
        in
        setState env id info.meleestate w2

    else if info.missilestate /= State.Null then
        let
            movecount =
                World.mobj id w1 |> Maybe.map .movecount |> Maybe.withDefault 0
        in
        if Defs.skillIndex env.params.skill < Defs.skillIndex Nightmare && not env.params.fast && movecount /= 0 then
            noMissile env id info w1

        else
            let
                ( fireNow, w2 ) =
                    checkMissileRange env id w1
            in
            if fireNow then
                setState env id info.missilestate w2
                    |> World.updateMobj id (\m -> { m | flags = Flags.set Flags.mfJustAttacked m.flags })

            else
                noMissile env id info w2

    else
        noMissile env id info w1


{-| A\_Chase's `nomissile`: in a net game, maybe pick another player; then walk.
-}
noMissile : Env -> ThinkerId -> MobjType.Info -> World -> World
noMissile env id info world =
    case World.mobj id world of
        Nothing ->
            world

        Just actor ->
            case ( env.params.netgame && actor.threshold == 0, actor.target ) of
                ( True, Just targetId ) ->
                    let
                        ( seen, w1 ) =
                            Sight.checkSight env id targetId world
                    in
                    if seen then
                        chaseWalk env id info w1

                    else
                        let
                            ( found, w2 ) =
                                lookForPlayers env id True w1
                        in
                        if found then
                            w2

                        else
                            chaseWalk env id info w2

                _ ->
                    chaseWalk env id info world


chaseWalk : Env -> ThinkerId -> MobjType.Info -> World -> World
chaseWalk env id info world =
    case World.mobj id world of
        Nothing ->
            world

        Just actor ->
            let
                movecount =
                    actor.movecount - 1

                w1 =
                    World.setMobj { actor | movecount = movecount } world

                w2 =
                    if movecount < 0 then
                        newChaseDir env id w1

                    else
                        let
                            ( moved, wm ) =
                                move env id w1
                        in
                        if moved then
                            wm

                        else
                            newChaseDir env id wm
            in
            -- make active sound
            if info.activesound /= Sfx.None then
                let
                    ( r, w3 ) =
                        World.pRandom w2
                in
                if r < 3 then
                    soundFrom id info.activesound w3

                else
                    w3

            else
                w2


{-| A\_FaceTarget: turn to the target (inexactly, if it is fuzzy), and stop being deaf.
-}
faceTarget : Env -> ThinkerId -> World -> World
faceTarget _ id world =
    withTarget id world <|
        \actor target ->
            let
                faced =
                    { actor
                        | flags = Flags.clear Flags.mfAmbush actor.flags
                        , angle = Geometry.pointToAngle2 actor.x actor.y target.x target.y
                    }
            in
            if Flags.has Flags.mfShadow target.flags then
                let
                    ( spread, w1 ) =
                        randomSpread 21 world
                in
                World.setMobj { faced | angle = Angle.add faced.angle spread } w1

            else
                World.setMobj faced world



-- ATTACKS


{-| A\_PosAttack: the zombieman's pistol.
-}
posAttack : Env -> ThinkerId -> World -> World
posAttack env id world =
    withTarget id world <|
        \_ _ ->
            let
                w1 =
                    faceTarget env id world

                angle =
                    angleOf id w1

                ( slope, w2 ) =
                    Map.aimLineAttack env id angle Defs.missileRange w1
            in
            w2
                |> soundFrom id Sfx.Pistol
                |> hitscan env id angle slope


{-| One of the hitscan monsters' bullets: a random spread and damage, then the shot.
-}
hitscan : Env -> ThinkerId -> Angle -> Fixed -> World -> World
hitscan env id bangle slope world =
    let
        ( spread, w1 ) =
            randomSpread 20 world

        ( r, w2 ) =
            World.pRandom w1

        damage =
            (modBy 5 r + 1) * 3
    in
    Map.lineAttack env id (Angle.add bangle spread) Defs.missileRange slope damage w2


angleOf : ThinkerId -> World -> Angle
angleOf id world =
    World.mobj id world |> Maybe.map .angle |> Maybe.withDefault 0


{-| The shotgun guy and chaingunner's shots: the sound first, then aim.
-}
aimedShots : Int -> Env -> ThinkerId -> World -> World
aimedShots count env id world =
    withTarget id world <|
        \_ _ ->
            let
                w1 =
                    faceTarget env id (soundFrom id Sfx.Shotgn world)

                bangle =
                    angleOf id w1

                ( slope, w2 ) =
                    Map.aimLineAttack env id bangle Defs.missileRange w1
            in
            List.foldl (\_ w -> hitscan env id bangle slope w) w2 (List.range 1 count)


{-| A\_SPosAttack: three pellets.
-}
sPosAttack : Env -> ThinkerId -> World -> World
sPosAttack =
    aimedShots 3


{-| A\_CPosAttack: one bullet.
-}
cPosAttack : Env -> ThinkerId -> World -> World
cPosAttack =
    aimedShots 1


{-| Keep firing unless the target is dead or out of sight (A\_CPosRefire and
A\_SpidRefire, which differ in how likely they are to keep going regardless).
-}
refire : Int -> Env -> ThinkerId -> World -> World
refire chance env id world =
    let
        w1 =
            faceTarget env id world

        ( r, w2 ) =
            World.pRandom w1
    in
    if r < chance then
        w2

    else
        withMobj id w2 <|
            \actor ->
                case actor.target |> Maybe.andThen (\t -> World.mobj t w2) of
                    Just target ->
                        if target.health <= 0 then
                            setState env id (infoOf env actor).seestate w2

                        else
                            let
                                ( seen, w3 ) =
                                    Sight.checkSight env id target.id w2
                            in
                            if seen then
                                w3

                            else
                                setState env id (infoOf env actor).seestate w3

                    Nothing ->
                        setState env id (infoOf env actor).seestate w2


{-| A\_CPosRefire.
-}
cPosRefire : Env -> ThinkerId -> World -> World
cPosRefire =
    refire 40


{-| A\_SpidRefire.
-}
spidRefire : Env -> ThinkerId -> World -> World
spidRefire =
    refire 10


{-| P\_SpawnMissile at the actor's target, if it has one.
-}
launch : Env -> ThinkerId -> MobjType -> World -> World
launch env id missile world =
    withTarget id world <|
        \_ target ->
            Tuple.second (Movement.spawnMissile env id target.id missile world)


{-| P\_DamageMobj (actor->target, actor, actor, damage).
-}
hitTarget : Env -> ThinkerId -> Int -> World -> World
hitTarget env id damage world =
    withTarget id world <|
        \_ target ->
            Inter.damageMobj env target.id (Just id) (Just id) damage world


{-| A melee hit if the target is in range: `before` (a sound, or nothing) and then
`damage` of a random number. Nothing if it is out of range; `orElse` then.
-}
meleeOr : Env -> ThinkerId -> (World -> World) -> (Int -> Int) -> (World -> World) -> World -> World
meleeOr env id before damage orElse world =
    let
        ( inRange, w1 ) =
            checkMeleeRange env id world
    in
    if inRange then
        let
            ( r, w2 ) =
                World.pRandom (before w1)
        in
        hitTarget env id (damage r) w2

    else
        orElse w1


{-| A\_BspiAttack: the arachnotron's plasma.
-}
bspiAttack : Env -> ThinkerId -> World -> World
bspiAttack env id world =
    withTarget id world <|
        \_ _ ->
            launch env id MobjType.Arachplaz (faceTarget env id world)


{-| A\_TroopAttack: the imp's claw, or a fireball.
-}
troopAttack : Env -> ThinkerId -> World -> World
troopAttack env id world =
    withTarget id world <|
        \_ _ ->
            faceTarget env id world
                |> meleeOr env id (soundFrom id Sfx.Claw) (\r -> (modBy 8 r + 1) * 3) (launch env id MobjType.Troopshot)


{-| A\_SargAttack: the demon's bite.
-}
sargAttack : Env -> ThinkerId -> World -> World
sargAttack env id world =
    withTarget id world <|
        \_ _ ->
            faceTarget env id world
                |> meleeOr env id identity (\r -> (modBy 10 r + 1) * 4) identity


{-| A\_HeadAttack: the cacodemon's bite, or a ball of lightning.
-}
headAttack : Env -> ThinkerId -> World -> World
headAttack env id world =
    withTarget id world <|
        \_ _ ->
            faceTarget env id world
                |> meleeOr env id identity (\r -> (modBy 6 r + 1) * 10) (launch env id MobjType.Headshot)


{-| A\_CyberAttack: a rocket.
-}
cyberAttack : Env -> ThinkerId -> World -> World
cyberAttack env id world =
    withTarget id world <|
        \_ _ ->
            launch env id MobjType.Rocket (faceTarget env id world)


{-| A\_BruisAttack: the baron's claw, or a fireball (without turning to the target).
-}
bruisAttack : Env -> ThinkerId -> World -> World
bruisAttack env id world =
    withTarget id world <|
        \_ _ ->
            meleeOr env id (soundFrom id Sfx.Claw) (\r -> (modBy 8 r + 1) * 10) (launch env id MobjType.Bruisershot) world


{-| A\_SkelMissile: the revenant's homing rocket, launched from higher up and a step
ahead.
-}
skelMissile : Env -> ThinkerId -> World -> World
skelMissile env id world =
    withTarget id world <|
        \_ target ->
            let
                w1 =
                    faceTarget env id world
                        -- so missile spawns higher
                        |> World.updateMobj id (\m -> { m | z = Int32.add m.z (16 * Fixed.fracunit) })

                ( mo, w2 ) =
                    Movement.spawnMissile env id target.id MobjType.Tracer w1

                w3 =
                    World.updateMobj id (\m -> { m | z = Int32.sub m.z (16 * Fixed.fracunit) }) w2

                actorTarget =
                    World.mobj id w3 |> Maybe.andThen .target
            in
            World.updateMobj mo
                (\m ->
                    { m
                        | x = Int32.add m.x m.momx
                        , y = Int32.add m.y m.momy
                        , tracer = actorTarget
                    }
                )
                w3


{-| TRACEANGLE: how far a revenant rocket turns each time.
-}
traceAngle : Angle
traceAngle =
    0x0C000000


{-| A\_Tracer: every fourth game tic, a revenant rocket leaves smoke and steers
towards its quarry.
-}
tracer : Env -> ThinkerId -> World -> World
tracer env id world =
    if modBy 4 world.gametic /= 0 then
        world

    else
        withMobj id world <|
            \actor ->
                let
                    -- spawn a puff of smoke behind the rocket
                    w1 =
                        PlayMobj.spawnPuff env actor.x actor.y actor.z world

                    ( th, w2 ) =
                        PlayMobj.spawnMobj env (Int32.sub actor.x actor.momx) (Int32.sub actor.y actor.momy) actor.z MobjType.Smoke w1

                    ( r, w3 ) =
                        World.pRandom w2

                    w4 =
                        World.updateMobj th (\m -> { m | momz = Fixed.fracunit, tics = max 1 (m.tics - Bitwise.and r 3) }) w3
                in
                withMobj id w4 <|
                    \rocket ->
                        case rocket.tracer |> Maybe.andThen (\t -> World.mobj t w4) of
                            Just dest ->
                                if dest.health <= 0 then
                                    w4

                                else
                                    World.setMobj (steer env rocket dest) w4

                            Nothing ->
                                w4


{-| A\_Tracer's steering: turn by TRACEANGLE (but not past the target), then climb or
dive an eighth of a unit towards its height.
-}
steer : Env -> Mobj -> Mobj -> Mobj
steer env actor dest =
    let
        exact =
            Geometry.pointToAngle2 actor.x actor.y dest.x dest.y

        angle =
            if exact == actor.angle then
                actor.angle

            else if Angle.sub exact actor.angle > 0x80000000 then
                let
                    turned =
                        Angle.sub actor.angle traceAngle
                in
                if Angle.sub exact turned < 0x80000000 then
                    exact

                else
                    turned

            else
                let
                    turned =
                        Angle.add actor.angle traceAngle
                in
                if Angle.sub exact turned > 0x80000000 then
                    exact

                else
                    turned

        speed =
            (infoOf env actor).speed

        dist =
            max 1 (MapUtil.aproxDistance (Int32.sub dest.x actor.x) (Int32.sub dest.y actor.y) // speed)

        slope =
            Int32.sub (Int32.add dest.z (40 * Fixed.fracunit)) actor.z // dist
    in
    { actor
        | angle = angle
        , momx = Fixed.mul speed (Angle.cosine angle)
        , momy = Fixed.mul speed (Angle.sine angle)
        , momz =
            if slope < actor.momz then
                Int32.sub actor.momz (Fixed.fracunit // 8)

            else
                Int32.add actor.momz (Fixed.fracunit // 8)
    }


{-| A\_SkelWhoosh: the revenant swings.
-}
skelWhoosh : Env -> ThinkerId -> World -> World
skelWhoosh env id world =
    withTarget id world <|
        \_ _ ->
            soundFrom id Sfx.Skeswg (faceTarget env id world)


{-| A\_SkelFist: the revenant's punch (the sound comes after the damage roll).
-}
skelFist : Env -> ThinkerId -> World -> World
skelFist env id world =
    withTarget id world <|
        \_ _ ->
            let
                ( inRange, w1 ) =
                    checkMeleeRange env id (faceTarget env id world)
            in
            if inRange then
                let
                    ( r, w2 ) =
                        World.pRandom w1
                in
                w2
                    |> soundFrom id Sfx.Skepch
                    |> hitTarget env id ((modBy 10 r + 1) * 6)

            else
                w1



-- ARCH-VILE


{-| PIT\_VileCheck: whether the thing is a corpse the arch-vile, about to step to
(`viletryx`, `viletryy`), can raise; False (stop looking) if so, leaving it in
`corpsehit`.
-}
vileCheck : Env -> ThinkerId -> World -> ( Bool, World )
vileCheck env thingId world =
    case World.mobj thingId world of
        Nothing ->
            ( True, world )

        Just thing ->
            let
                info =
                    infoOf env thing

                maxdist =
                    info.radius + (Env.mobjInfo env MobjType.Vile).radius
            in
            if not (Flags.has Flags.mfCorpse thing.flags) then
                -- not a monster
                ( True, world )

            else if thing.tics /= -1 then
                -- not lying still yet
                ( True, world )

            else if info.raisestate == State.Null then
                -- monster doesn't have a raise state
                ( True, world )

            else if
                Int32.abs (Int32.sub thing.x world.scratch.viletryx)
                    > maxdist
                    || Int32.abs (Int32.sub thing.y world.scratch.viletryy)
                    > maxdist
            then
                -- not actually touching
                ( True, world )

            else
                let
                    w1 =
                        world
                            |> World.setScratch (\s -> { s | corpsehit = Just thingId })
                            |> World.setMobj { thing | momx = 0, momy = 0, height = Int32.shiftLeft 2 thing.height }

                    ( fits, w2 ) =
                        Map.checkPosition env thingId thing.x thing.y w1
                in
                ( not fits
                , World.updateMobj thingId (\m -> { m | height = Int32.shiftRight 2 m.height }) w2
                )


{-| A\_VileChase: raise a corpse the arch-vile is about to walk onto, if there is one;
otherwise A\_Chase.
-}
vileChase : Env -> ThinkerId -> World -> World
vileChase env id world =
    withMobj id world <|
        \actor ->
            if actor.movedir /= diNoDir then
                let
                    speed =
                        (infoOf env actor).speed

                    tryx =
                        Int32.add actor.x (Int32.mul speed (xspeed actor.movedir))

                    tryy =
                        Int32.add actor.y (Int32.mul speed (yspeed actor.movedir))

                    bm =
                        MapUtil.blockmap env world

                    cell org v delta =
                        Int32.shiftRight Defs.mapBlockShift (Int32.add (Int32.sub v org) delta)

                    reach =
                        Defs.maxRadius * 2

                    xs =
                        List.range (cell bm.orgx tryx -reach) (cell bm.orgx tryx reach)

                    ys =
                        List.range (cell bm.orgy tryy -reach) (cell bm.orgy tryy reach)

                    w1 =
                        World.setScratch (\s -> { s | viletryx = tryx, viletryy = tryy, vileobj = Just id }) world

                    ( found, w2 ) =
                        searchCells env (List.concatMap (\bx -> List.map (\by -> ( bx, by )) ys) xs) w1
                in
                if found then
                    case w2.scratch.corpsehit of
                        Just corpse ->
                            raiseCorpse env id corpse w2

                        Nothing ->
                            w2

                else
                    -- return to normal attack
                    chase env id w2

            else
                chase env id world


{-| Run PIT\_VileCheck over the blockmap cells in order; True once it stops.
-}
searchCells : Env -> List ( Int, Int ) -> World -> ( Bool, World )
searchCells env cells world =
    case cells of
        [] ->
            ( False, world )

        ( bx, by ) :: rest ->
            let
                ( continue, w1 ) =
                    MapUtil.blockThingsIterator env bx by (vileCheck env) world
            in
            if continue then
                searchCells env rest w1

            else
                ( True, w1 )


{-| A\_VileChase's "got one!": face the corpse, heal, and bring it back to life.
-}
raiseCorpse : Env -> ThinkerId -> ThinkerId -> World -> World
raiseCorpse env id corpse world =
    let
        temp =
            World.mobj id world |> Maybe.andThen .target

        w1 =
            world
                |> World.updateMobj id (\m -> { m | target = Just corpse })
                |> faceTarget env id
                |> World.updateMobj id (\m -> { m | target = temp })
                |> setState env id State.VileHeal1
                |> soundFrom corpse Sfx.Slop
    in
    withMobj corpse w1 <|
        \c ->
            let
                info =
                    infoOf env c
            in
            setState env corpse info.raisestate w1
                |> World.updateMobj corpse
                    (\m ->
                        { m
                            | height = Int32.shiftLeft 2 m.height
                            , flags = info.flags
                            , health = info.spawnhealth
                            , target = Nothing
                        }
                    )


{-| A\_VileStart.
-}
vileStart : Env -> ThinkerId -> World -> World
vileStart _ id =
    soundFrom id Sfx.Vilatk


{-| A\_StartFire.
-}
startFire : Env -> ThinkerId -> World -> World
startFire env id world =
    fire env id (soundFrom id Sfx.Flamst world)


{-| A\_FireCrackle.
-}
fireCrackle : Env -> ThinkerId -> World -> World
fireCrackle env id world =
    fire env id (soundFrom id Sfx.Flame world)


{-| A\_Fire: keep the arch-vile's fire in front of its victim, unless the vile has lost
sight of them.
-}
fire : Env -> ThinkerId -> World -> World
fire env id world =
    withMobj id world <|
        \actor ->
            case ( actor.tracer, actor.target ) of
                ( Just dest, Just vile ) ->
                    let
                        ( seen, w1 ) =
                            Sight.checkSight env vile dest world
                    in
                    if not seen then
                        w1

                    else
                        withMobj dest w1 <|
                            \d ->
                                MapUtil.unsetThingPosition env id w1
                                    |> World.updateMobj id
                                        (\m ->
                                            { m
                                                | x = Int32.add d.x (Fixed.mul (24 * Fixed.fracunit) (Angle.cosine d.angle))
                                                , y = Int32.add d.y (Fixed.mul (24 * Fixed.fracunit) (Angle.sine d.angle))
                                                , z = d.z
                                            }
                                        )
                                    |> MapUtil.setThingPosition env id

                _ ->
                    -- no victim; (C would crash on a fire with no vile)
                    world


{-| A\_VileTarget: spawn the hellfire on the target. The original spawns it at the
target's x for both x and y; so does this.
-}
vileTarget : Env -> ThinkerId -> World -> World
vileTarget env id world =
    withTarget id world <|
        \_ _ ->
            let
                w1 =
                    faceTarget env id world
            in
            withTarget id w1 <|
                \_ target ->
                    let
                        ( fog, w2 ) =
                            PlayMobj.spawnMobj env target.x target.x target.z MobjType.Fire w1
                    in
                    w2
                        |> World.updateMobj id (\m -> { m | tracer = Just fog })
                        |> World.updateMobj fog (\m -> { m | target = Just id, tracer = Just target.id })
                        |> fire env fog


{-| A\_VileAttack: hurt and launch the target, and explode the fire between them.
-}
vileAttack : Env -> ThinkerId -> World -> World
vileAttack env id world =
    withTarget id world <|
        \_ target ->
            let
                w1 =
                    faceTarget env id world

                ( seen, w2 ) =
                    Sight.checkSight env id target.id w1
            in
            if not seen then
                w2

            else
                let
                    w3 =
                        w2
                            |> soundFrom id Sfx.Barexp
                            |> Inter.damageMobj env target.id (Just id) (Just id) 20
                            |> World.updateMobj target.id (\m -> { m | momz = (1000 * Fixed.fracunit) // (infoOf env m).mass })
                in
                case ( World.mobj id w3, World.mobj target.id w3 ) of
                    ( Just actor, Just victim ) ->
                        case actor.tracer of
                            Just fireId ->
                                w3
                                    -- move the fire between the vile and the player
                                    |> World.updateMobj fireId
                                        (\m ->
                                            { m
                                                | x = Int32.sub victim.x (Fixed.mul (24 * Fixed.fracunit) (Angle.cosine actor.angle))
                                                , y = Int32.sub victim.y (Fixed.mul (24 * Fixed.fracunit) (Angle.sine actor.angle))
                                            }
                                        )
                                    |> Map.radiusAttack env fireId (Just id) 70

                            Nothing ->
                                w3

                    _ ->
                        w3



-- MANCUBUS


{-| FATSPREAD.
-}
fatSpread : Angle
fatSpread =
    Angle.ang90 // 8


{-| A\_FatRaise.
-}
fatRaise : Env -> ThinkerId -> World -> World
fatRaise env id world =
    soundFrom id Sfx.Manatk (faceTarget env id world)


{-| A mancubus fireball at the target, turned by `delta` from where P\_SpawnMissile
aimed it.
-}
fatShot : Env -> ThinkerId -> Int -> World -> World
fatShot env id delta world =
    withTarget id world <|
        \_ target ->
            let
                ( mo, w1 ) =
                    Movement.spawnMissile env id target.id MobjType.Fatshot world
            in
            World.updateMobj mo
                (\m ->
                    let
                        angle =
                            Angle.add m.angle delta

                        speed =
                            (infoOf env m).speed
                    in
                    { m | angle = angle, momx = Fixed.mul speed (Angle.cosine angle), momy = Fixed.mul speed (Angle.sine angle) }
                )
                w1


turnBy : Int -> ThinkerId -> World -> World
turnBy delta id =
    World.updateMobj id (\m -> { m | angle = Angle.add m.angle delta })


{-| A\_FatAttack1: two fireballs, turned left.
-}
fatAttack1 : Env -> ThinkerId -> World -> World
fatAttack1 env id world =
    faceTarget env id world
        |> turnBy fatSpread id
        |> launch env id MobjType.Fatshot
        |> fatShot env id fatSpread


{-| A\_FatAttack2: two fireballs, turned right.
-}
fatAttack2 : Env -> ThinkerId -> World -> World
fatAttack2 env id world =
    faceTarget env id world
        |> turnBy -fatSpread id
        |> launch env id MobjType.Fatshot
        |> fatShot env id (-fatSpread * 2)


{-| A\_FatAttack3: two fireballs either side of the target.
-}
fatAttack3 : Env -> ThinkerId -> World -> World
fatAttack3 env id world =
    faceTarget env id world
        |> fatShot env id (-fatSpread // 2)
        |> fatShot env id (fatSpread // 2)



-- LOST SOULS AND PAIN ELEMENTALS


{-| SKULLSPEED.
-}
skullSpeed : Fixed
skullSpeed =
    20 * Fixed.fracunit


{-| A\_SkullAttack: fly at the target like a missile.
-}
skullAttack : Env -> ThinkerId -> World -> World
skullAttack env id world =
    withTarget id world <|
        \actor _ ->
            let
                w1 =
                    world
                        |> World.setMobj { actor | flags = Flags.set Flags.mfSkullFly actor.flags }
                        |> soundFrom id (infoOf env actor).attacksound
                        |> faceTarget env id
            in
            withTarget id w1 <|
                \flier dest ->
                    let
                        dist =
                            max 1 (MapUtil.aproxDistance (Int32.sub dest.x flier.x) (Int32.sub dest.y flier.y) // skullSpeed)
                    in
                    World.setMobj
                        { flier
                            | momx = Fixed.mul skullSpeed (Angle.cosine flier.angle)
                            , momy = Fixed.mul skullSpeed (Angle.sine flier.angle)
                            , momz = Int32.sub (Int32.add dest.z (Int32.shiftRight 1 dest.height)) flier.z // dist
                        }
                        w1


{-| A\_PainShootSkull: spit a lost soul in this direction, unless there are already
more than 20; one spawned stuck in a wall dies at once.
-}
painShootSkull : Env -> ThinkerId -> Angle -> World -> World
painShootSkull env id angle world =
    let
        count =
            List.length (List.filter (\m -> m.type_ == MobjType.Skull) (liveMobjs world))
    in
    if count > 20 then
        world

    else
        withMobj id world <|
            \actor ->
                let
                    prestep =
                        4 * Fixed.fracunit + (3 * ((infoOf env actor).radius + (Env.mobjInfo env MobjType.Skull).radius)) // 2

                    x =
                        Int32.add actor.x (Fixed.mul prestep (Angle.cosine angle))

                    y =
                        Int32.add actor.y (Fixed.mul prestep (Angle.sine angle))

                    z =
                        Int32.add actor.z (8 * Fixed.fracunit)

                    ( newmobj, w1 ) =
                        PlayMobj.spawnMobj env x y z MobjType.Skull world
                in
                withMobj newmobj w1 <|
                    \skull ->
                        let
                            ( ok, w2 ) =
                                Map.tryMove env newmobj skull.x skull.y w1
                        in
                        if not ok then
                            -- kill it immediately
                            Inter.damageMobj env newmobj (Just id) (Just id) 10000 w2

                        else
                            let
                                target =
                                    World.mobj id w2 |> Maybe.andThen .target
                            in
                            w2
                                |> World.updateMobj newmobj (\m -> { m | target = target })
                                |> skullAttack env newmobj


{-| A\_PainAttack: spit a lost soul at the target.
-}
painAttack : Env -> ThinkerId -> World -> World
painAttack env id world =
    withTarget id world <|
        \_ _ ->
            let
                w1 =
                    faceTarget env id world
            in
            painShootSkull env id (angleOf id w1) w1


{-| A\_PainDie: three lost souls, to the sides and behind.
-}
painDie : Env -> ThinkerId -> World -> World
painDie env id world =
    let
        shoot turn w =
            painShootSkull env id (Angle.add (angleOf id w) turn) w
    in
    fall env id world
        |> shoot Angle.ang90
        |> shoot Angle.ang180
        |> shoot Angle.ang270



-- SOUNDS AND DEATHS


{-| A\_Scream: the death sound (a random one of the possessed or imp sounds), heard
everywhere for the two big bosses.
-}
scream : Env -> ThinkerId -> World -> World
scream env id world =
    withMobj id world <|
        \actor ->
            let
                deathsound =
                    (infoOf env actor).deathsound

                ( sound, w1 ) =
                    case deathsound of
                        Sfx.Podth1 ->
                            randomOf [ Sfx.Podth1, Sfx.Podth2, Sfx.Podth3 ] world

                        Sfx.Podth2 ->
                            randomOf [ Sfx.Podth1, Sfx.Podth2, Sfx.Podth3 ] world

                        Sfx.Podth3 ->
                            randomOf [ Sfx.Podth1, Sfx.Podth2, Sfx.Podth3 ] world

                        Sfx.Bgdth1 ->
                            randomOf [ Sfx.Bgdth1, Sfx.Bgdth2 ] world

                        Sfx.Bgdth2 ->
                            randomOf [ Sfx.Bgdth1, Sfx.Bgdth2 ] world

                        _ ->
                            ( deathsound, world )
            in
            if deathsound == Sfx.None then
                world

            else if actor.type_ == MobjType.Spider || actor.type_ == MobjType.Cyborg then
                -- full volume
                soundEverywhere sound w1

            else
                soundFrom id sound w1


{-| A\_XScream: gibbed.
-}
xScream : Env -> ThinkerId -> World -> World
xScream _ id =
    soundFrom id Sfx.Slop


{-| A\_Pain.
-}
pain : Env -> ThinkerId -> World -> World
pain env id world =
    withMobj id world <|
        \actor ->
            let
                painsound =
                    (infoOf env actor).painsound
            in
            if painsound /= Sfx.None then
                soundFrom id painsound world

            else
                world


{-| A\_Fall: the body can now be walked over.
-}
fall : Env -> ThinkerId -> World -> World
fall _ id =
    World.updateMobj id (\m -> { m | flags = Flags.clear Flags.mfSolid m.flags })


{-| A\_Explode: a rocket's or barrel's blast.
-}
explode : Env -> ThinkerId -> World -> World
explode env id world =
    withMobj id world <|
        \thingy ->
            Map.radiusAttack env id thingy.target 128 world


{-| A\_BossDeath: when the last boss of the level's kind dies (and a player is still
alive), lower the floors or open the doors tagged 666 (or 667), or end the level.
-}
bossDeath : Env -> ThinkerId -> World -> World
bossDeath env id world =
    withMobj id world <|
        \mo ->
            let
                someoneAlive =
                    List.any
                        (\i ->
                            (Array.get i world.playeringame |> Maybe.withDefault False)
                                && (World.player i world |> Maybe.map (\p -> p.health > 0) |> Maybe.withDefault False)
                        )
                        (List.range 0 (Defs.maxPlayers - 1))
            in
            if not (isBossLevelBoss env mo) || not someoneAlive || otherOfTypeAlive mo world then
                world

            else
                bossVictory env mo world


isBossLevelBoss : Env -> Mobj -> Bool
isBossLevelBoss env mo =
    let
        p =
            env.params
    in
    if p.gameMode == Commercial then
        p.map == 7 && (mo.type_ == MobjType.Fatso || mo.type_ == MobjType.Baby)

    else
        case p.episode of
            1 ->
                p.map == 8 && mo.type_ == MobjType.Bruiser

            2 ->
                p.map == 8 && mo.type_ == MobjType.Cyborg

            3 ->
                p.map == 8 && mo.type_ == MobjType.Spider

            4 ->
                case p.map of
                    6 ->
                        mo.type_ == MobjType.Cyborg

                    8 ->
                        mo.type_ == MobjType.Spider

                    _ ->
                        False

            _ ->
                p.map == 8


bossVictory : Env -> Mobj -> World -> World
bossVictory env mo world =
    let
        p =
            env.params

        floor tag floorType =
            Tuple.second (Floor.doFloorByTag env tag floorType world)
    in
    if p.gameMode == Commercial then
        if p.map == 7 && mo.type_ == MobjType.Fatso then
            floor 666 Thinker.LowerFloorToLowest

        else if p.map == 7 && mo.type_ == MobjType.Baby then
            floor 667 Thinker.RaiseToTexture

        else
            exitLevel world

    else
        case ( p.episode, p.map ) of
            ( 1, _ ) ->
                floor 666 Thinker.LowerFloorToLowest

            ( 4, 6 ) ->
                Tuple.second (Door.doDoorByTag env 666 Thinker.BlazeOpen world)

            ( 4, 8 ) ->
                floor 666 Thinker.LowerFloorToLowest

            _ ->
                exitLevel world


{-| A\_Hoof: the cyberdemon's step.
-}
hoof : Env -> ThinkerId -> World -> World
hoof env id world =
    chase env id (soundFrom id Sfx.Hoof world)


{-| A\_Metal: the cyberdemon's and spider mastermind's metal step.
-}
metal : Env -> ThinkerId -> World -> World
metal env id world =
    chase env id (soundFrom id Sfx.Metal world)


{-| A\_BabyMetal: the arachnotron's step.
-}
babyMetal : Env -> ThinkerId -> World -> World
babyMetal env id world =
    chase env id (soundFrom id Sfx.Bspwlk world)



-- THE BOSS BRAIN


{-| A\_BrainAwake: find every spawn spot (MT\_BOSSTARGET), in thinker order.
-}
brainAwake : Env -> ThinkerId -> World -> World
brainAwake _ _ world =
    let
        targets =
            liveMobjs world
                |> List.filter (\m -> m.type_ == MobjType.Bosstarget)
                |> List.map .id
                |> Array.fromList
    in
    soundEverywhere Sfx.Bossit { world | brainTargets = targets, brainTargetOn = 0 }


{-| A\_BrainPain.
-}
brainPain : Env -> ThinkerId -> World -> World
brainPain _ _ =
    soundEverywhere Sfx.Bospn


{-| A\_BrainScream: a row of explosions along the brain's wall.
-}
brainScream : Env -> ThinkerId -> World -> World
brainScream env id world =
    withMobj id world <|
        \mo ->
            let
                y =
                    Int32.sub mo.y (320 * Fixed.fracunit)

                limit =
                    Int32.add mo.x (320 * Fixed.fracunit)

                loop x w =
                    if x < limit then
                        loop (x + Fixed.fracunit * 8) (brainRocket env x y w)

                    else
                        w
            in
            loop (Int32.sub mo.x (196 * Fixed.fracunit)) world
                |> soundEverywhere Sfx.Bosdth


{-| One of the brain's explosions: a rocket at a random height, rising at a random
speed, put straight into its exploding state.
-}
brainRocket : Env -> Fixed -> Fixed -> World -> World
brainRocket env x y world =
    let
        ( r1, w1 ) =
            World.pRandom world

        z =
            128 + r1 * 2 * Fixed.fracunit

        ( th, w2 ) =
            PlayMobj.spawnMobj env x y z MobjType.Rocket w1

        ( r2, w3 ) =
            World.pRandom w2

        w4 =
            World.updateMobj th (\m -> { m | momz = r2 * 512 }) w3
                |> setState env th State.Brainexplode1

        ( r3, w5 ) =
            World.pRandom w4
    in
    World.updateMobj th (\m -> { m | tics = max 1 (m.tics - Bitwise.and r3 7) }) w5


{-| A\_BrainExplode: one more explosion near the dying brain.
-}
brainExplode : Env -> ThinkerId -> World -> World
brainExplode env id world =
    withMobj id world <|
        \mo ->
            let
                ( spread, w1 ) =
                    randomSpread 0 world
            in
            brainRocket env (Int32.add mo.x (Int32.mul spread 2048)) mo.y w1


{-| A\_BrainDie: the end.
-}
brainDie : Env -> ThinkerId -> World -> World
brainDie _ _ =
    exitLevel


{-| A\_BrainSpit: shoot a spawn cube at the next spawn spot (only every other time on
the easy skills, `World.brainEasy` being C's static `easy`). The cube's
`reactiontime` is how many of its states pass before it gets there.
-}
brainSpit : Env -> ThinkerId -> World -> World
brainSpit env id world0 =
    let
        world =
            { world0 | brainEasy = not world0.brainEasy }

        numTargets =
            Array.length world.brainTargets
    in
    if Defs.skillIndex env.params.skill <= Defs.skillIndex Easy && not world.brainEasy then
        world

    else
        case Array.get world.brainTargetOn world.brainTargets of
            Nothing ->
                -- no spawn spots: C divides by zero here
                world

            Just targ ->
                let
                    w1 =
                        { world | brainTargetOn = modBy numTargets (world.brainTargetOn + 1) }

                    -- spawn brain missile
                    ( newmobj, w2 ) =
                        Movement.spawnMissile env id targ MobjType.Spawnshot w1
                in
                case ( World.mobj id w2, World.mobj targ w2, World.mobj newmobj w2 ) of
                    ( Just mo, Just t, Just cube ) ->
                        World.setMobj
                            { cube
                                | target = Just targ

                                -- (C would divide by zero for a cube with no y speed)
                                , reactiontime = (Int32.sub t.y mo.y // cube.momy) // (Env.stateDef env cube.state).tics
                            }
                            w2
                            |> soundEverywhere Sfx.Bospit

                    _ ->
                        soundEverywhere Sfx.Bospit w2


{-| A\_SpawnSound: the travelling cube's sound.
-}
spawnSound : Env -> ThinkerId -> World -> World
spawnSound env id world =
    spawnFly env id (soundFrom id Sfx.Boscub world)


{-| A\_SpawnFly: when the cube arrives, spawn fire and a random monster at the spot,
telefragging whatever is there, and remove the cube.
-}
spawnFly : Env -> ThinkerId -> World -> World
spawnFly env id world =
    withMobj id world <|
        \mo0 ->
            let
                mo =
                    { mo0 | reactiontime = mo0.reactiontime - 1 }

                w0 =
                    World.setMobj mo world
            in
            if mo.reactiontime /= 0 then
                -- still flying
                w0

            else
                case mo.target |> Maybe.andThen (\t -> World.mobj t w0) of
                    Nothing ->
                        w0

                    Just targ ->
                        let
                            -- first spawn teleport fog
                            ( fog, w1 ) =
                                PlayMobj.spawnMobj env targ.x targ.y targ.z MobjType.Spawnfire w0

                            ( r, w2 ) =
                                World.pRandom (soundFrom fog Sfx.Telept w1)

                            ( newmobj, w3 ) =
                                PlayMobj.spawnMobj env targ.x targ.y targ.z (spawnFlyType r) w2

                            ( found, w4 ) =
                                lookForPlayers env newmobj True w3

                            w5 =
                                if found then
                                    case World.mobj newmobj w4 of
                                        Just m ->
                                            setState env newmobj (infoOf env m).seestate w4

                                        Nothing ->
                                            w4

                                else
                                    w4

                            -- telefrag anything in this spot
                            w6 =
                                case World.mobj newmobj w5 of
                                    Just m ->
                                        Tuple.second (Map.teleportMove env newmobj m.x m.y w5)

                                    Nothing ->
                                        w5
                        in
                        -- remove self (i.e., cube)
                        PlayMobj.removeMobj env id w6


{-| The monster a spawn cube brings, by a random number: imps most likely, the
arch-vile least.
-}
spawnFlyType : Int -> MobjType
spawnFlyType r =
    if r < 50 then
        MobjType.Troop

    else if r < 90 then
        MobjType.Sergeant

    else if r < 120 then
        MobjType.Shadows

    else if r < 130 then
        MobjType.Pain

    else if r < 160 then
        MobjType.Head

    else if r < 162 then
        MobjType.Vile

    else if r < 172 then
        MobjType.Undead

    else if r < 192 then
        MobjType.Baby

    else if r < 222 then
        MobjType.Fatso

    else if r < 246 then
        MobjType.Knight

    else
        MobjType.Bruiser


{-| A\_PlayerScream: the player's death cry, a worse one in Doom II for a death below
-50 health.
-}
playerScream : Env -> ThinkerId -> World -> World
playerScream env id world =
    withMobj id world <|
        \mo ->
            if env.params.gameMode == Commercial && mo.health < -50 then
                soundFrom id Sfx.Pdiehi world

            else
                soundFrom id Sfx.Pldeth world
