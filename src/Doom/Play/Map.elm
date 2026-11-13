module Doom.Play.Map exposing (teleportMove, checkPosition, tryMove, slideMove, aimLineAttack, lineAttack, useLines, radiusAttack, changeSector)

{-| Moving things and what they hit (p\_map.c): collision checks, moves, sliding,
hitscan aiming and attacks, using lines, explosions, and crushing when floors and
ceilings move. Results the C source leaves in globals (`tmfloorz`, `floatok`,
`linetarget`...) are in `World.scratch`.

@docs teleportMove, checkPosition, tryMove, slideMove, aimLineAttack, lineAttack, useLines, radiusAttack, changeSector

-}

import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Geometry as Geometry
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx
import Doom.Info.State as State
import Doom.Int32 as Int32
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Inter as Inter
import Doom.Play.MapUtil as MapUtil exposing (Intercept, InterceptTarget(..))
import Doom.Play.Mobj as PlayMobj
import Doom.Play.Sight as Sight
import Doom.Sound as Sound
import Doom.World as World exposing (World)



-- TELEPORT MOVE


{-| P\_TeleportMove thing x y: move there, killing whatever is in the way.

Only players stomp things, except on MAP30, where monsters do too (the boss brain's
spawned monsters telefrag what they land on). A monster blocked by a thing doesn't move.

-}
teleportMove : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
teleportMove env id x y world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just thing ->
            let
                started =
                    startCheck env thing x y world

                bbox =
                    started.scratch.tmbbox

                ( ok, stomped ) =
                    eachCell (thingCells env bbox)
                        (\bx by -> MapUtil.blockThingsIterator env bx by (stompThing env))
                        started
            in
            if not ok then
                ( False, stomped )

            else
                -- the move is ok, so link the thing into its new position
                ( True, relink env id x y stomped )


{-| PIT\_StompThing.
-}
stompThing : Env -> ThinkerId -> World -> ( Bool, World )
stompThing env thingId world =
    case ( World.mobj thingId world, tmThing world ) of
        ( Just thing, Just tm ) ->
            let
                blockdist =
                    Int32.add thing.radius tm.radius
            in
            if not (Flags.has Flags.mfShootable thing.flags) then
                ( True, world )

            else if
                (Int32.abs (Int32.sub thing.x world.scratch.tmx) >= blockdist)
                    || (Int32.abs (Int32.sub thing.y world.scratch.tmy) >= blockdist)
            then
                -- didn't hit it
                ( True, world )

            else if thing.id == tm.id then
                -- don't clip against self
                ( True, world )

            else if tm.player == Nothing && env.params.map /= 30 then
                -- monsters don't stomp things except on the boss level
                ( False, world )

            else
                ( True, Inter.damageMobj env thingId (Just tm.id) (Just tm.id) 10000 world )

        _ ->
            ( True, world )



-- MOVEMENT CLIPPING


{-| P\_CheckPosition thing x y.

Whether the thing could stand at (x, y), as far as things and walls go: things first
(picking up items it touches if it can pick them up, a lost soul slamming into what it
hits, a missile exploding on what it hits), then lines. Leaves in the scratch the floor
and ceiling heights there (`tmfloorz`, `tmceilingz`), the lowest floor nearby
(`tmdropoffz`), the line that lowered the ceiling (`ceilingline`) and the special
lines touched (`spechit`).

-}
checkPosition : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
checkPosition env id x y world =
    case World.mobj id world of
        Nothing ->
            ( False, world )

        Just thing ->
            let
                started =
                    startCheck env thing x y world

                bbox =
                    started.scratch.tmbbox
            in
            if Flags.has Flags.mfNoClip started.scratch.tmflags then
                ( True, started )

            else
                let
                    -- Things are linked into the cell their centre is in and can
                    -- overlap the next cells by up to MAXRADIUS, so the box is grown
                    -- by that much to look for them.
                    ( thingsOk, afterThings ) =
                        eachCell (thingCells env bbox)
                            (\bx by -> MapUtil.blockThingsIterator env bx by (checkThing env))
                            started
                in
                if not thingsOk then
                    ( False, afterThings )

                else
                    eachCell (lineCells env bbox)
                        (\bx by -> MapUtil.blockLinesIterator env bx by (checkLine env))
                        afterThings


{-| The start of P\_CheckPosition and P\_TeleportMove: the `tm` globals for a check of
`thing` at (x, y), the floor and ceiling of the sector there, and a new validcount.
-}
startCheck : Env -> Mobj -> Fixed -> Fixed -> World -> World
startCheck env thing x y world =
    let
        newsec =
            World.sector (Level.subsector (Geometry.pointInSubsector x y env.level) env.level).sector world

        ( _, counted ) =
            World.nextValidcount world
    in
    World.setScratch
        (\s ->
            { s
                | tmthing = Just thing.id
                , tmflags = thing.flags
                , tmx = x
                , tmy = y
                , tmbbox =
                    { top = Int32.add y thing.radius
                    , bottom = Int32.sub y thing.radius
                    , right = Int32.add x thing.radius
                    , left = Int32.sub x thing.radius
                    }
                , ceilingline = Nothing
                , tmfloorz = newsec.floorheight
                , tmdropoffz = newsec.floorheight
                , tmceilingz = newsec.ceilingheight
                , spechit = []
            }
        )
        counted


{-| PIT\_CheckThing.
-}
checkThing : Env -> ThinkerId -> World -> ( Bool, World )
checkThing env thingId world =
    case ( World.mobj thingId world, tmThing world ) of
        ( Just thing, Just tm ) ->
            let
                blockdist =
                    Int32.add thing.radius tm.radius
            in
            if not (Flags.has (Flags.mfSolid + Flags.mfSpecial + Flags.mfShootable) thing.flags) then
                ( True, world )

            else if
                (Int32.abs (Int32.sub thing.x world.scratch.tmx) >= blockdist)
                    || (Int32.abs (Int32.sub thing.y world.scratch.tmy) >= blockdist)
            then
                -- didn't hit it
                ( True, world )

            else if thing.id == tm.id then
                -- don't clip against self
                ( True, world )

            else if Flags.has Flags.mfSkullFly tm.flags then
                skullSlam env thing tm world

            else if Flags.has Flags.mfMissile tm.flags then
                missileHit env thing tm world

            else if Flags.has Flags.mfSpecial thing.flags then
                let
                    solid =
                        Flags.has Flags.mfSolid thing.flags
                in
                if Flags.has Flags.mfPickup world.scratch.tmflags then
                    -- can remove the thing
                    ( not solid, Inter.touchSpecialThing env thing.id tm.id world )

                else
                    ( not solid, world )

            else
                ( not (Flags.has Flags.mfSolid thing.flags), world )

        _ ->
            ( True, world )


{-| A lost soul slamming into a thing: it hurts it, stops, and goes back to its spawn
state.
-}
skullSlam : Env -> Mobj -> Mobj -> World -> ( Bool, World )
skullSlam env thing tm world =
    let
        ( r, w1 ) =
            World.pRandom world

        damage =
            (remainderBy 8 r + 1) * (Env.mobjInfo env tm.type_).damage

        w2 =
            Inter.damageMobj env thing.id (Just tm.id) (Just tm.id) damage w1
                |> World.updateMobj tm.id
                    (\m -> { m | flags = Flags.clear Flags.mfSkullFly m.flags, momx = 0, momy = 0, momz = 0 })

        ( _, w3 ) =
            Env.setMobjState env tm.id (Env.mobjInfo env tm.type_).spawnstate w2
    in
    -- stop moving
    ( False, w3 )


{-| A missile reaching a thing: it passes over or under it, doesn't hurt its shooter's
own kind (barons and knights count as one), and otherwise explodes on it, hurting it if
it can be hurt.
-}
missileHit : Env -> Mobj -> Mobj -> World -> ( Bool, World )
missileHit env thing tm world =
    let
        sameSpecies target =
            (target.type_ == thing.type_)
                || (target.type_ == MobjType.Knight && thing.type_ == MobjType.Bruiser)
                || (target.type_ == MobjType.Bruiser && thing.type_ == MobjType.Knight)

        shooter =
            tm.target |> Maybe.andThen (\t -> World.mobj t world)
    in
    if tm.z > Int32.add thing.z thing.height then
        -- overhead
        ( True, world )

    else if Int32.add tm.z tm.height < thing.z then
        -- underneath
        ( True, world )

    else
        case shooter of
            Just target ->
                if sameSpecies target && thing.id == target.id then
                    -- don't hit the originator
                    ( True, world )

                else if sameSpecies target && thing.type_ /= MobjType.Player then
                    -- explode, but do no damage; players can still shoot each other
                    ( False, world )

                else
                    missileDamage env thing tm world

            Nothing ->
                missileDamage env thing tm world


missileDamage : Env -> Mobj -> Mobj -> World -> ( Bool, World )
missileDamage env thing tm world =
    if not (Flags.has Flags.mfShootable thing.flags) then
        -- didn't do any damage
        ( not (Flags.has Flags.mfSolid thing.flags), world )

    else
        let
            ( r, w1 ) =
                World.pRandom world

            damage =
                (remainderBy 8 r + 1) * (Env.mobjInfo env tm.type_).damage
        in
        -- damage, explode, and don't go on
        ( False, Inter.damageMobj env thing.id (Just tm.id) tm.target damage w1 )


{-| PIT\_CheckLine: whether the moving thing's box may cross the line, lowering
`tmceilingz` and raising `tmfloorz` to the line's opening, and noting special lines.

Special lines are noted in the order found, not the order they would be crossed in.
The C source keeps at most MAXSPECIALCROSS (8) and writes past its array beyond that;
here there is no limit.

-}
checkLine : Env -> LineId -> World -> ( Bool, World )
checkLine env lineId world =
    let
        ld =
            Level.line lineId env.level

        bbox =
            world.scratch.tmbbox
    in
    if
        (bbox.right <= ld.bbox.left)
            || (bbox.left >= ld.bbox.right)
            || (bbox.top <= ld.bbox.bottom)
            || (bbox.bottom >= ld.bbox.top)
    then
        ( True, world )

    else if MapUtil.boxOnLineSide bbox ld /= -1 then
        ( True, world )

    else if ld.backsector == Nothing then
        -- one-sided line
        ( False, world )

    else
        let
            tm =
                tmThing world

            isMissile =
                Maybe.map (.flags >> Flags.has Flags.mfMissile) tm |> Maybe.withDefault False

            isPlayer =
                Maybe.map (.player >> (/=) Nothing) tm |> Maybe.withDefault False
        in
        if not isMissile && Flags.has Flags.mlBlocking ld.flags then
            -- explicitly blocking everything
            ( False, world )

        else if not isMissile && not isPlayer && Flags.has Flags.mlBlockMonsters ld.flags then
            -- blocks monsters only
            ( False, world )

        else
            let
                opened =
                    MapUtil.lineOpening env lineId world

                special =
                    (World.lineState lineId opened).special
            in
            ( True
            , World.setScratch
                (\s ->
                    let
                        lowered =
                            if s.opentop < s.tmceilingz then
                                { s | tmceilingz = s.opentop, ceilingline = Just lineId }

                            else
                                s

                        raised =
                            if lowered.openbottom > lowered.tmfloorz then
                                { lowered | tmfloorz = lowered.openbottom }

                            else
                                lowered

                        dropped =
                            if raised.lowfloor < raised.tmdropoffz then
                                { raised | tmdropoffz = raised.lowfloor }

                            else
                                raised
                    in
                    if special /= 0 then
                        { dropped | spechit = dropped.spechit ++ [ lineId ] }

                    else
                        dropped
                )
                opened
            )


{-| P\_TryMove thing x y.

Moves the thing if it fits there: it must not hit a wall or a solid thing, must fit
between floor and ceiling, can step up at most 24 units, and (unless it can drop off
ledges or fly) mustn't stand over a drop of more than 24. `floatok` in the scratch says
whether the move would have been fine at another height. Special lines whose side the
thing ends up changing are triggered, the last one found first.

-}
tryMove : Env -> ThinkerId -> Fixed -> Fixed -> World -> ( Bool, World )
tryMove env id x y world0 =
    let
        world =
            World.setScratch (\s -> { s | floatok = False }) world0

        ( ok, checked ) =
            checkPosition env id x y world
    in
    if not ok then
        -- a solid wall or thing
        ( False, checked )

    else
        case World.mobj id checked of
            Nothing ->
                ( False, checked )

            Just thing ->
                let
                    s =
                        checked.scratch

                    teleporting =
                        Flags.has Flags.mfTeleport thing.flags
                in
                if Flags.has Flags.mfNoClip thing.flags then
                    ( True, completeMove env thing x y checked )

                else if Int32.sub s.tmceilingz s.tmfloorz < thing.height then
                    -- doesn't fit
                    ( False, checked )

                else
                    let
                        floating =
                            World.setScratch (\sc -> { sc | floatok = True }) checked
                    in
                    if not teleporting && Int32.sub s.tmceilingz thing.z < thing.height then
                        -- must lower itself to fit
                        ( False, floating )

                    else if not teleporting && Int32.sub s.tmfloorz thing.z > 24 * Fixed.fracunit then
                        -- too big a step up
                        ( False, floating )

                    else if
                        not (Flags.has (Flags.mfDropOff + Flags.mfFloat) thing.flags)
                            && (Int32.sub s.tmfloorz s.tmdropoffz > 24 * Fixed.fracunit)
                    then
                        -- don't stand over a drop-off
                        ( False, floating )

                    else
                        ( True, completeMove env thing x y floating )


{-| The end of P\_TryMove: link the thing in at its new position, then cross the special
lines it changed sides of.

The C source loops `while (numspechit--)` over its global list, so a crossing that
runs another check (a teleport clears the list) changes what is left to do; the list
here is likewise read from the scratch each time round.

-}
completeMove : Env -> Mobj -> Fixed -> Fixed -> World -> World
completeMove env thing x y world =
    let
        moved =
            relink env thing.id x y world

        crossing =
            not (Flags.has (Flags.mfTeleport + Flags.mfNoClip) thing.flags)
    in
    if crossing then
        crossSpecials env thing.id thing.x thing.y moved

    else
        moved


crossSpecials : Env -> ThinkerId -> Fixed -> Fixed -> World -> World
crossSpecials env id oldx oldy world =
    case popLast world.scratch.spechit of
        Nothing ->
            world

        Just ( lineId, rest ) ->
            let
                popped =
                    World.setScratch (\s -> { s | spechit = rest }) world

                ld =
                    Level.line lineId env.level

                ( side, oldside ) =
                    case World.mobj id popped of
                        Just thing ->
                            ( MapUtil.pointOnLineSide thing.x thing.y ld
                            , MapUtil.pointOnLineSide oldx oldy ld
                            )

                        Nothing ->
                            ( 0, 0 )

                crossed =
                    if side /= oldside && (World.lineState lineId popped).special /= 0 then
                        Env.crossSpecialLine env lineId oldside id popped

                    else
                        popped
            in
            crossSpecials env id oldx oldy crossed


{-| Unlink the thing, give it the checked floor and ceiling and its new position, and
link it in again.
-}
relink : Env -> ThinkerId -> Fixed -> Fixed -> World -> World
relink env id x y world =
    let
        s =
            world.scratch
    in
    world
        |> MapUtil.unsetThingPosition env id
        |> World.updateMobj id (\m -> { m | floorz = s.tmfloorz, ceilingz = s.tmceilingz, x = x, y = y })
        |> MapUtil.setThingPosition env id


{-| P\_ThingHeightClip: fit the thing to the floor and ceiling where it stands (after a
sector moved). Things on the floor move with it; others are pushed down only if the
ceiling comes down on them. False if it doesn't fit.
-}
thingHeightClip : Env -> ThinkerId -> World -> ( Bool, World )
thingHeightClip env id world =
    case World.mobj id world of
        Nothing ->
            ( True, world )

        Just before ->
            let
                onfloor =
                    before.z == before.floorz

                ( _, checked ) =
                    checkPosition env id before.x before.y world
            in
            case World.mobj id checked of
                Nothing ->
                    ( True, checked )

                Just thing ->
                    let
                        s =
                            checked.scratch

                        z =
                            if onfloor then
                                s.tmfloorz

                            else if Int32.add thing.z thing.height > s.tmceilingz then
                                Int32.sub s.tmceilingz thing.height

                            else
                                thing.z
                    in
                    ( Int32.sub s.tmceilingz s.tmfloorz >= thing.height
                    , World.setMobj { thing | floorz = s.tmfloorz, ceilingz = s.tmceilingz, z = z } checked
                    )



-- SLIDE MOVE


{-| P\_SlideMove: the thing's momentum would take it into a wall, so move it up to the
wall and slide it along. Up to three tries, then a stairstep: try the y move alone,
then the x move alone.
-}
slideMove : Env -> ThinkerId -> World -> World
slideMove env id world =
    slideTry env id 0 (World.setScratch (\s -> { s | slidemo = Just id }) world)


slideTry : Env -> ThinkerId -> Int -> World -> World
slideTry env id hitcount0 world =
    let
        hitcount =
            hitcount0 + 1
    in
    case World.mobj id world of
        Nothing ->
            world

        Just mo ->
            if hitcount == 3 then
                -- don't loop forever
                stairstep env id world

            else
                let
                    -- trace along the three leading corners
                    ( leadx, trailx ) =
                        if mo.momx > 0 then
                            ( Int32.add mo.x mo.radius, Int32.sub mo.x mo.radius )

                        else
                            ( Int32.sub mo.x mo.radius, Int32.add mo.x mo.radius )

                    ( leady, traily ) =
                        if mo.momy > 0 then
                            ( Int32.add mo.y mo.radius, Int32.sub mo.y mo.radius )

                        else
                            ( Int32.sub mo.y mo.radius, Int32.add mo.y mo.radius )

                    trace fromX fromY w =
                        MapUtil.pathTraverse env
                            fromX
                            fromY
                            (Int32.add fromX mo.momx)
                            (Int32.add fromY mo.momy)
                            MapUtil.ptAddLines
                            (slideTraverse env)
                            w
                            |> Tuple.second

                    traced =
                        world
                            |> World.setScratch (\s -> { s | bestslidefrac = Fixed.fracunit + 1 })
                            |> trace leadx leady
                            |> trace trailx leady
                            |> trace leadx traily
                in
                if traced.scratch.bestslidefrac == Fixed.fracunit + 1 then
                    -- the move must have hit the middle, so stairstep
                    stairstep env id traced

                else
                    slideAlongWall env id hitcount traced


{-| Move up to the wall (a little short of it), then along it with what is left of the
move.
-}
slideAlongWall : Env -> ThinkerId -> Int -> World -> World
slideAlongWall env id hitcount world =
    let
        -- fudge a bit to make sure it doesn't hit
        best =
            Int32.sub world.scratch.bestslidefrac 0x0800

        fudged =
            World.setScratch (\s -> { s | bestslidefrac = best }) world

        ( reached, approached ) =
            if best > 0 then
                case World.mobj id fudged of
                    Just mo ->
                        tryMove env
                            id
                            (Int32.add mo.x (Fixed.mul mo.momx best))
                            (Int32.add mo.y (Fixed.mul mo.momy best))
                            fudged

                    Nothing ->
                        ( True, fudged )

            else
                ( True, fudged )
    in
    if not reached then
        stairstep env id approached

    else
        let
            -- now continue along the wall, with the remainder of the move
            remainder =
                min Fixed.fracunit (Int32.sub Fixed.fracunit (Int32.add best 0x0800))

            withRemainder =
                World.setScratch (\s -> { s | bestslidefrac = remainder }) approached
        in
        if remainder <= 0 then
            withRemainder

        else
            case World.mobj id withRemainder of
                Nothing ->
                    withRemainder

                Just mo ->
                    let
                        clipped =
                            withRemainder
                                |> World.setScratch
                                    (\s ->
                                        { s
                                            | tmxmove = Fixed.mul mo.momx remainder
                                            , tmymove = Fixed.mul mo.momy remainder
                                        }
                                    )
                                |> hitSlideLine env

                        tmxmove =
                            clipped.scratch.tmxmove

                        tmymove =
                            clipped.scratch.tmymove

                        ( moved, after ) =
                            clipped
                                |> World.updateMobj id (\m -> { m | momx = tmxmove, momy = tmymove })
                                |> tryMove env id (Int32.add mo.x tmxmove) (Int32.add mo.y tmymove)
                    in
                    if moved then
                        after

                    else
                        slideTry env id hitcount after


{-| The stairstep at the end of P\_SlideMove.
-}
stairstep : Env -> ThinkerId -> World -> World
stairstep env id world =
    case World.mobj id world of
        Nothing ->
            world

        Just mo ->
            let
                ( ok, w1 ) =
                    tryMove env id mo.x (Int32.add mo.y mo.momy) world
            in
            if ok then
                w1

            else
                case World.mobj id w1 of
                    Just mo1 ->
                        Tuple.second (tryMove env id (Int32.add mo1.x mo1.momx) mo1.y w1)

                    Nothing ->
                        w1


{-| P\_HitSlideLine: turn `tmxmove`/`tmymove` to run along `bestslideline`.
-}
hitSlideLine : Env -> World -> World
hitSlideLine env world =
    let
        s =
            world.scratch
    in
    case ( s.bestslideline, s.slidemo |> Maybe.andThen (\m -> World.mobj m world) ) of
        ( Just lineId, Just slidemo ) ->
            let
                ld =
                    Level.line lineId env.level
            in
            case ld.slopetype of
                Level.Horizontal ->
                    World.setScratch (\sc -> { sc | tmymove = 0 }) world

                Level.Vertical ->
                    World.setScratch (\sc -> { sc | tmxmove = 0 }) world

                _ ->
                    let
                        side =
                            MapUtil.pointOnLineSide slidemo.x slidemo.y ld

                        lineangle0 =
                            Geometry.pointToAngle2 0 0 ld.dx ld.dy

                        lineangle =
                            if side == 1 then
                                Angle.add lineangle0 Angle.ang180

                            else
                                lineangle0

                        moveangle =
                            Geometry.pointToAngle2 0 0 s.tmxmove s.tmymove

                        deltaangle0 =
                            Angle.sub moveangle lineangle

                        deltaangle =
                            if deltaangle0 > Angle.ang180 then
                                Angle.add deltaangle0 Angle.ang180

                            else
                                deltaangle0

                        movelen =
                            MapUtil.aproxDistance s.tmxmove s.tmymove

                        newlen =
                            Fixed.mul movelen (Angle.cosine deltaangle)
                    in
                    World.setScratch
                        (\sc ->
                            { sc
                                | tmxmove = Fixed.mul newlen (Angle.cosine lineangle)
                                , tmymove = Fixed.mul newlen (Angle.sine lineangle)
                            }
                        )
                        world

        _ ->
            world


{-| PTR\_SlideTraverse: note the nearest line that blocks the sliding thing.
-}
slideTraverse : Env -> Intercept -> World -> ( Bool, World )
slideTraverse env intercept world =
    case ( intercept.target, world.scratch.slidemo |> Maybe.andThen (\m -> World.mobj m world) ) of
        ( InterceptLine lineId, Just slidemo ) ->
            let
                li =
                    Level.line lineId env.level
            in
            if not (Flags.has Flags.mlTwoSided li.flags) then
                if MapUtil.pointOnLineSide slidemo.x slidemo.y li /= 0 then
                    -- don't hit the back side
                    ( True, world )

                else
                    slideBlocked lineId intercept.frac world

            else
                let
                    opened =
                        MapUtil.lineOpening env lineId world

                    s =
                        opened.scratch
                in
                if
                    (s.openrange < slidemo.height)
                        -- doesn't fit
                        || (Int32.sub s.opentop slidemo.z < slidemo.height)
                        -- too high
                        || (Int32.sub s.openbottom slidemo.z > 24 * Fixed.fracunit)
                    -- too big a step up
                then
                    slideBlocked lineId intercept.frac opened

                else
                    -- this line doesn't block movement
                    ( True, opened )

        _ ->
            -- Only lines are traced here; the C source stops the game for a thing.
            ( False, world )


{-| The line blocks the slide: keep it if it is the nearest so far.
-}
slideBlocked : LineId -> Fixed -> World -> ( Bool, World )
slideBlocked lineId frac world =
    if frac < world.scratch.bestslidefrac then
        ( False
        , World.setScratch
            (\s ->
                { s
                    | secondslidefrac = s.bestslidefrac
                    , secondslideline = s.bestslideline
                    , bestslidefrac = frac
                    , bestslideline = Just lineId
                }
            )
            world
        )

    else
        ( False, world )



-- LINE ATTACKS


{-| P\_AimLineAttack t1 angle distance: the slope to aim at; `linetarget` in the scratch.

Traces from t1 along the angle, narrowing the window of slopes (at first plus or minus
100/160, the screen's) at each opening passed, until a shootable thing is in the
window. Its middle (clipped to the window) is the slope.

-}
aimLineAttack : Env -> ThinkerId -> Angle -> Fixed -> World -> ( Fixed, World )
aimLineAttack env id angle distance world =
    case World.mobj id world of
        Nothing ->
            ( 0, world )

        Just t1 ->
            let
                ( x2, y2 ) =
                    attackEnd t1 angle distance

                aiming =
                    World.setScratch
                        (\s ->
                            { s
                                | shootthing = Just id
                                , shootz = shootHeight t1
                                , aimTopslope = 100 * Fixed.fracunit // 160
                                , aimBottomslope = -100 * Fixed.fracunit // 160
                                , attackrange = distance
                                , linetarget = Nothing
                            }
                        )
                        world

                ( _, aimed ) =
                    MapUtil.pathTraverse env
                        t1.x
                        t1.y
                        x2
                        y2
                        (MapUtil.ptAddLines + MapUtil.ptAddThings)
                        (aimTraverse env)
                        aiming
            in
            if aimed.scratch.linetarget /= Nothing then
                ( aimed.scratch.aimslope, aimed )

            else
                ( 0, aimed )


{-| Where an attack of this range along this angle ends.
-}
attackEnd : Mobj -> Angle -> Fixed -> ( Fixed, Fixed )
attackEnd t1 angle distance =
    let
        range =
            Fixed.toInt distance
    in
    ( Int32.add t1.x (Int32.mul range (Angle.cosine angle))
    , Int32.add t1.y (Int32.mul range (Angle.sine angle))
    )


{-| `shootz`: where shots leave from, 8 units above the middle of the shooter.
-}
shootHeight : Mobj -> Fixed
shootHeight t1 =
    Int32.add (Int32.add t1.z (Int32.shiftRight 1 t1.height)) (8 * Fixed.fracunit)


{-| PTR\_AimTraverse.

The C source shares the window of slopes with p\_sight.c (`topslope`, `bottomslope`);
here they are `aimTopslope`/`aimBottomslope`, which nothing else reads.

-}
aimTraverse : Env -> Intercept -> World -> ( Bool, World )
aimTraverse env intercept world =
    case intercept.target of
        InterceptLine lineId ->
            let
                li =
                    Level.line lineId env.level
            in
            if not (Flags.has Flags.mlTwoSided li.flags) then
                -- stop
                ( False, world )

            else
                let
                    opened =
                        MapUtil.lineOpening env lineId world

                    s =
                        opened.scratch
                in
                if s.openbottom >= s.opentop then
                    ( False, opened )

                else
                    let
                        ( front, back ) =
                            lineSectors li opened

                        dist =
                            Fixed.mul s.attackrange intercept.frac

                        bottomslope =
                            if front.floorheight /= back.floorheight then
                                max s.aimBottomslope (Fixed.div (Int32.sub s.openbottom s.shootz) dist)

                            else
                                s.aimBottomslope

                        topslope =
                            if front.ceilingheight /= back.ceilingheight then
                                min s.aimTopslope (Fixed.div (Int32.sub s.opentop s.shootz) dist)

                            else
                                s.aimTopslope
                    in
                    ( topslope > bottomslope
                    , World.setScratch (\sc -> { sc | aimTopslope = topslope, aimBottomslope = bottomslope }) opened
                    )

        InterceptThing thId ->
            let
                s =
                    world.scratch
            in
            case World.mobj thId world of
                Nothing ->
                    ( True, world )

                Just th ->
                    if Just thId == s.shootthing then
                        -- can't shoot self
                        ( True, world )

                    else if not (Flags.has Flags.mfShootable th.flags) then
                        -- corpse or something
                        ( True, world )

                    else
                        let
                            dist =
                                Fixed.mul s.attackrange intercept.frac

                            thingtopslope =
                                Fixed.div (Int32.sub (Int32.add th.z th.height) s.shootz) dist

                            thingbottomslope =
                                Fixed.div (Int32.sub th.z s.shootz) dist
                        in
                        if thingtopslope < s.aimBottomslope then
                            -- shot over the thing
                            ( True, world )

                        else if thingbottomslope > s.aimTopslope then
                            -- shot under the thing
                            ( True, world )

                        else
                            -- this thing can be hit
                            let
                                top =
                                    min thingtopslope s.aimTopslope

                                bottom =
                                    max thingbottomslope s.aimBottomslope
                            in
                            ( False
                            , World.setScratch
                                (\sc -> { sc | aimslope = Int32.add top bottom // 2, linetarget = Just thId })
                                world
                            )


{-| P\_LineAttack t1 angle distance slope damage: fire a hitscan shot. With damage 0 it
is only a test.
-}
lineAttack : Env -> ThinkerId -> Angle -> Fixed -> Fixed -> Int -> World -> World
lineAttack env id angle distance slope damage world =
    case World.mobj id world of
        Nothing ->
            world

        Just t1 ->
            let
                ( x2, y2 ) =
                    attackEnd t1 angle distance

                shooting =
                    World.setScratch
                        (\s ->
                            { s
                                | shootthing = Just id
                                , laDamage = damage
                                , shootz = shootHeight t1
                                , attackrange = distance
                                , aimslope = slope
                            }
                        )
                        world
            in
            MapUtil.pathTraverse env
                t1.x
                t1.y
                x2
                y2
                (MapUtil.ptAddLines + MapUtil.ptAddThings)
                (shootTraverse env)
                shooting
                |> Tuple.second


{-| PTR\_ShootTraverse: the shot hits a wall (a puff, unless it is the sky), passes
through an opening, or hits a thing (blood or a puff, and damage).
-}
shootTraverse : Env -> Intercept -> World -> ( Bool, World )
shootTraverse env intercept world =
    case intercept.target of
        InterceptLine lineId ->
            let
                li =
                    Level.line lineId env.level

                shot =
                    case world.scratch.shootthing of
                        Just shooter ->
                            if (World.lineState lineId world).special /= 0 then
                                Env.shootSpecialLine env shooter lineId world

                            else
                                world

                        Nothing ->
                            world
            in
            if not (Flags.has Flags.mlTwoSided li.flags) then
                hitLine env li intercept.frac shot

            else
                let
                    opened =
                        MapUtil.lineOpening env lineId shot

                    s =
                        opened.scratch

                    ( front, back ) =
                        lineSectors li opened

                    dist =
                        Fixed.mul s.attackrange intercept.frac

                    belowFloor =
                        (front.floorheight /= back.floorheight)
                            && (Fixed.div (Int32.sub s.openbottom s.shootz) dist > s.aimslope)

                    aboveCeiling =
                        (front.ceilingheight /= back.ceilingheight)
                            && (Fixed.div (Int32.sub s.opentop s.shootz) dist < s.aimslope)
                in
                if belowFloor || aboveCeiling then
                    hitLine env li intercept.frac opened

                else
                    -- the shot continues
                    ( True, opened )

        InterceptThing thId ->
            shootThing env thId intercept.frac world


{-| The shot hits a line: a puff a little short of it, unless it went into the sky.
-}
hitLine : Env -> Level.Line -> Fixed -> World -> ( Bool, World )
hitLine env li interceptFrac world =
    let
        s =
            world.scratch

        -- position a bit closer
        frac =
            Int32.sub interceptFrac (Fixed.div (4 * Fixed.fracunit) s.attackrange)

        ( x, y, z ) =
            shotPoint s frac

        front =
            World.sector li.frontsector world

        backIsSky =
            case li.backsector of
                Just b ->
                    (World.sector b world).ceilingpic == env.data.skyFlatNum

                Nothing ->
                    False
    in
    if front.ceilingpic == env.data.skyFlatNum && z > front.ceilingheight then
        -- don't shoot the sky
        ( False, world )

    else if front.ceilingpic == env.data.skyFlatNum && backIsSky then
        -- it's a sky hack wall
        ( False, world )

    else
        ( False, PlayMobj.spawnPuff env x y z world )


{-| The shot reaches a thing: it hits it if the aim slope is within it.
-}
shootThing : Env -> ThinkerId -> Fixed -> World -> ( Bool, World )
shootThing env thId interceptFrac world =
    let
        s =
            world.scratch
    in
    case World.mobj thId world of
        Nothing ->
            ( True, world )

        Just th ->
            if Just thId == s.shootthing then
                -- can't shoot self
                ( True, world )

            else if not (Flags.has Flags.mfShootable th.flags) then
                -- corpse or something
                ( True, world )

            else
                let
                    dist =
                        Fixed.mul s.attackrange interceptFrac

                    thingtopslope =
                        Fixed.div (Int32.sub (Int32.add th.z th.height) s.shootz) dist

                    thingbottomslope =
                        Fixed.div (Int32.sub th.z s.shootz) dist
                in
                if thingtopslope < s.aimslope then
                    -- shot over the thing
                    ( True, world )

                else if thingbottomslope > s.aimslope then
                    -- shot under the thing
                    ( True, world )

                else
                    let
                        -- position a bit closer
                        frac =
                            Int32.sub interceptFrac (Fixed.div (10 * Fixed.fracunit) s.attackrange)

                        ( x, y, z ) =
                            shotPoint s frac

                        marked =
                            if Flags.has Flags.mfNoBlood th.flags then
                                PlayMobj.spawnPuff env x y z world

                            else
                                PlayMobj.spawnBlood env x y z s.laDamage world

                        hurt =
                            if s.laDamage /= 0 then
                                Inter.damageMobj env thId s.shootthing s.shootthing s.laDamage marked

                            else
                                marked
                    in
                    -- don't go any farther
                    ( False, hurt )


{-| The point `frac` of the way along the trace, at the aimed height.
-}
shotPoint : World.Scratch -> Fixed -> ( Fixed, Fixed, Fixed )
shotPoint s frac =
    ( Int32.add s.trace.x (Fixed.mul s.trace.dx frac)
    , Int32.add s.trace.y (Fixed.mul s.trace.dy frac)
    , Int32.add s.shootz (Fixed.mul s.aimslope (Fixed.mul frac s.attackrange))
    )


{-| A two-sided line's front and back sectors. (A line marked two-sided without a back
sector would make the C source read through a null pointer; the front stands in.)
-}
lineSectors : Level.Line -> World -> ( World.SectorState, World.SectorState )
lineSectors li world =
    ( World.sector li.frontsector world
    , World.sector (Maybe.withDefault li.frontsector li.backsector) world
    )



-- USE LINES


{-| P\_UseLines player: press use on the first special line in front of the player,
within USERANGE. A wall (no opening) in the way stops it with an "oof".
-}
useLines : Env -> Int -> World -> World
useLines env playerNum world =
    case World.playerMobj playerNum world of
        Nothing ->
            world

        Just mo ->
            let
                ( x2, y2 ) =
                    attackEnd mo mo.angle Defs.useRange
            in
            world
                |> World.setScratch (\s -> { s | usething = Just mo.id })
                |> MapUtil.pathTraverse env mo.x mo.y x2 y2 MapUtil.ptAddLines (useTraverse env)
                |> Tuple.second


{-| PTR\_UseTraverse.
-}
useTraverse : Env -> Intercept -> World -> ( Bool, World )
useTraverse env intercept world =
    case ( intercept.target, world.scratch.usething ) of
        ( InterceptLine lineId, Just usething ) ->
            if (World.lineState lineId world).special == 0 then
                let
                    opened =
                        MapUtil.lineOpening env lineId world
                in
                if opened.scratch.openrange <= 0 then
                    -- can't use through a wall
                    ( False, World.startSound (Sound.FromMobj usething) Sfx.Noway opened )

                else
                    -- not a special line, but keep checking
                    ( True, opened )

            else
                let
                    side =
                        case World.mobj usething world of
                            Just m ->
                                if MapUtil.pointOnLineSide m.x m.y (Level.line lineId env.level) == 1 then
                                    1

                                else
                                    0

                            Nothing ->
                                0
                in
                -- can't use more than one special line in a row
                ( False, Tuple.second (Env.useSpecialLine env usething lineId side world) )

        _ ->
            ( False, world )



-- RADIUS ATTACK


{-| P\_RadiusAttack spot source damage: an explosion at `spot`, caused by `source`.
Everything shootable within `damage` units (by the longer of the x and y distances,
less its radius) that can see the spot takes `damage` less that distance, except the
Cyberdemon and the Spider Mastermind.
-}
radiusAttack : Env -> ThinkerId -> Maybe ThinkerId -> Int -> World -> World
radiusAttack env spotId source damage world =
    case World.mobj spotId world of
        Nothing ->
            world

        Just spot ->
            let
                bm =
                    env.level.blockmap

                -- (damage + MAXRADIUS) << FRACBITS: MAXRADIUS is already fixed-point,
                -- so it overflows out, as in the C source.
                dist =
                    Int32.shiftLeft Fixed.fracbits (Int32.add damage Defs.maxRadius)

                cell v org =
                    Int32.shiftRight Defs.mapBlockShift (Int32.sub v org)

                yh =
                    cell (Int32.add spot.y dist) bm.orgy

                yl =
                    cell (Int32.sub spot.y dist) bm.orgy

                xh =
                    cell (Int32.add spot.x dist) bm.orgx

                xl =
                    cell (Int32.sub spot.x dist) bm.orgx

                bombing =
                    World.setScratch
                        (\s -> { s | bombspot = Just spotId, bombsource = source, bombdamage = damage })
                        world

                cells =
                    List.range yl yh |> List.concatMap (\y -> List.range xl xh |> List.map (\x -> ( x, y )))
            in
            eachCell cells (\x y -> MapUtil.blockThingsIterator env x y (radiusAttackThing env)) bombing
                |> Tuple.second


{-| PIT\_RadiusAttack.
-}
radiusAttackThing : Env -> ThinkerId -> World -> ( Bool, World )
radiusAttackThing env thingId world =
    let
        s =
            world.scratch
    in
    case ( World.mobj thingId world, s.bombspot |> Maybe.andThen (\b -> World.mobj b world) ) of
        ( Just thing, Just bombspot ) ->
            if not (Flags.has Flags.mfShootable thing.flags) then
                ( True, world )

            else if thing.type_ == MobjType.Cyborg || thing.type_ == MobjType.Spider then
                -- the bosses take no damage from concussion
                ( True, world )

            else
                let
                    dx =
                        Int32.abs (Int32.sub thing.x bombspot.x)

                    dy =
                        Int32.abs (Int32.sub thing.y bombspot.y)

                    dist =
                        max 0 (Int32.shiftRight Fixed.fracbits (Int32.sub (max dx dy) thing.radius))
                in
                if dist >= s.bombdamage then
                    -- out of range
                    ( True, world )

                else
                    let
                        ( seen, looked ) =
                            Sight.checkSight env thingId bombspot.id world
                    in
                    if seen then
                        -- must be in direct path
                        ( True, Inter.damageMobj env thingId (Just bombspot.id) s.bombsource (s.bombdamage - dist) looked )

                    else
                        ( True, looked )

        _ ->
            ( True, world )



-- SECTOR HEIGHT CHANGING


{-| P\_ChangeSector sector crunch: fit things to the sector's new heights; True if something didn't fit.

Every thing in the blockmap cells the sector touches is fitted again. Corpses that
don't fit are crushed to gibs, dropped items that don't fit vanish, and anything else
that can be hurt and doesn't fit is crushed (10 damage every fourth tic, spraying
blood) when `crunch` is set.

-}
changeSector : Env -> SectorId -> Bool -> World -> ( Bool, World )
changeSector env sectorId crunch world =
    let
        box =
            (Level.sector sectorId env.level).blockbox

        cells =
            List.range box.left box.right
                |> List.concatMap (\x -> List.range box.bottom box.top |> List.map (\y -> ( x, y )))

        ( _, changed ) =
            world
                |> World.setScratch (\s -> { s | nofit = False, crushchange = crunch })
                |> eachCell cells (\x y -> MapUtil.blockThingsIterator env x y (changeSectorThing env))
    in
    ( changed.scratch.nofit, changed )


{-| PIT\_ChangeSector.
-}
changeSectorThing : Env -> ThinkerId -> World -> ( Bool, World )
changeSectorThing env thingId world =
    let
        ( fits, clipped ) =
            thingHeightClip env thingId world
    in
    if fits then
        ( True, clipped )

    else
        case World.mobj thingId clipped of
            Nothing ->
                ( True, clipped )

            Just thing ->
                if thing.health <= 0 then
                    -- crunch bodies to giblets
                    let
                        ( _, gibbed ) =
                            Env.setMobjState env thingId State.Gibs clipped
                    in
                    ( True
                    , World.updateMobj thingId
                        (\m -> { m | flags = Flags.clear Flags.mfSolid m.flags, height = 0, radius = 0 })
                        gibbed
                    )

                else if Flags.has Flags.mfDropped thing.flags then
                    -- crunch dropped items
                    ( True, PlayMobj.removeMobj env thingId clipped )

                else if not (Flags.has Flags.mfShootable thing.flags) then
                    -- assume it is bloody gibs or something
                    ( True, clipped )

                else
                    let
                        unfit =
                            World.setScratch (\s -> { s | nofit = True }) clipped
                    in
                    if unfit.scratch.crushchange && Bitwise.and unfit.leveltime 3 == 0 then
                        ( True, crush env thingId unfit )

                    else
                        ( True, unfit )


{-| Crushing damage, and blood sprayed in a random direction.
-}
crush : Env -> ThinkerId -> World -> World
crush env thingId world =
    let
        hurt =
            Inter.damageMobj env thingId Nothing Nothing 10 world
    in
    case World.mobj thingId hurt of
        Nothing ->
            hurt

        Just thing ->
            let
                ( mo, spawned ) =
                    PlayMobj.spawnMobj env thing.x thing.y (Int32.add thing.z (thing.height // 2)) MobjType.Blood hurt

                ( r1, w1 ) =
                    World.pRandom spawned

                ( r2, w2 ) =
                    World.pRandom w1

                ( r3, w3 ) =
                    World.pRandom w2

                ( r4, w4 ) =
                    World.pRandom w3
            in
            World.updateMobj mo
                (\m -> { m | momx = Int32.shiftLeft 12 (r1 - r2), momy = Int32.shiftLeft 12 (r3 - r4) })
                w4



-- HELPERS


{-| `tmthing`.
-}
tmThing : World -> Maybe Mobj
tmThing world =
    world.scratch.tmthing |> Maybe.andThen (\id -> World.mobj id world)


{-| Call `f` on each cell in turn until one gives False.
-}
eachCell : List ( Int, Int ) -> (Int -> Int -> World -> ( Bool, World )) -> World -> ( Bool, World )
eachCell cells f world =
    case cells of
        [] ->
            ( True, world )

        ( x, y ) :: rest ->
            let
                ( ok, next ) =
                    f x y world
            in
            if ok then
                eachCell rest f next

            else
                ( False, next )


{-| The cells to look for things touching the box in (x outer, y inner), the box grown
by MAXRADIUS.
-}
thingCells : Env -> Level.Box -> List ( Int, Int )
thingCells env bbox =
    boxCells env bbox Defs.maxRadius


{-| The cells to look for lines touching the box in.
-}
lineCells : Env -> Level.Box -> List ( Int, Int )
lineCells env bbox =
    boxCells env bbox 0


boxCells : Env -> Level.Box -> Fixed -> List ( Int, Int )
boxCells env bbox margin =
    let
        bm =
            env.level.blockmap

        cell v org extra =
            Int32.shiftRight Defs.mapBlockShift (Int32.add (Int32.sub v org) extra)

        xl =
            cell bbox.left bm.orgx (negate margin)

        xh =
            cell bbox.right bm.orgx margin

        yl =
            cell bbox.bottom bm.orgy (negate margin)

        yh =
            cell bbox.top bm.orgy margin
    in
    List.range xl xh |> List.concatMap (\x -> List.range yl yh |> List.map (\y -> ( x, y )))


{-| The last element, and the list without it.
-}
popLast : List a -> Maybe ( a, List a )
popLast list =
    case List.reverse list of
        [] ->
            Nothing

        x :: rest ->
            Just ( x, List.reverse rest )
