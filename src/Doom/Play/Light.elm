module Doom.Play.Light exposing (thinkFireFlicker, thinkLightFlash, thinkStrobe, thinkGlow, spawnFireFlicker, spawnLightFlash, spawnStrobeFlash, spawnGlowingLight, startLightStrobing, turnTagLightsOff, lightTurnOn)

{-| Flickering, flashing, strobing and glowing lights, and switching lights
(p\_lights.c).

The light thinkers are spawned at level start for sector specials, which they then
clear: during play the sector is nothing special.

@docs thinkFireFlicker, thinkLightFlash, thinkStrobe, thinkGlow, spawnFireFlicker, spawnLightFlash, spawnStrobeFlash, spawnGlowingLight, startLightStrobing, turnTagLightsOff, lightTurnOn

-}

import Array
import Bitwise
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.SpecUtil as SpecUtil
import Doom.Thinker as Thinker exposing (FireFlicker, Glow, LightFlash, Strobe, Thinker(..))
import Doom.World as World exposing (World)


setLight : SectorId -> Int -> World -> World
setLight sec level =
    World.updateSector sec (\s -> { s | lightlevel = level })


lightOf : SectorId -> World -> Int
lightOf sec world =
    (World.sector sec world).lightlevel


clearSpecial : SectorId -> World -> World
clearSpecial sec =
    World.updateSector sec (\s -> { s | special = 0 })


{-| T\_FireFlicker: every 4 tics, a random dip below the brightest.
-}
thinkFireFlicker : Env -> ThinkerId -> FireFlicker -> World -> World
thinkFireFlicker _ id flick world =
    if flick.count - 1 /= 0 then
        World.setThinker id (FireFlickerThinker { flick | count = flick.count - 1 }) world

    else
        let
            ( r, rolled ) =
                World.pRandom world

            amount =
                Bitwise.and r 3 * 16

            level =
                if lightOf flick.sector rolled - amount < flick.minlight then
                    flick.minlight

                else
                    flick.maxlight - amount
        in
        rolled
            |> setLight flick.sector level
            |> World.setThinker id (FireFlickerThinker { flick | count = 4 })


{-| P\_SpawnFireFlicker: sector special 17.
-}
spawnFireFlicker : Env -> SectorId -> World -> World
spawnFireFlicker env sec world =
    let
        cleared =
            clearSpecial sec world

        light =
            lightOf sec cleared
    in
    World.addThinker
        (FireFlickerThinker
            { sector = sec
            , count = 4
            , maxlight = light
            , minlight = SpecUtil.findMinSurroundingLight env sec light cleared + 16
            }
        )
        cleared
        |> Tuple.second


{-| T\_LightFlash: broken lights, switching between bright and dark at random
intervals.
-}
thinkLightFlash : Env -> ThinkerId -> LightFlash -> World -> World
thinkLightFlash _ id flash world =
    if flash.count - 1 /= 0 then
        World.setThinker id (LightFlashThinker { flash | count = flash.count - 1 }) world

    else
        let
            ( r, rolled ) =
                World.pRandom world

            ( level, count ) =
                if lightOf flash.sector world == flash.maxlight then
                    ( flash.minlight, Bitwise.and r flash.mintime + 1 )

                else
                    ( flash.maxlight, Bitwise.and r flash.maxtime + 1 )
        in
        rolled
            |> setLight flash.sector level
            |> World.setThinker id (LightFlashThinker { flash | count = count })


{-| P\_SpawnLightFlash: sector special 1.
-}
spawnLightFlash : Env -> SectorId -> World -> World
spawnLightFlash env sec world =
    let
        cleared =
            clearSpecial sec world

        light =
            lightOf sec cleared

        ( r, rolled ) =
            World.pRandom cleared
    in
    World.addThinker
        (LightFlashThinker
            { sector = sec
            , count = Bitwise.and r 64 + 1
            , maxlight = light
            , minlight = SpecUtil.findMinSurroundingLight env sec light cleared
            , maxtime = 64
            , mintime = 7
            }
        )
        rolled
        |> Tuple.second


{-| T\_StrobeFlash: bright for `brighttime` tics, dark for `darktime`.
-}
thinkStrobe : Env -> ThinkerId -> Strobe -> World -> World
thinkStrobe _ id flash world =
    if flash.count - 1 /= 0 then
        World.setThinker id (StrobeThinker { flash | count = flash.count - 1 }) world

    else
        let
            ( level, count ) =
                if lightOf flash.sector world == flash.minlight then
                    ( flash.maxlight, flash.brighttime )

                else
                    ( flash.minlight, flash.darktime )
        in
        world
            |> setLight flash.sector level
            |> World.setThinker id (StrobeThinker { flash | count = count })


{-| P\_SpawnStrobeFlash sector fastOrSlow inSync: sector specials 2, 3, 4, 12, 13.
`fastOrSlow` is the dark time; strobes in sync all start on the same tic.
-}
spawnStrobeFlash : Env -> SectorId -> Int -> Bool -> World -> World
spawnStrobeFlash env sec fastOrSlow inSync world =
    let
        light =
            lightOf sec world

        found =
            SpecUtil.findMinSurroundingLight env sec light world

        cleared =
            clearSpecial sec world

        ( count, rolled ) =
            if inSync then
                ( 1, cleared )

            else
                World.pRandom cleared |> Tuple.mapFirst (\r -> Bitwise.and r 7 + 1)
    in
    World.addThinker
        (StrobeThinker
            { sector = sec
            , count = count
            , minlight =
                if found == light then
                    0

                else
                    found
            , maxlight = light
            , darktime = fastOrSlow
            , brighttime = Thinker.strobeBright
            }
        )
        rolled
        |> Tuple.second


{-| T\_Glow: the light fades down to `minlight` and back up to `maxlight`, 8 a tic.
-}
thinkGlow : Env -> ThinkerId -> Glow -> World -> World
thinkGlow _ id g world =
    let
        light =
            lightOf g.sector world
    in
    if g.direction == -1 then
        let
            down =
                light - Thinker.glowSpeed
        in
        if down <= g.minlight then
            world
                |> setLight g.sector (down + Thinker.glowSpeed)
                |> World.setThinker id (GlowThinker { g | direction = 1 })

        else
            setLight g.sector down world

    else if g.direction == 1 then
        let
            up =
                light + Thinker.glowSpeed
        in
        if up >= g.maxlight then
            world
                |> setLight g.sector (up - Thinker.glowSpeed)
                |> World.setThinker id (GlowThinker { g | direction = -1 })

        else
            setLight g.sector up world

    else
        world


{-| P\_SpawnGlowingLight: sector special 8.
-}
spawnGlowingLight : Env -> SectorId -> World -> World
spawnGlowingLight env sec world =
    let
        light =
            lightOf sec world
    in
    World.addThinker
        (GlowThinker
            { sector = sec
            , minlight = SpecUtil.findMinSurroundingLight env sec light world
            , maxlight = light
            , direction = -1
            }
        )
        world
        |> Tuple.second
        |> clearSpecial sec


{-| EV\_StartLightStrobing: slow strobes in the sectors with the line's tag that have
nothing moving.
-}
startLightStrobing : Env -> LineId -> World -> World
startLightStrobing env lineId world =
    SpecUtil.foldTaggedSectors (Level.line lineId env.level).tag
        (\sec ( rtn, w ) ->
            if (World.sector sec w).specialdata /= Nothing then
                ( rtn, w )

            else
                ( rtn, spawnStrobeFlash env sec Thinker.slowDark False w )
        )
        world
        |> Tuple.second


{-| The sector numbers whose tag is the line's, in order.
-}
taggedSectors : Env -> LineId -> World -> List SectorId
taggedSectors env lineId world =
    let
        tag =
            (Level.line lineId env.level).tag
    in
    Array.toIndexedList world.sectors
        |> List.filterMap
            (\( i, s ) ->
                if s.tag == tag then
                    Just i

                else
                    Nothing
            )


{-| EV\_TurnTagLightsOff: each sector with the line's tag goes as dark as its darkest
neighbour (or stays, if it is darker).
-}
turnTagLightsOff : Env -> LineId -> World -> World
turnTagLightsOff env lineId world =
    List.foldl
        (\sec w -> setLight sec (SpecUtil.findMinSurroundingLight env sec (lightOf sec w) w) w)
        world
        (taggedSectors env lineId world)


{-| EV\_LightTurnOn line bright: the sectors with the line's tag go to light level
`bright`. 0 means as bright as the brightest neighbour of the first such sector:
as in the C source, the level found there is then used for the rest.
-}
lightTurnOn : Env -> LineId -> Int -> World -> World
lightTurnOn env lineId bright world =
    List.foldl
        (\sec ( b, w ) ->
            let
                level =
                    if b == 0 then
                        List.foldl
                            (\l acc ->
                                case SpecUtil.getNextSector env.level l sec of
                                    Just other ->
                                        max acc (lightOf other w)

                                    Nothing ->
                                        acc
                            )
                            b
                            (SpecUtil.sectorLines env.level sec)

                    else
                        b
            in
            ( level, setLight sec level w )
        )
        ( bright, world )
        (taggedSectors env lineId world)
        |> Tuple.second
