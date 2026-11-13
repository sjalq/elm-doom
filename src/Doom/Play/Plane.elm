module Doom.Play.Plane exposing (PlaneResult(..), Crush(..), movePlane, movePlaneWith)

{-| Moving a sector's floor or ceiling (T\_MovePlane, p\_floor.c), which every moving
floor, ceiling, door and lift uses.

@docs PlaneResult, Crush, movePlane, movePlaneWith

-}

import Doom.Fixed exposing (Fixed)
import Doom.Int32 as Int32
import Doom.Level exposing (SectorId)
import Doom.Play.Env exposing (Env)
import Doom.Play.Map as Map
import Doom.World as World exposing (World)


{-| result\_e.
-}
type PlaneResult
    = PlaneOk
    | PlaneCrushed
    | PlanePastDest


{-| Which plane moves.
-}
type Plane
    = Floor
    | Ceiling


{-| T\_MovePlane sector speed dest crush floorOrCeiling direction (floorOrCeiling 0 floor, 1 ceiling).

Moves the plane `speed` towards `dest` (direction -1 down, 1 up), stopping at `dest`
(`PlanePastDest`). Things that no longer fit are refitted (`Map.changeSector`); if
something doesn't fit, the plane goes back where it was, except that a crushing floor
going up or ceiling going down stays put and reports `PlaneCrushed` (the things take
damage instead), and a ceiling going up never goes back. Reaching `dest` is reported as
`PlanePastDest` even when the plane had to go back.

-}
movePlane : Env -> SectorId -> Fixed -> Fixed -> Bool -> Int -> Int -> World -> ( PlaneResult, World )
movePlane env sectorId speed dest crush =
    movePlaneWith env
        sectorId
        speed
        dest
        (if crush then
            Crushes

         else
            NoCrush
        )


{-| The `crush` argument of T\_MovePlane, a C boolean that is not always 0 or 1.

`StairCrush` is a stair step's: EV\_BuildStairs never sets its `crush`, and in the DOS
executable that field holds neither 0 nor 1 (chocolate-doom p\_floor.c,
STAIRS\_UNINITIALIZED\_CRUSH\_FIELD\_VALUE = 10). It is true for P\_ChangeSector (things in
the way take crush damage) but not `== true`, so the step still goes back.

-}
type Crush
    = NoCrush
    | Crushes
    | StairCrush


{-| movePlane with the crush argument as a `Crush`.
-}
movePlaneWith : Env -> SectorId -> Fixed -> Fixed -> Crush -> Int -> Int -> World -> ( PlaneResult, World )
movePlaneWith env sectorId speed dest crushArg floorOrCeiling direction world =
    let
        -- crushchange: any nonzero value
        crunch =
            crushArg /= NoCrush

        -- `crush == true`
        crush =
            crushArg == Crushes

        plane =
            if floorOrCeiling == 0 then
                Floor

            else
                Ceiling

        lastpos =
            height plane (World.sector sectorId world)

        moveTo h w =
            World.updateSector sectorId (setHeight plane h) w

        -- put the plane at h and refit things; True if something doesn't fit
        changeTo h w =
            Map.changeSector env sectorId crunch (moveTo h w)

        goBack w =
            Tuple.second (changeTo lastpos w)
    in
    if floorOrCeiling /= 0 && floorOrCeiling /= 1 then
        ( PlaneOk, world )

    else if direction == -1 then
        let
            lowered =
                Int32.sub lastpos speed
        in
        if lowered < dest then
            pastDest (changeTo dest world) goBack

        else
            let
                ( nofit, moved ) =
                    changeTo lowered world
            in
            if not nofit then
                ( PlaneOk, moved )

            else if plane == Ceiling && crush then
                -- could get crushed
                ( PlaneCrushed, moved )

            else
                ( PlaneCrushed, goBack moved )

    else if direction == 1 then
        let
            raised =
                Int32.add lastpos speed
        in
        if raised > dest then
            pastDest (changeTo dest world) goBack

        else
            let
                ( nofit, moved ) =
                    changeTo raised world
            in
            if not nofit then
                ( PlaneOk, moved )

            else if plane == Ceiling then
                -- the C source's check here is compiled out
                ( PlaneOk, moved )

            else if crush then
                -- could get crushed
                ( PlaneCrushed, moved )

            else
                ( PlaneCrushed, goBack moved )

    else
        ( PlaneOk, world )


{-| The plane reached its destination: if something doesn't fit there, go back.
-}
pastDest : ( Bool, World ) -> (World -> World) -> ( PlaneResult, World )
pastDest ( nofit, moved ) goBack =
    if nofit then
        ( PlanePastDest, goBack moved )

    else
        ( PlanePastDest, moved )


height : Plane -> World.SectorState -> Fixed
height plane s =
    case plane of
        Floor ->
            s.floorheight

        Ceiling ->
            s.ceilingheight


setHeight : Plane -> Fixed -> World.SectorState -> World.SectorState
setHeight plane h s =
    case plane of
        Floor ->
            { s | floorheight = h }

        Ceiling ->
            { s | ceilingheight = h }
