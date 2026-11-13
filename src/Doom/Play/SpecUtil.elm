module Doom.Play.SpecUtil exposing
    ( getSide, getSector, twoSided, getNextSector
    , findLowestFloorSurrounding, findHighestFloorSurrounding, findNextHighestFloor
    , findLowestCeilingSurrounding, findHighestCeilingSurrounding
    , findSectorFromLineTag, findSectorFromTag, foldTaggedSectors, findMinSurroundingLight
    , sectorLines, sectorSound
    )

{-| The sector and line lookups p\_spec.c gives the moving floors, doors, lifts and
lights.

A sector's lines are numbered as `sector->lines[]` has them (`Level.Sector.lines`),
which is the order P\_GroupLines found them in; the searches below go through them in
that order, as the C source does.

@docs getSide, getSector, twoSided, getNextSector
@docs findLowestFloorSurrounding, findHighestFloorSurrounding, findNextHighestFloor
@docs findLowestCeilingSurrounding, findHighestCeilingSurrounding
@docs findSectorFromLineTag, findSectorFromTag, foldTaggedSectors, findMinSurroundingLight
@docs sectorLines, sectorSound

-}

import Array
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Info.Sfx exposing (Sfx)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Play.Env exposing (Env)
import Doom.Sound as Sound
import Doom.World as World exposing (World)


{-| The lines around a sector, in `sector->lines[]` order.
-}
sectorLines : Level -> SectorId -> List LineId
sectorLines level sec =
    Array.toList (Level.sector sec level).lines


lineOfSector : Level -> SectorId -> Int -> Level.Line
lineOfSector level sec i =
    let
        lineId =
            Array.get i (Level.sector sec level).lines |> Maybe.withDefault 0
    in
    Level.line lineId level


{-| getSide currentSector line side: side `side` (0 front, 1 back) of the sector's
`line`th line. A one-sided line has no back side (the C source would index
`sides[-1]`).
-}
getSide : Level -> SectorId -> Int -> Int -> Maybe SideId
getSide level sec i side =
    let
        l =
            lineOfSector level sec i
    in
    if side == 0 then
        Just l.frontSide

    else
        l.backSide


{-| getSector currentSector line side: the sector side `side` of the sector's `line`th
line faces.
-}
getSector : Level -> SectorId -> Int -> Int -> Maybe SectorId
getSector level sec i side =
    getSide level sec i side |> Maybe.map (\s -> (Level.side s level).sector)


{-| twoSided sector line: whether the sector's `line`th line has ML\_TWOSIDED.
-}
twoSided : Level -> SectorId -> Int -> Bool
twoSided level sec i =
    Flags.has Flags.mlTwoSided (lineOfSector level sec i).flags


{-| getNextSector line sec: the sector on the line's other side from `sec`, Nothing if
the line isn't two-sided.
-}
getNextSector : Level -> LineId -> SectorId -> Maybe SectorId
getNextSector level lineId sec =
    let
        l =
            Level.line lineId level
    in
    if not (Flags.has Flags.mlTwoSided l.flags) then
        Nothing

    else if l.frontsector == sec then
        l.backsector

    else
        Just l.frontsector


{-| The sectors next to `sec`, one per two-sided line, in line order (a neighbour
sharing several lines comes up several times, as in the C loops).
-}
neighbours : Env -> SectorId -> List SectorId
neighbours env sec =
    List.filterMap (\l -> getNextSector env.level l sec) (sectorLines env.level sec)


{-| P\_FindLowestFloorSurrounding: the lowest floor around the sector, or its own if
that is lower.
-}
findLowestFloorSurrounding : Env -> SectorId -> World -> Fixed
findLowestFloorSurrounding env sec world =
    List.foldl (\other floor -> min floor (World.sector other world).floorheight)
        (World.sector sec world).floorheight
        (neighbours env sec)


{-| P\_FindHighestFloorSurrounding: the highest floor around the sector, at least
-500.
-}
findHighestFloorSurrounding : Env -> SectorId -> World -> Fixed
findHighestFloorSurrounding env sec world =
    List.foldl (\other floor -> max floor (World.sector other world).floorheight)
        (-500 * Fixed.fracunit)
        (neighbours env sec)


{-| P\_FindNextHighestFloor sec currentheight: the lowest floor around the sector that
is above `currentheight`, or `currentheight` if there is none.

As the DOS executable does (chocolate-doom p\_spec.c:P\_FindNextHighestFloor), not
linuxdoom-1.10, which stops looking after 20: `heightlist[20]` has no limit check, so
the 21st floor found goes past the array harmlessly and the 22nd lands on `height`
itself, the floor later ones are compared with, as well as in the list. (A 23rd crashes
the DOS game; here the list just grows.)

-}
findNextHighestFloor : Env -> SectorId -> Fixed -> World -> Fixed
findNextHighestFloor env sec currentheight world =
    let
        collect others height found count =
            case others of
                [] ->
                    found

                other :: rest ->
                    let
                        h =
                            (World.sector other world).floorheight
                    in
                    if h > height then
                        collect rest
                            (if count == 21 then
                                h

                             else
                                height
                            )
                            (h :: found)
                            (count + 1)

                    else
                        collect rest height found count
    in
    collect (neighbours env sec) currentheight [] 0
        |> List.minimum
        |> Maybe.withDefault currentheight


{-| P\_FindLowestCeilingSurrounding: MAXINT if the sector has no neighbours.
-}
findLowestCeilingSurrounding : Env -> SectorId -> World -> Fixed
findLowestCeilingSurrounding env sec world =
    List.foldl (\other height -> min height (World.sector other world).ceilingheight)
        Fixed.maxInt
        (neighbours env sec)


{-| P\_FindHighestCeilingSurrounding: at least 0.
-}
findHighestCeilingSurrounding : Env -> SectorId -> World -> Fixed
findHighestCeilingSurrounding env sec world =
    List.foldl (\other height -> max height (World.sector other world).ceilingheight)
        0
        (neighbours env sec)


{-| P\_FindSectorFromLineTag line start: the next sector after `start` with the line's
tag, -1 if none.
-}
findSectorFromLineTag : Env -> LineId -> Int -> World -> Int
findSectorFromLineTag env lineId start world =
    findSectorFromTag (Level.line lineId env.level).tag start world


{-| The same, given the tag itself.
-}
findSectorFromTag : Int -> Int -> World -> Int
findSectorFromTag tag start world =
    let
        search i =
            case Array.get i world.sectors of
                Nothing ->
                    -1

                Just s ->
                    if s.tag == tag then
                        i

                    else
                        search (i + 1)
    in
    search (start + 1)


{-| The C loop `while ((secnum = P_FindSectorFromLineTag(line, secnum)) >= 0)` that
the EV\_ functions run: the body is given each sector with the tag in turn and the
`rtn` so far, and gives the new `rtn`. Starts with `rtn` False.
-}
foldTaggedSectors : Int -> (SectorId -> ( Bool, World ) -> ( Bool, World )) -> World -> ( Bool, World )
foldTaggedSectors tag body world =
    let
        loop secnum acc =
            let
                next =
                    findSectorFromTag tag secnum (Tuple.second acc)
            in
            if next < 0 then
                acc

            else
                loop next (body next acc)
    in
    loop -1 ( False, world )


{-| P\_FindMinSurroundingLight sector max: the darkest light around the sector, at
most `max`.
-}
findMinSurroundingLight : Env -> SectorId -> Int -> World -> Int
findMinSurroundingLight env sec maxLight world =
    List.foldl (\other m -> min m (World.sector other world).lightlevel)
        maxLight
        (neighbours env sec)


{-| `S_StartSound((mobj_t *)&sector->soundorg, sfx)`: a sound from the middle of the
sector.
-}
sectorSound : SectorId -> Sfx -> World -> World
sectorSound sec sfx =
    World.startSound (Sound.FromSector sec) sfx
