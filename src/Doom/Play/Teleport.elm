module Doom.Play.Teleport exposing (teleport)

{-| Teleporters (p\_telept.c).

@docs teleport

-}

import Array
import Dict
import Doom.Angle as Angle
import Doom.Fixed as Fixed
import Doom.Flags as Flags
import Doom.Info.MobjType as MobjType
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId, SectorId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.Map as Map
import Doom.Play.Mobj as PMobj
import Doom.Sound as Sound
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)


{-| EV\_Teleport line side thing: move the thing to the teleport destination (the
first MT\_TELEPORTMAN, in thinker order, in the first sector with the line's tag that
has one), with fog where it was and in front of where it lands. Missiles don't
teleport, nor does anything crossing the line from the back. False if nothing
happened, also when something at the destination couldn't be telefragged.
-}
teleport : Env -> LineId -> Int -> ThinkerId -> World -> ( Bool, World )
teleport env lineId side thingId world =
    case World.mobj thingId world of
        Nothing ->
            ( False, world )

        Just thing ->
            if Flags.has Flags.mfMissile thing.flags || side == 1 then
                ( False, world )

            else
                case destination env (Level.line lineId env.level).tag world of
                    Nothing ->
                        ( False, world )

                    Just dest ->
                        teleportTo env thing dest world


{-| The first sector, in order, with the tag and a teleport destination in it, and the
first destination there in thinker order.
-}
destination : Env -> Int -> World -> Maybe Mobj
destination env tag world =
    let
        destinations =
            Dict.foldr
                (\id t acc ->
                    case t of
                        MobjThinker ->
                            case World.mobj id world of
                                Just m ->
                                    if m.type_ == MobjType.Teleportman then
                                        m :: acc

                                    else
                                        acc

                                Nothing ->
                                    acc

                        _ ->
                            acc
                )
                []
                world.thinkers

        inSector : SectorId -> Maybe Mobj
        inSector sec =
            List.filter (\m -> World.mobjSector env.level m == sec) destinations |> List.head

        searchFrom sec =
            if sec >= Array.length world.sectors then
                Nothing

            else if (World.sector sec world).tag == tag then
                case inSector sec of
                    Just m ->
                        Just m

                    Nothing ->
                        searchFrom (sec + 1)

            else
                searchFrom (sec + 1)
    in
    searchFrom 0


teleportTo : Env -> Mobj -> Mobj -> World -> ( Bool, World )
teleportTo env thing dest world =
    let
        ( ok, moved ) =
            Map.teleportMove env thing.id dest.x dest.y world
    in
    if not ok then
        ( False, moved )

    else
        let
            landed =
                World.updateMobj thing.id (\m -> { m | z = m.floorz }) moved

            z =
                World.mobj thing.id landed |> Maybe.map .z |> Maybe.withDefault thing.z

            withView =
                case thing.player of
                    Just pnum ->
                        World.updatePlayer pnum (\p -> { p | viewz = Fixed.add z p.viewheight }) landed

                    Nothing ->
                        landed

            ( fog1, w1 ) =
                PMobj.spawnMobj env thing.x thing.y thing.z MobjType.Tfog withView

            w2 =
                World.startSound (Sound.FromMobj fog1) Sfx.Telept w1

            ( fog2, w3 ) =
                PMobj.spawnMobj env
                    (Fixed.add dest.x (20 * Angle.cosine dest.angle))
                    (Fixed.add dest.y (20 * Angle.sine dest.angle))
                    z
                    MobjType.Tfog
                    w2

            w4 =
                World.startSound (Sound.FromMobj fog2) Sfx.Telept w3
        in
        ( True
        , World.updateMobj thing.id
            (\m ->
                { m
                    | reactiontime =
                        if thing.player /= Nothing then
                            18

                        else
                            m.reactiontime
                    , angle = dest.angle
                    , momx = 0
                    , momy = 0
                    , momz = 0
                }
            )
            w4
        )
