module Doom.StateHash exposing (line, combine)

{-| One line summing up a level's state after a tic, to compare this port with the C
game tic by tic (doom/oracle prints the same line from linuxdoom-1.10).

The line, values in decimal, separated by single spaces:

    <leveltime> r<prndindex> p<x>,<y>,<z>,<angle>,<momx>,<momy>,<health>,<armorpoints>,<readyweapon>,<ammo0>,<ammo1>,<ammo2>,<ammo3> m<count> h<mobjhash> s<sectorhash>

  - The p part is player 0's body (x, y, z, angle, momx, momy) and player state
    (health, armorpoints, readyweapon's index, ammo by ammo type). Without a body,
    the six body values are 0.
  - `count` is the number of things: thinkers in the thinker list running
    P\_MobjThinker (not removed).
  - `mobjhash` runs over those things in thinker-list order, and for each over x, y,
    z, angle, momx, momy, momz, type (mobjtype\_t index), state (statenum\_t index),
    tics, health, flags: h = (h \* 31 + v) mod 2^32, starting from 0, each v taken
    mod 2^32.
  - `sectorhash` runs the same way over every sector in order: floorheight,
    ceilingheight, lightlevel, special.

`angle` is unsigned; the hashes are unsigned; everything else is a signed 32-bit
value.

@docs line, combine

-}

import Array
import Dict
import Doom.Defs as Defs
import Doom.Info.MobjType as MobjType
import Doom.Info.State as State
import Doom.Mobj exposing (Mobj)
import Doom.Player as Player
import Doom.Thinker exposing (Thinker(..))
import Doom.World as World exposing (World)


{-| The line for the world as it is.
-}
line : World -> String
line world =
    let
        things =
            Dict.toList world.thinkers
                |> List.filterMap
                    (\( id, t ) ->
                        case t of
                            MobjThinker ->
                                World.mobj id world

                            _ ->
                                Nothing
                    )

        body =
            World.playerMobj 0 world

        bodyValues =
            case body of
                Just m ->
                    [ m.x, m.y, m.z, m.angle, m.momx, m.momy ]

                Nothing ->
                    [ 0, 0, 0, 0, 0, 0 ]

        playerValues =
            case World.player 0 world of
                Just p ->
                    [ p.health, p.armorpoints, Defs.weaponIndex p.readyweapon ]
                        ++ List.map (\a -> Player.ammo a p) Defs.allAmmo

                Nothing ->
                    [ 0, 0, 0, 0, 0, 0, 0 ]

        sectorHash =
            Array.foldl
                (\s h -> List.foldl combine h [ s.floorheight, s.ceilingheight, s.lightlevel, s.special ])
                0
                world.sectors
    in
    String.join " "
        [ String.fromInt world.leveltime
        , "r" ++ String.fromInt world.rng.prndindex
        , "p" ++ String.join "," (List.map String.fromInt (bodyValues ++ playerValues))
        , "m" ++ String.fromInt (List.length things)
        , "h" ++ String.fromInt (List.foldl mobjHash 0 things)
        , "s" ++ String.fromInt sectorHash
        ]


mobjHash : Mobj -> Int -> Int
mobjHash m h =
    List.foldl combine
        h
        [ m.x
        , m.y
        , m.z
        , m.angle
        , m.momx
        , m.momy
        , m.momz
        , MobjType.index m.type_
        , State.index m.state
        , m.tics
        , m.health
        , m.flags
        ]


{-| One step of the hash: `(h * 31 + v) mod 2^32`, v taken mod 2^32 (h is already in
[0, 2^32), so the product stays exact).
-}
combine : Int -> Int -> Int
combine v h =
    modBy 4294967296 (h * 31 + modBy 4294967296 v)
