module Doom.Play.Inter exposing (giveAmmo, giveWeapon, giveBody, giveArmor, giveCard, givePower, touchSpecialThing, killMobj, damageMobj)

{-| Picking things up, getting hurt and dying (p\_inter.c).

@docs giveAmmo, giveWeapon, giveBody, giveArmor, giveCard, givePower, touchSpecialThing, killMobj, damageMobj

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs exposing (Ammo, Card, Power, Weapon)
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| P\_GiveAmmo player ammo num (num is clips, 0 for half a clip).
-}
giveAmmo : Env -> Int -> Ammo -> Int -> World -> ( Bool, World )
giveAmmo =
    Debug.todo "Doom.Play.Inter.giveAmmo"


{-| P\_GiveWeapon player weapon dropped.
-}
giveWeapon : Env -> Int -> Weapon -> Bool -> World -> ( Bool, World )
giveWeapon =
    Debug.todo "Doom.Play.Inter.giveWeapon"


{-| P\_GiveBody player num.
-}
giveBody : Env -> Int -> Int -> World -> ( Bool, World )
giveBody =
    Debug.todo "Doom.Play.Inter.giveBody"


{-| P\_GiveArmor player armortype.
-}
giveArmor : Env -> Int -> Int -> World -> ( Bool, World )
giveArmor =
    Debug.todo "Doom.Play.Inter.giveArmor"


{-| P\_GiveCard.
-}
giveCard : Env -> Int -> Card -> World -> World
giveCard =
    Debug.todo "Doom.Play.Inter.giveCard"


{-| P\_GivePower.
-}
givePower : Env -> Int -> Power -> World -> ( Bool, World )
givePower =
    Debug.todo "Doom.Play.Inter.givePower"


{-| P\_TouchSpecialThing special toucher.
-}
touchSpecialThing : Env -> ThinkerId -> ThinkerId -> World -> World
touchSpecialThing =
    Debug.todo "Doom.Play.Inter.touchSpecialThing"


{-| P\_KillMobj source target.
-}
killMobj : Env -> Maybe ThinkerId -> ThinkerId -> World -> World
killMobj =
    Debug.todo "Doom.Play.Inter.killMobj"


{-| P\_DamageMobj target inflictor source damage.
-}
damageMobj : Env -> ThinkerId -> Maybe ThinkerId -> Maybe ThinkerId -> Int -> World -> World
damageMobj =
    Debug.todo "Doom.Play.Inter.damageMobj"
