module Doom.Flags exposing
    ( has, set, clear
    , mfSpecial, mfSolid, mfShootable, mfNoSector, mfNoBlockmap, mfAmbush, mfJustHit, mfJustAttacked, mfSpawnCeiling, mfNoGravity, mfDropOff, mfPickup, mfNoClip, mfSlide, mfFloat, mfTeleport, mfMissile, mfDropped, mfShadow, mfNoBlood, mfCorpse, mfInFloat, mfCountKill, mfCountItem, mfSkullFly, mfNotDeathmatch, mfTranslation
    , mfTransShift
    , mlBlocking, mlBlockMonsters, mlTwoSided, mlDontPegTop, mlDontPegBottom, mlSecret, mlSoundBlock, mlDontDraw, mlMapped
    )

{-| Bit flags: things' `flags` (mobjflag\_t, p\_mobj.h) and lines' `flags`
(doomdata.h), as in the C source, so the game code can test and combine them the same
way.

@docs has, set, clear
@docs mfSpecial, mfSolid, mfShootable, mfNoSector, mfNoBlockmap, mfAmbush, mfJustHit, mfJustAttacked, mfSpawnCeiling, mfNoGravity, mfDropOff, mfPickup, mfNoClip, mfSlide, mfFloat, mfTeleport, mfMissile, mfDropped, mfShadow, mfNoBlood, mfCorpse, mfInFloat, mfCountKill, mfCountItem, mfSkullFly, mfNotDeathmatch, mfTranslation
@docs mfTransShift
@docs mlBlocking, mlBlockMonsters, mlTwoSided, mlDontPegTop, mlDontPegBottom, mlSecret, mlSoundBlock, mlDontDraw, mlMapped

-}

import Bitwise


{-| Whether `flags` has any of the bits in `bit`.
-}
has : Int -> Int -> Bool
has bit flags =
    Bitwise.and bit flags /= 0


{-| `flags | bit`.
-}
set : Int -> Int -> Int
set bit flags =
    Bitwise.or bit flags


{-| `flags & ~bit`.
-}
clear : Int -> Int -> Int
clear bit flags =
    Bitwise.and (Bitwise.complement bit) flags


{-| Picked up when touched.
-}
mfSpecial : Int
mfSpecial =
    0x01


{-| Blocks movement.
-}
mfSolid : Int
mfSolid =
    0x02


{-| Can be hurt.
-}
mfShootable : Int
mfShootable =
    0x04


{-| Not linked into its sector's thing list (invisible).
-}
mfNoSector : Int
mfNoSector =
    0x08


{-| Not linked into the blockmap (doesn't collide).
-}
mfNoBlockmap : Int
mfNoBlockmap =
    0x10


{-| Deaf: waits for sight, not sound.
-}
mfAmbush : Int
mfAmbush =
    0x20


{-| Was just hurt: will try to attack back.
-}
mfJustHit : Int
mfJustHit =
    0x40


{-| Just attacked: will move before attacking again.
-}
mfJustAttacked : Int
mfJustAttacked =
    0x80


{-| Spawns hanging from the ceiling.
-}
mfSpawnCeiling : Int
mfSpawnCeiling =
    0x0100


{-| Doesn't fall.
-}
mfNoGravity : Int
mfNoGravity =
    0x0200


{-| Can step off ledges.
-}
mfDropOff : Int
mfDropOff =
    0x0400


{-| Picks up items (players).
-}
mfPickup : Int
mfPickup =
    0x0800


{-| Walks through walls and things.
-}
mfNoClip : Int
mfNoClip =
    0x1000


{-| Slides along walls (players).
-}
mfSlide : Int
mfSlide =
    0x2000


{-| Flies (cacodemons, lost souls).
-}
mfFloat : Int
mfFloat =
    0x4000


{-| Doesn't cross line specials (being teleported).
-}
mfTeleport : Int
mfTeleport =
    0x8000


{-| A projectile: explodes on contact.
-}
mfMissile : Int
mfMissile =
    0x00010000


{-| Dropped by a monster rather than placed in the map.
-}
mfDropped : Int
mfDropped =
    0x00020000


{-| Drawn as fuzz (spectres, invisible players).
-}
mfShadow : Int
mfShadow =
    0x00040000


{-| Puffs instead of bleeding.
-}
mfNoBlood : Int
mfNoBlood =
    0x00080000


{-| Dead: slides down stairs.
-}
mfCorpse : Int
mfCorpse =
    0x00100000


{-| Floating to the target's height: don't move sideways.
-}
mfInFloat : Int
mfInFloat =
    0x00200000


{-| Counts towards the kill percentage.
-}
mfCountKill : Int
mfCountKill =
    0x00400000


{-| Counts towards the item percentage.
-}
mfCountItem : Int
mfCountItem =
    0x00800000


{-| A lost soul in mid-charge.
-}
mfSkullFly : Int
mfSkullFly =
    0x01000000


{-| Not spawned in deathmatch.
-}
mfNotDeathmatch : Int
mfNotDeathmatch =
    0x02000000


{-| Player colour translation bits.
-}
mfTranslation : Int
mfTranslation =
    0x0C000000


{-| MF\_TRANSSHIFT: how far to shift `mfTranslation` bits down.
-}
mfTransShift : Int
mfTransShift =
    26


{-| Blocks players and monsters.
-}
mlBlocking : Int
mlBlocking =
    0x01


{-| Blocks monsters.
-}
mlBlockMonsters : Int
mlBlockMonsters =
    0x02


{-| Has a back side.
-}
mlTwoSided : Int
mlTwoSided =
    0x04


{-| Upper texture unpegged.
-}
mlDontPegTop : Int
mlDontPegTop =
    0x08


{-| Lower texture unpegged.
-}
mlDontPegBottom : Int
mlDontPegBottom =
    0x10


{-| Shown as a one-sided wall on the automap.
-}
mlSecret : Int
mlSecret =
    0x20


{-| Blocks sound (two of these block it).
-}
mlSoundBlock : Int
mlSoundBlock =
    0x40


{-| Never shown on the automap.
-}
mlDontDraw : Int
mlDontDraw =
    0x80


{-| Seen by the player: shown on the automap.
-}
mlMapped : Int
mlMapped =
    0x0100
