module Doom.Play.Enemy exposing (noiseAlert, keenDie, look, chase, faceTarget, posAttack, sPosAttack, cPosAttack, cPosRefire, spidRefire, bspiAttack, troopAttack, sargAttack, headAttack, cyberAttack, bruisAttack, skelMissile, tracer, skelWhoosh, skelFist, vileChase, vileStart, startFire, fireCrackle, fire, vileTarget, vileAttack, fatRaise, fatAttack1, fatAttack2, fatAttack3, skullAttack, painAttack, painDie, scream, xScream, pain, fall, explode, bossDeath, hoof, metal, babyMetal, brainAwake, brainPain, brainScream, brainExplode, brainDie, brainSpit, spawnSound, spawnFly, playerScream)

{-| Monsters (p\_enemy.c): hearing, seeing, chasing and attacking, and their action
functions. The three super shotgun actions p\_enemy.c also holds are in
`Doom.Play.Weapon`.

@docs noiseAlert, keenDie, look, chase, faceTarget, posAttack, sPosAttack, cPosAttack, cPosRefire, spidRefire, bspiAttack, troopAttack, sargAttack, headAttack, cyberAttack, bruisAttack, skelMissile, tracer, skelWhoosh, skelFist, vileChase, vileStart, startFire, fireCrackle, fire, vileTarget, vileAttack, fatRaise, fatAttack1, fatAttack2, fatAttack3, skullAttack, painAttack, painDie, scream, xScream, pain, fall, explode, bossDeath, hoof, metal, babyMetal, brainAwake, brainPain, brainScream, brainExplode, brainDie, brainSpit, spawnSound, spawnFly, playerScream

-}

import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.State as State exposing (State)
import Doom.Level as Level exposing (Level, LineId, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env as Env exposing (Env)
import Doom.World as World exposing (World)


{-| P\_NoiseAlert target emitter.
-}
noiseAlert : Env -> ThinkerId -> ThinkerId -> World -> World
noiseAlert =
    Debug.todo "Doom.Play.Enemy.noiseAlert"


{-| A\_KeenDie.
-}
keenDie : Env -> ThinkerId -> World -> World
keenDie =
    Debug.todo "Doom.Play.Enemy.keenDie"


{-| A\_Look.
-}
look : Env -> ThinkerId -> World -> World
look =
    Debug.todo "Doom.Play.Enemy.look"


{-| A\_Chase.
-}
chase : Env -> ThinkerId -> World -> World
chase =
    Debug.todo "Doom.Play.Enemy.chase"


{-| A\_FaceTarget.
-}
faceTarget : Env -> ThinkerId -> World -> World
faceTarget =
    Debug.todo "Doom.Play.Enemy.faceTarget"


{-| A\_PosAttack.
-}
posAttack : Env -> ThinkerId -> World -> World
posAttack =
    Debug.todo "Doom.Play.Enemy.posAttack"


{-| A\_SPosAttack.
-}
sPosAttack : Env -> ThinkerId -> World -> World
sPosAttack =
    Debug.todo "Doom.Play.Enemy.sPosAttack"


{-| A\_CPosAttack.
-}
cPosAttack : Env -> ThinkerId -> World -> World
cPosAttack =
    Debug.todo "Doom.Play.Enemy.cPosAttack"


{-| A\_CPosRefire.
-}
cPosRefire : Env -> ThinkerId -> World -> World
cPosRefire =
    Debug.todo "Doom.Play.Enemy.cPosRefire"


{-| A\_SpidRefire.
-}
spidRefire : Env -> ThinkerId -> World -> World
spidRefire =
    Debug.todo "Doom.Play.Enemy.spidRefire"


{-| A\_BspiAttack.
-}
bspiAttack : Env -> ThinkerId -> World -> World
bspiAttack =
    Debug.todo "Doom.Play.Enemy.bspiAttack"


{-| A\_TroopAttack.
-}
troopAttack : Env -> ThinkerId -> World -> World
troopAttack =
    Debug.todo "Doom.Play.Enemy.troopAttack"


{-| A\_SargAttack.
-}
sargAttack : Env -> ThinkerId -> World -> World
sargAttack =
    Debug.todo "Doom.Play.Enemy.sargAttack"


{-| A\_HeadAttack.
-}
headAttack : Env -> ThinkerId -> World -> World
headAttack =
    Debug.todo "Doom.Play.Enemy.headAttack"


{-| A\_CyberAttack.
-}
cyberAttack : Env -> ThinkerId -> World -> World
cyberAttack =
    Debug.todo "Doom.Play.Enemy.cyberAttack"


{-| A\_BruisAttack.
-}
bruisAttack : Env -> ThinkerId -> World -> World
bruisAttack =
    Debug.todo "Doom.Play.Enemy.bruisAttack"


{-| A\_SkelMissile.
-}
skelMissile : Env -> ThinkerId -> World -> World
skelMissile =
    Debug.todo "Doom.Play.Enemy.skelMissile"


{-| A\_Tracer.
-}
tracer : Env -> ThinkerId -> World -> World
tracer =
    Debug.todo "Doom.Play.Enemy.tracer"


{-| A\_SkelWhoosh.
-}
skelWhoosh : Env -> ThinkerId -> World -> World
skelWhoosh =
    Debug.todo "Doom.Play.Enemy.skelWhoosh"


{-| A\_SkelFist.
-}
skelFist : Env -> ThinkerId -> World -> World
skelFist =
    Debug.todo "Doom.Play.Enemy.skelFist"


{-| A\_VileChase.
-}
vileChase : Env -> ThinkerId -> World -> World
vileChase =
    Debug.todo "Doom.Play.Enemy.vileChase"


{-| A\_VileStart.
-}
vileStart : Env -> ThinkerId -> World -> World
vileStart =
    Debug.todo "Doom.Play.Enemy.vileStart"


{-| A\_StartFire.
-}
startFire : Env -> ThinkerId -> World -> World
startFire =
    Debug.todo "Doom.Play.Enemy.startFire"


{-| A\_FireCrackle.
-}
fireCrackle : Env -> ThinkerId -> World -> World
fireCrackle =
    Debug.todo "Doom.Play.Enemy.fireCrackle"


{-| A\_Fire.
-}
fire : Env -> ThinkerId -> World -> World
fire =
    Debug.todo "Doom.Play.Enemy.fire"


{-| A\_VileTarget.
-}
vileTarget : Env -> ThinkerId -> World -> World
vileTarget =
    Debug.todo "Doom.Play.Enemy.vileTarget"


{-| A\_VileAttack.
-}
vileAttack : Env -> ThinkerId -> World -> World
vileAttack =
    Debug.todo "Doom.Play.Enemy.vileAttack"


{-| A\_FatRaise.
-}
fatRaise : Env -> ThinkerId -> World -> World
fatRaise =
    Debug.todo "Doom.Play.Enemy.fatRaise"


{-| A\_FatAttack1.
-}
fatAttack1 : Env -> ThinkerId -> World -> World
fatAttack1 =
    Debug.todo "Doom.Play.Enemy.fatAttack1"


{-| A\_FatAttack2.
-}
fatAttack2 : Env -> ThinkerId -> World -> World
fatAttack2 =
    Debug.todo "Doom.Play.Enemy.fatAttack2"


{-| A\_FatAttack3.
-}
fatAttack3 : Env -> ThinkerId -> World -> World
fatAttack3 =
    Debug.todo "Doom.Play.Enemy.fatAttack3"


{-| A\_SkullAttack.
-}
skullAttack : Env -> ThinkerId -> World -> World
skullAttack =
    Debug.todo "Doom.Play.Enemy.skullAttack"


{-| A\_PainAttack.
-}
painAttack : Env -> ThinkerId -> World -> World
painAttack =
    Debug.todo "Doom.Play.Enemy.painAttack"


{-| A\_PainDie.
-}
painDie : Env -> ThinkerId -> World -> World
painDie =
    Debug.todo "Doom.Play.Enemy.painDie"


{-| A\_Scream.
-}
scream : Env -> ThinkerId -> World -> World
scream =
    Debug.todo "Doom.Play.Enemy.scream"


{-| A\_XScream.
-}
xScream : Env -> ThinkerId -> World -> World
xScream =
    Debug.todo "Doom.Play.Enemy.xScream"


{-| A\_Pain.
-}
pain : Env -> ThinkerId -> World -> World
pain =
    Debug.todo "Doom.Play.Enemy.pain"


{-| A\_Fall.
-}
fall : Env -> ThinkerId -> World -> World
fall =
    Debug.todo "Doom.Play.Enemy.fall"


{-| A\_Explode.
-}
explode : Env -> ThinkerId -> World -> World
explode =
    Debug.todo "Doom.Play.Enemy.explode"


{-| A\_BossDeath.
-}
bossDeath : Env -> ThinkerId -> World -> World
bossDeath =
    Debug.todo "Doom.Play.Enemy.bossDeath"


{-| A\_Hoof.
-}
hoof : Env -> ThinkerId -> World -> World
hoof =
    Debug.todo "Doom.Play.Enemy.hoof"


{-| A\_Metal.
-}
metal : Env -> ThinkerId -> World -> World
metal =
    Debug.todo "Doom.Play.Enemy.metal"


{-| A\_BabyMetal.
-}
babyMetal : Env -> ThinkerId -> World -> World
babyMetal =
    Debug.todo "Doom.Play.Enemy.babyMetal"


{-| A\_BrainAwake.
-}
brainAwake : Env -> ThinkerId -> World -> World
brainAwake =
    Debug.todo "Doom.Play.Enemy.brainAwake"


{-| A\_BrainPain.
-}
brainPain : Env -> ThinkerId -> World -> World
brainPain =
    Debug.todo "Doom.Play.Enemy.brainPain"


{-| A\_BrainScream.
-}
brainScream : Env -> ThinkerId -> World -> World
brainScream =
    Debug.todo "Doom.Play.Enemy.brainScream"


{-| A\_BrainExplode.
-}
brainExplode : Env -> ThinkerId -> World -> World
brainExplode =
    Debug.todo "Doom.Play.Enemy.brainExplode"


{-| A\_BrainDie.
-}
brainDie : Env -> ThinkerId -> World -> World
brainDie =
    Debug.todo "Doom.Play.Enemy.brainDie"


{-| A\_BrainSpit.
-}
brainSpit : Env -> ThinkerId -> World -> World
brainSpit =
    Debug.todo "Doom.Play.Enemy.brainSpit"


{-| A\_SpawnSound.
-}
spawnSound : Env -> ThinkerId -> World -> World
spawnSound =
    Debug.todo "Doom.Play.Enemy.spawnSound"


{-| A\_SpawnFly.
-}
spawnFly : Env -> ThinkerId -> World -> World
spawnFly =
    Debug.todo "Doom.Play.Enemy.spawnFly"


{-| A\_PlayerScream.
-}
playerScream : Env -> ThinkerId -> World -> World
playerScream =
    Debug.todo "Doom.Play.Enemy.playerScream"
