module Doom.Save exposing (save, description, load, version)

{-| Saved games (G\_DoSaveGame, G\_DoLoadGame and p\_saveg.c), in the file format
linuxdoom-1.10 writes, laid out as the original 32-bit x86 program laid it out.

A save file is:

  - the description, 24 bytes (SAVESTRINGSIZE);
  - the version string, "version 109" padded with zeros to 16 bytes;
  - skill, episode and map, one byte each, then a byte for each of the four players
    saying whether they are in the game;
  - `leveltime` in three bytes, most significant first;
  - each player in the game (player\_t, 280 bytes);
  - the sectors, lines and sides (P\_ArchiveWorld: 16-bit numbers, heights and
    texture offsets in whole units);
  - every thing (a byte 1, then mobj\_t, 156 bytes), then a byte 0;
  - the moving floors, ceilings, doors, lifts and lights (a byte saying which, then
    the thinker's struct), then a byte 7;
  - the byte 0x1d.

Before each struct the position is rounded up to a multiple of 4 (PADSAVEP).

The structs are written as the original wrote them, with a pointer's 4 bytes
standing in for each pointer. The original wrote raw memory addresses there, which
mean nothing outside the program that wrote them, and its loader reads none of them
except a thinker's `function` (only to see whether it is NULL: a stopped crusher).
So here every pointer is written as 0, except `function`, which is 1 for a thinker
that is running and 0 for a crusher in stasis. The pointers the original does
translate are translated as it does: a thing's state and a weapon sprite's state are
written as state numbers (a weapon sprite that isn't shown as 0), a thing's player as
the player's number plus one (0 for none), a special's sector as the sector's number.
The padding bytes the original skipped over (whatever was in its buffer) are 0.

What a loaded game gets back, and what it loses, is exactly what it loses in the
original:

  - Every thing's `target` is gone, so monsters forget who they were chasing and
    missiles who fired them. `tracer` is a raw pointer the original keeps as it was,
    pointing at memory from before the load; here it is gone too (only Doom II's
    revenant missiles and arch-vile flames read it).
  - The players' `attacker` and `message` are gone.
  - A sector's `soundtarget` is gone (monsters forget what they heard).
  - Sector heights, and wall texture offsets, are rounded down to whole units.
  - A thing's `floorz` and `ceilingz` become its sector's floor and ceiling height.
  - Flickering lights (fireflicker\_t) and stopped lifts (plats in stasis) are not
    saved, so they are gone: their sectors are free for a new special.
  - Things come back first, in the order they were in, and the moving floors, lights
    and so on after them, so the order thinkers run in changes.
  - The thing and blockmap lists are rebuilt by linking each thing in turn, as
    P\_SetThingPosition does, so they end up in the reverse of the things' order.

Loading starts the level afresh first (G\_InitNew), which resets the random numbers
and then spawns the map's things before they are thrown away: the random number
index after a load is what spawning the level used, as in the original, and not
what it was when the game was saved. The things thrown away go through P\_RemoveMobj,
so in a deathmatch their items go on the respawn queue, at the loaded `leveltime`.

Nothing else of the game is in the file: the skill comes back, but -fast, -respawn,
-nomonsters, deathmatch, the net game and who the console player is stay as the
game being loaded into has them, as do `gametic` and whether the monsters are fast.

@docs save, description, load, version

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)
import Dict
import Doom.Angle as Angle
import Doom.Defs as Defs exposing (PlayerState(..), Skill(..))
import Doom.Game as Game exposing (Game)
import Doom.Info.MobjType as MobjType
import Doom.Info.Sprite as Sprite
import Doom.Info.State as State
import Doom.Level exposing (MapThing)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Play.Env exposing (Env)
import Doom.Play.MapUtil as MapUtil
import Doom.Play.Mobj as PlayMobj
import Doom.Player exposing (Player, PspDef)
import Doom.Thinker as Thinker exposing (Thinker(..))
import Doom.Ticcmd exposing (Ticcmd)
import Doom.World as World exposing (World)



-- CONSTANTS


{-| The version string a save starts with after its description, "version 109"
(`sprintf (name2, "version %i", VERSION)`; VERSION is 109 in the DOS v1.9
executable, 110 in linuxdoom-1.10). Loading refuses any other.
-}
version : String
version =
    "version 109"


saveStringSize : Int
saveStringSize =
    24


versionSize : Int
versionSize =
    16


{-| Where the players start: the description, version, skill, episode, map, four
player bytes and three bytes of leveltime.
-}
headerSize : Int
headerSize =
    saveStringSize + versionSize + 3 + Defs.maxPlayers + 3


consistencyMarker : Int
consistencyMarker =
    0x1D


playerSize : Int
playerSize =
    280


mobjSize : Int
mobjSize =
    156


{-| thinkerclass\_t.
-}
tcEnd : Int
tcEnd =
    0


tcMobj : Int
tcMobj =
    1


{-| specials\_e.
-}
tcCeiling : Int
tcCeiling =
    0


tcDoor : Int
tcDoor =
    1


tcFloor : Int
tcFloor =
    2


tcPlat : Int
tcPlat =
    3


tcFlash : Int
tcFlash =
    4


tcStrobe : Int
tcStrobe =
    5


tcGlow : Int
tcGlow =
    6


tcEndSpecials : Int
tcEndSpecials =
    7


{-| wp\_nochange.
-}
wpNoChange : Int
wpNoChange =
    10



-- DESCRIPTION


{-| The description at the start of a save (what the load and save menus list): up to
24 characters, ending at the first zero byte, as M\_ReadSaveStrings reads it.
Nothing if the file is too short to have one.
-}
description : Bytes -> Maybe String
description bytes =
    Decode.decode (cString saveStringSize) bytes


{-| `n` bytes holding a C string: the characters before the first zero byte.
-}
cString : Int -> Decoder String
cString n =
    Decode.loop ( n, [] )
        (\( left, codes ) ->
            if left <= 0 then
                Decode.succeed (Done (toCString codes))

            else
                Decode.unsignedInt8 |> Decode.map (\b -> Loop ( left - 1, b :: codes ))
        )


toCString : List Int -> String
toCString reversedCodes =
    List.reverse reversedCodes
        |> takeWhile (\c -> c /= 0)
        |> List.map Char.fromCode
        |> String.fromList


takeWhile : (a -> Bool) -> List a -> List a
takeWhile keep list =
    case list of
        x :: rest ->
            if keep x then
                x :: takeWhile keep rest

            else
                []

        [] ->
            []



-- SAVING


{-| G\_DoSaveGame: the game as a save file with this description (the first 24
characters of it, one byte each).

Fails unless the game is in a level (the menu only saves then), and, as the
original does with "Savegame buffer overrun", when the save would be bigger than
SAVEGAMESIZE (180224 bytes): a level with a lot of things can't be saved. (The
original quits there; here the game goes on unsaved.)

-}
save : String -> Game -> Result String Bytes
save desc game =
    case Game.playing game of
        Nothing ->
            Err "Not in a level"

        Just ( env, world ) ->
            let
                out =
                    emptyOut
                        |> fixedString saveStringSize desc
                        |> fixedString versionSize version
                        |> u8 (Defs.skillIndex game.params.skill)
                        |> u8 game.params.episode
                        |> u8 game.params.map
                        |> forEach (Array.toList world.playeringame) (\inGame -> u8 (boolInt inGame))
                        |> u8 (Bitwise.shiftRightBy 16 world.leveltime)
                        |> u8 (Bitwise.shiftRightBy 8 world.leveltime)
                        |> u8 world.leveltime
                        |> archivePlayers world
                        |> archiveWorld env world
                        |> archiveThinkers world
                        |> archiveSpecials world
                        |> u8 consistencyMarker
            in
            if out.offset > saveGameSize then
                Err "Savegame buffer overrun"

            else
                Ok (toBytes out)


{-| SAVEGAMESIZE: the most a save may take.
-}
saveGameSize : Int
saveGameSize =
    0x0002C000


{-| The bytes written so far (newest first) and how many there are, which PADSAVEP
needs.
-}
type alias Out =
    { offset : Int
    , parts : List Encoder
    }


emptyOut : Out
emptyOut =
    { offset = 0, parts = [] }


toBytes : Out -> Bytes
toBytes out =
    Encode.encode (Encode.sequence (List.reverse out.parts))


put : Int -> Encoder -> Out -> Out
put size encoder out =
    { offset = out.offset + size, parts = encoder :: out.parts }


u8 : Int -> Out -> Out
u8 n =
    put 1 (Encode.unsignedInt8 (Bitwise.and n 0xFF))


i8 : Int -> Out -> Out
i8 =
    u8


i16 : Int -> Out -> Out
i16 n =
    put 2 (Encode.signedInt16 LE (Bitwise.shiftRightBy 16 (Bitwise.shiftLeftBy 16 n)))


i32 : Int -> Out -> Out
i32 n =
    put 4 (Encode.signedInt32 LE (Bitwise.or n 0))


{-| A pointer the loader doesn't read.
-}
pointer : Out -> Out
pointer =
    i32 0


zeros : Int -> Out -> Out
zeros n out =
    if n <= 0 then
        out

    else
        zeros (n - 1) (u8 0 out)


{-| PADSAVEP: on to the next multiple of 4.
-}
pad : Out -> Out
pad out =
    zeros (padding out.offset) out


padding : Int -> Int
padding offset =
    Bitwise.and (4 - Bitwise.and offset 3) 3


fixedString : Int -> String -> Out -> Out
fixedString size s out =
    let
        codes =
            String.toList s |> List.map (Char.toCode >> Bitwise.and 0xFF) |> List.take size
    in
    List.foldl u8 out codes |> zeros (size - List.length codes)


forEach : List a -> (a -> Out -> Out) -> Out -> Out
forEach items f out =
    List.foldl f out items


boolInt : Bool -> Int
boolInt b =
    if b then
        1

    else
        0


{-| Whole units, as `fixed >> FRACBITS`.
-}
wholeUnits : Int -> Int
wholeUnits fixed =
    Bitwise.shiftRightBy 16 fixed


{-| A thinker\_t: `prev`, `next`, then `function` (1 running, 0 stopped).
-}
thinkerHead : Bool -> Out -> Out
thinkerHead running =
    pointer >> pointer >> i32 (boolInt running)



-- P_ArchivePlayers


archivePlayers : World -> Out -> Out
archivePlayers world out =
    List.foldl
        (\( inGame, p ) o ->
            if inGame then
                writePlayer p (pad o)

            else
                o
        )
        out
        (List.map2 Tuple.pair (Array.toList world.playeringame) (Array.toList world.players))


writePlayer : Player -> Out -> Out
writePlayer p out =
    out
        |> pointer
        |> i32 (playerStateIndex p.playerstate)
        |> writeTiccmd p.cmd
        |> i32 p.viewz
        |> i32 p.viewheight
        |> i32 p.deltaviewheight
        |> i32 p.bob
        |> i32 p.health
        |> i32 p.armorpoints
        |> i32 p.armortype
        |> intArray 6 p.powers
        |> intArray 6 (Array.map boolInt p.cards)
        |> i32 (boolInt p.backpack)
        |> intArray Defs.maxPlayers p.frags
        |> i32 (Defs.weaponIndex p.readyweapon)
        |> i32 (p.pendingweapon |> Maybe.map Defs.weaponIndex |> Maybe.withDefault wpNoChange)
        |> intArray 9 (Array.map boolInt p.weaponowned)
        |> intArray 4 p.ammo
        |> intArray 4 p.maxammo
        |> i32 (boolInt p.attackdown)
        |> i32 (boolInt p.usedown)
        |> i32 p.cheats
        |> i32 p.refire
        |> i32 p.killcount
        |> i32 p.itemcount
        |> i32 p.secretcount
        |> pointer
        |> i32 p.damagecount
        |> i32 p.bonuscount
        |> pointer
        |> i32 p.extralight
        |> i32 p.fixedcolormap
        |> i32 p.colormap
        |> writePsp p.weaponSprite
        |> writePsp p.flashSprite
        |> i32 (boolInt p.didsecret)


intArray : Int -> Array Int -> Out -> Out
intArray n values out =
    List.foldl (\i o -> i32 (Array.get i values |> Maybe.withDefault 0) o) out (List.range 0 (n - 1))


writeTiccmd : Ticcmd -> Out -> Out
writeTiccmd cmd =
    i8 cmd.forwardmove
        >> i8 cmd.sidemove
        >> i16 cmd.angleturn
        >> i16 cmd.consistancy
        >> u8 cmd.chatchar
        >> u8 cmd.buttons


writePsp : PspDef -> Out -> Out
writePsp psp =
    i32 (psp.state |> Maybe.map State.index |> Maybe.withDefault 0)
        >> i32 psp.tics
        >> i32 psp.sx
        >> i32 psp.sy


playerStateIndex : PlayerState -> Int
playerStateIndex state =
    case state of
        Live ->
            0

        Dead ->
            1

        Reborn ->
            2



-- P_ArchiveWorld


archiveWorld : Env -> World -> Out -> Out
archiveWorld env world out =
    let
        withSectors =
            Array.foldl
                (\sec o ->
                    o
                        |> i16 (wholeUnits sec.floorheight)
                        |> i16 (wholeUnits sec.ceilingheight)
                        |> i16 sec.floorpic
                        |> i16 sec.ceilingpic
                        |> i16 sec.lightlevel
                        |> i16 sec.special
                        |> i16 sec.tag
                )
                out
                world.sectors

        writeSide sideId o =
            let
                side =
                    World.sideState sideId world
            in
            o
                |> i16 (wholeUnits side.textureoffset)
                |> i16 (wholeUnits side.rowoffset)
                |> i16 side.toptexture
                |> i16 side.bottomtexture
                |> i16 side.midtexture
    in
    List.foldl
        (\( state, line ) o ->
            o
                |> i16 state.flags
                |> i16 state.special
                |> i16 line.tag
                |> writeSide line.frontSide
                |> (case line.backSide of
                        Just back ->
                            writeSide back

                        Nothing ->
                            identity
                   )
        )
        withSectors
        (List.map2 Tuple.pair (Array.toList world.lines) (Array.toList env.level.lines))



-- P_ArchiveThinkers


archiveThinkers : World -> Out -> Out
archiveThinkers world out =
    Dict.foldl
        (\id t o ->
            case ( t, World.mobj id world ) of
                ( MobjThinker, Just m ) ->
                    o |> u8 tcMobj |> pad |> writeMobj m

                _ ->
                    o
        )
        out
        world.thinkers
        |> u8 tcEnd


writeMobj : Mobj -> Out -> Out
writeMobj m out =
    out
        |> thinkerHead True
        |> i32 m.x
        |> i32 m.y
        |> i32 m.z
        |> pointer
        |> pointer
        |> i32 m.angle
        |> i32 (Sprite.index m.sprite)
        |> i32 m.frame
        |> pointer
        |> pointer
        |> pointer
        |> i32 m.floorz
        |> i32 m.ceilingz
        |> i32 m.radius
        |> i32 m.height
        |> i32 m.momx
        |> i32 m.momy
        |> i32 m.momz
        |> i32 m.validcount
        |> i32 (MobjType.index m.type_)
        |> pointer
        |> i32 m.tics
        |> i32 (State.index m.state)
        |> i32 m.flags
        |> i32 m.health
        |> i32 m.movedir
        |> i32 m.movecount
        |> pointer
        |> i32 m.reactiontime
        |> i32 m.threshold
        |> i32 (m.player |> Maybe.map ((+) 1) |> Maybe.withDefault 0)
        |> i32 m.lastlook
        |> writeMapThing m.spawnpoint
        |> zeros 2
        |> pointer


writeMapThing : MapThing -> Out -> Out
writeMapThing mt =
    i16 mt.x >> i16 mt.y >> i16 mt.angle >> i16 mt.type_ >> i16 mt.options



-- P_ArchiveSpecials


{-| Each moving floor, ceiling, door, lift and light, in thinker order. A crusher in
stasis is saved (it is in `activeCeilings`); a lift in stasis is not, and neither is
a flickering light: the original doesn't look for them.
-}
archiveSpecials : World -> Out -> Out
archiveSpecials world out =
    Dict.foldl
        (\id t o ->
            case t of
                CeilingThinker c ->
                    let
                        inStasis =
                            c.direction == 0
                    in
                    if not inStasis || List.member (Just id) (Array.toList world.activeCeilings) then
                        o |> u8 tcCeiling |> pad |> writeCeiling (not inStasis) c

                    else
                        o

                DoorThinker d ->
                    o |> u8 tcDoor |> pad |> writeDoor d

                FloorThinker f ->
                    o |> u8 tcFloor |> pad |> writeFloor f

                PlatThinker p ->
                    if p.status == Thinker.PlatInStasis then
                        o

                    else
                        o |> u8 tcPlat |> pad |> writePlat p

                LightFlashThinker l ->
                    o |> u8 tcFlash |> pad |> writeFlash l

                StrobeThinker s ->
                    o |> u8 tcStrobe |> pad |> writeStrobe s

                GlowThinker g ->
                    o |> u8 tcGlow |> pad |> writeGlow g

                _ ->
                    o
        )
        out
        world.thinkers
        |> u8 tcEndSpecials


writeCeiling : Bool -> Thinker.Ceiling -> Out -> Out
writeCeiling running c =
    thinkerHead running
        >> i32 (indexIn ceilingTypes c.type_)
        >> i32 c.sector
        >> i32 c.bottomheight
        >> i32 c.topheight
        >> i32 c.speed
        >> i32 (boolInt c.crush)
        >> i32 c.direction
        >> i32 c.tag
        >> i32 c.olddirection


writeDoor : Thinker.Door -> Out -> Out
writeDoor d =
    thinkerHead True
        >> i32 (indexIn doorTypes d.type_)
        >> i32 d.sector
        >> i32 d.topheight
        >> i32 d.speed
        >> i32 d.direction
        >> i32 d.topwait
        >> i32 d.topcountdown


writeFloor : Thinker.FloorMove -> Out -> Out
writeFloor f =
    thinkerHead True
        >> i32 (indexIn floorTypes f.type_)
        >> i32 (floorCrush f)
        >> i32 f.sector
        >> i32 f.direction
        >> i32 f.newspecial
        >> i16 f.texture
        >> zeros 2
        >> i32 f.floordestheight
        >> i32 f.speed


{-| A floor's `crush` as the DOS executable has it in memory. A stair step (the only
floor of type LowerFloor that goes up) never has `crush` set by EV\_BuildStairs, and
there it holds 10 (see `Doom.Play.Plane.StairCrush`). Loaded back, any nonzero value
is True; a step is still told apart by its type and direction.
-}
floorCrush : Thinker.FloorMove -> Int
floorCrush f =
    if f.type_ == Thinker.LowerFloor && f.direction == 1 then
        stairCrush

    else
        boolInt f.crush


{-| chocolate-doom's STAIRS\_UNINITIALIZED\_CRUSH\_FIELD\_VALUE.
-}
stairCrush : Int
stairCrush =
    10


writePlat : Thinker.Plat -> Out -> Out
writePlat p =
    thinkerHead True
        >> i32 p.sector
        >> i32 p.speed
        >> i32 p.low
        >> i32 p.high
        >> i32 p.wait
        >> i32 p.count
        >> i32 (indexIn platStatuses p.status)
        >> i32 (indexIn platStatuses p.oldstatus)
        >> i32 (boolInt p.crush)
        >> i32 p.tag
        >> i32 (indexIn platTypes p.type_)


writeFlash : Thinker.LightFlash -> Out -> Out
writeFlash l =
    thinkerHead True
        >> i32 l.sector
        >> i32 l.count
        >> i32 l.maxlight
        >> i32 l.minlight
        >> i32 l.maxtime
        >> i32 l.mintime


writeStrobe : Thinker.Strobe -> Out -> Out
writeStrobe s =
    thinkerHead True
        >> i32 s.sector
        >> i32 s.count
        >> i32 s.minlight
        >> i32 s.maxlight
        >> i32 s.darktime
        >> i32 s.brighttime


writeGlow : Thinker.Glow -> Out -> Out
writeGlow g =
    thinkerHead True
        >> i32 g.sector
        >> i32 g.minlight
        >> i32 g.maxlight
        >> i32 g.direction



-- ENUMS (in their C order)


ceilingTypes : List Thinker.CeilingType
ceilingTypes =
    [ Thinker.LowerToFloor, Thinker.RaiseToHighest, Thinker.LowerAndCrush, Thinker.CrushAndRaise, Thinker.FastCrushAndRaise, Thinker.SilentCrushAndRaise ]


doorTypes : List Thinker.DoorType
doorTypes =
    [ Thinker.NormalDoor, Thinker.Close30ThenOpen, Thinker.CloseDoor, Thinker.OpenDoor, Thinker.RaiseIn5Mins, Thinker.BlazeRaise, Thinker.BlazeOpen, Thinker.BlazeClose ]


floorTypes : List Thinker.FloorType
floorTypes =
    [ Thinker.LowerFloor
    , Thinker.LowerFloorToLowest
    , Thinker.TurboLower
    , Thinker.RaiseFloor
    , Thinker.RaiseFloorToNearest
    , Thinker.RaiseToTexture
    , Thinker.LowerAndChange
    , Thinker.RaiseFloor24
    , Thinker.RaiseFloor24AndChange
    , Thinker.RaiseFloorCrush
    , Thinker.RaiseFloorTurbo
    , Thinker.DonutRaise
    , Thinker.RaiseFloor512
    ]


platStatuses : List Thinker.PlatStatus
platStatuses =
    [ Thinker.PlatUp, Thinker.PlatDown, Thinker.PlatWaiting, Thinker.PlatInStasis ]


platTypes : List Thinker.PlatType
platTypes =
    [ Thinker.PerpetualRaise, Thinker.DownWaitUpStay, Thinker.RaiseAndChange, Thinker.RaiseToNearestAndChange, Thinker.BlazeDWUS ]


playerStates : List PlayerState
playerStates =
    [ Live, Dead, Reborn ]


indexIn : List a -> a -> Int
indexIn list x =
    let
        go i rest =
            case rest of
                y :: more ->
                    if y == x then
                        i

                    else
                        go (i + 1) more

                [] ->
                    0
    in
    go 0 list


atIndex : List a -> Int -> Maybe a
atIndex list i =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i list)



-- LOADING


{-| G\_DoLoadGame: load a save into the game. Fails (and the game should go on as it
was) if the version string isn't `version`, as the original then quietly does
nothing; fails if the file is cut short or holds something the original would stop
with an error on ("Bad savegame", an unknown thinker class), or that names a
sector, state or thing type that doesn't exist; and fails if the level can't be
loaded.
-}
load : Bytes -> Game -> Result String Game
load bytes game =
    case Decode.decode headerDecoder bytes of
        Nothing ->
            Err "Bad savegame: too short"

        Just header ->
            if header.version /= version then
                Err ("Not a savegame of this version (" ++ header.version ++ ")")

            else
                let
                    params =
                        game.params

                    world =
                        game.world

                    -- gameskill and playeringame are set before G_InitNew is called
                    skill =
                        Defs.skillFromIndex header.skill |> Maybe.withDefault Nightmare

                    started =
                        Game.initNew skill
                            header.episode
                            header.map
                            { game
                                | params = { params | skill = skill }
                                , world = { world | playeringame = Array.fromList header.playeringame }
                            }
                in
                case ( started.error, Game.playing started ) of
                    ( Nothing, Just ( env, fresh ) ) ->
                        Decode.decode (bodyDecoder env fresh) bytes
                            |> Result.fromMaybe "Bad savegame"
                            |> Result.andThen identity
                            |> Result.map (\body -> { started | world = unarchive env header body fresh })

                    ( Just problem, _ ) ->
                        Err problem

                    ( Nothing, Nothing ) ->
                        Err "The saved level couldn't be loaded"


type alias Header =
    { version : String
    , skill : Int
    , episode : Int
    , map : Int
    , playeringame : List Bool
    , leveltime : Int
    }


headerDecoder : Decoder Header
headerDecoder =
    Decode.map2 (\_ h -> h)
        (Decode.bytes saveStringSize)
        (Decode.map5
            (\v ( skill, episode, map ) pig ( a, b, c ) _ ->
                { version = v
                , skill = skill
                , episode = episode
                , map = map
                , playeringame = pig
                , leveltime = Bitwise.shiftLeftBy 16 a + Bitwise.shiftLeftBy 8 b + c
                }
            )
            (cString versionSize)
            (Decode.map3 (\a b c -> ( a, b, c )) Decode.unsignedInt8 Decode.unsignedInt8 Decode.unsignedInt8)
            (Decode.map4 (\a b c d -> List.map ((/=) 0) [ a, b, c, d ]) Decode.unsignedInt8 Decode.unsignedInt8 Decode.unsignedInt8 Decode.unsignedInt8)
            (Decode.map3 (\a b c -> ( a, b, c )) Decode.unsignedInt8 Decode.unsignedInt8 Decode.unsignedInt8)
            (Decode.succeed ())
        )


type alias SavedSector =
    { floorheight : Int
    , ceilingheight : Int
    , floorpic : Int
    , ceilingpic : Int
    , lightlevel : Int
    , special : Int
    , tag : Int
    }


{-| A line as saved: its flags and special (its tag can't change), and its sides.
-}
type alias SavedLine =
    { flags : Int
    , special : Int
    , sides : List World.SideState
    }


{-| Everything after the header, as read.
-}
type alias Body =
    { players : List ( Int, Player )
    , sectors : List SavedSector
    , lines : List SavedLine
    , mobjs : List Mobj
    , specials : List Thinker
    }


{-| Everything after the header, read with the offset kept so PADSAVEP can be
followed. Something wrong in a part (an `Err`) fails the load.
-}
bodyDecoder : Env -> World -> Decoder (Result String Body)
bodyDecoder env world =
    let
        inGame =
            List.indexedMap Tuple.pair (Array.toList world.playeringame)
                |> List.filter Tuple.second
                |> List.map Tuple.first

        numSectors =
            Array.length world.sectors
    in
    skip headerSize
        |> Decode.andThen (\_ -> playersDecoder inGame headerSize)
        |> Decode.andThen
            (\( players, afterPlayers ) ->
                worldDecoder env numSectors
                    |> Decode.andThen
                        (\( sectors, lines, worldSize ) ->
                            thinkersDecoder (afterPlayers + worldSize)
                                |> Decode.andThen
                                    (\( mobjs, afterThinkers ) ->
                                        Decode.map2
                                            (\specials marker ->
                                                if marker /= consistencyMarker then
                                                    Err "Bad savegame"

                                                else
                                                    Result.map3
                                                        (\ps ms ss ->
                                                            { players = ps, sectors = sectors, lines = lines, mobjs = ms, specials = ss }
                                                        )
                                                        (combine players)
                                                        (combine mobjs)
                                                        (combine specials)
                                            )
                                            (specialsDecoder numSectors afterThinkers)
                                            Decode.unsignedInt8
                                    )
                        )
            )


combine : List (Result String a) -> Result String (List a)
combine results =
    List.foldr (Result.map2 (::)) (Ok []) results


skip : Int -> Decoder ()
skip n =
    Decode.bytes n |> Decode.map (\_ -> ())


s16 : Decoder Int
s16 =
    Decode.signedInt16 LE


s32 : Decoder Int
s32 =
    Decode.signedInt32 LE


{-| `n` decoded in a row.
-}
repeat : Int -> Decoder a -> Decoder (List a)
repeat n decoder =
    Decode.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                Decode.succeed (Done (List.reverse acc))

            else
                decoder |> Decode.map (\v -> Loop ( left - 1, v :: acc ))
        )


{-| `n` 32-bit numbers, given to `f` as an array.
-}
fields : Int -> (Array Int -> a) -> Decoder a
fields n f =
    Decode.map (Array.fromList >> f) (repeat n s32)


at : Int -> Array Int -> Int
at i values =
    Array.get i values |> Maybe.withDefault 0


slice : Int -> Int -> Array Int -> Array Int
slice from n values =
    Array.slice from (from + n) values


{-| A struct after PADSAVEP: skip to the next multiple of 4 from `offset`, read it,
and give the offset after it (the struct is `size` bytes).
-}
padded : Int -> Int -> Decoder a -> Decoder ( a, Int )
padded offset size decoder =
    let
        start =
            offset + padding offset
    in
    skip (padding offset) |> Decode.andThen (\_ -> decoder) |> Decode.map (\a -> ( a, start + size ))



-- P_UnArchivePlayers


playersDecoder : List Int -> Int -> Decoder ( List (Result String ( Int, Player )), Int )
playersDecoder inGame offset0 =
    Decode.loop ( inGame, offset0, [] )
        (\( left, offset, acc ) ->
            case left of
                [] ->
                    Decode.succeed (Done ( List.reverse acc, offset ))

                n :: rest ->
                    padded offset playerSize playerDecoder
                        |> Decode.map (\( p, next ) -> Loop ( rest, next, Result.map (Tuple.pair n) p :: acc ))
        )


{-| player\_t: `mo` and `playerstate`, the ticcmd, 57 numbers (viewz to colormap),
the two weapon sprites and `didsecret`. `mo`, `message` and `attacker` are not
restored: the body is linked up when the things are read.
-}
playerDecoder : Decoder (Result String Player)
playerDecoder =
    Decode.map5
        (\state cmd v ( weaponSprite, flashSprite ) didsecret ->
            Result.map4
                (\playerstate readyweapon pendingweapon ( ws, fs ) ->
                    { mo = Nothing
                    , playerstate = playerstate
                    , cmd = cmd
                    , viewz = at 0 v
                    , viewheight = at 1 v
                    , deltaviewheight = at 2 v
                    , bob = at 3 v
                    , health = at 4 v
                    , armorpoints = at 5 v
                    , armortype = at 6 v
                    , powers = slice 7 6 v
                    , cards = slice 13 6 v |> Array.map ((/=) 0)
                    , backpack = at 19 v /= 0
                    , frags = slice 20 4 v
                    , readyweapon = readyweapon
                    , pendingweapon = pendingweapon
                    , weaponowned = slice 26 9 v |> Array.map ((/=) 0)
                    , ammo = slice 35 4 v
                    , maxammo = slice 39 4 v
                    , attackdown = at 43 v /= 0
                    , usedown = at 44 v /= 0
                    , cheats = at 45 v
                    , refire = at 46 v
                    , killcount = at 47 v
                    , itemcount = at 48 v
                    , secretcount = at 49 v
                    , message = Nothing
                    , damagecount = at 51 v
                    , bonuscount = at 52 v
                    , attacker = Nothing
                    , extralight = at 54 v
                    , fixedcolormap = at 55 v
                    , colormap = at 56 v
                    , weaponSprite = ws
                    , flashSprite = fs
                    , didsecret = didsecret /= 0
                    }
                )
                (atIndex playerStates state |> Result.fromMaybe "Bad player state")
                (Defs.weaponFromIndex (at 24 v) |> Result.fromMaybe "Bad weapon")
                (if at 25 v == wpNoChange then
                    Ok Nothing

                 else
                    Defs.weaponFromIndex (at 25 v) |> Maybe.map Just |> Result.fromMaybe "Bad weapon"
                )
                (Result.map2 Tuple.pair weaponSprite flashSprite)
        )
        (skip 4 |> Decode.andThen (\_ -> s32))
        ticcmdDecoder
        (fields 57 identity)
        (Decode.map2 Tuple.pair pspDecoder pspDecoder)
        s32


ticcmdDecoder : Decoder Ticcmd
ticcmdDecoder =
    Decode.map4
        (\( forwardmove, sidemove ) angleturn consistancy ( chatchar, buttons ) ->
            { forwardmove = forwardmove
            , sidemove = sidemove
            , angleturn = angleturn
            , consistancy = consistancy
            , chatchar = chatchar
            , buttons = buttons
            }
        )
        (Decode.map2 Tuple.pair Decode.signedInt8 Decode.signedInt8)
        s16
        s16
        (Decode.map2 Tuple.pair Decode.unsignedInt8 Decode.unsignedInt8)


pspDecoder : Decoder (Result String PspDef)
pspDecoder =
    fields 4
        (\v ->
            Result.map (\s -> { state = s, tics = at 1 v, sx = at 2 v, sy = at 3 v })
                (if at 0 v == 0 then
                    Ok Nothing

                 else
                    State.fromIndex (at 0 v) |> Maybe.map Just |> Result.fromMaybe "Bad weapon sprite state"
                )
        )



-- P_UnArchiveWorld


{-| The sectors and lines, and how many bytes they took.
-}
worldDecoder : Env -> Int -> Decoder ( List SavedSector, List SavedLine, Int )
worldDecoder env numSectors =
    let
        lines =
            Array.toList env.level.lines

        sideCount line =
            if line.backSide == Nothing then
                1

            else
                2

        size =
            2 * (7 * numSectors + List.sum (List.map (\l -> 3 + 5 * sideCount l) lines))

        sectorDecoder =
            repeat 7 s16
                |> Decode.map
                    (\v ->
                        let
                            a =
                                Array.fromList v
                        in
                        { floorheight = at 0 a
                        , ceilingheight = at 1 a
                        , floorpic = at 2 a
                        , ceilingpic = at 3 a
                        , lightlevel = at 4 a
                        , special = at 5 a
                        , tag = at 6 a
                        }
                    )

        sideDecoder =
            repeat 5 s16
                |> Decode.map
                    (\v ->
                        let
                            a =
                                Array.fromList v
                        in
                        { textureoffset = Bitwise.shiftLeftBy 16 (at 0 a)
                        , rowoffset = Bitwise.shiftLeftBy 16 (at 1 a)
                        , toptexture = at 2 a
                        , bottomtexture = at 3 a
                        , midtexture = at 4 a
                        }
                    )

        lineDecoder line =
            Decode.map4 (\flags special _ sides -> { flags = flags, special = special, sides = sides })
                s16
                s16
                s16
                (repeat (sideCount line) sideDecoder)
    in
    Decode.map2 (\sectors ls -> ( sectors, ls, size ))
        (repeat numSectors sectorDecoder)
        (Decode.loop ( lines, [] )
            (\( left, acc ) ->
                case left of
                    [] ->
                        Decode.succeed (Done (List.reverse acc))

                    line :: rest ->
                        lineDecoder line |> Decode.map (\l -> Loop ( rest, l :: acc ))
            )
        )



-- P_UnArchiveThinkers


thinkersDecoder : Int -> Decoder ( List (Result String Mobj), Int )
thinkersDecoder offset0 =
    Decode.loop ( offset0, [] )
        (\( offset, acc ) ->
            Decode.unsignedInt8
                |> Decode.andThen
                    (\tclass ->
                        if tclass == tcEnd then
                            Decode.succeed (Done ( List.reverse acc, offset + 1 ))

                        else if tclass == tcMobj then
                            padded (offset + 1) mobjSize mobjDecoder
                                |> Decode.map (\( m, next ) -> Loop ( next, m :: acc ))

                        else
                            -- I_Error ("Unknown tclass %i in savegame")
                            Decode.fail
                    )
        )


{-| mobj\_t: 35 numbers (the thinker links to `lastlook`), the spawn point, two bytes
of padding and `tracer`. `id`, `subsector`, `floorz` and `ceilingz` are filled in when
the thing is put back; `target` and `tracer` are gone.
-}
mobjDecoder : Decoder (Result String Mobj)
mobjDecoder =
    Decode.map3
        (\v spawnpoint _ ->
            Result.map3
                (\sprite type_ state ->
                    { id = 0
                    , x = at 3 v
                    , y = at 4 v
                    , z = at 5 v
                    , angle = Angle.wrap (at 8 v)
                    , sprite = sprite
                    , frame = at 10 v
                    , subsector = 0
                    , floorz = at 14 v
                    , ceilingz = at 15 v
                    , radius = at 16 v
                    , height = at 17 v
                    , momx = at 18 v
                    , momy = at 19 v
                    , momz = at 20 v
                    , validcount = at 21 v
                    , type_ = type_
                    , tics = at 24 v
                    , state = state
                    , flags = at 26 v
                    , health = at 27 v
                    , movedir = at 28 v
                    , movecount = at 29 v
                    , target = Nothing
                    , reactiontime = at 31 v
                    , threshold = at 32 v
                    , player =
                        if at 33 v == 0 then
                            Nothing

                        else
                            Just (at 33 v - 1)
                    , lastlook = at 34 v
                    , spawnpoint = spawnpoint
                    , tracer = Nothing
                    }
                )
                (Sprite.fromIndex (at 9 v) |> Result.fromMaybe "Bad sprite")
                (MobjType.fromIndex (at 22 v) |> Result.fromMaybe "Bad thing type")
                (State.fromIndex (at 25 v) |> Result.fromMaybe "Bad state")
                |> Result.andThen
                    (\m ->
                        case m.player of
                            Just n ->
                                if n < 0 || n >= Defs.maxPlayers then
                                    Err "Bad player number"

                                else
                                    Ok m

                            Nothing ->
                                Ok m
                    )
        )
        (fields 35 identity)
        (Decode.map5 MapThing s16 s16 s16 s16 s16)
        (skip 2 |> Decode.andThen (\_ -> s32))



-- P_UnArchiveSpecials


specialsDecoder : Int -> Int -> Decoder (List (Result String Thinker))
specialsDecoder numSectors offset0 =
    let
        sector i =
            if i >= 0 && i < numSectors then
                Ok i

            else
                Err "Bad sector number"

        enum list i =
            atIndex list i |> Result.fromMaybe "Bad special type"

        bool i =
            i /= 0

        ceiling =
            fields 12
                (\v ->
                    Result.map2
                        (\type_ sec ->
                            CeilingThinker
                                { type_ = type_
                                , sector = sec
                                , bottomheight = at 5 v
                                , topheight = at 6 v
                                , speed = at 7 v
                                , crush = bool (at 8 v)
                                , direction = at 9 v
                                , tag = at 10 v
                                , olddirection = at 11 v
                                }
                        )
                        (enum ceilingTypes (at 3 v))
                        (sector (at 4 v))
                )

        door =
            fields 10
                (\v ->
                    Result.map2
                        (\type_ sec ->
                            DoorThinker
                                { type_ = type_
                                , sector = sec
                                , topheight = at 5 v
                                , speed = at 6 v
                                , direction = at 7 v
                                , topwait = at 8 v
                                , topcountdown = at 9 v
                                }
                        )
                        (enum doorTypes (at 3 v))
                        (sector (at 4 v))
                )

        floor =
            Decode.map3
                (\v texture tail ->
                    Result.map2
                        (\type_ sec ->
                            FloorThinker
                                { type_ = type_
                                , crush = bool (at 4 v)
                                , sector = sec
                                , direction = at 6 v
                                , newspecial = at 7 v
                                , texture = texture
                                , floordestheight = at 0 tail
                                , speed = at 1 tail
                                }
                        )
                        (enum floorTypes (at 3 v))
                        (sector (at 5 v))
                )
                (fields 8 identity)
                (s16 |> Decode.andThen (\t -> skip 2 |> Decode.map (\_ -> t)))
                (fields 2 identity)

        plat =
            fields 14
                (\v ->
                    Result.map4
                        (\sec status oldstatus type_ ->
                            PlatThinker
                                { sector = sec
                                , speed = at 4 v
                                , low = at 5 v
                                , high = at 6 v
                                , wait = at 7 v
                                , count = at 8 v
                                , status = status
                                , oldstatus = oldstatus
                                , crush = bool (at 11 v)
                                , tag = at 12 v
                                , type_ = type_
                                }
                        )
                        (sector (at 3 v))
                        (enum platStatuses (at 9 v))
                        (enum platStatuses (at 10 v))
                        (enum platTypes (at 13 v))
                )

        flash =
            fields 9
                (\v ->
                    sector (at 3 v)
                        |> Result.map
                            (\sec ->
                                LightFlashThinker
                                    { sector = sec, count = at 4 v, maxlight = at 5 v, minlight = at 6 v, maxtime = at 7 v, mintime = at 8 v }
                            )
                )

        strobe =
            fields 9
                (\v ->
                    sector (at 3 v)
                        |> Result.map
                            (\sec ->
                                StrobeThinker
                                    { sector = sec, count = at 4 v, minlight = at 5 v, maxlight = at 6 v, darktime = at 7 v, brighttime = at 8 v }
                            )
                )

        glow =
            fields 7
                (\v ->
                    sector (at 3 v)
                        |> Result.map
                            (\sec -> GlowThinker { sector = sec, minlight = at 4 v, maxlight = at 5 v, direction = at 6 v })
                )

        classes =
            Dict.fromList
                [ ( tcCeiling, ( 48, ceiling ) )
                , ( tcDoor, ( 40, door ) )
                , ( tcFloor, ( 44, floor ) )
                , ( tcPlat, ( 56, plat ) )
                , ( tcFlash, ( 36, flash ) )
                , ( tcStrobe, ( 36, strobe ) )
                , ( tcGlow, ( 28, glow ) )
                ]
    in
    Decode.loop ( offset0, [] )
        (\( offset, acc ) ->
            Decode.unsignedInt8
                |> Decode.andThen
                    (\tclass ->
                        if tclass == tcEndSpecials then
                            Decode.succeed (Done (List.reverse acc))

                        else
                            case Dict.get tclass classes of
                                Just ( size, decoder ) ->
                                    padded (offset + 1) size decoder
                                        |> Decode.map (\( t, next ) -> Loop ( next, t :: acc ))

                                Nothing ->
                                    -- I_Error ("P_UnarchiveSpecials:Unknown tclass %i in savegame")
                                    Decode.fail
                    )
        )



-- PUTTING THE GAME BACK


{-| What G\_DoLoadGame does after G\_InitNew, in its order: `leveltime`, then the
players, the world, the things (throwing away the ones the level spawned) and the
specials.
-}
unarchive : Env -> Header -> Body -> World -> World
unarchive env header body world =
    { world | leveltime = header.leveltime }
        |> unarchivePlayers body.players
        |> unarchiveWorld env body
        |> unarchiveThinkers env body.mobjs
        |> unarchiveSpecials body.specials


unarchivePlayers : List ( Int, Player ) -> World -> World
unarchivePlayers players world =
    List.foldl (\( n, p ) w -> World.updatePlayer n (\_ -> p) w) world players


unarchiveWorld : Env -> Body -> World -> World
unarchiveWorld env body world =
    let
        sectors =
            List.map2
                (\saved sec ->
                    { sec
                        | floorheight = Bitwise.shiftLeftBy 16 saved.floorheight
                        , ceilingheight = Bitwise.shiftLeftBy 16 saved.ceilingheight
                        , floorpic = saved.floorpic
                        , ceilingpic = saved.ceilingpic
                        , lightlevel = saved.lightlevel
                        , special = saved.special
                        , tag = saved.tag
                        , specialdata = Nothing
                        , soundtarget = Nothing
                    }
                )
                body.sectors
                (Array.toList world.sectors)
                |> Array.fromList

        sideIds line =
            line.frontSide :: (line.backSide |> Maybe.map List.singleton |> Maybe.withDefault [])
    in
    List.map2 Tuple.pair body.lines (Array.toList env.level.lines)
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, ( saved, line ) ) w ->
                List.map2 Tuple.pair (sideIds line) saved.sides
                    |> List.foldl (\( sideId, side ) w2 -> World.updateSide sideId (\_ -> side) w2)
                        (World.updateLine i (\l -> { l | flags = saved.flags, special = saved.special }) w)
            )
            { world | sectors = sectors }


{-| Remove every thinker (things through P\_RemoveMobj), start the list afresh, and add
the saved things in order: each is linked into its sector and the blockmap
(P\_SetThingPosition), its floor and ceiling are its sector's, and a player's body is
linked to its player.
-}
unarchiveThinkers : Env -> List Mobj -> World -> World
unarchiveThinkers env mobjs world =
    let
        removed =
            Dict.foldl
                (\id t w ->
                    case t of
                        MobjThinker ->
                            PlayMobj.removeMobj env id w

                        _ ->
                            w
                )
                world
                world.thinkers

        add saved w =
            let
                ( id, withThinker ) =
                    World.addThinker MobjThinker w

                placed =
                    withThinker
                        |> World.setMobj { saved | id = id }
                        |> MapUtil.setThingPosition env id
                        |> settleOnSector env id
            in
            case saved.player of
                Just n ->
                    World.updatePlayer n (\p -> { p | mo = Just id }) placed

                Nothing ->
                    placed
    in
    List.foldl add { removed | thinkers = Dict.empty } mobjs


settleOnSector : Env -> ThinkerId -> World -> World
settleOnSector env id world =
    case World.mobj id world of
        Just m ->
            let
                sec =
                    World.sector (World.mobjSector env.level m) world
            in
            World.setMobj { m | floorz = sec.floorheight, ceilingz = sec.ceilingheight } world

        Nothing ->
            world


{-| Add each special; a moving floor, ceiling, door or lift becomes its sector's
`specialdata`, and ceilings and lifts go in the first free slot of their active list.
-}
unarchiveSpecials : List Thinker -> World -> World
unarchiveSpecials specials world =
    List.foldl
        (\t w ->
            let
                ( id, added ) =
                    World.addThinker t w

                claim sec =
                    World.updateSector sec (\s -> { s | specialdata = Just id })
            in
            case t of
                CeilingThinker c ->
                    added |> claim c.sector |> (\x -> { x | activeCeilings = intoFirstFree id x.activeCeilings })

                DoorThinker d ->
                    claim d.sector added

                FloorThinker f ->
                    claim f.sector added

                PlatThinker p ->
                    added |> claim p.sector |> (\x -> { x | activePlats = intoFirstFree id x.activePlats })

                _ ->
                    added
        )
        world
        specials


{-| P\_AddActiveCeiling and P\_AddActivePlat: the first free slot (none if all are
taken).
-}
intoFirstFree : ThinkerId -> Array (Maybe ThinkerId) -> Array (Maybe ThinkerId)
intoFirstFree id slots =
    let
        search i =
            case Array.get i slots of
                Just Nothing ->
                    Array.set i (Just id) slots

                Just (Just _) ->
                    search (i + 1)

                Nothing ->
                    slots
    in
    search 0
