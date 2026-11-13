module Doom.World exposing
    ( World, SectorState, LineState, SideState, Button, ButtonWhere(..), ItemRespawn, Exit(..)
    , Scratch, Divline, emptyScratch, maxButtons, itemQueSize
    , mobj, setMobj, updateMobj, mobjSector
    , sector, updateSector, lineState, updateLine, sideState, updateSide
    , player, updatePlayer, playerMobj
    , addThinker, thinker, setThinker, removeThinker
    , pRandom, startSound, stopSound
    , nextValidcount, setScratch
    , BlockmapHeader
    )

{-| Everything about a level that changes while it is played: the thinkers and things,
sector heights and lights, line specials, switch textures, the players, the random
number index, and the C source's file-level variables, so that the game is one value
that one tic turns into the next.

Static map data is in `Doom.Level`; `sectors`, `lines` and `sides` here are indexed
the same way as there.


# The world

@docs World, SectorState, LineState, SideState, Button, ButtonWhere, ItemRespawn, Exit
@docs Scratch, Divline, emptyScratch, maxButtons, itemQueSize


# Things

@docs mobj, setMobj, updateMobj, mobjSector


# Sectors, lines and sides

@docs sector, updateSector, lineState, updateLine, sideState, updateSide


# Players

@docs player, updatePlayer, playerMobj


# Thinkers

@docs addThinker, thinker, setThinker, removeThinker


# Randomness, sound and the rest

@docs pRandom, startSound, stopSound
@docs nextValidcount, setScratch

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Doom.Fixed exposing (Fixed)
import Doom.Info.Sfx exposing (Sfx)
import Doom.Level as Level exposing (Level, LineId, MapThing, SectorId, SideId)
import Doom.Mobj exposing (Mobj, ThinkerId)
import Doom.Player exposing (Player)
import Doom.Random as Random exposing (Rng)
import Doom.Sound as Sound
import Doom.Thinker exposing (Thinker(..))


{-| The changing state of a level.

  - `thinkers`: every thinker, by id; P\_RunThinkers runs them in id order, which is
    the order the C source's thinker list has them in.
  - `mobjs`: every thing's data, by id. A removed thing's thinker becomes `Removed`
    and is later dropped from `thinkers`, but its data stays here until the level
    ends, so a stale `target` still reads what C's freed memory would.
  - `blocklinks`: for each blockmap cell, the things in it, most recently linked
    first (P\_SetThingPosition pushes on the front, as C's `blocklinks` does).
  - `sectorThings`: each sector's thing list, likewise.
  - `activePlats`, `activeCeilings`: C's `activeplats[]` and `activeceilings[]`.
  - `exit`: set when something ends the level (G\_ExitLevel, G\_SecretExitLevel).
  - `sounds`: sounds asked for this tic, newest first.
  - `validcount`: C's global `validcount`.
  - `scratch`: the file-level variables p\_map.c, p\_maputl.c and p\_sight.c pass
    results through.
  - `bmap`: p\_setup.c's `bmaporgx`, `bmaporgy`, `bmapwidth`, `bmapheight`, as loaded
    from the map's blockmap, unless an intercepts overrun (MapUtil.pathTraverse) has
    since written over them as the DOS executable's does.
  - `gametic`: g\_game.c's `gametic`, the number of tics the game has run since it
    started (a new level doesn't reset it). A\_Tracer reads it. Whoever runs the tic
    loop keeps it: it is the number of the tic being run, going up by one after each
    tic, as TryRunTics does after G\_Ticker.

-}
type alias World =
    { leveltime : Int
    , rng : Rng
    , validcount : Int
    , thinkers : Dict ThinkerId Thinker
    , mobjs : Dict ThinkerId Mobj
    , nextThinkerId : Int
    , sectors : Array SectorState
    , lines : Array LineState
    , sides : Array SideState
    , blocklinks : Array (List ThinkerId)
    , sectorThings : Array (List ThinkerId)
    , players : Array Player
    , playeringame : Array Bool
    , playerStarts : Array (Maybe MapThing)
    , deathmatchStarts : List MapThing
    , activePlats : Array (Maybe ThinkerId)
    , activeCeilings : Array (Maybe ThinkerId)
    , buttons : Array (Maybe Button)
    , itemRespawn : ItemRespawn
    , brainTargets : Array ThinkerId
    , brainTargetOn : Int
    , brainEasy : Bool
    , lineSpecialList : List LineId
    , totalkills : Int
    , totalitems : Int
    , totalsecret : Int
    , exit : Maybe Exit
    , sounds : List Sound.Event
    , scratch : Scratch
    , bmap : BlockmapHeader
    , gametic : Int
    }


{-| The blockmap's origin and size in cells.
-}
type alias BlockmapHeader =
    { orgx : Fixed
    , orgy : Fixed
    , width : Int
    , height : Int
    }


{-| The parts of sector\_t that change. `floorpic`/`ceilingpic` are flat numbers;
`soundtarget` the thing that last made a noise monsters in the sector heard;
`specialdata` the thinker moving its floor or ceiling, if one is.
-}
type alias SectorState =
    { floorheight : Fixed
    , ceilingheight : Fixed
    , floorpic : Int
    , ceilingpic : Int
    , lightlevel : Int
    , special : Int
    , tag : Int
    , soundtraversed : Int
    , soundtarget : Maybe ThinkerId
    , validcount : Int
    , specialdata : Maybe ThinkerId
    }


{-| The parts of line\_t that change: `flags` (ML\_MAPPED gets set), `special`
(cleared once a one-time special has run), `validcount`.
-}
type alias LineState =
    { flags : Int
    , special : Int
    , validcount : Int
    }


{-| The parts of side\_t that change: offsets (scrolling walls) and texture numbers
(switches).
-}
type alias SideState =
    { textureoffset : Fixed
    , rowoffset : Fixed
    , toptexture : Int
    , bottomtexture : Int
    , midtexture : Int
    }


{-| button\_t: a switch waiting to flip back. `soundorg` is the sector whose middle
its click comes from.
-}
type alias Button =
    { line : LineId
    , where_ : ButtonWhere
    , btexture : Int
    , btimer : Int
    , soundorg : SectorId
    }


{-| bwhere\_e: which texture of the line the switch is.
-}
type ButtonWhere
    = ButtonTop
    | ButtonMiddle
    | ButtonBottom


{-| The deathmatch item respawn queue (p\_mobj.c's itemrespawnque).
-}
type alias ItemRespawn =
    { things : Array MapThing
    , times : Array Int
    , head : Int
    , tail : Int
    }


{-| How the level ended.
-}
type Exit
    = NormalExit
    | SecretExit


{-| divline\_t.
-}
type alias Divline =
    { x : Fixed
    , y : Fixed
    , dx : Fixed
    , dy : Fixed
    }


{-| The C source's file-level variables that functions leave results in for other
functions to read (named as there):

  - p\_map.c: `tmbbox`, `tmthing`, `tmflags`, `tmx`, `tmy`, `floatok`, `tmfloorz`,
    `tmceilingz`, `tmdropoffz`, `ceilingline`, `spechit` (in the order added),
    the sliding-move variables, the attack variables (`linetarget`, `shootthing`,
    `shootz`, `laDamage`, `attackrange`, `aimslope`, `aimTopslope`,
    `aimBottomslope`), `bombspot`, `bombsource`, `bombdamage`, `crushchange`,
    `nofit`, `usething`.
  - p\_maputl.c: `opentop`, `openbottom`, `openrange`, `lowfloor`, `trace`,
    `earlyout`, `ptflags`.
  - p\_sight.c: `sightzstart`, `sightTopslope`, `sightBottomslope`, `strace`, `t2x`,
    `t2y`.
  - p\_enemy.c: `soundtarget`, `vileobj`, `viletryx`, `viletryy`, `corpsehit`
    (listed last).
  - p\_pspr.c: `swingx`, `swingy`, `bulletslope`.
  - p\_user.c: `onground`.

-}
type alias Scratch =
    { tmbbox : Level.Box
    , tmthing : Maybe ThinkerId
    , tmflags : Int
    , tmx : Fixed
    , tmy : Fixed
    , floatok : Bool
    , tmfloorz : Fixed
    , tmceilingz : Fixed
    , tmdropoffz : Fixed
    , ceilingline : Maybe LineId
    , spechit : List LineId
    , bestslidefrac : Fixed
    , secondslidefrac : Fixed
    , bestslideline : Maybe LineId
    , secondslideline : Maybe LineId
    , slidemo : Maybe ThinkerId
    , tmxmove : Fixed
    , tmymove : Fixed
    , linetarget : Maybe ThinkerId
    , shootthing : Maybe ThinkerId
    , shootz : Fixed
    , laDamage : Int
    , attackrange : Fixed
    , aimslope : Fixed
    , aimTopslope : Fixed
    , aimBottomslope : Fixed
    , bombspot : Maybe ThinkerId
    , bombsource : Maybe ThinkerId
    , bombdamage : Int
    , crushchange : Bool
    , nofit : Bool
    , usething : Maybe ThinkerId
    , opentop : Fixed
    , openbottom : Fixed
    , openrange : Fixed
    , lowfloor : Fixed
    , trace : Divline
    , earlyout : Bool
    , ptflags : Int
    , sightzstart : Fixed
    , sightTopslope : Fixed
    , sightBottomslope : Fixed
    , strace : Divline
    , t2x : Fixed
    , t2y : Fixed
    , soundtarget : Maybe ThinkerId
    , vileobj : Maybe ThinkerId
    , viletryx : Fixed
    , viletryy : Fixed
    , swingx : Fixed
    , swingy : Fixed
    , bulletslope : Fixed
    , onground : Bool
    , corpsehit : Maybe ThinkerId
    }


{-| All zero, as C's statics start.
-}
emptyScratch : Scratch
emptyScratch =
    let
        noLine =
            { x = 0, y = 0, dx = 0, dy = 0 }
    in
    { tmbbox = { top = 0, bottom = 0, left = 0, right = 0 }
    , tmthing = Nothing
    , tmflags = 0
    , tmx = 0
    , tmy = 0
    , floatok = False
    , tmfloorz = 0
    , tmceilingz = 0
    , tmdropoffz = 0
    , ceilingline = Nothing
    , spechit = []
    , bestslidefrac = 0
    , secondslidefrac = 0
    , bestslideline = Nothing
    , secondslideline = Nothing
    , slidemo = Nothing
    , tmxmove = 0
    , tmymove = 0
    , linetarget = Nothing
    , shootthing = Nothing
    , shootz = 0
    , laDamage = 0
    , attackrange = 0
    , aimslope = 0
    , aimTopslope = 0
    , aimBottomslope = 0
    , bombspot = Nothing
    , bombsource = Nothing
    , bombdamage = 0
    , crushchange = False
    , nofit = False
    , usething = Nothing
    , opentop = 0
    , openbottom = 0
    , openrange = 0
    , lowfloor = 0
    , trace = noLine
    , earlyout = False
    , ptflags = 0
    , sightzstart = 0
    , sightTopslope = 0
    , sightBottomslope = 0
    , strace = noLine
    , t2x = 0
    , t2y = 0
    , soundtarget = Nothing
    , vileobj = Nothing
    , viletryx = 0
    , viletryy = 0
    , swingx = 0
    , swingy = 0
    , bulletslope = 0
    , onground = False
    , corpsehit = Nothing
    }


{-| MAXBUTTONS: the size of `buttons`.
-}
maxButtons : Int
maxButtons =
    16


{-| ITEMQUESIZE: the size of the item respawn queue.
-}
itemQueSize : Int
itemQueSize =
    128



-- THINGS


{-| The thing with this id, if there is (or was) one.
-}
mobj : ThinkerId -> World -> Maybe Mobj
mobj id world =
    Dict.get id world.mobjs


{-| Store a thing (under its own id).
-}
setMobj : Mobj -> World -> World
setMobj m world =
    { world | mobjs = Dict.insert m.id m world.mobjs }


{-| Change the thing with this id, if there is one.
-}
updateMobj : ThinkerId -> (Mobj -> Mobj) -> World -> World
updateMobj id f world =
    { world | mobjs = Dict.update id (Maybe.map f) world.mobjs }


{-| The sector a thing is in (`mobj->subsector->sector`).
-}
mobjSector : Level -> Mobj -> SectorId
mobjSector level m =
    (Level.subsector m.subsector level).sector



-- SECTORS, LINES, SIDES


{-| A sector's changing state.
-}
sector : SectorId -> World -> SectorState
sector i world =
    Array.get i world.sectors
        |> Maybe.withDefault
            { floorheight = 0
            , ceilingheight = 0
            , floorpic = 0
            , ceilingpic = 0
            , lightlevel = 0
            , special = 0
            , tag = 0
            , soundtraversed = 0
            , soundtarget = Nothing
            , validcount = 0
            , specialdata = Nothing
            }


{-| Change a sector's state.
-}
updateSector : SectorId -> (SectorState -> SectorState) -> World -> World
updateSector i f world =
    case Array.get i world.sectors of
        Just s ->
            { world | sectors = Array.set i (f s) world.sectors }

        Nothing ->
            world


{-| A line's changing state.
-}
lineState : LineId -> World -> LineState
lineState i world =
    Array.get i world.lines |> Maybe.withDefault { flags = 0, special = 0, validcount = 0 }


{-| Change a line's state.
-}
updateLine : LineId -> (LineState -> LineState) -> World -> World
updateLine i f world =
    case Array.get i world.lines of
        Just l ->
            { world | lines = Array.set i (f l) world.lines }

        Nothing ->
            world


{-| A side's changing state.
-}
sideState : SideId -> World -> SideState
sideState i world =
    Array.get i world.sides
        |> Maybe.withDefault { textureoffset = 0, rowoffset = 0, toptexture = 0, bottomtexture = 0, midtexture = 0 }


{-| Change a side's state.
-}
updateSide : SideId -> (SideState -> SideState) -> World -> World
updateSide i f world =
    case Array.get i world.sides of
        Just s ->
            { world | sides = Array.set i (f s) world.sides }

        Nothing ->
            world



-- PLAYERS


{-| Player n (0 to 3).
-}
player : Int -> World -> Maybe Player
player n world =
    Array.get n world.players


{-| Change player n.
-}
updatePlayer : Int -> (Player -> Player) -> World -> World
updatePlayer n f world =
    case Array.get n world.players of
        Just p ->
            { world | players = Array.set n (f p) world.players }

        Nothing ->
            world


{-| Player n's body.
-}
playerMobj : Int -> World -> Maybe Mobj
playerMobj n world =
    player n world |> Maybe.andThen .mo |> Maybe.andThen (\id -> mobj id world)



-- THINKERS


{-| P\_AddThinker: add a thinker at the end of the list, giving its id.
-}
addThinker : Thinker -> World -> ( ThinkerId, World )
addThinker t world =
    let
        id =
            world.nextThinkerId
    in
    ( id
    , { world
        | thinkers = Dict.insert id t world.thinkers
        , nextThinkerId = id + 1
      }
    )


{-| The thinker with this id, if it is still in the list.
-}
thinker : ThinkerId -> World -> Maybe Thinker
thinker id world =
    Dict.get id world.thinkers


{-| Replace a thinker's state (it keeps its place in the list).
-}
setThinker : ThinkerId -> Thinker -> World -> World
setThinker id t world =
    if Dict.member id world.thinkers then
        { world | thinkers = Dict.insert id t world.thinkers }

    else
        world


{-| P\_RemoveThinker: mark it removed; P\_RunThinkers drops it when it gets to it.
-}
removeThinker : ThinkerId -> World -> World
removeThinker id world =
    setThinker id Removed world



-- RANDOMNESS, SOUND


{-| P\_Random: the game's next random number, 0 to 255.
-}
pRandom : World -> ( Int, World )
pRandom world =
    let
        ( n, rng ) =
            Random.pRandom world.rng
    in
    ( n, { world | rng = rng } )


{-| S\_StartSound.
-}
startSound : Sound.Origin -> Sfx -> World -> World
startSound origin sfx world =
    { world | sounds = Sound.Start origin sfx :: world.sounds }


{-| S\_StopSound.
-}
stopSound : ThinkerId -> World -> World
stopSound id world =
    { world | sounds = Sound.Stop id :: world.sounds }


{-| `validcount++`, giving the new value.
-}
nextValidcount : World -> ( Int, World )
nextValidcount world =
    ( world.validcount + 1, { world | validcount = world.validcount + 1 } )


{-| Change the scratch variables.
-}
setScratch : (Scratch -> Scratch) -> World -> World
setScratch f world =
    { world | scratch = f world.scratch }
