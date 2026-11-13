module Doom.Automap exposing
    ( Model, init, active, start, stop, stopOnDeath, deactivate, setSingleDemo
    , ticker, responder, drawer
    )

{-| The automap (am\_map.c): the map drawn as lines over the top 168 rows of the
screen, with the player's arrow, optional grid, marks, and the iddt cheat.


# What the caller does

These follow the C source's split of the work:

  - `responder` is AM\_Responder, called from G\_Responder in a level after the chat
    and the status bar have passed on the event. Tab starts the automap when it is
    off (AM\_Start, and the C sets `viewactive` false) and stops it when it is on
    (AM\_Stop, `viewactive` true): it does both itself. Whenever `active` changes,
    tell the status bar: AM\_Start and AM\_Stop send it AM\_MSGENTERED and
    AM\_MSGEXITED through ST\_Responder.
  - G\_DoCompleted calls `stop` when the automap is showing, and then (like
    G\_InitNew) clears `automapactive` by hand: that is `deactivate`, which doesn't
    mark the automap stopped, so the next AM\_Start calls AM\_Stop first, as in C.
  - P\_KillMobj stops the automap when the console player dies: after P\_Ticker,
    call `stopOnDeath` with the worlds before and after it.
  - `ticker` is AM\_Ticker, run in G\_Ticker after ST\_Ticker.
  - `drawer` is AM\_Drawer, run by D\_Display before the status bar when `active`.
  - The `Int` given to `start`, `ticker`, `responder` and `drawer` is
    `consoleplayer`.

The lines the player has seen carry ML\_MAPPED in `World.lines`; the renderer sets
it, the automap only reads it.

@docs Model, init, active, start, stop, stopOnDeath, deactivate, setSingleDemo
@docs ticker, responder, drawer

-}

import Array exposing (Array)
import Bitwise
import Doom.Angle as Angle exposing (Angle)
import Doom.Defs as Defs
import Doom.Event exposing (Event(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Flags as Flags
import Doom.Int32 as Int32
import Doom.Level as Level
import Doom.Mobj exposing (Mobj)
import Doom.Play.Env exposing (Env)
import Doom.Play.MapUtil as MapUtil
import Doom.Player as Player
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (World)



-- MODEL


{-| The automap's state: am\_map.c's file-level variables (and the statics inside its
functions), named as there.
-}
type Model
    = Model State


type alias Point =
    { x : Fixed
    , y : Fixed
    }


type alias State =
    { automapactive : Bool
    , stopped : Bool
    , cheating : Int
    , grid : Bool
    , followplayer : Bool
    , bigstate : Bool
    , cheatPos : Int
    , lastLevel : Maybe ( Int, Int )
    , singledemo : Bool
    , paninc : Point
    , mtofZoommul : Fixed
    , ftomZoommul : Fixed
    , mx : Fixed
    , my : Fixed
    , mx2 : Fixed
    , my2 : Fixed
    , mw : Fixed
    , mh : Fixed
    , minX : Fixed
    , minY : Fixed
    , maxX : Fixed
    , maxY : Fixed
    , minScaleMtof : Fixed
    , maxScaleMtof : Fixed
    , oldMx : Fixed
    , oldMy : Fixed
    , oldMw : Fixed
    , oldMh : Fixed
    , oldloc : Point
    , scaleMtof : Fixed
    , scaleFtom : Fixed
    , plr : Int
    , marknums : Array (Maybe Patch)
    , markpoints : Array Point
    , markpointnum : Int
    }


{-| Load the mark number patches (AMMNUM0 to AMMNUM9) and start with the C source's
initial values: following the player, no grid, no cheat, never started.
-}
init : Wad -> Model
init wad =
    Model
        { automapactive = False
        , stopped = True
        , cheating = 0
        , grid = False
        , followplayer = True
        , bigstate = False
        , cheatPos = 0
        , lastLevel = Nothing
        , singledemo = False
        , paninc = { x = 0, y = 0 }
        , mtofZoommul = 0
        , ftomZoommul = 0
        , mx = 0
        , my = 0
        , mx2 = 0
        , my2 = 0
        , mw = 0
        , mh = 0
        , minX = 0
        , minY = 0
        , maxX = 0
        , maxY = 0
        , minScaleMtof = 0
        , maxScaleMtof = 0
        , oldMx = 0
        , oldMy = 0
        , oldMw = 0
        , oldMh = 0
        , oldloc = { x = 0, y = 0 }
        , scaleMtof = 13107
        , scaleFtom = 0
        , plr = 0
        , marknums =
            List.range 0 9
                |> List.map (\i -> Wad.lump ("AMMNUM" ++ String.fromInt i) wad |> Maybe.andThen Video.patch)
                |> Array.fromList
        , markpoints = Array.repeat numMarkPoints { x = 0, y = 0 }
        , markpointnum = 0
        }


{-| `automapactive`.
-}
active : Model -> Bool
active (Model s) =
    s.automapactive


{-| d\_main.c's `singledemo` (a demo played with -playdemo or -timedemo): in a
deathmatch demo the automap then shows every player, not just the one it follows.
-}
setSingleDemo : Bool -> Model -> Model
setSingleDemo b (Model s) =
    Model { s | singledemo = b }


{-| P\_KillMobj's AM\_Stop: "don't die in auto map". Given the world before and after
P\_Ticker and the console player: if the automap is showing and the console player
has just died (their `playerstate` became `Dead` during the tic), stop it. Run it
after P\_Ticker and before `ticker`, as the C source stops the automap inside
P\_Ticker.
-}
stopOnDeath : World -> World -> Int -> Model -> Model
stopOnDeath before after consoleplayer (Model s) =
    let
        dead w =
            World.player consoleplayer w |> Maybe.map (\p -> p.playerstate == Defs.Dead) |> Maybe.withDefault False
    in
    if s.automapactive && dead after && not (dead before) then
        Model (stopState s)

    else
        Model s


{-| G\_InitNew's and G\_DoCompleted's `automapactive = false`, without AM\_Stop.
-}
deactivate : Model -> Model
deactivate (Model s) =
    Model { s | automapactive = False }



-- CONSTANTS


fX : Int
fX =
    0


fY : Int
fY =
    0


fW : Int
fW =
    Video.width


fH : Int
fH =
    Video.height - 32


numMarkPoints : Int
numMarkPoints =
    10


playerRadius : Fixed
playerRadius =
    16 * Fixed.fracunit


{-| M\_ZOOMIN: (int) (1.02 \* FRACUNIT).
-}
zoomIn : Fixed
zoomIn =
    66846


{-| M\_ZOOMOUT: (int) (FRACUNIT / 1.02).
-}
zoomOut : Fixed
zoomOut =
    64250


fPaninc : Int
fPaninc =
    4


reds : Int
reds =
    256 - 5 * 16


grays : Int
grays =
    6 * 16


browns : Int
browns =
    4 * 16


greens : Int
greens =
    7 * 16


yellows : Int
yellows =
    256 - 32 + 7


white : Int
white =
    256 - 47


{-| `lightlev`: AM\_updateLightLev is commented out in AM\_Ticker, so it stays 0.
-}
lightlev : Int
lightlev =
    0



-- COORDINATES


{-| FTOM: frame-buffer distance to map distance.
-}
ftom : State -> Int -> Fixed
ftom s x =
    Fixed.mul (Int32.shiftLeft 16 x) s.scaleFtom


{-| MTOF: map distance to frame-buffer distance.
-}
mtof : State -> Fixed -> Int
mtof s x =
    Int32.shiftRight 16 (Fixed.mul x s.scaleMtof)


cxmtof : State -> Fixed -> Int
cxmtof s x =
    Int32.add fX (mtof s (Int32.sub x s.mx))


cymtof : State -> Fixed -> Int
cymtof s y =
    Int32.add fY (Int32.sub fH (mtof s (Int32.sub y s.my)))


{-| C's `/` on ints: truncates towards zero.
-}
cdiv : Int -> Int -> Int
cdiv n d =
    if d == 0 then
        0

    else
        Int32.wrap (n // d)


half : Int -> Int
half n =
    cdiv n 2



-- SCALE AND WINDOW


activateNewScale : State -> State
activateNewScale s =
    let
        cx =
            Int32.add s.mx (half s.mw)

        cy =
            Int32.add s.my (half s.mh)

        w =
            ftom s fW

        h =
            ftom s fH

        x =
            Int32.sub cx (half w)

        y =
            Int32.sub cy (half h)
    in
    { s | mw = w, mh = h, mx = x, my = y, mx2 = Int32.add x w, my2 = Int32.add y h }


saveScaleAndLoc : State -> State
saveScaleAndLoc s =
    { s | oldMx = s.mx, oldMy = s.my, oldMw = s.mw, oldMh = s.mh }


restoreScaleAndLoc : World -> State -> State
restoreScaleAndLoc world s =
    let
        ( x, y ) =
            if not s.followplayer then
                ( s.oldMx, s.oldMy )

            else
                case World.playerMobj s.plr world of
                    Just mo ->
                        ( Int32.sub mo.x (half s.oldMw), Int32.sub mo.y (half s.oldMh) )

                    Nothing ->
                        ( s.mx, s.my )

        scale =
            Fixed.div (Int32.shiftLeft Fixed.fracbits fW) s.oldMw
    in
    { s
        | mw = s.oldMw
        , mh = s.oldMh
        , mx = x
        , my = y
        , mx2 = Int32.add x s.oldMw
        , my2 = Int32.add y s.oldMh
        , scaleMtof = scale
        , scaleFtom = Fixed.div Fixed.fracunit scale
    }


addMark : State -> State
addMark s =
    { s
        | markpoints =
            Array.set s.markpointnum
                { x = Int32.add s.mx (half s.mw), y = Int32.add s.my (half s.mh) }
                s.markpoints
        , markpointnum = modBy numMarkPoints (s.markpointnum + 1)
    }


clearMarks : State -> State
clearMarks s =
    { s | markpoints = Array.repeat numMarkPoints { x = -1, y = 0 }, markpointnum = 0 }


{-| AM\_findMinMaxBoundaries, with its `else if`s: a vertex that lowers the minimum
isn't checked against the maximum.
-}
findMinMaxBoundaries : Level.Level -> State -> State
findMinMaxBoundaries level s =
    let
        b =
            Array.foldl
                (\v acc ->
                    let
                        ( minX, maxX ) =
                            if v.x < acc.minX then
                                ( v.x, acc.maxX )

                            else if v.x > acc.maxX then
                                ( acc.minX, v.x )

                            else
                                ( acc.minX, acc.maxX )

                        ( minY, maxY ) =
                            if v.y < acc.minY then
                                ( v.y, acc.maxY )

                            else if v.y > acc.maxY then
                                ( acc.minY, v.y )

                            else
                                ( acc.minY, acc.maxY )
                    in
                    { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
                )
                { minX = Fixed.maxInt, minY = Fixed.maxInt, maxX = -Fixed.maxInt, maxY = -Fixed.maxInt }
                level.vertexes

        a =
            Fixed.div (Int32.shiftLeft Fixed.fracbits fW) (Int32.sub b.maxX b.minX)

        c =
            Fixed.div (Int32.shiftLeft Fixed.fracbits fH) (Int32.sub b.maxY b.minY)
    in
    { s
        | minX = b.minX
        , minY = b.minY
        , maxX = b.maxX
        , maxY = b.maxY
        , minScaleMtof = Basics.min a c
        , maxScaleMtof = Fixed.div (Int32.shiftLeft Fixed.fracbits fH) (2 * playerRadius)
    }


changeWindowLoc : State -> State
changeWindowLoc s0 =
    let
        s =
            if s0.paninc.x /= 0 || s0.paninc.y /= 0 then
                { s0 | followplayer = False, oldloc = { x = Fixed.maxInt, y = s0.oldloc.y } }

            else
                s0

        x1 =
            Int32.add s.mx s.paninc.x

        y1 =
            Int32.add s.my s.paninc.y

        x =
            if Int32.add x1 (half s.mw) > s.maxX then
                Int32.sub s.maxX (half s.mw)

            else if Int32.add x1 (half s.mw) < s.minX then
                Int32.sub s.minX (half s.mw)

            else
                x1

        y =
            if Int32.add y1 (half s.mh) > s.maxY then
                Int32.sub s.maxY (half s.mh)

            else if Int32.add y1 (half s.mh) < s.minY then
                Int32.sub s.minY (half s.mh)

            else
                y1
    in
    { s | mx = x, my = y, mx2 = Int32.add x s.mw, my2 = Int32.add y s.mh }


initVariables : World -> Int -> State -> State
initVariables world consoleplayer s0 =
    let
        inGame n =
            Array.get n world.playeringame |> Maybe.withDefault False

        pnum =
            if inGame consoleplayer then
                consoleplayer

            else
                List.range 0 (Defs.maxPlayers - 1)
                    |> List.filter inGame
                    |> List.head
                    |> Maybe.withDefault Defs.maxPlayers

        s1 =
            { s0
                | automapactive = True
                , oldloc = { x = Fixed.maxInt, y = s0.oldloc.y }
                , paninc = { x = 0, y = 0 }
                , ftomZoommul = Fixed.fracunit
                , mtofZoommul = Fixed.fracunit
                , mw = ftom s0 fW
                , mh = ftom s0 fH
                , plr = pnum
            }

        ( px, py ) =
            World.playerMobj pnum world
                |> Maybe.map (\mo -> ( mo.x, mo.y ))
                |> Maybe.withDefault ( 0, 0 )

        s2 =
            changeWindowLoc { s1 | mx = Int32.sub px (half s1.mw), my = Int32.sub py (half s1.mh) }
    in
    saveScaleAndLoc s2


levelInit : Env -> State -> State
levelInit env s0 =
    let
        s =
            findMinMaxBoundaries env.level (clearMarks s0)

        scale0 =
            Fixed.div s.minScaleMtof 45875

        scale =
            if scale0 > s.maxScaleMtof then
                s.minScaleMtof

            else
                scale0
    in
    { s | scaleMtof = scale, scaleFtom = Fixed.div Fixed.fracunit scale }


stopState : State -> State
stopState s =
    { s | automapactive = False, stopped = True }


startState : Env -> World -> Int -> State -> State
startState env world consoleplayer s0 =
    let
        s1 =
            if s0.stopped then
                s0

            else
                stopState s0

        here =
            ( env.params.episode, env.params.map )

        s2 =
            if s1.lastLevel /= Just here then
                levelInit env { s1 | stopped = False, lastLevel = Just here }

            else
                { s1 | stopped = False }
    in
    initVariables world consoleplayer s2


{-| AM\_Start: show the automap, centred on the console player (or the first player
in the game). On a different map than last time it first runs AM\_LevelInit: marks
cleared, zoom limits from the map's size, the starting scale.
-}
start : Env -> World -> Int -> Model -> Model
start env world consoleplayer (Model s) =
    Model (startState env world consoleplayer s)


{-| AM\_Stop.
-}
stop : Model -> Model
stop (Model s) =
    Model (stopState s)


minOutWindowScale : State -> State
minOutWindowScale s =
    activateNewScale { s | scaleMtof = s.minScaleMtof, scaleFtom = Fixed.div Fixed.fracunit s.minScaleMtof }


maxOutWindowScale : State -> State
maxOutWindowScale s =
    activateNewScale { s | scaleMtof = s.maxScaleMtof, scaleFtom = Fixed.div Fixed.fracunit s.maxScaleMtof }


changeWindowScale : State -> State
changeWindowScale s0 =
    let
        scale =
            Fixed.mul s0.scaleMtof s0.mtofZoommul

        s =
            { s0 | scaleMtof = scale, scaleFtom = Fixed.div Fixed.fracunit scale }
    in
    if scale < s.minScaleMtof then
        minOutWindowScale s

    else if scale > s.maxScaleMtof then
        maxOutWindowScale s

    else
        activateNewScale s


doFollowPlayer : World -> State -> State
doFollowPlayer world s =
    case World.playerMobj s.plr world of
        Just mo ->
            if s.oldloc.x /= mo.x || s.oldloc.y /= mo.y then
                let
                    x =
                        Int32.sub (ftom s (mtof s mo.x)) (half s.mw)

                    y =
                        Int32.sub (ftom s (mtof s mo.y)) (half s.mh)
                in
                { s
                    | mx = x
                    , my = y
                    , mx2 = Int32.add x s.mw
                    , my2 = Int32.add y s.mh
                    , oldloc = { x = mo.x, y = mo.y }
                }

            else
                s

        Nothing ->
            s


{-| AM\_Ticker: follow the player, zoom and pan while their keys are held.
-}
ticker : Env -> World -> Int -> Model -> Model
ticker _ world _ (Model s0) =
    if not s0.automapactive then
        Model s0

    else
        let
            s1 =
                if s0.followplayer then
                    doFollowPlayer world s0

                else
                    s0

            s2 =
                if s1.ftomZoommul /= Fixed.fracunit then
                    changeWindowScale s1

                else
                    s1

            s3 =
                if s2.paninc.x /= 0 || s2.paninc.y /= 0 then
                    changeWindowLoc s2

                else
                    s2
        in
        Model s3



-- RESPONDER


{-| cht\_CheckCheat's scramble of a key code.
-}
scramble : Int -> Int
scramble a =
    Bitwise.shiftLeftBy 7 (Bitwise.and a 1)
        + Bitwise.shiftLeftBy 5 (Bitwise.and a 2)
        + Bitwise.and a 4
        + Bitwise.shiftLeftBy 1 (Bitwise.and a 8)
        + Bitwise.shiftRightBy 1 (Bitwise.and a 16)
        + Bitwise.and a 32
        + Bitwise.shiftRightBy 5 (Bitwise.and a 64)
        + Bitwise.shiftRightBy 7 (Bitwise.and a 128)


{-| cheat\_amap\_seq: "iddt", scrambled.
-}
cheatAmapSeq : Array Int
cheatAmapSeq =
    Array.fromList [ 0xB2, 0x26, 0x26, 0x2E, 0xFF ]


{-| cht\_CheckCheat for iddt: the position in the sequence after this key, and
whether the sequence just completed. A wrong key starts it over (without trying the
key as the sequence's first).
-}
checkCheat : Int -> Int -> ( Int, Bool )
checkCheat pos key =
    let
        expected =
            Array.get pos cheatAmapSeq |> Maybe.withDefault 0xFF

        next =
            if scramble (modBy 256 key) == expected then
                pos + 1

            else
                0
    in
    if Array.get next cheatAmapSeq == Just 0xFF then
        ( 0, True )

    else
        ( next, False )


setMessage : Int -> String -> World -> World
setMessage n msg world =
    World.updatePlayer n (\p -> { p | message = Just msg }) world


{-| AM\_Responder. Gives the automap and world after the event (the player's message
may change) and whether the automap ate the event (C's return value). The model can
change even when the event isn't eaten: releasing a pan or zoom key, typing iddt.
Arrow keys are eaten only while not following the player.
-}
responder : Event -> Env -> World -> Int -> Model -> ( Model, World, Bool )
responder event env world consoleplayer (Model s) =
    if not s.automapactive then
        case event of
            KeyDown key ->
                if key == 9 then
                    ( Model (startState env world consoleplayer s), world, True )

                else
                    ( Model s, world, False )

            _ ->
                ( Model s, world, False )

    else
        case event of
            KeyDown key ->
                let
                    pan f =
                        if s.followplayer then
                            ( s, world, False )

                        else
                            ( { s | paninc = f s.paninc }, world, True )

                    ( s1, w1, rc ) =
                        if key == 0xAE then
                            pan (\p -> { p | x = ftom s fPaninc })

                        else if key == 0xAC then
                            pan (\p -> { p | x = Int32.neg (ftom s fPaninc) })

                        else if key == 0xAD then
                            pan (\p -> { p | y = ftom s fPaninc })

                        else if key == 0xAF then
                            pan (\p -> { p | y = Int32.neg (ftom s fPaninc) })

                        else if key == Char.toCode '-' then
                            ( { s | mtofZoommul = zoomOut, ftomZoommul = zoomIn }, world, True )

                        else if key == Char.toCode '=' then
                            ( { s | mtofZoommul = zoomIn, ftomZoommul = zoomOut }, world, True )

                        else if key == 9 then
                            ( stopState { s | bigstate = False }, world, True )

                        else if key == Char.toCode '0' then
                            if s.bigstate then
                                ( restoreScaleAndLoc world { s | bigstate = False }, world, True )

                            else
                                ( minOutWindowScale (saveScaleAndLoc { s | bigstate = True }), world, True )

                        else if key == Char.toCode 'f' then
                            let
                                follow =
                                    not s.followplayer
                            in
                            ( { s | followplayer = follow, oldloc = { x = Fixed.maxInt, y = s.oldloc.y } }
                            , setMessage s.plr
                                (if follow then
                                    "Follow Mode ON"

                                 else
                                    "Follow Mode OFF"
                                )
                                world
                            , True
                            )

                        else if key == Char.toCode 'g' then
                            ( { s | grid = not s.grid }
                            , setMessage s.plr
                                (if s.grid then
                                    "Grid OFF"

                                 else
                                    "Grid ON"
                                )
                                world
                            , True
                            )

                        else if key == Char.toCode 'm' then
                            ( addMark s
                            , setMessage s.plr ("Marked Spot " ++ String.fromInt s.markpointnum) world
                            , True
                            )

                        else if key == Char.toCode 'c' then
                            ( clearMarks s, setMessage s.plr "All Marks Cleared" world, True )

                        else
                            ( s, world, False )
                in
                if env.params.deathmatch == 0 then
                    let
                        ( pos, done ) =
                            checkCheat s1.cheatPos key
                    in
                    if done then
                        ( Model { s1 | cheatPos = pos, cheating = modBy 3 (s1.cheating + 1) }, w1, False )

                    else
                        ( Model { s1 | cheatPos = pos }, w1, rc )

                else
                    ( Model s1, w1, rc )

            KeyUp key ->
                let
                    stopPan f =
                        if s.followplayer then
                            s

                        else
                            { s | paninc = f s.paninc }

                    s1 =
                        if key == 0xAE || key == 0xAC then
                            stopPan (\p -> { p | x = 0 })

                        else if key == 0xAD || key == 0xAF then
                            stopPan (\p -> { p | y = 0 })

                        else if key == Char.toCode '-' || key == Char.toCode '=' then
                            { s | mtofZoommul = Fixed.fracunit, ftomZoommul = Fixed.fracunit }

                        else
                            s
                in
                ( Model s1, world, False )

            Mouse _ _ _ ->
                ( Model s, world, False )



-- LINES


type alias Mline =
    { ax : Fixed
    , ay : Fixed
    , bx : Fixed
    , by : Fixed
    }


type alias Fline =
    { ax : Int
    , ay : Int
    , bx : Int
    , by : Int
    }


codeLeft : Int
codeLeft =
    1


codeRight : Int
codeRight =
    2


codeBottom : Int
codeBottom =
    4


codeTop : Int
codeTop =
    8


{-| DOOUTCODE: where a frame-buffer point is outside the window.
-}
outcode : Int -> Int -> Int
outcode x y =
    (if y < 0 then
        codeTop

     else if y >= fH then
        codeBottom

     else
        0
    )
        + (if x < 0 then
            codeLeft

           else if x >= fW then
            codeRight

           else
            0
          )


has : Int -> Int -> Bool
has bit code =
    Bitwise.and bit code /= 0


{-| AM\_clipMline: Cohen–Sutherland, after a trivial reject in map coordinates.
-}
clipMline : State -> Mline -> Maybe Fline
clipMline s ml =
    let
        yCode y =
            if y > s.my2 then
                codeTop

            else if y < s.my then
                codeBottom

            else
                0

        xCode x =
            if x < s.mx then
                codeLeft

            else if x > s.mx2 then
                codeRight

            else
                0

        oy1 =
            yCode ml.ay

        oy2 =
            yCode ml.by
    in
    if Bitwise.and oy1 oy2 /= 0 then
        Nothing

    else if Bitwise.and (Bitwise.or oy1 (xCode ml.ax)) (Bitwise.or oy2 (xCode ml.bx)) /= 0 then
        Nothing

    else
        let
            fl =
                { ax = cxmtof s ml.ax, ay = cymtof s ml.ay, bx = cxmtof s ml.bx, by = cymtof s ml.by }
        in
        clipLoop fl (outcode fl.ax fl.ay) (outcode fl.bx fl.by)


clipLoop : Fline -> Int -> Int -> Maybe Fline
clipLoop fl oc1 oc2 =
    if Bitwise.and oc1 oc2 /= 0 then
        Nothing

    else if Bitwise.or oc1 oc2 == 0 then
        Just fl

    else
        let
            outside =
                if oc1 /= 0 then
                    oc1

                else
                    oc2

            ( tx, ty ) =
                if has codeTop outside then
                    let
                        dy =
                            Int32.sub fl.ay fl.by

                        dx =
                            Int32.sub fl.bx fl.ax
                    in
                    ( Int32.add fl.ax (cdiv (Int32.mul dx fl.ay) dy), 0 )

                else if has codeBottom outside then
                    let
                        dy =
                            Int32.sub fl.ay fl.by

                        dx =
                            Int32.sub fl.bx fl.ax
                    in
                    ( Int32.add fl.ax (cdiv (Int32.mul dx (Int32.sub fl.ay fH)) dy), fH - 1 )

                else if has codeRight outside then
                    let
                        dy =
                            Int32.sub fl.by fl.ay

                        dx =
                            Int32.sub fl.bx fl.ax
                    in
                    ( fW - 1, Int32.add fl.ay (cdiv (Int32.mul dy (Int32.sub (fW - 1) fl.ax)) dx) )

                else
                    let
                        dy =
                            Int32.sub fl.by fl.ay

                        dx =
                            Int32.sub fl.bx fl.ax
                    in
                    ( 0, Int32.add fl.ay (cdiv (Int32.mul dy (Int32.neg fl.ax)) dx) )
        in
        if outside == oc1 then
            clipLoop { fl | ax = tx, ay = ty } (outcode tx ty) oc2

        else
            clipLoop { fl | bx = tx, by = ty } oc1 (outcode tx ty)


type alias Dot =
    ( Int, Int, Int )


{-| AM\_drawFline: Bresenham, onto a list of dots (newest first). A line with an end
off the window draws nothing.
-}
drawFline : Fline -> Int -> List Dot -> List Dot
drawFline fl color acc =
    if
        fl.ax
            < 0
            || fl.ax
            >= fW
            || fl.ay
            < 0
            || fl.ay
            >= fH
            || fl.bx
            < 0
            || fl.bx
            >= fW
            || fl.by
            < 0
            || fl.by
            >= fH
    then
        acc

    else
        let
            dx =
                fl.bx - fl.ax

            ax =
                2 * Basics.abs dx

            sx =
                if dx < 0 then
                    -1

                else
                    1

            dy =
                fl.by - fl.ay

            ay =
                2 * Basics.abs dy

            sy =
                if dy < 0 then
                    -1

                else
                    1
        in
        if ax > ay then
            xMajor fl.bx color ax ay sx sy fl.ax fl.ay (ay - ax // 2) acc

        else
            yMajor fl.by color ax ay sx sy fl.ax fl.ay (ax - ay // 2) acc


xMajor : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> List Dot -> List Dot
xMajor bx color ax ay sx sy x y d acc =
    let
        acc1 =
            ( x, y, color ) :: acc
    in
    if x == bx then
        acc1

    else if d >= 0 then
        xMajor bx color ax ay sx sy (x + sx) (y + sy) (d - ax + ay) acc1

    else
        xMajor bx color ax ay sx sy (x + sx) y (d + ay) acc1


yMajor : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> List Dot -> List Dot
yMajor by color ax ay sx sy x y d acc =
    let
        acc1 =
            ( x, y, color ) :: acc
    in
    if y == by then
        acc1

    else if d >= 0 then
        yMajor by color ax ay sx sy (x + sx) (y + sy) (d - ay + ax) acc1

    else
        yMajor by color ax ay sx sy x (y + sy) (d + ax) acc1


{-| AM\_drawMline.
-}
drawMline : State -> Int -> Mline -> List Dot -> List Dot
drawMline s color ml acc =
    case clipMline s ml of
        Just fl ->
            drawFline fl color acc

        Nothing ->
            acc



-- WHAT IS DRAWN


mapBlockUnits : Fixed
mapBlockUnits =
    128 * Fixed.fracunit


{-| AM\_drawGrid: lines on the blockmap's 128-unit grid, from the origin the game
now reads (`World.bmap`: the lump's, unless an intercepts overrun wrote over it,
as on DOS).
-}
drawGrid : Env -> World -> State -> Int -> List Dot -> List Dot
drawGrid env world s color acc0 =
    let
        firstLine from org =
            let
                r =
                    remainderBy mapBlockUnits (Int32.sub from org)
            in
            if r /= 0 then
                Int32.add from (mapBlockUnits - r)

            else
                from

        vertical x end acc =
            if x < end then
                vertical (Int32.add x mapBlockUnits)
                    end
                    (drawMline s color { ax = x, ay = s.my, bx = x, by = Int32.add s.my s.mh } acc)

            else
                acc

        horizontal y end acc =
            if y < end then
                horizontal (Int32.add y mapBlockUnits)
                    end
                    (drawMline s color { ax = s.mx, ay = y, bx = Int32.add s.mx s.mw, by = y } acc)

            else
                acc

        bm =
            MapUtil.blockmap env world
    in
    acc0
        |> vertical (firstLine s.mx bm.orgx) (Int32.add s.mx s.mw)
        |> horizontal (firstLine s.my bm.orgy) (Int32.add s.my s.mh)


{-| AM\_drawWalls: the lines seen (or all, cheating), coloured by kind; with the
computer map, the unseen ones in grey.
-}
drawWalls : Env -> World -> State -> List Dot -> List Dot
drawWalls env world s acc0 =
    let
        allmap =
            World.player s.plr world
                |> Maybe.map (\p -> Player.power Defs.AllMap p /= 0)
                |> Maybe.withDefault False

        cheating =
            s.cheating /= 0

        drawLine i line acc =
            let
                state =
                    World.lineState i world

                flags =
                    state.flags

                ml =
                    { ax = line.v1.x, ay = line.v1.y, bx = line.v2.x, by = line.v2.y }
            in
            if cheating || Flags.has Flags.mlMapped flags then
                if Flags.has Flags.mlDontDraw flags && not cheating then
                    acc

                else
                    case line.backsector of
                        Nothing ->
                            drawMline s (reds + lightlev) ml acc

                        Just back ->
                            let
                                front =
                                    World.sector line.frontsector world

                                backS =
                                    World.sector back world
                            in
                            if state.special == 39 then
                                drawMline s (reds + 16 // 2) ml acc

                            else if Flags.has Flags.mlSecret flags then
                                drawMline s (reds + lightlev) ml acc

                            else if backS.floorheight /= front.floorheight then
                                drawMline s (browns + lightlev) ml acc

                            else if backS.ceilingheight /= front.ceilingheight then
                                drawMline s (yellows + lightlev) ml acc

                            else if cheating then
                                drawMline s (grays + lightlev) ml acc

                            else
                                acc

            else if allmap then
                if not (Flags.has Flags.mlDontDraw flags) then
                    drawMline s (grays + 3) ml acc

                else
                    acc

            else
                acc
    in
    Array.foldl (\line ( i, acc ) -> ( i + 1, drawLine i line acc )) ( 0, acc0 ) env.level.lines
        |> Tuple.second


{-| AM\_rotate.
-}
rotate : Angle -> ( Fixed, Fixed ) -> ( Fixed, Fixed )
rotate a ( x, y ) =
    let
        c =
            Angle.cosine a

        sn =
            Angle.sine a
    in
    ( Int32.sub (Fixed.mul x c) (Fixed.mul y sn)
    , Int32.add (Fixed.mul x sn) (Fixed.mul y c)
    )


{-| AM\_drawLineCharacter: a vector picture scaled (unless the scale is 0), rotated
(unless the angle is 0) and moved to (x, y).
-}
drawLineCharacter : State -> List Mline -> Fixed -> Angle -> Int -> Fixed -> Fixed -> List Dot -> List Dot
drawLineCharacter s lineguy scale angle color x y acc0 =
    let
        place ( px, py ) =
            let
                scaled =
                    if scale /= 0 then
                        ( Fixed.mul scale px, Fixed.mul scale py )

                    else
                        ( px, py )

                ( rx, ry ) =
                    if angle /= 0 then
                        rotate angle scaled

                    else
                        scaled
            in
            ( Int32.add rx x, Int32.add ry y )
    in
    List.foldl
        (\l acc ->
            let
                ( ax, ay ) =
                    place ( l.ax, l.ay )

                ( bx, by ) =
                    place ( l.bx, l.by )
            in
            drawMline s color { ax = ax, ay = ay, bx = bx, by = by } acc
        )
        acc0
        lineguy


mline : Int -> Int -> Int -> Int -> Mline
mline ax ay bx by =
    { ax = ax, ay = ay, bx = bx, by = by }


{-| R: (8 \* PLAYERRADIUS) / 7.
-}
arrowR : Int
arrowR =
    8 * playerRadius // 7


playerArrow : List Mline
playerArrow =
    let
        r =
            arrowR
    in
    [ mline (-r + r // 8) 0 r 0
    , mline r 0 (r - r // 2) (r // 4)
    , mline r 0 (r - r // 2) (negate r // 4)
    , mline (-r + r // 8) 0 (-r - r // 8) (r // 4)
    , mline (-r + r // 8) 0 (-r - r // 8) (negate r // 4)
    , mline (-r + 3 * r // 8) 0 (-r + r // 8) (r // 4)
    , mline (-r + 3 * r // 8) 0 (-r + r // 8) (negate r // 4)
    ]


cheatPlayerArrow : List Mline
cheatPlayerArrow =
    let
        r =
            arrowR
    in
    [ mline (-r + r // 8) 0 r 0
    , mline r 0 (r - r // 2) (r // 6)
    , mline r 0 (r - r // 2) (negate r // 6)
    , mline (-r + r // 8) 0 (-r - r // 8) (r // 6)
    , mline (-r + r // 8) 0 (-r - r // 8) (negate r // 6)
    , mline (-r + 3 * r // 8) 0 (-r + r // 8) (r // 6)
    , mline (-r + 3 * r // 8) 0 (-r + r // 8) (negate r // 6)
    , mline (negate r // 2) 0 (negate r // 2) (negate r // 6)
    , mline (negate r // 2) (negate r // 6) (negate r // 2 + r // 6) (negate r // 6)
    , mline (negate r // 2 + r // 6) (negate r // 6) (negate r // 2 + r // 6) (r // 4)
    , mline (negate r // 6) 0 (negate r // 6) (negate r // 6)
    , mline (negate r // 6) (negate r // 6) 0 (negate r // 6)
    , mline 0 (negate r // 6) 0 (r // 4)
    , mline (r // 6) (r // 4) (r // 6) (negate r // 7)
    , mline (r // 6) (negate r // 7) (r // 6 + r // 32) (negate r // 7 - r // 32)
    , mline (r // 6 + r // 32) (negate r // 7 - r // 32) (r // 6 + r // 10) (negate r // 7)
    ]


{-| thintriangle\_guy, its doubles truncated to fixed point as C's initialiser does.
-}
thinTriangleGuy : List Mline
thinTriangleGuy =
    [ mline -32768 -45875 65536 0
    , mline 65536 0 -32768 45875
    , mline -32768 45875 -32768 -45875
    ]


{-| AM\_drawPlayers: the followed player's arrow (the cheat's with iddt) in a
single-player game; in a netgame every player in the game, in their colour (all but
the followed one hidden in deathmatch, unless playing back a single demo), near-black
when invisible.
-}
drawPlayers : Env -> World -> State -> List Dot -> List Dot
drawPlayers env world s acc =
    if not env.params.netgame then
        case World.playerMobj s.plr world of
            Just mo ->
                drawLineCharacter s
                    (if s.cheating /= 0 then
                        cheatPlayerArrow

                     else
                        playerArrow
                    )
                    0
                    mo.angle
                    white
                    mo.x
                    mo.y
                    acc

            Nothing ->
                acc

    else
        List.foldl
            (\i a ->
                let
                    inGame =
                        Array.get i world.playeringame |> Maybe.withDefault False

                    hidden =
                        (env.params.deathmatch /= 0 && not s.singledemo) && i /= s.plr
                in
                if hidden || not inGame then
                    a

                else
                    case ( World.player i world, World.playerMobj i world ) of
                        ( Just p, Just mo ) ->
                            let
                                color =
                                    if Player.power Defs.Invisibility p /= 0 then
                                        246

                                    else
                                        Array.get i theirColors |> Maybe.withDefault greens
                            in
                            drawLineCharacter s playerArrow 0 mo.angle color mo.x mo.y a

                        _ ->
                            a
            )
            acc
            (List.range 0 (Defs.maxPlayers - 1))


theirColors : Array Int
theirColors =
    Array.fromList [ greens, grays, browns, reds ]


{-| AM\_drawThings: every thing as a triangle, sector by sector.
-}
drawThings : World -> State -> Int -> List Dot -> List Dot
drawThings world s colors acc0 =
    Array.foldl
        (\ids acc ->
            List.foldl
                (\id a ->
                    case World.mobj id world of
                        Just t ->
                            drawThing s colors t a

                        Nothing ->
                            a
                )
                acc
                ids
        )
        acc0
        world.sectorThings


drawThing : State -> Int -> Mobj -> List Dot -> List Dot
drawThing s colors t acc =
    drawLineCharacter s thinTriangleGuy (Fixed.fromInt 16) t.angle (colors + lightlev) t.x t.y acc


{-| AM\_drawMarks: the mark numbers, those whose 5 × 6 box fits in the window.
-}
drawMarks : State -> Framebuffer -> Framebuffer
drawMarks s fb0 =
    Array.foldl
        (\p ( i, fb ) ->
            if p.x /= -1 then
                let
                    fx =
                        cxmtof s p.x

                    fy =
                        cymtof s p.y
                in
                if fx >= fX && fx <= fW - 5 && fy >= fY && fy <= fH - 6 then
                    case Array.get i s.marknums |> Maybe.andThen identity of
                        Just patch ->
                            ( i + 1, Video.drawPatch fx fy patch fb )

                        Nothing ->
                            ( i + 1, fb )

                else
                    ( i + 1, fb )

            else
                ( i + 1, fb )
        )
        ( 0, fb0 )
        s.markpoints
        |> Tuple.second


{-| AM\_drawCrosshair: one dot at `fb[(f_w * (f_h + 1)) / 2]`, the window's middle.
-}
drawCrosshair : Int -> List Dot -> List Dot
drawCrosshair color acc =
    let
        i =
            fW * (fH + 1) // 2
    in
    ( modBy fW i, i // fW, color ) :: acc


{-| AM\_Drawer: clear the window to black, then the grid, walls, players, things
(iddt twice), the crosshair and the marks.
-}
drawer : Env -> World -> Int -> Model -> Framebuffer -> Framebuffer
drawer env world _ (Model s) fb =
    if not s.automapactive then
        fb

    else
        let
            dots =
                []
                    |> (if s.grid then
                            drawGrid env world s (grays + 16 // 2)

                        else
                            identity
                       )
                    |> drawWalls env world s
                    |> drawPlayers env world s
                    |> (if s.cheating == 2 then
                            drawThings world s greens

                        else
                            identity
                       )
                    |> drawCrosshair grays
        in
        fb
            |> Video.fill fX fY fW fH 0
            |> Video.plot (List.reverse dots)
            |> drawMarks s
