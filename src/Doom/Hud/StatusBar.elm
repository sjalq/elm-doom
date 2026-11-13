module Doom.Hud.StatusBar exposing
    ( Model, init, initFor, start, ticker, automapEntered
    , Request(..), Responded, responder
    , DrawFlags, drawer, palette
    )

{-| The status bar (st\_stuff.c, st\_lib.c) and the cheats it handles (m\_cheat.c):
ammo, health, arms, the face, armour, keys, and in deathmatch frags.

The original draws the bar incrementally: ST\_Drawer copies the whole bar onto the
screen only on a refresh (just after ST\_Start, after the automap opens, after a wipe or
a change of screen size), and otherwise redraws the numbers every frame and the icons
(the face, the keys, the arms numbers) only when they change, erasing the old icon
with the bar's background picture first. `drawer` does the same on the screen it is
given, so it must be given the previous frame's screen (C's `screens[0]` persists),
and the pixels come out the same as the original's, stale ones included: a key icon
lost without a refresh (a reborn in co-op) stays on the bar, and an arms number that
changes is erased with STBAR's pixels rather than STARMS's.

The background picture (C's `screens[4]`) is STBAR, with the player's face
background in a netgame. Before its first refresh C's buffer holds whatever
`Z_Malloc` gave; here it is that picture from the start (nothing draws from it
before a refresh in the game).

@docs Model, init, initFor, start, ticker, automapEntered
@docs Request, Responded, responder
@docs DrawFlags, drawer, palette

-}

import Array exposing (Array)
import Bitwise
import Doom.Angle as Angle
import Doom.Defs as Defs exposing (Ammo(..), GameMode(..), Power(..), Skill, Weapon(..))
import Doom.Event exposing (Event(..))
import Doom.Geometry as Geometry
import Doom.Int32 as Int32
import Doom.Play.Env exposing (Env)
import Doom.Play.Inter as Inter
import Doom.Player as Player exposing (Player)
import Doom.Random as Random
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (World)


{-| The status bar's state: its pictures, the face's animation, what the icons last
showed, and the cheat sequences typed so far.
-}
type Model
    = Model State


type alias State =
    { graphics : Graphics
    , background : Framebuffer
    , firsttime : Bool
    , face : Face
    , oldhealth : Int
    , keyboxes : Array Int
    , randomnumber : Int
    , notdeathmatch : Bool
    , fragscount : Int
    , ready : ReadyAmmo
    , oldArms : Array Int
    , oldFace : Int
    , oldKeys : Array Int
    , oldArmsbg : Bool
    , cheats : Cheats
    }


{-| The face widget's state. `priority` and `lastattackdown` are statics of
ST\_updateFaceWidget that nothing resets, so they carry over from level to level.
-}
type alias Face =
    { priority : Int
    , index : Int
    , count : Int
    , lastattackdown : Int
    , oldweaponsowned : Array Bool
    }


{-| What the ready-weapon number (`w_ready.num`) points at. ST\_createWidgets points it
at `ammo[weaponinfo[readyweapon].ammo]`, which for a weapon without ammo is one past
the end of `ammo`: `maxammo[0]`. ST\_updateWidgets points it at the "n/a" number 1994
instead.
-}
type ReadyAmmo
    = AmmoOf Ammo
    | MaxClips
    | NotApplicable


type alias Graphics =
    { tallnum : Array (Maybe Patch)
    , shortnum : Array (Maybe Patch)
    , tallpercent : Maybe Patch
    , keys : Array (Maybe Patch)
    , armsbg : Maybe Patch
    , grayArms : Array (Maybe Patch)
    , faceback : Maybe Patch
    , sbar : Maybe Patch
    , faces : Array (Maybe Patch)
    , minus : Maybe Patch
    }



-- LAYOUT (st_stuff.c's constants)


stY : Int
stY =
    168


faceStride : Int
faceStride =
    8


turnOffset : Int
turnOffset =
    3


ouchOffset : Int
ouchOffset =
    5


evilGrinOffset : Int
evilGrinOffset =
    6


rampageOffset : Int
rampageOffset =
    7


godFace : Int
godFace =
    40


deadFace : Int
deadFace =
    41


largeAmmo : Int
largeAmmo =
    1994



-- ST_Init


{-| ST\_Init: load its patches (ST\_loadGraphics and STlib\_init's STTMINUS).
With player 0's face background (see `initFor`).
-}
init : Wad -> Model
init =
    initFor 0


{-| ST\_Init when `consoleplayer` is already `n` at start-up (a real netgame). The face
background (STFB0 to STFB3) is the console player's at ST\_Init and is never loaded
again: playing back a netdemo, C's `consoleplayer` is still 0 at ST\_Init, so the
background is green whoever the demo's console player is.
-}
initFor : Int -> Wad -> Model
initFor player wad =
    let
        load name =
            Wad.lump name wad |> Maybe.andThen Video.patch

        loadAll names =
            Array.fromList (List.map load names)

        digits prefix =
            loadAll (List.map (\i -> prefix ++ String.fromInt i) (List.range 0 9))

        faceNames =
            List.concatMap
                (\i ->
                    let
                        n =
                            String.fromInt i
                    in
                    [ "STFST" ++ n ++ "0"
                    , "STFST" ++ n ++ "1"
                    , "STFST" ++ n ++ "2"
                    , "STFTR" ++ n ++ "0"
                    , "STFTL" ++ n ++ "0"
                    , "STFOUCH" ++ n
                    , "STFEVL" ++ n
                    , "STFKILL" ++ n
                    ]
                )
                (List.range 0 4)
                ++ [ "STFGOD0", "STFDEAD0" ]

        graphics =
            { tallnum = digits "STTNUM"
            , shortnum = digits "STYSNUM"
            , tallpercent = load "STTPRCNT"
            , keys = loadAll (List.map (\i -> "STKEYS" ++ String.fromInt i) (List.range 0 5))
            , armsbg = load "STARMS"
            , grayArms = loadAll (List.map (\i -> "STGNUM" ++ String.fromInt i) (List.range 2 7))
            , faceback = load ("STFB" ++ String.fromInt player)
            , sbar = load "STBAR"
            , faces = loadAll faceNames
            , minus = load "STTMINUS"
            }
    in
    Model
        { graphics = graphics
        , background = Video.blank 0
        , firsttime = True
        , face =
            { priority = 0
            , index = 0
            , count = 0
            , lastattackdown = -1
            , oldweaponsowned = Array.repeat 9 False
            }
        , oldhealth = -1
        , keyboxes = Array.repeat 3 -1
        , randomnumber = 0
        , notdeathmatch = False
        , fragscount = 0
        , ready = MaxClips
        , oldArms = Array.repeat 6 -1
        , oldFace = -1
        , oldKeys = Array.repeat 3 -1
        , oldArmsbg = False
        , cheats = initialCheats
        }



-- ST_Start


{-| ST\_Start for the console player `n` (P\_SpawnPlayer calls it when it spawns them):
ST\_initData and ST\_createWidgets. The next draw is a full refresh.
-}
start : Env -> World -> Int -> Model -> Model
start env world n (Model st) =
    let
        p =
            World.player n world |> Maybe.withDefault Player.new

        g =
            st.graphics

        bar =
            Video.blank 0 |> drawMaybe 0 0 g.sbar

        background =
            if env.params.netgame then
                drawMaybe 143 0 g.faceback bar

            else
                bar

        face =
            st.face
    in
    Model
        { st
            | background = background
            , firsttime = True
            , face = { face | index = 0, oldweaponsowned = p.weaponowned }
            , oldhealth = -1
            , keyboxes = Array.repeat 3 -1
            , ready =
                case (Defs.weaponInfo p.readyweapon).ammo of
                    Just a ->
                        AmmoOf a

                    Nothing ->
                        MaxClips
            , oldArms = Array.repeat 6 -1
            , oldFace = -1
            , oldKeys = Array.repeat 3 -1
            , oldArmsbg = False
        }


{-| The automap's AM\_MSGENTERED notice to ST\_Responder: the automap has opened, so
the next draw is a full refresh. Call it when the automap opens.
-}
automapEntered : Model -> Model
automapEntered (Model st) =
    Model { st | firsttime = True }



-- ST_Ticker


{-| ST\_Ticker for the console player `n`: take a random number for the face
(M\_Random, from `world.rng`), and update the widgets' values (ST\_updateWidgets). It
runs every tic in a level, after P\_Ticker, paused or not.
-}
ticker : Env -> World -> Int -> Model -> ( Model, World )
ticker env world n (Model st) =
    let
        ( rnd, rng ) =
            Random.mRandom world.rng

        world1 =
            { world | rng = rng }

        p =
            World.player n world1 |> Maybe.withDefault Player.new

        card i =
            Array.get i p.cards |> Maybe.withDefault False

        keyboxes =
            Array.initialize 3
                (\i ->
                    if card (i + 3) then
                        i + 3

                    else if card i then
                        i

                    else
                        -1
                )

        frags =
            List.foldl
                (\i acc ->
                    let
                        f =
                            Array.get i p.frags |> Maybe.withDefault 0
                    in
                    if i /= n then
                        acc + f

                    else
                        acc - f
                )
                0
                (List.range 0 (Defs.maxPlayers - 1))

        st1 =
            { st | randomnumber = rnd }
    in
    ( Model
        { st1
            | ready =
                case (Defs.weaponInfo p.readyweapon).ammo of
                    Just a ->
                        AmmoOf a

                    Nothing ->
                        NotApplicable
            , keyboxes = keyboxes
            , face = updateFace world1 p st1
            , notdeathmatch = env.params.deathmatch == 0
            , fragscount = frags
            , oldhealth = p.health
        }
    , world1
    )


{-| ST\_calcPainOffset: which row of faces the health picks.
-}
painOffset : Player -> Int
painOffset p =
    let
        health =
            min 100 p.health
    in
    faceStride * (((100 - health) * 5) // 101)


{-| ST\_updateFaceWidget. Its order of precedence: dead, evil grin, turned head (being
attacked), ouch, rampage, god mode, straight ahead.
-}
updateFace : World -> Player -> State -> Face
updateFace world p st =
    let
        pain =
            painOffset p

        f0 =
            st.face

        -- dead
        f1 =
            if f0.priority < 10 && p.health == 0 then
                { f0 | priority = 9, index = deadFace, count = 1 }

            else
                f0

        -- picking up a weapon: evil grin
        f2 =
            if f1.priority < 9 && p.bonuscount /= 0 then
                let
                    doevilgrin =
                        Array.toList f1.oldweaponsowned /= Array.toList p.weaponowned

                    f =
                        { f1 | oldweaponsowned = p.weaponowned }
                in
                if doevilgrin then
                    { f | priority = 8, count = 2 * Defs.ticrate, index = pain + evilGrinOffset }

                else
                    f

            else
                f1

        -- being attacked
        f3 =
            if f2.priority < 8 && p.damagecount /= 0 && p.attacker /= Nothing && p.attacker /= p.mo then
                if p.health - st.oldhealth > 20 then
                    { f2 | priority = 7, count = Defs.ticrate, index = pain + ouchOffset }

                else
                    { f2 | priority = 7, count = Defs.ticrate, index = pain + lookTowards world p }

            else
                f2

        -- getting hurt because of your own damn stupidity
        f4 =
            if f3.priority < 7 && p.damagecount /= 0 then
                if p.health - st.oldhealth > 20 then
                    { f3 | priority = 7, count = Defs.ticrate, index = pain + ouchOffset }

                else
                    { f3 | priority = 6, count = Defs.ticrate, index = pain + rampageOffset }

            else
                f3

        -- rapid firing
        f5 =
            if f4.priority < 6 then
                if p.attackdown then
                    if f4.lastattackdown == -1 then
                        { f4 | lastattackdown = 2 * Defs.ticrate }

                    else if f4.lastattackdown - 1 == 0 then
                        { f4 | priority = 5, index = pain + rampageOffset, count = 1, lastattackdown = 1 }

                    else
                        { f4 | lastattackdown = f4.lastattackdown - 1 }

                else
                    { f4 | lastattackdown = -1 }

            else
                f4

        -- invulnerability
        f6 =
            if f5.priority < 5 && (Bitwise.and p.cheats Defs.cfGodmode /= 0 || Player.power Invulnerability p /= 0) then
                { f5 | priority = 4, index = godFace, count = 1 }

            else
                f5

        -- look left or right when the count runs out
        f7 =
            if f6.count == 0 then
                { f6 | index = pain + modBy 3 st.randomnumber, count = Defs.ticrate // 2, priority = 0 }

            else
                f6
    in
    { f7 | count = f7.count - 1 }


{-| Which way the face looks at the attacker: head-on, right or left (offsets in the
row of faces).
-}
lookTowards : World -> Player -> Int
lookTowards world p =
    case ( p.mo |> Maybe.andThen (\id -> World.mobj id world), p.attacker |> Maybe.andThen (\id -> World.mobj id world) ) of
        ( Just mo, Just attacker ) ->
            let
                badguyangle =
                    Geometry.pointToAngle2 mo.x mo.y attacker.x attacker.y

                ( diffang, i ) =
                    if badguyangle > mo.angle then
                        let
                            d =
                                Angle.sub badguyangle mo.angle
                        in
                        ( d, d > Angle.ang180 )

                    else
                        let
                            d =
                                Angle.sub mo.angle badguyangle
                        in
                        ( d, d <= Angle.ang180 )
            in
            if diffang < Angle.ang45 then
                rampageOffset

            else if i then
                turnOffset

            else
                turnOffset + 1

        _ ->
            rampageOffset



-- ST_doPaletteStuff


{-| ST\_doPaletteStuff: the PLAYPAL palette to show for player `n` now: red while hurt
or fading out of berserk, gold after a pickup, green in a radiation suit (flickering
as it runs out).
-}
palette : World -> Int -> Int
palette world n =
    case World.player n world of
        Nothing ->
            0

        Just p ->
            let
                strength =
                    Player.power Strength p

                cnt =
                    if strength /= 0 then
                        max p.damagecount (12 - Bitwise.shiftRightBy 6 strength)

                    else
                        p.damagecount

                ironfeet =
                    Player.power IronFeet p
            in
            if cnt /= 0 then
                1 + min 7 (Bitwise.shiftRightBy 3 (cnt + 7))

            else if p.bonuscount /= 0 then
                9 + min 3 (Bitwise.shiftRightBy 3 (p.bonuscount + 7))

            else if ironfeet > 4 * 32 || Bitwise.and ironfeet 8 /= 0 then
                13

            else
                0



-- ST_Drawer


{-| ST\_Drawer's arguments: `fullscreen` (`viewheight == 200`), `refresh` (D\_Display's
`redrawsbar`: after a wipe, a change from full screen, or the help screen) and
`automap` (`automapactive`).
-}
type alias DrawFlags =
    { fullscreen : Bool
    , refresh : Bool
    , automap : Bool
    }


{-| ST\_Drawer for the console player `n`, drawing onto the screen as the last frame
left it.
-}
drawer : Env -> World -> Int -> DrawFlags -> Model -> Framebuffer -> ( Model, Framebuffer )
drawer env world n flags (Model st) fb =
    let
        statusbaron =
            not flags.fullscreen || flags.automap
    in
    if st.firsttime || flags.refresh then
        let
            refreshed =
                if statusbaron then
                    Video.copyRect 0 0 Video.width 32 st.background 0 stY fb

                else
                    fb
        in
        drawWidgets env world n statusbaron True { st | firsttime = False } refreshed

    else
        drawWidgets env world n statusbaron False st fb


{-| ST\_drawWidgets, in its order (later widgets draw over earlier ones).
-}
drawWidgets : Env -> World -> Int -> Bool -> Bool -> State -> Framebuffer -> ( Model, Framebuffer )
drawWidgets env world n on refresh st fb0 =
    let
        p =
            World.player n world |> Maybe.withDefault Player.new

        g =
            st.graphics

        bg =
            st.background

        deathmatch =
            env.params.deathmatch /= 0

        armson =
            on && not deathmatch

        fragson =
            deathmatch && on

        number digits x y width value fb =
            if on then
                drawNum bg g.minus digits x y width value fb

            else
                fb

        percent x y value fb =
            let
                withSign =
                    if refresh && on then
                        drawMaybe x y g.tallpercent fb

                    else
                        fb
            in
            number g.tallnum x y 3 value withSign

        readyValue =
            case st.ready of
                AmmoOf a ->
                    Player.ammo a p

                MaxClips ->
                    Player.maxAmmoOf Clip p

                NotApplicable ->
                    largeAmmo

        ammoRows =
            [ ( Clip, 173 ), ( Shell, 179 ), ( Cell, 191 ), ( Misl, 185 ) ]

        fb1 =
            List.foldl
                (\( a, y ) fb ->
                    fb
                        |> number g.shortnum 288 y 3 (Player.ammo a p)
                        |> number g.shortnum 314 y 3 (Player.maxAmmoOf a p)
                )
                (number g.tallnum 44 171 3 readyValue fb0)
                ammoRows
                |> percent 90 171 p.health
                |> percent 221 171 p.armorpoints

        -- the arms background
        ( oldArmsbg, fb2 ) =
            if on && (st.oldArmsbg /= st.notdeathmatch || refresh) then
                if st.notdeathmatch then
                    ( st.notdeathmatch, drawMaybe 104 168 g.armsbg fb1 )

                else
                    ( st.notdeathmatch, eraseFor bg g.armsbg 104 168 fb1 )

            else
                ( st.oldArmsbg, fb1 )

        -- the weapons owned
        ( oldArms, fb3 ) =
            List.foldl
                (\i ( olds, fb ) ->
                    let
                        owned =
                            if Array.get (i + 1) p.weaponowned |> Maybe.withDefault False then
                                1

                            else
                                0

                        ( old, fb_ ) =
                            multIcon bg
                                (Array.fromList [ get i g.grayArms, get (i + 2) g.shortnum ])
                                (111 + modBy 3 i * 12)
                                (172 + (i // 3) * 10)
                                armson
                                refresh
                                owned
                                (Array.get i olds |> Maybe.withDefault -1)
                                fb
                    in
                    ( Array.set i old olds, fb_ )
                )
                ( st.oldArms, fb2 )
                (List.range 0 5)

        ( oldFace, fb4 ) =
            multIcon bg g.faces 143 168 on refresh st.face.index st.oldFace fb3

        ( oldKeys, fb5 ) =
            List.foldl
                (\i ( olds, fb ) ->
                    let
                        ( old, fb_ ) =
                            multIcon bg
                                g.keys
                                239
                                (171 + i * 10)
                                on
                                refresh
                                (Array.get i st.keyboxes |> Maybe.withDefault -1)
                                (Array.get i olds |> Maybe.withDefault -1)
                                fb
                    in
                    ( Array.set i old olds, fb_ )
                )
                ( st.oldKeys, fb4 )
                (List.range 0 2)

        fb6 =
            if fragson then
                drawNum bg g.minus g.tallnum 138 171 2 st.fragscount fb5

            else
                fb5
    in
    ( Model { st | oldArmsbg = oldArmsbg, oldArms = oldArms, oldFace = oldFace, oldKeys = oldKeys }
    , fb6
    )


{-| STlib\_drawNum: a right-aligned number of up to `numdigits` digits ending at `x`,
erasing its whole field first. Negative numbers get a minus sign (clamped to -9 or
-99), 1994 means "n/a" and draws nothing, and 0 draws a single zero.
-}
drawNum : Framebuffer -> Maybe Patch -> Array (Maybe Patch) -> Int -> Int -> Int -> Int -> Framebuffer -> Framebuffer
drawNum bg minus digits x y numdigits value fb =
    case get 0 digits of
        Nothing ->
            fb

        Just zero ->
            let
                size =
                    Video.patchSize zero

                w =
                    size.width

                neg =
                    value < 0

                num =
                    if neg then
                        if numdigits == 2 && value < -9 then
                            9

                        else if numdigits == 3 && value < -99 then
                            99

                        else
                            negate value

                    else
                        value

                cleared =
                    copyFromBackground bg (x - numdigits * w) y (w * numdigits) size.height fb

                digitsLoop left digitsLeft dx acc =
                    if left /= 0 && digitsLeft > 0 then
                        digitsLoop (left // 10) (digitsLeft - 1) (dx - w) (drawMaybe (dx - w) y (get (modBy 10 left) digits) acc)

                    else
                        ( dx, acc )
            in
            if num == largeAmmo then
                cleared

            else
                let
                    withZero =
                        if num == 0 then
                            Video.drawPatch (x - w) y zero cleared

                        else
                            cleared

                    ( endX, drawn ) =
                        digitsLoop num numdigits x withZero
                in
                if neg then
                    drawMaybe (endX - 8) y minus drawn

                else
                    drawn


{-| STlib\_updateMultIcon: when the value changes (or on a refresh), erase the old
icon's rectangle with the background and draw the new one; a value of -1 draws
nothing and leaves the old icon there. Gives the new "old value".
-}
multIcon : Framebuffer -> Array (Maybe Patch) -> Int -> Int -> Bool -> Bool -> Int -> Int -> Framebuffer -> ( Int, Framebuffer )
multIcon bg patches x y on refresh value old fb =
    if on && (old /= value || refresh) && value /= -1 then
        let
            erased =
                if old /= -1 then
                    eraseFor bg (get old patches) x y fb

                else
                    fb
        in
        ( value, drawMaybe x y (get value patches) erased )

    else
        ( old, fb )


{-| Copy the background over the rectangle the patch would cover at (x, y).
-}
eraseFor : Framebuffer -> Maybe Patch -> Int -> Int -> Framebuffer -> Framebuffer
eraseFor bg maybePatch x y fb =
    case maybePatch of
        Just p ->
            let
                s =
                    Video.patchSize p
            in
            copyFromBackground bg (x - s.leftoffset) (y - s.topoffset) s.width s.height fb

        Nothing ->
            fb


{-| V\_CopyRect from the background (screens[4], the bar's 32 rows) to the screen.
-}
copyFromBackground : Framebuffer -> Int -> Int -> Int -> Int -> Framebuffer -> Framebuffer
copyFromBackground bg x y w h fb =
    Video.copyRect x (y - stY) w h bg x y fb


drawMaybe : Int -> Int -> Maybe Patch -> Framebuffer -> Framebuffer
drawMaybe x y maybePatch fb =
    case maybePatch of
        Just p ->
            Video.drawPatch x y p fb

        Nothing ->
            fb


get : Int -> Array (Maybe a) -> Maybe a
get i arr =
    Array.get i arr |> Maybe.andThen identity



-- CHEATS (m_cheat.c)


{-| A cheat sequence (cheatseq\_t): its scrambled bytes (parameter slots are 0 until
typed into, and hold what was typed until cht\_GetParam reads them), and how far
along it the player has typed.
-}
type alias Cheat =
    { sequence : Array Int
    , p : Int
    }


type alias Cheats =
    { mus : Cheat
    , god : Cheat
    , ammo : Cheat
    , ammonokey : Cheat
    , noclip : Cheat
    , commercialNoclip : Cheat
    , powerup : Array Cheat
    , choppers : Cheat
    , clev : Cheat
    , mypos : Cheat
    }


cheat : List Int -> Cheat
cheat bytes =
    { sequence = Array.fromList bytes, p = 0 }


initialCheats : Cheats
initialCheats =
    let
        behold last =
            cheat ([ 0xB2, 0x26, 0x62, 0xA6, 0x32, 0xF6, 0x36, 0x26 ] ++ last ++ [ 0xFF ])
    in
    { mus = cheat [ 0xB2, 0x26, 0xB6, 0xAE, 0xEA, 1, 0, 0, 0xFF ]
    , god = cheat [ 0xB2, 0x26, 0x26, 0xAA, 0x26, 0xFF ]
    , ammo = cheat [ 0xB2, 0x26, 0xF2, 0x66, 0xA2, 0xFF ]
    , ammonokey = cheat [ 0xB2, 0x26, 0x66, 0xA2, 0xFF ]
    , noclip = cheat [ 0xB2, 0x26, 0xEA, 0x2A, 0xB2, 0xEA, 0x2A, 0xF6, 0x2A, 0x26, 0xFF ]
    , commercialNoclip = cheat [ 0xB2, 0x26, 0xE2, 0x36, 0xB2, 0x2A, 0xFF ]
    , powerup =
        Array.fromList
            [ behold [ 0x6E ]
            , behold [ 0xEA ]
            , behold [ 0xB2 ]
            , behold [ 0x6A ]
            , behold [ 0xA2 ]
            , behold [ 0x36 ]
            , behold []
            ]
    , choppers = cheat [ 0xB2, 0x26, 0xE2, 0x32, 0xF6, 0x2A, 0x2A, 0xA6, 0x6A, 0xEA, 0xFF ]
    , clev = cheat [ 0xB2, 0x26, 0xE2, 0x36, 0xA6, 0x6E, 1, 0, 0, 0xFF ]
    , mypos = cheat [ 0xB2, 0x26, 0xB6, 0xBA, 0x2A, 0xF6, 0xEA, 0xFF ]
    }


{-| SCRAMBLE (m\_cheat.h): the bits of a byte shuffled, as the sequences are stored.
-}
scramble : Int -> Int
scramble a =
    let
        bit mask =
            Bitwise.and a mask
    in
    Bitwise.shiftLeftBy 7 (bit 1)
        + Bitwise.shiftLeftBy 5 (bit 2)
        + bit 4
        + Bitwise.shiftLeftBy 1 (bit 8)
        + Bitwise.shiftRightBy 1 (bit 16)
        + bit 32
        + Bitwise.shiftRightBy 5 (bit 64)
        + Bitwise.shiftRightBy 7 (bit 128)


at : Int -> Cheat -> Int
at i c =
    Array.get i c.sequence |> Maybe.withDefault 0xFF


{-| cht\_CheckCheat: feed one key (as a `char`) to a sequence. A parameter slot takes
any key; a wrong key starts the sequence over (without trying the key as its first).
True when the key completes the sequence.
-}
checkCheat : Int -> Cheat -> ( Bool, Cheat )
checkCheat key c0 =
    let
        k =
            Int32.toByte key

        c1 =
            if at c0.p c0 == 0 then
                { sequence = Array.set c0.p k c0.sequence, p = c0.p + 1 }

            else if scramble k == at c0.p c0 then
                { c0 | p = c0.p + 1 }

            else
                { c0 | p = 0 }
    in
    if at c1.p c1 == 1 then
        ( False, { c1 | p = c1.p + 1 } )

    else if at c1.p c1 == 0xFF then
        ( True, { c1 | p = 0 } )

    else
        ( False, c1 )


{-| cht\_GetParam: the bytes typed into the parameter slots (as C's signed `char`s),
clearing the slots. Gives the first two.
-}
getParam : Cheat -> ( ( Int, Int ), Cheat )
getParam c =
    let
        firstSlot =
            findOne 0

        findOne i =
            if i >= Array.length c.sequence || at i c == 1 then
                i + 1

            else
                findOne (i + 1)

        copy i seq acc =
            let
                ch =
                    Array.get i seq |> Maybe.withDefault 0

                seq1 =
                    Array.set i 0 seq

                next =
                    Array.get (i + 1) seq1 |> Maybe.withDefault 0xFF
            in
            if ch /= 0 && next /= 0xFF then
                copy (i + 1) seq1 (ch :: acc)

            else
                ( List.reverse (ch :: acc), seq1, next == 0xFF )

        ( copied, sequence, ended ) =
            copy firstSlot c.sequence []

        buffer =
            if ended then
                copied ++ [ 0 ]

            else
                copied

        byte i =
            List.drop i buffer |> List.head |> Maybe.withDefault 0 |> Int32.toSignedByte
    in
    ( ( byte 0, byte 1 ), { c | sequence = sequence } )



-- ST_Responder


{-| What a cheat asks of the game, beyond the player changes the responder makes
itself:

  - `Warp skill episode map`: idclev's G\_DeferedInitNew.
  - `ChangeMusic lump`: idmus's S\_ChangeMusic (looping), with the lump's name
    ("D\_E1M1").
  - `BadMusic n`: idmus asked for music number `n`, which isn't one; the original's
    S\_ChangeMusic quits with I\_Error "Bad music number n".

-}
type Request
    = Warp Skill Int Int
    | ChangeMusic String
    | BadMusic Int


{-| What ST\_Responder did: the status bar and the world (the player's cheats and
message) after the event, and what it asks of the game, in order.
-}
type alias Responded =
    { model : Model
    , world : World
    , requests : List Request
    }


{-| ST\_Responder for the console player `n`: the cheats, typed as key presses. It never
takes the event (C returns false), so the event goes on to the automap's responder.

The cheats work at any skill (linuxdoom-1.10 has the nightmare check commented out);
all but idclev are off in a netgame. As in C, a key that completes one cheat in the
chain god, fa, kfa, mus, noclip isn't seen by the later ones, nor is a key that
completes idbehold seen by idchoppers and idmypos.

-}
responder : Event -> Env -> World -> Int -> Model -> Responded
responder event env world n (Model st) =
    case event of
        KeyDown key ->
            let
                c0 =
                    st.cheats

                ( c1, world1, reqs1 ) =
                    if env.params.netgame then
                        ( c0, world, [] )

                    else
                        singlePlayerCheats key env n c0 world

                ( clevDone, clev ) =
                    checkCheat key c1.clev

                ( c2, world2, reqs2 ) =
                    if clevDone then
                        let
                            ( ( b0, b1 ), clev1 ) =
                                getParam clev

                            ( epsd, map ) =
                                if env.params.gameMode == Commercial then
                                    ( 0, (b0 - 48) * 10 + b1 - 48 )

                                else
                                    ( b0 - 48, b1 - 48 )

                            valid =
                                epsd
                                    >= 1
                                    && map
                                    >= 1
                                    && (case env.params.gameMode of
                                            Retail ->
                                                epsd <= 4 && map <= 9

                                            Registered ->
                                                epsd <= 3 && map <= 9

                                            Shareware ->
                                                epsd <= 1 && map <= 9

                                            Commercial ->
                                                epsd <= 1 && map <= 34
                                       )

                            c =
                                { c1 | clev = clev1 }
                        in
                        if valid then
                            ( c, setMessage n "Changing Level..." world1, reqs1 ++ [ Warp env.params.skill epsd map ] )

                        else
                            ( c, world1, reqs1 )

                    else
                        ( { c1 | clev = clev }, world1, reqs1 )
            in
            { model = Model { st | cheats = c2 }, world = world2, requests = reqs2 }

        _ ->
            { model = Model st, world = world, requests = [] }


singlePlayerCheats : Int -> Env -> Int -> Cheats -> World -> ( Cheats, World, List Request )
singlePlayerCheats key env n c0 world0 =
    let
        -- the else-if chain: god, fa, kfa, mus, noclip
        ( c1, world1, reqs ) =
            firstCheat key env n c0 world0

        -- the power-ups, each one checked
        ( c2, world2 ) =
            List.foldl
                (\( i, power ) ( c, w ) ->
                    let
                        ( done, seq ) =
                            checkCheat key (Array.get i c.powerup |> Maybe.withDefault (cheat [ 0xFF ]))

                        c_ =
                            { c | powerup = Array.set i seq c.powerup }
                    in
                    if done then
                        ( c_, togglePower env n power w |> setMessage n "Power-up Toggled" )

                    else
                        ( c_, w )
                )
                ( c1, world1 )
                [ ( 0, Invulnerability ), ( 1, Strength ), ( 2, Invisibility ), ( 3, IronFeet ), ( 4, AllMap ), ( 5, Infrared ) ]

        ( beholdDone, behold ) =
            checkCheat key (Array.get 6 c2.powerup |> Maybe.withDefault (cheat [ 0xFF ]))

        c3 =
            { c2 | powerup = Array.set 6 behold c2.powerup }
    in
    if beholdDone then
        ( c3, setMessage n "inVuln, Str, Inviso, Rad, Allmap, or Lite-amp" world2, reqs )

    else
        let
            ( choppersDone, choppers ) =
                checkCheat key c3.choppers

            c4 =
                { c3 | choppers = choppers }
        in
        if choppersDone then
            ( c4
            , world2
                |> World.updatePlayer n
                    (\p ->
                        p
                            |> Player.setWeaponOwned Chainsaw True
                            |> Player.setPower Invulnerability 1
                            |> (\q -> { q | message = Just "... doesn't suck - GM" })
                    )
            , reqs
            )

        else
            let
                ( myposDone, mypos ) =
                    checkCheat key c4.mypos

                c5 =
                    { c4 | mypos = mypos }
            in
            if myposDone then
                ( c5, setMessage n (myposMessage world2 n) world2, reqs )

            else
                ( c5, world2, reqs )


firstCheat : Int -> Env -> Int -> Cheats -> World -> ( Cheats, World, List Request )
firstCheat key env n c0 world =
    let
        ( godDone, god ) =
            checkCheat key c0.god

        c1 =
            { c0 | god = god }
    in
    if godDone then
        ( c1, toggleGod n world, [] )

    else
        let
            ( faDone, ammonokey ) =
                checkCheat key c1.ammonokey

            c2 =
                { c1 | ammonokey = ammonokey }
        in
        if faDone then
            ( c2, world |> World.updatePlayer n (giveArsenal >> withMessage "Ammo (no keys) Added"), [] )

        else
            let
                ( kfaDone, ammo ) =
                    checkCheat key c2.ammo

                c3 =
                    { c2 | ammo = ammo }
            in
            if kfaDone then
                ( c3
                , world
                    |> World.updatePlayer n
                        (giveArsenal
                            >> (\p -> { p | cards = Array.repeat 6 True })
                            >> withMessage "Very Happy Ammo Added"
                        )
                , []
                )

            else
                let
                    ( musDone, mus ) =
                        checkCheat key c3.mus

                    c4 =
                        { c3 | mus = mus }
                in
                if musDone then
                    let
                        ( ( b0, b1 ), mus1 ) =
                            getParam mus

                        ( index, musnum ) =
                            if env.params.gameMode == Commercial then
                                let
                                    i =
                                        (b0 - 48) * 10 + b1 - 48
                                in
                                ( i, i + 32 )

                            else
                                let
                                    i =
                                        (b0 - 49) * 9 + (b1 - 49)
                                in
                                ( i, i + 1 )

                        tooBig =
                            if env.params.gameMode == Commercial then
                                index > 35

                            else
                                index > 31

                        c =
                            { c4 | mus = mus1 }
                    in
                    if tooBig then
                        ( c, setMessage n "IMPOSSIBLE SELECTION" world, [] )

                    else
                        ( c, setMessage n "Music Change" world, [ musicRequest musnum ] )

                else
                    let
                        ( noclipDone, noclip ) =
                            checkCheat key c4.noclip

                        ( commercialDone, commercialNoclip ) =
                            if noclipDone then
                                ( False, c4.commercialNoclip )

                            else
                                checkCheat key c4.commercialNoclip

                        c5 =
                            { c4 | noclip = noclip, commercialNoclip = commercialNoclip }
                    in
                    if noclipDone || commercialDone then
                        ( c5, toggleNoclip n world, [] )

                    else
                        ( c5, world, [] )


toggleGod : Int -> World -> World
toggleGod n world =
    case World.player n world of
        Nothing ->
            world

        Just p ->
            let
                cheats =
                    Bitwise.xor p.cheats Defs.cfGodmode
            in
            if Bitwise.and cheats Defs.cfGodmode /= 0 then
                let
                    withBody =
                        case p.mo of
                            Just mo ->
                                World.updateMobj mo (\m -> { m | health = 100 }) world

                            Nothing ->
                                world
                in
                World.updatePlayer n (\q -> { q | cheats = cheats, health = 100, message = Just "Degreelessness Mode On" }) withBody

            else
                World.updatePlayer n (\q -> { q | cheats = cheats, message = Just "Degreelessness Mode Off" }) world


toggleNoclip : Int -> World -> World
toggleNoclip n =
    World.updatePlayer n
        (\p ->
            let
                cheats =
                    Bitwise.xor p.cheats Defs.cfNoclip
            in
            { p
                | cheats = cheats
                , message =
                    Just
                        (if Bitwise.and cheats Defs.cfNoclip /= 0 then
                            "No Clipping Mode ON"

                         else
                            "No Clipping Mode OFF"
                        )
            }
        )


{-| idfa (and the start of idkfa): 200% blue armour, every weapon, full ammo.
-}
giveArsenal : Player -> Player
giveArsenal p =
    { p
        | armorpoints = 200
        , armortype = 2
        , weaponowned = Array.repeat 9 True
        , ammo = p.maxammo
    }


{-| An idbehold power-up: given if the player hasn't got it, otherwise run out (berserk
is taken away at once, the others end on the next tic).
-}
togglePower : Env -> Int -> Power -> World -> World
togglePower env n power world =
    case World.player n world of
        Nothing ->
            world

        Just p ->
            if Player.power power p == 0 then
                Inter.givePower env n power world |> Tuple.second

            else if power /= Strength then
                World.updatePlayer n (Player.setPower power 1) world

            else
                World.updatePlayer n (Player.setPower power 0) world


musicRequest : Int -> Request
musicRequest musnum =
    case List.drop musnum musicNames |> List.head of
        Just name ->
            if musnum <= 0 then
                BadMusic musnum

            else
                ChangeMusic ("D_" ++ String.toUpper name)

        Nothing ->
            BadMusic musnum


{-| S\_music[]'s names, by musicenum\_t (mus\_None first).
-}
musicNames : List String
musicNames =
    [ ""
    , "e1m1"
    , "e1m2"
    , "e1m3"
    , "e1m4"
    , "e1m5"
    , "e1m6"
    , "e1m7"
    , "e1m8"
    , "e1m9"
    , "e2m1"
    , "e2m2"
    , "e2m3"
    , "e2m4"
    , "e2m5"
    , "e2m6"
    , "e2m7"
    , "e2m8"
    , "e2m9"
    , "e3m1"
    , "e3m2"
    , "e3m3"
    , "e3m4"
    , "e3m5"
    , "e3m6"
    , "e3m7"
    , "e3m8"
    , "e3m9"
    , "inter"
    , "intro"
    , "bunny"
    , "victor"
    , "introa"
    , "runnin"
    , "stalks"
    , "countd"
    , "betwee"
    , "doom"
    , "the_da"
    , "shawn"
    , "ddtblu"
    , "in_cit"
    , "dead"
    , "stlks2"
    , "theda2"
    , "doom2"
    , "ddtbl2"
    , "runni2"
    , "dead2"
    , "stlks3"
    , "romero"
    , "shawn2"
    , "messag"
    , "count2"
    , "ddtbl3"
    , "ampie"
    , "theda3"
    , "adrian"
    , "messg2"
    , "romer2"
    , "tense"
    , "shawn3"
    , "openin"
    , "evil"
    , "ultima"
    , "read_m"
    , "dm2ttl"
    , "dm2int"
    ]


{-| idmypos: "ang=0x%x;x,y=(0x%x,0x%x)" of the player's body.
-}
myposMessage : World -> Int -> String
myposMessage world n =
    case World.playerMobj n world of
        Just mo ->
            "ang=0x" ++ hex mo.angle ++ ";x,y=(0x" ++ hex mo.x ++ ",0x" ++ hex mo.y ++ ")"

        Nothing ->
            "ang=0x0;x,y=(0x0,0x0)"


{-| printf's %x: lower-case hexadecimal of the value as an unsigned 32-bit number.
-}
hex : Int -> String
hex value =
    let
        go v acc =
            let
                d =
                    modBy 16 v

                ch =
                    String.slice d (d + 1) "0123456789abcdef"
            in
            if v < 16 then
                ch ++ acc

            else
                go (v // 16) (ch ++ acc)
    in
    go (Int32.toUnsigned value) ""


setMessage : Int -> String -> World -> World
setMessage n text =
    World.updatePlayer n (withMessage text)


withMessage : String -> Player -> Player
withMessage text p =
    { p | message = Just text }
