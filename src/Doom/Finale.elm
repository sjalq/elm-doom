module Doom.Finale exposing
    ( Model, Tick, Music
    , start, ticker, responder, drawer
    , music, finalestage, finalecount, wipeRequested
    )

{-| The end of an episode (f\_finale.c): the story text typing itself out over a
tiled flat, then the art screens (Doom 1's last pictures, the bunny scroll and "THE
END" after episode 3), and Doom II's cast call.

Sounds have no origin (S\_StartSound(NULL, ...)): a host plays them as menu sounds.
Each draws its pitch variation from M\_Random as it starts (`StartDrawn`).
The bunny scroll's pistol shots are made by F\_BunnyScroll, which is drawing code in
the original; here the ticker makes them, on the same tics, since the original draws
once per tic.

@docs Model, Tick, Music
@docs start, ticker, responder, drawer
@docs music, finalestage, finalecount, wipeRequested

-}

import Array exposing (Array)
import Bitwise
import Doom.Defs exposing (GameMode(..))
import Doom.Event exposing (Event(..))
import Doom.Info.MobjType as MobjType exposing (MobjType)
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Info.Sprite as Sprite
import Doom.Info.State as State exposing (State)
import Doom.Random as Random exposing (Rng)
import Doom.Render.SpriteFrames as SpriteFrames
import Doom.Sound as Sound
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)


{-| The finale's state: f\_finale.c's statics.
-}
type Model
    = Model Internal


type alias Internal =
    { gameMode : GameMode
    , episode : Int
    , map : Int
    , text : String
    , flat : String
    , stage : Stage
    , count : Int
    , laststage : Int
    , wipe : Bool
    }


type Stage
    = Text
    | Art
    | Cast CastState


{-| The cast call: which monster (`castnum`, into `castorder`), its state and how long
is left in it, whether it is dying, how many frames it has walked or attacked, and
whether its next attack is the melee one.
-}
type alias CastState =
    { castnum : Int
    , casttics : Int
    , caststate : State
    , castdeath : Bool
    , castframes : Int
    , castonmelee : Bool
    , castattacking : Bool
    }


{-| What a tic did: the new state, the sounds it started (each `StartDrawn`, its
pitch variation already drawn), the M\_Random state after those draws, whether the
game moves on to the next level (`gameaction = ga_worlddone`, Doom II after a button
press), and whether the screen should melt to the next picture (`wipegamestate =
-1`).
-}
type alias Tick =
    { model : Model
    , sounds : List Sound.Event
    , rng : Rng
    , worldDone : Bool
    , wipe : Bool
    }


{-| S\_StartSound(NULL, sfx) for each, in order: s\_sound.c draws each one's pitch
variation from M\_Random as it starts it.
-}
startSounds : List Sfx -> Rng -> ( List Sound.Event, Rng )
startSounds sounds rng0 =
    List.foldl
        (\sfx ( acc, rng ) ->
            if Sound.drawsPitch sfx then
                Random.mRandom rng |> Tuple.mapFirst (\byte -> Sound.StartDrawn Sound.Ambient sfx byte :: acc)

            else
                ( Sound.StartDrawn Sound.Ambient sfx 0 :: acc, rng )
        )
        ( [], rng0 )
        sounds
        |> Tuple.mapFirst List.reverse


{-| A music lump and whether it loops.
-}
type alias Music =
    { lump : String
    , looping : Bool
    }


{-| F\_StartFinale gamemode episode map: the story text and flat for the episode (or
Doom II map) just finished.
-}
start : GameMode -> Int -> Int -> Model
start mode episode map =
    let
        ( flat, text ) =
            if mode == Commercial then
                case map of
                    6 ->
                        ( "SLIME16", c1text )

                    11 ->
                        ( "RROCK14", c2text )

                    20 ->
                        ( "RROCK07", c3text )

                    30 ->
                        ( "RROCK17", c4text )

                    15 ->
                        ( "RROCK13", c5text )

                    31 ->
                        ( "RROCK19", c6text )

                    _ ->
                        ( "", "" )

            else
                case episode of
                    1 ->
                        ( "FLOOR4_8", e1text )

                    2 ->
                        ( "SFLR6_1", e2text )

                    3 ->
                        ( "MFLR8_4", e3text )

                    4 ->
                        ( "MFLR8_3", e4text )

                    _ ->
                        ( "", "" )
    in
    Model
        { gameMode = mode
        , episode = episode
        , map = map
        , text = text
        , flat = flat
        , stage = Text
        , count = 0
        , laststage = 0
        , wipe = False
        }


{-| The music the finale has started, as S\_ChangeMusic would play it (doing nothing
if it is already playing): D\_VICTOR in Doom 1, D\_READ\_M in Doom II, D\_BUNNY once
for the bunny, D\_EVIL for the cast.
-}
music : Model -> Music
music (Model m) =
    case m.stage of
        Cast _ ->
            { lump = "D_EVIL", looping = True }

        Art ->
            if m.episode == 3 then
                { lump = "D_BUNNY", looping = False }

            else
                { lump = "D_VICTOR", looping = True }

        Text ->
            if m.gameMode == Commercial then
                { lump = "D_READ_M", looping = True }

            else
                { lump = "D_VICTOR", looping = True }


{-| `finalestage`: 0 the text, 1 the art screen, 2 the cast.
-}
finalestage : Model -> Int
finalestage (Model m) =
    case m.stage of
        Text ->
            0

        Art ->
            1

        Cast _ ->
            2


{-| `finalecount`: tics since the finale (or its art screen) started.
-}
finalecount : Model -> Int
finalecount (Model m) =
    m.count


textSpeed : Int
textSpeed =
    3


textWait : Int
textWait =
    250



-- TICKING


{-| F\_Ticker, given whether any player's ticcmd has a button pressed. In Doom II,
after 50 tics a button moves on: to the cast after MAP30 (again on every tic it is
held), to the next level otherwise. In Doom 1 the text gives way to the art screen
once it has all been typed and read. The sounds it starts draw from M\_Random
(`rng`, `rndindex`) for their pitch.
-}
ticker : Bool -> Rng -> Model -> Tick
ticker anyButton rng model =
    let
        t =
            step anyButton model

        (Model m) =
            t.model

        ( sounds, rngOut ) =
            startSounds t.sfx rng
    in
    { model = Model { m | wipe = t.wipe }
    , sounds = sounds
    , rng = rngOut
    , worldDone = t.worldDone
    , wipe = t.wipe
    }


type alias Step =
    { model : Model
    , sfx : List Sfx
    , worldDone : Bool
    , wipe : Bool
    }


{-| Whether the last tic asked for the screen to melt (`wipegamestate = -1`): when
the art screen or the cast starts.
-}
wipeRequested : Model -> Bool
wipeRequested (Model m) =
    m.wipe


step : Bool -> Model -> Step
step anyButton (Model m0) =
    let
        skipping =
            m0.gameMode == Commercial && m0.count > 50 && anyButton

        ( m1, worldDone, castWipe ) =
            if skipping then
                if m0.map == 30 then
                    ( { m0 | stage = Cast startCast }, False, True )

                else
                    ( m0, True, False )

            else
                ( m0, False, False )

        m =
            { m1 | count = m1.count + 1 }
    in
    case m.stage of
        Cast cast ->
            let
                ( next, sounds ) =
                    castTicker cast
            in
            { model = Model { m | stage = Cast next }
            , sfx = sounds
            , worldDone = worldDone
            , wipe = castWipe
            }

        Text ->
            if m.gameMode /= Commercial && m.count > String.length m.text * textSpeed + textWait then
                { model = Model { m | count = 0, stage = Art }, sfx = [], worldDone = worldDone, wipe = True }

            else
                { model = Model m, sfx = [], worldDone = worldDone, wipe = castWipe }

        Art ->
            let
                ( laststage, sounds ) =
                    bunnySounds m
            in
            { model = Model { m | laststage = laststage }
            , sfx = sounds
            , worldDone = worldDone
            , wipe = False
            }


{-| F\_BunnyScroll's `laststage`: "THE END" is shot onto the screen a letter every 5
tics, with a pistol shot each.
-}
bunnySounds : Internal -> ( Int, List Sfx )
bunnySounds m =
    if m.gameMode == Commercial || m.episode /= 3 || m.count < 1130 then
        ( m.laststage, [] )

    else if m.count < 1180 then
        ( 0, [] )

    else
        let
            stage =
                min 6 ((m.count - 1180) // 5)
        in
        if stage > m.laststage then
            ( stage, [ Sfx.Pistol ] )

        else
            ( m.laststage, [] )



-- THE CAST


{-| castorder: the cast in order of appearance.
-}
castorder : Array ( String, MobjType )
castorder =
    Array.fromList
        [ ( "ZOMBIEMAN", MobjType.Possessed )
        , ( "SHOTGUN GUY", MobjType.Shotguy )
        , ( "HEAVY WEAPON DUDE", MobjType.Chainguy )
        , ( "IMP", MobjType.Troop )
        , ( "DEMON", MobjType.Sergeant )
        , ( "LOST SOUL", MobjType.Skull )
        , ( "CACODEMON", MobjType.Head )
        , ( "HELL KNIGHT", MobjType.Knight )
        , ( "BARON OF HELL", MobjType.Bruiser )
        , ( "ARACHNOTRON", MobjType.Baby )
        , ( "PAIN ELEMENTAL", MobjType.Pain )
        , ( "REVENANT", MobjType.Undead )
        , ( "MANCUBUS", MobjType.Fatso )
        , ( "ARCH-VILE", MobjType.Vile )
        , ( "THE SPIDER MASTERMIND", MobjType.Spider )
        , ( "THE CYBERDEMON", MobjType.Cyborg )
        , ( "OUR HERO", MobjType.Player )
        ]


castMember : Int -> ( String, MobjType )
castMember n =
    Array.get n castorder |> Maybe.withDefault ( "ZOMBIEMAN", MobjType.Possessed )


castInfo : CastState -> MobjType.Info
castInfo cast =
    MobjType.info (Tuple.second (castMember cast.castnum))


{-| F\_StartCast: the zombieman, walking.
-}
startCast : CastState
startCast =
    let
        see =
            (MobjType.info MobjType.Possessed).seestate
    in
    { castnum = 0
    , casttics = (State.def see).tics
    , caststate = see
    , castdeath = False
    , castframes = 0
    , castonmelee = False
    , castattacking = False
    }


{-| F\_CastTicker: when the state's time is up, the next state, or after a death the
next monster. After 12 frames of walking it attacks (melee and missile in turn), and
walks again after 12 more or when the attack is over.
-}
castTicker : CastState -> ( CastState, List Sfx )
castTicker cast0 =
    let
        cast1 =
            { cast0 | casttics = cast0.casttics - 1 }

        current =
            State.def cast1.caststate
    in
    if cast1.casttics > 0 then
        ( cast1, [] )

    else if current.tics == -1 || current.next == State.Null then
        -- on to the next monster
        let
            castnum =
                if cast1.castnum + 1 >= Array.length castorder then
                    0

                else
                    cast1.castnum + 1

            info =
                MobjType.info (Tuple.second (castMember castnum))

            seesound =
                if info.seesound /= Sfx.None then
                    [ info.seesound ]

                else
                    []
        in
        ( { cast1 | castnum = castnum, castdeath = False, caststate = info.seestate, castframes = 0 }
            |> attackOrWalk
            |> setTics
        , seesound
        )

    else if cast1.caststate == State.PlayAtk1 then
        -- "Oh, gross hack!": the player's attack goes straight back to walking
        ( stopAttack cast1 |> setTics, [] )

    else
        let
            st =
                current.next
        in
        ( { cast1 | caststate = st, castframes = cast1.castframes + 1 }
            |> attackOrWalk
            |> setTics
        , attackSound st
        )


attackOrWalk : CastState -> CastState
attackOrWalk cast =
    let
        info =
            castInfo cast

        attacking =
            if cast.castframes == 12 then
                let
                    pick melee =
                        if melee then
                            info.meleestate

                        else
                            info.missilestate

                    first =
                        pick cast.castonmelee

                    toggled =
                        not cast.castonmelee
                in
                { cast
                    | castattacking = True
                    , castonmelee = toggled
                    , caststate =
                        if first == State.Null then
                            pick toggled

                        else
                            first
                }

            else
                cast
    in
    if attacking.castattacking && (attacking.castframes == 24 || attacking.caststate == info.seestate) then
        stopAttack attacking

    else
        attacking


stopAttack : CastState -> CastState
stopAttack cast =
    { cast | castattacking = False, castframes = 0, caststate = (castInfo cast).seestate }


setTics : CastState -> CastState
setTics cast =
    let
        tics =
            (State.def cast.caststate).tics
    in
    { cast
        | casttics =
            if tics == -1 then
                15

            else
                tics
    }


{-| F\_CastTicker's "sound hacks": the attacks' sounds, by the state the cast member
has just gone into.
-}
attackSound : State -> List Sfx
attackSound st =
    let
        sfx =
            case st of
                State.PlayAtk1 ->
                    Sfx.Dshtgn

                State.PossAtk2 ->
                    Sfx.Pistol

                State.SposAtk2 ->
                    Sfx.Shotgn

                State.VileAtk2 ->
                    Sfx.Vilatk

                State.SkelFist2 ->
                    Sfx.Skeswg

                State.SkelFist4 ->
                    Sfx.Skepch

                State.SkelMiss2 ->
                    Sfx.Skeatk

                State.FattAtk8 ->
                    Sfx.Firsht

                State.FattAtk5 ->
                    Sfx.Firsht

                State.FattAtk2 ->
                    Sfx.Firsht

                State.CposAtk2 ->
                    Sfx.Shotgn

                State.CposAtk3 ->
                    Sfx.Shotgn

                State.CposAtk4 ->
                    Sfx.Shotgn

                State.TrooAtk3 ->
                    Sfx.Claw

                State.SargAtk2 ->
                    Sfx.Sgtatk

                State.BossAtk2 ->
                    Sfx.Firsht

                State.Bos2Atk2 ->
                    Sfx.Firsht

                State.HeadAtk2 ->
                    Sfx.Firsht

                State.SkullAtk2 ->
                    Sfx.Sklatk

                State.SpidAtk2 ->
                    Sfx.Shotgn

                State.SpidAtk3 ->
                    Sfx.Shotgn

                State.BspiAtk2 ->
                    Sfx.Plasma

                State.CyberAtk2 ->
                    Sfx.Rlaunc

                State.CyberAtk4 ->
                    Sfx.Rlaunc

                State.CyberAtk6 ->
                    Sfx.Rlaunc

                State.PainAtk3 ->
                    Sfx.Sklatk

                _ ->
                    Sfx.None
    in
    if sfx == Sfx.None then
        []

    else
        [ sfx ]


{-| F\_Responder: during the cast, a key press kills the monster on show (and is
eaten, as is every key press while it dies). Nothing when the finale doesn't take the
event. The death sound draws from M\_Random for its pitch, so the M\_Random state goes
in and comes back.
-}
responder : Event -> Rng -> Model -> Maybe ( Model, List Sound.Event, Rng )
responder event rng model =
    respond event model
        |> Maybe.map
            (\( m, sfx ) ->
                let
                    ( sounds, rngOut ) =
                        startSounds sfx rng
                in
                ( m, sounds, rngOut )
            )


respond : Event -> Model -> Maybe ( Model, List Sfx )
respond event (Model m) =
    case ( m.stage, event ) of
        ( Cast cast, KeyDown _ ) ->
            if cast.castdeath then
                Just ( Model m, [] )

            else
                let
                    info =
                        castInfo cast

                    death =
                        info.deathstate
                in
                Just
                    ( Model
                        { m
                            | stage =
                                Cast
                                    { cast
                                        | castdeath = True
                                        , caststate = death
                                        , casttics = (State.def death).tics
                                        , castframes = 0
                                        , castattacking = False
                                    }
                        }
                    , if info.deathsound /= Sfx.None then
                        [ info.deathsound ]

                      else
                        []
                    )

        _ ->
            Nothing



-- DRAWING


{-| F\_Drawer. The art screens and the cast are drawn over what was on the screen;
the text screen covers it all.
-}
drawer : Wad -> Model -> Framebuffer -> Framebuffer
drawer wad (Model m) fb =
    let
        picture name =
            Wad.lump name wad |> Maybe.andThen Video.patch

        drawAt x y maybePatch screen =
            case maybePatch of
                Just pt ->
                    Video.drawPatch x y pt screen

                Nothing ->
                    screen
    in
    case m.stage of
        Cast cast ->
            castDrawer wad cast fb

        Text ->
            textWrite wad m

        Art ->
            case m.episode of
                1 ->
                    if m.gameMode == Retail then
                        drawAt 0 0 (picture "CREDIT") fb

                    else
                        drawAt 0 0 (picture "HELP2") fb

                2 ->
                    drawAt 0 0 (picture "VICTORY2") fb

                3 ->
                    bunnyScroll wad m fb

                4 ->
                    drawAt 0 0 (picture "ENDPIC") fb

                _ ->
                    fb


{-| hu\_font: STCFN033 to STCFN095, the characters `!` to `_`. Lower case is drawn
as upper case; anything else is a 4-pixel space.
-}
type Font
    = Font (Array (Maybe Patch))


font : Wad -> Font
font wad =
    Font
        (Array.initialize 63
            (\i -> Wad.lump ("STCFN" ++ String.padLeft 3 '0' (String.fromInt (i + 33))) wad |> Maybe.andThen Video.patch)
        )


fontPatch : Font -> Int -> Maybe Patch
fontPatch (Font glyphs) i =
    Array.get i glyphs |> Maybe.andThen identity


{-| A character's place in the font: `toupper(c) - HU_FONTSTART`, Nothing for a
space.
-}
glyph : Char -> Maybe Int
glyph c =
    let
        code =
            Char.toCode (Char.toUpper c) - 33
    in
    if Char.toCode c > 127 || code < 0 || code >= 63 then
        Nothing

    else
        Just code


{-| F\_TextWrite: the flat tiled over the screen and as much of the text as has been
typed, a character every 3 tics after the first 10.
-}
textWrite : Wad -> Internal -> Framebuffer
textWrite wad m =
    let
        background =
            case Wad.lump m.flat wad of
                Just flat ->
                    Video.tileFlat 0 0 Video.width Video.height flat (Video.blank 0)

                Nothing ->
                    Video.blank 0

        hu =
            font wad

        shown =
            max 0 ((m.count - 10) // textSpeed)

        go chars n cx cy fb =
            case chars of
                [] ->
                    fb

                c :: rest ->
                    if n <= 0 then
                        fb

                    else if c == '\n' then
                        go rest (n - 1) 10 (cy + 11) fb

                    else
                        case glyph c |> Maybe.andThen (fontPatch hu) of
                            Nothing ->
                                go rest (n - 1) (cx + 4) cy fb

                            Just pt ->
                                let
                                    w =
                                        (Video.patchSize pt).width
                                in
                                if cx + w > Video.width then
                                    fb

                                else
                                    go rest (n - 1) (cx + w) cy (Video.drawPatch cx cy pt fb)
    in
    go (String.toList m.text) shown 10 10 background


{-| F\_BunnyScroll: the two halves of the panorama (PFUB2 then PFUB1) scrolling by,
and from tic 1130 "THE END" (END0 to END6).
-}
bunnyScroll : Wad -> Internal -> Framebuffer -> Framebuffer
bunnyScroll wad m fb =
    let
        picture name =
            Wad.lump name wad |> Maybe.andThen Video.patch

        scrolled =
            clamp 0 320 (320 - (m.count - 230) // 2)

        -- F_DrawPatchCol draws a picture's columns from the top of the screen,
        -- ignoring its offsets: column x + scrolled of PFUB2 at x while that is on
        -- it, then PFUB1's from its left edge
        column x0 maybePatch screen =
            case maybePatch of
                Just pt ->
                    let
                        s =
                            Video.patchSize pt
                    in
                    Video.drawPatch (x0 + s.leftoffset) s.topoffset pt screen

                Nothing ->
                    screen

        panorama =
            fb
                |> column (negate scrolled) (picture "PFUB2")
                |> column (320 - scrolled) (picture "PFUB1")

        theEnd name =
            case picture name of
                Just pt ->
                    Video.drawPatch ((Video.width - 13 * 8) // 2) ((Video.height - 8 * 8) // 2) pt panorama

                Nothing ->
                    panorama
    in
    if m.count < 1130 then
        panorama

    else if m.count < 1180 then
        theEnd "END0"

    else
        theEnd ("END" ++ String.fromInt (min 6 ((m.count - 1180) // 5)))


{-| F\_CastDrawer: BOSSBACK, the monster's name, and its current frame seen from the
front in the middle.
-}
castDrawer : Wad -> CastState -> Framebuffer -> Framebuffer
castDrawer wad cast fb =
    let
        withBack =
            case Wad.lump "BOSSBACK" wad |> Maybe.andThen Video.patch of
                Just pt ->
                    Video.drawPatch 0 0 pt fb

                Nothing ->
                    fb

        named =
            castPrint (font wad) (Tuple.first (castMember cast.castnum)) withBack

        def =
            State.def cast.caststate

        prefix =
            Sprite.name def.sprite

        spriteLumps =
            Wad.between "S_START" "S_END" wad |> Array.fromList

        frames =
            spriteLumps
                |> Array.toList
                |> List.map
                    (\l ->
                        if String.startsWith prefix (String.toUpper l.name) then
                            l.name

                        else
                            ""
                    )
                |> SpriteFrames.build

        rotation =
            SpriteFrames.frame def.sprite (Bitwise.and def.frame 0x7FFF) frames
                |> Maybe.andThen (\fr -> Array.get 0 fr.rotations)
    in
    case rotation of
        Just rot ->
            case Array.get rot.lump spriteLumps |> Maybe.andThen (.bytes >> Video.patch) of
                Just pt ->
                    if rot.flip then
                        Video.drawPatchFlipped 160 170 pt named

                    else
                        Video.drawPatch 160 170 pt named

                Nothing ->
                    named

        Nothing ->
            named


{-| F\_CastPrint: a line of text centred at the bottom of the screen.
-}
castPrint : Font -> String -> Framebuffer -> Framebuffer
castPrint hu text fb =
    let
        chars =
            String.toList text

        advance c =
            case glyph c |> Maybe.andThen (fontPatch hu) of
                Just pt ->
                    ( (Video.patchSize pt).width, Just pt )

                Nothing ->
                    ( 4, Nothing )

        width =
            List.foldl (\c acc -> acc + Tuple.first (advance c)) 0 chars
    in
    List.foldl
        (\c ( cx, screen ) ->
            case advance c of
                ( w, Just pt ) ->
                    ( cx + w, Video.drawPatch cx 180 pt screen )

                ( w, Nothing ) ->
                    ( cx + w, screen )
        )
        ( 160 - width // 2, fb )
        chars
        |> Tuple.second



-- THE TEXTS (d_englsh.h)


e1text : String
e1text =
    "Once you beat the big badasses and\nclean out the moon base you're supposed\nto win, aren't you? Aren't you? Where's\nyour fat reward and ticket home? What\nthe hell is this? It's not supposed to\nend this way!\n\nIt stinks like rotten meat, but looks\nlike the lost Deimos base.  Looks like\nyou're stuck on The Shores of Hell.\nThe only way out is through.\n\nTo continue the DOOM experience, play\nThe Shores of Hell and its amazing\nsequel, Inferno!\n"


e2text : String
e2text =
    "You've done it! The hideous cyber-\ndemon lord that ruled the lost Deimos\nmoon base has been slain and you\nare triumphant! But ... where are\nyou? You clamber to the edge of the\nmoon and look down to see the awful\ntruth.\n\nDeimos floats above Hell itself!\nYou've never heard of anyone escaping\nfrom Hell, but you'll make the bastards\nsorry they ever heard of you! Quickly,\nyou rappel down to  the surface of\nHell.\n\nNow, it's on to the final chapter of\nDOOM! -- Inferno."


e3text : String
e3text =
    "The loathsome spiderdemon that\nmasterminded the invasion of the moon\nbases and caused so much death has had\nits ass kicked for all time.\n\nA hidden doorway opens and you enter.\nYou've proven too tough for Hell to\ncontain, and now Hell at last plays\nfair -- for you emerge from the door\nto see the green fields of Earth!\nHome at last.\n\nYou wonder what's been happening on\nEarth while you were battling evil\nunleashed. It's good that no Hell-\nspawn could have come through that\ndoor with you ..."


e4text : String
e4text =
    "the spider mastermind must have sent forth\nits legions of hellspawn before your\nfinal confrontation with that terrible\nbeast from hell.  but you stepped forward\nand brought forth eternal damnation and\nsuffering upon the horde as a true hero\nwould in the face of something so evil.\n\nbesides, someone was gonna pay for what\nhappened to daisy, your pet rabbit.\n\nbut now, you see spread before you more\npotential pain and gibbitude as a nation\nof demons run amok among our cities.\n\nnext stop, hell on earth!"


c1text : String
c1text =
    "YOU HAVE ENTERED DEEPLY INTO THE INFESTED\nSTARPORT. BUT SOMETHING IS WRONG. THE\nMONSTERS HAVE BROUGHT THEIR OWN REALITY\nWITH THEM, AND THE STARPORT'S TECHNOLOGY\nIS BEING SUBVERTED BY THEIR PRESENCE.\n\nAHEAD, YOU SEE AN OUTPOST OF HELL, A\nFORTIFIED ZONE. IF YOU CAN GET PAST IT,\nYOU CAN PENETRATE INTO THE HAUNTED HEART\nOF THE STARBASE AND FIND THE CONTROLLING\nSWITCH WHICH HOLDS EARTH'S POPULATION\nHOSTAGE."


c2text : String
c2text =
    "YOU HAVE WON! YOUR VICTORY HAS ENABLED\nHUMANKIND TO EVACUATE EARTH AND ESCAPE\nTHE NIGHTMARE.  NOW YOU ARE THE ONLY\nHUMAN LEFT ON THE FACE OF THE PLANET.\nCANNIBAL MUTATIONS, CARNIVOROUS ALIENS,\nAND EVIL SPIRITS ARE YOUR ONLY NEIGHBORS.\nYOU SIT BACK AND WAIT FOR DEATH, CONTENT\nTHAT YOU HAVE SAVED YOUR SPECIES.\n\nBUT THEN, EARTH CONTROL BEAMS DOWN A\nMESSAGE FROM SPACE: \"SENSORS HAVE LOCATED\nTHE SOURCE OF THE ALIEN INVASION. IF YOU\nGO THERE, YOU MAY BE ABLE TO BLOCK THEIR\nENTRY.  THE ALIEN BASE IS IN THE HEART OF\nYOUR OWN HOME CITY, NOT FAR FROM THE\nSTARPORT.\" SLOWLY AND PAINFULLY YOU GET\nUP AND RETURN TO THE FRAY."


c3text : String
c3text =
    "YOU ARE AT THE CORRUPT HEART OF THE CITY,\nSURROUNDED BY THE CORPSES OF YOUR ENEMIES.\nYOU SEE NO WAY TO DESTROY THE CREATURES'\nENTRYWAY ON THIS SIDE, SO YOU CLENCH YOUR\nTEETH AND PLUNGE THROUGH IT.\n\nTHERE MUST BE A WAY TO CLOSE IT ON THE\nOTHER SIDE. WHAT DO YOU CARE IF YOU'VE\nGOT TO GO THROUGH HELL TO GET TO IT?"


c4text : String
c4text =
    "THE HORRENDOUS VISAGE OF THE BIGGEST\nDEMON YOU'VE EVER SEEN CRUMBLES BEFORE\nYOU, AFTER YOU PUMP YOUR ROCKETS INTO\nHIS EXPOSED BRAIN. THE MONSTER SHRIVELS\nUP AND DIES, ITS THRASHING LIMBS\nDEVASTATING UNTOLD MILES OF HELL'S\nSURFACE.\n\nYOU'VE DONE IT. THE INVASION IS OVER.\nEARTH IS SAVED. HELL IS A WRECK. YOU\nWONDER WHERE BAD FOLKS WILL GO WHEN THEY\nDIE, NOW. WIPING THE SWEAT FROM YOUR\nFOREHEAD YOU BEGIN THE LONG TREK BACK\nHOME. REBUILDING EARTH OUGHT TO BE A\nLOT MORE FUN THAN RUINING IT WAS.\n"


c5text : String
c5text =
    "CONGRATULATIONS, YOU'VE FOUND THE SECRET\nLEVEL! LOOKS LIKE IT'S BEEN BUILT BY\nHUMANS, RATHER THAN DEMONS. YOU WONDER\nWHO THE INMATES OF THIS CORNER OF HELL\nWILL BE."


c6text : String
c6text =
    "CONGRATULATIONS, YOU'VE FOUND THE\nSUPER SECRET LEVEL!  YOU'D BETTER\nBLAZE THROUGH THIS ONE!\n"
