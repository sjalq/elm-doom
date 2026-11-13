module Doom.Intermission exposing
    ( WbStart, WbPlayer, Intermission, Stage(..), Outcome(..), Context, Statics
    , start, ticker, statics, initialStatics, music
    , Anim, AnimKind(..), animDefs
    , percent, fragSum
    )

{-| The intermission between levels (wi\_stuff.c's WI\_Start and WI\_Ticker; the
drawing is `Doom.Intermission.Draw`): the statistics counting up, the next level's
spot on the episode map, the background animations' timing, and the button presses
that hurry it along. Its timing matters to demos, which keep recording ticcmds through
it.

`wbs` is the `wbstartstruct_t` G\_DoCompleted fills in; the counters are what the
drawer shows.

The sounds the intermission makes (the pistol while counting, the explosion when a
count is done, the shotgun cock when moving on) go into `World.sounds` with no origin,
as S\_StartSound(NULL, ...) does, for a host to play as menu sounds; the music it
starts is `music`.

@docs WbStart, WbPlayer, Intermission, Stage, Outcome, Context, Statics
@docs start, ticker, statics, initialStatics, music
@docs Anim, AnimKind, animDefs
@docs percent, fragSum

-}

import Array exposing (Array)
import Bitwise
import Doom.Defs as Defs exposing (GameMode(..))
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Random as Random exposing (Rng)
import Doom.Sound as Sound
import Doom.Ticcmd as Ticcmd
import Doom.World as World exposing (World)


{-| wbstartstruct\_t: `epsd` and `last`/`next` are 0-based; `partime` in tics.
-}
type alias WbStart =
    { epsd : Int
    , didsecret : Bool
    , last : Int
    , next : Int
    , maxkills : Int
    , maxitems : Int
    , maxsecret : Int
    , maxfrags : Int
    , partime : Int
    , pnum : Int
    , plyr : Array WbPlayer
    }


{-| wbplayerstruct\_t.
-}
type alias WbPlayer =
    { in_ : Bool
    , skills : Int
    , sitems : Int
    , ssecret : Int
    , stime : Int
    , frags : Array Int
    }


{-| stateenum\_t.
-}
type Stage
    = StatCount
    | ShowNextLoc
    | NoState


{-| wi\_stuff.c's statics. `statState` is `sp_state`, `ng_state` or `dm_state` (which
one by the kind of game). `anims` are the background animations' `ctr` (the frame
showing, -1 for none) and `nexttic`, in the order of `animDefs`. `pointerOn` is
`snl_pointeron`: whether the "you are here" arrow shows.
-}
type alias Intermission =
    { ctx : Context
    , wbs : WbStart
    , stage : Stage
    , acceleratestage : Bool
    , cnt : Int
    , bcnt : Int
    , statState : Int
    , cntKills : Array Int
    , cntItems : Array Int
    , cntSecret : Array Int
    , cntFrags : Array Int
    , cntTime : Int
    , cntPar : Int
    , cntPause : Int
    , dofrags : Int
    , dmFrags : Array (Array Int)
    , dmTotals : Array Int
    , anims : Array AnimState
    , pointerOn : Bool
    }


type alias AnimState =
    { ctr : Int
    , nexttic : Int
    }


{-| What a tic of the intermission led to: carry on, or G\_WorldDone.
-}
type Outcome
    = Continue
    | WorldDone


{-| What the intermission needs to know about the game.
-}
type alias Context =
    { gameMode : GameMode
    , deathmatch : Bool
    , netgame : Bool
    , playeringame : Array Bool
    }


{-| The two statics of wi\_stuff.c that outlive an intermission: `dofrags` (which
WI\_initNetgameStats adds to rather than resets) and `snl_pointeron` (which the first
frame of the next episode map shows before the ticker sets it).
-}
type alias Statics =
    { dofrags : Int
    , pointerOn : Bool
    }


{-| The statics when the program starts.
-}
initialStatics : Statics
initialStatics =
    { dofrags = 0, pointerOn = False }


{-| The statics to carry over to the next intermission.
-}
statics : Intermission -> Statics
statics wi =
    { dofrags = wi.dofrags, pointerOn = wi.pointerOn }


{-| The music lump the intermission has started (WI\_Ticker starts it on its first
tic): D\_DM2INT in Doom II, D\_INTER otherwise, looping. A host plays it as
S\_ChangeMusic does, which does nothing if it is already playing.
-}
music : Intermission -> Maybe String
music wi =
    if wi.bcnt < 1 then
        Nothing

    else if wi.ctx.gameMode == Commercial then
        Just "D_DM2INT"

    else
        Just "D_INTER"



-- BACKGROUND ANIMATIONS


{-| animenum\_t: ANIM\_ALWAYS, ANIM\_RANDOM (with its period deviation and base
period, `data1` and `data2`) and ANIM\_LEVEL (shown when the next level is this one,
0-based).
-}
type AnimKind
    = Always
    | Random Int Int
    | OnLevel Int


{-| An anim\_t's fixed part: its kind, period in tics, number of frames, and where it
goes on the screen. Its pictures are WIAeeaaff (episode, animation, frame).
-}
type alias Anim =
    { kind : AnimKind
    , period : Int
    , nanims : Int
    , x : Int
    , y : Int
    }


{-| epsd0animinfo, epsd1animinfo and epsd2animinfo, by `wbs.epsd`: none for Doom II
or episode 4.
-}
animDefs : GameMode -> Int -> Array Anim
animDefs mode epsd =
    let
        third =
            Defs.ticrate // 3

        always period x y =
            { kind = Always, period = period, nanims = 3, x = x, y = y }

        level n nanims x y =
            { kind = OnLevel n, period = third, nanims = nanims, x = x, y = y }
    in
    if mode == Commercial then
        Array.empty

    else
        Array.fromList <|
            case epsd of
                0 ->
                    [ always third 224 104
                    , always third 184 160
                    , always third 112 136
                    , always third 72 112
                    , always third 88 96
                    , always third 64 48
                    , always third 192 40
                    , always third 136 16
                    , always third 80 16
                    , always third 64 24
                    ]

                1 ->
                    [ level 1 1 128 136
                    , level 2 1 128 136
                    , level 3 1 128 136
                    , level 4 1 128 136
                    , level 5 1 128 136
                    , level 6 1 128 136
                    , level 7 1 128 136
                    , level 8 3 192 144
                    , level 8 1 128 136
                    ]

                2 ->
                    [ always third 104 168
                    , always third 40 136
                    , always third 160 96
                    , always third 104 80
                    , always third 120 32
                    , always (Defs.ticrate // 4) 40 0
                    ]

                _ ->
                    []


{-| WI\_initAnimatedBack: no frame showing, and the first change soon (after a random
part of the period for those that always run).
-}
initAnimatedBack : Intermission -> Rng -> ( Intermission, Rng )
initAnimatedBack wi rng0 =
    let
        ( states, rng ) =
            Array.foldl
                (\a ( acc, r ) ->
                    let
                        ( nexttic, r2 ) =
                            case a.kind of
                                Always ->
                                    Random.mRandom r |> Tuple.mapFirst (\n -> wi.bcnt + 1 + modBy a.period n)

                                Random deviation base ->
                                    Random.mRandom r |> Tuple.mapFirst (\n -> wi.bcnt + 1 + base + modBy deviation n)

                                OnLevel _ ->
                                    ( wi.bcnt + 1, r )
                    in
                    ( Array.push { ctr = -1, nexttic = nexttic } acc, r2 )
                )
                ( Array.empty, rng0 )
                (animDefs wi.ctx.gameMode wi.wbs.epsd)
    in
    ( { wi | anims = states }, rng )


{-| WI\_updateAnimatedBack: the animations whose time has come move on a frame.
-}
updateAnimatedBack : Intermission -> Rng -> ( Intermission, Rng )
updateAnimatedBack wi rng0 =
    let
        defs =
            animDefs wi.ctx.gameMode wi.wbs.epsd

        step i st rng =
            case Array.get i defs of
                Nothing ->
                    ( st, rng )

                Just a ->
                    if wi.bcnt /= st.nexttic then
                        ( st, rng )

                    else
                        case a.kind of
                            Always ->
                                ( { ctr =
                                        if st.ctr + 1 >= a.nanims then
                                            0

                                        else
                                            st.ctr + 1
                                  , nexttic = wi.bcnt + a.period
                                  }
                                , rng
                                )

                            Random deviation base ->
                                if st.ctr + 1 == a.nanims then
                                    let
                                        ( n, r2 ) =
                                            Random.mRandom rng
                                    in
                                    ( { ctr = -1, nexttic = wi.bcnt + base + modBy deviation n }, r2 )

                                else
                                    ( { ctr = st.ctr + 1, nexttic = wi.bcnt + a.period }, rng )

                            OnLevel n ->
                                -- the original's "gawd-awful hack": the eighth one
                                -- waits until the stats are done
                                if not (wi.stage == StatCount && i == 7) && wi.wbs.next == n then
                                    ( { ctr = min (st.ctr + 1) (a.nanims - 1), nexttic = wi.bcnt + a.period }, rng )

                                else
                                    ( st, rng )

        ( states, rngOut ) =
            Array.foldl
                (\st ( acc, ( i, r ) ) ->
                    let
                        ( st2, r2 ) =
                            step i st r
                    in
                    ( Array.push st2 acc, ( i + 1, r2 ) )
                )
                ( Array.empty, ( 0, rng0 ) )
                wi.anims
                |> (\( acc, ( _, r ) ) -> ( acc, r ))
    in
    ( { wi | anims = states }, rngOut )



-- STARTING


{-| WI\_Start, given what the last intermission left in wi\_stuff.c's statics and
the M\_Random state (`rndindex`), which it draws from for the background animations.
-}
start : Context -> Statics -> WbStart -> Rng -> ( Intermission, Rng )
start ctx carried wbs0 rng =
    let
        wbs =
            { wbs0
                | maxkills = atLeastOne wbs0.maxkills
                , maxitems = atLeastOne wbs0.maxitems
                , maxsecret = atLeastOne wbs0.maxsecret
                , epsd =
                    if ctx.gameMode /= Retail && wbs0.epsd > 2 then
                        wbs0.epsd - 3

                    else
                        wbs0.epsd
            }

        zeros =
            Array.repeat Defs.maxPlayers 0

        base =
            { ctx = ctx
            , wbs = wbs
            , stage = StatCount
            , acceleratestage = False
            , cnt = 0
            , bcnt = 0
            , statState = 1
            , cntKills = zeros
            , cntItems = zeros
            , cntSecret = zeros
            , cntFrags = zeros
            , cntTime = 0
            , cntPar = 0
            , cntPause = Defs.ticrate
            , dofrags = carried.dofrags
            , dmFrags = Array.repeat Defs.maxPlayers zeros
            , dmTotals = zeros
            , anims = Array.empty
            , pointerOn = carried.pointerOn
            }

        initialised =
            if ctx.deathmatch then
                base

            else if ctx.netgame then
                let
                    sum =
                        players ctx |> List.foldl (\i acc -> acc + fragSum ctx wbs i) carried.dofrags
                in
                { base
                    | dofrags =
                        if sum /= 0 then
                            1

                        else
                            0
                }

            else
                { base
                    | cntKills = Array.set 0 -1 zeros
                    , cntItems = Array.set 0 -1 zeros
                    , cntSecret = Array.set 0 -1 zeros
                    , cntTime = -1
                    , cntPar = -1
                }
    in
    initAnimatedBack initialised rng


atLeastOne : Int -> Int
atLeastOne n =
    if n == 0 then
        1

    else
        n


players : Context -> List Int
players ctx =
    List.range 0 (Defs.maxPlayers - 1) |> List.filter (\i -> Array.get i ctx.playeringame == Just True)


{-| `(count * 100) / max`, as the stats show it.
-}
percent : Int -> Int -> Int
percent count most =
    (count * 100) // most


get : Int -> Array Int -> Int
get i a =
    Array.get i a |> Maybe.withDefault 0


plyr : Int -> WbStart -> WbPlayer
plyr i wbs =
    Array.get i wbs.plyr |> Maybe.withDefault { in_ = False, skills = 0, sitems = 0, ssecret = 0, stime = 0, frags = Array.repeat Defs.maxPlayers 0 }


{-| WI\_fragSum: a player's frags of the others, less the times they killed
themselves.
-}
fragSum : Context -> WbStart -> Int -> Int
fragSum ctx wbs n =
    let
        p =
            plyr n wbs
    in
    (players ctx |> List.filter (\i -> i /= n) |> List.map (\i -> get i p.frags) |> List.sum)
        - get n p.frags



-- TICKING


{-| WI\_Ticker: one tic, given the world (for the players' buttons and their
`attackdown`/`usedown`, which WI\_checkForAccelerate updates, for M\_Random, and for
sounds).
-}
ticker : Context -> World -> Intermission -> ( Intermission, World, Outcome )
ticker ctx world0 wi0 =
    let
        ( accelerate, world1 ) =
            checkForAccelerate ctx world0

        wi1 =
            { wi0
                | bcnt = wi0.bcnt + 1
                , acceleratestage = wi0.acceleratestage || accelerate
            }

        ( wi, rng ) =
            updateAnimatedBack wi1 world1.rng

        world =
            { world1 | rng = rng }
    in
    case wi.stage of
        StatCount ->
            let
                ( next, sounds ) =
                    if ctx.deathmatch then
                        updateDeathmatchStats ctx wi

                    else if ctx.netgame then
                        updateNetgameStats ctx wi

                    else
                        updateStats ctx wi

                played =
                    playAll sounds world
            in
            if next.stage == ShowNextLoc then
                -- WI_initShowNextLoc restarts the animations
                let
                    ( shown, r ) =
                        initAnimatedBack next played.rng
                in
                ( shown, { played | rng = r }, Continue )

            else
                ( next, played, Continue )

        ShowNextLoc ->
            let
                cnt =
                    wi.cnt - 1
            in
            if cnt == 0 || wi.acceleratestage then
                ( initNoState wi, world, Continue )

            else
                ( { wi | cnt = cnt, pointerOn = Bitwise.and cnt 31 < 20 }, world, Continue )

        NoState ->
            let
                cnt =
                    wi.cnt - 1
            in
            if cnt == 0 then
                ( { wi | cnt = cnt }, world, WorldDone )

            else
                ( { wi | cnt = cnt }, world, Continue )


{-| S\_StartSound(NULL, sfx) for each, in order, each drawing its pitch variation
from M\_Random then and there, as s\_sound.c does, between the intermission's own
draws.
-}
playAll : List Sfx -> World -> World
playAll sounds world =
    List.foldl
        (\sfx w ->
            if Sound.drawsPitch sfx then
                let
                    ( byte, rng ) =
                        Random.mRandom w.rng
                in
                { w | rng = rng, sounds = Sound.StartDrawn Sound.Ambient sfx byte :: w.sounds }

            else
                { w | sounds = Sound.StartDrawn Sound.Ambient sfx 0 :: w.sounds }
        )
        world
        sounds


{-| WI\_checkForAccelerate: a fresh press of attack or use by anyone hurries things on.
-}
checkForAccelerate : Context -> World -> ( Bool, World )
checkForAccelerate ctx world =
    players ctx
        |> List.foldl
            (\i ( acc, w ) ->
                case World.player i w of
                    Just p ->
                        let
                            attack =
                                Bitwise.and p.cmd.buttons Ticcmd.btAttack /= 0

                            use =
                                Bitwise.and p.cmd.buttons Ticcmd.btUse /= 0
                        in
                        ( acc || (attack && not p.attackdown) || (use && not p.usedown)
                        , World.updatePlayer i (\q -> { q | attackdown = attack, usedown = use }) w
                        )

                    Nothing ->
                        ( acc, w )
            )
            ( False, world )


{-| WI\_initNoState. WI\_drawNoState turns the arrow on for good before it draws, so
it is on from here.
-}
initNoState : Intermission -> Intermission
initNoState wi =
    { wi | stage = NoState, acceleratestage = False, cnt = 10, pointerOn = True }


initShowNextLoc : Intermission -> Intermission
initShowNextLoc wi =
    { wi | stage = ShowNextLoc, acceleratestage = False, cnt = 4 * Defs.ticrate }


afterStats : Context -> Intermission -> Intermission
afterStats ctx wi =
    if ctx.gameMode == Commercial then
        initNoState wi

    else
        initShowNextLoc wi


pistolEvery4 : Intermission -> List Sfx
pistolEvery4 wi =
    if modBy 4 wi.bcnt == 0 then
        [ Sfx.Pistol ]

    else
        []


{-| The odd states pause a second before the next count starts.
-}
pause : Intermission -> Intermission
pause wi =
    if wi.cntPause - 1 == 0 then
        { wi | statState = wi.statState + 1, cntPause = Defs.ticrate }

    else
        { wi | cntPause = wi.cntPause - 1 }


{-| WI\_updateStats (single player).
-}
updateStats : Context -> Intermission -> ( Intermission, List Sfx )
updateStats ctx wi0 =
    let
        wbs =
            wi0.wbs

        me =
            plyr wbs.pnum wbs

        kills =
            percent me.skills wbs.maxkills

        items =
            percent me.sitems wbs.maxitems

        secret =
            percent me.ssecret wbs.maxsecret

        time =
            me.stime // Defs.ticrate

        par =
            wbs.partime // Defs.ticrate

        ( wi, skipped ) =
            if wi0.acceleratestage && wi0.statState /= 10 then
                ( { wi0
                    | acceleratestage = False
                    , cntKills = Array.set 0 kills wi0.cntKills
                    , cntItems = Array.set 0 items wi0.cntItems
                    , cntSecret = Array.set 0 secret wi0.cntSecret
                    , cntTime = time
                    , cntPar = par
                    , statState = 10
                  }
                , [ Sfx.Barexp ]
                )

            else
                ( wi0, [] )

        count field target w =
            let
                c =
                    get 0 (field w) + 2
            in
            if c >= target then
                ( target, True )

            else
                ( c, False )
    in
    Tuple.mapSecond ((++) skipped) <|
        case wi.statState of
            2 ->
                let
                    ( c, done ) =
                        count .cntKills kills wi
                in
                ( { wi | cntKills = Array.set 0 c wi.cntKills } |> stepIf done, pistolEvery4 wi ++ barexpIf done )

            4 ->
                let
                    ( c, done ) =
                        count .cntItems items wi
                in
                ( { wi | cntItems = Array.set 0 c wi.cntItems } |> stepIf done, pistolEvery4 wi ++ barexpIf done )

            6 ->
                let
                    ( c, done ) =
                        count .cntSecret secret wi
                in
                ( { wi | cntSecret = Array.set 0 c wi.cntSecret } |> stepIf done, pistolEvery4 wi ++ barexpIf done )

            8 ->
                let
                    t =
                        min time (wi.cntTime + 3)

                    p =
                        wi.cntPar + 3

                    done =
                        p >= par && t >= time
                in
                ( { wi | cntTime = t, cntPar = min p par } |> stepIf done, pistolEvery4 wi ++ barexpIf done )

            10 ->
                if wi.acceleratestage then
                    ( afterStats ctx wi, [ Sfx.Sgcock ] )

                else
                    ( wi, [] )

            n ->
                if modBy 2 n == 1 then
                    ( pause wi, [] )

                else
                    ( wi, [] )


stepIf : Bool -> Intermission -> Intermission
stepIf done wi =
    if done then
        { wi | statState = wi.statState + 1 }

    else
        wi


barexpIf : Bool -> List Sfx
barexpIf done =
    if done then
        [ Sfx.Barexp ]

    else
        []


{-| WI\_updateNetgameStats.
-}
updateNetgameStats : Context -> Intermission -> ( Intermission, List Sfx )
updateNetgameStats ctx wi0 =
    let
        wbs =
            wi0.wbs

        inGame =
            players ctx

        target field i =
            let
                p =
                    plyr i wbs
            in
            case field of
                0 ->
                    percent p.skills wbs.maxkills

                1 ->
                    percent p.sitems wbs.maxitems

                2 ->
                    percent p.ssecret wbs.maxsecret

                _ ->
                    fragSum ctx wbs i

        setAll field values w =
            case field of
                0 ->
                    { w | cntKills = values }

                1 ->
                    { w | cntItems = values }

                2 ->
                    { w | cntSecret = values }

                _ ->
                    { w | cntFrags = values }

        current field w =
            case field of
                0 ->
                    w.cntKills

                1 ->
                    w.cntItems

                2 ->
                    w.cntSecret

                _ ->
                    w.cntFrags

        ( wi, skipped ) =
            if wi0.acceleratestage && wi0.statState /= 10 then
                ( List.foldl
                    (\i w ->
                        let
                            set field ww =
                                setAll field (Array.set i (target field i) (current field ww)) ww
                        in
                        w
                            |> set 0
                            |> set 1
                            |> set 2
                            |> (if w.dofrags /= 0 then
                                    set 3

                                else
                                    identity
                               )
                    )
                    { wi0 | acceleratestage = False, statState = 10 }
                    inGame
                , [ Sfx.Barexp ]
                )

            else
                ( wi0, [] )

        -- one counting step of a field for every player, by `step`
        countUp field step finishSound advance =
            let
                ( values, stillticking ) =
                    List.foldl
                        (\i ( vs, ticking ) ->
                            let
                                c =
                                    get i vs + step

                                t =
                                    target field i
                            in
                            if c >= t then
                                ( Array.set i t vs, ticking )

                            else
                                ( Array.set i c vs, True )
                        )
                        ( current field wi, False )
                        inGame

                counted =
                    setAll field values wi
            in
            if stillticking then
                ( counted, pistolEvery4 wi )

            else
                ( { counted | statState = counted.statState + advance }, pistolEvery4 wi ++ [ finishSound ] )
    in
    Tuple.mapSecond ((++) skipped) <|
        case wi.statState of
            2 ->
                countUp 0 2 Sfx.Barexp 1

            4 ->
                countUp 1 2 Sfx.Barexp 1

            6 ->
                countUp 2
                    2
                    Sfx.Barexp
                    (if wi.dofrags /= 0 then
                        1

                     else
                        3
                    )

            8 ->
                countUp 3 1 Sfx.Pldeth 1

            10 ->
                if wi.acceleratestage then
                    ( afterStats ctx wi, [ Sfx.Sgcock ] )

                else
                    ( wi, [] )

            n ->
                if modBy 2 n == 1 then
                    ( pause wi, [] )

                else
                    ( wi, [] )


{-| WI\_updateDeathmatchStats.
-}
updateDeathmatchStats : Context -> Intermission -> ( Intermission, List Sfx )
updateDeathmatchStats ctx wi0 =
    let
        wbs =
            wi0.wbs

        inGame =
            players ctx

        frag i j =
            get j (plyr i wbs).frags

        dmFrag i j w =
            Array.get i w.dmFrags |> Maybe.map (get j) |> Maybe.withDefault 0

        setDmFrag i j v w =
            { w | dmFrags = Array.set i (Array.set j v (Array.get i w.dmFrags |> Maybe.withDefault Array.empty)) w.dmFrags }

        clamp99 =
            clamp -99 99

        ( wi, skipped ) =
            if wi0.acceleratestage && wi0.statState /= 4 then
                ( List.foldl
                    (\i w ->
                        List.foldl (\j ww -> setDmFrag i j (frag i j) ww) w inGame
                            |> (\ww -> { ww | dmTotals = Array.set i (fragSum ctx wbs i) ww.dmTotals })
                    )
                    { wi0 | acceleratestage = False, statState = 4 }
                    inGame
                , [ Sfx.Barexp ]
                )

            else
                ( wi0, [] )
    in
    Tuple.mapSecond ((++) skipped) <|
        case wi.statState of
            2 ->
                let
                    ( counted, stillticking ) =
                        List.foldl
                            (\i ( w, ticking ) ->
                                let
                                    ( w2, t2 ) =
                                        List.foldl
                                            (\j ( ww, tt ) ->
                                                if dmFrag i j ww /= frag i j then
                                                    let
                                                        v =
                                                            if frag i j < 0 then
                                                                dmFrag i j ww - 1

                                                            else
                                                                dmFrag i j ww + 1
                                                    in
                                                    ( setDmFrag i j (clamp99 v) ww, True )

                                                else
                                                    ( ww, tt )
                                            )
                                            ( w, ticking )
                                            inGame
                                in
                                ( { w2 | dmTotals = Array.set i (clamp99 (fragSum ctx wbs i)) w2.dmTotals }, t2 )
                            )
                            ( wi, False )
                            inGame
                in
                if stillticking then
                    ( counted, pistolEvery4 wi )

                else
                    ( { counted | statState = 3 }, pistolEvery4 wi ++ [ Sfx.Barexp ] )

            4 ->
                if wi.acceleratestage then
                    ( afterStats ctx wi, [ Sfx.Slop ] )

                else
                    ( wi, [] )

            n ->
                if modBy 2 n == 1 then
                    ( pause wi, [] )

                else
                    ( wi, [] )
