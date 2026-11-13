module Doom.Game exposing
    ( Game, GameState(..), GameAction(..), Tic
    , new, newGame, playDemo, tick, initNew
    , playing, consolePlayer, gametic, playerNames
    )

{-| The game (g\_game.c): starting games and demos, loading levels, running one tic at
a time (G\_Ticker), and moving between levels, the intermission and the finale.

A game is driven one tic at a time with every player's input for that tic, the
`netcmds` of the C source (players who aren't at this computer send empty ones; a
demo replaces them with its own):

    Game.new wad data (Game.newGame Defs.Medium 1 1)
        |> Game.tick (Array.repeat 4 Ticcmd.empty)
        |> Tuple.first
        |> Game.playing

Not translated here: saving and loading games (`Doom.Save` does them; a save request in a ticcmd is ignored), the menus, the
status bar, automap and chat tickers (they don't change the game), the demo loop and
title pages (with `singledemo`, a demo that ends leaves the game on `DemoScreen`, as
`-playdemo` quits; without it, `Tic.demoEnded` tells the demo loop to move on).
The intermission and the finale run here (`Doom.Intermission`, `Doom.Finale`); their
sounds go into `Tic.sounds` with no origin, and the finale's F\_Responder is
`Doom.Finale.responder`.

@docs Game, GameState, GameAction, Tic
@docs new, newGame, playDemo, tick, initNew
@docs playing, consolePlayer, gametic, playerNames

-}

import Array exposing (Array)
import Bitwise
import Doom.Data exposing (Data)
import Doom.Defs as Defs exposing (GameMode(..), PlayerState(..), Skill(..))
import Doom.Demo as Demo exposing (Demo)
import Doom.Finale as Finale
import Doom.Flags as Flags
import Doom.Intermission as Intermission exposing (Intermission, WbStart)
import Doom.Play.Env exposing (Env, Params)
import Doom.Play.Setup as Setup exposing (BodyQue)
import Doom.Play.Spawn as Spawn
import Doom.Play.Tick as Tick
import Doom.Random as Random
import Doom.Sound as Sound
import Doom.Ticcmd as Ticcmd exposing (Ticcmd)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (Exit(..), World)


{-| g\_game.c's globals.

  - `params`: the settings the play code sees (skill, episode, map, deathmatch...).
    `params.fast` is whether G\_InitNew has sped the monsters up.
  - `respawnparm`, `fastparm`: -respawn and -fast (or a demo's).
  - `world`: the players and, while a level is loaded, everything in it. It outlives
    levels, as the C source's globals do; `world.gametic` is the game's tic count.
  - `env`: the level being played, once one has been loaded.
  - `wminfo`: what the last level's intermission shows and where it goes next.
  - `error`: why a level couldn't be loaded, if one couldn't.

-}
type alias Game =
    { wad : Wad
    , data : Data
    , params : Params
    , respawnparm : Bool
    , fastparm : Bool
    , demoplayback : Bool
    , singledemo : Bool
    , netdemo : Bool
    , state : GameState
    , action : GameAction
    , world : World
    , env : Maybe Env
    , levelstarttic : Int
    , paused : Bool
    , secretexit : Bool
    , wminfo : WbStart
    , bodyque : BodyQue
    , demo : Maybe Demo
    , wiStatics : Intermission.Statics
    , error : Maybe String
    }


{-| gamestate\_t. `DemoScreen` is where the game is before anything starts and after a
demo ends.
-}
type GameState
    = Level
    | Intermission Intermission
    | Finale Finale.Model
    | DemoScreen


{-| gameaction\_t: something to do at the start of the next tic.
-}
type GameAction
    = NoAction
    | LoadLevel
    | NewGame Skill Int Int
    | PlayDemo Demo
    | Completed
    | Victory
    | WorldDone


{-| What a tic did: whether the level ran (P\_Ticker), and the sounds it asked for, in
order.
-}
type alias Tic =
    { ranLevel : Bool
    , demoEnded : Bool
    , sounds : List Sound.Event
    }


{-| A game with an action to start with: `newGame` or `playDemo`.
-}
new : Wad -> Data -> GameAction -> Game
new wad data action =
    { wad = wad
    , data = data
    , params =
        { gameMode = data.gameMode
        , skill = Baby
        , episode = 1
        , map = 1
        , deathmatch = 0
        , netgame = False
        , respawnmonsters = False
        , fast = False
        , nomonsters = False
        , consoleplayer = 0
        }
    , respawnparm = False
    , fastparm = False
    , demoplayback = False
    , singledemo = True
    , netdemo = False
    , state = DemoScreen
    , action = action
    , world = Setup.newWorld (Array.fromList [ True, False, False, False ])
    , env = Nothing
    , levelstarttic = 0
    , paused = False
    , secretexit = False
    , wminfo =
        { epsd = 0
        , didsecret = False
        , last = 0
        , next = 0
        , maxkills = 0
        , maxitems = 0
        , maxsecret = 0
        , maxfrags = 0
        , partime = 0
        , pnum = 0
        , plyr = Array.empty
        }
    , bodyque = Setup.emptyBodyQue
    , demo = Nothing
    , wiStatics = Intermission.initialStatics
    , error = Nothing
    }


{-| G\_DeferedInitNew: a single-player game at this skill, episode and map.
-}
newGame : Skill -> Int -> Int -> GameAction
newGame =
    NewGame


{-| G\_DeferedPlayDemo.
-}
playDemo : Demo -> GameAction
playDemo =
    PlayDemo


{-| The level being played and its world, if the game is in a level.
-}
playing : Game -> Maybe ( Env, World )
playing game =
    case ( game.state, game.env ) of
        ( Level, Just env ) ->
            Just ( env, game.world )

        _ ->
            Nothing


{-| The player at this computer.
-}
consolePlayer : Game -> Int
consolePlayer game =
    game.params.consoleplayer


{-| The number of tics the game has run.
-}
gametic : Game -> Int
gametic game =
    game.world.gametic


{-| player\_names[]: the players' colours, for messages.
-}
playerNames : List String
playerNames =
    [ "Green", "Indigo", "Brown", "Red" ]



-- G_TICKER


{-| G\_Ticker, then `gametic++`: respawn dead players, do the pending action, take
each player's ticcmd (from `netcmds`, or the demo), handle pause, and run the level,
the intermission or the finale.
-}
tick : Array Ticcmd -> Game -> ( Game, Tic )
tick netcmds game0 =
    let
        game1 =
            { game0 | world = clearSounds game0.world }
                |> doReborns
                |> doActions
    in
    case readCmds netcmds game1 of
        Nothing ->
            if game1.singledemo then
                -- the demo is over: like -playdemo, stop here
                let
                    ended =
                        checkDemoStatus game1
                in
                ( ended, { ranLevel = False, demoEnded = True, sounds = List.reverse ended.world.sounds } )

            else
                -- in the demo loop, G_CheckDemoStatus asks for the next page and the tic
                -- goes on with the console player's own command
                let
                    ended =
                        endDemo game1
                in
                runTic True (readCmds netcmds ended |> Maybe.withDefault ended)

        Just game2 ->
            runTic False game2


runTic : Bool -> Game -> ( Game, Tic )
runTic demoEnded game2 =
    let
        ( game3, ranLevel ) =
            game2 |> specialButtons |> mainAction

        world =
            game3.world
    in
    ( { game3 | world = { world | gametic = world.gametic + 1, sounds = [] } }
    , { ranLevel = ranLevel, demoEnded = demoEnded, sounds = List.reverse world.sounds }
    )


clearSounds : World -> World
clearSounds world =
    { world | sounds = [] }


inGame : Int -> World -> Bool
inGame i world =
    Array.get i world.playeringame |> Maybe.withDefault False


playerIndexes : List Int
playerIndexes =
    List.range 0 (Defs.maxPlayers - 1)


doReborns : Game -> Game
doReborns game =
    List.foldl
        (\i g ->
            case World.player i g.world of
                Just p ->
                    if inGame i g.world && p.playerstate == Reborn then
                        doReborn i g

                    else
                        g

                Nothing ->
                    g
        )
        game
        playerIndexes


doActions : Game -> Game
doActions game =
    case game.action of
        NoAction ->
            game

        LoadLevel ->
            doActions (doLoadLevel game)

        NewGame skill episode map ->
            doActions (doNewGame skill episode map game)

        PlayDemo demo ->
            doActions (doPlayDemo demo game)

        Completed ->
            doActions (doCompleted game)

        Victory ->
            doActions (startFinale game)

        WorldDone ->
            doActions (doWorldDone game)


{-| Each player's ticcmd for this tic, and the "is turbo!" message. Nothing if the
demo has ended.
-}
readCmds : Array Ticcmd -> Game -> Maybe Game
readCmds netcmds game =
    List.foldl
        (\i maybeGame ->
            maybeGame
                |> Maybe.andThen
                    (\g ->
                        if not (inGame i g.world) then
                            Just g

                        else
                            let
                                net =
                                    Array.get i netcmds |> Maybe.withDefault Ticcmd.empty

                                fromDemo =
                                    case ( g.demoplayback, g.demo ) of
                                        ( True, Just demo ) ->
                                            Demo.readTiccmd net demo |> Maybe.map (\( c, d ) -> ( c, Just d ))

                                        _ ->
                                            Just ( net, g.demo )
                            in
                            fromDemo
                                |> Maybe.map
                                    (\( cmd, demo ) ->
                                        let
                                            tic =
                                                g.world.gametic

                                            turbo =
                                                cmd.forwardmove > 0x32 && modBy 32 tic == 0 && modBy 4 (tic // 32) == i

                                            withCmd =
                                                World.updatePlayer i (\p -> { p | cmd = cmd }) g.world

                                            withMessage =
                                                if turbo then
                                                    World.updatePlayer g.params.consoleplayer
                                                        (\p -> { p | message = Just (playerName i ++ " is turbo!") })
                                                        withCmd

                                                else
                                                    withCmd
                                        in
                                        { g | world = withMessage, demo = demo }
                                    )
                    )
        )
        (Just game)
        playerIndexes


playerName : Int -> String
playerName i =
    List.drop i playerNames |> List.head |> Maybe.withDefault ""


{-| BT\_SPECIAL buttons: pause toggles; a save request is ignored.
-}
specialButtons : Game -> Game
specialButtons game =
    List.foldl
        (\i g ->
            case World.player i g.world of
                Just p ->
                    if
                        inGame i g.world
                            && Bitwise.and p.cmd.buttons Ticcmd.btSpecial
                            /= 0
                            && Bitwise.and p.cmd.buttons Ticcmd.btSpecialMask
                            == Ticcmd.btsPause
                    then
                        { g | paused = not g.paused }

                    else
                        g

                Nothing ->
                    g
        )
        game
        playerIndexes


mainAction : Game -> ( Game, Bool )
mainAction game =
    case game.state of
        Level ->
            case game.env of
                Just env ->
                    if game.paused then
                        ( game, False )

                    else
                        let
                            ticked =
                                Tick.ticker env game.world
                        in
                        ( exitLevel { game | world = { ticked | exit = Nothing } } ticked.exit, True )

                Nothing ->
                    ( game, False )

        Intermission wi ->
            let
                ( next, world, outcome ) =
                    Intermission.ticker (intermissionContext game) game.world wi

                g =
                    { game | state = Intermission next, world = world }
            in
            case outcome of
                Intermission.Continue ->
                    ( g, False )

                Intermission.WorldDone ->
                    ( worldDone { g | wiStatics = Intermission.statics next }, False )

        Finale finale ->
            ( finaleTicker finale game, False )

        DemoScreen ->
            ( game, False )


intermissionContext : Game -> Intermission.Context
intermissionContext game =
    { gameMode = game.params.gameMode
    , deathmatch = game.params.deathmatch /= 0
    , netgame = game.params.netgame
    , playeringame = game.world.playeringame
    }


{-| G\_ExitLevel and G\_SecretExitLevel, which the level asked for through
`World.exit`. Doom II has no secret exit without a MAP31.
-}
exitLevel : Game -> Maybe Exit -> Game
exitLevel game exit =
    case exit of
        Nothing ->
            game

        Just NormalExit ->
            { game | secretexit = False, action = Completed }

        Just SecretExit ->
            { game
                | secretexit = not (game.params.gameMode == Commercial && Wad.checkNumForName "MAP31" game.wad == Nothing)
                , action = Completed
            }



-- STARTING GAMES AND LEVELS


{-| G\_DoNewGame: a single-player game with no special settings.
-}
doNewGame : Skill -> Int -> Int -> Game -> Game
doNewGame skill episode map game =
    let
        params =
            game.params

        world =
            game.world
    in
    initNew skill
        episode
        map
        { game
            | demoplayback = False
            , netdemo = False
            , params = { params | netgame = False, deathmatch = 0, nomonsters = False, consoleplayer = 0 }
            , respawnparm = False
            , fastparm = False
            , world = { world | playeringame = Array.fromList [ firstInGame world, False, False, False ] }
            , action = NoAction
        }


firstInGame : World -> Bool
firstInGame world =
    inGame 0 world


{-| G\_DoPlayDemo: take the game's settings from the demo's header and start it.
-}
doPlayDemo : Demo -> Game -> Game
doPlayDemo demo game =
    let
        h =
            Demo.header demo

        params =
            game.params

        world =
            game.world

        net =
            Array.get 1 h.playeringame == Just True

        started =
            initNew h.skill
                h.episode
                h.map
                { game
                    | action = NoAction
                    , params =
                        { params
                            | deathmatch = h.deathmatch
                            , nomonsters = h.nomonsters
                            , consoleplayer = h.consoleplayer
                            , netgame = params.netgame || net
                        }
                    , netdemo = game.netdemo || net
                    , respawnparm = h.respawnparm
                    , fastparm = h.fastparm
                    , world = { world | playeringame = h.playeringame }
                }
    in
    { started | demoplayback = True, demo = Just demo }


{-| G\_InitNew (what -warp and -skill start with, keeping -fast, -respawn and
-nomonsters, which G\_DoNewGame clears): clamp the episode and map to the game, reset the random numbers,
settle respawning and fast monsters, make every player start afresh, and load the
level.
-}
initNew : Skill -> Int -> Int -> Game -> Game
initNew skill episode0 map0 game =
    let
        mode =
            game.params.gameMode

        episode =
            case mode of
                Retail ->
                    clamp 1 4 episode0

                Shareware ->
                    clamp 1 1 episode0

                _ ->
                    clamp 1 3 episode0

        map =
            if mode /= Commercial then
                clamp 1 9 map0

            else
                max 1 map0

        params =
            game.params

        fast =
            if game.fastparm || (skill == Nightmare && params.skill /= Nightmare) then
                True

            else if skill /= Nightmare && params.skill == Nightmare then
                False

            else
                params.fast

        world =
            game.world
    in
    doLoadLevel
        { game
            | paused = False
            , demoplayback = False
            , params =
                { params
                    | skill = skill
                    , episode = episode
                    , map = map
                    , respawnmonsters = skill == Nightmare || game.respawnparm
                    , fast = fast
                }
            , world =
                { world
                    | rng = Random.init
                    , players = Array.map (\p -> { p | playerstate = Reborn }) world.players
                }
        }


{-| G\_DoLoadLevel: dead players will be reborn, frags are cleared, and P\_SetupLevel
loads the map.
-}
doLoadLevel : Game -> Game
doLoadLevel game =
    let
        world =
            game.world

        players =
            Array.indexedMap
                (\i p ->
                    { p
                        | playerstate =
                            if inGame i world && p.playerstate == Dead then
                                Reborn

                            else
                                p.playerstate
                        , frags = Array.repeat Defs.maxPlayers 0
                    }
                )
                world.players

        prepared =
            { game
                | levelstarttic = world.gametic
                , state = Level
                , action = NoAction
                , paused = False
                , world = { world | players = players }
            }
    in
    case Setup.setupLevel game.wad game.data game.params game.bodyque prepared.world of
        Ok ( env, newWorld, que ) ->
            { prepared | env = Just env, world = newWorld, bodyque = que, error = Nothing }

        Err problem ->
            { prepared | state = DemoScreen, env = Nothing, error = Just problem }


{-| G\_DoReborn: in a single-player game, reload the level; in a net game, leave the
body behind and respawn at the deathmatch starts, the player's own start, or anyone's.
-}
doReborn : Int -> Game -> Game
doReborn playernum game =
    if not game.params.netgame then
        { game | action = LoadLevel }

    else
        case game.env of
            Nothing ->
                game

            Just env ->
                let
                    world =
                        case World.player playernum game.world |> Maybe.andThen .mo of
                            Just body ->
                                World.updateMobj body (\m -> { m | player = Nothing }) game.world

                            Nothing ->
                                game.world

                    start =
                        Setup.playerStart playernum world

                    ( que, respawned ) =
                        if game.params.deathmatch /= 0 then
                            Setup.deathMatchSpawnPlayer env playernum ( game.bodyque, world )

                        else
                            case Setup.checkSpot env playernum start ( game.bodyque, world ) of
                                ( True, ( q, w ) ) ->
                                    ( q, Spawn.spawnPlayer env start w )

                                ( False, state ) ->
                                    tryOtherStarts env playernum 0 state
                in
                { game | bodyque = que, world = respawned }


tryOtherStarts : Env -> Int -> Int -> ( BodyQue, World ) -> ( BodyQue, World )
tryOtherStarts env playernum i ( que, world ) =
    if i >= Defs.maxPlayers then
        ( que, Spawn.spawnPlayer env (Setup.playerStart playernum world) world )

    else
        let
            start =
                Setup.playerStart i world
        in
        case Setup.checkSpot env playernum start ( que, world ) of
            ( True, ( q, w ) ) ->
                -- fake it as the other player's start, just for the spawn
                ( q, Spawn.spawnPlayer env { start | type_ = playernum + 1 } w )

            ( False, state ) ->
                tryOtherStarts env playernum (i + 1) state


{-| G\_CheckDemoStatus when a -playdemo demo ends: the game stops, and its settings go
back to a single-player game's.
-}
checkDemoStatus : Game -> Game
checkDemoStatus game =
    let
        ended =
            endDemo game
    in
    { ended | state = DemoScreen, env = Nothing }


{-| G\_CheckDemoStatus in the demo loop: the settings go back to a single-player
game's; the level stays until D\_DoAdvanceDemo moves on.
-}
endDemo : Game -> Game
endDemo game =
    let
        p =
            game.params

        params =
            { p | netgame = False, deathmatch = 0, nomonsters = False, consoleplayer = 0 }

        world =
            game.world
    in
    { game
        | demoplayback = False
        , netdemo = False
        , demo = Nothing
        , params = params
        , env = Maybe.map (\env -> { env | params = params }) game.env
        , respawnparm = False
        , fastparm = False
        , world = { world | playeringame = Array.fromList [ firstInGame world, False, False, False ] }
    }



-- ENDING LEVELS


{-| G\_PlayerFinishLevel: powers and keys go, invisibility and the light from gun
flashes end.
-}
playerFinishLevel : Int -> World -> World
playerFinishLevel i world =
    let
        withPlayer =
            World.updatePlayer i
                (\p ->
                    { p
                        | powers = Array.map (\_ -> 0) p.powers
                        , cards = Array.map (\_ -> False) p.cards
                        , extralight = 0
                        , fixedcolormap = 0
                        , damagecount = 0
                        , bonuscount = 0
                    }
                )
                world
    in
    case World.player i world |> Maybe.andThen .mo of
        Just body ->
            World.updateMobj body (\m -> { m | flags = Flags.clear Flags.mfShadow m.flags }) withPlayer

        Nothing ->
            withPlayer


{-| G\_DoCompleted: players finish the level; after E?M8 comes the finale; otherwise
work out the next map and start the intermission.
-}
doCompleted : Game -> Game
doCompleted game0 =
    let
        finished =
            List.foldl
                (\i w ->
                    if inGame i w then
                        playerFinishLevel i w

                    else
                        w
                )
                game0.world
                playerIndexes

        game =
            { game0 | action = NoAction, world = finished }

        mode =
            game.params.gameMode

        map =
            game.params.map

        episode =
            game.params.episode
    in
    if mode /= Commercial && map == 8 then
        { game | action = Victory }

    else
        let
            world =
                if mode /= Commercial && map == 9 then
                    { finished | players = Array.map (\p -> { p | didsecret = True }) finished.players }

                else
                    finished

            console =
                World.player game.params.consoleplayer world

            next =
                if mode == Commercial then
                    if game.secretexit then
                        case map of
                            15 ->
                                30

                            31 ->
                                31

                            _ ->
                                game.wminfo.next

                    else
                        case map of
                            31 ->
                                15

                            32 ->
                                15

                            _ ->
                                map

                else if game.secretexit then
                    8

                else if map == 9 then
                    case episode of
                        1 ->
                            3

                        2 ->
                            5

                        3 ->
                            6

                        4 ->
                            2

                        _ ->
                            game.wminfo.next

                else
                    map

            wminfo =
                { epsd = episode - 1
                , didsecret = console |> Maybe.map .didsecret |> Maybe.withDefault False
                , last = map - 1
                , next = next
                , maxkills = world.totalkills
                , maxitems = world.totalitems
                , maxsecret = world.totalsecret
                , maxfrags = 0
                , partime = 35 * parTime mode episode map
                , pnum = game.params.consoleplayer
                , plyr =
                    Array.indexedMap
                        (\i p ->
                            { in_ = inGame i world
                            , skills = p.killcount
                            , sitems = p.itemcount
                            , ssecret = p.secretcount
                            , stime = world.leveltime
                            , frags = p.frags
                            }
                        )
                        world.players
                }
        in
        let
            ( wi, started ) =
                Intermission.start (intermissionContext game) game.wiStatics wminfo world
        in
        { game
            | world = started
            , wminfo = wminfo
            , state = Intermission wi
        }


{-| Par times in seconds (pars[] and cpars[]). linuxdoom's pars table has no row for
episode 4: reading past it lands, in the C program's memory, on cpars, so that is
what episode 4 uses here.
-}
parTime : GameMode -> Int -> Int -> Int
parTime mode episode map =
    let
        at i list =
            List.drop i list |> List.head |> Maybe.withDefault 0
    in
    if mode == Commercial then
        at (map - 1) cpars

    else
        case episode of
            1 ->
                at map [ 0, 30, 75, 120, 90, 165, 180, 180, 30, 165 ]

            2 ->
                at map [ 0, 90, 90, 90, 120, 90, 360, 240, 30, 170 ]

            3 ->
                at map [ 0, 90, 45, 90, 150, 90, 90, 165, 30, 135 ]

            4 ->
                at map cpars

            _ ->
                0


cpars : List Int
cpars =
    [ 30, 90, 120, 120, 90, 150, 120, 120, 270, 90, 210, 150, 150, 150, 210, 150, 420, 150, 210, 150, 240, 150, 180, 150, 150, 300, 330, 420, 300, 180, 120, 30 ]


{-| G\_WorldDone: the intermission is over. Doom II's story screens come after some
maps.
-}
worldDone : Game -> Game
worldDone game =
    let
        world =
            if game.secretexit then
                World.updatePlayer game.params.consoleplayer (\p -> { p | didsecret = True }) game.world

            else
                game.world

        g =
            { game | action = WorldDone, world = world }

        map =
            game.params.map
    in
    if game.params.gameMode == Commercial then
        if List.member map [ 6, 11, 20, 30 ] || (List.member map [ 15, 31 ] && game.secretexit) then
            startFinale g

        else
            g

    else
        g


{-| G\_DoWorldDone: on to the next map.
-}
doWorldDone : Game -> Game
doWorldDone game =
    let
        params =
            game.params
    in
    doLoadLevel { game | state = Level, params = { params | map = game.wminfo.next + 1 } }


{-| F\_StartFinale.
-}
startFinale : Game -> Game
startFinale game =
    { game
        | action = NoAction
        , state = Finale (Finale.start game.params.gameMode game.params.episode game.params.map)
    }


{-| F\_Ticker, with every player's buttons (in Doom II, a press after the first 50
tics moves on); its sounds go into `World.sounds`.
-}
finaleTicker : Finale.Model -> Game -> Game
finaleTicker finale game =
    let
        anyButton =
            Array.toList game.world.players |> List.any (\p -> p.cmd.buttons /= 0)

        t =
            Finale.ticker anyButton finale

        world =
            List.foldl (\s w -> { w | sounds = s :: w.sounds }) game.world t.sounds
    in
    { game
        | state = Finale t.model
        , world = world
        , action =
            if t.worldDone then
                WorldDone

            else
                game.action
    }
