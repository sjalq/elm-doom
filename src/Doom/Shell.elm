module Doom.Shell exposing
    ( Shell, Options, defaultOptions, init
    , Output, iteration, wiping, wipeStep
    , screen, game, menuSettings, savedGames, saveCount, setSavedGames, finished
    )

{-| The game shell (d\_main.c, with g\_game.c's G\_Responder and the parts of G\_Ticker
around the game): one pass of D\_DoomLoop at a time.

A pass (`iteration`) takes the events that arrived since the last one and does what
the original's -singletics loop does: D\_ProcessEvents (the menu first, then
G\_Responder: the spy key, the menu during demos, the heads-up, status bar and
automap responders, the pause key and the controls), G\_BuildTiccmd, D\_DoAdvanceDemo
when the demo loop moves on, M\_Ticker, G\_Ticker with the status bar, automap and
heads-up tickers and the title pages' timer, S\_UpdateSounds, and D\_Display.

D\_Display draws in the original's order onto the screen as the last frame left it
(the status bar and the border are only redrawn when needed, as in the original):
the level's automap or status bar, the intermission, the finale or the title page,
then the 3D view, the heads-up messages, the view's border, the pause sign and the
menu on top. When the game state changes (a level starts, the intermission comes),
the new screen melts in over the old one: `iteration` gives the melt's first frame
and `wiping` is True until `wipeStep` has given the others, one per frame, as
D\_Display's wipe loop does (the first step moves the melt by 2 tics, the others by
1, as the original's I\_GetTime gives them in the C oracle).

@docs Shell, Options, defaultOptions, init
@docs Output, iteration, wiping, wipeStep
@docs screen, game, menuSettings, savedGames, saveCount, setSavedGames, finished

-}

import Array
import Bitwise
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Doom.Audio exposing (Command)
import Doom.Audio.Sound as Sound
import Doom.Automap as Automap
import Doom.Data exposing (Data)
import Doom.Defs exposing (GameMode(..), PlayerState(..), Skill(..))
import Doom.Demo as Demo
import Doom.Event as Event exposing (Event(..))
import Doom.Finale as Finale
import Doom.Flags as Flags
import Doom.Game as Game exposing (Game, GameAction(..), GameState(..))
import Doom.Gfx as Gfx exposing (Gfx)
import Doom.Hud.Messages as Messages
import Doom.Hud.StatusBar as StatusBar
import Doom.Info.Sfx exposing (Sfx)
import Doom.Input as Input exposing (Bindings, Controls)
import Doom.Intermission as Intermission
import Doom.Intermission.Draw as IntermissionDraw
import Doom.Menu as Menu exposing (Menu, Settings)
import Doom.Play.Env exposing (Env)
import Doom.Random as Random exposing (Rng)
import Doom.Render.Data as RenderData exposing (RenderData)
import Doom.Render.View as View
import Doom.Save as Save
import Doom.Screen exposing (Screen)
import Doom.Sound
import Doom.Ticcmd as Ticcmd exposing (Ticcmd)
import Doom.Video as Video exposing (Framebuffer)
import Doom.Wad as Wad exposing (Wad)
import Doom.Wipe as Wipe exposing (Melt)
import Doom.World as World exposing (World)


{-| How to start, as D\_DoomMain's command line does.

  - `warp`: -warp episode map (map alone for Doom II, whose episode is 1). With
    `skill` or `episode` it starts a game at once (autostart), skipping the title.
  - `skill`: -skill, `episode`: -episode (map 1).
  - `playdemo`: -playdemo: play this demo lump (or the demo's bytes) once, then stop.
  - `nomonsters`, `respawn`, `fast`: -nomonsters, -respawn, -fast.
  - `settings`, `bindings`: the configuration (m\_misc.c's defaults unless changed).
  - `savedGames`: the saved game slots 0 to 5 the host has (doomsav0.dsg...).
  - `quitToTitle`: what the menu's Quit does: True goes back to the title (a browser
    can't exit), False ends the game (`finished`), as I\_Quit does.
  - `soundChannels`: snd\_channels (3 by default).

-}
type alias Options =
    { warp : Maybe ( Int, Int )
    , skill : Maybe Skill
    , episode : Maybe Int
    , playdemo : Maybe Bytes
    , nomonsters : Bool
    , respawn : Bool
    , fast : Bool
    , settings : Settings
    , bindings : Bindings
    , savedGames : Dict Int Bytes
    , quitToTitle : Bool
    , soundChannels : Int
    }


{-| The title and demo loop with the default configuration and no saved games.
-}
defaultOptions : Options
defaultOptions =
    { warp = Nothing
    , skill = Nothing
    , episode = Nothing
    , playdemo = Nothing
    , nomonsters = False
    , respawn = False
    , fast = False
    , settings = Menu.defaultSettings
    , bindings = Input.defaultBindings
    , savedGames = Dict.empty
    , quitToTitle = True
    , soundChannels = 3
    }


{-| gamestate\_t, without the state's data (for D\_Display's comparisons).
-}
type Gs
    = GsLevel
    | GsIntermission
    | GsFinale
    | GsDemoScreen


gsOf : Game -> Gs
gsOf g =
    case g.state of
        Level ->
            GsLevel

        Intermission _ ->
            GsIntermission

        Finale _ ->
            GsFinale

        DemoScreen ->
            GsDemoScreen


{-| The whole program's state: the game, the menu, the status bar, heads-up, automap
and sound, the controls, the demo loop, D\_Display's statics, the screen and palette,
a melt under way, and I\_GetTime's counter.
-}
type Shell
    = Shell Model


type alias Model =
    { wad : Wad
    , data : Data
    , gfx : Gfx
    , renderData : Maybe RenderData
    , game : Game
    , menu : Menu
    , sound : Sound.Model
    , statusBar : StatusBar.Model
    , messages : Messages.Model
    , automap : Automap.Model
    , controls : Controls
    , bindings : Bindings
    , sendpause : Bool
    , sendsave : Maybe ( Int, String )
    , savedescription : String
    , savegame : Maybe ( Int, String )
    , loadgame : Maybe Int
    , slots : Dict Int Bytes
    , saveCount : Int
    , usergame : Bool
    , singledemo : Bool
    , quitToTitle : Bool
    , advancedemo : Bool
    , demosequence : Int
    , pagetic : Int
    , pagename : String
    , displayplayer : Int
    , time : Int

    -- D_Display's statics and R_SetViewSize's request
    , setsizeneeded : Bool
    , viewBlocks : Int
    , viewDetail : Int
    , viewactivestate : Bool
    , menuactivestate : Bool
    , inhelpscreensstate : Bool
    , fullscreen : Bool
    , oldgamestate : Maybe Gs
    , wipegamestate : Maybe Gs
    , borderdrawcount : Int

    -- the screen
    , screen : Framebuffer
    , palette : Int
    , stPalette : Int
    , stStopped : Bool
    , melt : Maybe Melt
    , finished : Bool
    }


{-| What a pass or a wipe step produced: the frames it showed (each I\_FinishUpdate),
the sound and music commands, the saved game slots it wrote, in order.
-}
type alias Output =
    { frames : List Screen
    , audio : List Command
    , saved : List ( Int, Bytes )
    }


{-| D\_DoomMain: load the WAD's data and start the title loop, a game, or a demo.
-}
init : Options -> Wad -> Data -> ( Shell, Output )
init options wad data =
    let
        mode =
            data.gameMode

        settings =
            options.settings

        ( sound, soundCmds ) =
            Sound.init wad { channels = options.soundChannels, sfxVolume = settings.sfxVolume, musicVolume = settings.musicVolume }

        base =
            Game.new wad data NoAction

        params =
            base.params

        startGame =
            { base
                | respawnparm = options.respawn
                , fastparm = options.fast
                , params = { params | nomonsters = options.nomonsters }
            }

        m0 =
            { wad = wad
            , data = data
            , gfx = Gfx.load wad
            , renderData = Nothing
            , game = startGame
            , menu = Menu.init mode settings
            , sound = sound
            , statusBar = StatusBar.init wad
            , messages = Messages.init wad
            , automap = Automap.init wad
            , controls = Input.noControls
            , bindings = options.bindings
            , sendpause = False
            , sendsave = Nothing
            , savedescription = ""
            , savegame = Nothing
            , loadgame = Nothing
            , slots = options.savedGames
            , saveCount = 0
            , usergame = False
            , singledemo = False
            , quitToTitle = options.quitToTitle
            , advancedemo = False
            , demosequence = 0
            , pagetic = 0
            , pagename = "TITLEPIC"
            , displayplayer = 0
            , time = 0
            , setsizeneeded = True
            , viewBlocks = settings.screenblocks
            , viewDetail = settings.detailLevel
            , viewactivestate = False
            , menuactivestate = False
            , inhelpscreensstate = False
            , fullscreen = False
            , oldgamestate = Nothing
            , wipegamestate = Just GsDemoScreen
            , borderdrawcount = 0
            , screen = Video.blank 0
            , palette = 0
            , stPalette = 0
            , stStopped = True
            , melt = Nothing
            , finished = False
            }

        autostart =
            options.warp /= Nothing || options.skill /= Nothing || options.episode /= Nothing

        ( episode, map ) =
            case options.warp of
                Just ( e, mp ) ->
                    if mode == Commercial then
                        ( Maybe.withDefault 1 options.episode, mp )

                    else
                        ( e, mp )

                Nothing ->
                    ( Maybe.withDefault 1 options.episode, 1 )

        ( m1, out ) =
            case options.playdemo of
                Just demoBytes ->
                    -- -playdemo: G_DeferedPlayDemo, quit when it ends
                    ( { m0
                        | singledemo = True
                        , automap = Automap.setSingleDemo True m0.automap
                        , game = deferedPlayDemo demoBytes { startGame | singledemo = True }
                      }
                    , []
                    )

                Nothing ->
                    if autostart then
                        -- G_InitNew straight away
                        let
                            before =
                                { startGame | singledemo = False }

                            started =
                                Game.initNew (Maybe.withDefault Medium options.skill) episode map before
                        in
                        afterLoad True before started { m0 | game = started } noAcc |> Tuple.mapSecond (.audio >> List.reverse)

                    else
                        ( startTitle { m0 | game = { startGame | singledemo = False } }, [] )
    in
    ( Shell m1, { frames = [], audio = soundCmds ++ out, saved = [] } )


deferedPlayDemo : Bytes -> Game -> Game
deferedPlayDemo bytes g =
    case Demo.fromBytes bytes of
        Ok demo ->
            { g | action = Game.playDemo demo }

        Err _ ->
            -- "Demo is from a different game version!": nothing happens
            { g | action = NoAction }


{-| D\_StartTitle.
-}
startTitle : Model -> Model
startTitle m =
    let
        g =
            m.game
    in
    { m | game = { g | action = NoAction }, loadgame = Nothing, savegame = Nothing, demosequence = -1, advancedemo = True }


{-| The screen as the last frame left it, with its palette and gamma.
-}
screen : Shell -> Screen
screen (Shell m) =
    currentScreen m


currentScreen : Model -> Screen
currentScreen m =
    { pixels = m.screen, palette = m.palette, gamma = (Menu.settings m.menu).usegamma }


{-| The game.
-}
game : Shell -> Game
game (Shell m) =
    m.game


{-| The settings as the menu has left them (for the host to keep).
-}
menuSettings : Shell -> Settings
menuSettings (Shell m) =
    Menu.settings m.menu


{-| The saved game slots.
-}
savedGames : Shell -> Dict Int Bytes
savedGames (Shell m) =
    m.slots


{-| How many games have been saved so far (G\_DoSaveGame): a host stores the slots
again when it changes.
-}
saveCount : Shell -> Int
saveCount (Shell m) =
    m.saveCount


{-| Hand in the host's saved game slots (read when a load or save menu opens).
-}
setSavedGames : Dict Int Bytes -> Shell -> Shell
setSavedGames slots (Shell m) =
    Shell { m | slots = slots }


{-| Whether the program has ended (I\_Quit: a -playdemo demo is over, the menu's Quit
without `quitToTitle`, or a level couldn't be loaded).
-}
finished : Shell -> Bool
finished (Shell m) =
    m.finished


{-| Whether a melt is under way (call `wipeStep` for its next frame).
-}
wiping : Shell -> Bool
wiping (Shell m) =
    m.melt /= Nothing



-- ONE PASS OF THE LOOP


type alias Acc =
    { audio : List Command
    , saved : List ( Int, Bytes )
    }


noAcc : Acc
noAcc =
    { audio = [], saved = [] }


{-| One pass of D\_DoomLoop with these events (posted before it, in order). Gives the
frame D\_Display showed (the first melt frame when a wipe starts; none once the game
has ended).
-}
iteration : List Event -> Shell -> ( Shell, Output )
iteration events (Shell m0) =
    if m0.finished then
        ( Shell m0, { frames = [], audio = [], saved = [] } )

    else
        let
            ( m1, acc1 ) =
                processEvents events ( m0, noAcc )

            ( cmd, controls ) =
                buildTiccmd m1

            m2 =
                { m1 | controls = controls, sendpause = False, sendsave = Nothing }

            ( m3, acc3 ) =
                if m2.advancedemo then
                    doAdvanceDemo ( m2, acc1 )

                else
                    ( m2, acc1 )

            ( m4, acc4 ) =
                if m1.finished then
                    -- I_Quit in the middle of D_ProcessEvents
                    ( m1, acc1 )

                else
                    gTicker cmd ( { m3 | menu = Menu.ticker m3.menu }, acc3 )
        in
        if m4.finished then
            ( Shell m4, { frames = [], audio = List.reverse acc4.audio, saved = List.reverse acc4.saved } )

        else
            let
                ( m5, acc5 ) =
                    updateSounds ( m4, acc4 )

                ( m6, frame ) =
                    display m5
            in
            ( Shell m6, { frames = [ frame ], audio = List.reverse acc5.audio, saved = List.reverse acc5.saved } )


{-| The next frame of the melt.
-}
wipeStep : Shell -> ( Shell, Output )
wipeStep (Shell m) =
    case m.melt of
        Nothing ->
            ( Shell m, { frames = [], audio = [], saved = [] } )

        Just melt ->
            let
                ( m2, frame ) =
                    meltStep 1 melt m
            in
            ( Shell m2, { frames = [ frame ], audio = [], saved = [] } )


meltStep : Int -> Melt -> Model -> ( Model, Screen )
meltStep tics melt m =
    let
        ( melted, melt2, done ) =
            Wipe.step tics m.screen melt

        ( menu, withMenu ) =
            Menu.drawer m.gfx m.menu melted

        m2 =
            { m
                | time = m.time + 1
                , screen = withMenu
                , menu = menu
                , melt =
                    if done then
                        Nothing

                    else
                        Just melt2
            }
    in
    ( m2, currentScreen m2 )



-- SOUND


{-| Run a sound call with M\_Random's index taken from the world and put back after
(the sounds' pitches draw from the same index as the wipe and the status bar).
-}
withSound : (Sound.Model -> ( Sound.Model, List Command )) -> ( Model, Acc ) -> ( Model, Acc )
withSound f ( m, acc ) =
    let
        ( sound, cmds ) =
            f m.sound
    in
    ( { m | sound = sound }, { acc | audio = List.reverse cmds ++ acc.audio } )


{-| A sound call that draws M\_Random (a sound's pitch), from the one index the whole
program shares: the world's `rng.rndindex`, as the status bar's face, the
intermission's animations and the wipe draw from it too.
-}
withRandomSound : (Rng -> Sound.Model -> ( Sound.Model, Rng, List Command )) -> ( Model, Acc ) -> ( Model, Acc )
withRandomSound f ( m, acc ) =
    let
        g =
            m.game

        world =
            g.world

        ( sound, rng, cmds ) =
            f world.rng m.sound
    in
    ( { m | sound = sound, game = { g | world = { world | rng = rng } } }
    , { acc | audio = List.reverse cmds ++ acc.audio }
    )


startSound : Sfx -> ( Model, Acc ) -> ( Model, Acc )
startSound sfx ( m, acc ) =
    withRandomSound (Sound.startSound m.game.world.gametic sfx) ( m, acc )


{-| A sound with no origin from the intermission, the finale or the menu (the pitch
already drawn, or to draw now).
-}
ambientSound : Int -> Doom.Sound.Event -> ( Model, Acc ) -> ( Model, Acc )
ambientSound gametic ev state =
    case ev of
        Doom.Sound.Start _ sfx ->
            withRandomSound (Sound.startSound gametic sfx) state

        Doom.Sound.StartAt _ sfx _ ->
            withRandomSound (Sound.startSound gametic sfx) state

        Doom.Sound.StartDrawn _ sfx byte ->
            withSound (Sound.startSoundDrawn gametic sfx byte) state

        _ ->
            state


updateSounds : ( Model, Acc ) -> ( Model, Acc )
updateSounds ( m, acc ) =
    withSound
        (Sound.updateSounds m.game.world.gametic
            (Maybe.map (\env -> ( env, m.game.world )) m.game.env)
        )
        ( m, acc )



-- EVENTS


processEvents : List Event -> ( Model, Acc ) -> ( Model, Acc )
processEvents events ( m, acc ) =
    if m.data.gameMode == Commercial && Wad.checkNumForName "MAP01" m.wad == Nothing then
        -- "IF STORE DEMO, DO NOT ACCEPT INPUT"
        ( m, acc )

    else
        List.foldl processEvent ( m, acc ) events


processEvent : Event -> ( Model, Acc ) -> ( Model, Acc )
processEvent ev ( m, acc ) =
    let
        g =
            m.game

        response =
            Menu.responder
                { usergame = m.usergame
                , inLevel = g.state == Level
                , netgame = g.params.netgame
                , demoplayback = g.demoplayback
                , automapActive = Automap.active m.automap
                , gametic = g.world.gametic
                , time = m.time
                , saveDescription = \slot -> Dict.get slot m.slots |> Maybe.andThen Save.description
                , gfx = m.gfx
                }
                ev
                m.menu

        ( m2, acc2 ) =
            List.foldl menuEffect ( { m | menu = response.menu, time = response.time }, acc ) response.effects
    in
    if response.eaten || m2.finished then
        ( m2, acc2 )

    else
        gResponder ev ( m2, acc2 )


menuEffect : Menu.Effect -> ( Model, Acc ) -> ( Model, Acc )
menuEffect effect ( m, acc ) =
    let
        g =
            m.game

        console =
            g.params.consoleplayer
    in
    case effect of
        Menu.PlaySound sfx ->
            startSound sfx ( m, acc )

        Menu.NewGame skill episode map ->
            ( { m | game = { g | action = Game.newGame skill episode map } }, acc )

        Menu.EndGame ->
            ( startTitle m, acc )

        Menu.LoadGame slot ->
            ( { m | loadgame = Just slot }, acc )

        Menu.SaveGame slot description ->
            ( { m | sendsave = Just ( slot, description ), savedescription = description }, acc )

        Menu.Quit ->
            if m.quitToTitle then
                ( startTitle m, acc )

            else
                ( { m | finished = True }, acc )

        Menu.SetViewSize ->
            ( { m | setsizeneeded = True }, acc )

        Menu.PlayerMessage text ->
            ( { m | game = { g | world = World.updatePlayer console (\p -> { p | message = Just text }) g.world } }, acc )

        Menu.ForceMessage ->
            ( { m | messages = Messages.forceNextMessage m.messages }, acc )

        Menu.ResetPalette ->
            ( { m | palette = 0 }, acc )

        Menu.SfxVolume v ->
            withSound (Sound.setSfxVolume v) ( m, acc )

        Menu.MusicVolume v ->
            withSound (Sound.setMusicVolume v) ( m, acc )


{-| G\_Responder.
-}
gResponder : Event -> ( Model, Acc ) -> ( Model, Acc )
gResponder ev ( m, acc ) =
    let
        g =
            m.game

        inLevel =
            g.state == Level

        noAction =
            g.action == NoAction && m.loadgame == Nothing && m.savegame == Nothing
    in
    if inLevel && ev == KeyDown Event.keyF12 && (m.singledemo || g.params.deathmatch == 0) then
        ( { m | displayplayer = nextDisplayPlayer g m.displayplayer }, acc )

    else if noAction && not m.singledemo && (g.demoplayback || g.state == DemoScreen) then
        case ev of
            KeyDown _ ->
                ( { m | menu = Menu.startControlPanel m.menu }, acc )

            Mouse buttons _ _ ->
                if buttons /= 0 then
                    ( { m | menu = Menu.startControlPanel m.menu }, acc )

                else
                    ( m, acc )

            KeyUp _ ->
                ( m, acc )

    else
        case ( g.state, g.env ) of
            ( Level, Just env ) ->
                levelResponders env ev ( m, acc )

            ( Finale finale, _ ) ->
                case Finale.responder ev g.world.rng finale of
                    Just ( finale2, sounds, rng ) ->
                        let
                            world =
                                g.world
                        in
                        List.foldl (ambientSound world.gametic)
                            ( { m | game = { g | state = Finale finale2, world = { world | rng = rng } } }, acc )
                            sounds

                    Nothing ->
                        gameKeys ev ( m, acc )

            _ ->
                gameKeys ev ( m, acc )


nextDisplayPlayer : Game -> Int -> Int
nextDisplayPlayer g current =
    let
        next =
            if current + 1 == 4 then
                0

            else
                current + 1

        inGame =
            Array.get next g.world.playeringame |> Maybe.withDefault False
    in
    if not inGame && next /= g.params.consoleplayer then
        nextDisplayPlayer g next

    else
        next


levelResponders : Env -> Event -> ( Model, Acc ) -> ( Model, Acc )
levelResponders env ev ( m, acc ) =
    case Messages.responder ev m.messages of
        Just messages ->
            ( { m | messages = messages }, acc )

        Nothing ->
            let
                g =
                    m.game

                console =
                    g.params.consoleplayer

                st =
                    StatusBar.responder ev env g.world console m.statusBar

                m2 =
                    { m | statusBar = st.model, game = { g | world = st.world } }

                ( m3, acc3 ) =
                    List.foldl statusBarRequest ( m2, acc ) st.requests

                g3 =
                    m3.game

                wasActive =
                    Automap.active m3.automap

                ( automap, world, eaten ) =
                    Automap.responder ev env g3.world console m3.automap

                m4 =
                    { m3
                        | automap = automap
                        , game = { g3 | world = world }
                        , statusBar =
                            if not wasActive && Automap.active automap then
                                StatusBar.automapEntered m3.statusBar

                            else
                                m3.statusBar
                    }
            in
            if eaten then
                ( m4, acc3 )

            else
                gameKeys ev ( m4, acc3 )


statusBarRequest : StatusBar.Request -> ( Model, Acc ) -> ( Model, Acc )
statusBarRequest request ( m, acc ) =
    case request of
        StatusBar.Warp skill episode map ->
            let
                g =
                    m.game
            in
            ( { m | game = { g | action = Game.newGame skill episode map } }, acc )

        StatusBar.ChangeMusic lump ->
            withSound (Sound.changeMusic (songName lump) True) ( m, acc )

        StatusBar.BadMusic _ ->
            -- S_ChangeMusic's I_Error "Bad music number"
            ( { m | finished = True }, acc )


{-| The end of G\_Responder: the pause key, the controls.
-}
gameKeys : Event -> ( Model, Acc ) -> ( Model, Acc )
gameKeys ev ( m, acc ) =
    if ev == KeyDown Event.keyPause then
        ( { m | sendpause = True }, acc )

    else
        let
            ( controls, _ ) =
                Input.respond (Menu.settings m.menu).mouseSensitivity ev m.controls
        in
        ( { m | controls = controls }, acc )


buildTiccmd : Model -> ( Ticcmd, Controls )
buildTiccmd m =
    let
        ( cmd, controls ) =
            Input.build m.bindings m.controls

        withSpecial =
            if m.sendpause then
                { cmd | buttons = Bitwise.or Ticcmd.btSpecial Ticcmd.btsPause }

            else
                cmd
    in
    case m.sendsave of
        Just ( slot, _ ) ->
            ( { withSpecial
                | buttons =
                    Bitwise.or Ticcmd.btSpecial (Bitwise.or Ticcmd.btsSaveGame (Bitwise.shiftLeftBy Ticcmd.btsSaveShift slot))
              }
            , controls
            )

        Nothing ->
            ( withSpecial, controls )



-- THE DEMO LOOP


{-| D\_DoAdvanceDemo: the next title page or demo.
-}
doAdvanceDemo : ( Model, Acc ) -> ( Model, Acc )
doAdvanceDemo ( m0, acc ) =
    let
        g0 =
            m0.game

        g =
            { g0
                | world = World.updatePlayer g0.params.consoleplayer (\p -> { p | playerstate = Live }) g0.world
                , paused = False
                , action = NoAction
            }

        mode =
            m0.data.gameMode

        sequence =
            modBy
                (if mode == Retail then
                    7

                 else
                    6
                )
                (m0.demosequence + 1)

        m =
            { m0
                | game = g
                , advancedemo = False
                , usergame = False
                , loadgame = Nothing
                , savegame = Nothing
                , demosequence = sequence
            }

        page tics name =
            ( { m | pagetic = tics, pagename = name, game = { g | state = DemoScreen } }, acc )

        demo name =
            case Wad.lump name m.wad of
                Just bytes ->
                    ( { m | game = deferedPlayDemo bytes g }, acc )

                Nothing ->
                    -- W_CacheLumpName's I_Error
                    ( { m | finished = True }, acc )

        titleMusic =
            withSound
                (Sound.startMusic
                    (if mode == Commercial then
                        "dm2ttl"

                     else
                        "intro"
                    )
                )
    in
    case sequence of
        0 ->
            page
                (if mode == Commercial then
                    35 * 11

                 else
                    170
                )
                "TITLEPIC"
                |> titleMusic

        1 ->
            demo "DEMO1"

        2 ->
            page 200 "CREDIT"

        3 ->
            demo "DEMO2"

        4 ->
            if mode == Commercial then
                page (35 * 11) "TITLEPIC" |> titleMusic

            else if mode == Retail then
                page 200 "CREDIT"

            else
                page 200 "HELP2"

        5 ->
            demo "DEMO3"

        _ ->
            demo "DEMO4"



-- G_TICKER


{-| G\_Ticker around `Game.tick`: the save and load the menu asked for, the tic, and
what the C source does in the same G\_Ticker that the Elm game leaves to the shell
(starting the status bar, heads-up and sound for a new level, its tickers, the
automap's, the title pages' timer, the next demo).
-}
gTicker : Ticcmd -> ( Model, Acc ) -> ( Model, Acc )
gTicker cmd ( m0, acc0 ) =
    let
        ( m, acc ) =
            pendingActions ( m0, acc0 )

        before =
            -- P_Ticker reads menuactive (the game stands still under the menu)
            m.game |> (\g -> { g | menuactive = Menu.active m.menu })

        console =
            before.params.consoleplayer

        netcmds =
            Array.initialize 4
                (\i ->
                    if i == console then
                        cmd

                    else
                        Ticcmd.empty
                )

        ( after, tic ) =
            Game.tick netcmds before

        initNew =
            case before.action of
                NewGame _ _ _ ->
                    True

                PlayDemo _ ->
                    True

                _ ->
                    False

        ( m1, acc1 ) =
            if levelLoaded before after then
                afterLoad initNew before after { m | game = after } acc

            else
                ( respawned before after { m | game = after }, acc )

        -- BTS_PAUSE's S_PauseSound and S_ResumeSound (and G_InitNew's S_ResumeSound)
        ( m2, acc2 ) =
            if before.paused /= after.paused then
                withSound
                    (if after.paused then
                        Sound.pauseSound

                     else
                        Sound.resumeSound
                    )
                    ( m1, acc1 )

            else
                ( m1, acc1 )

        ( m3, acc3 ) =
            ticSounds before.world.gametic tic.sounds (music ( m2, acc2 ))

        m4 =
            if before.action == Completed && Automap.active m3.automap then
                { m3 | automap = Automap.stop m3.automap }

            else
                m3

        m5 =
            case ( gsOf m4.game, m4.game.env ) of
                ( GsLevel, Just env ) ->
                    levelTickers env before.world m4

                ( GsDemoScreen, _ ) ->
                    -- D_PageTicker
                    if m4.pagetic - 1 < 0 then
                        { m4 | pagetic = m4.pagetic - 1, advancedemo = True }

                    else
                        { m4 | pagetic = m4.pagetic - 1 }

                _ ->
                    m4

        m6 =
            if tic.demoEnded then
                if m5.singledemo then
                    { m5 | finished = True }

                else
                    { m5 | advancedemo = True }

            else
                m5
    in
    ( { m6
        | finished = m6.finished || m6.game.error /= Nothing
        , savegame = saveRequest before.demoplayback cmd m6
      }
    , acc3
    )


{-| BTS\_SAVEGAME in the console player's command for the tic (as G\_Ticker sees it,
before P\_PlayerThink clears the special buttons): G\_DoSaveGame comes at the next
tic. A demo's commands are its own, and none of Freedoom's save.
-}
saveRequest : Bool -> Ticcmd -> Model -> Maybe ( Int, String )
saveRequest demoplayback cmd m =
    if
        not demoplayback
            && Bitwise.and cmd.buttons Ticcmd.btSpecial
            /= 0
            && Bitwise.and cmd.buttons Ticcmd.btSpecialMask
            == Ticcmd.btsSaveGame
    then
        Just ( Bitwise.shiftRightBy Ticcmd.btsSaveShift (Bitwise.and cmd.buttons Ticcmd.btsSaveMask), m.savedescription )

    else
        m.savegame


{-| G\_Ticker's ga\_savegame and ga\_loadgame (unless the player is being reborn, whose
ga\_loadlevel takes their place).
-}
pendingActions : ( Model, Acc ) -> ( Model, Acc )
pendingActions ( m, acc ) =
    let
        g =
            m.game

        reborn =
            not g.params.netgame
                && (World.player g.params.consoleplayer g.world |> Maybe.map (\p -> p.playerstate == Reborn) |> Maybe.withDefault False)
    in
    if reborn then
        ( { m | savegame = Nothing, loadgame = Nothing }, acc )

    else
        case ( m.savegame, m.loadgame ) of
            ( Just ( slot, description0 ), _ ) ->
                let
                    description =
                        if description0 == "" then
                            "NET GAME"

                        else
                            description0

                    world =
                        World.updatePlayer g.params.consoleplayer (\p -> { p | message = Just "game saved." }) g.world
                in
                case Save.save description g of
                    Ok bytes ->
                        ( { m
                            | savegame = Nothing
                            , savedescription = ""
                            , slots = Dict.insert slot bytes m.slots
                            , saveCount = m.saveCount + 1
                            , game = { g | world = world }
                          }
                        , { acc | saved = ( slot, bytes ) :: acc.saved }
                        )

                    Err _ ->
                        -- "Savegame buffer overrun" (the original quits): go on unsaved
                        ( { m | savegame = Nothing, savedescription = "" }, acc )

            ( Nothing, Just slot ) ->
                let
                    m2 =
                        { m | loadgame = Nothing }
                in
                case Dict.get slot m.slots |> Maybe.map (\bytes -> Save.load bytes g) of
                    Just (Ok loaded) ->
                        afterLoad True g loaded { m2 | game = loaded } acc

                    _ ->
                        -- no such file, or a bad version: nothing happens
                        ( m2, acc )

            ( Nothing, Nothing ) ->
                ( m, acc )


{-| P\_SpawnPlayer's ST\_Start and HU\_Start when the console player is respawned in
a net game (G\_DoReborn), without a level load: their body changed.
-}
respawned : Game -> Game -> Model -> Model
respawned before after m =
    let
        console =
            after.params.consoleplayer

        body g =
            World.player console g.world |> Maybe.andThen .mo
    in
    case after.env of
        Just env ->
            if body before /= body after && body after /= Nothing then
                { m
                    | palette =
                        if m.stStopped then
                            m.palette

                        else
                            0
                    , stStopped = False
                    , stPalette = -1
                    , statusBar = StatusBar.start env after.world console m.statusBar
                    , messages = Messages.start env after.world m.messages
                }

            else
                m

        Nothing ->
            m


{-| Whether the tic loaded a level (G\_DoLoadLevel).
-}
levelLoaded : Game -> Game -> Bool
levelLoaded before after =
    let
        tic =
            before.world.gametic
    in
    after.levelstarttic == tic && isJust after.env && (before.levelstarttic /= tic || not (isJust before.env))


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


{-| What happens with a level load outside the play code: G\_DoLoadLevel's wipe,
I\_GetTime and clearing of the controls, P\_SpawnPlayer's ST\_Start and HU\_Start,
P\_SetupLevel's S\_Start, and with G\_InitNew the automap going off and `usergame`.
-}
afterLoad : Bool -> Game -> Game -> Model -> Acc -> ( Model, Acc )
afterLoad initNew before after m acc =
    case after.env of
        Nothing ->
            ( m, acc )

        Just env ->
            let
                console =
                    after.params.consoleplayer

                renderData =
                    case m.renderData of
                        Just rd ->
                            Just rd

                        Nothing ->
                            Just (RenderData.load m.wad m.data)

                m2 =
                    { m
                        | time = m.time + 1
                        , wipegamestate =
                            if m.wipegamestate == Just GsLevel then
                                Nothing

                            else
                                m.wipegamestate
                        , controls = Input.clear m.controls
                        , sendpause = False
                        , sendsave = Nothing
                        , displayplayer = console
                        , renderData = renderData
                        , palette =
                            if m.stStopped then
                                m.palette

                            else
                                0
                        , stStopped = False
                        , stPalette = -1
                        , statusBar = StatusBar.start env after.world console m.statusBar
                        , messages = Messages.start env after.world m.messages
                        , automap =
                            if initNew then
                                Automap.deactivate m.automap

                            else
                                m.automap
                        , usergame =
                            if initNew then
                                not after.demoplayback

                            else
                                m.usergame
                    }
            in
            withSound (Sound.startLevel before.world.gametic env.params) ( m2, acc )


{-| The intermission's and the finale's music (WI\_Ticker's and F\_StartFinale's
S\_ChangeMusic, which does nothing for the song already playing), and the finale's
request for a melt.
-}
music : ( Model, Acc ) -> ( Model, Acc )
music ( m, acc ) =
    case m.game.state of
        Intermission wi ->
            case Intermission.music wi of
                Just lump ->
                    withSound (Sound.changeMusic (songName lump) True) ( m, acc )

                Nothing ->
                    ( m, acc )

        Finale finale ->
            let
                song =
                    Finale.music finale

                m2 =
                    if Finale.wipeRequested finale then
                        { m | wipegamestate = Nothing }

                    else
                        m
            in
            withSound (Sound.changeMusic (songName song.lump) song.looping) ( m2, acc )

        _ ->
            ( m, acc )


{-| A music lump's song name, as `Doom.Audio.Sound` takes it ("D\_INTER" is "inter").
-}
songName : String -> String
songName lump =
    if String.startsWith "D_" (String.toUpper lump) then
        String.dropLeft 2 lump

    else
        lump


ticSounds : Int -> List Doom.Sound.Event -> ( Model, Acc ) -> ( Model, Acc )
ticSounds gametic sounds ( m, acc ) =
    if List.isEmpty sounds then
        ( m, acc )

    else
        case ( m.game.state, m.game.env ) of
            ( Level, Just env ) ->
                withRandomSound (Sound.play gametic env m.game.world sounds) ( m, acc )

            _ ->
                -- the intermission's and the finale's sounds have no origin
                List.foldl (ambientSound gametic) ( m, acc ) sounds


{-| ST\_Ticker, AM\_Ticker and HU\_Ticker (and the automap going off when the console
player dies, P\_KillMobj's AM\_Stop).
-}
levelTickers : Env -> World -> Model -> Model
levelTickers env worldBefore m =
    let
        g =
            m.game

        console =
            g.params.consoleplayer

        automap0 =
            Automap.stopOnDeath worldBefore g.world console m.automap

        ( statusBar, world1 ) =
            StatusBar.ticker env g.world console m.statusBar

        automap =
            Automap.ticker env world1 console automap0

        ( messages, world2 ) =
            Messages.ticker { showMessages = (Menu.settings m.menu).showMessages } world1 console m.messages
    in
    { m | statusBar = statusBar, automap = automap, messages = messages, game = { g | world = world2 } }



-- D_DISPLAY


type alias Window =
    { x : Int, y : Int, width : Int, height : Int }


{-| D\_Display, up to its first frame.
-}
display : Model -> ( Model, Screen )
display m0 =
    let
        -- R_ExecuteSetViewSize
        m =
            if m0.setsizeneeded then
                let
                    st =
                        Menu.settings m0.menu
                in
                { m0
                    | setsizeneeded = False
                    , viewBlocks = st.screenblocks
                    , viewDetail = st.detailLevel
                    , oldgamestate = Nothing
                    , borderdrawcount = 3
                }

            else
                m0

        g =
            m.game

        gs =
            gsOf g

        wipe =
            Just gs /= m.wipegamestate

        startScreen =
            m.screen

        gametic =
            g.world.gametic

        automapActive =
            Automap.active m.automap

        win =
            viewWindow m

        console =
            g.params.consoleplayer

        backScreen rd =
            View.drawBorder rd m.viewBlocks (Video.blank 0)

        -- HU_Erase
        ( messages1, screen1 ) =
            case ( gs, m.renderData ) of
                ( GsLevel, Just rd ) ->
                    if gametic /= 0 then
                        Messages.erase automapActive
                            { x = win.x
                            , y = win.y
                            , width = win.width // (2 ^ m.viewDetail)
                            , height = win.height
                            }
                            (backScreen rd)
                            m.messages
                            m.screen

                    else
                        ( m.messages, m.screen )

                _ ->
                    ( m.messages, m.screen )

        -- the buffered drawing
        ( m2, screen2 ) =
            case ( g.state, g.env ) of
                ( Level, Just env ) ->
                    if gametic == 0 then
                        ( m, screen1 )

                    else
                        let
                            withMap =
                                if automapActive then
                                    Automap.drawer env g.world console m.automap screen1

                                else
                                    screen1

                            redrawsbar =
                                wipe
                                    || (win.height /= 200 && m.fullscreen)
                                    || (m.inhelpscreensstate && not (Menu.inHelpScreens m.menu))

                            ( statusBar, withBar ) =
                                StatusBar.drawer env
                                    g.world
                                    console
                                    { fullscreen = win.height == 200, refresh = redrawsbar, automap = automapActive }
                                    m.statusBar
                                    withMap

                            barPalette =
                                StatusBar.palette g.world console
                        in
                        ( if barPalette /= m.stPalette then
                            { m | statusBar = statusBar, fullscreen = win.height == 200, stPalette = barPalette, palette = barPalette }

                          else
                            { m | statusBar = statusBar, fullscreen = win.height == 200 }
                        , withBar
                        )

                ( Intermission wi, _ ) ->
                    ( m, IntermissionDraw.drawer m.wad wi screen1 )

                ( Finale finale, _ ) ->
                    ( m, Finale.drawer m.wad finale screen1 )

                ( DemoScreen, _ ) ->
                    ( m, Gfx.draw 0 0 m.pagename m.gfx screen1 )

                ( Level, Nothing ) ->
                    ( m, screen1 )

        -- the view, the heads-up
        ( m3, screen3 ) =
            case ( g.state, g.env, m2.renderData ) of
                ( Level, Just env, Just rd ) ->
                    if gametic /= 0 then
                        let
                            ( withView, mapped, rd2 ) =
                                if not automapActive then
                                    View.render rd
                                        env
                                        g.world
                                        { screenblocks = m2.viewBlocks, detailshift = m2.viewDetail, player = m2.displayplayer, skytexture = g.skytexture }
                                        screen2

                                else
                                    ( screen2, [], rd )

                            world =
                                markMapped mapped g.world
                        in
                        ( { m2 | game = { g | world = world }, renderData = Just rd2 }
                        , Messages.drawer env automapActive messages1 withView
                        )

                    else
                        ( m2, screen2 )

                _ ->
                    ( m2, screen2 )

        palette3 =
            if Just gs /= m3.oldgamestate && gs /= GsLevel then
                0

            else
                m3.palette

        viewactivestate =
            if gs == GsLevel && m3.oldgamestate /= Just GsLevel then
                False

            else
                m3.viewactivestate

        -- the border
        ( borderdrawcount, screen4 ) =
            case m3.renderData of
                Just rd ->
                    if gs == GsLevel && not automapActive && win.width /= 320 then
                        let
                            count =
                                if Menu.active m3.menu || m3.menuactivestate || not viewactivestate then
                                    3

                                else
                                    m3.borderdrawcount
                        in
                        if count > 0 then
                            ( count - 1, View.drawBorder rd m3.viewBlocks screen3 )

                        else
                            ( count, screen3 )

                    else
                        ( m3.borderdrawcount, screen3 )

                Nothing ->
                    ( m3.borderdrawcount, screen3 )

        -- the pause sign
        screen5 =
            if g.paused then
                Gfx.draw (win.x + (win.width - 68) // 2)
                    (if automapActive then
                        4

                     else
                        win.y + 4
                    )
                    "M_PAUSE"
                    m3.gfx
                    screen4

            else
                screen4

        -- the menu
        ( menu, screen6 ) =
            Menu.drawer m3.gfx m3.menu screen5

        m4 =
            { m3
                | messages = messages1
                , palette = palette3
                , borderdrawcount = borderdrawcount
                , menuactivestate = Menu.active m3.menu
                , viewactivestate = not automapActive
                , inhelpscreensstate = Menu.inHelpScreens m3.menu
                , oldgamestate = Just gs
                , wipegamestate = Just gs
                , menu = menu
                , screen = screen6
            }
    in
    if not wipe then
        ( m4, currentScreen m4 )

    else
        -- wipe_EndScreen, then the first step of the loop (wipestart = I_GetTime () - 1)
        let
            world =
                m4.game.world

            ( melt, rng ) =
                Wipe.start world.rng startScreen screen6

            g4 =
                m4.game
        in
        meltStep 2
            melt
            { m4
                | screen = startScreen
                , time = m4.time + 1
                , game = { g4 | world = { world | rng = rng } }
            }


{-| The view window after R\_ExecuteSetViewSize.
-}
viewWindow : Model -> Window
viewWindow m =
    View.window m.viewBlocks


markMapped : List Int -> World -> World
markMapped lines world =
    if List.isEmpty lines then
        world

    else
        { world
            | lines =
                List.foldl
                    (\i acc ->
                        case Array.get i acc of
                            Just line ->
                                Array.set i { line | flags = Flags.set Flags.mlMapped line.flags } acc

                            Nothing ->
                                acc
                    )
                    world.lines
                    lines
        }
