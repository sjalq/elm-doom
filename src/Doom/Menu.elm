module Doom.Menu exposing
    ( Menu, Settings, defaultSettings, init, settings, setSettings, active, inHelpScreens, quickSaveSlot
    , Context, Effect(..), Response, responder, startControlPanel, ticker, drawer
    , quitMessages, doom2QuitMessages
    )

{-| The menus (m\_menu.c): the main menu and everything under it, the messages that
ask "are you sure?", typing a saved game's name, and the function keys (F1 help to
F11 gamma, - and = for the screen size).

The menu doesn't change the game itself: `responder` hands back the `Effect`s for the
game shell to carry out (start a game, load or save one, play a sound, change the
view size), in the order the C source does them.

@docs Menu, Settings, defaultSettings, init, settings, setSettings, active, inHelpScreens, quickSaveSlot
@docs Context, Effect, Response, responder, startControlPanel, ticker, drawer
@docs quitMessages, doom2QuitMessages

-}

import Array exposing (Array)
import Doom.Config as Config
import Doom.Defs as Defs exposing (GameMode(..), Skill(..))
import Doom.Event as Event exposing (Event(..))
import Doom.Gfx as Gfx exposing (Gfx)
import Doom.Info.Sfx exposing (Sfx(..))
import Doom.Video exposing (Framebuffer)


{-| The settings the menu changes: `Doom.Config.Settings`.
-}
type alias Settings =
    Config.Settings


{-| m\_misc.c's defaults: `Doom.Config.defaultSettings`.
-}
defaultSettings : Settings
defaultSettings =
    Config.defaultSettings


type MenuId
    = MainDef
    | EpiDef
    | NewDef
    | OptionsDef
    | ReadDef1
    | ReadDef2
    | SoundDef
    | LoadDef
    | SaveDef


menuIndex : MenuId -> Int
menuIndex id =
    case id of
        MainDef ->
            0

        EpiDef ->
            1

        NewDef ->
            2

        OptionsDef ->
            3

        ReadDef1 ->
            4

        ReadDef2 ->
            5

        SoundDef ->
            6

        LoadDef ->
            7

        SaveDef ->
            8


type Routine
    = NewGameR
    | OptionsR
    | LoadGameR
    | SaveGameR
    | ReadThisR
    | QuitDoomR
    | EpisodeR
    | ChooseSkillR
    | EndGameR
    | ChangeMessagesR
    | ChangeDetailR
    | SizeDisplayR
    | ChangeSensitivityR
    | SoundR
    | ReadThis2R
    | FinishReadThisR
    | SfxVolR
    | MusicVolR
    | LoadSelectR
    | SaveSelectR


{-| menuitem\_t: status 0 no cursor, 1 ok, 2 arrows ok, -1 an empty line.
-}
type alias Item =
    { status : Int
    , name : String
    , routine : Maybe Routine
    , alphaKey : Int
    }


type Draw
    = DrawMain
    | DrawEpisode
    | DrawNewGame
    | DrawOptions
    | DrawReadThis1
    | DrawReadThis2
    | DrawSound
    | DrawLoad
    | DrawSave


type alias Def =
    { items : Array Item
    , prev : Maybe MenuId
    , draw : Draw
    , x : Int
    , y : Int
    }


item : Int -> String -> Maybe Routine -> Char -> Item
item status name routine key =
    { status = status, name = name, routine = routine, alphaKey = Char.toCode key }


emptyLine : Item
emptyLine =
    { status = -1, name = "", routine = Nothing, alphaKey = 0 }


{-| The menus, with M\_Init's changes for each game mode (Doom II has no Read This!
item and goes straight to the skills; Doom and the shareware have three episodes).
-}
def : GameMode -> Array Bool -> MenuId -> Def
def mode loadStatus id =
    case id of
        MainDef ->
            let
                all =
                    [ item 1 "M_NGAME" (Just NewGameR) 'n'
                    , item 1 "M_OPTION" (Just OptionsR) 'o'
                    , item 1 "M_LOADG" (Just LoadGameR) 'l'
                    , item 1 "M_SAVEG" (Just SaveGameR) 's'
                    , item 1
                        "M_RDTHIS"
                        (Just
                            (if mode == Retail then
                                ReadThis2R

                             else
                                ReadThisR
                            )
                        )
                        'r'
                    , item 1 "M_QUITG" (Just QuitDoomR) 'q'
                    ]
            in
            if mode == Commercial then
                -- MainMenu[readthis] = MainMenu[quitdoom]; numitems--; y += 8
                { items = Array.fromList (List.take 4 all ++ List.drop 5 all), prev = Nothing, draw = DrawMain, x = 97, y = 72 }

            else
                { items = Array.fromList all, prev = Nothing, draw = DrawMain, x = 97, y = 64 }

        EpiDef ->
            let
                all =
                    [ item 1 "M_EPI1" (Just EpisodeR) 'k'
                    , item 1 "M_EPI2" (Just EpisodeR) 't'
                    , item 1 "M_EPI3" (Just EpisodeR) 'i'
                    , item 1 "M_EPI4" (Just EpisodeR) 't'
                    ]

                count =
                    if mode == Shareware || mode == Registered then
                        3

                    else
                        4
            in
            { items = Array.fromList (List.take count all), prev = Just MainDef, draw = DrawEpisode, x = 48, y = 63 }

        NewDef ->
            { items =
                Array.fromList
                    [ item 1 "M_JKILL" (Just ChooseSkillR) 'i'
                    , item 1 "M_ROUGH" (Just ChooseSkillR) 'h'
                    , item 1 "M_HURT" (Just ChooseSkillR) 'h'
                    , item 1 "M_ULTRA" (Just ChooseSkillR) 'u'
                    , item 1 "M_NMARE" (Just ChooseSkillR) 'n'
                    ]
            , prev =
                if mode == Commercial then
                    Just MainDef

                else
                    Just EpiDef
            , draw = DrawNewGame
            , x = 48
            , y = 63
            }

        OptionsDef ->
            { items =
                Array.fromList
                    [ item 1 "M_ENDGAM" (Just EndGameR) 'e'
                    , item 1 "M_MESSG" (Just ChangeMessagesR) 'm'
                    , item 1 "M_DETAIL" (Just ChangeDetailR) 'g'
                    , item 2 "M_SCRNSZ" (Just SizeDisplayR) 's'
                    , emptyLine
                    , item 2 "M_MSENS" (Just ChangeSensitivityR) 'm'
                    , emptyLine
                    , item 1 "M_SVOL" (Just SoundR) 's'
                    ]
            , prev = Just MainDef
            , draw = DrawOptions
            , x = 60
            , y = 37
            }

        ReadDef1 ->
            if mode == Commercial then
                { items = Array.fromList [ { status = 1, name = "", routine = Just FinishReadThisR, alphaKey = 0 } ]
                , prev = Just MainDef
                , draw = DrawReadThis1
                , x = 330
                , y = 165
                }

            else
                { items = Array.fromList [ { status = 1, name = "", routine = Just ReadThis2R, alphaKey = 0 } ]
                , prev = Just MainDef
                , draw = DrawReadThis1
                , x = 280
                , y = 185
                }

        ReadDef2 ->
            { items = Array.fromList [ { status = 1, name = "", routine = Just FinishReadThisR, alphaKey = 0 } ]
            , prev =
                -- Ultimate Doom has one help page: backing out closes the menu
                if mode == Retail then
                    Nothing

                else
                    Just ReadDef1
            , draw = DrawReadThis2
            , x = 330
            , y = 175
            }

        SoundDef ->
            { items =
                Array.fromList
                    [ item 2 "M_SFXVOL" (Just SfxVolR) 's'
                    , emptyLine
                    , item 2 "M_MUSVOL" (Just MusicVolR) 'm'
                    , emptyLine
                    ]
            , prev = Just OptionsDef
            , draw = DrawSound
            , x = 80
            , y = 64
            }

        LoadDef ->
            { items =
                Array.fromList
                    (List.map
                        (\i ->
                            { status =
                                if Array.get i loadStatus |> Maybe.withDefault False then
                                    1

                                else
                                    0
                            , name = ""
                            , routine = Just LoadSelectR
                            , alphaKey = Char.toCode '1' + i
                            }
                        )
                        (List.range 0 (slots - 1))
                    )
            , prev = Just MainDef
            , draw = DrawLoad
            , x = 80
            , y = 54
            }

        SaveDef ->
            { items =
                Array.fromList
                    (List.map (\i -> { status = 1, name = "", routine = Just SaveSelectR, alphaKey = Char.toCode '1' + i })
                        (List.range 0 (slots - 1))
                    )
            , prev = Just MainDef
            , draw = DrawSave
            , x = 80
            , y = 54
            }


{-| KEY\_PAUSE, KEY\_CAPSLOCK, KEY\_SCRLCK and KEY\_NUMLOCK.
-}
nullKeys : List Int
nullKeys =
    [ Event.keyPause, Event.keyCapsLock, Event.keyScrollLock, Event.keyNumLock ]


slots : Int
slots =
    6


lineHeight : Int
lineHeight =
    16


skullXOff : Int
skullXOff =
    -32


saveStringSize : Int
saveStringSize =
    24


emptyString : String
emptyString =
    "empty slot"



-- THE MODEL


type MessageRoutine
    = QuickSaveResponse
    | QuickLoadResponse
    | VerifyNightmare
    | EndGameResponse
    | QuitResponse


type alias Message =
    { text : String
    , routine : Maybe MessageRoutine
    , needsInput : Bool
    }


{-| m\_menu.c's state: whether the menu is up and which one, where the skull is and
how it blinks, the message on the screen, the saved games' names and the one being
typed, the quicksave slot, the settings, and the mouse's position for moving the
skull.
-}
type Menu
    = Menu Model


type alias Model =
    { gameMode : GameMode
    , active : Bool
    , current : MenuId
    , itemOn : Int
    , lastOn : Array Int
    , skullAnimCounter : Int
    , whichSkull : Int
    , message : Maybe Message
    , messageLastMenuActive : Bool
    , saveStringEnter : Bool
    , saveSlot : Int
    , saveCharIndex : Int
    , saveOldString : String
    , savegamestrings : Array String
    , loadStatus : Array Bool
    , quickSaveSlot : Int
    , epi : Int
    , inhelpscreens : Bool
    , settings : Settings
    , mousewait : Int
    , mousey : Int
    , lasty : Int
    , mousex : Int
    , lastx : Int
    }


{-| M\_Init for a game mode, with these settings.
-}
init : GameMode -> Settings -> Menu
init mode s =
    Menu
        { gameMode = mode
        , active = False
        , current = MainDef
        , itemOn = 0
        , lastOn = Array.fromList [ 0, 0, 2, 0, 0, 0, 0, 0, 0 ]
        , skullAnimCounter = 10
        , whichSkull = 0
        , message = Nothing
        , messageLastMenuActive = False
        , saveStringEnter = False
        , saveSlot = 0
        , saveCharIndex = 0
        , saveOldString = ""
        , savegamestrings = Array.repeat slots ""
        , loadStatus = Array.repeat slots False
        , quickSaveSlot = -1
        , epi = 0
        , inhelpscreens = False
        , settings = s
        , mousewait = 0
        , mousey = 0
        , lasty = 0
        , mousex = 0
        , lastx = 0
        }


{-| The settings as they are now.
-}
settings : Menu -> Settings
settings (Menu m) =
    m.settings


{-| Replace the settings (a host restoring saved ones).
-}
setSettings : Settings -> Menu -> Menu
setSettings s (Menu m) =
    Menu { m | settings = s }


{-| menuactive: the menu (or a message) is on the screen.
-}
active : Menu -> Bool
active (Menu m) =
    m.active


{-| inhelpscreens: the last M\_Drawer drew a Read This! page.
-}
inHelpScreens : Menu -> Bool
inHelpScreens (Menu m) =
    m.inhelpscreens


{-| The quicksave slot (-1 none picked yet, -2 picking one now).
-}
quickSaveSlot : Menu -> Int
quickSaveSlot (Menu m) =
    m.quickSaveSlot



-- RESPONDING


{-| What the menu needs to know about the game: the mode, whether a game the player
started is going on (`usergame`), whether it is in a level, whether a demo plays or
the automap shows, the tic count (for the quit message), the time (I\_GetTime, which
the menu reads for mouse events), and the description of each saved game slot 0 to 5
(Nothing for an empty slot), and the font (for the width of a name being typed).
-}
type alias Context =
    { usergame : Bool
    , inLevel : Bool
    , netgame : Bool
    , demoplayback : Bool
    , automapActive : Bool
    , gametic : Int
    , time : Int
    , saveDescription : Int -> Maybe String
    , gfx : Gfx
    }


{-| What the game shell has to do for the menu, in order.

  - `PlaySound`: S\_StartSound(NULL, sfx).
  - `NewGame`: G\_DeferedInitNew skill episode map.
  - `EndGame`: D\_StartTitle.
  - `LoadGame slot`: G\_LoadGame.
  - `SaveGame slot description`: G\_SaveGame.
  - `Quit`: I\_Quit (a browser host goes back to the title instead).
  - `SetViewSize`: R\_SetViewSize with the settings' screenblocks and detail.
  - `PlayerMessage text`: the console player's message.
  - `ForceMessage`: message\_dontfuckwithme (show that message even if messages are off).
  - `ResetPalette`: I\_SetPalette(PLAYPAL), the normal palette.
  - `SfxVolume`, `MusicVolume`: S\_SetSfxVolume, S\_SetMusicVolume.

-}
type Effect
    = PlaySound Sfx
    | NewGame Skill Int Int
    | EndGame
    | LoadGame Int
    | SaveGame Int String
    | Quit
    | SetViewSize
    | PlayerMessage String
    | ForceMessage
    | ResetPalette
    | SfxVolume Int
    | MusicVolume Int


{-| What `responder` did: the menu after it, whether it ate the event, the effects,
and the time after the menu's I\_GetTime calls.
-}
type alias Response =
    { menu : Menu
    , eaten : Bool
    , effects : List Effect
    , time : Int
    }


type alias Step =
    { m : Model
    , fx : List Effect
    , time : Int
    }


emit : Effect -> Step -> Step
emit e s =
    { s | fx = e :: s.fx }


sound : Sfx -> Step -> Step
sound sfx =
    emit (PlaySound sfx)


finish : Bool -> Step -> Response
finish eaten s =
    { menu = Menu s.m, eaten = eaten, effects = List.reverse s.fx, time = s.time }


getTime : Step -> ( Int, Step )
getTime s =
    ( s.time, { s | time = s.time + 1 } )


{-| M\_Responder.
-}
responder : Context -> Event -> Menu -> Response
responder ctx ev (Menu m0) =
    let
        s0 =
            { m = m0, fx = [], time = ctx.time }

        ( ch, s1 ) =
            case ev of
                Mouse buttons dx dy ->
                    let
                        ( now, sNow ) =
                            getTime s0
                    in
                    if m0.mousewait < now then
                        mouseKey buttons dx dy sNow

                    else
                        ( -1, sNow )

                KeyDown k ->
                    ( k, s0 )

                KeyUp _ ->
                    ( -1, s0 )
    in
    if ch == -1 then
        finish False s1

    else
        respondKey ctx ch s1


mouseKey : Int -> Int -> Int -> Step -> ( Int, Step )
mouseKey buttons dx dy s0 =
    let
        wait n s =
            let
                ( now, sNow ) =
                    getTime s

                m =
                    sNow.m
            in
            { sNow | m = { m | mousewait = now + n } }

        m0 =
            s0.m

        my =
            m0.mousey + dy

        ( ch1, s1 ) =
            if my < m0.lasty - 30 then
                ( Event.keyDownArrow, wait 5 { s0 | m = { m0 | lasty = m0.lasty - 30, mousey = m0.lasty - 30 } } )

            else if my > m0.lasty + 30 then
                ( Event.keyUpArrow, wait 5 { s0 | m = { m0 | lasty = m0.lasty + 30, mousey = m0.lasty + 30 } } )

            else
                ( -1, { s0 | m = { m0 | mousey = my } } )

        m1 =
            s1.m

        mx =
            m1.mousex + dx

        ( ch2, s2 ) =
            if mx < m1.lastx - 30 then
                ( Event.keyLeftArrow, wait 5 { s1 | m = { m1 | lastx = m1.lastx - 30, mousex = m1.lastx - 30 } } )

            else if mx > m1.lastx + 30 then
                ( Event.keyRightArrow, wait 5 { s1 | m = { m1 | lastx = m1.lastx + 30, mousex = m1.lastx + 30 } } )

            else
                ( ch1, { s1 | m = { m1 | mousex = mx } } )

        ( ch3, s3 ) =
            if modBy 2 buttons == 1 then
                ( Event.keyEnter, wait 15 s2 )

            else
                ( ch2, s2 )
    in
    if modBy 4 buttons >= 2 then
        ( Event.keyBackspace, wait 15 s3 )

    else
        ( ch3, s3 )


toUpper : Int -> Int
toUpper c =
    if c >= Char.toCode 'a' && c <= Char.toCode 'z' then
        c - 32

    else
        c


respondKey : Context -> Int -> Step -> Response
respondKey ctx ch s =
    let
        m =
            s.m
    in
    if m.saveStringEnter then
        finish True (saveStringKey ctx.gfx ch s)

    else
        case m.message of
            Just msg ->
                if msg.needsInput && not (ch == Char.toCode ' ' || ch == Char.toCode 'n' || ch == Char.toCode 'y' || ch == Event.keyEscape) then
                    finish False s

                else
                    let
                        cleared =
                            { s | m = { m | active = m.messageLastMenuActive, message = Nothing } }

                        afterRoutine =
                            case msg.routine of
                                Just r ->
                                    messageResponse ctx r ch cleared

                                Nothing ->
                                    cleared

                        m2 =
                            afterRoutine.m
                    in
                    { afterRoutine | m = { m2 | active = False } }
                        |> sound Swtchx
                        |> finish True

            Nothing ->
                if not m.active then
                    functionKey ctx ch s

                else
                    menuKey ctx ch s


saveStringKey : Gfx -> Int -> Step -> Step
saveStringKey gfx ch s =
    let
        m =
            s.m

        current =
            Array.get m.saveSlot m.savegamestrings |> Maybe.withDefault ""

        setString str model =
            { model | savegamestrings = Array.set model.saveSlot str model.savegamestrings }
    in
    if ch == Event.keyBackspace then
        if m.saveCharIndex > 0 then
            { s | m = setString (String.left (m.saveCharIndex - 1) current) { m | saveCharIndex = m.saveCharIndex - 1 } }

        else
            s

    else if ch == Event.keyEscape then
        { s | m = setString m.saveOldString { m | saveStringEnter = False } }

    else if ch == Event.keyEnter then
        let
            ended =
                { s | m = { m | saveStringEnter = False } }
        in
        if current /= "" then
            doSave m.saveSlot ended

        else
            ended

    else
        let
            c =
                toUpper ch

            printable =
                c == 32 || (c - Gfx.fontStart >= 0 && c - Gfx.fontStart < Gfx.fontSize)
        in
        -- the width test is in the patches' pixels: a string of wide letters fills up first
        if printable && c >= 32 && c <= 127 && m.saveCharIndex < saveStringSize - 1 && Gfx.stringWidth current gfx < (saveStringSize - 2) * 8 then
            { s | m = setString (current ++ String.fromChar (Char.fromCode c)) { m | saveCharIndex = m.saveCharIndex + 1 } }

        else
            s


doSave : Int -> Step -> Step
doSave slot s =
    let
        m =
            s.m

        desc =
            Array.get slot m.savegamestrings |> Maybe.withDefault ""

        saved =
            emit (SaveGame slot desc) s
                |> clearMenus
    in
    if m.quickSaveSlot == -2 then
        let
            m2 =
                saved.m
        in
        { saved | m = { m2 | quickSaveSlot = slot } }

    else
        saved


clearMenus : Step -> Step
clearMenus s =
    let
        m =
            s.m
    in
    { s | m = { m | active = False } }


setupNextMenu : MenuId -> Step -> Step
setupNextMenu id s =
    let
        m =
            s.m
    in
    { s | m = { m | current = id, itemOn = Array.get (menuIndex id) m.lastOn |> Maybe.withDefault 0 } }


saveLastOn : Step -> Step
saveLastOn s =
    let
        m =
            s.m
    in
    { s | m = { m | lastOn = Array.set (menuIndex m.current) m.itemOn m.lastOn } }


startMessage : String -> Maybe MessageRoutine -> Bool -> Step -> Step
startMessage text routine needsInput s =
    let
        m =
            s.m
    in
    { s
        | m =
            { m
                | messageLastMenuActive = m.active
                , message = Just { text = text, routine = routine, needsInput = needsInput }
                , active = True
            }
    }


startPanel : Step -> Step
startPanel s =
    let
        m =
            s.m
    in
    if m.active then
        s

    else
        { s | m = { m | active = True, current = MainDef, itemOn = Array.get 0 m.lastOn |> Maybe.withDefault 0 } }


{-| M\_StartControlPanel: bring up the main menu (G\_Responder does this when a key is
pressed during the demos).
-}
startControlPanel : Menu -> Menu
startControlPanel menu =
    let
        (Menu m) =
            menu
    in
    Menu (startPanel { m = m, fx = [], time = 0 }).m


readSaveStrings : Context -> Step -> Step
readSaveStrings ctx s =
    let
        m =
            s.m

        descs =
            List.range 0 (slots - 1) |> List.map ctx.saveDescription
    in
    { s
        | m =
            { m
                | savegamestrings = Array.fromList (List.map (Maybe.withDefault emptyString) descs)
                , loadStatus = Array.fromList (List.map ((/=) Nothing) descs)
            }
    }


messageResponse : Context -> MessageRoutine -> Int -> Step -> Step
messageResponse ctx routine ch s =
    let
        yes =
            ch == Char.toCode 'y'
    in
    case routine of
        QuickSaveResponse ->
            if yes then
                doSave s.m.quickSaveSlot s |> sound Swtchx

            else
                s

        QuickLoadResponse ->
            if yes then
                loadSelect s.m.quickSaveSlot s |> sound Swtchx

            else
                s

        VerifyNightmare ->
            if yes then
                emit (NewGame Nightmare (s.m.epi + 1) 1) s |> clearMenus

            else
                s

        EndGameResponse ->
            if yes then
                saveLastOn s |> clearMenus |> emit EndGame

            else
                s

        QuitResponse ->
            if yes then
                let
                    sounds =
                        if s.m.gameMode == Commercial then
                            [ Vilact, Getpow, Boscub, Slop, Skeswg, Kntdth, Bspact, Sgtatk ]

                        else
                            [ Pldeth, Dmpain, Popain, Slop, Telept, Posit1, Posit3, Sgtatk ]

                    pick =
                        List.drop (modBy 8 (ctx.gametic // 4)) sounds |> List.head |> Maybe.withDefault Sgtatk
                in
                (if ctx.netgame then
                    s

                 else
                    sound pick s
                )
                    |> emit Quit

            else
                s


loadSelect : Int -> Step -> Step
loadSelect slot s =
    emit (LoadGame slot) s |> clearMenus


{-| The function keys and - =, when no menu is up; then Escape brings it up.
-}
functionKey : Context -> Int -> Step -> Response
functionKey ctx ch s =
    let
        mode =
            s.m.gameMode

        withMenu id itemOn st =
            let
                m =
                    st.m
            in
            { st | m = { m | current = id, itemOn = itemOn } }
    in
    if ch == Event.keyMinus || ch == Event.keyEquals then
        if ctx.automapActive then
            finish False s

        else
            sizeDisplay
                (if ch == Event.keyMinus then
                    0

                 else
                    1
                )
                s
                |> sound Stnmov
                |> finish True

    else if ch == Event.keyF1 then
        startPanel s
            |> withMenu
                (if mode == Retail then
                    ReadDef2

                 else
                    ReadDef1
                )
                0
            |> sound Swtchn
            |> finish True

    else if ch == Event.keyF2 then
        startPanel s |> sound Swtchn |> saveGame ctx |> finish True

    else if ch == Event.keyF3 then
        startPanel s |> sound Swtchn |> loadGame ctx |> finish True

    else if ch == Event.keyF4 then
        startPanel s |> withMenu SoundDef 0 |> sound Swtchn |> finish True

    else if ch == Event.keyF5 then
        changeDetail s |> sound Swtchn |> finish True

    else if ch == Event.keyF6 then
        sound Swtchn s |> quickSave ctx |> finish True

    else if ch == Event.keyF7 then
        sound Swtchn s |> endGame ctx |> finish True

    else if ch == Event.keyF8 then
        changeMessages s |> sound Swtchn |> finish True

    else if ch == Event.keyF9 then
        sound Swtchn s |> quickLoad ctx |> finish True

    else if ch == Event.keyF10 then
        sound Swtchn s |> quitDoom ctx |> finish True

    else if ch == Event.keyF11 then
        let
            m =
                s.m

            st =
                m.settings

            gamma =
                if st.usegamma + 1 > 4 then
                    0

                else
                    st.usegamma + 1
        in
        { s | m = { m | settings = { st | usegamma = gamma } } }
            |> emit (PlayerMessage (gammaMessage gamma))
            |> emit ResetPalette
            |> finish True

    else if ch == Event.keyEscape then
        startPanel s |> sound Swtchn |> finish True

    else
        finish False s


gammaMessage : Int -> String
gammaMessage gamma =
    if gamma == 0 then
        "Gamma correction OFF"

    else
        "Gamma correction level " ++ String.fromInt gamma


{-| The keys that work in a menu.
-}
menuKey : Context -> Int -> Step -> Response
menuKey ctx ch s =
    let
        m =
            s.m

        d =
            def m.gameMode m.loadStatus m.current

        count =
            Array.length d.items

        itemAt i =
            Array.get i d.items |> Maybe.withDefault emptyLine

        current =
            itemAt m.itemOn

        moveTo next st =
            let
                sm =
                    st.m

                st2 =
                    sound Pstop { st | m = { sm | itemOn = next } }
            in
            if (itemAt next).status == -1 then
                st2

            else
                st2

        down st =
            let
                next =
                    if st.m.itemOn + 1 > count - 1 then
                        0

                    else
                        st.m.itemOn + 1

                st2 =
                    moveTo next st
            in
            if (itemAt next).status == -1 then
                down st2

            else
                st2

        up st =
            let
                next =
                    if st.m.itemOn == 0 then
                        count - 1

                    else
                        st.m.itemOn - 1

                st2 =
                    moveTo next st
            in
            if (itemAt next).status == -1 then
                up st2

            else
                st2
    in
    if ch == Event.keyDownArrow then
        finish True (down s)

    else if ch == Event.keyUpArrow then
        finish True (up s)

    else if ch == Event.keyLeftArrow || ch == Event.keyRightArrow then
        case current.routine of
            Just r ->
                if current.status == 2 then
                    sound Stnmov s
                        |> runRoutine ctx
                            r
                            (if ch == Event.keyLeftArrow then
                                0

                             else
                                1
                            )
                        |> finish True

                else
                    finish True s

            Nothing ->
                finish True s

    else if ch == Event.keyEnter then
        case current.routine of
            Just r ->
                if current.status /= 0 then
                    let
                        saved =
                            saveLastOn s
                    in
                    if current.status == 2 then
                        runRoutine ctx r 1 saved |> sound Stnmov |> finish True

                    else
                        runRoutine ctx r m.itemOn saved |> sound Pistol |> finish True

                else
                    finish True s

            Nothing ->
                finish True s

    else if ch == Event.keyEscape then
        saveLastOn s |> clearMenus |> sound Swtchx |> finish True

    else if ch == Event.keyBackspace then
        let
            saved =
                saveLastOn s
        in
        case d.prev of
            Just prev ->
                let
                    sm =
                        saved.m
                in
                { saved | m = { sm | current = prev, itemOn = Array.get (menuIndex prev) sm.lastOn |> Maybe.withDefault 0 } }
                    |> sound Swtchn
                    |> finish True

            Nothing ->
                finish True saved

    else
        let
            -- DOS gives keys that type nothing (Pause, Caps Lock, Scroll Lock, Num Lock)
            -- the character 0, which finds the items without a hotkey
            typed =
                if List.member ch nullKeys then
                    0

                else
                    ch

            matching from to =
                List.range from to |> List.filter (\i -> (itemAt i).alphaKey == typed) |> List.head
        in
        case
            case matching (m.itemOn + 1) (count - 1) of
                Just i ->
                    Just i

                Nothing ->
                    matching 0 m.itemOn
        of
            Just i ->
                { s | m = { m | itemOn = i } } |> sound Pstop |> finish True

            Nothing ->
                finish False s


runRoutine : Context -> Routine -> Int -> Step -> Step
runRoutine ctx routine choice s =
    case routine of
        NewGameR ->
            if ctx.netgame && not ctx.demoplayback then
                startMessage newGameNet Nothing False s

            else if s.m.gameMode == Commercial then
                setupNextMenu NewDef s

            else
                setupNextMenu EpiDef s

        OptionsR ->
            setupNextMenu OptionsDef s

        LoadGameR ->
            loadGame ctx s

        SaveGameR ->
            saveGame ctx s

        ReadThisR ->
            setupNextMenu ReadDef1 s

        QuitDoomR ->
            quitDoom ctx s

        EpisodeR ->
            if s.m.gameMode == Shareware && choice /= 0 then
                startMessage swString Nothing False s |> setupNextMenu ReadDef1

            else
                let
                    epi =
                        if s.m.gameMode == Registered && choice > 2 then
                            0

                        else
                            choice

                    m =
                        s.m
                in
                setupNextMenu NewDef { s | m = { m | epi = epi } }

        ChooseSkillR ->
            if choice == 4 then
                startMessage nightmareMessage (Just VerifyNightmare) True s

            else
                emit (NewGame (Defs.skillFromIndex choice |> Maybe.withDefault Medium) (s.m.epi + 1) 1) s
                    |> clearMenus

        EndGameR ->
            endGame ctx s

        ChangeMessagesR ->
            changeMessages s

        ChangeDetailR ->
            changeDetail s

        SizeDisplayR ->
            sizeDisplay choice s

        ChangeSensitivityR ->
            updateSettings
                (\st ->
                    if choice == 0 then
                        if st.mouseSensitivity > 0 then
                            { st | mouseSensitivity = st.mouseSensitivity - 1 }

                        else
                            st

                    else if st.mouseSensitivity < 9 then
                        { st | mouseSensitivity = st.mouseSensitivity + 1 }

                    else
                        st
                )
                s

        SoundR ->
            setupNextMenu SoundDef s

        ReadThis2R ->
            setupNextMenu ReadDef2 s

        FinishReadThisR ->
            setupNextMenu MainDef s

        SfxVolR ->
            let
                s2 =
                    updateSettings (\st -> { st | sfxVolume = volume choice st.sfxVolume }) s
            in
            emit (SfxVolume s2.m.settings.sfxVolume) s2

        MusicVolR ->
            let
                s2 =
                    updateSettings (\st -> { st | musicVolume = volume choice st.musicVolume }) s
            in
            emit (MusicVolume s2.m.settings.musicVolume) s2

        LoadSelectR ->
            loadSelect choice s

        SaveSelectR ->
            let
                m =
                    s.m

                old =
                    Array.get choice m.savegamestrings |> Maybe.withDefault ""

                str =
                    if old == emptyString then
                        ""

                    else
                        old
            in
            { s
                | m =
                    { m
                        | saveStringEnter = True
                        , saveSlot = choice
                        , saveOldString = old
                        , savegamestrings = Array.set choice str m.savegamestrings
                        , saveCharIndex = String.length str
                    }
            }


volume : Int -> Int -> Int
volume choice v =
    if choice == 0 then
        if v > 0 then
            v - 1

        else
            v

    else if v < 15 then
        v + 1

    else
        v


updateSettings : (Settings -> Settings) -> Step -> Step
updateSettings f s =
    let
        m =
            s.m
    in
    { s | m = { m | settings = f m.settings } }


sizeDisplay : Int -> Step -> Step
sizeDisplay choice s =
    let
        screenSize =
            s.m.settings.screenblocks - 3
    in
    updateSettings
        (\st ->
            if choice == 0 then
                if screenSize > 0 then
                    { st | screenblocks = st.screenblocks - 1 }

                else
                    st

            else if screenSize < 8 then
                { st | screenblocks = st.screenblocks + 1 }

            else
                st
        )
        s
        |> emit SetViewSize


{-| M\_ChangeDetail as DOS Doom does it (linuxdoom only flipped the setting):
high or low detail, at once, with a message.
-}
changeDetail : Step -> Step
changeDetail s =
    let
        s2 =
            updateSettings (\st -> { st | detailLevel = 1 - st.detailLevel }) s
    in
    s2
        |> emit SetViewSize
        |> emit
            (PlayerMessage
                (if s2.m.settings.detailLevel == 0 then
                    "High detail"

                 else
                    "Low detail"
                )
            )


changeMessages : Step -> Step
changeMessages s =
    let
        s2 =
            updateSettings (\st -> { st | showMessages = not st.showMessages }) s
    in
    s2
        |> emit
            (PlayerMessage
                (if s2.m.settings.showMessages then
                    "Messages ON"

                 else
                    "Messages OFF"
                )
            )
        |> emit ForceMessage


loadGame : Context -> Step -> Step
loadGame ctx s =
    if ctx.netgame then
        startMessage (loadNet ++ pressKey) Nothing False s

    else
        setupNextMenu LoadDef s |> readSaveStrings ctx


saveGame : Context -> Step -> Step
saveGame ctx s =
    if not ctx.usergame then
        startMessage saveDead Nothing False s

    else if not ctx.inLevel then
        s

    else
        setupNextMenu SaveDef s |> readSaveStrings ctx


quickSave : Context -> Step -> Step
quickSave ctx s =
    if not ctx.usergame then
        sound Oof s

    else if not ctx.inLevel then
        s

    else if s.m.quickSaveSlot < 0 then
        let
            s2 =
                startPanel s |> readSaveStrings ctx |> setupNextMenu SaveDef

            m =
                s2.m
        in
        { s2 | m = { m | quickSaveSlot = -2 } }

    else
        startMessage
            ("quicksave over your game named\n\n'" ++ slotString s.m.quickSaveSlot s.m ++ "'?\n\n" ++ pressYN)
            (Just QuickSaveResponse)
            True
            s


quickLoad : Context -> Step -> Step
quickLoad ctx s =
    if ctx.netgame then
        startMessage ("you can't quickload during a netgame!\n\n" ++ pressKey) Nothing False s

    else if s.m.quickSaveSlot < 0 then
        startMessage ("you haven't picked a quicksave slot yet!\n\n" ++ pressKey) Nothing False s

    else
        startMessage
            ("do you want to quickload the game named\n\n'" ++ slotString s.m.quickSaveSlot s.m ++ "'?\n\n" ++ pressYN)
            (Just QuickLoadResponse)
            True
            s


slotString : Int -> Model -> String
slotString slot m =
    Array.get slot m.savegamestrings |> Maybe.withDefault ""


endGame : Context -> Step -> Step
endGame ctx s =
    if not ctx.usergame then
        sound Oof s

    else if ctx.netgame then
        startMessage ("you can't end a netgame!\n\n" ++ pressKey) Nothing False s

    else
        startMessage ("are you sure you want to end the game?\n\n" ++ pressYN) (Just EndGameResponse) True s


quitDoom : Context -> Step -> Step
quitDoom ctx s =
    let
        messages =
            if s.m.gameMode == Commercial then
                doom2QuitMessages

            else
                quitMessages

        msg =
            List.drop (modBy (List.length messages) ctx.gametic) messages
                |> List.head
                |> Maybe.withDefault ""
    in
    startMessage (msg ++ "\n\n(press y to quit to dos.)") (Just QuitResponse) True s


{-| The quit messages of DOS Doom v1.9 (one picked by gametic modulo 8; linuxdoom's
longer list, with its missing commas, isn't what the DOS game shows).
-}
quitMessages : List String
quitMessages =
    [ "are you sure you want to\nquit this great game?"
    , "please don't leave, there's more\ndemons to toast!"
    , "let's beat it -- this is turning\ninto a bloodbath!"
    , "i wouldn't leave if i were you.\ndos is much worse."
    , "you're trying to say you like dos\nbetter than me, right?"
    , "don't leave yet -- there's a\ndemon around that corner!"
    , "ya know, next time you come in here\ni'm gonna toast ya."
    , "go ahead and leave. see if i care."
    ]


{-| Doom II's.
-}
doom2QuitMessages : List String
doom2QuitMessages =
    [ "are you sure you want to\nquit this great game?"
    , "you want to quit?\nthen, thou hast lost an eighth!"
    , "don't go now, there's a \ndimensional shambler waiting\nat the dos prompt!"
    , "get outta here and go back\nto your boring programs."
    , "if i were your boss, i'd \n deathmatch ya in a minute!"
    , "look, bud. you leave now\nand you forfeit your body count!"
    , "just leave. when you come\nback, i'll be waiting with a bat."
    , "you're lucky i don't smack\nyou for thinking about leaving."
    ]


pressKey : String
pressKey =
    "press a key."


pressYN : String
pressYN =
    "press y or n."


loadNet : String
loadNet =
    "you can't do load while in a net game!\n\n"


saveDead : String
saveDead =
    "you can't save if you aren't playing!\n\n" ++ pressKey


newGameNet : String
newGameNet =
    "you can't start a new game\nwhile in a network game.\n\n" ++ pressKey


nightmareMessage : String
nightmareMessage =
    "are you sure? this skill level\nisn't even remotely fair.\n\n" ++ pressYN


swString : String
swString =
    "this is the shareware version of doom.\n\nyou need to order the entire trilogy.\n\n" ++ pressKey



-- TICKING


{-| M\_Ticker: the skull blinks every 8 tics.
-}
ticker : Menu -> Menu
ticker (Menu m) =
    if m.skullAnimCounter - 1 <= 0 then
        Menu { m | whichSkull = 1 - m.whichSkull, skullAnimCounter = 8 }

    else
        Menu { m | skullAnimCounter = m.skullAnimCounter - 1 }



-- DRAWING


{-| M\_Drawer: the message if there is one, else the menu and its skull. Gives back
the menu with `inHelpScreens` set if it drew a Read This! page.
-}
drawer : Gfx -> Menu -> Framebuffer -> ( Menu, Framebuffer )
drawer gfx (Menu m0) fb =
    let
        m =
            { m0 | inhelpscreens = False }
    in
    case m.message of
        Just msg ->
            ( Menu m, drawMessage gfx msg.text fb )

        Nothing ->
            if not m.active then
                ( Menu m, fb )

            else
                let
                    d =
                        def m.gameMode m.loadStatus m.current

                    ( m2, withPage ) =
                        drawRoutine gfx d m fb

                    withItems =
                        Array.foldl
                            (\it ( y, acc ) ->
                                ( y + lineHeight
                                , if it.name /= "" then
                                    Gfx.draw d.x y it.name gfx acc

                                  else
                                    acc
                                )
                            )
                            ( d.y, withPage )
                            d.items
                            |> Tuple.second

                    skull =
                        if m.whichSkull == 0 then
                            "M_SKULL1"

                        else
                            "M_SKULL2"
                in
                ( Menu m2, Gfx.draw (d.x + skullXOff) (d.y - 5 + m.itemOn * lineHeight) skull gfx withItems )


{-| M\_Drawer's message: centred line by line, as the C loop cuts it. That loop has a
slip: after cutting a line at its "\\n" it compares the line's length with what is
left of the string, and when they happen to be equal it takes everything left as
the line instead (so "at the dos prompt!" never shows: the rest, "\\n(press y to
quit)", replaces it, drawn a line further down).
-}
drawMessage : Gfx -> String -> Framebuffer -> Framebuffer
drawMessage gfx text fb =
    let
        lineH =
            Gfx.fontHeight gfx

        total =
            String.length text

        go start y acc =
            if start >= total then
                acc

            else
                let
                    rest =
                        String.dropLeft start text

                    ( line, next ) =
                        case List.head (String.indexes "\n" rest) of
                            Just i ->
                                let
                                    after =
                                        start + i + 1
                                in
                                if i == total - after then
                                    ( String.dropLeft after text, after + i )

                                else
                                    ( String.left i rest, after )

                            Nothing ->
                                ( rest, total )
                in
                go next (y + lineH) (Gfx.writeText (160 - Gfx.stringWidth line gfx // 2) y line gfx acc)
    in
    go 0 (100 - Gfx.stringHeight text gfx // 2) fb


drawRoutine : Gfx -> Def -> Model -> Framebuffer -> ( Model, Framebuffer )
drawRoutine gfx d m fb =
    let
        draw x y name =
            Gfx.draw x y name gfx

        st =
            m.settings
    in
    case d.draw of
        DrawMain ->
            ( m, draw 94 2 "M_DOOM" fb )

        DrawEpisode ->
            ( m, draw 54 38 "M_EPISOD" fb )

        DrawNewGame ->
            ( m, fb |> draw 96 14 "M_NEWG" |> draw 54 38 "M_SKILL" )

        DrawOptions ->
            let
                od =
                    def m.gameMode m.loadStatus OptionsDef
            in
            ( m
            , fb
                |> draw 108 15 "M_OPTTTL"
                |> draw (od.x + 175)
                    (od.y + lineHeight * 2)
                    (if st.detailLevel == 0 then
                        "M_GDHIGH"

                     else
                        "M_GDLOW"
                    )
                |> draw (od.x + 120)
                    (od.y + lineHeight * 1)
                    (if st.showMessages then
                        "M_MSGON"

                     else
                        "M_MSGOFF"
                    )
                |> drawThermo gfx od.x (od.y + lineHeight * 6) 10 st.mouseSensitivity
                |> drawThermo gfx od.x (od.y + lineHeight * 4) 9 (st.screenblocks - 3)
            )

        DrawReadThis1 ->
            ( { m | inhelpscreens = True }
            , case m.gameMode of
                Commercial ->
                    draw 0 0 "HELP" fb

                _ ->
                    draw 0 0 "HELP2" fb
            )

        DrawReadThis2 ->
            ( { m | inhelpscreens = True }, draw 0 0 "HELP1" fb )

        DrawSound ->
            ( m
            , fb
                |> draw 60 38 "M_SVOL"
                |> drawThermo gfx d.x (d.y + lineHeight * 1) 16 st.sfxVolume
                |> drawThermo gfx d.x (d.y + lineHeight * 3) 16 st.musicVolume
            )

        DrawLoad ->
            ( m, draw 72 28 "M_LOADG" fb |> drawSlots gfx m )

        DrawSave ->
            let
                withSlots =
                    draw 72 28 "M_SAVEG" fb |> drawSlots gfx m
            in
            ( m
            , if m.saveStringEnter then
                let
                    w =
                        Gfx.stringWidth (slotString m.saveSlot m) gfx
                in
                Gfx.writeText (80 + w) (54 + lineHeight * m.saveSlot) "_" gfx withSlots

              else
                withSlots
            )


drawSlots : Gfx -> Model -> Framebuffer -> Framebuffer
drawSlots gfx m fb =
    List.foldl
        (\i acc ->
            let
                y =
                    54 + lineHeight * i
            in
            acc
                |> drawSaveLoadBorder gfx 80 y
                |> Gfx.writeText 80 y (slotString i m) gfx
        )
        fb
        (List.range 0 (slots - 1))


drawSaveLoadBorder : Gfx -> Int -> Int -> Framebuffer -> Framebuffer
drawSaveLoadBorder gfx x y fb =
    let
        middle =
            List.foldl (\i acc -> Gfx.draw (x + 8 * i) (y + 7) "M_LSCNTR" gfx acc)
                (Gfx.draw (x - 8) (y + 7) "M_LSLEFT" gfx fb)
                (List.range 0 23)
    in
    Gfx.draw (x + 8 * 24) (y + 7) "M_LSRGHT" gfx middle


drawThermo : Gfx -> Int -> Int -> Int -> Int -> Framebuffer -> Framebuffer
drawThermo gfx x y thermWidth thermDot fb =
    let
        cells =
            List.foldl (\i acc -> Gfx.draw (x + 8 + 8 * i) y "M_THERMM" gfx acc)
                (Gfx.draw x y "M_THERML" gfx fb)
                (List.range 0 (thermWidth - 1))
    in
    cells
        |> Gfx.draw (x + 8 + 8 * thermWidth) y "M_THERMR" gfx
        |> Gfx.draw (x + 8 + thermDot * 8) y "M_THERMO" gfx
