module Doom.Hud.Messages exposing
    ( Model, init, start, forceNextMessage
    , ticker, responder
    , ViewWindow, erase, drawer
    , title
    )

{-| The heads-up messages (hu\_stuff.c, hu\_lib.c): the player's message line at the
top, and the level's name on the automap.

Chat isn't translated: it only happens in a netgame (the 't' key, the destination
keys, the chat macros, the incoming chat characters of HU\_Ticker). Of HU\_Responder
only what works outside chat is here: Enter shows the last message again.

The original draws the message straight onto the screen each frame and erases it
(HU\_Erase, copying the view border back from the back screen) for four frames after
it changes or goes away, so `erase` and `drawer` must be given the screen as the last
frame left it.

@docs Model, init, start, forceNextMessage
@docs ticker, responder
@docs ViewWindow, erase, drawer
@docs title

-}

import Array exposing (Array)
import Doom.Defs exposing (GameMode(..))
import Doom.Event exposing (Event(..))
import Doom.Play.Env exposing (Env)
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (World)


{-| The message line's state, the level title line, and the font.
-}
type Model
    = Model State


type alias State =
    { font : Array (Maybe Patch)
    , fontHeight : Int
    , messageOn : Bool
    , dontFuckWithMe : Bool
    , notToBeFuckedWith : Bool
    , counter : Int
    , message : Line
    , messageLaston : Bool
    , title : Line
    , chat : Line
    , chatLaston : Bool
    }


{-| hu\_textline\_t: where it is, its text, and for how many more frames HU\_Erase
erases it (`needsupdate`).
-}
type alias Line =
    { x : Int
    , y : Int
    , text : List Char
    , needsupdate : Int
    }


fontStart : Int
fontStart =
    Char.toCode '!'


fontEnd : Int
fontEnd =
    Char.toCode '_'


maxLineLength : Int
maxLineLength =
    80


{-| HU\_MSGTIMEOUT: four seconds.
-}
msgTimeout : Int
msgTimeout =
    4 * 35


{-| HU\_Init: load the font (STCFN033 to STCFN095, '!' to '\_').
-}
init : Wad -> Model
init wad =
    let
        font =
            List.range fontStart fontEnd
                |> List.map (\c -> Wad.lump ("STCFN" ++ String.padLeft 3 '0' (String.fromInt c)) wad |> Maybe.andThen Video.patch)
                |> Array.fromList

        height =
            Array.get 0 font
                |> Maybe.andThen identity
                |> Maybe.map (\p -> (Video.patchSize p).height)
                |> Maybe.withDefault 0

        empty =
            { x = 0, y = 0, text = [], needsupdate = 1 }
    in
    Model
        { font = font
        , fontHeight = height
        , messageOn = False
        , dontFuckWithMe = False
        , notToBeFuckedWith = False
        , counter = 0
        , message = empty
        , messageLaston = True
        , title = empty
        , chat = empty
        , chatLaston = True
        }


{-| HU\_Start (P\_SpawnPlayer calls it with ST\_Start): no message showing, and the level's
title ready for the automap. The message counter carries on (C doesn't reset it).
-}
start : Env -> World -> Model -> Model
start env _ (Model st) =
    let
        h =
            st.fontHeight
    in
    Model
        { st
            | messageOn = False
            , dontFuckWithMe = False
            , notToBeFuckedWith = False
            , message = { x = 0, y = 0, text = [], needsupdate = 1 }
            , messageLaston = True
            , title = addText (title env) { x = 0, y = 167 - h, text = [], needsupdate = 1 }
            , chat = { x = 0, y = h + 1, text = [], needsupdate = 1 }
            , chatLaston = True
        }


{-| The level's name as the automap shows it (HU\_TITLE, HU\_TITLE2: d\_englsh.h's
names, whatever the WAD's levels are called).
-}
title : Env -> String
title env =
    let
        p =
            env.params
    in
    case p.gameMode of
        Commercial ->
            List.drop (p.map - 1) commercialNames |> List.head |> Maybe.withDefault ""

        _ ->
            List.drop ((p.episode - 1) * 9 + p.map - 1) doomNames |> List.head |> Maybe.withDefault ""


{-| HUlib\_addCharToTextLine for each character, up to the line's 80.
-}
addText : String -> Line -> Line
addText s line =
    String.foldl
        (\c l ->
            if List.length l.text >= maxLineLength then
                l

            else
                { l | text = l.text ++ [ c ], needsupdate = 4 }
        )
        line
        s


{-| Set `message_dontfuckwithme`, as M\_ChangeMessages does when messages are turned
on or off: the next message shows even with messages off, and isn't replaced by
another until it times out. HU\_Ticker clears it again.
-}
forceNextMessage : Model -> Model
forceNextMessage (Model st) =
    Model { st | dontFuckWithMe = True }


{-| HU\_Ticker for the console player `n`: time the message out, and pick up the
player's new message (clearing it) when messages are on. With messages off a message
stays in the player until they're turned on again (or a level starts).
-}
ticker : { showMessages : Bool } -> World -> Int -> Model -> ( Model, World )
ticker { showMessages } world n (Model st0) =
    let
        st1 =
            if st0.counter /= 0 then
                if st0.counter - 1 == 0 then
                    { st0 | counter = 0, messageOn = False, notToBeFuckedWith = False }

                else
                    { st0 | counter = st0.counter - 1 }

            else
                st0
    in
    if showMessages || st1.dontFuckWithMe then
        case World.player n world |> Maybe.andThen .message of
            Just text ->
                if not st1.notToBeFuckedWith || st1.dontFuckWithMe then
                    ( Model
                        { st1
                            | message = addText text { x = st1.message.x, y = st1.message.y, text = [], needsupdate = 4 }
                            , messageOn = True
                            , counter = msgTimeout
                            , notToBeFuckedWith = st1.dontFuckWithMe
                            , dontFuckWithMe = False
                        }
                    , World.updatePlayer n (\p -> { p | message = Nothing }) world
                    )

                else
                    ( Model st1, world )

            Nothing ->
                ( Model st1, world )

    else
        ( Model st1, world )


{-| HU\_Responder outside chat: Enter (HU\_MSGREFRESH) shows the last message again for
another four seconds, and is taken; nothing else is.
-}
responder : Event -> Model -> Maybe Model
responder event (Model st) =
    case event of
        KeyDown 13 ->
            Just (Model { st | messageOn = True, counter = msgTimeout })

        _ ->
            Nothing


{-| The 3D view's window on the screen (r\_draw.c's `viewwindowx`, `viewwindowy`,
`viewwidth`, `viewheight`, in screen pixels).
-}
type alias ViewWindow =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


{-| HU\_Erase: for the frames after a line changed or went away, copy the back screen
(`screens[1]`, the view border) over it, outside the view window. Only with the view
reduced and the automap off. The first screen is the back screen, the second the
screen being drawn.
-}
erase : Bool -> ViewWindow -> Framebuffer -> Model -> Framebuffer -> ( Model, Framebuffer )
erase automap view back (Model st) fb0 =
    let
        lineHeight =
            st.fontHeight + 1

        eraseLine line fb =
            if not automap && view.x /= 0 && line.needsupdate /= 0 then
                let
                    -- the line's rows: those above and below the view window
                    -- whole, those beside it on either side (in one rectangle each,
                    -- as the row-by-row R_VideoErase calls come to the same)
                    bottom =
                        line.y + lineHeight

                    viewBottom =
                        view.y + view.height

                    whole from to acc =
                        if from < to then
                            Video.copyRect 0 from Video.width (to - from) back 0 from acc

                        else
                            acc

                    beside from to acc =
                        if from < to then
                            acc
                                |> Video.copyRect 0 from view.x (to - from) back 0 from
                                |> Video.copyRect (view.x + view.width) from view.x (to - from) back (view.x + view.width) from

                        else
                            acc
                in
                ( { line | needsupdate = line.needsupdate - 1 }
                , fb
                    |> whole line.y (min bottom view.y)
                    |> beside (max line.y view.y) (min bottom viewBottom)
                    |> whole (max line.y viewBottom) bottom
                )

            else if line.needsupdate /= 0 then
                ( { line | needsupdate = line.needsupdate - 1 }, fb )

            else
                ( line, fb )

        touch laston on line =
            if laston && not on then
                { line | needsupdate = 4 }

            else
                line

        ( message, fb1 ) =
            eraseLine (touch st.messageLaston st.messageOn st.message) fb0

        ( chat, fb2 ) =
            eraseLine (touch st.chatLaston False st.chat) fb1

        ( titleLine, fb3 ) =
            eraseLine st.title fb2
    in
    ( Model
        { st
            | message = message
            , messageLaston = st.messageOn
            , chat = chat
            , chatLaston = False
            , title = titleLine
        }
    , fb3
    )


{-| HU\_Drawer: the message line if it's showing, and the level's title when the
automap is up (`automapActive`).
-}
drawer : Env -> Bool -> Model -> Framebuffer -> Framebuffer
drawer _ automapActive (Model st) fb =
    let
        withMessage =
            if st.messageOn then
                drawLine st.font st.message fb

            else
                fb
    in
    if automapActive then
        drawLine st.font st.title withMessage

    else
        withMessage


{-| HUlib\_drawTextLine without the cursor: each character in upper case from the font,
a space (or anything the font hasn't got) 4 pixels wide, stopping at the right edge.
-}
drawLine : Array (Maybe Patch) -> Line -> Framebuffer -> Framebuffer
drawLine font line fb0 =
    let
        go chars x fb =
            case chars of
                [] ->
                    fb

                ch :: rest ->
                    let
                        c =
                            Char.toCode (toUpper ch)
                    in
                    if c /= 32 && c >= fontStart && c <= fontEnd then
                        case Array.get (c - fontStart) font |> Maybe.andThen identity of
                            Just p ->
                                let
                                    w =
                                        (Video.patchSize p).width
                                in
                                if x + w > Video.width then
                                    fb

                                else
                                    go rest (x + w) (Video.drawPatch x line.y p fb)

                            Nothing ->
                                fb

                    else if x + 4 >= Video.width then
                        fb

                    else
                        go rest (x + 4) fb
    in
    go line.text line.x fb0


{-| C's toupper: only a to z.
-}
toUpper : Char -> Char
toUpper c =
    if c >= 'a' && c <= 'z' then
        Char.toUpper c

    else
        c


{-| mapnames[]: Doom's level names, episode by episode.
-}
doomNames : List String
doomNames =
    [ "E1M1: Hangar"
    , "E1M2: Nuclear Plant"
    , "E1M3: Toxin Refinery"
    , "E1M4: Command Control"
    , "E1M5: Phobos Lab"
    , "E1M6: Central Processing"
    , "E1M7: Computer Station"
    , "E1M8: Phobos Anomaly"
    , "E1M9: Military Base"
    , "E2M1: Deimos Anomaly"
    , "E2M2: Containment Area"
    , "E2M3: Refinery"
    , "E2M4: Deimos Lab"
    , "E2M5: Command Center"
    , "E2M6: Halls of the Damned"
    , "E2M7: Spawning Vats"
    , "E2M8: Tower of Babel"
    , "E2M9: Fortress of Mystery"
    , "E3M1: Hell Keep"
    , "E3M2: Slough of Despair"
    , "E3M3: Pandemonium"
    , "E3M4: House of Pain"
    , "E3M5: Unholy Cathedral"
    , "E3M6: Mt. Erebus"
    , "E3M7: Limbo"
    , "E3M8: Dis"
    , "E3M9: Warrens"
    , "E4M1: Hell Beneath"
    , "E4M2: Perfect Hatred"
    , "E4M3: Sever The Wicked"
    , "E4M4: Unruly Evil"
    , "E4M5: They Will Repent"
    , "E4M6: Against Thee Wickedly"
    , "E4M7: And Hell Followed"
    , "E4M8: Unto The Cruel"
    , "E4M9: Fear"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    , "NEWLEVEL"
    ]


{-| mapnames2[]: Doom II's level names.
-}
commercialNames : List String
commercialNames =
    [ "level 1: entryway"
    , "level 2: underhalls"
    , "level 3: the gantlet"
    , "level 4: the focus"
    , "level 5: the waste tunnels"
    , "level 6: the crusher"
    , "level 7: dead simple"
    , "level 8: tricks and traps"
    , "level 9: the pit"
    , "level 10: refueling base"
    , "level 11: 'o' of destruction!"
    , "level 12: the factory"
    , "level 13: downtown"
    , "level 14: the inmost dens"
    , "level 15: industrial zone"
    , "level 16: suburbs"
    , "level 17: tenements"
    , "level 18: the courtyard"
    , "level 19: the citadel"
    , "level 20: gotcha!"
    , "level 21: nirvana"
    , "level 22: the catacombs"
    , "level 23: barrels o' fun"
    , "level 24: the chasm"
    , "level 25: bloodfalls"
    , "level 26: the abandoned mines"
    , "level 27: monster condo"
    , "level 28: the spirit world"
    , "level 29: the living end"
    , "level 30: icon of sin"
    , "level 31: wolfenstein"
    , "level 32: grosse"
    ]
