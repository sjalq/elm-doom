module Doom.Intermission.Draw exposing (drawer)

{-| The intermission screen (wi\_stuff.c's WI\_Drawer and its pieces): the background
(the episode's map, or INTERPIC) with its animations, the finished and next level's
names, the statistics with their numbers, percentages and times, the net game and
deathmatch tables, and on the episode maps the splats on the levels done and the
blinking "you are here".

The pictures are read from the WAD each time, as W\_CacheLumpName does; one missing
is left out (the original stops with an error).

This is DOS Doom v1.9's screen. linuxdoom-1.10 differs in two bugs, both tests of an
enum constant (always true) instead of a variable: it never draws the background
animations (`if (commercial)` in WI\_drawAnimatedBack), and a co-op game asks for a
French "items" picture no IWAD has (`if (french)` in WI\_loadData).

@docs drawer

-}

import Array
import Doom.Defs as Defs exposing (GameMode(..))
import Doom.Intermission exposing (Intermission, Stage(..))
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)


{-| WI\_Drawer: the current stage's screen. Everything is drawn over the background
(WI\_slamBackground copies all of it), so what was on the screen doesn't show.
-}
drawer : Wad -> Intermission -> Framebuffer -> Framebuffer
drawer wad wi _ =
    let
        p =
            pics wad wi
    in
    case wi.stage of
        StatCount ->
            if wi.ctx.deathmatch then
                drawDeathmatchStats p wi

            else if wi.ctx.netgame then
                drawNetgameStats p wi

            else
                drawStats p wi

        ShowNextLoc ->
            drawShowNextLoc p wi

        NoState ->
            -- WI_drawNoState turns the arrow on before drawing
            drawShowNextLoc p { wi | pointerOn = True }



-- PICTURES


type alias Pics =
    { wad : Wad
    , get : String -> Maybe Patch
    , num : Int -> Maybe Patch
    , lname : Int -> Maybe Patch
    }


pics : Wad -> Intermission -> Pics
pics wad wi =
    let
        get name =
            Wad.lump name wad |> Maybe.andThen Video.patch
    in
    { wad = wad
    , get = get
    , num = \n -> get ("WINUM" ++ String.fromInt n)
    , lname =
        \n ->
            if wi.ctx.gameMode == Commercial then
                get ("CWILV" ++ String.padLeft 2 '0' (String.fromInt n))

            else
                get ("WILV" ++ String.fromInt wi.wbs.epsd ++ String.fromInt n)
    }


draw : Int -> Int -> Maybe Patch -> Framebuffer -> Framebuffer
draw x y maybePatch fb =
    case maybePatch of
        Just pt ->
            Video.drawPatch x y pt fb

        Nothing ->
            fb


widthOf : Maybe Patch -> Int
widthOf =
    Maybe.map (Video.patchSize >> .width) >> Maybe.withDefault 0


heightOf : Maybe Patch -> Int
heightOf =
    Maybe.map (Video.patchSize >> .height) >> Maybe.withDefault 0


{-| WI\_loadData's background, drawn into screens[1] (which starts out black), and
WI\_slamBackground's copy of it.
-}
slamBackground : Pics -> Intermission -> Framebuffer
slamBackground p wi =
    let
        name =
            if wi.ctx.gameMode == Commercial || (wi.ctx.gameMode == Retail && wi.wbs.epsd == 3) then
                "INTERPIC"

            else
                "WIMAP" ++ String.fromInt wi.wbs.epsd
    in
    Video.blank 0 |> draw 0 0 (p.get name)


{-| WI\_drawAnimatedBack: each animation's current frame, if it has one showing.
-}
drawAnimatedBack : Pics -> Intermission -> Framebuffer -> Framebuffer
drawAnimatedBack p wi fb =
    if wi.ctx.gameMode == Commercial || wi.wbs.epsd > 2 then
        fb

    else
        Doom.Intermission.animDefs wi.ctx.gameMode wi.wbs.epsd
            |> Array.toIndexedList
            |> List.foldl
                (\( i, a ) acc ->
                    case Array.get i wi.anims of
                        Just st ->
                            if st.ctr >= 0 then
                                let
                                    -- the "MONDO HACK": episode 2's ninth uses the fifth's pictures
                                    j =
                                        if wi.wbs.epsd == 1 && i == 8 then
                                            4

                                        else
                                            i
                                in
                                draw a.x a.y (p.get ("WIA" ++ String.fromInt wi.wbs.epsd ++ two j ++ two st.ctr)) acc

                            else
                                acc

                        Nothing ->
                            acc
                )
                fb


two : Int -> String
two n =
    String.padLeft 2 '0' (String.fromInt n)



-- TITLES


titleY : Int
titleY =
    2


{-| WI\_drawLF: "<level> Finished!".
-}
drawLF : Pics -> Intermission -> Framebuffer -> Framebuffer
drawLF p wi fb =
    let
        name =
            p.lname wi.wbs.last

        finished =
            p.get "WIF"
    in
    fb
        |> draw ((Video.width - widthOf name) // 2) titleY name
        |> draw ((Video.width - widthOf finished) // 2) (titleY + (5 * heightOf name) // 4) finished


{-| WI\_drawEL: "Entering <level>".
-}
drawEL : Pics -> Intermission -> Framebuffer -> Framebuffer
drawEL p wi fb =
    let
        name =
            p.lname wi.wbs.next

        entering =
            p.get "WIENTER"
    in
    fb
        |> draw ((Video.width - widthOf entering) // 2) titleY entering
        |> draw ((Video.width - widthOf name) // 2) (titleY + (5 * heightOf name) // 4) name



-- THE EPISODE MAP


{-| lnodes: where each level is on the three episode maps.
-}
lnode : Int -> Int -> ( Int, Int )
lnode epsd n =
    let
        nodes =
            case epsd of
                0 ->
                    [ ( 185, 164 ), ( 148, 143 ), ( 69, 122 ), ( 209, 102 ), ( 116, 89 ), ( 166, 55 ), ( 71, 56 ), ( 135, 29 ), ( 71, 24 ) ]

                1 ->
                    [ ( 254, 25 ), ( 97, 50 ), ( 188, 64 ), ( 128, 78 ), ( 214, 92 ), ( 133, 130 ), ( 208, 136 ), ( 148, 140 ), ( 235, 158 ) ]

                _ ->
                    [ ( 156, 168 ), ( 48, 154 ), ( 174, 95 ), ( 265, 75 ), ( 130, 48 ), ( 279, 23 ), ( 198, 48 ), ( 140, 25 ), ( 281, 136 ) ]
    in
    List.drop n nodes |> List.head |> Maybe.withDefault ( 0, 0 )


{-| WI\_drawOnLnode: the first of the pictures that fits on the screen there.
-}
drawOnLnode : Intermission -> Int -> List (Maybe Patch) -> Framebuffer -> Framebuffer
drawOnLnode wi n choices fb =
    let
        ( x, y ) =
            lnode wi.wbs.epsd n

        fits pt =
            let
                s =
                    Video.patchSize pt

                left =
                    x - s.leftoffset

                top =
                    y - s.topoffset
            in
            left >= 0 && left + s.width < Video.width && top >= 0 && top + s.height < Video.height
    in
    case List.filterMap identity choices |> List.filter fits |> List.head of
        Just pt ->
            Video.drawPatch x y pt fb

        Nothing ->
            fb


{-| WI\_drawShowNextLoc: the map with the levels done splatted, the arrow on the next
one when it is on, and "Entering <level>" (not before Doom II's MAP31).
-}
drawShowNextLoc : Pics -> Intermission -> Framebuffer
drawShowNextLoc p wi =
    let
        wbs =
            wi.wbs

        background =
            slamBackground p wi |> drawAnimatedBack p wi

        entering fb =
            if wi.ctx.gameMode /= Commercial || wbs.next /= 30 then
                drawEL p wi fb

            else
                fb
    in
    if wi.ctx.gameMode /= Commercial then
        if wbs.epsd > 2 then
            drawEL p wi background

        else
            let
                last =
                    if wbs.last == 8 then
                        wbs.next - 1

                    else
                        wbs.last

                splat =
                    [ p.get "WISPLAT" ]

                splatted =
                    List.foldl (\i fb -> drawOnLnode wi i splat fb) background (List.range 0 last)
                        |> (if wbs.didsecret then
                                drawOnLnode wi 8 splat

                            else
                                identity
                           )
                        |> (if wi.pointerOn then
                                drawOnLnode wi wbs.next [ p.get "WIURH0", p.get "WIURH1" ]

                            else
                                identity
                           )
            in
            entering splatted

    else
        entering background



-- NUMBERS


{-| WI\_drawNum: `n` right-aligned to `x`, with at least `digits` digits (as many as
it takes if `digits` is negative), a minus sign in front if negative. 1994 is "no
number" and isn't drawn. Gives the left edge.
-}
drawNum : Pics -> Int -> Int -> Int -> Int -> Framebuffer -> ( Int, Framebuffer )
drawNum p x y n digits0 fb =
    let
        fontwidth =
            widthOf (p.num 0)

        digits =
            if digits0 < 0 then
                if n == 0 then
                    1

                else
                    countDigits n 0

            else
                digits0

        neg =
            n < 0

        magnitude =
            abs n

        drawDigits k m ( cx, acc ) =
            if k <= 0 then
                ( cx, acc )

            else
                let
                    nx =
                        cx - fontwidth
                in
                drawDigits (k - 1) (m // 10) ( nx, draw nx y (p.num (remainderBy 10 m)) acc )
    in
    if magnitude == 1994 then
        ( 0, fb )

    else
        let
            ( left, drawn ) =
                drawDigits digits magnitude ( x, fb )
        in
        if neg then
            ( left - 8, draw (left - 8) y (p.get "WIMINUS") drawn )

        else
            ( left, drawn )


countDigits : Int -> Int -> Int
countDigits n acc =
    if n == 0 then
        acc

    else
        countDigits (n // 10) (acc + 1)


{-| WI\_drawPercent: nothing while the count hasn't started (negative).
-}
drawPercent : Pics -> Int -> Int -> Int -> Framebuffer -> Framebuffer
drawPercent p x y n fb =
    if n < 0 then
        fb

    else
        fb |> draw x y (p.get "WIPCNT") |> drawNum p x y n -1 |> Tuple.second


{-| WI\_drawTime: seconds as m:ss (or h:mm:ss) right-aligned to `x`, nothing while
the count hasn't started, and "sucks" past 61 \* 59 seconds.
-}
drawTime : Pics -> Int -> Int -> Int -> Framebuffer -> Framebuffer
drawTime p x0 y t fb0 =
    let
        colon =
            p.get "WICOLON"

        go x div fb =
            let
                ( left, drawn ) =
                    drawNum p x y (modBy 60 (t // div)) 2 fb

                cx =
                    left - widthOf colon

                nextDiv =
                    div * 60

                withColon =
                    if nextDiv == 60 || t // nextDiv /= 0 then
                        draw cx y colon drawn

                    else
                        drawn
            in
            if t // nextDiv /= 0 then
                go cx nextDiv withColon

            else
                withColon
    in
    if t < 0 then
        fb0

    else if t <= 61 * 59 then
        go x0 1 fb0

    else
        let
            sucks =
                p.get "WISUCKS"
        in
        draw (x0 - widthOf sucks) y sucks fb0



-- STATISTICS


spacingY : Int
spacingY =
    33


inGame : Intermission -> Int -> Bool
inGame wi i =
    Array.get i wi.ctx.playeringame == Just True


count : Int -> Array.Array Int -> Int
count i a =
    Array.get i a |> Maybe.withDefault 0


{-| WI\_drawStats (single player).
-}
drawStats : Pics -> Intermission -> Framebuffer
drawStats p wi =
    let
        lh =
            (3 * heightOf (p.num 0)) // 2

        statsX =
            50

        statsY =
            50

        timeX =
            16

        timeY =
            Video.height - 32
    in
    slamBackground p wi
        |> drawAnimatedBack p wi
        |> drawLF p wi
        |> draw statsX statsY (p.get "WIOSTK")
        |> drawPercent p (Video.width - statsX) statsY (count 0 wi.cntKills)
        |> draw statsX (statsY + lh) (p.get "WIOSTI")
        |> drawPercent p (Video.width - statsX) (statsY + lh) (count 0 wi.cntItems)
        |> draw statsX (statsY + 2 * lh) (p.get "WISCRT2")
        |> drawPercent p (Video.width - statsX) (statsY + 2 * lh) (count 0 wi.cntSecret)
        |> draw timeX timeY (p.get "WITIME")
        |> drawTime p (Video.width // 2 - timeX) timeY wi.cntTime
        |> (if wi.wbs.epsd < 3 then
                draw (Video.width // 2 + timeX) timeY (p.get "WIPAR")
                    >> drawTime p (Video.width - timeX) timeY wi.cntPar

            else
                identity
           )


{-| WI\_drawNetgameStats: a row per player, their colour patch with a face on their own.
-}
drawNetgameStats : Pics -> Intermission -> Framebuffer
drawNetgameStats p wi =
    let
        star =
            p.get "STFST01"

        kills =
            p.get "WIOSTK"

        statsX =
            32
                + widthOf star
                // 2
                + (if wi.dofrags == 0 then
                    32

                   else
                    0
                  )

        statsY =
            50

        spacingX =
            64

        pwidth =
            widthOf (p.get "WIPCNT")

        titles fb =
            fb
                |> draw (statsX + spacingX - widthOf kills) statsY kills
                |> drawTitle (statsX + 2 * spacingX) (p.get "WIOSTI")
                |> drawTitle (statsX + 3 * spacingX) (p.get "WIOSTS")
                |> (if wi.dofrags /= 0 then
                        drawTitle (statsX + 4 * spacingX) (p.get "WIFRGS")

                    else
                        identity
                   )

        drawTitle right pt =
            draw (right - widthOf pt) statsY pt

        row i ( y, fb ) =
            if not (inGame wi i) then
                ( y, fb )

            else
                let
                    face =
                        p.get ("STPB" ++ String.fromInt i)

                    x =
                        statsX
                in
                ( y + spacingY
                , fb
                    |> draw (x - widthOf face) y face
                    |> (if i == wi.wbs.pnum then
                            draw (x - widthOf face) y star

                        else
                            identity
                       )
                    |> drawPercent p (x + spacingX - pwidth) (y + 10) (count i wi.cntKills)
                    |> drawPercent p (x + 2 * spacingX - pwidth) (y + 10) (count i wi.cntItems)
                    |> drawPercent p (x + 3 * spacingX - pwidth) (y + 10) (count i wi.cntSecret)
                    |> (if wi.dofrags /= 0 then
                            drawNum p (x + 4 * spacingX) (y + 10) (count i wi.cntFrags) -1 >> Tuple.second

                        else
                            identity
                       )
                )
    in
    slamBackground p wi
        |> drawAnimatedBack p wi
        |> drawLF p wi
        |> titles
        |> (\fb -> List.foldl row ( statsY + heightOf kills, fb ) (List.range 0 (Defs.maxPlayers - 1)))
        |> Tuple.second


{-| WI\_drawDeathmatchStats: who killed whom, and each player's total.
-}
drawDeathmatchStats : Pics -> Intermission -> Framebuffer
drawDeathmatchStats p wi =
    let
        matrixX =
            42

        matrixY =
            68

        spacingX =
            40

        totalsX =
            269

        total =
            p.get "WIMSTT"

        w =
            widthOf (p.num 0)

        players =
            List.range 0 (Defs.maxPlayers - 1)

        heads fb =
            List.foldl
                (\i acc ->
                    let
                        x =
                            matrixX + spacingX + i * spacingX

                        y =
                            matrixY + i * spacingY

                        face =
                            p.get ("STPB" ++ String.fromInt i)

                        half =
                            widthOf face // 2
                    in
                    if inGame wi i then
                        acc
                            |> draw (x - half) (matrixY - spacingY) face
                            |> draw (matrixX - half) y face
                            |> (if i == wi.wbs.pnum then
                                    draw (x - half) (matrixY - spacingY) (p.get "STFDEAD0")
                                        >> draw (matrixX - half) y (p.get "STFST01")

                                else
                                    identity
                               )

                    else
                        acc
                )
                fb
                players

        frags i j =
            Array.get i wi.dmFrags |> Maybe.map (count j) |> Maybe.withDefault 0

        stats fb =
            List.foldl
                (\i acc ->
                    let
                        y =
                            matrixY + 10 + i * spacingY
                    in
                    if inGame wi i then
                        List.foldl
                            (\j a ->
                                if inGame wi j then
                                    drawNum p (matrixX + spacingX + j * spacingX + w) y (frags i j) 2 a |> Tuple.second

                                else
                                    a
                            )
                            acc
                            players
                            |> drawNum p (totalsX + w) y (count i wi.dmTotals) 2
                            |> Tuple.second

                    else
                        acc
                )
                fb
                players
    in
    slamBackground p wi
        |> drawAnimatedBack p wi
        |> drawLF p wi
        |> draw (totalsX - widthOf total // 2) (matrixY - spacingY + 10) total
        |> draw 10 100 (p.get "WIKILRS")
        |> draw 5 50 (p.get "WIVCTMS")
        |> heads
        |> stats
