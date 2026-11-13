module Doom.Wipe exposing (Melt, start, step)

{-| The screen melt between game states (f\_wipe.c's wipe\_Melt: wipe\_initMelt,
wipe\_doMelt, wipe\_exitMelt).

The screen is cut into 160 columns two pixels wide. Each column waits a few steps
(how many is random, but close to its neighbour's), then slides down, faster and
faster up to 8 rows a step, uncovering the new screen above it.

    ( melt, rng ) =
        Wipe.start world.rng oldScreen newScreen

    ( screen, melt2, done ) =
        Wipe.step 2 oldScreen melt

D\_Display starts with the old screen on display and calls `step` once per frame;
the first frame's step is 2 tics long, the others 1 (see `Doom.Shell`).

@docs Melt, start, step

-}

import Array exposing (Array)
import Bytes.Encode as E
import Doom.Random as Random exposing (Rng)
import Doom.Video as Video exposing (Framebuffer)


{-| A melt under way: the screens it goes from and to, and how far down each column
is (below 0: not moving yet).
-}
type Melt
    = Melt
        { from : Framebuffer
        , to : Framebuffer
        , ys : Array Int
        }


{-| wipe\_initMelt: the columns' starting heights, from M\_Random. The C source draws
a number for each of the 320 pixel columns, though only the first 160 entries are
used by the melt (it moves pairs of pixels), so `start` draws 320 numbers too.
-}
start : Rng -> Framebuffer -> Framebuffer -> ( Melt, Rng )
start rng from to =
    let
        ( first, rng1 ) =
            Random.mRandom rng

        y0 =
            -(modBy 16 first)

        ( ysRev, rngN ) =
            List.foldl
                (\_ ( acc, r ) ->
                    let
                        ( n, r2 ) =
                            Random.mRandom r

                        prev =
                            List.head acc |> Maybe.withDefault 0

                        y =
                            prev + (modBy 3 n - 1)

                        clamped =
                            if y > 0 then
                                0

                            else if y == -16 then
                                -15

                            else
                                y
                    in
                    ( clamped :: acc, r2 )
                )
                ( [ y0 ], rng1 )
                (List.range 1 (Video.width - 1))
    in
    ( Melt
        { from = from
        , to = to
        , ys = Array.fromList (List.reverse ysRev) |> Array.slice 0 columns
        }
    , rngN
    )


columns : Int
columns =
    Video.width // 2


{-| wipe\_doMelt for `ticks` tics, on the screen as it is now (whatever has been drawn
over it since the last step, such as the menu, stays where the melt doesn't touch).
True when the melt had nothing left to move: the step that finds every column at the
bottom changes nothing and ends the wipe.
-}
step : Int -> Framebuffer -> Melt -> ( Framebuffer, Melt, Bool )
step ticks screen melt =
    if ticks <= 0 then
        ( screen, melt, True )

    else
        let
            ( s1, m1, done1 ) =
                stepOnce screen melt

            ( s2, m2, done2 ) =
                step (ticks - 1) s1 m1
        in
        ( s2, m2, done1 && done2 )


stepOnce : Framebuffer -> Melt -> ( Framebuffer, Melt, Bool )
stepOnce screen (Melt m) =
    let
        h =
            Video.height

        moved =
            Array.map
                (\y ->
                    if y < 0 then
                        { old = y, new = y + 1, copies = False }

                    else if y < h then
                        let
                            dy0 =
                                if y < 16 then
                                    y + 1

                                else
                                    8

                            dy =
                                if y + dy0 >= h then
                                    h - y

                                else
                                    dy0
                        in
                        { old = y, new = y + dy, copies = True }

                    else
                        { old = y, new = y, copies = False }
                )
                m.ys

        done =
            Array.foldl (\c acc -> acc && c.old >= h) True moved

        anyCopies =
            Array.foldl (\c acc -> acc || c.copies) False moved

        newScreen =
            if anyCopies then
                redraw screen m.from m.to moved

            else
                screen
    in
    ( newScreen, Melt { m | ys = Array.map .new moved }, done )


redraw : Framebuffer -> Framebuffer -> Framebuffer -> Array { old : Int, new : Int, copies : Bool } -> Framebuffer
redraw screen from to moved =
    let
        cols =
            Array.toList moved

        row y =
            List.indexedMap
                (\c col ->
                    let
                        x =
                            2 * c
                    in
                    if col.copies && y >= col.old then
                        if y < col.new then
                            [ Video.pixel x y to, Video.pixel (x + 1) y to ]

                        else
                            [ Video.pixel x (y - col.new) from, Video.pixel (x + 1) (y - col.new) from ]

                    else
                        [ Video.pixel x y screen, Video.pixel (x + 1) y screen ]
                )
                cols
                |> List.concat
    in
    List.range 0 (Video.height - 1)
        |> List.concatMap row
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> Video.fromBytes
        |> Maybe.withDefault screen
