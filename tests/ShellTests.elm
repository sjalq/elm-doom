module ShellTests exposing (suite)

import Array
import Doom.Event as Event exposing (Event(..))
import Doom.Input as Input
import Doom.Random as Random
import Doom.Ticcmd as Ticcmd
import Doom.Video as Video
import Doom.Wipe as Wipe
import Expect
import Fuzz
import Test exposing (Test, describe, fuzzWith, test)


suite : Test
suite =
    describe "the game shell"
        [ describe "Doom.Wipe"
            [ fuzzWith { runs = 3, distribution = Test.noDistribution } (Fuzz.intRange 0 255) "the melt ends on the new screen, 1 to 2 + 15 + 200 steps in" <|
                \index ->
                    let
                        from =
                            Video.blank 3

                        to =
                            Video.fill 0 0 320 100 7 (Video.blank 9)

                        ( melt, _ ) =
                            Wipe.start { prndindex = 0, rndindex = index } from to

                        run n tics screen m =
                            let
                                ( s2, m2, done ) =
                                    Wipe.step tics screen m
                            in
                            if done || n > 400 then
                                ( n, s2 )

                            else
                                run (n + 1) 1 s2 m2

                        ( steps, final ) =
                            run 1 2 from melt
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Video.toBytes final) (Video.toBytes to)
                        , \_ -> Expect.atMost 217 steps
                        ]
                        ()
            , test "wipe_initMelt draws 320 numbers from M_Random" <|
                \_ ->
                    let
                        ( _, rng ) =
                            Wipe.start Random.init (Video.blank 0) (Video.blank 1)
                    in
                    Expect.equal rng.rndindex 64
            , test "the first step moves only the columns that start at 0 or -1" <|
                \_ ->
                    let
                        from =
                            Video.blank 3

                        ( melt, _ ) =
                            Wipe.start Random.init from (Video.blank 9)

                        ( screen, _, done ) =
                            Wipe.step 2 from melt

                        changed =
                            List.range 0 159 |> List.filter (\c -> Video.pixel (2 * c) 0 screen == 9)
                    in
                    Expect.all [ \_ -> Expect.equal done False, \_ -> Expect.notEqual changed [] ] ()
            ]
        , describe "Doom.Input.build"
            [ test "arrows and run" <|
                \_ ->
                    let
                        b =
                            Input.defaultBindings

                        ( c, _ ) =
                            Input.noControls
                                |> Input.respond 5 (KeyDown b.up)
                                |> Tuple.first
                                |> Input.respond 5 (KeyDown Event.keyRShift)
                                |> Tuple.first
                                |> Input.build b
                    in
                    Expect.equal c.forwardmove 0x32
            , test "mouse movement is scaled by the sensitivity and used once" <|
                \_ ->
                    let
                        b =
                            Input.defaultBindings

                        ( c1, controls ) =
                            Input.noControls
                                |> Input.respond 5 (Mouse 0 10 0)
                                |> Tuple.first
                                |> Input.build b

                        ( c2, _ ) =
                            Input.build b controls
                    in
                    Expect.equal ( c1.angleturn, c2.angleturn ) ( -80, 0 )
            , test "a weapon key and the fire button" <|
                \_ ->
                    let
                        b =
                            Input.defaultBindings

                        ( c, _ ) =
                            Input.noControls
                                |> Input.respond 5 (KeyDown (Char.toCode '3'))
                                |> Tuple.first
                                |> Input.respond 5 (Mouse 1 0 0)
                                |> Tuple.first
                                |> Input.build b
                    in
                    Expect.equal c.buttons (Ticcmd.btAttack + Ticcmd.btChange + 2 * 8)
            , test "a double click of the forward button is a use" <|
                \_ ->
                    let
                        b =
                            Input.defaultBindings

                        press buttons controls =
                            Input.respond 5 (Mouse buttons 0 0) controls |> Tuple.first

                        tics n controls =
                            List.foldl (\_ ( uses, c ) -> Input.build b c |> Tuple.mapFirst (\cmd -> uses ++ [ cmd.buttons ])) ( [], controls ) (List.range 1 n)
                                |> (\( uses, c ) -> ( uses, c ))

                        ( u1, c1 ) =
                            tics 3 Input.noControls

                        ( u2, c2 ) =
                            tics 3 (press 4 c1)

                        ( u3, c3 ) =
                            tics 3 (press 0 c2)

                        ( u4, _ ) =
                            tics 1 (press 4 c3)
                    in
                    Expect.equal (List.any (\bt -> bt == Ticcmd.btUse) (u1 ++ u2 ++ u3 ++ u4)) True
            , test "key up isn't eaten, key down is" <|
                \_ ->
                    Expect.equal
                        ( Tuple.second (Input.respond 5 (KeyDown 32) Input.noControls)
                        , Tuple.second (Input.respond 5 (KeyUp 32) Input.noControls)
                        )
                        ( True, False )
            , test "keys and mouse are cleared, held turning isn't" <|
                \_ ->
                    let
                        c =
                            Input.respond 5 (KeyDown 32) Input.noControls |> Tuple.first

                        cleared =
                            Input.clear { c | turnheld = 4 }
                    in
                    Expect.equal ( Array.get 32 cleared.keys, cleared.turnheld ) ( Just False, 4 )
            ]
        ]
