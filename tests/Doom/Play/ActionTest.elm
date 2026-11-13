module Doom.Play.ActionTest exposing (suite)

{-| Every state's action goes to the right kind of function: the weapon sprites'
states (the player's gun, drawn on the screen) to weapon actions, every other state
(things in the world) to thing actions. A misrouted action would be called with the
wrong arguments in the C source, and is silently skipped here, so this catches it.
-}

import Array
import Doom.Info.Sprite as Sprite
import Doom.Info.State as State
import Doom.Play.Action as Action exposing (ActionKind(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Doom.Play.Action"
        [ test "weapon states have weapon actions, all others thing actions" <|
            \() ->
                State.all
                    |> Array.toList
                    |> List.filterMap
                        (\s ->
                            let
                                d =
                                    State.def s
                            in
                            Maybe.map
                                (\a ->
                                    let
                                        expected =
                                            if isWeaponSprite d.sprite then
                                                WeaponAction

                                            else
                                                MobjAction
                                    in
                                    ( State.index s, expected, Action.kind a )
                                )
                                d.action
                        )
                    |> List.filter (\( _, expected, actual ) -> expected /= actual)
                    |> Expect.equal []
        , test "the weapon sprites are SHTG to BFGF" <|
            \() ->
                List.filter isWeaponSprite [ Sprite.Troo, Sprite.Shtg, Sprite.Pung, Sprite.Bfgf, Sprite.Blud ]
                    |> Expect.equal [ Sprite.Shtg, Sprite.Pung, Sprite.Bfgf ]
        , test "A_BFGSpray is a thing action, A_Light0 a weapon action" <|
            \() ->
                Expect.equal
                    [ Maybe.map Action.kind (State.def State.Bfgland3).action
                    , Maybe.map Action.kind (State.def State.Lightdone).action
                    ]
                    [ Just MobjAction, Just WeaponAction ]
        ]


{-| The player's weapon sprites: sprnames[] from "SHTG" to "BFGF" (after "TROO").
-}
isWeaponSprite : Sprite.Sprite -> Bool
isWeaponSprite sprite =
    Sprite.index sprite >= Sprite.index Sprite.Shtg && Sprite.index sprite <= Sprite.index Sprite.Bfgf
