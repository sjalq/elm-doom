module EnemyTests exposing (suite)

{-| p\_enemy.c's direction tables, checked against the C source's arrays.
-}

import Doom.Play.Enemy as Enemy
import Expect
import Test exposing (Test, describe, test)


directions : List Int
directions =
    List.range 0 7


suite : Test
suite =
    describe "Doom.Play.Enemy direction tables"
        [ test "opposite[] matches the C table" <|
            \_ ->
                List.map Enemy.opposite (List.range 0 8)
                    |> Expect.equal [ 4, 5, 6, 7, 0, 1, 2, 3, 8 ]
        , test "opposite is its own inverse" <|
            \_ ->
                List.map (Enemy.opposite >> Enemy.opposite) directions
                    |> Expect.equal directions
        , test "diags[] matches the C table" <|
            \_ ->
                List.map Enemy.diags [ 0, 1, 2, 3 ]
                    |> Expect.equal [ 3, 1, 5, 7 ]
        , test "xspeed[] matches the C table" <|
            \_ ->
                List.map Enemy.xspeed directions
                    |> Expect.equal [ 65536, 47000, 0, -47000, -65536, -47000, 0, 47000 ]
        , test "yspeed[] matches the C table" <|
            \_ ->
                List.map Enemy.yspeed directions
                    |> Expect.equal [ 0, 47000, 65536, 47000, 0, -47000, -65536, -47000 ]
        , test "no direction is DI_NODIR (8)" <|
            \_ -> Expect.equal 8 Enemy.diNoDir
        ]
