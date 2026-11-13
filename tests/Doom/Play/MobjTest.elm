module Doom.Play.MobjTest exposing (suite)

{-| The pure helpers of p\_mobj.c's spawning: finding a thing's type by its editor
number, and a map thing's facing.
-}

import Doom.Angle as Angle
import Doom.Info.MobjType as MobjType
import Doom.Play.Mobj as PMobj
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Doom.Play.Mobj"
        [ test "editor numbers find their types" <|
            \() ->
                List.map PMobj.typeForDoomednum [ 3004, 2001, 9, 2035, 12345 ]
                    |> Expect.equal
                        [ Just MobjType.Possessed
                        , Just MobjType.Shotgun
                        , Just MobjType.Shotguy
                        , Just MobjType.Barrel
                        , Nothing
                        ]
        , test "facings round down to 45 degrees and wrap at a full turn" <|
            \() ->
                List.map (\a -> PMobj.spawnAngle { x = 0, y = 0, angle = a, type_ = 1, options = 0 }) [ 0, 44, 90, 359, 360, 405 ]
                    |> Expect.equal [ 0, 0, Angle.ang90, Angle.ang45 * 7, 0, Angle.ang45 ]
        ]
