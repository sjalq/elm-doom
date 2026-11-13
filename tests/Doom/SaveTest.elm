module Doom.SaveTest exposing (suite)

{-| The description at the start of a save file. Saving and loading whole games is
checked against linuxdoom-1.10 by doom/tools/savecheck and the oracle's
-savecheck.
-}

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Doom.Save as Save
import Expect
import Test exposing (Test, describe, test)


bytes : List Int -> Bytes
bytes list =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 list))


header : String -> List Int
header desc =
    let
        codes =
            String.toList desc |> List.map Char.toCode
    in
    codes ++ List.repeat (24 - List.length codes) 0 ++ (String.toList Save.version |> List.map Char.toCode)


suite : Test
suite =
    describe "Doom.Save.description"
        [ test "reads up to the first zero byte" <|
            \_ ->
                Save.description (bytes (header "E1M1 lift"))
                    |> Expect.equal (Just "E1M1 lift")
        , test "a full 24 characters has no terminator" <|
            \_ ->
                Save.description (bytes (header "abcdefghijklmnopqrstuvwx"))
                    |> Expect.equal (Just "abcdefghijklmnopqrstuvwx")
        , test "a file shorter than a description has none" <|
            \_ ->
                Save.description (bytes [ 65, 66 ])
                    |> Expect.equal Nothing
        ]
