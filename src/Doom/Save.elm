module Doom.Save exposing (save, description, load)

{-| Saved games (p\_saveg.c, G\_DoSaveGame, G\_DoLoadGame).

@docs save, description, load

-}

import Bytes exposing (Bytes)
import Doom.Data exposing (Data)
import Doom.Game exposing (Game)
import Doom.Wad exposing (Wad)


{-| G\_DoSaveGame: the game (in a level) with its description.
-}
save : String -> Game -> Bytes
save a0 a1 =
    Debug.todo "Doom.Save.save"


{-| The description a save starts with.
-}
description : Bytes -> Maybe String
description a0 =
    Debug.todo "Doom.Save.description"


{-| G\_DoLoadGame.
-}
load : Wad -> Data -> Bytes -> Result String Game
load a0 a1 a2 =
    Debug.todo "Doom.Save.load"
