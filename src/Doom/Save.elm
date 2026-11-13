module Doom.Save exposing (save, description, load)

{-| Saved games (p\_saveg.c). Stub with doom-saves' announced API.

@docs save, description, load

-}

import Bytes exposing (Bytes)
import Doom.Game exposing (Game)


{-| G\_DoSaveGame.
-}
save : String -> Game -> Bytes
save _ _ =
    Debug.todo "Doom.Save.save"


{-| The description a save starts with.
-}
description : Bytes -> Maybe String
description _ =
    Debug.todo "Doom.Save.description"


{-| G\_DoLoadGame onto the current game.
-}
load : Bytes -> Game -> Result String Game
load _ _ =
    Debug.todo "Doom.Save.load"
