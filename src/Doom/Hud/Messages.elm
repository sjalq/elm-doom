module Doom.Hud.Messages exposing (Model, init, start, ticker, responder, drawer)

{-| The heads-up messages (hu\_stuff.c, hu\_lib.c): the player's message line at the
top, and the level's name on the automap. Chat isn't translated.

@docs Model, init, start, ticker, responder, drawer

-}

import Doom.Event exposing (Event)
import Doom.Play.Env exposing (Env)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The message line's state.
-}
type Model
    = Model


{-| HU\_Init: load the font.
-}
init : Wad -> Model
init a0 =
    Debug.todo "Doom.Hud.Messages.init"


{-| HU\_Start.
-}
start : Env -> World -> Model -> Model
start a0 a1 a2 =
    Debug.todo "Doom.Hud.Messages.start"


{-| HU\_Ticker showMessages: picks up and clears the player's message.
-}
ticker : Bool -> World -> Int -> Model -> ( Model, World )
ticker a0 a1 a2 a3 =
    Debug.todo "Doom.Hud.Messages.ticker"


{-| HU\_Responder: the message refresh key.
-}
responder : Event -> Model -> Maybe Model
responder a0 a1 =
    Debug.todo "Doom.Hud.Messages.responder"


{-| HU\_Drawer automapActive.
-}
drawer : Env -> Bool -> Model -> Framebuffer -> Framebuffer
drawer a0 a1 a2 a3 =
    Debug.todo "Doom.Hud.Messages.drawer"
