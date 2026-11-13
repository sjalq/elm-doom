module Doom.Hud.Messages exposing (Model, init, start, forceNextMessage, ticker, responder, erase, drawer)

{-| The heads-up messages (hu\_stuff.c). Stub with doom-hud's announced API.

@docs Model, init, start, forceNextMessage, ticker, responder, erase, drawer

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


{-| HU\_Init.
-}
init : Wad -> Model
init _ =
    Debug.todo "Doom.Hud.Messages.init"


{-| HU\_Start.
-}
start : Env -> World -> Model -> Model
start _ _ _ =
    Debug.todo "Doom.Hud.Messages.start"


{-| message\_dontfuckwithme.
-}
forceNextMessage : Model -> Model
forceNextMessage _ =
    Debug.todo "Doom.Hud.Messages.forceNextMessage"


{-| HU\_Ticker.
-}
ticker : { showMessages : Bool } -> World -> Int -> Model -> ( Model, World )
ticker _ _ _ _ =
    Debug.todo "Doom.Hud.Messages.ticker"


{-| HU\_Responder.
-}
responder : Event -> Model -> Maybe Model
responder _ _ =
    Debug.todo "Doom.Hud.Messages.responder"


{-| HU\_Erase: back screen, then screens[0].
-}
erase : { automap : Bool, viewwindowx : Int, viewwindowy : Int, viewwidth : Int, viewheight : Int } -> Framebuffer -> Model -> Framebuffer -> ( Model, Framebuffer )
erase _ _ _ _ =
    Debug.todo "Doom.Hud.Messages.erase"


{-| HU\_Drawer.
-}
drawer : Env -> Bool -> Model -> Framebuffer -> Framebuffer
drawer _ _ _ _ =
    Debug.todo "Doom.Hud.Messages.drawer"
