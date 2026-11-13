module Doom.Automap exposing (Model, init, active, start, stop, ticker, responder, drawer)

{-| The automap (am\_map.c).

@docs Model, init, active, start, stop, ticker, responder, drawer

-}

import Doom.Event exposing (Event)
import Doom.Play.Env exposing (Env)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The automap's state: whether it's showing, where it looks, the zoom, marks.
-}
type Model
    = Model


{-| Load the mark number patches.
-}
init : Wad -> Model
init a0 =
    Debug.todo "Doom.Automap.init"


{-| automapactive.
-}
active : Model -> Bool
active a0 =
    Debug.todo "Doom.Automap.active"


{-| AM\_Start.
-}
start : Env -> World -> Int -> Model -> Model
start a0 a1 a2 a3 =
    Debug.todo "Doom.Automap.start"


{-| AM\_Stop.
-}
stop : Model -> Model
stop a0 =
    Debug.todo "Doom.Automap.stop"


{-| AM\_Ticker.
-}
ticker : Env -> World -> Int -> Model -> Model
ticker a0 a1 a2 a3 =
    Debug.todo "Doom.Automap.ticker"


{-| AM\_Responder (the player's message may change, so the world comes back).
-}
responder : Event -> Env -> World -> Int -> Model -> Maybe ( Model, World )
responder a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Automap.responder"


{-| AM\_Drawer.
-}
drawer : Env -> World -> Int -> Model -> Framebuffer -> Framebuffer
drawer a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Automap.drawer"
