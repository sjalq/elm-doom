module Doom.Automap exposing (Model, init, active, start, stop, deactivate, setSingleDemo, stopOnDeath, ticker, responder, drawer)

{-| The automap (am\_map.c). Stub with doom-automap's announced API.

@docs Model, init, active, start, stop, deactivate, setSingleDemo, stopOnDeath, ticker, responder, drawer

-}

import Doom.Event exposing (Event)
import Doom.Play.Env exposing (Env)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The automap's state.
-}
type Model
    = Model


{-| Load the mark number patches.
-}
init : Wad -> Model
init _ =
    Debug.todo "Doom.Automap.init"


{-| automapactive.
-}
active : Model -> Bool
active _ =
    Debug.todo "Doom.Automap.active"


{-| AM\_Start.
-}
start : Env -> World -> Int -> Model -> Model
start _ _ _ _ =
    Debug.todo "Doom.Automap.start"


{-| AM\_Stop.
-}
stop : Model -> Model
stop _ =
    Debug.todo "Doom.Automap.stop"


{-| automapactive = false.
-}
deactivate : Model -> Model
deactivate _ =
    Debug.todo "Doom.Automap.deactivate"


{-| singledemo.
-}
setSingleDemo : Bool -> Model -> Model
setSingleDemo _ _ =
    Debug.todo "Doom.Automap.setSingleDemo"


{-| AM\_Ticker.
-}
ticker : Env -> World -> Int -> Model -> Model
ticker _ _ _ _ =
    Debug.todo "Doom.Automap.ticker"


{-| AM\_Responder: the model, the world, whether it ate the event.
-}
responder : Event -> Env -> World -> Int -> Model -> ( Model, World, Bool )
responder _ _ _ _ _ =
    Debug.todo "Doom.Automap.responder"


{-| AM\_Drawer.
-}
drawer : Env -> World -> Int -> Model -> Framebuffer -> Framebuffer
drawer _ _ _ _ _ =
    Debug.todo "Doom.Automap.drawer"


{-| AM\_Stop when the console player died during the tic.
-}
stopOnDeath : World -> World -> Int -> Model -> Model
stopOnDeath _ _ _ _ =
    Debug.todo "Doom.Automap.stopOnDeath"
