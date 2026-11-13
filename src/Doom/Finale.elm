module Doom.Finale exposing (Model, start, ticker, responder, drawer)

{-| The end of an episode (f\_finale.c): the text screen, the art screens, the bunny
scroll, and Doom II's cast call.

@docs Model, start, ticker, responder, drawer

-}

import Doom.Defs exposing (GameMode)
import Doom.Event exposing (Event)
import Doom.Sound as Sound
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)


{-| The finale's state.
-}
type Model
    = Model


{-| F\_StartFinale gamemode episode map.
-}
start : GameMode -> Int -> Int -> Model
start a0 a1 a2 =
    Debug.todo "Doom.Finale.start"


{-| F\_Ticker (the cast call plays sounds).
-}
ticker : Model -> ( Model, List Sound.Event )
ticker a0 =
    Debug.todo "Doom.Finale.ticker"


{-| F\_Responder.
-}
responder : Event -> Model -> Maybe ( Model, List Sound.Event )
responder a0 a1 =
    Debug.todo "Doom.Finale.responder"


{-| F\_Drawer.
-}
drawer : Wad -> Model -> Framebuffer -> Framebuffer
drawer a0 a1 a2 =
    Debug.todo "Doom.Finale.drawer"
