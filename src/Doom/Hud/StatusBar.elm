module Doom.Hud.StatusBar exposing (Model, Request(..), init, start, ticker, automapEntered, responder, drawer, palette)

{-| The status bar (st\_stuff.c). Stub with doom-hud's announced API.

@docs Model, Request, init, start, ticker, automapEntered, responder, drawer, palette

-}

import Doom.Defs exposing (Skill)
import Doom.Event exposing (Event)
import Doom.Play.Env exposing (Env)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The status bar's state.
-}
type Model
    = Model


{-| What a cheat asks of the game.
-}
type Request
    = Warp Skill Int Int
    | ChangeMusic String
    | BadMusic Int


{-| ST\_Init.
-}
init : Wad -> Model
init _ =
    Debug.todo "Doom.Hud.StatusBar.init"


{-| ST\_Start.
-}
start : Env -> World -> Int -> Model -> Model
start _ _ _ _ =
    Debug.todo "Doom.Hud.StatusBar.start"


{-| ST\_Ticker.
-}
ticker : Env -> World -> Int -> Model -> ( Model, World )
ticker _ _ _ _ =
    Debug.todo "Doom.Hud.StatusBar.ticker"


{-| AM\_MSGENTERED.
-}
automapEntered : Model -> Model
automapEntered _ =
    Debug.todo "Doom.Hud.StatusBar.automapEntered"


{-| ST\_Responder.
-}
responder : Event -> Env -> World -> Int -> Model -> { model : Model, world : World, request : Maybe Request }
responder _ _ _ _ _ =
    Debug.todo "Doom.Hud.StatusBar.responder"


{-| ST\_Drawer.
-}
drawer : Env -> World -> Int -> { fullscreen : Bool, refresh : Bool, automap : Bool } -> Model -> Framebuffer -> ( Model, Framebuffer )
drawer _ _ _ _ _ _ =
    Debug.todo "Doom.Hud.StatusBar.drawer"


{-| ST\_doPaletteStuff's palette.
-}
palette : World -> Int -> Int
palette _ _ =
    Debug.todo "Doom.Hud.StatusBar.palette"
