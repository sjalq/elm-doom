module Doom.Hud.StatusBar exposing (Model, init, start, ticker, responder, drawer, palette)

{-| The status bar (st\_stuff.c, st\_lib.c) and the cheats it handles (m\_cheat.c):
ammo, health, arms, the face, armour, keys, and in deathmatch frags.

@docs Model, init, start, ticker, responder, drawer, palette

-}

import Doom.Event exposing (Event)
import Doom.Play.Env exposing (Env)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)
import Doom.World exposing (World)


{-| The status bar's state (the face's animation, the widgets' last values, the cheat
sequences typed so far).
-}
type Model
    = Model


{-| ST\_Init: load its patches.
-}
init : Wad -> Model
init a0 =
    Debug.todo "Doom.Hud.StatusBar.init"


{-| ST\_Start for the console player.
-}
start : Env -> World -> Int -> Model -> Model
start a0 a1 a2 a3 =
    Debug.todo "Doom.Hud.StatusBar.start"


{-| ST\_Ticker.
-}
ticker : Env -> World -> Int -> Model -> Model
ticker a0 a1 a2 a3 =
    Debug.todo "Doom.Hud.StatusBar.ticker"


{-| ST\_Responder: cheats (changing the world: iddqd, idkfa...) and the automap's messages; Nothing if it doesn't take the event.
-}
responder : Event -> Env -> World -> Int -> Model -> Maybe ( Model, World )
responder a0 a1 a2 a3 a4 =
    Debug.todo "Doom.Hud.StatusBar.responder"


{-| ST\_Drawer fullscreen: draws the bar unless fullscreen (screenblocks 11).
-}
drawer : Env -> World -> Int -> Bool -> Model -> Framebuffer -> Framebuffer
drawer a0 a1 a2 a3 a4 a5 =
    Debug.todo "Doom.Hud.StatusBar.drawer"


{-| ST\_doPaletteStuff: the PLAYPAL palette for the player now.
-}
palette : World -> Int -> Int
palette a0 a1 =
    Debug.todo "Doom.Hud.StatusBar.palette"
