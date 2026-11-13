module Doom.Intermission.Draw exposing (drawer)

{-| The intermission screen (wi\_stuff.c's WI\_Drawer and its pieces).

@docs drawer

-}

import Doom.Intermission exposing (Intermission)
import Doom.Video exposing (Framebuffer)
import Doom.Wad exposing (Wad)


{-| WI\_Drawer.
-}
drawer : Wad -> Intermission -> Framebuffer -> Framebuffer
drawer a0 a1 a2 =
    Debug.todo "Doom.Intermission.Draw.drawer"
