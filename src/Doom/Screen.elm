module Doom.Screen exposing (Screen)

{-| Everything to show for one frame, as D\_Display leaves it: the 320 × 200 screen of
palette indices (`screens[0]`), and which palette and gamma correction to show it
with. A host only has to turn indices into colours (PLAYPAL's palette `palette`,
through v\_video.c's gammatable row `gamma`) and scale the picture to its screen (the
original's pixels are 1.2 times taller than wide on a 4:3 display).

@docs Screen

-}

import Doom.Video exposing (Framebuffer)


{-| A frame: `palette` 0 to 13 (normal, red when hurt, gold when picking things up,
green in a radiation suit), `gamma` 0 to 4.
-}
type alias Screen =
    { pixels : Framebuffer
    , palette : Int
    , gamma : Int
    }
