module Doom.Data exposing
    ( Data, Texture, TexturePatch, Anim
    , textureNumForName, checkTextureNumForName, flatNumForName, textureHeight
    )

{-| The WAD's wall textures and flats, by number (r\_data.c's tables): the game refers
to them by these numbers, and the renderer draws them.

  - `textures`: TEXTURE1 then TEXTURE2, in order; a texture's number is its position.
    Each is put together from patches (PNAMES).
  - `flats`: the lumps between F\_START and F\_END, in order; a flat's number is its
    position there.

@docs Data, Texture, TexturePatch, Anim
@docs textureNumForName, checkTextureNumForName, flatNumForName, textureHeight

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Doom.Defs exposing (GameMode)
import Doom.Fixed as Fixed exposing (Fixed)


{-| What the WAD gives the game, besides maps. `skyFlatNum` is F\_SKY1's number.
`switchList` is p\_switch.c's `switchlist[]` after P\_InitSwitchList: texture numbers
in pairs (off, on), for the switches this game mode has. `anims` is P\_InitPicAnims'
list.
-}
type alias Data =
    { gameMode : GameMode
    , textures : Array Texture
    , textureNums : Dict String Int
    , flats : Array String
    , flatNums : Dict String Int
    , skyFlatNum : Int
    , switchList : Array Int
    , anims : List Anim
    }


{-| anim\_t (p\_spec.c): an animated texture or flat, cycling through `numpics`
numbers from `basepic`, `speed` tics each. P\_InitPicAnims builds the list from the
animations whose first and last frames the WAD has.
-}
type alias Anim =
    { istexture : Bool
    , picnum : Int
    , basepic : Int
    , numpics : Int
    , speed : Int
    }


{-| A wall texture: its name, size in pixels and the patches drawn into it.
-}
type alias Texture =
    { name : String
    , width : Int
    , height : Int
    , patches : List TexturePatch
    }


{-| One patch of a texture: where it goes, and the patch lump's name (from PNAMES).
-}
type alias TexturePatch =
    { originX : Int
    , originY : Int
    , patch : String
    }


{-| R\_CheckTextureNumForName: the texture's number, 0 for "-" (no texture), or
Nothing if there is no such texture. Names are compared in upper case, up to 8
characters.
-}
checkTextureNumForName : String -> Data -> Maybe Int
checkTextureNumForName name data =
    if String.startsWith "-" name then
        Just 0

    else
        Dict.get (normalize name) data.textureNums


{-| R\_TextureNumForName. The C source stops the game for a name that isn't there;
this gives 0 (no texture) instead.
-}
textureNumForName : String -> Data -> Int
textureNumForName name data =
    checkTextureNumForName name data |> Maybe.withDefault 0


{-| R\_FlatNumForName (0 if there is no such flat).
-}
flatNumForName : String -> Data -> Int
flatNumForName name data =
    Dict.get (normalize name) data.flatNums |> Maybe.withDefault 0


{-| `textureheight[n]`: the texture's height, fixed-point.
-}
textureHeight : Int -> Data -> Fixed
textureHeight n data =
    Array.get n data.textures
        |> Maybe.map (\t -> t.height * Fixed.fracunit)
        |> Maybe.withDefault 0


normalize : String -> String
normalize name =
    String.toUpper (String.left 8 name)
