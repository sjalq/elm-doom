module Doom.Data.Load exposing (fromWad, gameMode, readTextures)

{-| Building `Doom.Data` from a WAD: which game it is, the wall textures (r\_data.c's
R\_InitTextures), the flats (R\_InitFlats), the switches (p\_switch.c's
P\_InitSwitchList) and the animated textures and flats (p\_spec.c's P\_InitPicAnims).

    Wad.fromBytes bytes |> Result.andThen Load.fromWad

@docs fromWad, gameMode, readTextures

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Doom.Data as Data exposing (Anim, Data, Texture)
import Doom.Defs exposing (GameMode(..))
import Doom.Wad as Wad exposing (Wad)


{-| Everything `Data` holds, read from the WAD.
-}
fromWad : Wad -> Result String Data
fromWad wad =
    let
        mode =
            gameMode wad
    in
    readTextures wad
        |> Result.andThen
            (\textures ->
                flatRange wad
                    |> Result.andThen
                        (\( firstflat, flats ) ->
                            let
                                partial =
                                    { gameMode = mode
                                    , textures = textures
                                    , textureNums = textureNumbers textures
                                    , flats = flats
                                    , flatNums = flatNumbers firstflat wad
                                    , skyFlatNum = 0
                                    , switchList = Array.empty
                                    , anims = []
                                    }

                                withSky =
                                    { partial | skyFlatNum = Data.flatNumForName "F_SKY1" partial }
                            in
                            initPicAnims withSky
                                |> Result.map
                                    (\anims ->
                                        { withSky
                                            | switchList = initSwitchList withSky
                                            , anims = anims
                                        }
                                    )
                        )
            )


{-| Which game the WAD is, from the maps it has: Commercial (Doom II) if it has MAP01,
Retail (The Ultimate Doom) if it has E4M1, Registered if E3M1, Shareware otherwise.
-}
gameMode : Wad -> GameMode
gameMode wad =
    let
        has n =
            Wad.checkNumForName n wad /= Nothing
    in
    if has "MAP01" then
        Commercial

    else if has "E4M1" then
        Retail

    else if has "E3M1" then
        Registered

    else
        Shareware



-- TEXTURES


{-| R\_InitTextures: TEXTURE1's textures then TEXTURE2's (if there is one), each with
its patches' names looked up in PNAMES.
-}
readTextures : Wad -> Result String (Array Texture)
readTextures wad =
    case Wad.lump "PNAMES" wad |> Maybe.andThen (Decode.decode pnames) of
        Nothing ->
            Err "the WAD has no readable PNAMES lump"

        Just patchNames ->
            let
                read lumpName =
                    case Wad.lump lumpName wad of
                        Nothing ->
                            Ok []

                        Just bytes ->
                            textureLump patchNames bytes
                                |> Result.fromMaybe (lumpName ++ " is not a valid texture lump")
            in
            if Wad.checkNumForName "TEXTURE1" wad == Nothing then
                Err "the WAD has no TEXTURE1 lump"

            else
                Result.map2 (\t1 t2 -> Array.fromList (t1 ++ t2))
                    (read "TEXTURE1")
                    (read "TEXTURE2")


pnames : Decoder (Array String)
pnames =
    Decode.signedInt32 LE
        |> Decode.andThen (\n -> list n (Wad.name8 |> Decode.map String.toUpper))
        |> Decode.map Array.fromList


{-| A TEXTURE1/TEXTURE2 lump: a count, the offsets of the textures, then each texture
(maptexture\_t) at its offset.
-}
textureLump : Array String -> Bytes -> Maybe (List Texture)
textureLump patchNames bytes =
    Decode.decode (Decode.signedInt32 LE |> Decode.andThen (\n -> list n (Decode.signedInt32 LE))) bytes
        |> Maybe.andThen
            (\offsets ->
                offsets
                    |> List.map (\offset -> Decode.decode (skipThen offset (mapTexture patchNames)) bytes)
                    |> List.foldr (Maybe.map2 (::)) (Just [])
            )


{-| maptexture\_t: name[8], masked (int), width, height (shorts), columndirectory (4
bytes, unused), patchcount, then mappatch\_t's (originx, originy, patch, stepdir,
colormap: shorts).
-}
mapTexture : Array String -> Decoder Texture
mapTexture patchNames =
    Decode.map4 (\n _ w h -> ( n, w, h ))
        Wad.name8
        (Decode.signedInt32 LE)
        short
        short
        |> Decode.andThen
            (\( n, w, h ) ->
                Decode.map2 (\_ count -> count) (Decode.signedInt32 LE) short
                    |> Decode.andThen
                        (\count ->
                            list count
                                (Decode.map5
                                    (\ox oy p _ _ ->
                                        { originX = ox
                                        , originY = oy
                                        , patch = Array.get p patchNames |> Maybe.withDefault ""
                                        }
                                    )
                                    short
                                    short
                                    short
                                    short
                                    short
                                )
                        )
                    |> Decode.map (\patches -> { name = n, width = w, height = h, patches = patches })
            )


{-| R\_CheckTextureNumForName finds the first texture with a name.
-}
textureNumbers : Array Texture -> Dict String Int
textureNumbers textures =
    Array.foldr
        (\t ( i, d ) -> ( i - 1, Dict.insert (normalize t.name) i d ))
        ( Array.length textures - 1, Dict.empty )
        textures
        |> Tuple.second



-- FLATS


{-| R\_InitFlats: firstflat is the lump after F\_START, and the flats run to the lump
before F\_END (FF\_START and FF\_END, as some WADs have, if there is no F\_START).
-}
flatRange : Wad -> Result String ( Int, Array String )
flatRange wad =
    let
        markers start end =
            Maybe.map2 Tuple.pair (Wad.checkNumForName start wad) (Wad.checkNumForName end wad)
    in
    case
        markers "F_START" "F_END"
            |> orElse (\_ -> markers "FF_START" "FF_END")
    of
        Just ( start, end ) ->
            Ok
                ( start + 1
                , List.range (start + 1) (end - 1)
                    |> List.filterMap (\i -> Wad.lumpAt i wad |> Maybe.map (.name >> String.toUpper))
                    |> Array.fromList
                )

        Nothing ->
            Err "the WAD has no F_START and F_END markers"


{-| R\_FlatNumForName for every name: `W_CheckNumForName(name) - firstflat`, where the
last lump of a name wins, as there.
-}
flatNumbers : Int -> Wad -> Dict String Int
flatNumbers firstflat wad =
    List.range 0 (Wad.numLumps wad - 1)
        |> List.foldl
            (\i d ->
                case Wad.lumpAt i wad of
                    Just l ->
                        Dict.insert (normalize l.name) (i - firstflat) d

                    Nothing ->
                        d
            )
            Dict.empty



-- SWITCHES AND ANIMATIONS


{-| P\_InitSwitchList: the (off, on) texture pairs of the switches the game mode has.
As in linuxdoom-1.10, only the registered game gets episode 2 and 3's switches; the
retail game (The Ultimate Doom) only gets the shareware ones.
-}
initSwitchList : Data -> Array Int
initSwitchList data =
    let
        episode =
            case data.gameMode of
                Registered ->
                    2

                Commercial ->
                    3

                _ ->
                    1
    in
    switches
        |> List.filter (\( _, _, e ) -> e <= episode)
        |> List.concatMap (\( name1, name2, _ ) -> [ Data.textureNumForName name1 data, Data.textureNumForName name2 data ])
        |> Array.fromList


switches : List ( String, String, Int )
switches =
    [ ( "SW1BRCOM", "SW2BRCOM", 1 )
    , ( "SW1BRN1", "SW2BRN1", 1 )
    , ( "SW1BRN2", "SW2BRN2", 1 )
    , ( "SW1BRNGN", "SW2BRNGN", 1 )
    , ( "SW1BROWN", "SW2BROWN", 1 )
    , ( "SW1COMM", "SW2COMM", 1 )
    , ( "SW1COMP", "SW2COMP", 1 )
    , ( "SW1DIRT", "SW2DIRT", 1 )
    , ( "SW1EXIT", "SW2EXIT", 1 )
    , ( "SW1GRAY", "SW2GRAY", 1 )
    , ( "SW1GRAY1", "SW2GRAY1", 1 )
    , ( "SW1METAL", "SW2METAL", 1 )
    , ( "SW1PIPE", "SW2PIPE", 1 )
    , ( "SW1SLAD", "SW2SLAD", 1 )
    , ( "SW1STARG", "SW2STARG", 1 )
    , ( "SW1STON1", "SW2STON1", 1 )
    , ( "SW1STON2", "SW2STON2", 1 )
    , ( "SW1STONE", "SW2STONE", 1 )
    , ( "SW1STRTN", "SW2STRTN", 1 )
    , ( "SW1BLUE", "SW2BLUE", 2 )
    , ( "SW1CMT", "SW2CMT", 2 )
    , ( "SW1GARG", "SW2GARG", 2 )
    , ( "SW1GSTON", "SW2GSTON", 2 )
    , ( "SW1HOT", "SW2HOT", 2 )
    , ( "SW1LION", "SW2LION", 2 )
    , ( "SW1SATYR", "SW2SATYR", 2 )
    , ( "SW1SKIN", "SW2SKIN", 2 )
    , ( "SW1VINE", "SW2VINE", 2 )
    , ( "SW1WOOD", "SW2WOOD", 2 )
    , ( "SW1PANEL", "SW2PANEL", 3 )
    , ( "SW1ROCK", "SW2ROCK", 3 )
    , ( "SW1MET2", "SW2MET2", 3 )
    , ( "SW1WDMET", "SW2WDMET", 3 )
    , ( "SW1BRIK", "SW2BRIK", 3 )
    , ( "SW1MOD1", "SW2MOD1", 3 )
    , ( "SW1ZIM", "SW2ZIM", 3 )
    , ( "SW1STON6", "SW2STON6", 3 )
    , ( "SW1TEK", "SW2TEK", 3 )
    , ( "SW1MARB", "SW2MARB", 3 )
    , ( "SW1SKULL", "SW2SKULL", 3 )
    ]


{-| P\_InitPicAnims: the animations whose first frame the WAD has. Fails, as the C
source does, on a cycle of fewer than two pictures.
-}
initPicAnims : Data -> Result String (List Anim)
initPicAnims data =
    animdefs
        |> List.filterMap
            (\( istexture, endname, startname ) ->
                let
                    numbers =
                        if istexture then
                            Data.checkTextureNumForName startname data
                                |> Maybe.map (\base -> ( Data.textureNumForName endname data, base ))

                        else if Dict.member (normalize startname) data.flatNums then
                            Just ( Data.flatNumForName endname data, Data.flatNumForName startname data )

                        else
                            Nothing
                in
                numbers
                    |> Maybe.map
                        (\( picnum, basepic ) ->
                            if picnum - basepic + 1 < 2 then
                                Err ("P_InitPicAnims: bad cycle from " ++ startname ++ " to " ++ endname)

                            else
                                Ok
                                    { istexture = istexture
                                    , picnum = picnum
                                    , basepic = basepic
                                    , numpics = picnum - basepic + 1
                                    , speed = 8
                                    }
                        )
            )
        |> List.foldr (Result.map2 (::)) (Ok [])


{-| animdefs[]: (istexture, endname, startname); every one runs at 8 tics a picture.
-}
animdefs : List ( Bool, String, String )
animdefs =
    [ ( False, "NUKAGE3", "NUKAGE1" )
    , ( False, "FWATER4", "FWATER1" )
    , ( False, "SWATER4", "SWATER1" )
    , ( False, "LAVA4", "LAVA1" )
    , ( False, "BLOOD3", "BLOOD1" )
    , ( False, "RROCK08", "RROCK05" )
    , ( False, "SLIME04", "SLIME01" )
    , ( False, "SLIME08", "SLIME05" )
    , ( False, "SLIME12", "SLIME09" )
    , ( True, "BLODGR4", "BLODGR1" )
    , ( True, "SLADRIP3", "SLADRIP1" )
    , ( True, "BLODRIP4", "BLODRIP1" )
    , ( True, "FIREWALL", "FIREWALA" )
    , ( True, "GSTFONT3", "GSTFONT1" )
    , ( True, "FIRELAVA", "FIRELAV3" )
    , ( True, "FIREMAG3", "FIREMAG1" )
    , ( True, "FIREBLU2", "FIREBLU1" )
    , ( True, "ROCKRED3", "ROCKRED1" )
    , ( True, "BFALL4", "BFALL1" )
    , ( True, "SFALL4", "SFALL1" )
    , ( True, "WFALL4", "WFALL1" )
    , ( True, "DBRAIN4", "DBRAIN1" )
    ]



-- HELPERS


normalize : String -> String
normalize n =
    String.toUpper (String.left 8 n)


short : Decoder Int
short =
    Decode.signedInt16 LE


list : Int -> Decoder a -> Decoder (List a)
list n item =
    Decode.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\x -> Decode.Loop ( left - 1, x :: acc )) item
        )


skipThen : Int -> Decoder a -> Decoder a
skipThen n d =
    if n <= 0 then
        d

    else
        Decode.bytes n |> Decode.andThen (\_ -> d)


orElse : (() -> Maybe a) -> Maybe a -> Maybe a
orElse other m =
    case m of
        Just _ ->
            m

        Nothing ->
            other ()
