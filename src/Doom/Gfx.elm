module Doom.Gfx exposing
    ( Gfx, load, patch, draw, drawPatch
    , fontStart, fontSize, fontChar, fontHeight, stringWidth, stringHeight, writeText
    )

{-| The 2D pictures the game shell draws (the menus, the title pages, the pause sign)
and the small font (hu\_font, the STCFN lumps), decoded once from the WAD, and
V\_DrawPatch as linuxdoom-1.10 calls it: with RANGECHECK on, a patch that would go
past an edge of the screen is not drawn at all ("Patch at ... exceeds LFB").

@docs Gfx, load, patch, draw, drawPatch
@docs fontStart, fontSize, fontChar, fontHeight, stringWidth, stringHeight, writeText

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Doom.Video as Video exposing (Framebuffer, Patch)
import Doom.Wad as Wad exposing (Wad)


{-| Decoded patches by lump name, and the font.
-}
type Gfx
    = Gfx
        { patches : Dict String Patch
        , font : Array (Maybe Patch)
        }


{-| Decode the shell's pictures and the font from the WAD. Lumps the WAD doesn't have
are left out (the original would stop with an error if it ever drew them).
-}
load : Wad -> Gfx
load wad =
    let
        decode name =
            Wad.lump name wad |> Maybe.andThen Video.patch
    in
    Gfx
        { patches =
            List.filterMap (\name -> decode name |> Maybe.map (Tuple.pair name)) names
                |> Dict.fromList
        , font =
            Array.initialize fontSize
                (\i -> decode ("STCFN" ++ String.padLeft 3 '0' (String.fromInt (fontStart + i))))
        }


names : List String
names =
    [ "TITLEPIC"
    , "CREDIT"
    , "HELP"
    , "HELP1"
    , "HELP2"
    , "M_PAUSE"
    , "M_DOOM"
    , "M_NGAME"
    , "M_OPTION"
    , "M_LOADG"
    , "M_SAVEG"
    , "M_RDTHIS"
    , "M_QUITG"
    , "M_EPISOD"
    , "M_EPI1"
    , "M_EPI2"
    , "M_EPI3"
    , "M_EPI4"
    , "M_NEWG"
    , "M_SKILL"
    , "M_JKILL"
    , "M_ROUGH"
    , "M_HURT"
    , "M_ULTRA"
    , "M_NMARE"
    , "M_OPTTTL"
    , "M_ENDGAM"
    , "M_MESSG"
    , "M_DETAIL"
    , "M_SCRNSZ"
    , "M_MSENS"
    , "M_SVOL"
    , "M_GDHIGH"
    , "M_GDLOW"
    , "M_MSGOFF"
    , "M_MSGON"
    , "M_THERML"
    , "M_THERMM"
    , "M_THERMR"
    , "M_THERMO"
    , "M_CELL1"
    , "M_CELL2"
    , "M_LSLEFT"
    , "M_LSCNTR"
    , "M_LSRGHT"
    , "M_SFXVOL"
    , "M_MUSVOL"
    , "M_SKULL1"
    , "M_SKULL2"
    ]


{-| A decoded picture by lump name.
-}
patch : String -> Gfx -> Maybe Patch
patch name (Gfx g) =
    Dict.get name g.patches


{-| V\_DrawPatch x y of a picture by lump name.
-}
draw : Int -> Int -> String -> Gfx -> Framebuffer -> Framebuffer
draw x y name gfx fb =
    case patch name gfx of
        Just p ->
            drawPatch x y p fb

        Nothing ->
            fb


{-| V\_DrawPatch with linuxdoom's RANGECHECK: skipped if any of it is off the screen.
-}
drawPatch : Int -> Int -> Patch -> Framebuffer -> Framebuffer
drawPatch x y p fb =
    let
        s =
            Video.patchSize p

        left =
            x - s.leftoffset

        top =
            y - s.topoffset
    in
    if left < 0 || left + s.width > Video.width || top < 0 || top + s.height > Video.height then
        fb

    else
        Video.drawPatch x y p fb



-- THE FONT


{-| HU\_FONTSTART: the font's first character, '!'.
-}
fontStart : Int
fontStart =
    Char.toCode '!'


{-| HU\_FONTSIZE: '!' to '\_'.
-}
fontSize : Int
fontSize =
    Char.toCode '_' - fontStart + 1


{-| The font's picture for a character code (after upper-casing), if it has one.
-}
fontChar : Int -> Gfx -> Maybe Patch
fontChar code (Gfx g) =
    Array.get (code - fontStart) g.font |> Maybe.andThen identity


{-| The height of hu\_font[0].
-}
fontHeight : Gfx -> Int
fontHeight gfx =
    fontChar fontStart gfx |> Maybe.map (Video.patchSize >> .height) |> Maybe.withDefault 0


toUpper : Int -> Int
toUpper c =
    if c >= Char.toCode 'a' && c <= Char.toCode 'z' then
        c - 32

    else
        c


inFont : Int -> Bool
inFont c =
    c - fontStart >= 0 && c - fontStart < fontSize


{-| M\_StringWidth: characters the font doesn't have count 4 pixels.
-}
stringWidth : String -> Gfx -> Int
stringWidth s gfx =
    String.foldl
        (\ch w ->
            let
                c =
                    toUpper (Char.toCode ch)
            in
            if inFont c then
                w + (fontChar c gfx |> Maybe.map (Video.patchSize >> .width) |> Maybe.withDefault 0)

            else
                w + 4
        )
        0
        s


{-| M\_StringHeight: a line's height for each line.
-}
stringHeight : String -> Gfx -> Int
stringHeight s gfx =
    fontHeight gfx * (1 + List.length (String.indexes "\n" s))


{-| M\_WriteText: the string in the font from (x, y), "\\n" starting a new line 12
pixels down. Drawing stops at the first character that would go past the right edge.
-}
writeText : Int -> Int -> String -> Gfx -> Framebuffer -> Framebuffer
writeText x y s gfx fb =
    let
        go chars cx cy acc =
            case chars of
                [] ->
                    acc

                ch :: rest ->
                    if ch == '\n' then
                        go rest x (cy + 12) acc

                    else
                        let
                            c =
                                toUpper (Char.toCode ch)
                        in
                        if not (inFont c) then
                            go rest (cx + 4) cy acc

                        else
                            case fontChar c gfx of
                                Just p ->
                                    let
                                        w =
                                            (Video.patchSize p).width
                                    in
                                    if cx + w > Video.width then
                                        acc

                                    else
                                        go rest (cx + w) cy (drawPatch cx cy p acc)

                                Nothing ->
                                    acc
    in
    go (String.toList s) x y fb
