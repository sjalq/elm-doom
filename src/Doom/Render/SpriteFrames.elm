module Doom.Render.SpriteFrames exposing
    ( SpriteFrames, Frame, Rotation
    , build, frame, rotationFor
    )

{-| Which sprite lump to draw for a thing (r\_things.c's R\_InitSpriteDefs and the
rotation choice in R\_ProjectSprite).

Sprite lumps are named by sprite, frame letter and rotation: `TROOA1` is the imp's
frame A seen from the front (rotation 1), `TROOA2A8` is frame A rotation 2, and,
mirrored, rotation 8. Rotation 0 (`BAR1A0`) is the same from every side.

A sprite lump is referred to by its **number**: its position in the list of sprite
lump names given to `build` (the lumps between S\_START and S\_END, in WAD order).

@docs SpriteFrames, Frame, Rotation
@docs build, frame, rotationFor

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Doom.Angle as Angle exposing (Angle)
import Doom.Info.Sprite as Sprite exposing (Sprite)
import Doom.Int32 as Int32


{-| Every sprite's frames.
-}
type SpriteFrames
    = SpriteFrames (Dict ( String, Int ) Frame)


{-| One frame: the lump and mirroring for each of the 8 rotations (index 0 is rotation
1, seen from the front; they go round anticlockwise as the thing sees it, i.e. index
`r` is for a viewer at an angle of `r * 45` degrees from the way the thing faces).
`rotate` is False for a frame drawn the same from every side (all 8 the same).
-}
type alias Frame =
    { rotate : Bool
    , rotations : Array Rotation
    }


{-| A sprite lump, by number, and whether to draw it mirrored.
-}
type alias Rotation =
    { lump : Int
    , flip : Bool
    }


{-| R\_InitSpriteDefs, from the sprite lumps' names in WAD order. Later lumps replace
earlier ones for the same frame and rotation, as a PWAD's would.
-}
build : List String -> SpriteFrames
build names =
    names
        |> List.indexedMap Tuple.pair
        |> List.foldl addLump Dict.empty
        |> Dict.map (\_ partial -> finish partial)
        |> SpriteFrames


type alias Partial =
    Dict Int Rotation


addLump : ( Int, String ) -> Dict ( String, Int ) Partial -> Dict ( String, Int ) Partial
addLump ( lump, rawName ) acc =
    let
        name =
            String.toUpper rawName

        sprite =
            String.left 4 name

        install frameChar rotChar flip a =
            case ( letterIndex frameChar, digit rotChar ) of
                ( Just f, Just r ) ->
                    Dict.update ( sprite, f )
                        (\existing ->
                            let
                                p =
                                    Maybe.withDefault Dict.empty existing
                            in
                            Just (installRotation r { lump = lump, flip = flip } p)
                        )
                        a

                _ ->
                    a
    in
    case String.toList (String.dropLeft 4 name) of
        [ f, r ] ->
            install f r False acc

        [ f, r, f2, r2 ] ->
            acc |> install f r False |> install f2 r2 True

        _ ->
            acc


installRotation : Int -> Rotation -> Partial -> Partial
installRotation r rot p =
    if r == 0 then
        Dict.singleton 0 rot

    else
        Dict.insert r rot (Dict.remove 0 p)


finish : Partial -> Frame
finish p =
    case Dict.get 0 p of
        Just rot ->
            { rotate = False, rotations = Array.repeat 8 rot }

        Nothing ->
            let
                fallback =
                    Dict.values p |> List.head |> Maybe.withDefault { lump = -1, flip = False }
            in
            { rotate = True
            , rotations = Array.initialize 8 (\i -> Dict.get (i + 1) p |> Maybe.withDefault fallback)
            }


letterIndex : Char -> Maybe Int
letterIndex c =
    let
        n =
            Char.toCode c - Char.toCode 'A'
    in
    if n >= 0 && n < 29 then
        Just n

    else
        Nothing


digit : Char -> Maybe Int
digit c =
    let
        n =
            Char.toCode c - Char.toCode '0'
    in
    if n >= 0 && n <= 8 then
        Just n

    else
        Nothing


{-| A sprite's frame (0 is A), if the WAD has it.
-}
frame : Sprite -> Int -> SpriteFrames -> Maybe Frame
frame sprite n (SpriteFrames frames) =
    Dict.get ( Sprite.name sprite, n ) frames


{-| R\_ProjectSprite's choice of rotation: the frame seen from `viewAngle` (the angle
from the viewer to the thing, R\_PointToAngle) by a thing facing `thingAngle`:
`(ang - thing->angle + (ANG45/2)*9) >> 29`.
-}
rotationFor : Angle -> Angle -> Frame -> Rotation
rotationFor viewAngle thingAngle fr =
    let
        r =
            if fr.rotate then
                Int32.shiftRightUnsigned 29
                    (Angle.wrap (viewAngle - thingAngle + (Angle.ang45 // 2) * 9))

            else
                0
    in
    Array.get r fr.rotations |> Maybe.withDefault { lump = -1, flip = False }
