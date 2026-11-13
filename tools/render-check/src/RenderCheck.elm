module RenderCheck exposing (main)

{-| Screenshots of the renderer, without the game: reads a WAD and writes PNGs of
TITLEPIC, a few wall textures, a flat, a sprite frame, and views of a map from its
player 1 start.

    eco make src
        / RenderCheck.elm
        --output=render-check
        ./ render
        - check path
        / to
        / doom.wad [ out - dir ] [ map ] [ width ] [ height ]

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Doom.Angle as Angle
import Doom.Data as Data exposing (Data)
import Doom.Fixed as Fixed
import Doom.Render.Build as Build
import Doom.Render.Picture as Picture exposing (Palette, Picture)
import Doom.Render.Png as Png
import Doom.Render.Raster as Raster
import Doom.Render.Scene as Scene
import Doom.Render.SpriteFrames as SpriteFrames
import FileSystem
import Path
import RenderCheck.Map as Map
import RenderCheck.Wad as Wad exposing (Wad)
import Stream
import System
import Task exposing (Task)


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


type alias Options =
    { wad : String
    , out : String
    , map : String
    , width : Int
    , height : Int
    , at : Maybe { x : Int, y : Int, angle : Int }
    }


options : List String -> Maybe Options
options args =
    let
        int s d =
            Maybe.andThen String.toInt s |> Maybe.withDefault d

        at i =
            List.head (List.drop i args)
    in
    at 0
        |> Maybe.map
            (\wad ->
                { wad = wad
                , out = at 1 |> Maybe.withDefault "out"
                , map = at 2 |> Maybe.withDefault "E1M1"
                , width = int (at 3) 320
                , height = int (at 4) 200
                , at =
                    Maybe.map3 (\x y a -> { x = x, y = y, angle = a })
                        (Maybe.andThen String.toInt (at 5))
                        (Maybe.andThen String.toInt (at 6))
                        (Maybe.andThen String.toInt (at 7))
                }
            )


run : System.Environment -> Task Never ()
run env =
    case options (List.drop 1 env.args) of
        Nothing ->
            say env "usage: render-check WAD [out-dir] [map] [width] [height] [x y angle]"

        Just opts ->
            FileSystem.readFile (Path.fromPosixString opts.wad)
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen
                    (\bytes ->
                        case Wad.parse bytes of
                            Just wad ->
                                Task.succeed wad

                            Nothing ->
                                Task.fail "not a WAD"
                    )
                |> Task.andThen (\wad -> writeAll env opts wad)
                |> Task.onError (\err -> say env ("render-check: " ++ err))


say : System.Environment -> String -> Task x ()
say env msg =
    Stream.logLine msg env.stdout


write : System.Environment -> Options -> String -> Bytes -> Task String ()
write env opts name bytes =
    let
        path =
            opts.out ++ "/" ++ name
    in
    FileSystem.writeFile bytes (Path.fromPosixString path)
        |> Task.mapError FileSystem.errorToString
        |> Task.andThen (\_ -> say env ("wrote " ++ path))


{-| The picture with its holes filled with a backdrop colour, as a PNG.
-}
png : Palette -> Picture -> Bytes
png palette pic =
    Png.encode palette
        { pic
            | pixels =
                Array.map
                    (\p ->
                        if p < 0 then
                            backdrop

                        else
                            p
                    )
                    pic.pixels
        }


backdrop : Int
backdrop =
    -- A light blue in Doom's palette, unlike anything in a sprite.
    200


writeAll : System.Environment -> Options -> Wad -> Task String ()
writeAll env opts wad =
    let
        lookup =
            Wad.lookup wad

        palettes =
            lookup "PLAYPAL" |> Maybe.map Picture.decodePlaypal |> Maybe.withDefault Array.empty

        palette =
            Array.get 0 palettes |> Maybe.withDefault Array.empty

        data =
            Wad.readData wad

        texture n =
            Dict.get n data.textureNums |> Maybe.andThen (\i -> Array.get i data.textures)

        patchPng name =
            lookup name
                |> Maybe.andThen Picture.decodePatch
                |> Maybe.map (\p -> write env opts (String.toLower name ++ ".png") (png palette p))

        texturePng solid name =
            texture name
                |> Maybe.map
                    (\t ->
                        write env
                            opts
                            ("texture-" ++ String.toLower name ++ ".png")
                            (png palette
                                ((if solid then
                                    Picture.composeSolidTexture

                                  else
                                    Picture.composeTexture
                                 )
                                    lookup
                                    t
                                )
                            )
                    )

        flatPng name =
            lookup name
                |> Maybe.andThen Picture.decodeFlat
                |> Maybe.map (\p -> write env opts ("flat-" ++ String.toLower name ++ ".png") (png palette p))

        jobs =
            List.filterMap identity
                [ patchPng "TITLEPIC"
                , texturePng True "STARTAN3"
                , texturePng True "BIGDOOR2"
                , texturePng True "SKY1"
                , texturePng False "MIDGRATE"
                , flatPng "FLOOR4_8"
                , flatPng "NUKAGE1"
                , patchPng "TROOA1"
                , patchPng "POSSA2A8"
                , patchPng "PISGA0"
                ]
    in
    Task.sequence jobs
        |> Task.andThen (\_ -> renderMap env opts wad data palettes)


renderMap : System.Environment -> Options -> Wad -> Data -> Array Palette -> Task String ()
renderMap env opts wad data palettes =
    case Map.load wad data opts.map of
        Nothing ->
            Task.fail ("no map " ++ opts.map)

        Just loaded ->
            let
                lookup =
                    Wad.lookup wad

                spriteNames =
                    Wad.between wad "S_START" "S_END" |> List.map .name

                spriteLumps =
                    Array.fromList spriteNames

                mesh =
                    Build.levelMesh loaded.level

                input =
                    { level = loaded.level
                    , data = data
                    , world = loaded.world
                    , mesh = mesh
                    , spriteFrames = SpriteFrames.build spriteNames
                    , spriteInfo = Build.spriteInfo lookup spriteNames
                    , translations = Build.noTranslations
                    , skyTexture = Data.textureNumForName (Build.skyTextureName data.gameMode 1 1) data
                    , player = 0
                    }

                colormaps =
                    lookup "COLORMAP" |> Maybe.map Picture.decodeColormaps |> Maybe.withDefault Array.empty

                shoot ( name, turn ) =
                    case Build.viewpoint 0 loaded.world of
                        Nothing ->
                            Task.fail "no player 1 start"

                        Just view ->
                            let
                                frame =
                                    Build.frame input (placed opts loaded view turn)

                                images =
                                    imagesFor lookup data spriteLumps colormaps frame

                                fb =
                                    Raster.render opts.width opts.height images mesh frame

                                palette =
                                    Array.get frame.palette palettes |> Maybe.withDefault Array.empty
                            in
                            write env opts (String.toLower opts.map ++ "-" ++ name ++ ".png") (Png.encode palette fb)
            in
            [ ( "start", 0 ), ( "left", Angle.ang90 ), ( "back", Angle.ang180 ), ( "right", Angle.ang270 ) ]
                |> List.map shoot
                |> Task.sequence
                |> Task.andThen
                    (\_ ->
                        say env
                            (String.fromInt (Array.length mesh.subsectors)
                                ++ " subsectors, "
                                ++ String.fromInt (Array.length (Array.filter (\s -> List.length s.polygon < 3) mesh.subsectors))
                                ++ " of them with no area"
                            )
                    )
                |> Task.andThen
                    (\_ ->
                        Map.skySpots data loaded
                            |> List.map (\( s, ( x, y ) ) -> "sector " ++ String.fromInt s ++ " has sky, a line at " ++ String.fromInt x ++ " " ++ String.fromInt y)
                            |> String.join "\n"
                            |> say env
                    )


{-| The view from the player start, or from (x, y) on the floor there facing `angle`
degrees, if given; turned by `turn`.
-}
placed : Options -> Map.Loaded -> Build.Viewpoint -> Angle.Angle -> Build.Viewpoint
placed opts loaded view turn =
    case opts.at of
        Nothing ->
            { view | angle = Angle.add view.angle turn }

        Just p ->
            let
                x =
                    Fixed.fromInt p.x

                y =
                    Fixed.fromInt p.y
            in
            { x = x
            , y = y
            , z = Map.floorAt loaded x y + Fixed.fromInt 41
            , angle = Angle.add (Angle.wrap (p.angle * 11930465)) turn
            }


{-| Decode just the pictures this frame uses.
-}
imagesFor : (String -> Maybe Bytes) -> Data -> Array String -> Picture.Colormaps -> Scene.Frame -> Raster.Images
imagesFor lookup data spriteLumps colormaps frame =
    let
        wallTextures wantMasked =
            frame.walls
                |> List.filterMap
                    (\w ->
                        case w.look of
                            Scene.Textured t ->
                                if t.masked == wantMasked then
                                    Just t.texture

                                else
                                    Nothing

                            Scene.SkyWall ->
                                Nothing
                    )

        compose f nums =
            nums
                |> List.foldl
                    (\n acc ->
                        if Dict.member n acc then
                            acc

                        else
                            case Array.get n data.textures of
                                Just t ->
                                    Dict.insert n (f lookup t) acc

                                Nothing ->
                                    acc
                    )
                    Dict.empty

        solid =
            compose Picture.composeSolidTexture (frame.skyTexture :: wallTextures False)

        masked =
            compose Picture.composeTexture (wallTextures True)

        flatNums =
            frame.sectors
                |> Array.toList
                |> List.concatMap (\s -> [ s.floorPic, s.ceilingPic ])
                |> List.filterMap
                    (\p ->
                        case p of
                            Scene.Flat n ->
                                Just n

                            Scene.Sky ->
                                Nothing
                    )

        flats =
            List.foldl
                (\n acc ->
                    if Dict.member n acc then
                        acc

                    else
                        Array.get n data.flats
                            |> Maybe.andThen lookup
                            |> Maybe.andThen Picture.decodeFlat
                            |> Maybe.map (\p -> Dict.insert n p acc)
                            |> Maybe.withDefault acc
                )
                Dict.empty
                flatNums

        sprites =
            (List.map .lump frame.sprites ++ List.map .lump frame.weapon)
                |> List.foldl
                    (\n acc ->
                        if Dict.member n acc then
                            acc

                        else
                            Array.get n spriteLumps
                                |> Maybe.andThen lookup
                                |> Maybe.andThen Picture.decodePatch
                                |> Maybe.map (\p -> Dict.insert n p acc)
                                |> Maybe.withDefault acc
                    )
                    Dict.empty
    in
    { texture = \n -> Dict.get n masked
    , solidTexture = \n -> Dict.get n solid
    , flat = \n -> Dict.get n flats
    , sprite = \n -> Dict.get n sprites
    , colormaps = colormaps
    }
