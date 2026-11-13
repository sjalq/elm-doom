module DemoFrames exposing (main)

{-| Frames from the real game: plays a demo through the `Doom` TEA API and writes
PNGs of the console player's view at the tics asked for.

    ./demo-frames doom.wad DEMO1 out 640 400 300 1500 4000

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict
import Doom
import Doom.Data as Data exposing (Data)
import Doom.Render.Build as Build
import Doom.Render.Picture as Picture
import Doom.Render.Png as Png
import Doom.Render.Raster as Raster
import Doom.Render.Scene as Scene
import Doom.Render.SpriteFrames as SpriteFrames
import Doom.Wad as Wad
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


run : System.Environment -> Task Never ()
run env =
    case List.drop 1 env.args of
        wadPath :: demo :: out :: w :: h :: tics ->
            FileSystem.readFile (Path.fromPosixString wadPath)
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen
                    (\bytes ->
                        case Doom.start (Doom.Demo demo) bytes of
                            Ok model ->
                                shots env
                                    out
                                    (Maybe.withDefault 320 (String.toInt w))
                                    (Maybe.withDefault 200 (String.toInt h))
                                    (List.filterMap String.toInt tics |> List.sort)
                                    0
                                    model

                            Err e ->
                                Task.fail e
                    )
                |> Task.onError (\e -> say env ("demo-frames: " ++ e))

        _ ->
            say env "usage: demo-frames WAD DEMO OUTDIR WIDTH HEIGHT TIC..."


say : System.Environment -> String -> Task x ()
say env msg =
    Stream.logLine msg env.stdout


{-| Run tics one at a time, shooting at each requested one. -}
shots : System.Environment -> String -> Int -> Int -> List Int -> Int -> Doom.Model -> Task String ()
shots env out w h wanted tic model =
    case wanted of
        [] ->
            Task.succeed ()

        next :: rest ->
            if tic < next then
                shots env out w h wanted (tic + 1) (Tuple.first (Doom.update (Doom.Tick Doom.msPerTic) model))

            else
                shoot env out w h tic model
                    |> Task.andThen (\_ -> shots env out w h rest tic model)


shoot : System.Environment -> String -> Int -> Int -> Int -> Doom.Model -> Task String ()
shoot env out w h tic model =
    case Doom.playing model of
        Nothing ->
            say env ("tic " ++ String.fromInt tic ++ ": not in a level")

        Just ( gameEnv, world ) ->
            let
                wad =
                    Doom.wad model

                lookup name =
                    Wad.lump name wad

                spriteNames =
                    Wad.between "S_START" "S_END" wad |> List.map .name

                data =
                    gameEnv.data

                mesh =
                    Build.levelMesh gameEnv.level

                player =
                    gameEnv.params.consoleplayer

                input =
                    { level = gameEnv.level
                    , data = data
                    , world = world
                    , mesh = mesh
                    , spriteFrames = SpriteFrames.build spriteNames
                    , spriteInfo = Build.spriteInfo lookup spriteNames
                    , translations = Build.specTranslations gameEnv world.leveltime
                    , skyTexture = Data.textureNumForName (Build.skyTextureName data.gameMode gameEnv.params.episode gameEnv.params.map) data
                    , player = player
                    }

                colormaps =
                    lookup "COLORMAP" |> Maybe.map Picture.decodeColormaps |> Maybe.withDefault Array.empty

                palettes =
                    lookup "PLAYPAL" |> Maybe.map Picture.decodePlaypal |> Maybe.withDefault Array.empty
            in
            case Build.viewpoint player world of
                Nothing ->
                    say env ("tic " ++ String.fromInt tic ++ ": no view")

                Just view ->
                    let
                        frame =
                            Build.frame input view

                        fb =
                            Raster.render w h (imagesFor lookup data (Array.fromList spriteNames) colormaps frame) mesh frame

                        palette =
                            Array.get frame.palette palettes |> Maybe.withDefault Array.empty

                        path =
                            out ++ "/tic" ++ String.padLeft 5 '0' (String.fromInt tic) ++ ".png"
                    in
                    FileSystem.writeFile (Png.encode palette fb) (Path.fromPosixString path)
                        |> Task.mapError FileSystem.errorToString
                        |> Task.andThen (\_ -> say env ("wrote " ++ path))


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
