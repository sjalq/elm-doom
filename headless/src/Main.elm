module Main exposing (main)

{-| Run Doom without a screen, to compare it with linuxdoom-1.10 (the C oracle in
doom/oracle).

    doom-headless -full -iwad <wad> [-frames FILE] [-events FILE] [-iterations N] ...
    doom-headless <wad> <demo lump name or demo file> [maxtics]
    doom-headless --load-only <wad> [map]

`-full` runs the whole program, title, menus, wipes and all, and writes every frame
(see `Full`). The second form plays a demo without drawing anything, printing one
`Doom.StateHash` line after every tic that ran the level, to compare tic by tic.

Only the hash lines go to stdout; anything else goes to stderr. `maxtics` stops after
that many game tics. `--load-only` loads a map (E1M1, or MAP01 for Doom II) and prints
how many of each thing it has, to check the loaders against the WAD.

-}

import Array
import Bytes exposing (Bytes)
import Doom.Data exposing (Data)
import Doom.Data.Load as DataLoad
import Doom.Defs exposing (GameMode(..))
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game)
import Doom.Level.Load as LevelLoad
import Doom.StateHash as StateHash
import Doom.Ticcmd as Ticcmd
import Doom.Wad as Wad exposing (Wad)
import FileSystem
import Full
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
        args ->
            if List.member "-full" args then
                Full.run env args

            else
                legacy env args


legacy : System.Environment -> List String -> Task Never ()
legacy env args =
    case args of
        [ "--load-only", wadPath ] ->
            withWad env wadPath (loadOnly env Nothing)

        [ "--load-only", wadPath, map ] ->
            withWad env wadPath (loadOnly env (Just map))

        [ wadPath, demo ] ->
            withWad env wadPath (playDemo env demo Nothing)

        [ wadPath, demo, maxtics ] ->
            case String.toInt maxtics of
                Just n ->
                    withWad env wadPath (playDemo env demo (Just n))

                Nothing ->
                    fail env ("maxtics must be a number, not " ++ maxtics)

        _ ->
            fail env "usage: doom-headless <wad> <demo lump or file> [maxtics]\n       doom-headless --load-only <wad> [map]"


withWad : System.Environment -> String -> (Wad -> Data -> Task Never ()) -> Task Never ()
withWad env path next =
    readFile path
        |> Task.andThen
            (\bytes ->
                case Wad.fromBytes bytes |> Result.andThen (\w -> DataLoad.fromWad w |> Result.map (Tuple.pair w)) of
                    Ok ( wad, data ) ->
                        next wad data |> Task.mapError never

                    Err problem ->
                        Task.fail (path ++ ": " ++ problem)
            )
        |> Task.onError (fail env)


readFile : String -> Task String Bytes
readFile path =
    FileSystem.readFile (Path.fromPosixString path)
        |> Task.mapError (\err -> path ++ ": " ++ FileSystem.errorToString err)


fail : System.Environment -> String -> Task Never ()
fail env message =
    Stream.logLine message env.stderr
        |> Task.andThen (\_ -> System.setExitCode 1)



-- LOAD ONLY


loadOnly : System.Environment -> Maybe String -> Wad -> Data -> Task Never ()
loadOnly env maybeMap wad data =
    let
        map =
            Maybe.withDefault
                (if data.gameMode == Commercial then
                    "MAP01"

                 else
                    "E1M1"
                )
                maybeMap
    in
    case LevelLoad.load data map wad of
        Err problem ->
            fail env problem

        Ok loaded ->
            let
                level =
                    loaded.level

                line name n =
                    name ++ " " ++ String.fromInt n
            in
            Stream.logLine
                (String.join "\n"
                    [ "map " ++ map
                    , "gamemode " ++ gameModeName data.gameMode
                    , line "lumps" (Wad.numLumps wad)
                    , line "textures" (Array.length data.textures)
                    , line "flats" (Array.length data.flats)
                    , line "switches" (Array.length data.switchList // 2)
                    , line "anims" (List.length data.anims)
                    , line "skyflatnum" data.skyFlatNum
                    , line "things" (List.length level.things)
                    , line "linedefs" (Array.length level.lines)
                    , line "sidedefs" (Array.length level.sides)
                    , line "vertexes" (Array.length level.vertexes)
                    , line "segs" (Array.length level.segs)
                    , line "subsectors" (Array.length level.subsectors)
                    , line "nodes" (Array.length level.nodes)
                    , line "sectors" (Array.length level.sectors)
                    , line "reject" (Array.length level.reject)
                    , "blockmap " ++ String.fromInt level.blockmap.width ++ "x" ++ String.fromInt level.blockmap.height
                    ]
                )
                env.stdout


gameModeName : GameMode -> String
gameModeName mode =
    case mode of
        Shareware ->
            "shareware"

        Registered ->
            "registered"

        Commercial ->
            "commercial"

        Retail ->
            "retail"



-- PLAYING A DEMO


playDemo : System.Environment -> String -> Maybe Int -> Wad -> Data -> Task Never ()
playDemo env demoName maxtics wad data =
    demoBytes wad demoName
        |> Task.andThen (\bytes -> Demo.fromBytes bytes |> resultToTask)
        |> Task.andThen (\demo -> runTics env maxtics 0 (Game.new wad data (Game.playDemo demo)))
        |> Task.onError (fail env)


demoBytes : Wad -> String -> Task String Bytes
demoBytes wad name =
    case Wad.lump name wad of
        Just bytes ->
            Task.succeed bytes

        Nothing ->
            readFile name


resultToTask : Result String a -> Task String a
resultToTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err e ->
            Task.fail e


{-| How many tics to run between writes to stdout.
-}
batch : Int
batch =
    350


runTics : System.Environment -> Maybe Int -> Int -> Game -> Task String ()
runTics env maxtics done game =
    Task.succeed ()
        |> Task.andThen
            (\_ ->
                let
                    ( after, count, lines ) =
                        runBatch maxtics done game
                in
                (if List.isEmpty lines then
                    Task.succeed ()

                 else
                    Stream.logLine (String.join "\n" (List.reverse lines)) env.stdout
                )
                    |> Task.andThen
                        (\_ ->
                            case after.error of
                                Just problem ->
                                    Task.fail problem

                                Nothing ->
                                    if finished maxtics count after then
                                        Stream.logLine ("ran " ++ String.fromInt count ++ " tics") env.stderr

                                    else
                                        runTics env maxtics count after
                        )
            )


finished : Maybe Int -> Int -> Game -> Bool
finished maxtics count game =
    (Maybe.map (\n -> count >= n) maxtics |> Maybe.withDefault False)
        || (game.demo == Nothing && game.state == Game.DemoScreen && game.action == Game.NoAction)


runBatch : Maybe Int -> Int -> Game -> ( Game, Int, List String )
runBatch maxtics done game =
    let
        go n count g lines =
            if n <= 0 || finished maxtics count g || g.error /= Nothing then
                ( g, count, lines )

            else
                let
                    ( next, tic ) =
                        Game.tick noCmds g
                in
                go (n - 1)
                    (count + 1)
                    next
                    (if tic.ranLevel then
                        StateHash.line next.world :: lines

                     else
                        lines
                    )
    in
    go batch done game []


noCmds : Array.Array Ticcmd.Ticcmd
noCmds =
    Array.repeat 4 Ticcmd.empty
