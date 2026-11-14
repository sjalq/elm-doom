module SaveCheck exposing (main)

{-| Check saved games against linuxdoom-1.10 (verify/oracle's `-savecheck`).

    savecheck <wad> <demo lump or file> <leveltime> <save file>

Plays the demo; at the start of the first tic of a level whose `leveltime` is the
one given, saves the game (`Doom.Save.save`) to the save file, loads it back
(`Doom.Save.load`), and plays the rest of the demo from the loaded game, as the
oracle does (loading stops demo playback, as G\_InitNew does; both put it back on).
Prints one `Doom.StateHash` line after every tic that ran the level, before and
after the load. On stderr it reports whether saving the loaded game and loading and
saving that again give the same bytes, and how many bytes the first save and the
save of the loaded game differ in (only things' `floorz`/`ceilingz` may: loading
puts them at their sector's heights).

-}

import Array
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Doom.Data.Load as DataLoad
import Doom.Demo as Demo
import Doom.Game as Game exposing (Game)
import Doom.Save as Save
import Doom.StateHash as StateHash
import Doom.Ticcmd as Ticcmd
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
        [ wadPath, demoName, leveltime, savePath ] ->
            case String.toInt leveltime of
                Just n ->
                    check env wadPath demoName n savePath
                        |> Task.onError (fail env)

                Nothing ->
                    fail env "leveltime must be a number"

        _ ->
            fail env "usage: savecheck <wad> <demo lump or file> <leveltime> <save file>"


fail : System.Environment -> String -> Task Never ()
fail env message =
    Stream.logLine message env.stderr
        |> Task.andThen (\_ -> System.setExitCode 1)


readFile : String -> Task String Bytes
readFile path =
    FileSystem.readFile (Path.fromPosixString path)
        |> Task.mapError (\err -> path ++ ": " ++ FileSystem.errorToString err)


fromResult : Result String a -> Task String a
fromResult result =
    case result of
        Ok a ->
            Task.succeed a

        Err e ->
            Task.fail e


check : System.Environment -> String -> String -> Int -> String -> Task String ()
check env wadPath demoName leveltime savePath =
    readFile wadPath
        |> Task.andThen (\bytes -> Wad.fromBytes bytes |> fromResult)
        |> Task.andThen
            (\wad ->
                DataLoad.fromWad wad
                    |> fromResult
                    |> Task.andThen
                        (\data ->
                            (case Wad.lump demoName wad of
                                Just bytes ->
                                    Task.succeed bytes

                                Nothing ->
                                    readFile demoName
                            )
                                |> Task.andThen (\bytes -> Demo.fromBytes bytes |> fromResult)
                                |> Task.map (\demo -> Game.new wad data (Game.playDemo demo))
                        )
            )
        |> Task.andThen
            (\game ->
                let
                    result =
                        play leveltime game
                in
                Stream.logLine (String.join "\n" result.lines) env.stdout
                    |> Task.andThen
                        (\_ ->
                            case ( result.saved, result.error ) of
                                ( Nothing, Just problem ) ->
                                    Task.fail problem

                                ( Nothing, Nothing ) ->
                                    Task.fail ("never reached leveltime " ++ String.fromInt leveltime ++ " in a level")

                                ( Just ( first, report ), _ ) ->
                                    FileSystem.writeFile first (Path.fromPosixString savePath)
                                        |> Task.mapError FileSystem.errorToString
                                        |> Task.andThen (\_ -> Stream.logLine report env.stderr)
                        )
                    |> Task.andThen
                        (\_ ->
                            case result.error of
                                Just problem ->
                                    Task.fail problem

                                Nothing ->
                                    Task.succeed ()
                        )
            )


type alias Result_ =
    { lines : List String
    , saved : Maybe ( Bytes, String )
    , error : Maybe String
    }


play : Int -> Game -> Result_
play leveltime game =
    go leveltime game Nothing []


over : Game -> Bool
over g =
    g.demo == Nothing && g.state == Game.DemoScreen && g.action == Game.NoAction


go : Int -> Game -> Maybe ( Bytes, String ) -> List String -> Result_
go leveltime g saved lines =
    if over g || g.error /= Nothing then
        { lines = List.reverse lines, saved = saved, error = g.error }

    else
        case ( saved, Game.playing g ) of
            ( Nothing, Just ( _, world ) ) ->
                if world.leveltime == leveltime && g.action == Game.NoAction then
                    case saveAndLoad g of
                        Ok ( loaded, first, report ) ->
                            step leveltime loaded (Just ( first, report )) lines

                        Err problem ->
                            { lines = List.reverse lines, saved = Nothing, error = Just problem }

                else
                    step leveltime g saved lines

            _ ->
                step leveltime g saved lines


step : Int -> Game -> Maybe ( Bytes, String ) -> List String -> Result_
step leveltime g saved lines =
    let
        ( next, tic ) =
            Game.tick (Array.repeat 4 Ticcmd.empty) g
    in
    go leveltime
        next
        saved
        (if tic.ranLevel then
            StateHash.line next.world :: lines

         else
            lines
        )


saveAndLoad : Game -> Result String ( Game, Bytes, String )
saveAndLoad game =
    let
        saveOf g =
            Save.save "SAVECHECK" g

        reload bytes =
            Save.load bytes game |> Result.map (\g -> { g | demoplayback = True })
    in
    saveOf game
        |> Result.andThen
            (\first ->
                reload first
                    |> Result.andThen
                        (\loaded ->
                            saveOf loaded
                                |> Result.andThen
                                    (\second ->
                                        reload second
                                            |> Result.andThen saveOf
                                            |> Result.map
                                                (\third ->
                                                    ( loaded
                                                    , first
                                                    , String.join "\n"
                                                        [ "save: " ++ String.fromInt (Bytes.width first) ++ " bytes"
                                                        , "save of the loaded game: "
                                                            ++ String.fromInt (differences first second)
                                                            ++ " bytes differ from the first save"
                                                        , "save, load, save again: "
                                                            ++ (if toList second == toList third then
                                                                    "identical"

                                                                else
                                                                    "DIFFERENT (" ++ String.fromInt (differences second third) ++ " bytes)"
                                                               )
                                                        ]
                                                    )
                                                )
                                    )
                        )
            )


toList : Bytes -> List Int
toList bytes =
    Decode.decode
        (Decode.loop ( Bytes.width bytes, [] )
            (\( n, acc ) ->
                if n <= 0 then
                    Decode.succeed (Decode.Done (List.reverse acc))

                else
                    Decode.unsignedInt8 |> Decode.map (\b -> Decode.Loop ( n - 1, b :: acc ))
            )
        )
        bytes
        |> Maybe.withDefault []


differences : Bytes -> Bytes -> Int
differences a b =
    let
        la =
            toList a

        lb =
            toList b
    in
    List.map2 (/=) la lb
        |> List.filter identity
        |> List.length
        |> (+) (abs (List.length la - List.length lb))
