module AutomapCheck exposing (main)

{-| Automap frames from the real game, to compare with the C automap pixel for pixel.
Plays a demo through the `Doom` TEA API and, after each level tic, runs a script
of automap events, then `Doom.Automap.ticker`, and writes the screen
(`OUTDIR/amNNNNN.raw`, 64000 bytes, rows 0 to 167 drawn by the automap) at the tics
asked for.

    ./automap-check doom.wad DEMO1 script.txt out

Script lines, by leveltime: `T down KEY`, `T up KEY` (Doom key codes), `T map`
(set ML\_MAPPED on every line whose index mod 3 isn't 2), `T allmap N` (the
console player's computer map power), `T dump`. The C harness made from
`doom/oracle` reads the same script.

-}

import Array
import Bytes
import Bytes.Decode
import Doom
import Doom.Automap as Automap
import Doom.Defs as Defs
import Doom.Event exposing (Event(..))
import Doom.Flags as Flags
import Doom.Player as Player
import Doom.Video as Video
import Doom.World as World exposing (World)
import FileSystem
import Path
import Stream
import System
import Task exposing (Task)


main : Platform.Program System.Environment () ()
main =
    System.simpleProgram run


type alias Command =
    { tic : Int
    , cmd : String
    , arg : Int
    }


parse : String -> List Command
parse text =
    String.lines text
        |> List.filterMap
            (\line ->
                case String.words line of
                    t :: c :: rest ->
                        String.toInt t
                            |> Maybe.map
                                (\tic ->
                                    { tic = tic
                                    , cmd = c
                                    , arg = rest |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0
                                    }
                                )

                    _ ->
                        Nothing
            )


run : System.Environment -> Task Never ()
run env =
    case List.drop 1 env.args of
        [ wadPath, demo, scriptPath, out ] ->
            FileSystem.readFile (Path.fromPosixString wadPath)
                |> Task.andThen
                    (\bytes ->
                        FileSystem.readFile (Path.fromPosixString scriptPath)
                            |> Task.map (\s -> ( bytes, s ))
                    )
                |> Task.mapError FileSystem.errorToString
                |> Task.andThen
                    (\( bytes, scriptBytes ) ->
                        case Doom.start (Doom.Demo demo) bytes of
                            Ok model ->
                                let
                                    script =
                                        scriptBytes
                                            |> (\b -> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width b)) b)
                                            |> Maybe.withDefault ""
                                            |> parse

                                    last =
                                        script |> List.map .tic |> List.maximum |> Maybe.withDefault 0
                                in
                                loop env out script last -1 { mapped = False, allmap = Nothing } (Automap.init (Doom.wad model)) model

                            Err e ->
                                Task.fail e
                    )
                |> Task.onError (\e -> say env ("automap-check: " ++ e))

        _ ->
            say env "usage: automap-check WAD DEMO SCRIPT OUTDIR"


say : System.Environment -> String -> Task x ()
say env msg =
    Stream.logLine msg env.stdout


{-| One game tic at a time; after a tic that advanced leveltime, the script's events
for it, then the automap's ticker, then the dumps.
-}
loop : System.Environment -> String -> List Command -> Int -> Int -> Overlay -> Automap.Model -> Doom.Model -> Task String ()
loop env out script last lastLeveltime overlay am model =
    let
        next =
            Tuple.first (Doom.update (Doom.Tick Doom.msPerTic) model)
    in
    case Doom.playing next of
        Nothing ->
            loop env out script last lastLeveltime overlay am next

        Just ( gameEnv, world0 ) ->
            let
                lt =
                    world0.leveltime
            in
            if lt == lastLeveltime then
                loop env out script last lastLeveltime overlay am next

            else if lt > last || lt < lastLeveltime then
                Task.succeed ()

            else
                let
                    player =
                        gameEnv.params.consoleplayer

                    now =
                        List.filter (\c -> c.tic == lt) script

                    -- The script's changes to the world stay made: the game
                    -- doesn't touch ML_MAPPED or the computer map power.
                    overlay1 =
                        List.foldl
                            (\c o ->
                                case c.cmd of
                                    "map" ->
                                        { o | mapped = True }

                                    "allmap" ->
                                        { o | allmap = Just c.arg }

                                    _ ->
                                        o
                            )
                            overlay
                            now

                    worldIn =
                        applyOverlay player overlay1 world0

                    -- P_KillMobj stops the automap when the console player dies.
                    amAlive =
                        case Doom.playing model of
                            Just ( _, before ) ->
                                Automap.stopOnDeath before world0 player am

                            Nothing ->
                                am

                    ( am1, world1 ) =
                        List.foldl
                            (\c ( a, w ) ->
                                case c.cmd of
                                    "down" ->
                                        let
                                            ( a2, w2, _ ) =
                                                Automap.responder (KeyDown c.arg) gameEnv w player a
                                        in
                                        ( a2, w2 )

                                    "up" ->
                                        let
                                            ( a2, w2, _ ) =
                                                Automap.responder (KeyUp c.arg) gameEnv w player a
                                        in
                                        ( a2, w2 )

                                    _ ->
                                        ( a, w )
                            )
                            ( amAlive, worldIn )
                            now

                    am2 =
                        Automap.ticker gameEnv world1 player am1

                    dumps =
                        if List.any (\c -> c.cmd == "dump") now && Automap.active am2 then
                            let
                                fb =
                                    Automap.drawer gameEnv world1 player am2 (Video.blank 0)

                                path =
                                    out ++ "/am" ++ String.padLeft 5 '0' (String.fromInt lt) ++ ".raw"
                            in
                            FileSystem.writeFile (Video.toBytes fb) (Path.fromPosixString path)
                                |> Task.mapError FileSystem.errorToString
                                |> Task.map (\_ -> ())

                        else
                            Task.succeed ()
                in
                dumps |> Task.andThen (\_ -> loop env out script last lt overlay1 am2 next)


type alias Overlay =
    { mapped : Bool
    , allmap : Maybe Int
    }


applyOverlay : Int -> Overlay -> World -> World
applyOverlay player o w0 =
    let
        w1 =
            if o.mapped then
                markLines w0

            else
                w0
    in
    case o.allmap of
        Just n ->
            World.updatePlayer player (Player.setPower Defs.AllMap n) w1

        Nothing ->
            w1


markLines : World -> World
markLines w =
    { w
        | lines =
            Array.indexedMap
                (\i l ->
                    if modBy 3 i /= 2 then
                        { l | flags = Flags.set Flags.mlMapped l.flags }

                    else
                        l
                )
                w.lines
    }
