module Doom.Demo exposing (Demo, Header, fromBytes, header, readTiccmd, marker)

{-| Demos (g\_game.c's G\_DoPlayDemo and G\_ReadDemoTiccmd): a recording of a game's
settings and of every player's ticcmd, tic by tic.

A demo is a 13-byte header (version, skill, episode, map, deathmatch, respawnparm,
fastparm, nomonsters, consoleplayer, then playeringame for the four players), then
four bytes per player in the game per tic: forwardmove and sidemove (signed),
angleturn's high byte, buttons. A byte of 0x80 where a tic would start ends it.

    Demo.fromBytes bytes
        |> Result.map (\demo -> (Demo.header demo).map)

@docs Demo, Header, fromBytes, header, readTiccmd, marker

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Doom.Defs as Defs exposing (Skill)
import Doom.Int32 as Int32
import Doom.Ticcmd exposing (Ticcmd)


{-| A demo being played: its header, its tic data and where playback is in it.
-}
type Demo
    = Demo
        { header : Header
        , data : Array Int
        , pos : Int
        }


{-| The settings the demo was recorded with. `skill` is clamped to Nightmare, as
G\_InitNew does with a bigger number.
-}
type alias Header =
    { version : Int
    , skill : Skill
    , episode : Int
    , map : Int
    , deathmatch : Int
    , respawnparm : Bool
    , fastparm : Bool
    , nomonsters : Bool
    , consoleplayer : Int
    , playeringame : Array Bool
    }


{-| DEMOMARKER: the byte that ends a demo.
-}
marker : Int
marker =
    0x80


{-| Read a demo lump or file. Only version 109 demos (Doom 1.9, which Freedoom's demos
are) are played, as by the DOS v1.9 executable's G\_DoPlayDemo (chocolate-doom
g\_game.c); linuxdoom-1.10 wanted 110 instead.
-}
fromBytes : Bytes -> Result String Demo
fromBytes bytes =
    case Decode.decode (bytesDecoder (Bytes.width bytes)) bytes of
        Nothing ->
            Err "the demo is too short for its header"

        Just all ->
            case all of
                version :: skill :: episode :: map :: deathmatch :: respawn :: fast :: nomonsters :: console :: p0 :: p1 :: p2 :: p3 :: rest ->
                    if version /= 109 then
                        Err ("Demo is from a different game version! (version " ++ String.fromInt version ++ ")")

                    else
                        Ok
                            (Demo
                                { header =
                                    { version = version
                                    , skill = Defs.skillFromIndex (min 4 skill) |> Maybe.withDefault Defs.Nightmare
                                    , episode = episode
                                    , map = map
                                    , deathmatch = deathmatch
                                    , respawnparm = respawn /= 0
                                    , fastparm = fast /= 0
                                    , nomonsters = nomonsters /= 0
                                    , consoleplayer = console
                                    , playeringame = Array.fromList (List.map (\b -> b /= 0) [ p0, p1, p2, p3 ])
                                    }
                                , data = Array.fromList rest
                                , pos = 0
                                }
                            )

                _ ->
                    Err "the demo is too short for its header"


bytesDecoder : Int -> Decoder (List Int)
bytesDecoder n =
    Decode.loop ( n, [] )
        (\( left, acc ) ->
            if left <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\b -> Decode.Loop ( left - 1, b :: acc )) Decode.unsignedInt8
        )


{-| The demo's settings.
-}
header : Demo -> Header
header (Demo d) =
    d.header


{-| G\_ReadDemoTiccmd: the next player's ticcmd, filling in forwardmove, sidemove,
angleturn and buttons of the one given (the rest are kept, as the C source keeps the
network command's). Nothing at the end marker (or the end of the data): the demo is
over.
-}
readTiccmd : Ticcmd -> Demo -> Maybe ( Ticcmd, Demo )
readTiccmd base (Demo d) =
    let
        at i =
            Array.get (d.pos + i) d.data
    in
    case ( at 0, at 1 ) of
        ( Just forward, Just side ) ->
            if forward == marker then
                Nothing

            else
                Maybe.map2
                    (\turn buttons ->
                        ( { base
                            | forwardmove = Int32.toSignedByte forward
                            , sidemove = Int32.toSignedByte side
                            , angleturn = Int32.toShort (turn * 256)
                            , buttons = buttons
                          }
                        , Demo { d | pos = d.pos + 4 }
                        )
                    )
                    (at 2)
                    (at 3)

        _ ->
            Nothing
