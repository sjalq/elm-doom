module LoadTests exposing (suite)

import Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Doom.Data exposing (Data)
import Doom.Defs as Defs
import Doom.Demo as Demo
import Doom.Fixed as Fixed
import Doom.Input as Input
import Doom.Level as Level
import Doom.Level.Load as LevelLoad
import Doom.Ticcmd as Ticcmd
import Doom.Wad as Wad exposing (MapLumps)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "loading"
        [ wadTests
        , levelTests
        , demoTests
        , inputTests
        ]



-- WAD


name8 : String -> Encode.Encoder
name8 n =
    Encode.sequence
        (List.map (\c -> Encode.unsignedInt8 (Char.toCode c)) (String.toList (String.left 8 n))
            ++ List.repeat (8 - String.length (String.left 8 n)) (Encode.unsignedInt8 0)
        )


bytesOf : List Int -> Bytes
bytesOf list =
    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 list))


toList : Bytes -> List Int
toList b =
    Decode.decode
        (Decode.loop ( Bytes.width b, [] )
            (\( n, acc ) ->
                if n <= 0 then
                    Decode.succeed (Decode.Done (List.reverse acc))

                else
                    Decode.map (\x -> Decode.Loop ( n - 1, x :: acc )) Decode.unsignedInt8
            )
        )
        b
        |> Maybe.withDefault []


{-| A PWAD whose lumps are stored out of directory order: header, "BBB" data, "AAA"
data, a second "aaa", then the directory.
-}
smallWad : Bytes
smallWad =
    let
        dirEntry pos size n =
            Encode.sequence [ Encode.signedInt32 LE pos, Encode.signedInt32 LE size, name8 n ]
    in
    Encode.encode
        (Encode.sequence
            [ Encode.string "PWAD"
            , Encode.signedInt32 LE 4
            , Encode.signedInt32 LE (12 + 2 + 3 + 1)
            , Encode.sequence (List.map Encode.unsignedInt8 [ 4, 5 ])
            , Encode.sequence (List.map Encode.unsignedInt8 [ 1, 2, 3 ])
            , Encode.unsignedInt8 9
            , dirEntry 14 3 "AAA"
            , dirEntry 12 2 "BBB"
            , dirEntry 0 0 "MARK"
            , dirEntry 17 1 "AAA"
            ]
        )


{-| A WAD laid out as `Wad.toBytes` lays it out, written independently: the header,
the lumps' data in order, then the directory.
-}
wadOf : String -> List ( String, List Int ) -> Bytes
wadOf ident lumps =
    let
        dataSize =
            List.foldl (\( _, d ) n -> n + List.length d) 0 lumps

        dir =
            List.foldl
                (\( n, d ) ( pos, acc ) ->
                    ( pos + List.length d
                    , acc ++ [ Encode.signedInt32 LE pos, Encode.signedInt32 LE (List.length d), name8 n ]
                    )
                )
                ( 12, [] )
                lumps
                |> Tuple.second
    in
    Encode.encode
        (Encode.sequence
            ([ Encode.string ident, Encode.signedInt32 LE (List.length lumps), Encode.signedInt32 LE (12 + dataSize) ]
                ++ List.concatMap (\( _, d ) -> List.map Encode.unsignedInt8 d) lumps
                ++ dir
            )
        )


lumpsOf : Wad.Wad -> List ( String, List Int )
lumpsOf wad =
    List.range 0 (Wad.numLumps wad - 1)
        |> List.filterMap (\i -> Wad.lumpAt i wad)
        |> List.map (\l -> ( l.name, toList l.bytes ))


patched : List ( String, List Int ) -> List ( String, List Int ) -> Result String ( Bool, List ( String, List Int ) )
patched base patchLumps =
    Result.map2 (\b p -> Wad.patch p b)
        (Wad.fromBytes (wadOf "IWAD" base))
        (Wad.fromBytes (wadOf "PWAD" patchLumps))
        |> Result.map (\w -> ( Wad.kind w == Wad.Iwad, lumpsOf w ))


mapGroup : String -> Int -> List ( String, List Int )
mapGroup marker v =
    ( marker, [] ) :: List.map (\n -> ( n, [ v ] )) [ "THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SEGS", "SSECTORS", "NODES", "SECTORS", "REJECT", "BLOCKMAP" ]


wadTests : Test
wadTests =
    describe "Doom.Wad"
        [ test "reads every lump, in directory order" <|
            \_ ->
                case Wad.fromBytes smallWad of
                    Ok wad ->
                        List.range 0 3
                            |> List.filterMap (\i -> Wad.lumpAt i wad)
                            |> List.map (\l -> ( l.name, toList l.bytes ))
                            |> Expect.equal [ ( "AAA", [ 1, 2, 3 ] ), ( "BBB", [ 4, 5 ] ), ( "MARK", [] ), ( "AAA", [ 9 ] ) ]

                    Err e ->
                        Expect.fail e
        , test "the last lump of a name wins, in any case" <|
            \_ ->
                Wad.fromBytes smallWad
                    |> Result.toMaybe
                    |> Maybe.andThen (Wad.lump "aaa")
                    |> Maybe.map toList
                    |> Expect.equal (Just [ 9 ])
        , test "lumps between markers" <|
            \_ ->
                Wad.fromBytes smallWad
                    |> Result.map (Wad.between "BBB" "AAA" >> List.map .name)
                    |> Expect.equal (Ok [ "MARK" ])
        , fuzz
            (Fuzz.list
                (Fuzz.pair (Fuzz.oneOfValues [ "A", "PLAYPAL", "F_START", "STFST01", "E1M1" ])
                    (Fuzz.list (Fuzz.intRange 0 255))
                )
            )
            "toBytes writes back what fromBytes read, laid out as the original does"
          <|
            \lumps ->
                Wad.fromBytes (wadOf "PWAD" lumps)
                    |> Result.map Wad.toBytes
                    |> Result.map toList
                    |> Expect.equal (Ok (toList (wadOf "PWAD" lumps)))
        , test "a patch's lumps take the place of the base's, and new ones go at the end" <|
            \_ ->
                patched
                    [ ( "PLAYPAL", [ 1 ] ), ( "F_START", [] ), ( "DEM1_1", [ 2, 2 ] ), ( "F_END", [] ), ( "STFST01", [ 3 ] ) ]
                    [ ( "STFST01", [ 7, 7, 7 ] ), ( "dem1_1", [ 8 ] ), ( "NEW", [ 9 ] ) ]
                    |> Expect.equal
                        (Ok
                            ( True
                            , [ ( "PLAYPAL", [ 1 ] ), ( "F_START", [] ), ( "dem1_1", [ 8 ] ), ( "F_END", [] ), ( "STFST01", [ 7, 7, 7 ] ), ( "NEW", [ 9 ] ) ]
                            )
                        )
        , test "a map in a patch takes the place of the base's map, all eleven lumps" <|
            \_ ->
                patched (mapGroup "E1M1" 1 ++ mapGroup "E1M2" 2 ++ [ ( "ENDOOM", [ 5 ] ) ]) (mapGroup "E1M2" 3)
                    |> Expect.equal (Ok ( True, mapGroup "E1M1" 1 ++ mapGroup "E1M2" 3 ++ [ ( "ENDOOM", [ 5 ] ) ] ))
        , test "refuses what isn't a WAD" <|
            \_ ->
                Wad.fromBytes (bytesOf [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ])
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- A TINY MAP


shorts : List Int -> Bytes
shorts list =
    Encode.encode (Encode.sequence (List.map (Encode.signedInt16 LE) list))


data : Data
data =
    { gameMode = Defs.Shareware
    , textures = Array.empty
    , textureNums = Dict.fromList [ ( "WALL", 1 ), ( "DOOR", 2 ) ]
    , flats = Array.empty
    , flatNums = Dict.fromList [ ( "FLOOR", 5 ), ( "CEIL", 7 ) ]
    , skyFlatNum = 0
    , switchList = Array.empty
    , anims = []
    }


{-| Two square rooms side by side, (0,0)-(64,64) and (64,0)-(128,64), sharing the
line x = 64; the blockmap's origin is (-8, -8), one row of two cells.
-}
tinyMap : MapLumps
tinyMap =
    let
        sector floor ceil flat light =
            Encode.sequence
                [ Encode.signedInt16 LE floor, Encode.signedInt16 LE ceil, name8 flat, name8 "CEIL", Encode.signedInt16 LE light, Encode.signedInt16 LE 0, Encode.signedInt16 LE 3 ]

        side sec mid =
            Encode.sequence [ Encode.signedInt16 LE 4, Encode.signedInt16 LE -2, name8 "-", name8 "-", name8 mid, Encode.signedInt16 LE sec ]
    in
    { things = shorts [ 32, 32, 90, 1, 7 ]
    , linedefs =
        -- v1 v2 flags special tag side0 side1
        shorts
            [ 0
            , 1
            , 1
            , 0
            , 0
            , 0
            , -1
            , 1
            , 2
            , 4
            , 1
            , 0
            , 1
            , 2
            , 2
            , 3
            , 1
            , 0
            , 0
            , 3
            , -1
            , 3
            , 0
            , 1
            , 0
            , 0
            , 4
            , -1
            , 1
            , 4
            , 1
            , 0
            , 0
            , 5
            , -1
            , 4
            , 5
            , 1
            , 0
            , 0
            , 6
            , -1
            , 5
            , 2
            , 1
            , 0
            , 0
            , 7
            , -1
            ]
    , sidedefs =
        Encode.encode
            (Encode.sequence
                [ side 0 "WALL", side 0 "-", side 1 "-", side 0 "WALL", side 0 "WALL", side 1 "WALL", side 1 "DOOR", side 1 "WALL" ]
            )
    , vertexes = shorts [ 0, 0, 64, 0, 64, 64, 0, 64, 128, 0, 128, 64 ]
    , segs = shorts [ 0, 1, 0, 0, 0, 0, 1, 2, 16384, 1, 0, 8, 2, 1, -16384, 1, 1, 0 ]
    , ssectors = shorts [ 2, 0, 1, 2 ]
    , nodes = shorts [ 64, 0, 0, 64, 64, 0, 64, 128, 64, 0, 0, 64, 0x8000, 0x8001 ]
    , sectors = Encode.encode (Encode.sequence [ sector 0 128 "FLOOR" 160, sector 8 120 "floor" 200 ])
    , reject = bytesOf [ 0x02 ]
    , blockmap = shorts [ -8, -8, 2, 1, 6, 10, 0, 0, 3, -1, 0, 1, 4, 5, 6, -1 ]
    }


loaded : Result String LevelLoad.Loaded
loaded =
    LevelLoad.fromLumps data "TINY" tinyMap


expectLevel : (LevelLoad.Loaded -> Expect.Expectation) -> () -> Expect.Expectation
expectLevel check _ =
    case loaded of
        Ok l ->
            check l

        Err e ->
            Expect.fail e


levelTests : Test
levelTests =
    let
        f =
            Fixed.fromInt
    in
    describe "Doom.Level.Load"
        [ test "counts" <|
            expectLevel
                (\l ->
                    [ Array.length l.level.vertexes, Array.length l.level.lines, Array.length l.level.sides, Array.length l.level.sectors, Array.length l.level.segs, Array.length l.level.subsectors, Array.length l.level.nodes, List.length l.level.things ]
                        |> Expect.equal [ 6, 7, 8, 2, 3, 2, 1, 1 ]
                )
        , test "a two-sided line's derived values" <|
            expectLevel
                (\l ->
                    Level.line 1 l.level
                        |> Expect.equal
                            { v1 = { x = f 64, y = 0 }
                            , v2 = { x = f 64, y = f 64 }
                            , dx = 0
                            , dy = f 64
                            , flags = 4
                            , tag = 0
                            , frontSide = 1
                            , backSide = Just 2
                            , bbox = { top = f 64, bottom = 0, left = f 64, right = f 64 }
                            , slopetype = Level.Vertical
                            , frontsector = 0
                            , backsector = Just 1
                            }
                )
        , test "slope types" <|
            expectLevel (\l -> List.map (\i -> (Level.line i l.level).slopetype) [ 0, 1, 2 ] |> Expect.equal [ Level.Horizontal, Level.Vertical, Level.Horizontal ])
        , test "line states and side textures" <|
            expectLevel
                (\l ->
                    ( Array.get 1 l.lines, Array.get 6 l.sides |> Maybe.map (\s -> [ s.midtexture, s.toptexture, s.textureoffset, s.rowoffset ]) )
                        |> Expect.equal ( Just { flags = 4, special = 1, validcount = 0 }, Just [ 2, 0, f 4, f -2 ] )
                )
        , test "sector states, flats looked up in upper case" <|
            expectLevel
                (\l ->
                    Array.get 1 l.sectors
                        |> Maybe.map (\s -> ( ( s.floorheight, s.ceilingheight ), ( s.floorpic, s.ceilingpic ), ( s.lightlevel, s.tag ) ))
                        |> Expect.equal (Just ( ( f 8, f 120 ), ( 5, 7 ), ( 200, 3 ) ))
                )
        , test "segs: angle << 16, offset << 16, sides and sectors through the line" <|
            expectLevel
                (\l ->
                    ( Level.seg 1 l.level, Level.seg 2 l.level )
                        |> (\( s1, s2 ) -> ( ( [ s1.angle, s1.offset, s1.frontsector ], s1.backsector ), ( [ s2.angle, s2.sidedef, s2.frontsector ], s2.backsector ) ))
                        |> Expect.equal ( ( [ 0x40000000, f 8, 0 ], Just 1 ), ( [ 0xC0000000, 2, 1 ], Just 0 ) )
                )
        , test "subsectors take their sector from their first seg" <|
            expectLevel (\l -> List.map (\i -> (Level.subsector i l.level).sector) [ 0, 1 ] |> Expect.equal [ 0, 1 ])
        , test "nodes" <|
            expectLevel
                (\l ->
                    Level.node 0 l.level
                        |> (\n -> ( [ n.x, n.dy, Tuple.second n.bbox |> .right ], n.children ))
                        |> Expect.equal ( [ f 64, f 64, f 64 ], ( Level.ChildSubsector 0, Level.ChildSubsector 1 ) )
                )
        , test "sector line lists are in line order, two-sided lines in both" <|
            expectLevel (\l -> List.map (\i -> Array.toList (Level.sector i l.level).lines) [ 0, 1 ] |> Expect.equal [ [ 0, 1, 2, 3 ], [ 1, 4, 5, 6 ] ])
        , test "sector sound origin and blockbox" <|
            expectLevel
                (\l ->
                    Level.sector 1 l.level
                        |> (\s -> ( s.soundorgX, s.soundorgY, s.blockbox ))
                        |> Expect.equal ( f 96, f 32, { top = 0, bottom = 0, left = 0, right = 1 } )
                )
        , test "blockmap: origin, size, cell lists with their leading 0" <|
            expectLevel
                (\l ->
                    ( ( l.level.blockmap.orgx, l.level.blockmap.width, l.level.blockmap.height )
                    , ( Level.blockmapLines 0 0 l.level.blockmap, Level.blockmapLines 1 0 l.level.blockmap )
                    )
                        |> Expect.equal ( ( f -8, 2, 1 ), ( [ 0, 0, 3 ], [ 0, 1, 4, 5, 6 ] ) )
                )
        , test "reject" <|
            expectLevel (\l -> ( Level.rejected 0 1 l.level, Level.rejected 1 0 l.level ) |> Expect.equal ( True, False ))
        , test "things" <|
            expectLevel (\l -> l.level.things |> Expect.equal [ { x = 32, y = 32, angle = 90, type_ = 1, options = 7 } ])
        ]



-- DEMOS


demoBytes : Bytes
demoBytes =
    bytesOf
        ([ 109, 2, 1, 6, 0, 0, 1, 0, 0, 1, 0, 0, 0 ]
            ++ [ 25, 0xE8, 0x80, 0x81 ]
            ++ [ 0xCE, 24, 3, 2 ]
            ++ [ 0x80 ]
        )


demoTests : Test
demoTests =
    describe "Doom.Demo"
        [ test "header" <|
            \_ ->
                Demo.fromBytes demoBytes
                    |> Result.map Demo.header
                    |> Result.map (\h -> ( ( [ h.version, h.episode, h.map ], h.skill ), ( h.fastparm, h.respawnparm, h.consoleplayer ), Array.toList h.playeringame ))
                    |> Expect.equal (Ok ( ( [ 109, 1, 6 ], Defs.Medium ), ( True, False, 0 ), [ True, False, False, False ] ))
        , test "ticcmds, then the end marker" <|
            \_ ->
                case Demo.fromBytes demoBytes of
                    Ok demo ->
                        case Demo.readTiccmd Ticcmd.empty demo of
                            Just ( c1, d1 ) ->
                                case Demo.readTiccmd Ticcmd.empty d1 of
                                    Just ( c2, d2 ) ->
                                        ( [ c1.forwardmove, c1.sidemove, c1.angleturn, c1.buttons ]
                                        , [ c2.forwardmove, c2.sidemove, c2.angleturn, c2.buttons ]
                                        , Demo.readTiccmd Ticcmd.empty d2 == Nothing
                                        )
                                            |> Expect.equal ( [ 25, -24, -32768, 0x81 ], [ -50, 24, 768, 2 ], True )

                                    Nothing ->
                                        Expect.fail "no second tic"

                            Nothing ->
                                Expect.fail "no first tic"

                    Err e ->
                        Expect.fail e
        , test "other versions are refused" <|
            \_ ->
                Demo.fromBytes (bytesOf [ 108, 2, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0x80 ])
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- INPUT


inputTests : Test
inputTests =
    let
        none =
            Input.none
    in
    describe "Doom.Input"
        [ test "running forward and turning right, at first slowly" <|
            \_ ->
                Input.buildTiccmd { none | forward = True, run = True, turnRight = True } Input.notTurning
                    |> Tuple.mapFirst (\c -> ( c.forwardmove, c.angleturn ))
                    |> Expect.equal ( ( 0x32, -320 ), 1 )
        , test "turning speeds up after SLOWTURNTICS" <|
            \_ ->
                Input.buildTiccmd { none | turnLeft = True } 5
                    |> Tuple.first
                    |> .angleturn
                    |> Expect.equal 640
        , test "strafing with the modifier, capped" <|
            \_ ->
                Input.buildTiccmd { none | strafe = True, turnLeft = True, mouseX = -100 } 0
                    |> Tuple.first
                    |> .sidemove
                    |> Expect.equal -0x32
        , test "fire, use and a weapon key" <|
            \_ ->
                Input.buildTiccmd { none | fire = True, use = True, weapon = Just 3 } 0
                    |> Tuple.first
                    |> .buttons
                    |> Expect.equal (1 + 2 + 4 + 2 * 8)
        ]
