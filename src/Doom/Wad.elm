module Doom.Wad exposing
    ( Wad, Kind(..), Lump, fromBytes, kind
    , numLumps, lumpAt, checkNumForName, lump
    , between, MapLumps, mapLumps
    , patch, toBytes
    , name8
    )

{-| A WAD file (w\_wad.c): a directory of named lumps.

A WAD starts with a 12-byte header ("IWAD" or "PWAD", the number of lumps and where
the directory is), and its directory has 16 bytes per lump: where the lump's data
starts, how long it is, and its name (8 characters, padded with zeroes).

`fromBytes` reads the directory and cuts every lump out in a single pass over the
file, in file order, so that no lump read goes back over the bytes.

    case Wad.fromBytes bytes of
        Ok wad ->
            Wad.lump "PLAYPAL" wad

        Err problem ->
            Nothing

@docs Wad, Kind, Lump, fromBytes, kind
@docs numLumps, lumpAt, checkNumForName, lump
@docs between, MapLumps, mapLumps
@docs patch, toBytes
@docs name8

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode
import Dict exposing (Dict)


{-| A loaded WAD: its lumps, in directory order (a lump's number is its position).
-}
type Wad
    = Wad
        { kind : Kind
        , lumps : Array Lump
        , numsByName : Dict String Int
        }


{-| An IWAD (a whole game) or a PWAD (a patch to one).
-}
type Kind
    = Iwad
    | Pwad


{-| A lump: its name as the directory gives it and its data.
-}
type alias Lump =
    { name : String
    , bytes : Bytes
    }


type alias Entry =
    { index : Int
    , filepos : Int
    , size : Int
    , name : String
    }


{-| Read a WAD file. Fails if the header isn't IWAD or PWAD, or the directory or a lump
runs past the end of the file.
-}
fromBytes : Bytes -> Result String Wad
fromBytes bytes =
    case Decode.decode header bytes of
        Nothing ->
            Err "not a WAD file: too short for its header"

        Just ( identification, numlumps, infotableofs ) ->
            let
                theKind =
                    case identification of
                        "IWAD" ->
                            Ok Iwad

                        "PWAD" ->
                            Ok Pwad

                        other ->
                            Err ("not a WAD file: it starts with " ++ other ++ ", not IWAD or PWAD")
            in
            theKind
                |> Result.andThen
                    (\k ->
                        case Decode.decode (skip infotableofs |> Decode.andThen (\_ -> directory numlumps)) bytes of
                            Nothing ->
                                Err "the WAD's directory runs past the end of the file"

                            Just entries ->
                                cutLumps bytes entries
                                    |> Result.map
                                        -- later lumps overwrite earlier ones in numsByName: the last one wins
                                        (fromLumps k)
                    )


{-| Whether it is an IWAD or a PWAD.
-}
kind : Wad -> Kind
kind (Wad w) =
    w.kind


{-| How many lumps the WAD has.
-}
numLumps : Wad -> Int
numLumps (Wad w) =
    Array.length w.lumps


{-| The lump with this number.
-}
lumpAt : Int -> Wad -> Maybe Lump
lumpAt i (Wad w) =
    Array.get i w.lumps


{-| W\_CheckNumForName: the number of the last lump with this name (compared in upper
case, up to 8 characters), so a lump added later replaces an earlier one.
-}
checkNumForName : String -> Wad -> Maybe Int
checkNumForName name (Wad w) =
    Dict.get (String.toUpper (String.left 8 name)) w.numsByName


{-| The data of the last lump with this name (W\_CacheLumpName).
-}
lump : String -> Wad -> Maybe Bytes
lump name wad =
    checkNumForName name wad
        |> Maybe.andThen (\i -> lumpAt i wad)
        |> Maybe.map .bytes


{-| The lumps strictly between two marker lumps, such as `between "F_START" "F_END"`
for the flats or `"S_START" "S_END"` for the sprites (the last marker of each name,
as W\_GetNumForName finds them). Inner markers (F1\_START...) are included, as the C
source counts them. Empty if either marker is missing.
-}
between : String -> String -> Wad -> List Lump
between start end ((Wad w) as wad) =
    case ( checkNumForName start wad, checkNumForName end wad ) of
        ( Just first, Just last ) ->
            Array.slice (first + 1) last w.lumps |> Array.toList

        _ ->
            []


{-| A map's lumps, which follow its marker lump (E1M1, MAP01...) in this order
(ML\_THINGS ... ML\_BLOCKMAP in doomdata.h).
-}
type alias MapLumps =
    { things : Bytes
    , linedefs : Bytes
    , sidedefs : Bytes
    , vertexes : Bytes
    , segs : Bytes
    , ssectors : Bytes
    , nodes : Bytes
    , sectors : Bytes
    , reject : Bytes
    , blockmap : Bytes
    }


{-| The lumps of the map with this marker name. As in P\_SetupLevel, they are found by
position after the marker; their names aren't checked.
-}
mapLumps : String -> Wad -> Maybe MapLumps
mapLumps name wad =
    checkNumForName name wad
        |> Maybe.andThen
            (\n ->
                let
                    at i =
                        lumpAt (n + i) wad |> Maybe.map .bytes
                in
                Maybe.map5 (\a b c d e -> MapLumps a b c d e)
                    (at 1)
                    (at 2)
                    (at 3)
                    (at 4)
                    (at 5)
                    |> Maybe.andThen
                        (\partial ->
                            Maybe.map5 (\f g h i j -> partial f g h i j)
                                (at 6)
                                (at 7)
                                (at 8)
                                (at 9)
                                (at 10)
                        )
            )



-- PATCHING


{-| The `base` WAD with a patch WAD's lumps put into it, the way a WAD editor merges
one into another: each of the patch's lumps takes the place of the base's lump of that
name (the last one, as `checkNumForName` finds it), and a lump the base hasn't got
goes at the end. A map in the patch (its marker, E1M1 or MAP01, and the ten lumps
after it) takes the place of the base's map of that name, all eleven lumps. The
result is the base's kind (an IWAD, for a patched IWAD).

Replacing in place is what makes a patch of flats work: the original finds a flat by
its place between F\_START and F\_END, so a new flat added at the end of the WAD,
as `-file` adds a PWAD's lumps, would be outside them. The shareware game refuses
`-file` altogether ("You cannot -file with the shareware version"), so a patched
copy is the only way to change its graphics.

-}
patch : Wad -> Wad -> Wad
patch (Wad p) (Wad base) =
    let
        go i lumps =
            case Array.get i p.lumps of
                Nothing ->
                    lumps

                Just l ->
                    let
                        isMap =
                            isMapName l.name && i + 10 < Array.length p.lumps

                        count =
                            if isMap then
                                11

                            else
                                1

                        replacement =
                            Array.slice i (i + count) p.lumps

                        at =
                            checkNumForName l.name (fromLumps base.kind lumps)
                    in
                    go (i + count)
                        (case at of
                            Just n ->
                                Array.append (Array.slice 0 n lumps)
                                    (Array.append replacement (Array.slice (n + count) (Array.length lumps) lumps))

                            Nothing ->
                                Array.append lumps replacement
                        )
    in
    fromLumps base.kind (go 0 base.lumps)


{-| E\\dM\\d or MAP\\d\\d: the marker of a map.
-}
isMapName : String -> Bool
isMapName name =
    case String.toList (String.toUpper name) of
        [ 'E', e, 'M', m ] ->
            Char.isDigit e && Char.isDigit m

        [ 'M', 'A', 'P', a, b ] ->
            Char.isDigit a && Char.isDigit b

        _ ->
            False


fromLumps : Kind -> Array Lump -> Wad
fromLumps k lumps =
    Wad
        { kind = k
        , lumps = lumps
        , numsByName =
            Array.foldl
                (\l ( i, d ) -> ( i + 1, Dict.insert (String.toUpper l.name) i d ))
                ( 0, Dict.empty )
                lumps
                |> Tuple.second
        }


{-| The WAD as a file: the header, every lump's data in directory order, then the
directory.
-}
toBytes : Wad -> Bytes
toBytes (Wad w) =
    let
        lumps =
            Array.toList w.lumps

        dataSize =
            List.foldl (\l n -> n + Bytes.width l.bytes) 0 lumps

        entries =
            List.foldl
                (\l ( pos, acc ) ->
                    ( pos + Bytes.width l.bytes
                    , Encode.sequence
                        [ Encode.signedInt32 LE pos
                        , Encode.signedInt32 LE (Bytes.width l.bytes)
                        , encodeName 8 l.name
                        ]
                        :: acc
                    )
                )
                ( 12, [] )
                lumps
                |> Tuple.second
                |> List.reverse
    in
    Encode.encode
        (Encode.sequence
            (encodeName 4
                (case w.kind of
                    Iwad ->
                        "IWAD"

                    Pwad ->
                        "PWAD"
                )
                :: Encode.signedInt32 LE (List.length lumps)
                :: Encode.signedInt32 LE (12 + dataSize)
                :: List.map (\l -> Encode.bytes l.bytes) lumps
                ++ entries
            )
        )


{-| A name in `n` bytes, padded with zeroes (Latin-1, as `fixedName` reads it).
-}
encodeName : Int -> String -> Encode.Encoder
encodeName n name =
    let
        codes =
            List.map Char.toCode (String.toList (String.left n name))
    in
    Encode.sequence (List.map Encode.unsignedInt8 (codes ++ List.repeat (n - List.length codes) 0))



-- DECODING


header : Decoder ( String, Int, Int )
header =
    Decode.map3 (\a b c -> ( a, b, c ))
        (fixedName 4)
        (Decode.signedInt32 LE)
        (Decode.signedInt32 LE)


directory : Int -> Decoder (List Entry)
directory count =
    Decode.loop ( 0, [] )
        (\( i, acc ) ->
            if i >= count then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map3 (\pos size n -> Decode.Loop ( i + 1, Entry i pos size n :: acc ))
                    (Decode.signedInt32 LE)
                    (Decode.signedInt32 LE)
                    name8
        )


{-| An 8-character name as WADs store them: bytes up to the first zero, read as
Latin-1 (C's `char[8]`).
-}
name8 : Decoder String
name8 =
    fixedName 8


fixedName : Int -> Decoder String
fixedName n =
    Decode.loop ( 0, [], False )
        (\( i, acc, ended ) ->
            if i >= n then
                Decode.succeed (Decode.Done (String.fromList (List.reverse acc)))

            else
                Decode.unsignedInt8
                    |> Decode.map
                        (\b ->
                            if ended || b == 0 then
                                Decode.Loop ( i + 1, acc, True )

                            else
                                Decode.Loop ( i + 1, Char.fromCode b :: acc, False )
                        )
        )


skip : Int -> Decoder ()
skip n =
    if n <= 0 then
        Decode.succeed ()

    else
        Decode.bytes n |> Decode.map (\_ -> ())


{-| Cut every lump out of the file: one pass through it in file order. A lump whose
data starts before the end of the one before it (lumps may share data) is cut out
separately afterwards.
-}
cutLumps : Bytes -> List Entry -> Result String (Array Lump)
cutLumps bytes entries =
    let
        ( empty, nonEmpty ) =
            List.partition (\e -> e.size <= 0) entries

        sorted =
            List.sortBy .filepos nonEmpty

        pass =
            Decode.loop ( 0, sorted, [] )
                (\( pos, rest, acc ) ->
                    case rest of
                        [] ->
                            Decode.succeed (Decode.Done acc)

                        e :: more ->
                            if e.filepos < pos then
                                -- overlaps the lump before: cut out on its own below
                                Decode.succeed (Decode.Loop ( pos, more, ( e, Nothing ) :: acc ))

                            else
                                skip (e.filepos - pos)
                                    |> Decode.andThen (\_ -> Decode.bytes e.size)
                                    |> Decode.map (\b -> Decode.Loop ( e.filepos + e.size, more, ( e, Just b ) :: acc ))
                )

        emptyBytes =
            Decode.decode (Decode.bytes 0) bytes
    in
    case ( Decode.decode pass bytes, emptyBytes ) of
        ( Just cut, Just nothing ) ->
            let
                resolved =
                    List.map
                        (\( e, mb ) ->
                            case mb of
                                Just b ->
                                    Ok ( e, b )

                                Nothing ->
                                    Decode.decode (skip e.filepos |> Decode.andThen (\_ -> Decode.bytes e.size)) bytes
                                        |> Maybe.map (\b -> ( e, b ))
                                        |> Result.fromMaybe ("lump " ++ e.name ++ " runs past the end of the file")
                        )
                        cut
                        ++ List.map (\e -> Ok ( e, nothing )) empty
            in
            List.foldr (Result.map2 (::)) (Ok []) resolved
                |> Result.map
                    (\pairs ->
                        pairs
                            |> List.sortBy (\( e, _ ) -> e.index)
                            |> List.map (\( e, b ) -> { name = e.name, bytes = b })
                            |> Array.fromList
                    )

        _ ->
            Err "a lump runs past the end of the WAD file"
