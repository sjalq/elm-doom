module RenderCheck.Wad exposing (Lump, Wad, afterMarker, between, lookup, parse, readData)

{-| A minimal WAD reader, private to this tool (the package's real one is
`Doom.Wad`): the lump directory, lumps by name, the lumps between markers, and the
texture tables `Doom.Data` needs.
-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode
import Dict exposing (Dict)
import Doom.Data exposing (Data, Texture)
import Doom.Defs exposing (GameMode(..))


type alias Lump =
    { name : String
    , bytes : Bytes
    }


type alias Wad =
    { lumps : Array Lump
    , byName : Dict String Bytes
    }


type alias Entry =
    { index : Int, pos : Int, size : Int, name : String }


i32 : Decoder Int
i32 =
    Decode.signedInt32 LE


name8 : Decoder String
name8 =
    Decode.string 8
        |> Decode.map
            (\s ->
                s
                    |> String.toList
                    |> List.filter (\c -> c /= '\u{0000}')
                    |> String.fromList
                    |> String.toUpper
            )


{-| The lumps, read in one pass over the file in position order.
-}
parse : Bytes -> Maybe Wad
parse bytes =
    let
        header =
            Decode.map3 (\_ n ofs -> ( n, ofs )) (Decode.string 4) i32 i32
    in
    Decode.decode header bytes
        |> Maybe.andThen
            (\( count, dirOfs ) ->
                let
                    entry =
                        Decode.map3 (\pos size n -> { pos = pos, size = size, name = n }) i32 i32 name8

                    dir =
                        Decode.map2 (\_ es -> es)
                            (Decode.bytes dirOfs)
                            (Decode.loop ( count, [] )
                                (\( left, acc ) ->
                                    if left == 0 then
                                        Decode.succeed (Decode.Done (List.reverse acc))

                                    else
                                        Decode.map (\e -> Decode.Loop ( left - 1, e :: acc )) entry
                                )
                            )
                in
                Decode.decode dir bytes
            )
        |> Maybe.map
            (\entries ->
                let
                    indexed =
                        List.indexedMap (\i e -> { index = i, pos = e.pos, size = e.size, name = e.name }) entries

                    contents =
                        readAll bytes (List.sortBy .pos indexed)

                    lumps =
                        indexed
                            |> List.map (\e -> { name = e.name, bytes = Dict.get e.index contents |> Maybe.withDefault empty })
                            |> Array.fromList
                in
                { lumps = lumps
                , byName = Array.foldl (\l d -> Dict.insert l.name l.bytes d) Dict.empty lumps
                }
            )


empty : Bytes
empty =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


{-| The lumps' bytes by directory index, reading the file once from start to end.
-}
readAll : Bytes -> List Entry -> Dict Int Bytes
readAll bytes sorted =
    let
        step ( cursor, left, acc ) =
            case left of
                [] ->
                    Decode.succeed (Decode.Done acc)

                e :: rest ->
                    if e.size == 0 then
                        Decode.succeed (Decode.Loop ( cursor, rest, Dict.insert e.index empty acc ))

                    else if e.pos < cursor then
                        -- Overlaps the one before: read it on its own.
                        Decode.succeed
                            (Decode.Loop
                                ( cursor
                                , rest
                                , Dict.insert e.index (slice bytes e.pos e.size) acc
                                )
                            )

                    else
                        Decode.map2 (\_ b -> Decode.Loop ( e.pos + e.size, rest, Dict.insert e.index b acc ))
                            (Decode.bytes (e.pos - cursor))
                            (Decode.bytes e.size)
    in
    Decode.decode (Decode.loop ( 0, sorted, Dict.empty ) step) bytes
        |> Maybe.withDefault Dict.empty


slice : Bytes -> Int -> Int -> Bytes
slice bytes pos size =
    Decode.decode (Decode.map2 (\_ b -> b) (Decode.bytes pos) (Decode.bytes size)) bytes
        |> Maybe.withDefault empty


lookup : Wad -> String -> Maybe Bytes
lookup wad n =
    Dict.get (String.toUpper n) wad.byName


{-| The names of the lumps strictly between the markers (as R\_InitFlats and
R\_InitSpriteLumps count them, inner markers included).
-}
between : Wad -> String -> String -> List Lump
between wad start end =
    let
        lumps =
            Array.toList wad.lumps

        afterStart =
            dropUntil (\l -> l.name == start) lumps
    in
    takeWhile (\l -> l.name /= end) afterStart


{-| The `n` lumps after the marker `name` (a map's lumps).
-}
afterMarker : Wad -> String -> Int -> List Lump
afterMarker wad n count =
    Array.toList wad.lumps
        |> dropUntil (\l -> l.name == n)
        |> List.take count


dropUntil : (a -> Bool) -> List a -> List a
dropUntil f l =
    case l of
        x :: rest ->
            if f x then
                rest

            else
                dropUntil f rest

        [] ->
            []


takeWhile : (a -> Bool) -> List a -> List a
takeWhile f l =
    case l of
        x :: rest ->
            if f x then
                x :: takeWhile f rest

            else
                []

        [] ->
            []



-- DATA


{-| `Doom.Data` from PNAMES, TEXTURE1, TEXTURE2 and the flats (no animations or
switches: this tool doesn't play).
-}
readData : Wad -> Data
readData wad =
    let
        pnames =
            lookup wad "PNAMES"
                |> Maybe.andThen (Decode.decode (countedList i32 name8))
                |> Maybe.withDefault []
                |> Array.fromList

        textures =
            [ "TEXTURE1", "TEXTURE2" ]
                |> List.filterMap (lookup wad)
                |> List.concatMap (readTextures pnames)

        flats =
            between wad "F_START" "F_END" |> List.map .name

        flatNums =
            flats |> List.indexedMap (\i n -> ( n, i )) |> Dict.fromList
    in
    { gameMode = Retail
    , textures = Array.fromList textures
    , textureNums = textures |> List.indexedMap (\i t -> ( t.name, i )) |> List.reverse |> Dict.fromList
    , flats = Array.fromList flats
    , flatNums = flatNums
    , skyFlatNum = Dict.get "F_SKY1" flatNums |> Maybe.withDefault -1
    , switchList = Array.empty
    , anims = []
    }


countedList : Decoder Int -> Decoder a -> Decoder (List a)
countedList count item =
    count
        |> Decode.andThen
            (\n ->
                Decode.loop ( n, [] )
                    (\( left, acc ) ->
                        if left <= 0 then
                            Decode.succeed (Decode.Done (List.reverse acc))

                        else
                            Decode.map (\x -> Decode.Loop ( left - 1, x :: acc )) item
                    )
            )


{-| A TEXTURE1/TEXTURE2 lump: a count, offsets, then `maptexture_t`s.
-}
readTextures : Array String -> Bytes -> List Texture
readTextures pnames lump =
    let
        offsets =
            Decode.decode (countedList i32 i32) lump |> Maybe.withDefault []

        s16 =
            Decode.signedInt16 LE

        patch =
            Decode.map5 (\ox oy p _ _ -> { originX = ox, originY = oy, patch = Array.get p pnames |> Maybe.withDefault "" })
                s16
                s16
                s16
                s16
                s16

        texture =
            Decode.map5 (\n _ w h _ -> ( n, w, h ))
                name8
                i32
                s16
                s16
                i32
                |> Decode.andThen
                    (\( n, w, h ) ->
                        countedList s16 patch
                            |> Decode.map (\ps -> { name = n, width = w, height = h, patches = ps })
                    )
    in
    List.filterMap
        (\ofs -> Decode.decode (Decode.map2 (\_ t -> t) (Decode.bytes ofs) texture) lump)
        offsets
