module Doom.Render.Compose exposing (Columns, Op(..), apply, emptyColumns, fuzzTable, push)

{-| Putting the view's columns together. The walk (`Doom.Render.Walk`) records, for
each screen column, every write the original makes to it, in order: runs of pixels
from R\_DrawColumn, R\_DrawSpan and friends, and R\_DrawFuzzColumn's runs, which
darken what is already there. `apply` then replays them over the framebuffer's
columns.

Walls, skies and flats normally cover each pixel once (the original's clip arrays see
to that), so a column's solid runs are usually laid side by side in one pass; where
they overlap (low detail's spans run past their end) they are replayed in order.

-}

import Array exposing (Array)
import Doom.Video.Internal as VI exposing (Framebuffer(..))


{-| A write to one screen column: `Run top pixels` sets the pixels from row `top`
down; `Fuzz top bottom pos` is R\_DrawFuzzColumn over those rows, starting at
`fuzzpos` = pos.
-}
type Op
    = Run Int (List Int)
    | Fuzz Int Int Int


{-| For each of the 320 screen columns, its writes, the latest first: those before
R\_DrawMasked (walls, planes) and those after (sprites, masked walls, the weapon).
-}
type alias Columns =
    { solid : Array (List Op)
    , masked : Array (List Op)
    }


{-| No writes.
-}
emptyColumns : Columns
emptyColumns =
    { solid = Array.repeat VI.columnCount [], masked = Array.repeat VI.columnCount [] }


{-| Add a write to a column's list.
-}
push : Int -> Op -> Array (List Op) -> Array (List Op)
push x op ops =
    case Array.get x ops of
        Just l ->
            Array.set x (op :: l) ops

        Nothing ->
            ops


{-| fuzzoffset[] in rows: +1 is FUZZOFF (the pixel below), -1 the one above.
-}
fuzzTable : Array Int
fuzzTable =
    Array.fromList
        [ 1, -1, 1, -1, 1, 1, -1, 1, 1, -1, 1, 1, 1, -1, 1, 1, 1, -1, -1, -1, -1, 1, -1, -1, 1, 1, 1, 1, -1, 1, -1, 1, 1, -1, -1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1, -1, 1, 1, -1, 1 ]


{-| Replay the writes over the framebuffer. `fuzzmap` is colormap 6, which
R\_DrawFuzzColumn darkens with.
-}
apply : Array Int -> Columns -> Framebuffer -> Framebuffer
apply fuzzmap columns (Framebuffer cols) =
    Framebuffer
        (Array.indexedMap
            (\x col ->
                case ( Array.get x columns.solid |> Maybe.withDefault [], Array.get x columns.masked |> Maybe.withDefault [] ) of
                    ( [], [] ) ->
                        col

                    ( solid, masked ) ->
                        List.foldr (applyMasked fuzzmap) (solidColumn solid col) masked
            )
            cols
        )


type alias Placed =
    { top : Int
    , length : Int
    , pixels : List Int
    }


{-| Lay the solid runs (latest first) over a column.
-}
solidColumn : List Op -> Array Int -> Array Int
solidColumn newestFirst old =
    case newestFirst of
        [] ->
            old

        _ ->
            let
                placed =
                    List.filterMap
                        (\op ->
                            case op of
                                Run top px ->
                                    Just { top = top, length = List.length px, pixels = px }

                                Fuzz _ _ _ ->
                                    Nothing
                        )
                        newestFirst

                descending =
                    List.sortBy (\p -> -p.top) placed
            in
            if disjoint VI.rowCount descending then
                case contiguous descending of
                    Just ( top, bottom, pixels ) ->
                        -- the usual case: the view's rows in one piece, the rest
                        -- of the column as it was
                        Array.append (Array.append (Array.slice 0 top old) (Array.fromList pixels)) (Array.slice bottom VI.rowCount old)

                    Nothing ->
                        Array.fromList (layDown old VI.rowCount descending [])

            else
                List.foldr (\p col -> spliceRun p.top p.pixels col) old placed


{-| Whether runs sorted from the bottom up lie inside the column without touching
each other.
-}
disjoint : Int -> List Placed -> Bool
disjoint end runs =
    case runs of
        [] ->
            True

        p :: rest ->
            p.top >= 0 && p.top + p.length <= end && disjoint p.top rest


{-| Runs (bottom up, disjoint) that leave no gap between them: the rows they cover
and their pixels from the top.
-}
contiguous : List Placed -> Maybe ( Int, Int, List Int )
contiguous runs =
    case runs of
        [] ->
            Nothing

        last :: _ ->
            let
                go top rest acc =
                    case rest of
                        [] ->
                            Just ( top, last.top + last.length, acc )

                        p :: more ->
                            if p.top + p.length == top then
                                go p.top more (p.pixels ++ acc)

                            else
                                Nothing
            in
            case runs of
                p :: more ->
                    go p.top more p.pixels

                [] ->
                    Nothing


{-| The column from row 0 to `end - 1`: the runs (bottom up) with the old pixels in
the gaps.
-}
layDown : Array Int -> Int -> List Placed -> List Int -> List Int
layDown old end runs acc =
    case runs of
        [] ->
            oldRows old 0 (end - 1) acc

        p :: rest ->
            layDown old p.top rest (p.pixels ++ oldRows old (p.top + p.length) (end - 1) acc)


oldRows : Array Int -> Int -> Int -> List Int -> List Int
oldRows old from to acc =
    if to < from then
        acc

    else
        oldRows old from (to - 1) ((Array.get to old |> Maybe.withDefault 0) :: acc)


{-| `setRun` for a run that may be long (a sprite's or masked wall's post): cut
into the column in one piece when it lies on the screen.
-}
spliceRun : Int -> List Int -> Array Int -> Array Int
spliceRun top pixels col =
    let
        length =
            List.length pixels
    in
    if length > 6 && top >= 0 && top + length <= VI.rowCount then
        VI.splice top (top + length) (Array.fromList pixels) col

    else
        setRun top pixels col


setRun : Int -> List Int -> Array Int -> Array Int
setRun y pixels col =
    case pixels of
        [] ->
            col

        c :: rest ->
            setRun (y + 1) rest (Array.set y c col)


applyMasked : Array Int -> Op -> Array Int -> Array Int
applyMasked fuzzmap op col =
    case op of
        Run top px ->
            spliceRun top px col

        Fuzz top bottom pos ->
            fuzz fuzzmap top bottom pos col


{-| R\_DrawFuzzColumn: each pixel becomes the one above or below it (by the fuzz
table), darkened; it reads the screen as it goes, so a pixel can pick up the one just
written above it.
-}
fuzz : Array Int -> Int -> Int -> Int -> Array Int -> Array Int
fuzz fuzzmap y bottom pos col =
    if y > bottom then
        col

    else
        let
            from =
                y + (Array.get pos fuzzTable |> Maybe.withDefault 1)

            c =
                Array.get (Array.get from col |> Maybe.withDefault 0) fuzzmap |> Maybe.withDefault 0

            next =
                if pos + 1 == 50 then
                    0

                else
                    pos + 1
        in
        fuzz fuzzmap (y + 1) bottom next (Array.set y c col)
