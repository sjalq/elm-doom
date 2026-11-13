module Doom.Fixed exposing
    ( Fixed, fracbits, fracunit, fromInt, toInt
    , mul, div, add, sub
    , maxInt, minInt
    )

{-| Doom's 16.16 fixed-point numbers (`fixed_t`, m\_fixed.c): an int32 whose low 16
bits are the fraction, so `fracunit` (65536) is 1.0.

`Fixed` is an alias for `Int` so the game code reads like the C source and costs
nothing; every value is an int32 (see `Doom.Int32`).

@docs Fixed, fracbits, fracunit, fromInt, toInt
@docs mul, div, add, sub
@docs maxInt, minInt

-}

import Doom.Int32 as Int32


{-| A 16.16 fixed-point number.
-}
type alias Fixed =
    Int


{-| FRACBITS.
-}
fracbits : Int
fracbits =
    16


{-| FRACUNIT: 1.0.
-}
fracunit : Fixed
fracunit =
    65536


{-| MAXINT, C's biggest `int`.
-}
maxInt : Int
maxInt =
    2147483647


{-| MININT, C's smallest `int`.
-}
minInt : Int
minInt =
    -2147483648


{-| `n << FRACBITS`.
-}
fromInt : Int -> Fixed
fromInt n =
    Int32.shiftLeft fracbits n


{-| `x >> FRACBITS`: the whole part, rounded towards minus infinity.
-}
toInt : Fixed -> Int
toInt x =
    Int32.shiftRight fracbits x


{-| `a + b`, wrapping like C.
-}
add : Fixed -> Fixed -> Fixed
add =
    Int32.add


{-| `a - b`, wrapping like C.
-}
sub : Fixed -> Fixed -> Fixed
sub =
    Int32.sub


{-| FixedMul: `((long long) a * b) >> FRACBITS`, cast to `int`.

The 64-bit product is formed in two halves, so this is exact even where Elm's Int is a
double.

-}
mul : Fixed -> Fixed -> Fixed
mul a b =
    let
        bLow =
            modBy 65536 b

        bHigh =
            (b - bLow) // 65536

        low =
            a * bLow
    in
    Int32.wrap (a * bHigh + Int32.floorDiv low 65536)


{-| FixedDiv: `a / b` in fixed point, saturating to MAXINT or MININT (by the sign of
a ^ b) when the result wouldn't fit, including b = 0. Like the C source, it divides
in double precision and truncates towards zero.
-}
div : Fixed -> Fixed -> Fixed
div a b =
    if Int32.shiftRight 14 (Int32.abs a) >= Int32.abs b then
        if (a < 0) /= (b < 0) then
            minInt

        else
            maxInt

    else
        Int32.wrap (truncate (toFloat a / toFloat b * toFloat fracunit))
