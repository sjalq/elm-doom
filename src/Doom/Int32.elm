module Doom.Int32 exposing
    ( wrap, add, sub, mul, neg, abs
    , toUnsigned, shiftLeft, shiftRight, shiftRightUnsigned, floorDiv
    , toByte, toSignedByte, toShort, toUnsignedShort
    , min, max
    )

{-| C's 32-bit `int` arithmetic, giving the same bits under every Elm backend.

Doom's results depend on 32-bit wrap-around (angles wrap all the time, and a few
calculations overflow on purpose). Elm's `Int` doesn't promise a width: compiled to
JavaScript it is a double, exact to 2^53, whose `Bitwise` operations work on 32 bits;
compiled natively by eco it is 64 bits, and so are its `Bitwise` operations. The
functions here only rely on what both agree on: `+`, `-`, `*` and `modBy` while the
values stay below 2^53, `//` on exact multiples, and `Bitwise.shiftRightBy` on values
already in the signed 32-bit range.

A value is "an int32" when it is in [-2^31, 2^31). Every function returns one, given
int32s, except `toUnsigned` and `shiftRightUnsigned`, whose results are in [0, 2^32)
(C's `unsigned int`, which `Doom.Angle` uses).


# Arithmetic

@docs wrap, add, sub, mul, neg, abs


# Bits and conversions

@docs toUnsigned, shiftLeft, shiftRight, shiftRightUnsigned, floorDiv
@docs toByte, toSignedByte, toShort, toUnsignedShort


# Comparisons

@docs min, max

-}

import Bitwise


two32 : Int
two32 =
    4294967296


two31 : Int
two31 =
    2147483648


{-| Wrap any integer below 2^53 in size to an int32, as a C cast to `int` would.
-}
wrap : Int -> Int
wrap x =
    modBy two32 (x + two31) - two31


{-| `a + b` with C overflow.
-}
add : Int -> Int -> Int
add a b =
    wrap (a + b)


{-| `a - b` with C overflow.
-}
sub : Int -> Int -> Int
sub a b =
    wrap (a - b)


{-| `-a` with C overflow (the negation of -2^31 is itself).
-}
neg : Int -> Int
neg a =
    wrap -a


{-| C's `abs`: the absolute value, except that abs(-2^31) is -2^31.
-}
abs : Int -> Int
abs a =
    wrap (Basics.abs a)


{-| `a * b` with C overflow: the low 32 bits of the product. Exact on every backend,
since it never forms a product bigger than 2^48.
-}
mul : Int -> Int -> Int
mul a b =
    let
        bLow =
            modBy 65536 b

        bHigh =
            (b - bLow) // 65536
    in
    wrap (wrap (a * bHigh) * 65536 + a * bLow)


{-| The same 32 bits read as C's `unsigned int`, in [0, 2^32).
-}
toUnsigned : Int -> Int
toUnsigned x =
    modBy two32 x


{-| `x << n` for 0 <= n < 32, with C overflow. Works on int32s and unsigned values alike
(the result is an int32; use `toUnsigned` for the unsigned reading).
-}
shiftLeft : Int -> Int -> Int
shiftLeft n x =
    mul x (2 ^ n)


{-| C's `x >> n` on a signed int: rounds towards minus infinity.
-}
shiftRight : Int -> Int -> Int
shiftRight n x =
    Bitwise.shiftRightBy n (wrap x)


{-| C's `x >> n` on an unsigned int, for x in [0, 2^32) (or any int32, read as unsigned).
-}
shiftRightUnsigned : Int -> Int -> Int
shiftRightUnsigned n x =
    let
        u =
            toUnsigned x
    in
    (u - modBy (2 ^ n) u) // (2 ^ n)


{-| `n / d` rounded towards minus infinity, for d > 0 (C's `/` on ints truncates towards
zero: that is Elm's `//`).
-}
floorDiv : Int -> Int -> Int
floorDiv n d =
    (n - modBy d n) // d


{-| C's cast to `unsigned char` (`byte`): the low 8 bits.
-}
toByte : Int -> Int
toByte x =
    modBy 256 x


{-| C's cast to `signed char`: the low 8 bits, as -128 to 127.
-}
toSignedByte : Int -> Int
toSignedByte x =
    modBy 256 (x + 128) - 128


{-| C's cast to `short`: the low 16 bits, as -32768 to 32767.
-}
toShort : Int -> Int
toShort x =
    modBy 65536 (x + 32768) - 32768


{-| C's cast to `unsigned short`: the low 16 bits.
-}
toUnsignedShort : Int -> Int
toUnsignedShort x =
    modBy 65536 x


{-| The smaller.
-}
min : Int -> Int -> Int
min =
    Basics.min


{-| The bigger.
-}
max : Int -> Int -> Int
max =
    Basics.max
