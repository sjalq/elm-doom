module FoundationTest exposing (suite)

{-| The exact-arithmetic foundation: every other module trusts these to give C's
32-bit results. Each property checks a function against an independent
implementation that works on 16-bit limbs, so neither can hide the other's mistake.
-}

import Doom.Angle as Angle
import Doom.Fixed as Fixed
import Doom.Int32 as Int32
import Doom.Random as Random
import Doom.StateHash as StateHash
import Doom.Tables as Tables
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


int32 : Fuzzer Int
int32 =
    Fuzz.oneOf
        [ Fuzz.intRange -2147483648 2147483647
        , Fuzz.intRange -70000 70000
        , Fuzz.oneOfValues [ 0, 1, -1, 65536, -65536, 2147483647, -2147483648, 32767, -32768 ]
        ]


{-| The low 32 bits of a \* b, signed, from 16-bit limbs of both numbers taken as
unsigned: only products below 2^32 are ever formed.
-}
referenceMul : Int -> Int -> Int
referenceMul a b =
    let
        ua =
            modBy 4294967296 a

        ub =
            modBy 4294967296 b

        a0 =
            modBy 65536 ua

        a1 =
            ua // 65536

        b0 =
            modBy 65536 ub

        b1 =
            ub // 65536

        low =
            a0 * b0

        mid =
            modBy 65536 (a0 * b1 + a1 * b0)
    in
    Int32.wrap (modBy 4294967296 (low + mid * 65536))


{-| ((int64) a \* b) >> 16, cast to int32, from signed 16-bit-limb arithmetic: the
product's sign and magnitude are worked out separately, then floored.
-}
referenceFixedMul : Int -> Int -> Int
referenceFixedMul a b =
    let
        negative =
            (a < 0) /= (b < 0)

        ma =
            abs a

        mb =
            abs b

        a0 =
            modBy 65536 ma

        a1 =
            ma // 65536

        b0 =
            modBy 65536 mb

        b1 =
            mb // 65536

        -- |a * b| = a1*b1*2^32 + (a1*b0 + a0*b1)*2^16 + a0*b0; divided by 2^16:
        whole =
            a1 * b1 * 65536 + (a1 * b0 + a0 * b1)

        fraction =
            a0 * b0

        magnitudeFloor =
            whole + fraction // 65536

        exact =
            modBy 65536 fraction == 0
    in
    if negative then
        -- floor(-x) = -ceil(x)
        Int32.wrap
            -(magnitudeFloor
                + (if exact then
                    0

                   else
                    1
                  )
             )

    else
        Int32.wrap magnitudeFloor


{-| The same pseudo-random walk as the eco check program (scratch tool): 2 million
pairs through Int32.mul, Fixed.mul, Fixed.div and the shifts, folded into a checksum.
eco (64-bit Int) printed 813851004 with no mismatches against the references, so this
pins the JavaScript backend to the same bits.
-}
crossBackendChecksum : Int -> Int -> Int -> Int -> Int
crossBackendChecksum n s bad checksum =
    if n == 0 then
        bad * 1000000000 + checksum

    else
        let
            s1 =
                lcg s

            s2 =
                lcg s1

            a =
                toI32 s1

            b =
                toI32
                    (if modBy 3 s2 == 0 then
                        modBy 200000 s2

                     else
                        s2
                    )

            m1 =
                Int32.mul a b

            m2 =
                Fixed.mul a b

            ok =
                m1 == referenceMul a b && m2 == referenceFixedMul a b

            d =
                Fixed.div a b

            sh =
                Int32.shiftRight 5 a + Int32.shiftRightUnsigned 7 a + Angle.toFine (Angle.wrap a)

            c =
                modBy 1000000000 (checksum * 31 + modBy 1000000 (m1 + m2 + d + sh))
        in
        crossBackendChecksum (n - 1)
            s2
            (if ok then
                bad

             else
                bad + 1
            )
            c


lcg : Int -> Int
lcg s =
    modBy 4294967296 (s * 1664525 + 1013904223)


toI32 : Int -> Int
toI32 u =
    if u >= 2147483648 then
        u - 4294967296

    else
        u


suite : Test
suite =
    describe "Foundation"
        [ describe "Int32"
            [ fuzz int32 "wrap leaves int32s alone" <|
                \a -> Int32.wrap a |> Expect.equal a
            , test "wrap folds 2^31 to -2^31" <|
                \_ -> Int32.wrap 2147483648 |> Expect.equal -2147483648
            , fuzz2 int32 int32 "mul matches the 16-bit-limb reference" <|
                \a b -> Int32.mul a b |> Expect.equal (referenceMul a b)
            , fuzz2 int32 int32 "add wraps" <|
                \a b -> Int32.add a b |> Expect.equal (referenceMul 1 (a + b))
            , fuzz int32 "shiftRight floors" <|
                \a -> Int32.shiftRight 3 a |> Expect.equal (Int32.floorDiv a 8)
            , fuzz int32 "shiftRightUnsigned reads the bits unsigned" <|
                \a -> Int32.shiftRightUnsigned 19 a |> Expect.equal (modBy 4294967296 a // 524288)
            , fuzz int32 "shiftLeft is multiplication, wrapped" <|
                \a -> Int32.shiftLeft 16 a |> Expect.equal (referenceMul a 65536)
            , test "abs of MININT is MININT, as in C" <|
                \_ -> Int32.abs -2147483648 |> Expect.equal -2147483648
            , fuzz int32 "toShort and toSignedByte stay in range" <|
                \a ->
                    ( Int32.toShort a >= -32768 && Int32.toShort a <= 32767
                    , Int32.toSignedByte a >= -128 && Int32.toSignedByte a <= 127
                    )
                        |> Expect.equal ( True, True )
            ]
        , describe "Fixed"
            [ fuzz2 int32 int32 "mul matches the reference" <|
                \a b -> Fixed.mul a b |> Expect.equal (referenceFixedMul a b)
            , test "1.5 * 2.5 = 3.75" <|
                \_ -> Fixed.mul 98304 163840 |> Expect.equal 245760
            , test "-1.5 * 2.5 = -3.75" <|
                \_ -> Fixed.mul -98304 163840 |> Expect.equal -245760
            , test "div 1 by 3" <|
                \_ -> Fixed.div 65536 196608 |> Expect.equal 21845
            , test "div by zero saturates by the sign of a" <|
                \_ -> ( Fixed.div 5 0, Fixed.div -5 0 ) |> Expect.equal ( Fixed.maxInt, Fixed.minInt )
            , test "div that overflows saturates by the sign of a ^ b" <|
                \_ -> Fixed.div 2000000000 -3 |> Expect.equal Fixed.minInt
            , fuzz2 (Fuzz.intRange -1000000 1000000) (Fuzz.intRange 70000 2000000000) "div then mul comes back close" <|
                \a b ->
                    let
                        back =
                            Fixed.mul (Fixed.div a b) b
                    in
                    Expect.atMost (b // 65536 + 2) (abs (back - a))
            ]
        , describe "Angle"
            [ fuzz2 int32 int32 "add wraps into [0, 2^32)" <|
                \a b ->
                    let
                        s =
                            Angle.add (Angle.wrap a) (Angle.wrap b)
                    in
                    ( s >= 0 && s < 4294967296, s ) |> Expect.equal ( True, modBy 4294967296 (a + b) )
            , fuzz int32 "toFine is the top 13 bits" <|
                \a -> Angle.toFine (Angle.wrap a) |> Expect.equal (modBy 4294967296 a // 524288)
            , test "sine of ANG90 is 1.0, cosine is 0-ish" <|
                \_ -> ( Angle.sine Angle.ang90, abs (Angle.cosine Angle.ang90) < 50 ) |> Expect.equal ( 65535, True )
            , test "tantoangle ends at ANG45" <|
                \_ -> Tables.tantoangle 2048 |> Expect.equal Angle.ang45
            , test "slopeDiv caps at SLOPERANGE" <|
                \_ -> ( Angle.slopeDiv 100 10, Angle.slopeDiv 1 1000000 ) |> Expect.equal ( 2048, 0 )
            ]
        , describe "Random"
            [ test "P_Random's first numbers are the table's" <|
                \_ ->
                    let
                        step ( acc, rng ) =
                            let
                                ( n, next ) =
                                    Random.pRandom rng
                            in
                            ( n :: acc, next )

                        ( got, _ ) =
                            List.foldl (\_ s -> step s) ( [], Random.init ) (List.range 1 5)
                    in
                    List.reverse got |> Expect.equal [ 8, 109, 220, 222, 241 ]
            , test "the indexes wrap at 256" <|
                \_ ->
                    Random.pRandom { prndindex = 255, rndindex = 0 }
                        |> Tuple.second
                        |> .prndindex
                        |> Expect.equal 0
            ]
        , test "2 million mixed cases give eco's checksum" <|
            \_ -> crossBackendChecksum 2000000 12345 0 0 |> Expect.equal 813851004
        , describe "StateHash"
            [ fuzz2 (Fuzz.intRange 0 4294967295) int32 "combine stays in [0, 2^32)" <|
                \h v ->
                    let
                        c =
                            StateHash.combine v h
                    in
                    (c >= 0 && c < 4294967296) |> Expect.equal True
            ]
        ]
