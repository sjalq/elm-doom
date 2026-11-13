module Doom.Angle exposing
    ( Angle, ang45, ang90, ang180, ang270, angMax
    , add, sub, neg, wrap, toSigned
    , fineangles, finemask, angletofineshift, toFine
    , sine, cosine
    , slopeRange, slopeBits, dbits, slopeDiv
    )

{-| Binary angle measurement (`angle_t`, tables.h): a full circle is 2^32, held as an
unsigned 32-bit value in [0, 2^32). Adding and subtracting wrap around, as in C.

Fine angles index the trigonometry tables: 8192 to the circle, so an angle's fine angle
is its top 13 bits (`toFine`).

@docs Angle, ang45, ang90, ang180, ang270, angMax
@docs add, sub, neg, wrap, toSigned
@docs fineangles, finemask, angletofineshift, toFine
@docs sine, cosine
@docs slopeRange, slopeBits, dbits, slopeDiv

-}

import Doom.Fixed exposing (Fixed)
import Doom.Int32 as Int32
import Doom.Tables as Tables


{-| A binary angle, unsigned.
-}
type alias Angle =
    Int


{-| ANG45.
-}
ang45 : Angle
ang45 =
    0x20000000


{-| ANG90.
-}
ang90 : Angle
ang90 =
    0x40000000


{-| ANG180.
-}
ang180 : Angle
ang180 =
    0x80000000


{-| ANG270.
-}
ang270 : Angle
ang270 =
    0xC0000000


{-| ANGLE\_MAX: the biggest angle, just short of a full circle.
-}
angMax : Angle
angMax =
    0xFFFFFFFF


{-| Any integer (an int32 or an unsigned value) as an angle: C's cast to `angle_t`.
-}
wrap : Int -> Angle
wrap =
    Int32.toUnsigned


{-| `a + b`, wrapping.
-}
add : Angle -> Angle -> Angle
add a b =
    wrap (a + b)


{-| `a - b`, wrapping.
-}
sub : Angle -> Angle -> Angle
sub a b =
    wrap (a - b)


{-| `-a`, wrapping.
-}
neg : Angle -> Angle
neg a =
    wrap -a


{-| The same bits as C's `int`: angles past ANG180 become negative.
-}
toSigned : Angle -> Int
toSigned =
    Int32.wrap


{-| FINEANGLES.
-}
fineangles : Int
fineangles =
    8192


{-| FINEMASK.
-}
finemask : Int
finemask =
    8191


{-| ANGLETOFINESHIFT.
-}
angletofineshift : Int
angletofineshift =
    19


{-| `angle >> ANGLETOFINESHIFT`: the angle's fine angle, 0 to 8191.
-}
toFine : Angle -> Int
toFine a =
    Int32.shiftRightUnsigned angletofineshift a


{-| `finesine[angle >> ANGLETOFINESHIFT]`.
-}
sine : Angle -> Fixed
sine a =
    Tables.finesine (toFine a)


{-| `finecosine[angle >> ANGLETOFINESHIFT]`.
-}
cosine : Angle -> Fixed
cosine a =
    Tables.finecosine (toFine a)


{-| SLOPERANGE: tantoangle's index range.
-}
slopeRange : Int
slopeRange =
    2048


{-| SLOPEBITS.
-}
slopeBits : Int
slopeBits =
    11


{-| DBITS: FRACBITS - SLOPEBITS.
-}
dbits : Int
dbits =
    5


{-| SlopeDiv (tables.c): the index into `tantoangle` for the slope num / den, both
read as unsigned, capped at SLOPERANGE.
-}
slopeDiv : Int -> Int -> Int
slopeDiv num den =
    if Int32.toUnsigned den < 512 then
        slopeRange

    else
        let
            ans =
                Int32.toUnsigned (Int32.shiftLeft 3 num) // Int32.shiftRightUnsigned 8 den
        in
        if ans <= slopeRange then
            ans

        else
            slopeRange
