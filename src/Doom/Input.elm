module Doom.Input exposing
    ( Input, none, Turning, notTurning, buildTiccmd
    , forwardMove, sideMove, angleTurn, slowTurnTics
    )

{-| Turning what the player holds down into a ticcmd (g\_game.c's G\_BuildTiccmd), for
the keyboard and the mouse's sideways movement.

    Input.buildTiccmd { Input.none | forward = True, run = True } Input.notTurning
    --> ( { forwardmove = 0x32, ... }, turning )

@docs Input, none, Turning, notTurning, buildTiccmd
@docs forwardMove, sideMove, angleTurn, slowTurnTics

-}

import Bitwise
import Doom.Int32 as Int32
import Doom.Ticcmd as Ticcmd exposing (Ticcmd)


{-| The controls for one tic. `strafe` is the strafe modifier (turning keys strafe
while it is held), `run` the speed key. `weapon` is a weapon key held, 1 to 8.
`mouseX` is how far the mouse moved sideways since the last tic, already scaled by
the mouse sensitivity (`ev->data2 * (mouseSensitivity + 5) / 10` in the C source).
-}
type alias Input =
    { forward : Bool
    , backward : Bool
    , turnLeft : Bool
    , turnRight : Bool
    , strafeLeft : Bool
    , strafeRight : Bool
    , strafe : Bool
    , run : Bool
    , fire : Bool
    , use : Bool
    , weapon : Maybe Int
    , mouseX : Int
    }


{-| Nothing held.
-}
none : Input
none =
    { forward = False
    , backward = False
    , turnLeft = False
    , turnRight = False
    , strafeLeft = False
    , strafeRight = False
    , strafe = False
    , run = False
    , fire = False
    , use = False
    , weapon = Nothing
    , mouseX = 0
    }


{-| `turnheld`: how many tics a turning key has been held, for accelerating turns.
-}
type alias Turning =
    Int


{-| No turning key held.
-}
notTurning : Turning
notTurning =
    0


{-| forwardmove[]: walking and running.
-}
forwardMove : Bool -> Int
forwardMove running =
    if running then
        0x32

    else
        0x19


{-| sidemove[].
-}
sideMove : Bool -> Int
sideMove running =
    if running then
        0x28

    else
        0x18


{-| angleturn[]: 0 walking, 1 running, 2 the slow turn of the first tics.
-}
angleTurn : Int -> Int
angleTurn speed =
    case speed of
        0 ->
            640

        1 ->
            1280

        _ ->
            320


{-| SLOWTURNTICS: tics of slow turning before turning gets faster.
-}
slowTurnTics : Int
slowTurnTics =
    6


{-| G\_BuildTiccmd: the ticcmd for this tic, and the new `turnheld`. Movement keys
cancel each other out; forward and sideways movement are capped at running speed.
-}
buildTiccmd : Input -> Turning -> ( Ticcmd, Turning )
buildTiccmd input turnheld =
    let
        speed =
            if input.run then
                1

            else
                0

        newTurnheld =
            if input.turnLeft || input.turnRight then
                turnheld + 1

            else
                0

        tspeed =
            if newTurnheld < slowTurnTics then
                2

            else
                speed

        add cond n acc =
            if cond then
                acc + n

            else
                acc

        side0 =
            if input.strafe then
                0 |> add input.turnRight (sideMove input.run) |> add input.turnLeft -(sideMove input.run)

            else
                0

        turn0 =
            if input.strafe then
                0

            else
                0 |> add input.turnRight -(angleTurn tspeed) |> add input.turnLeft (angleTurn tspeed)

        forward =
            0
                |> add input.forward (forwardMove input.run)
                |> add input.backward -(forwardMove input.run)

        side1 =
            side0
                |> add input.strafeRight (sideMove input.run)
                |> add input.strafeLeft -(sideMove input.run)

        ( side, turn ) =
            if input.strafe then
                ( side1 + input.mouseX * 2, turn0 )

            else
                ( side1, turn0 - input.mouseX * 8 )

        maxMove =
            forwardMove True

        clamp n =
            Basics.clamp -maxMove maxMove n

        buttons =
            0
                |> orIf input.fire Ticcmd.btAttack
                |> orIf input.use Ticcmd.btUse
                |> (\b ->
                        case input.weapon of
                            Just key ->
                                if key >= 1 && key <= 8 then
                                    Bitwise.or b (Bitwise.or Ticcmd.btChange (Bitwise.shiftLeftBy Ticcmd.btWeaponShift (key - 1)))

                                else
                                    b

                            Nothing ->
                                b
                   )
    in
    ( { forwardmove = clamp forward
      , sidemove = clamp side
      , angleturn = Int32.toShort turn
      , consistancy = 0
      , chatchar = 0
      , buttons = buttons
      }
    , newTurnheld
    )


orIf : Bool -> Int -> Int -> Int
orIf cond bit b =
    if cond then
        Bitwise.or b bit

    else
        b
