module Doom.Input exposing
    ( Input, none, Turning, notTurning, buildTiccmd
    , forwardMove, sideMove, angleTurn, slowTurnTics
    , Bindings, defaultBindings, Controls, noControls, respond, clear, build
    )

{-| Turning what the player holds down into a ticcmd (g\_game.c's G\_BuildTiccmd), for
the keyboard and the mouse's sideways movement.

    Input.buildTiccmd { Input.none | forward = True, run = True } Input.notTurning
    --> ( { forwardmove = 0x32, ... }, turning )

@docs Input, none, Turning, notTurning, buildTiccmd
@docs forwardMove, sideMove, angleTurn, slowTurnTics


# The whole of G\_BuildTiccmd

G\_Responder keeps which keys are down (`gamekeydown`), the mouse's buttons and last
movement; G\_BuildTiccmd makes the tic's command from them with the key bindings,
including the mouse's forward button and its double clicks for "use".

@docs Bindings, defaultBindings, Controls, noControls, respond, clear, build

-}

import Array exposing (Array)
import Bitwise
import Doom.Config as Config
import Doom.Event exposing (Event(..))
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



-- G_RESPONDER AND G_BUILDTICCMD


{-| The key and mouse button bindings: `Doom.Config.Bindings`.
-}
type alias Bindings =
    Config.Bindings


{-| m\_misc.c's defaults: `Doom.Config.defaultBindings`.
-}
defaultBindings : Bindings
defaultBindings =
    Config.defaultBindings


{-| g\_game.c's input state: `gamekeydown`, `mousebuttons`, the mouse movement since
the last tic, `turnheld`, and the double click counters.
-}
type alias Controls =
    { keys : Array Bool
    , mouseButtons : Array Bool
    , mousex : Int
    , mousey : Int
    , turnheld : Int
    , dclicktime : Int
    , dclickstate : Bool
    , dclicks : Int
    , dclicktime2 : Int
    , dclickstate2 : Bool
    , dclicks2 : Int
    }


{-| Nothing held, as at the start.
-}
noControls : Controls
noControls =
    { keys = Array.repeat 256 False
    , mouseButtons = Array.repeat 3 False
    , mousex = 0
    , mousey = 0
    , turnheld = 0
    , dclicktime = 0
    , dclickstate = False
    , dclicks = 0
    , dclicktime2 = 0
    , dclickstate2 = False
    , dclicks2 = 0
    }


{-| G\_DoLoadLevel's "clear cmd building stuff": the keys, the mouse's movement and
buttons (not `turnheld` or the double clicks).
-}
clear : Controls -> Controls
clear c =
    { c | keys = Array.repeat 256 False, mouseButtons = Array.repeat 3 False, mousex = 0, mousey = 0 }


{-| The end of G\_Responder: keys down and up, and mouse events scaled by the mouse
sensitivity (0 to 9). Key down and mouse events are eaten (True); key up events are
not. The pause key is the caller's.
-}
respond : Int -> Event -> Controls -> ( Controls, Bool )
respond sensitivity ev c =
    case ev of
        KeyDown k ->
            ( { c | keys = setKey k True c.keys }, True )

        KeyUp k ->
            ( { c | keys = setKey k False c.keys }, False )

        Mouse buttons dx dy ->
            ( { c
                | mouseButtons =
                    Array.fromList
                        [ Bitwise.and buttons 1 /= 0
                        , Bitwise.and buttons 2 /= 0
                        , Bitwise.and buttons 4 /= 0
                        ]
                , mousex = dx * (sensitivity + 5) // 10
                , mousey = dy * (sensitivity + 5) // 10
              }
            , True
            )


setKey : Int -> Bool -> Array Bool -> Array Bool
setKey k down keys =
    if k >= 0 && k < 256 then
        Array.set k down keys

    else
        keys


type alias Clicks =
    { time : Int
    , state : Bool
    , clicks : Int
    }


{-| A double click detector: two presses within 20 tics (with the button changing
state after more than a tic) are a "use".
-}
doubleClick : Bool -> Clicks -> ( Clicks, Bool )
doubleClick pressed d =
    if pressed /= d.state && d.time > 1 then
        let
            clicks =
                if pressed then
                    d.clicks + 1

                else
                    d.clicks
        in
        if clicks == 2 then
            ( { d | state = pressed, clicks = 0 }, True )

        else
            ( { time = 0, state = pressed, clicks = clicks }, False )

    else if d.time + 1 > 20 then
        ( { time = d.time + 1, state = False, clicks = 0 }, False )

    else
        ( { d | time = d.time + 1 }, False )


{-| G\_BuildTiccmd: the console player's command for the next tic (without the pause
and save requests, which the caller adds), and the controls after it.
-}
build : Bindings -> Controls -> ( Ticcmd, Controls )
build b c =
    let
        key k =
            Array.get k c.keys |> Maybe.withDefault False

        mouse n =
            Array.get n c.mouseButtons |> Maybe.withDefault False

        strafe =
            key b.strafe || mouse b.mouseStrafe

        run =
            key b.speed

        turnheld =
            if key b.right || key b.left then
                c.turnheld + 1

            else
                0

        tspeed =
            if turnheld < slowTurnTics then
                2

            else if run then
                1

            else
                0

        add cond n acc =
            if cond then
                acc + n

            else
                acc

        ( side0, turn0 ) =
            if strafe then
                ( 0 |> add (key b.right) (sideMove run) |> add (key b.left) -(sideMove run), 0 )

            else
                ( 0, 0 |> add (key b.right) -(angleTurn tspeed) |> add (key b.left) (angleTurn tspeed) )

        side1 =
            side0 |> add (key b.strafeRight) (sideMove run) |> add (key b.strafeLeft) -(sideMove run)

        useKey =
            key b.use

        weapon =
            List.range 0 7 |> List.filter (\i -> key (Char.toCode '1' + i)) |> List.head

        buttons0 =
            0
                |> orIf (key b.fire || mouse b.mouseFire) Ticcmd.btAttack
                |> orIf useKey Ticcmd.btUse
                |> (\bt ->
                        case weapon of
                            Just i ->
                                Bitwise.or bt (Bitwise.or Ticcmd.btChange (Bitwise.shiftLeftBy Ticcmd.btWeaponShift i))

                            Nothing ->
                                bt
                   )

        forwardButton =
            mouse b.mouseForward

        forward =
            0
                |> add (key b.up) (forwardMove run)
                |> add (key b.down) -(forwardMove run)
                |> add forwardButton (forwardMove run)
                |> (+) c.mousey

        ( d1, use1 ) =
            doubleClick forwardButton
                { time = c.dclicktime
                , state = c.dclickstate
                , clicks =
                    if useKey then
                        0

                    else
                        c.dclicks
                }

        ( d2, use2 ) =
            doubleClick (mouse b.mouseStrafe) { time = c.dclicktime2, state = c.dclickstate2, clicks = c.dclicks2 }

        ( side, turn ) =
            if strafe then
                ( side1 + c.mousex * 2, turn0 )

            else
                ( side1, turn0 - c.mousex * 8 )

        maxMove =
            forwardMove True
    in
    ( { forwardmove = Basics.clamp -maxMove maxMove forward
      , sidemove = Basics.clamp -maxMove maxMove side
      , angleturn = Int32.toShort turn
      , consistancy = 0
      , chatchar = 0
      , buttons = buttons0 |> orIf use1 Ticcmd.btUse |> orIf use2 Ticcmd.btUse
      }
    , { c
        | mousex = 0
        , mousey = 0
        , turnheld = turnheld
        , dclicktime = d1.time
        , dclickstate = d1.state
        , dclicks = d1.clicks
        , dclicktime2 = d2.time
        , dclickstate2 = d2.state
        , dclicks2 = d2.clicks
      }
    )
