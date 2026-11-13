module Doom.Play.Switch exposing (changeSwitchTexture, startButton, buttonTime, useSpecialLine)

{-| Switches and buttons, and what using a line does (p\_switch.c).

A switch flips its texture to the other of its pair in `Data.switchList`; a button
(a switch that can be used again) flips back a second later, when P\_UpdateSpecials
counts its `World.buttons` entry down.

@docs changeSwitchTexture, startButton, buttonTime, useSpecialLine

-}

import Array
import Bitwise
import Doom.Flags as Flags
import Doom.Info.Sfx as Sfx
import Doom.Level as Level exposing (LineId)
import Doom.Mobj exposing (ThinkerId)
import Doom.Play.Ceiling as Ceiling
import Doom.Play.Door as Door
import Doom.Play.Env exposing (Env)
import Doom.Play.Floor as Floor
import Doom.Play.Light as Light
import Doom.Play.Plat as Plat
import Doom.Sound as Sound
import Doom.Thinker exposing (CeilingType(..), DoorType(..), FloorType(..), PlatType(..), StairType(..))
import Doom.World as World exposing (ButtonWhere(..), Exit(..), World)


{-| BUTTONTIME: how long a button stays pressed, in tics.
-}
buttonTime : Int
buttonTime =
    35


{-| P\_StartButton line where texture time: remember to put `texture` back on the line
in `time` tics, unless the line already has a button counting down. (C stops the game
when all MAXBUTTONS are taken; then nothing happens.)
-}
startButton : Env -> LineId -> ButtonWhere -> Int -> Int -> World -> World
startButton env lineId where_ texture time world =
    let
        pressed =
            Array.toList world.buttons
                |> List.any
                    (\slot ->
                        case slot of
                            Just b ->
                                b.btimer /= 0 && b.line == lineId

                            Nothing ->
                                False
                    )

        free i =
            case Array.get i world.buttons of
                Nothing ->
                    world

                Just slot ->
                    if Maybe.withDefault 0 (Maybe.map .btimer slot) == 0 then
                        { world
                            | buttons =
                                Array.set i
                                    (Just
                                        { line = lineId
                                        , where_ = where_
                                        , btexture = texture
                                        , btimer = time
                                        , soundorg = (Level.line lineId env.level).frontsector
                                        }
                                    )
                                    world.buttons
                        }

                    else
                        free (i + 1)
    in
    if pressed then
        world

    else
        free 0


{-| P\_ChangeSwitchTexture line useAgain: flip the switch texture on the line's front
side (the first of top, middle, bottom found in the switch list, in list order), with
a click; a switch that can't be used again loses its special.

Two quirks of the C source are kept: the click comes from `buttonlist[0]`'s sound
origin (so it is heard at full volume unless button 0 is counting down), and the exit
switch sound never plays, because the line's special is cleared before it is checked.

-}
changeSwitchTexture : Env -> LineId -> Bool -> World -> World
changeSwitchTexture env lineId useAgain world0 =
    let
        world =
            if useAgain then
                world0

            else
                World.updateLine lineId (\l -> { l | special = 0 }) world0

        sideId =
            (Level.line lineId env.level).frontSide

        side =
            World.sideState sideId world

        sound =
            if (World.lineState lineId world).special == 11 then
                Sfx.Swtchx

            else
                Sfx.Swtchn

        origin =
            case Array.get 0 world.buttons of
                Just (Just b) ->
                    Sound.FromSector b.soundorg

                _ ->
                    Sound.Ambient

        list =
            env.data.switchList

        count =
            (Array.length list // 2) * 2

        flip i where_ set =
            let
                tex =
                    Array.get i list |> Maybe.withDefault -1

                other =
                    Array.get (Bitwise.xor i 1) list |> Maybe.withDefault 0

                flipped =
                    world
                        |> World.startSound origin sound
                        |> World.updateSide sideId (set other)
            in
            if useAgain then
                startButton env lineId where_ tex buttonTime flipped

            else
                flipped

        search i =
            if i >= count then
                world

            else
                let
                    tex =
                        Array.get i list |> Maybe.withDefault -1
                in
                if tex == side.toptexture then
                    flip i ButtonTop (\t s -> { s | toptexture = t })

                else if tex == side.midtexture then
                    flip i ButtonMiddle (\t s -> { s | midtexture = t })

                else if tex == side.bottomtexture then
                    flip i ButtonBottom (\t s -> { s | bottomtexture = t })

                else
                    search (i + 1)
    in
    search 0


{-| P\_UseSpecialLine thing line side: a thing pressed use on the line. Only front sides
work; monsters only open doors (1, 32, 33, 34), and never secret ones. False if the
line does nothing when used this way.
-}
useSpecialLine : Env -> ThinkerId -> LineId -> Int -> World -> ( Bool, World )
useSpecialLine env thingId lineId side world =
    let
        state =
            World.lineState lineId world

        isPlayer =
            World.mobj thingId world |> Maybe.andThen .player |> (/=) Nothing

        monsterMay =
            not (Flags.has Flags.mlSecret state.flags) && List.member state.special [ 1, 32, 33, 34 ]
    in
    if side /= 0 && state.special /= 124 then
        ( False, world )

    else if not isPlayer && not monsterMay then
        ( False, world )

    else
        ( True, use env thingId lineId state.special world )


{-| Do it, and flip the switch if it worked: `useAgain` False for switches, True for
buttons.
-}
use : Env -> ThinkerId -> LineId -> Int -> World -> World
use env thingId lineId special world =
    let
        switch useAgain ( ok, w ) =
            if ok then
                changeSwitchTexture env lineId useAgain w

            else
                w

        once =
            switch False

        button =
            switch True

        flipAnyway useAgain w =
            changeSwitchTexture env lineId useAgain w
    in
    case special of
        -- MANUALS
        1 ->
            Door.verticalDoor env lineId thingId world

        26 ->
            Door.verticalDoor env lineId thingId world

        27 ->
            Door.verticalDoor env lineId thingId world

        28 ->
            Door.verticalDoor env lineId thingId world

        31 ->
            Door.verticalDoor env lineId thingId world

        32 ->
            Door.verticalDoor env lineId thingId world

        33 ->
            Door.verticalDoor env lineId thingId world

        34 ->
            Door.verticalDoor env lineId thingId world

        117 ->
            Door.verticalDoor env lineId thingId world

        118 ->
            Door.verticalDoor env lineId thingId world

        -- SWITCHES
        7 ->
            once (Floor.buildStairs env lineId Build8 world)

        9 ->
            once (Floor.doDonut env lineId world)

        11 ->
            world |> flipAnyway False |> setExit NormalExit

        14 ->
            once (Plat.doPlat env lineId RaiseAndChange 32 world)

        15 ->
            once (Plat.doPlat env lineId RaiseAndChange 24 world)

        18 ->
            once (Floor.doFloor env lineId RaiseFloorToNearest world)

        20 ->
            once (Plat.doPlat env lineId RaiseToNearestAndChange 0 world)

        21 ->
            once (Plat.doPlat env lineId DownWaitUpStay 0 world)

        23 ->
            once (Floor.doFloor env lineId LowerFloorToLowest world)

        29 ->
            once (Door.doDoor env lineId NormalDoor world)

        41 ->
            once (Ceiling.doCeiling env lineId LowerToFloor world)

        71 ->
            once (Floor.doFloor env lineId TurboLower world)

        49 ->
            once (Ceiling.doCeiling env lineId CrushAndRaise world)

        50 ->
            once (Door.doDoor env lineId CloseDoor world)

        51 ->
            world |> flipAnyway False |> setExit SecretExit

        55 ->
            once (Floor.doFloor env lineId RaiseFloorCrush world)

        101 ->
            once (Floor.doFloor env lineId RaiseFloor world)

        102 ->
            once (Floor.doFloor env lineId LowerFloor world)

        103 ->
            once (Door.doDoor env lineId OpenDoor world)

        111 ->
            once (Door.doDoor env lineId BlazeRaise world)

        112 ->
            once (Door.doDoor env lineId BlazeOpen world)

        113 ->
            once (Door.doDoor env lineId BlazeClose world)

        122 ->
            once (Plat.doPlat env lineId BlazeDWUS 0 world)

        127 ->
            once (Floor.buildStairs env lineId Turbo16 world)

        131 ->
            once (Floor.doFloor env lineId RaiseFloorTurbo world)

        133 ->
            once (Door.doLockedDoor env lineId BlazeOpen thingId world)

        135 ->
            once (Door.doLockedDoor env lineId BlazeOpen thingId world)

        137 ->
            once (Door.doLockedDoor env lineId BlazeOpen thingId world)

        140 ->
            once (Floor.doFloor env lineId RaiseFloor512 world)

        -- BUTTONS
        42 ->
            button (Door.doDoor env lineId CloseDoor world)

        43 ->
            button (Ceiling.doCeiling env lineId LowerToFloor world)

        45 ->
            button (Floor.doFloor env lineId LowerFloor world)

        60 ->
            button (Floor.doFloor env lineId LowerFloorToLowest world)

        61 ->
            button (Door.doDoor env lineId OpenDoor world)

        62 ->
            button (Plat.doPlat env lineId DownWaitUpStay 1 world)

        63 ->
            button (Door.doDoor env lineId NormalDoor world)

        64 ->
            button (Floor.doFloor env lineId RaiseFloor world)

        66 ->
            button (Plat.doPlat env lineId RaiseAndChange 24 world)

        67 ->
            button (Plat.doPlat env lineId RaiseAndChange 32 world)

        65 ->
            button (Floor.doFloor env lineId RaiseFloorCrush world)

        68 ->
            button (Plat.doPlat env lineId RaiseToNearestAndChange 0 world)

        69 ->
            button (Floor.doFloor env lineId RaiseFloorToNearest world)

        70 ->
            button (Floor.doFloor env lineId TurboLower world)

        114 ->
            button (Door.doDoor env lineId BlazeRaise world)

        115 ->
            button (Door.doDoor env lineId BlazeOpen world)

        116 ->
            button (Door.doDoor env lineId BlazeClose world)

        123 ->
            button (Plat.doPlat env lineId BlazeDWUS 0 world)

        132 ->
            button (Floor.doFloor env lineId RaiseFloorTurbo world)

        99 ->
            button (Door.doLockedDoor env lineId BlazeOpen thingId world)

        134 ->
            button (Door.doLockedDoor env lineId BlazeOpen thingId world)

        136 ->
            button (Door.doLockedDoor env lineId BlazeOpen thingId world)

        138 ->
            Light.lightTurnOn env lineId 255 world |> flipAnyway True

        139 ->
            Light.lightTurnOn env lineId 35 world |> flipAnyway True

        _ ->
            world


{-| G\_ExitLevel / G\_SecretExitLevel.
-}
setExit : Exit -> World -> World
setExit exit world =
    { world | exit = Just exit }
