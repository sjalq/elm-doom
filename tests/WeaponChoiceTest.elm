module WeaponChoiceTest exposing (suite)

import Bitwise
import Doom.Defs exposing (Ammo(..), GameMode(..), Power(..), Weapon(..))
import Doom.Play.WeaponChoice as WeaponChoice
import Doom.Player as Player exposing (Player)
import Doom.Ticcmd as Ticcmd
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


{-| A player holding `ready`, owning `owned`, with `ammo`.
-}
player : Weapon -> List Weapon -> List ( Ammo, Int ) -> Player
player ready owned ammo =
    let
        withWeapons =
            List.foldl (\w p -> Player.setWeaponOwned w True p) Player.new (ready :: owned)
    in
    List.foldl (\( a, n ) p -> Player.setAmmo a n p) { withWeapons | readyweapon = ready } ammo


{-| The player asking to change to weapon slot `slot` (0 to 7).
-}
asking : Int -> Player -> Player
asking slot p =
    let
        cmd =
            p.cmd
    in
    { p | cmd = { cmd | buttons = Bitwise.or Ticcmd.btChange (Bitwise.shiftLeftBy Ticcmd.btWeaponShift slot) } }


weaponFuzzer : Fuzzer Weapon
weaponFuzzer =
    Fuzz.oneOfValues [ Fist, Pistol, Shotgun, Chaingun, Missile, Plasma, Bfg, Chainsaw, SuperShotgun ]


playerFuzzer : Fuzzer Player
playerFuzzer =
    Fuzz.map4
        (\ready owned ammo strength ->
            player ready owned ammo |> Player.setPower Strength strength
        )
        weaponFuzzer
        (Fuzz.list weaponFuzzer)
        (Fuzz.list (Fuzz.pair (Fuzz.oneOfValues [ Clip, Shell, Cell, Misl ]) (Fuzz.intRange 0 300)))
        (Fuzz.oneOfValues [ 0, 1 ])


modeFuzzer : Fuzzer GameMode
modeFuzzer =
    Fuzz.oneOfValues [ Shareware, Registered, Commercial, Retail ]


suite : Test
suite =
    describe "Doom.Play.WeaponChoice"
        [ describe "hasAmmoForShot"
            [ test "the fist never runs out" <|
                \_ -> WeaponChoice.hasAmmoForShot (player Fist [] []) |> Expect.equal True
            , test "the BFG needs 40 cells" <|
                \_ ->
                    ( WeaponChoice.hasAmmoForShot (player Bfg [] [ ( Cell, 39 ) ])
                    , WeaponChoice.hasAmmoForShot (player Bfg [] [ ( Cell, 40 ) ])
                    )
                        |> Expect.equal ( False, True )
            , test "the super shotgun needs two shells" <|
                \_ ->
                    ( WeaponChoice.hasAmmoForShot (player SuperShotgun [] [ ( Shell, 1 ) ])
                    , WeaponChoice.hasAmmoForShot (player SuperShotgun [] [ ( Shell, 2 ) ])
                    )
                        |> Expect.equal ( False, True )
            ]
        , describe "fallbackWeapon"
            [ test "plasma comes first, but not in shareware" <|
                \_ ->
                    let
                        p =
                            player Pistol [ Plasma, Chaingun ] [ ( Cell, 1 ), ( Clip, 5 ) ]
                    in
                    ( WeaponChoice.fallbackWeapon Retail p, WeaponChoice.fallbackWeapon Shareware p )
                        |> Expect.equal ( Plasma, Chaingun )
            , test "the super shotgun needs Doom II and more than two shells" <|
                \_ ->
                    ( WeaponChoice.fallbackWeapon Commercial (player Fist [ SuperShotgun, Shotgun ] [ ( Shell, 3 ) ])
                    , WeaponChoice.fallbackWeapon Commercial (player Fist [ SuperShotgun, Shotgun ] [ ( Shell, 2 ) ])
                    , WeaponChoice.fallbackWeapon Retail (player Fist [ SuperShotgun, Shotgun ] [ ( Shell, 3 ) ])
                    )
                        |> Expect.equal ( SuperShotgun, Shotgun, Shotgun )
            , test "bullets give the pistol even when it isn't owned" <|
                \_ -> WeaponChoice.fallbackWeapon Retail (player Fist [] [ ( Clip, 1 ) ]) |> Expect.equal Pistol
            , test "the chainsaw beats the rocket launcher" <|
                \_ ->
                    WeaponChoice.fallbackWeapon Retail (player Fist [ Chainsaw, Missile ] [ ( Misl, 5 ) ])
                        |> Expect.equal Chainsaw
            , test "the BFG needs more than 40 cells, and plasma rifle goes first" <|
                \_ ->
                    ( WeaponChoice.fallbackWeapon Retail (player Fist [ Bfg ] [ ( Cell, 41 ) ])
                    , WeaponChoice.fallbackWeapon Retail (player Fist [ Bfg ] [ ( Cell, 40 ) ])
                    , WeaponChoice.fallbackWeapon Retail (player Fist [ Bfg, Plasma ] [ ( Cell, 41 ) ])
                    )
                        |> Expect.equal ( Bfg, Fist, Plasma )
            , fuzz (Fuzz.pair modeFuzzer playerFuzzer) "the choice is owned or the pistol with bullets, or the fist" <|
                \( mode, p ) ->
                    let
                        w =
                            WeaponChoice.fallbackWeapon mode p
                    in
                    (w == Fist || Player.ownsWeapon w p || (w == Pistol && Player.ammo Clip p > 0))
                        |> Expect.equal True
            ]
        , describe "requestedWeapon"
            [ test "no change button, no change" <|
                \_ -> WeaponChoice.requestedWeapon Retail (player Pistol [ Shotgun ] []) |> Expect.equal Nothing
            , test "asking for an owned weapon" <|
                \_ ->
                    WeaponChoice.requestedWeapon Retail (asking 2 (player Pistol [ Shotgun ] []))
                        |> Expect.equal (Just Shotgun)
            , test "asking for one not owned, or the one held" <|
                \_ ->
                    ( WeaponChoice.requestedWeapon Retail (asking 3 (player Pistol [ Shotgun ] []))
                    , WeaponChoice.requestedWeapon Retail (asking 1 (player Pistol [ Shotgun ] []))
                    )
                        |> Expect.equal ( Nothing, Nothing )
            , test "the fist slot gives the chainsaw, unless holding it with berserk" <|
                \_ ->
                    ( WeaponChoice.requestedWeapon Retail (asking 0 (player Pistol [ Chainsaw ] []))
                    , WeaponChoice.requestedWeapon Retail
                        (asking 0 (player Chainsaw [ Fist ] [] |> Player.setPower Strength 1))
                    , WeaponChoice.requestedWeapon Retail (asking 0 (player Chainsaw [ Fist ] []))
                    )
                        |> Expect.equal ( Just Chainsaw, Just Fist, Nothing )
            , test "the shotgun slot gives the super shotgun in Doom II only" <|
                \_ ->
                    ( WeaponChoice.requestedWeapon Commercial (asking 2 (player Pistol [ Shotgun, SuperShotgun ] []))
                    , WeaponChoice.requestedWeapon Retail (asking 2 (player Pistol [ Shotgun, SuperShotgun ] []))
                    , WeaponChoice.requestedWeapon Commercial (asking 2 (player SuperShotgun [ Shotgun ] []))
                    )
                        |> Expect.equal ( Just SuperShotgun, Just Shotgun, Just Shotgun )
            , test "shareware never changes to the plasma rifle or BFG" <|
                \_ ->
                    ( WeaponChoice.requestedWeapon Shareware (asking 5 (player Pistol [ Plasma ] []))
                    , WeaponChoice.requestedWeapon Registered (asking 6 (player Pistol [ Bfg ] []))
                    )
                        |> Expect.equal ( Nothing, Just Bfg )
            ]
        ]
