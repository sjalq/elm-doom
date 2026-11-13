module SpecTests exposing (suite)

{-| Texture and flat animation (`Doom.Play.Spec.textureTranslation` and
`flatTranslation`): what P\_UpdateSpecials would have put in `texturetranslation[]`
and `flattranslation[]`.
-}

import Array
import Dict
import Doom.Data exposing (Anim, Data)
import Doom.Defs exposing (GameMode(..), Skill(..))
import Doom.Play.Env as Env exposing (Env)
import Doom.Play.Spec as Spec
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, fuzz2, test)


nukage : Anim
nukage =
    { istexture = False, picnum = 12, basepic = 10, numpics = 3, speed = 8 }


blodgr : Anim
blodgr =
    { istexture = True, picnum = 23, basepic = 20, numpics = 4, speed = 8 }


env : Env
env =
    { level =
        { name = "TEST"
        , vertexes = Array.empty
        , sectors = Array.empty
        , sides = Array.empty
        , lines = Array.empty
        , segs = Array.empty
        , subsectors = Array.empty
        , nodes = Array.empty
        , blockmap = { orgx = 0, orgy = 0, width = 0, height = 0, cells = Array.empty }
        , reject = Array.empty
        , things = []
        }
    , data = data
    , params =
        { gameMode = Shareware
        , skill = Medium
        , episode = 1
        , map = 1
        , deathmatch = 0
        , netgame = False
        , respawnmonsters = False
        , fast = False
        , nomonsters = False
        , consoleplayer = 0
        }
    , hooks =
        Env.Hooks
            { setMobjState = \_ _ _ w -> ( True, w )
            , setPsprite = \_ _ _ _ w -> w
            , crossSpecialLine = \_ _ _ _ w -> w
            , shootSpecialLine = \_ _ _ w -> w
            , useSpecialLine = \_ _ _ _ w -> ( False, w )
            }
    }


data : Data
data =
    { gameMode = Shareware
    , textures = Array.empty
    , textureNums = Dict.empty
    , flats = Array.empty
    , flatNums = Dict.empty
    , skyFlatNum = 0
    , switchList = Array.empty
    , anims = [ nukage, blodgr ]
    }


leveltime : Fuzz.Fuzzer Int
leveltime =
    Fuzz.intRange 0 1000000


suite : Test
suite =
    describe "Doom.Play.Spec animation"
        [ test "C's formula: basepic + (leveltime / speed + pic) % numpics" <|
            \_ ->
                List.map (Spec.flatTranslation env 0) [ 10, 11, 12 ]
                    |> Expect.equal [ 11, 12, 10 ]
        , test "a flat moves on one frame every `speed` tics" <|
            \_ ->
                List.map (\t -> Spec.flatTranslation env t 10) [ 0, 7, 8, 15, 16, 24 ]
                    |> Expect.equal [ 11, 11, 12, 12, 10, 11 ]
        , fuzz2 leveltime (Fuzz.intRange 20 23) "textures cycle with period numpics * speed" <|
            \t pic ->
                Spec.textureTranslation env (t + 32) pic
                    |> Expect.equal (Spec.textureTranslation env t pic)
        , fuzz2 leveltime (Fuzz.intRange 10 12) "flats stay within their animation" <|
            \t pic ->
                Spec.flatTranslation env t pic
                    |> Expect.all [ Expect.atLeast 10, Expect.atMost 12 ]
        , fuzz leveltime "one cycle shows every frame of a texture once" <|
            \t ->
                List.map (\k -> Spec.textureTranslation env (t + 8 * k) 20) [ 0, 1, 2, 3 ]
                    |> List.sort
                    |> Expect.equal [ 20, 21, 22, 23 ]
        , fuzz2 leveltime (Fuzz.intRange 0 100) "pictures outside every animation are themselves" <|
            \t pic ->
                if pic >= 10 && pic <= 12 then
                    Expect.pass

                else
                    Spec.flatTranslation env t pic |> Expect.equal pic
        , fuzz leveltime "flat animations don't touch textures with the same numbers" <|
            \t ->
                Spec.textureTranslation env t 11 |> Expect.equal 11
        ]
