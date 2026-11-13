module Doom.Audio.Sound exposing
    ( Model, Settings, defaults, init
    , startLevel, play, startSound, startSoundDrawn, updateSounds
    , changeMusic, startMusic, stopMusic, levelMusic, pauseSound, resumeSound
    , setSfxVolume, setMusicVolume, sfxVolume, musicVolume
    , soundTics, lumpName
    )

{-| Sound (s\_sound.c): which of the channels a sound goes on and which sound it
pushes off, how loud it is and where it is in the stereo picture from where it is
and where the listener is, keeping moving sounds up to date, and which music each
level has. It gives back `Doom.Audio.Command`s for the host to play, with the C
source's channel numbers.

Volumes are set with the menu's 0 to 15 and used, as DOS Doom v1.9 did, multiplied
by 8 (0 to 120 of DMX's 127; linuxdoom-1.10 commented the `* 8` out, which made its
sounds fade out short of S\_CLIPPING\_DIST and its boss-level floor of 15 loud).
A sound counts as playing for its length: a
lump of N samples at R samples a second started at tic T plays while the game's tic
count is below `T + ceiling (N * 35 / R)` (the C oracle's I\_SoundIsPlaying).


# Starting

@docs Model, Settings, defaults, init


# Sounds

@docs startLevel, play, startSound, startSoundDrawn, updateSounds


# Music

@docs changeMusic, startMusic, stopMusic, levelMusic, pauseSound, resumeSound


# Volume

@docs setSfxVolume, setMusicVolume, sfxVolume, musicVolume


# The WAD's sounds

@docs soundTics, lumpName

-}

import Array exposing (Array)
import Bitwise
import Bytes
import Bytes.Decode as D
import Doom.Angle as Angle
import Doom.Audio exposing (Command(..))
import Doom.Defs exposing (GameMode(..))
import Doom.Fixed as Fixed exposing (Fixed)
import Doom.Geometry as Geometry
import Doom.Info.Sfx as Sfx exposing (Sfx)
import Doom.Int32 as Int32
import Doom.Level as Level
import Doom.Play.Env exposing (Env, Params)
import Doom.Random as Random exposing (Rng)
import Doom.Sound as Sound exposing (Origin(..), Spot)
import Doom.Tables as Tables
import Doom.Wad as Wad exposing (Wad)
import Doom.World as World exposing (World)


{-| The channels and what is on them, the volumes, the music playing, and each
sound's lump and length.
-}
type Model
    = Model
        { channels : Array (Maybe Channel)
        , sfxVolume : Int
        , musicVolume : Int
        , music : Maybe String
        , musicPaused : Bool
        , lumps : Array SfxLump
        }


{-| channel\_t: the sound on a channel, where it comes from, and the tic it ends.
-}
type alias Channel =
    { sfx : Sfx
    , origin : Origin
    , ends : Int
    }


type alias SfxLump =
    { lump : String
    , tics : Int
    }


{-| The settings from the config file: `snd_channels` (how many sounds play at
once), `sfx_volume` and `music_volume` (the menu's 0 to 15).
-}
type alias Settings =
    { channels : Int
    , sfxVolume : Int
    , musicVolume : Int
    }


{-| m\_misc.c's defaults: 3 channels, both volumes 8.
-}
defaults : Settings
defaults =
    { channels = 3, sfxVolume = 8, musicVolume = 8 }


{-| S\_Init: no sounds playing, no music, the volumes set (which the host is told
of), and each sound's lump found in the WAD (I\_InitSound's getsfx: a linked sound
plays its link's lump; a sound the WAD hasn't got plays DSPISTOL).
-}
init : Wad -> Settings -> ( Model, List Command )
init wad settings =
    let
        model =
            Model
                { channels = Array.repeat (max 1 settings.channels) Nothing
                , sfxVolume = scaled settings.sfxVolume
                , musicVolume = scaled settings.musicVolume
                , music = Nothing
                , musicPaused = False
                , lumps = Array.map (sfxLump wad) Sfx.all
                }
    in
    ( model, SfxVolume (scaled settings.sfxVolume) :: musicVolumeCommands (scaled settings.musicVolume) )


{-| The menu's 0 to 15 as the sound code's 0 to 120 (DOS's `* 8`).
-}
scaled : Int -> Int
scaled v =
    clamp 0 15 v * 8


sfxLump : Wad -> Sfx -> SfxLump
sfxLump wad sfx =
    let
        info =
            Sfx.info sfx

        name =
            case info.link of
                Just link ->
                    (Sfx.info link).name

                Nothing ->
                    info.name

        wanted =
            "DS" ++ String.toUpper name

        lump =
            case Wad.checkNumForName wanted wad of
                Just _ ->
                    wanted

                Nothing ->
                    "DSPISTOL"
    in
    { lump = lump
    , tics =
        Wad.lump lump wad
            |> Maybe.andThen (D.decode dmxHeader)
            |> Maybe.map
                (\( rate, samples ) ->
                    let
                        r =
                            if rate == 0 then
                                1

                            else
                                rate
                    in
                    (samples * 35 + r - 1) // r
                )
            |> Maybe.withDefault 0
    }


dmxHeader : D.Decoder ( Int, Int )
dmxHeader =
    D.map3 (\_ rate samples -> ( rate, samples ))
        (D.unsignedInt16 Bytes.LE)
        (D.unsignedInt16 Bytes.LE)
        (D.unsignedInt32 Bytes.LE)


{-| How many tics a sound plays for.
-}
soundTics : Sfx -> Model -> Int
soundTics sfx (Model m) =
    Array.get (Sfx.index sfx) m.lumps |> Maybe.map .tics |> Maybe.withDefault 0


{-| The lump a sound plays (DSPISTOL for `Chgun`).
-}
lumpName : Sfx -> Model -> String
lumpName sfx (Model m) =
    Array.get (Sfx.index sfx) m.lumps |> Maybe.map .lump |> Maybe.withDefault "DSPISTOL"



-- WHERE THINGS ARE


{-| What s\_sound.c reads of the world for one sound: the map number (map 8 is the
boss level, heard from anywhere), the listener (the console player's body), and
where an origin is.
-}
type alias Hearing =
    { map : Int
    , listener : Maybe Spot
    , place : Origin -> Maybe ( Fixed, Fixed )
    }


{-| Where things are now: for S\_UpdateSounds, and sounds given without a moment.
-}
hearingNow : Env -> World -> Hearing
hearingNow env world =
    { map = env.params.map
    , listener = spotOf (World.playerMobj env.params.consoleplayer world)
    , place =
        \origin ->
            case origin of
                FromMobj id ->
                    World.mobj id world |> Maybe.map (\mo -> ( mo.x, mo.y ))

                FromSector s ->
                    sectorPlace env s

                FromButton i ->
                    World.buttonOrigin i world |> (\b -> Just ( b.x, b.y ))

                Ambient ->
                    Nothing
    }


{-| Where things were when a sound started.
-}
hearingAt : Env -> Sound.Moment -> Hearing
hearingAt env moment =
    { map = env.params.map
    , listener = List.drop env.params.consoleplayer moment.players |> List.head |> Maybe.andThen identity
    , place =
        \origin ->
            case origin of
                FromMobj _ ->
                    moment.origin |> Maybe.map (\s -> ( s.x, s.y ))

                FromButton _ ->
                    moment.origin |> Maybe.map (\s -> ( s.x, s.y ))

                FromSector s ->
                    sectorPlace env s

                Ambient ->
                    Nothing
    }


spotOf : Maybe { a | id : Int, x : Fixed, y : Fixed, angle : Int } -> Maybe Spot
spotOf =
    Maybe.map (\mo -> { mo = mo.id, x = mo.x, y = mo.y, angle = mo.angle })


sectorPlace : Env -> Level.SectorId -> Maybe ( Fixed, Fixed )
sectorPlace env s =
    Array.get s env.level.sectors |> Maybe.map (\sec -> ( sec.soundorgX, sec.soundorgY ))


{-| Whether an origin is the listener's own body (C's `origin == listener`).
-}
isListener : Maybe Spot -> Origin -> Bool
isListener listener origin =
    case ( origin, listener ) of
        ( FromMobj id, Just l ) ->
            id == l.mo

        _ ->
            False



-- STARTING AND STOPPING


{-| S\_Start, at the start of a level (tic `gametic`): stop every sound, and start
the level's music, looping.
-}
startLevel : Int -> Params -> Model -> ( Model, List Command )
startLevel gametic params model =
    let
        ( Model stopped, stops ) =
            stopAllChannels gametic model
    in
    case levelMusic params.gameMode params.episode params.map of
        Just name ->
            changeMusic name True (Model { stopped | musicPaused = False })
                |> Tuple.mapSecond ((++) stops)

        Nothing ->
            ( Model { stopped | musicPaused = False }, stops )


stopAllChannels : Int -> Model -> ( Model, List Command )
stopAllChannels gametic (Model m) =
    List.range 0 (Array.length m.channels - 1)
        |> List.foldl
            (\c ( model, cmds ) ->
                stopChannel gametic c model |> Tuple.mapSecond ((++) cmds)
            )
            ( Model m, [] )


{-| The sounds one tic of a level asked for (tic `gametic`), in order:
S\_StartSound and S\_StopSound.

Most sounds' pitch is varied with M\_Random, the one sequence the wipe, the status
bar's face, the intermission and the finale also draw from: give the game's `Rng`
as it is when the tic's sounds start (after P\_Ticker, before ST\_Ticker) and carry
on with the one given back. Only `rndindex` is used.

-}
play : Int -> Env -> World -> List Sound.Event -> Rng -> Model -> ( Model, Rng, List Command )
play gametic env world events rng0 model0 =
    List.foldl
        (\event ( mdl, rng, cmds ) ->
            let
                ( next, rng2, more ) =
                    case event of
                        Sound.StartAt origin sfx moment ->
                            startSoundAt gametic (hearingAt env moment) origin sfx (Draw rng) mdl

                        Sound.Start origin sfx ->
                            startSoundAt gametic (hearingNow env world) origin sfx (Draw rng) mdl

                        Sound.StartDrawn origin sfx byte ->
                            startSoundAt gametic (hearingNow env world) origin sfx (Drawn byte) mdl
                                |> (\( m2, _, c ) -> ( m2, rng, c ))

                        Sound.Stop id ->
                            stopSound gametic (FromMobj id) mdl |> (\( m2, c ) -> ( m2, rng, c ))
            in
            ( next, rng2, cmds ++ more )
        )
        ( model0, rng0, [] )
        events


{-| S\_StartSound (NULL, sfx) at tic `gametic`: a sound from nowhere in particular
(menus, the intermission, the finale). Draws from M\_Random as `play` does.
-}
startSound : Int -> Sfx -> Rng -> Model -> ( Model, Rng, List Command )
startSound gametic sfx rng =
    startSoundAt gametic nowhere Ambient sfx (Draw rng)


{-| `startSound` with the pitch variation byte already drawn from M\_Random by the
caller (`Doom.Sound.StartDrawn`; ignored for sounds that don't draw one).
-}
startSoundDrawn : Int -> Sfx -> Int -> Model -> ( Model, List Command )
startSoundDrawn gametic sfx byte model =
    startSoundAt gametic nowhere Ambient sfx (Drawn byte) model
        |> (\( m, _, c ) -> ( m, c ))


nowhere : Hearing
nowhere =
    { map = 0, listener = Nothing, place = always Nothing }


{-| Where a sound's pitch variation comes from: M\_Random, or a byte drawn already.
-}
type PitchSource
    = Draw Rng
    | Drawn Int


{-| S\_StartSoundAtVolume with the full volume.
-}
startSoundAt : Int -> Hearing -> Origin -> Sfx -> PitchSource -> Model -> ( Model, Rng, List Command )
startSoundAt gametic hearing origin sfx source ((Model m) as model) =
    let
        rng =
            case source of
                Draw r ->
                    r

                Drawn _ ->
                    Random.init

        info =
            Sfx.info sfx

        sfxId =
            Sfx.index sfx

        ( linkedVolume, pitch0, priority ) =
            case info.link of
                Just _ ->
                    ( min m.sfxVolume (m.sfxVolume + info.volume), info.pitch, info.priority )

                Nothing ->
                    ( m.sfxVolume, 128, 64 )

        linkedAudible =
            case info.link of
                Just _ ->
                    m.sfxVolume + info.volume >= 1

                Nothing ->
                    True

        heard =
            if origin /= Ambient && not (isListener hearing.listener origin) then
                case ( hearing.listener, hearing.place origin ) of
                    ( Just listener, Just ( x, y ) ) ->
                        adjust m.sfxVolume hearing.map listener x y
                            |> Maybe.map
                                (\( v, s ) ->
                                    if x == listener.x && y == listener.y then
                                        ( v, 128 )

                                    else
                                        ( v, s )
                                )

                    _ ->
                        Just ( linkedVolume, 128 )

            else
                Just ( linkedVolume, 128 )
    in
    if sfxId < 1 || not linkedAudible then
        ( model, rng, [] )

    else
        case heard of
            Nothing ->
                ( model, rng, [] )

            Just ( volume, sep ) ->
                let
                    ( byte, rng2 ) =
                        if Sound.drawsPitch sfx then
                            case source of
                                Draw r ->
                                    Random.mRandom r

                                Drawn b ->
                                    ( b, rng )

                        else
                            ( 0, rng )

                    pitch =
                        if sfx == Sfx.Sawup || sfx == Sfx.Sawidl || sfx == Sfx.Sawful || sfx == Sfx.Sawhit then
                            clampByte (pitch0 + 8 - Bitwise.and byte 15)

                        else if Sound.drawsPitch sfx then
                            clampByte (pitch0 + 16 - Bitwise.and byte 31)

                        else
                            pitch0

                    ( stopped, stops ) =
                        stopSound gametic origin model
                in
                case getChannel gametic origin sfx stopped of
                    ( Model got, evictions, Just cnum ) ->
                        let
                            lump =
                                Array.get sfxId got.lumps |> Maybe.withDefault { lump = "DSPISTOL", tics = 0 }
                        in
                        ( Model { got | channels = Array.set cnum (Just { sfx = sfx, origin = origin, ends = gametic + lump.tics }) got.channels }
                        , rng2
                        , stops
                            ++ evictions
                            ++ [ StartSound { channel = cnum, lump = lump.lump, volume = volume, separation = sep, pitch = pitch, priority = priority } ]
                        )

                    ( none, evictions, Nothing ) ->
                        ( none, rng2, stops ++ evictions )


clampByte : Int -> Int
clampByte p =
    clamp 0 255 p


{-| S\_StopSound: stop the first channel playing a sound from this origin (for C's
NULL origin, the first channel with a sound from nowhere in particular).
-}
stopSound : Int -> Origin -> Model -> ( Model, List Command )
stopSound gametic origin ((Model m) as model) =
    case firstIndex (\ch -> ch.origin == origin) m.channels of
        Just c ->
            stopChannel gametic c model

        Nothing ->
            ( model, [] )


firstIndex : (Channel -> Bool) -> Array (Maybe Channel) -> Maybe Int
firstIndex test channels =
    firstIndexFrom 0 test channels


firstIndexFrom : Int -> (Channel -> Bool) -> Array (Maybe Channel) -> Maybe Int
firstIndexFrom i test channels =
    if i >= Array.length channels then
        Nothing

    else
        case Array.get i channels of
            Just (Just ch) ->
                if test ch then
                    Just i

                else
                    firstIndexFrom (i + 1) test channels

            _ ->
                firstIndexFrom (i + 1) test channels


{-| S\_StopChannel: free a channel, telling the host to stop it if its sound is still
playing.
-}
stopChannel : Int -> Int -> Model -> ( Model, List Command )
stopChannel gametic c ((Model m) as model) =
    case Array.get c m.channels of
        Just (Just ch) ->
            ( Model { m | channels = Array.set c Nothing m.channels }
            , if gametic < ch.ends then
                [ StopSound c ]

              else
                []
            )

        _ ->
            ( model, [] )


{-| S\_getChannel: the first free channel, or the one this origin is already making a
sound on; failing that, the first whose sound's priority number is at least the new
one's (a bigger number is less important) is pushed off. `Nothing` if every
channel's sound matters more.
-}
getChannel : Int -> Origin -> Sfx -> Model -> ( Model, List Command, Maybe Int )
getChannel gametic origin sfx ((Model m) as model) =
    let
        n =
            Array.length m.channels

        open i =
            if i >= n then
                Nothing

            else
                case Array.get i m.channels of
                    Just (Just ch) ->
                        if origin /= Ambient && ch.origin == origin then
                            Just ( i, True )

                        else
                            open (i + 1)

                    _ ->
                        Just ( i, False )

        priority =
            (Sfx.info sfx).priority
    in
    case open 0 of
        Just ( i, True ) ->
            let
                ( stopped, cmds ) =
                    stopChannel gametic i model
            in
            ( stopped, cmds, Just i )

        Just ( i, False ) ->
            ( model, [], Just i )

        Nothing ->
            case firstIndex (\ch -> (Sfx.info ch.sfx).priority >= priority) m.channels of
                Just i ->
                    let
                        ( stopped, cmds ) =
                            stopChannel gametic i model
                    in
                    ( stopped, cmds, Just i )

                Nothing ->
                    ( model, [], Nothing )



-- VOLUME AND STEREO


clippingDist : Fixed
clippingDist =
    1200 * 0x00010000


closeDist : Fixed
closeDist =
    160 * 0x00010000


attenuator : Int
attenuator =
    (clippingDist - closeDist) // 0x00010000


stereoSwing : Fixed
stereoSwing =
    96 * 0x00010000


{-| S\_AdjustSoundParams: the volume and separation of a sound at (x, y) for the
listener, or `Nothing` if it is too far away to hear.
-}
adjust : Int -> Int -> Spot -> Fixed -> Fixed -> Maybe ( Int, Int )
adjust sfxVol map listener x y =
    let
        adx =
            Int32.abs (Int32.sub listener.x x)

        ady =
            Int32.abs (Int32.sub listener.y y)

        approxDist =
            Int32.sub (Int32.add adx ady) (Int32.shiftRight 1 (min adx ady))
    in
    if map /= 8 && approxDist > clippingDist then
        Nothing

    else
        let
            angle0 =
                Geometry.pointToAngle2 listener.x listener.y x y

            angle =
                if angle0 > listener.angle then
                    Angle.sub angle0 listener.angle

                else
                    Angle.add angle0 (Angle.sub 0xFFFFFFFF listener.angle)

            sep =
                128 - Int32.shiftRight Fixed.fracbits (Fixed.mul stereoSwing (Tables.finesine (Angle.toFine angle)))

            vol =
                if approxDist < closeDist then
                    sfxVol

                else if map == 8 then
                    15 + Int32.mul (sfxVol - 15) (Int32.shiftRight Fixed.fracbits (clippingDist - min approxDist clippingDist)) // attenuator

                else
                    Int32.mul sfxVol (Int32.shiftRight Fixed.fracbits (Int32.sub clippingDist approxDist)) // attenuator
        in
        if vol > 0 then
            Just ( vol, sep )

        else
            Nothing


{-| S\_UpdateSounds, once a frame after the tics have run (`gametic` is the tic count
after them): free the channels whose sounds have ended, and move the sounds of
things that move (or stop them when they get out of earshot). Give the level and its
world if there is one (between levels, the last one's, as C's pointers still point
there).
-}
updateSounds : Int -> Maybe ( Env, World ) -> Model -> ( Model, List Command )
updateSounds gametic level ((Model m) as model) =
    let
        hearing =
            Maybe.map (\( env, world ) -> hearingNow env world) level
    in
    List.range 0 (Array.length m.channels - 1)
        |> List.foldl
            (\c ( mdl, cmds ) ->
                updateChannel gametic hearing c mdl |> Tuple.mapSecond ((++) cmds)
            )
            ( model, [] )


updateChannel : Int -> Maybe Hearing -> Int -> Model -> ( Model, List Command )
updateChannel gametic hearing c ((Model m) as model) =
    case Array.get c m.channels of
        Just (Just ch) ->
            if gametic < ch.ends then
                let
                    info =
                        Sfx.info ch.sfx

                    -- the volume S_UpdateSounds starts from is replaced by
                    -- S_AdjustSoundParams', so only the link's pitch survives
                    pitch =
                        case info.link of
                            Just _ ->
                                info.pitch

                            Nothing ->
                                128
                in
                if info.link /= Nothing && m.sfxVolume + info.volume < 1 then
                    stopChannel gametic c model

                else
                    case hearing of
                        Just h ->
                            if ch.origin /= Ambient && not (isListener h.listener ch.origin) then
                                case ( h.listener, h.place ch.origin ) of
                                    ( Just listener, Just ( x, y ) ) ->
                                        case adjust m.sfxVolume h.map listener x y of
                                            Just ( v, sep ) ->
                                                ( model, [ UpdateSound { channel = c, volume = v, separation = sep, pitch = pitch } ] )

                                            Nothing ->
                                                stopChannel gametic c model

                                    _ ->
                                        ( model, [] )

                            else
                                ( model, [] )

                        Nothing ->
                            ( model, [] )

            else
                stopChannel gametic c model

        _ ->
            ( model, [] )



-- MUSIC


{-| The music lumps' names without D\_, by musicenum\_t (S\_music[], from 1).
-}
musicNames : Array String
musicNames =
    Array.fromList
        [ ""
        , "e1m1"
        , "e1m2"
        , "e1m3"
        , "e1m4"
        , "e1m5"
        , "e1m6"
        , "e1m7"
        , "e1m8"
        , "e1m9"
        , "e2m1"
        , "e2m2"
        , "e2m3"
        , "e2m4"
        , "e2m5"
        , "e2m6"
        , "e2m7"
        , "e2m8"
        , "e2m9"
        , "e3m1"
        , "e3m2"
        , "e3m3"
        , "e3m4"
        , "e3m5"
        , "e3m6"
        , "e3m7"
        , "e3m8"
        , "e3m9"
        , "inter"
        , "intro"
        , "bunny"
        , "victor"
        , "introa"
        , "runnin"
        , "stalks"
        , "countd"
        , "betwee"
        , "doom"
        , "the_da"
        , "shawn"
        , "ddtblu"
        , "in_cit"
        , "dead"
        , "stlks2"
        , "theda2"
        , "doom2"
        , "ddtbl2"
        , "runni2"
        , "dead2"
        , "stlks3"
        , "romero"
        , "shawn2"
        , "messag"
        , "count2"
        , "ddtbl3"
        , "ampie"
        , "theda3"
        , "adrian"
        , "messg2"
        , "romer2"
        , "tense"
        , "shawn3"
        , "openin"
        , "evil"
        , "ultima"
        , "read_m"
        , "dm2ttl"
        , "dm2int"
        ]


{-| S\_Start's choice of a level's music (the name without D\_): Doom II's maps play
RUNNIN onwards in order, Doom's episodes their own E?M? songs, and The Ultimate
Doom's fourth episode borrows earlier episodes' songs.
-}
levelMusic : GameMode -> Int -> Int -> Maybe String
levelMusic mode episode map =
    let
        spmus =
            [ "e3m4", "e3m2", "e3m3", "e1m5", "e2m7", "e2m4", "e2m6", "e2m5", "e1m9" ]

        byNumber n =
            if n <= 0 then
                Nothing

            else
                Array.get n musicNames
    in
    case mode of
        Commercial ->
            byNumber (33 + map - 1)

        _ ->
            if episode < 4 then
                byNumber (1 + (episode - 1) * 9 + map - 1)

            else
                List.drop (map - 1) spmus |> List.head


{-| S\_ChangeMusic: play a song (its name without D\_, such as "e1m1" or "intro"),
unless it is already the one playing.
-}
changeMusic : String -> Bool -> Model -> ( Model, List Command )
changeMusic name looping model =
    let
        (Model m) =
            model

        key =
            String.toLower name
    in
    if m.music == Just key then
        ( model, [] )

    else
        let
            ( Model stopped, stops ) =
                stopMusic model
        in
        ( Model { stopped | music = Just key }
        , stops ++ [ PlayMusic { lump = "D_" ++ String.toUpper key, looping = looping } ]
        )


{-| S\_StartMusic: a song played once.
-}
startMusic : String -> Model -> ( Model, List Command )
startMusic name =
    changeMusic name False


{-| S\_StopMusic (a paused song is resumed before it is stopped, and the game stays
"paused" for the next song, as in the C source).
-}
stopMusic : Model -> ( Model, List Command )
stopMusic ((Model m) as model) =
    case m.music of
        Just _ ->
            ( Model { m | music = Nothing }
            , if m.musicPaused then
                [ ResumeMusic, StopMusic ]

              else
                [ StopMusic ]
            )

        Nothing ->
            ( model, [] )


{-| S\_PauseSound: pause the music (the game is paused).
-}
pauseSound : Model -> ( Model, List Command )
pauseSound ((Model m) as model) =
    if m.music /= Nothing && not m.musicPaused then
        ( Model { m | musicPaused = True }, [ PauseMusic ] )

    else
        ( model, [] )


{-| S\_ResumeSound.
-}
resumeSound : Model -> ( Model, List Command )
resumeSound ((Model m) as model) =
    if m.music /= Nothing && m.musicPaused then
        ( Model { m | musicPaused = False }, [ ResumeMusic ] )

    else
        ( model, [] )



-- VOLUME


{-| The menu's sound volume slider (0 to 15): S\_SetSfxVolume with 8 times it.
Sounds already playing keep their volume until they are next updated.
-}
setSfxVolume : Int -> Model -> ( Model, List Command )
setSfxVolume v (Model m) =
    ( Model { m | sfxVolume = scaled v }, [ SfxVolume (scaled v) ] )


{-| The menu's music volume slider (0 to 15): S\_SetMusicVolume with 8 times it.
-}
setMusicVolume : Int -> Model -> ( Model, List Command )
setMusicVolume v (Model m) =
    ( Model { m | musicVolume = scaled v }, musicVolumeCommands (scaled v) )


musicVolumeCommands : Int -> List Command
musicVolumeCommands v =
    [ MusicVolume 127, MusicVolume v ]


{-| snd\_SfxVolume (0 to 120).
-}
sfxVolume : Model -> Int
sfxVolume (Model m) =
    m.sfxVolume


{-| snd\_MusicVolume (0 to 120).
-}
musicVolume : Model -> Int
musicVolume (Model m) =
    m.musicVolume
