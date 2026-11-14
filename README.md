# elm-doom

id Software's 1993 Doom engine as a pure Elm program: the game (DOS Doom v1.9, from
the linuxdoom-1.10 source release), its renderer, menus, status bar, automap,
intermission, finale, saved games, sound decisions and music, in Elm with no ports
and no JavaScript. Give it a WAD's bytes, the time and the player's keys and mouse;
it gives back frames of palette indices and the sounds and music to play.

It is a port, not a remake: the same input gives the same game, tic for tic, and the
same screen, pixel for pixel, as the original.

## How exact, and how that is checked

`verify/oracle` is linuxdoom-1.10 built headless, with the few places where DOS v1.9
behaves differently from linuxdoom patched to behave as DOS does (every change is
marked `DOOM-ORACLE` and argued harmless or DOS-faithful; `verify/oracle/Makefile`
lists them). `hosts/headless` is the Elm package run the same way. The checks compare
them:

```
┌──────────────────────────────────────┬──────────────────────────────────────────────┐
│ What                                 │ Checked by                                   │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Game state every tic (players,       │ verify/oracle/compare.py: the same state     │
│ things, sectors, RNG), demos and     │ line per tic from C and Elm, for demos,      │
│ scripted input.                      │ random ticcmd streams and event scripts.     │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Every frame shown (title loop,       │ verify/oracle/compare_frames.py and          │
│ demos, menus, wipes, levels).        │ stream_compare.py, pixel for pixel, with     │
│                                      │ palette and gamma.                           │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Sound decisions (which sound, which  │ verify/oracle/compare_sounds.py and          │
│ channel, volume, separation, pitch). │ tools/soundlog.                              │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Saved games, byte for byte.          │ tools/savecheck against the oracle's         │
│                                      │ -savecheck.                                  │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Status bar, automap, intermission    │ tools/hud-check, tools/automap-check,        │
│ and finale.                          │ tools/screen-check: C harnesses built from   │
│                                      │ the oracle's sources, frame by frame.        │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Music as MIDI (Doom.Audio.Midi).     │ tools/midi-check: byte for byte against      │
│                                      │ Chocolate Doom's mus2mid for every song.     │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ The browser's OPL music synthesis.   │ tools/music-ref: sample for sample against   │
│                                      │ Chocolate Doom's OPL code with Nuked OPL3.   │
├──────────────────────────────────────┼──────────────────────────────────────────────┤
│ Fixed point, angles, geometry, the   │ tests/ (elm-test), with golden vectors       │
│ loaders, the shell, saves.           │ from the C functions (golden_geometry.c).    │
└──────────────────────────────────────┴──────────────────────────────────────────────┘
```

For example, with `doom1.wad` (the shareware IWAD) in `wads/`:

```
hosts/headless/build.sh                 # needs eco, see below
make -C verify/oracle
python3 verify/oracle/compare.py --wad wads/doom1.wad --demo DEMO1
```

says `identical` for every tic of the demo.

## The API

```elm
import Doom
import Doom.Event as Event

Doom.init wadBytes                       -- Result String ( Doom.Model, List Doom.Audio.Command )
Doom.update (Doom.Tick ms) doom          -- ( Doom.Model, List Doom.Audio.Command )
Doom.update (Doom.Event (Event.KeyDown Event.keyEnter)) doom
Doom.view doom                           -- Doom.Screen.Screen: pixels, palette, gamma
```

```
┌──────────────────┬───────────────────────────────────────────────────────────────┐
│ Module           │ What it is for                                                │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom             │ The program: start, update with Tick and Event, view, the     │
│                  │ settings and saved games to keep, whether a game is on.       │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Config      │ What DOS Doom kept in its config file: menu settings, key     │
│                  │ and mouse bindings; and the skill levels.                     │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Event       │ Key and mouse events, by Doom's key codes.                    │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Screen      │ A frame: 320 x 200 palette indices, the palette, the gamma.   │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Video       │ The framebuffer: size, pixels by row or column, as bytes.     │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Audio       │ The sound and music commands the game gives its host.         │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Audio.Midi  │ Doom's MUS music as MIDI files, as Chocolate Doom converts it.│
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ Doom.Wad         │ WAD files: lumps by name (PLAYPAL, sounds, music, GENMIDI).   │
└──────────────────┴───────────────────────────────────────────────────────────────┘
```

Everything else in `src/` is the engine itself. It isn't part of the package's API,
but this repository's hosts, tools and tests use it directly (their `elm.json`s list
`../../src` as a source directory).

## Writing a host

A host is whatever runs the game on something: a web page, an operating system, a
terminal. It has to:

- **Give it a WAD**: the IWAD's bytes to `Doom.init` (or `Doom.start` with
  `Doom.Options`: settings, bindings and saved games from last time, `-warp`,
  `-skill`, a demo to play).
- **Give it time**: `Doom.Tick ms` as often as it can (every animation frame). The
  game runs at 35 tics a second whatever the frame rate, and catches up at most
  `Doom.maxCatchUp` tics at once.
- **Give it the player**: `Doom.Event` key downs and ups (Doom key codes: lower-case
  ASCII for printable keys, `Doom.Event.key*` for the rest) and mouse movement and
  buttons. `Doom.menuActive` and `Doom.inGame` say when the mouse should be the
  game's.
- **Show the screen**: `Doom.view` when `Doom.frames` changes. Turn each palette
  index into a colour with the WAD's PLAYPAL (palette `palette` of the 14) through
  the gamma table row `gamma` (v_video.c's `gammatable`; `hosts/browser/host.js` has
  it), and stretch 320 x 200 to 4:3 (pixels 1.2 times taller than wide).
- **Play the sounds**: each `Doom.Audio.Command` `update` gives back. Sound effects
  are the WAD's DMX lumps (8-bit unsigned PCM with a small header) on numbered
  channels; music is a MUS lump, played with GENMIDI's OPL instruments (or converted
  to MIDI with `Doom.Audio.Midi` for a MIDI synthesizer).
- **Keep what the player keeps**: `Doom.savedGames` when `Doom.saveCount` changes,
  `Doom.settings` and `Doom.bindings`, to hand back next time in `Doom.Options`.

## In this repository

```
┌──────────────────┬───────────────────────────────────────────────────────────────┐
│ Directory        │ What                                                          │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ src/, tests/     │ The package and its tests (elm-test).                         │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ hosts/browser    │ A web page host: canvas, Web Audio sound effects, OPL3 music  │
│                  │ (Nuked OPL3 and Chocolate Doom's music code ported to JS),    │
│                  │ saves and settings in localStorage. build.sh, then serve.sh.  │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ hosts/headless   │ A command line host built with eco (Elm compiled to native    │
│                  │ code): runs demos, scripts and the full loop without a        │
│                  │ screen, and writes frames, sound logs and saves to compare.   │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ verify/oracle    │ linuxdoom-1.10, headless and DOS-faithful, and the scripts    │
│                  │ that compare it with hosts/headless.                          │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ tools/           │ The other checks (see the table above), the generator of the  │
│                  │ info tables from the C source (generate.py), a JS benchmark.  │
├──────────────────┼───────────────────────────────────────────────────────────────┤
│ wads/            │ Where WADs go. Nothing in it is committed.                    │
└──────────────────┴───────────────────────────────────────────────────────────────┘
```

The browser host:

```
hosts/browser/build.sh       # needs Elm 0.19.1 (ELM=/path/to/elm, or elm on PATH)
hosts/browser/serve.sh       # http://localhost:8000/ (fetches Freedoom on first use)
```

The page plays `freedoom1.wad`, or `?wad=doom1.wad` for another WAD in `wads/`, or
any IWAD dropped on it.

The headless host and the eco-built tools need [eco](https://github.com/eco-lang/eco-compiler)
(`ECO=/path/to/eco hosts/headless/build.sh`). The oracle needs a C compiler and make.

## WADs

No WADs are shipped, and none are ever committed. `tools/fetch-freedoom.sh` fetches
[Freedoom](https://freedoom.github.io/) 0.13.0 (a free IWAD under a BSD licence;
the release and its SHA-256 are pinned in the script) into `wads/`, and everything
here defaults to `wads/freedoom1.wad`. Bring your own IWAD otherwise: the
shareware `doom1.wad`, or `doom.wad` and `doom2.wad` if you own them, in `wads/` or
dropped on the browser page.

## Credits and licence

GPL-2.0, as the source it is a port of: id Software's linuxdoom-1.10, released under
the GPL in 1999 (`verify/oracle/src` is a copy of it). DOS behaviour where it differs
from linuxdoom follows Chocolate Doom. The music conversion (`Doom.Audio.Midi`) and
the browser's music player are ports of Chocolate Doom's `mus2mid.c` and
`i_oplmusic.c` (GPL-2.0-or-later); the browser's OPL3 emulator is a port of Nuked
OPL3 by Nuke.YKT (LGPL-2.1-or-later). `tools/music-ref/chocolate` holds the
unmodified Chocolate Doom files those ports are checked against. Doom is a trademark
of id Software; no id Software data is included.
