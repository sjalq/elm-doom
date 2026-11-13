#!/usr/bin/env python3
"""Compare the Elm port's sound decisions with the C oracle's, line by line.

    compare_sounds.py --files oracle.snd elm.snd
    compare_sounds.py --wad freedoom1.wad [--oracle BIN] [--elm BIN]
                      [--elm-args "ARGS"] [--out DIR] -- GAME ARGS...

Sound log (doom-oracle -full -soundlog FILE): one line per call of the
headless sound layer by s_sound.c, "<iteration> " followed by
    start <channel> <lumpname> <vol> <sep> <pitch> <priority>
    update <channel> <vol> <sep> <pitch>
    stop <channel>
    music <lumpname> <looping>
    musicstop | musicpause | musicresume
    musicvolume <v>          (every I_SetMusicVolume call)
    sfxvolume <v>            (every S_SetSfxVolume call)
iteration is the 0-based D_DoomLoop iteration, -1 before the loop starts.
"Is playing" rule (src/i_headless.c): a sound started at gametic T plays
while gametic < T + ceil(N*35/R), N and R the sample count (u32 at lump
offset 4) and rate (u16 at offset 2) of the lump the mixer would play.

Run mode runs
    BIN_ORACLE -full -iwad WAD GAME ARGS... -soundlog OUT/oracle.snd
    BIN_ELM    ELM_ARGS GAME ARGS... -soundlog OUT/elm.snd
with ELM_ARGS defaulting to "-full -iwad WAD", and compares the logs.

Exit status: 0 identical, 1 they differ (or one stops early), 2 a program
failed or a file is missing.
"""

import argparse
import os
import shlex
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_ORACLE = os.path.join(HERE, "doom-oracle")
DEFAULT_ELM = os.path.join(HERE, "..", "headless", "doom-headless")


def compare(a, b):
    n = min(len(a), len(b))
    for i in range(n):
        if a[i] != b[i]:
            print("first difference at line %d" % (i + 1))
            if i > 0:
                print("  last equal: %s" % a[i - 1])
            print("  oracle:     %s" % a[i])
            print("  elm:        %s" % b[i])
            return 1
    if len(a) != len(b):
        longer, rest = ("oracle", a) if len(a) > len(b) else ("elm", b)
        print("identical for %d lines, then only %s goes on: %s"
              % (n, longer, rest[n]))
        return 1
    print("identical: %d lines" % n)
    return 0


def run(name, cmd, errpath):
    print("%s: %s" % (name, " ".join(shlex.quote(c) for c in cmd)))
    try:
        with open(errpath, "wb") as err:
            proc = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=err)
    except OSError as e:
        print("%s: cannot run %s: %s" % (name, cmd[0], e), file=sys.stderr)
        sys.exit(2)
    if proc.returncode != 0:
        print("%s exited with status %d (stderr: %s)"
              % (name, proc.returncode, errpath), file=sys.stderr)
    return proc.returncode


def lines(path):
    try:
        return open(path).read().splitlines()
    except OSError as e:
        print("cannot read %s: %s" % (path, e), file=sys.stderr)
        sys.exit(2)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog=__doc__)
    ap.add_argument("--files", nargs=2, metavar=("ORACLE_LOG", "ELM_LOG"))
    ap.add_argument("--wad", help="IWAD (run mode)")
    ap.add_argument("--oracle", default=DEFAULT_ORACLE)
    ap.add_argument("--elm", default=DEFAULT_ELM)
    ap.add_argument("--elm-args", help='Elm runner arguments before the game '
                    'arguments (default "-full -iwad WAD")')
    ap.add_argument("--out", help="directory for the logs (default: temporary)")
    ap.add_argument("game", nargs="*", help="game arguments, after --")
    args = ap.parse_args()

    if args.files:
        sys.exit(compare(lines(args.files[0]), lines(args.files[1])))
    if not args.wad:
        ap.error("--wad is required unless --files")

    out = args.out or tempfile.mkdtemp(prefix="doom-sounds-")
    os.makedirs(out, exist_ok=True)
    osnd = os.path.join(out, "oracle.snd")
    esnd = os.path.join(out, "elm.snd")
    elm_pre = (shlex.split(args.elm_args) if args.elm_args is not None
               else ["-full", "-iwad", args.wad])
    orc = run("oracle", [args.oracle, "-full", "-iwad", args.wad] + args.game
              + ["-soundlog", osnd], os.path.join(out, "oracle.err"))
    erc = run("elm", [args.elm] + elm_pre + args.game + ["-soundlog", esnd],
              os.path.join(out, "elm.err"))
    status = compare(lines(osnd), lines(esnd))
    if (orc != 0 or erc != 0) and status == 0:
        status = 2
    sys.exit(status)


if __name__ == "__main__":
    main()
