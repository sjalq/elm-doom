#!/usr/bin/env python3
"""Full-game example runs of doom-oracle (make frames-check).

    frames_check.py --wad freedoom1.wad [--bin ./doom-oracle]
                    [--san ./doom-oracle-san] [--only NAME ...]

Each case runs doom-oracle -full and keeps its outputs in out/:
    out/<case>.frames   -frames dump        out/<case>.snd    -soundlog
    out/<case>.state    state lines         out/<case>.err    stderr
    out/<case>.saves/   -savedir (emptied before every run)
then runs it a second time and, with --san, under the AddressSanitizer +
UBSan build: every output (frames, sound log, state lines, savegames) must
be byte-identical across the three runs, and the sanitizer run must report
nothing. The repeat runs are hashed and deleted, only the first is kept.

Cases:
    title    no arguments: the title loop (TITLEPIC, demos, CREDIT, ...),
             3000 iterations
    demoN    -playdemo DEMON (N = 1..4), to the demo's end
    session  events/session.events: menu, new game on E1M1, walk,
             automap, save to slot 0, walk, load slot 0, menu
    exit     -warp 2 3 + events/exit.events: noclip cheat to the
             corridor west of the exit, walk over the exit line,
             intermission (tally skipped with use), entering E2M4
"""

import argparse
import hashlib
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "out")

CASES = {
    "title": ["-iterations", "3000"],
    "demo1": ["-playdemo", "DEMO1"],
    "demo2": ["-playdemo", "DEMO2"],
    "demo3": ["-playdemo", "DEMO3"],
    "demo4": ["-playdemo", "DEMO4"],
    "session": ["-iterations", "560",
                "-events", os.path.join(HERE, "events", "session.events")],
    "exit": ["-warp", "2", "3", "-iterations", "600",
             "-events", os.path.join(HERE, "events", "exit.events")],
}


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def run(binary, wad, name, tag, env=None):
    """Run one case; returns {output: sha256} and the paths it wrote."""
    base = os.path.join(OUT, name + tag)
    saves = base + ".saves"
    shutil.rmtree(saves, ignore_errors=True)
    os.makedirs(saves)
    cmd = ([binary, "-full", "-iwad", wad] + CASES[name]
           + ["-frames", base + ".frames", "-soundlog", base + ".snd",
              "-savedir", saves])
    with open(base + ".state", "wb") as so, open(base + ".err", "wb") as se:
        rc = subprocess.run(cmd, stdout=so, stderr=se, env=env).returncode
    if rc != 0:
        print("%s%s: exit status %d, see %s.err" % (name, tag, rc, base))
        sys.exit(1)
    digests = {ext: sha(base + ext) for ext in (".frames", ".snd", ".state")}
    for f in sorted(os.listdir(saves)):
        digests["saves/" + f] = sha(os.path.join(saves, f))
    return digests, base


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog=__doc__)
    ap.add_argument("--wad", required=True)
    ap.add_argument("--bin", default=os.path.join(HERE, "doom-oracle"))
    ap.add_argument("--san", help="sanitizer build to run as well")
    ap.add_argument("--only", nargs="+", choices=sorted(CASES))
    args = ap.parse_args()
    wad = os.path.abspath(args.wad)
    os.makedirs(OUT, exist_ok=True)

    for name in args.only or CASES:
        first, base = run(args.bin, wad, name, "")
        frames = os.path.getsize(base + ".frames") // (6 + 64000)
        again, abase = run(args.bin, wad, name, ".again")
        runs = [("second run", again, abase)]
        if args.san:
            env = dict(os.environ, ASAN_OPTIONS="detect_leaks=0",
                       UBSAN_OPTIONS="halt_on_error=1")
            sd, sbase = run(args.san, wad, name, ".san", env)
            if "runtime error" in open(sbase + ".err", errors="replace").read():
                print("%s: UBSan reports, see %s.err" % (name, sbase))
                sys.exit(1)
            runs.append(("sanitizer build", sd, sbase))
        for what, d, b in runs:
            if d != first:
                diff = sorted(k for k in set(d) | set(first) if d.get(k) != first.get(k))
                print("%s: %s differs in %s (kept as %s.*)" % (name, what, ", ".join(diff), b))
                sys.exit(1)
            for ext in (".frames", ".snd", ".state", ".err"):
                os.remove(b + ext)
            shutil.rmtree(b + ".saves", ignore_errors=True)
        print("%s: %d frames, %d sound lines, %d state lines, %d savegames: "
              "deterministic%s" % (
                  name, frames, sum(1 for _ in open(base + ".snd")),
                  sum(1 for _ in open(base + ".state")),
                  sum(1 for k in first if k.startswith("saves/")),
                  ", sanitizer-clean, identical across builds" if args.san else ""))


if __name__ == "__main__":
    main()
