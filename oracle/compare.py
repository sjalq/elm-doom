#!/usr/bin/env python3
"""Compare the Elm port with the C oracle, tic by tic.

    compare.py --wad freedoom1.wad --demo DEMO1          [--maxtics N]
    compare.py --wad freedoom1.wad --demo path/to/x.lmp  [--maxtics N]
    compare.py --wad freedoom1.wad --ticcmds run.ticcmds [--maxtics N]
    compare.py --files oracle.txt elm.txt

Runs doom-oracle (C, linuxdoom-1.10) and doom-headless (Elm) on the same input,
each printing one Doom.StateHash line per level tic, and reports the first tic
whose lines differ: both lines and the names of the fields that differ. A
ticcmd stream is handed to the Elm runner as the equivalent v1.9 demo file.

Exit status: 0 identical for every tic, 1 they differ (or one stops early),
2 a program failed or was missing.
"""

import argparse
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_ORACLE = os.path.join(HERE, "doom-oracle")
DEFAULT_ELM = os.path.join(HERE, "..", "headless", "doom-headless")

P_FIELDS = ["x", "y", "z", "angle", "momx", "momy", "health", "armorpoints",
            "readyweapon", "ammo0", "ammo1", "ammo2", "ammo3"]


def parse(line):
    """A state line as an ordered list of (field name, value string)."""
    parts = line.split()
    if len(parts) != 6:
        return [("line", line)]
    lt, r, p, m, h, s = parts
    fields = [("leveltime", lt), ("rng", r[1:])]
    pv = p[1:].split(",")
    if len(pv) == len(P_FIELDS):
        fields += [("p." + n, v) for n, v in zip(P_FIELDS, pv)]
    else:
        fields.append(("p", p[1:]))
    fields += [("count", m[1:]), ("mobjhash", h[1:]), ("sectorhash", s[1:])]
    return fields


def field_diff(a, b):
    fa, fb = parse(a), parse(b)
    if [n for n, _ in fa] != [n for n, _ in fb]:
        return ["(line shape)"]
    return ["%s: oracle %s, elm %s" % (n, va, vb)
            for (n, va), (_, vb) in zip(fa, fb) if va != vb]


def ticcmds_to_lmp(path, out):
    data = open(path, "rb").read()
    nl = data.index(b"\n")
    skill, episode, mapnum = (int(x) for x in data[:nl].split())
    body = data[nl + 1:]
    body = body[:len(body) - len(body) % 4]
    for i in range(0, len(body), 4):
        if body[i] == 0x80:  # the oracle stops the stream there too
            body = body[:i]
            break
    out.write(bytes([109, skill, episode, mapnum, 0, 0, 0, 0, 0, 1, 0, 0, 0])
              + body + bytes([0x80]))


def run(name, cmd, errpath):
    try:
        with open(errpath, "wb") as err:
            proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=err)
    except OSError as e:
        print("%s: cannot run %s: %s" % (name, cmd[0], e), file=sys.stderr)
        sys.exit(2)
    lines = proc.stdout.decode("latin-1").splitlines()
    if proc.returncode != 0:
        print("%s exited with status %d after %d lines (stderr: %s)"
              % (name, proc.returncode, len(lines), errpath), file=sys.stderr)
        tail = open(errpath, "rb").read().decode("latin-1").strip().splitlines()[-5:]
        for t in tail:
            print("  | " + t, file=sys.stderr)
    return lines, proc.returncode


def compare(oracle, elm, maxtics):
    if maxtics is not None:
        oracle, elm = oracle[:maxtics], elm[:maxtics]
    n = min(len(oracle), len(elm))
    for i in range(n):
        if oracle[i] != elm[i]:
            print("first difference at tic %d (line %d)" % (i + 1, i + 1))
            if i > 0:
                print("  last equal: %s" % oracle[i - 1])
            print("  oracle:     %s" % oracle[i])
            print("  elm:        %s" % elm[i])
            for d in field_diff(oracle[i], elm[i]):
                print("    differs   " + d)
            return 1
    if len(oracle) != len(elm):
        shorter = "elm" if len(elm) < len(oracle) else "oracle"
        print("identical for %d tics, then %s stops (oracle %d lines, elm %d lines)"
              % (n, shorter, len(oracle), len(elm)))
        return 1
    print("identical for all %d tics" % n)
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog=__doc__)
    src = ap.add_mutually_exclusive_group(required=True)
    src.add_argument("--demo", help="demo lump name (DEMO1) or demo file")
    src.add_argument("--ticcmds", help="ticcmd stream (gen_ticcmds.py)")
    src.add_argument("--files", nargs=2, metavar=("ORACLE_OUT", "ELM_OUT"),
                     help="compare two saved outputs instead of running anything")
    ap.add_argument("--wad", help="IWAD (needed unless --files)")
    ap.add_argument("--maxtics", type=int)
    ap.add_argument("--oracle", default=DEFAULT_ORACLE, help="doom-oracle binary")
    ap.add_argument("--elm", default=DEFAULT_ELM, help="doom-headless binary")
    ap.add_argument("--keep", metavar="DIR",
                    help="save both outputs and stderrs in DIR")
    args = ap.parse_args()

    if args.files:
        a = open(args.files[0]).read().splitlines()
        b = open(args.files[1]).read().splitlines()
        sys.exit(compare(a, b, args.maxtics))

    if not args.wad:
        ap.error("--wad is required")

    work = args.keep or tempfile.mkdtemp(prefix="doom-compare-")
    os.makedirs(work, exist_ok=True)
    max_o = ["-maxtics", str(args.maxtics)] if args.maxtics is not None else []
    max_e = [str(args.maxtics)] if args.maxtics is not None else []

    if args.ticcmds:
        lmp = os.path.join(work, "ticcmds.lmp")
        with open(lmp, "wb") as fh:
            ticcmds_to_lmp(args.ticcmds, fh)
        oracle_cmd = [args.oracle, "-iwad", args.wad, "-ticcmds", args.ticcmds] + max_o
        elm_cmd = [args.elm, args.wad, lmp] + max_e
    else:
        oracle_cmd = [args.oracle, "-iwad", args.wad, "-playdemo", args.demo] + max_o
        elm_cmd = [args.elm, args.wad, args.demo] + max_e

    oracle, orc = run("oracle", oracle_cmd, os.path.join(work, "oracle.err"))
    elm, erc = run("elm", elm_cmd, os.path.join(work, "elm.err"))
    if args.keep:
        for name, lines in (("oracle.txt", oracle), ("elm.txt", elm)):
            with open(os.path.join(work, name), "w") as fh:
                fh.write("\n".join(lines) + ("\n" if lines else ""))

    status = compare(oracle, elm, args.maxtics)
    if (orc != 0 or erc != 0) and status == 0:
        status = 2
    sys.exit(status)


if __name__ == "__main__":
    main()
