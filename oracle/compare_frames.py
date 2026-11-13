#!/usr/bin/env python3
"""Compare the frames the Elm port shows with the C oracle's, pixel for pixel.

    compare_frames.py --wad freedoom1.wad --files oracle.frames elm.frames
    compare_frames.py --wad freedoom1.wad [--oracle BIN] [--elm BIN]
                      [--elm-args "ARGS"] [--out DIR] -- GAME ARGS...
    compare_frames.py --wad freedoom1.wad --extract X.frames N [N ...] --out DIR

Frame file (doom-oracle -full -frames FILE): one record per I_FinishUpdate,
    u32 LE frame counter (0, 1, 2, ...: I_FinishUpdate calls before this one)
    u8     palette: PLAYPAL palette 0..13 of the last I_SetPalette call
    u8     usegamma at that call
    64000  screens[0], 320x200 palette indices, row-major
-framesfrom/-framesto only drop records; the counter keeps counting.

Run mode runs
    BIN_ORACLE -full -iwad WAD GAME ARGS... -frames OUT/oracle.frames
    BIN_ELM    ELM_ARGS GAME ARGS... -frames OUT/elm.frames
where ELM_ARGS defaults to "-full -iwad WAD" (the Elm runner mirrors the
oracle's flags), then compares the two files.

The first record that differs in any byte (counter, palette, gamma or a
pixel) is reported: its counter, which header fields differ, how many pixels
differ and their bounding box; OUT/expected.png (oracle), OUT/actual.png
(Elm) and OUT/diff.png (expected darkened, differing pixels in bright
magenta) are written, palette and gamma applied with the WAD's PLAYPAL and
v_video.c's gammatable. --extract writes OUT/frame<N>.png for given counters.

Time rule the Elm runner must reproduce (doom-oracle's I_GetTime goes up by
exactly one per call, NetUpdate does nothing in -singletics): a wipe runs
entirely inside one D_Display; its first melt step gets tics = 2 (wipestart
= I_GetTime() - 1, then one more call), every later step tics = 1, and each
step is one frame (wipe_ScreenWipe, M_Drawer, I_FinishUpdate) until the melt
is done. Details at the top of src/oracle_main.c.

Exit status: 0 identical, 1 they differ (or one stream stops early),
2 a program failed or a file is missing. No third-party modules.
"""

import argparse
import os
import re
import shlex
import struct
import subprocess
import sys
import tempfile
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_ORACLE = os.path.join(HERE, "doom-oracle")
DEFAULT_ELM = os.path.join(HERE, "..", "headless", "doom-headless")
VIDEO_C = os.path.join(HERE, "src", "v_video.c")

W, H = 320, 200
PIX = W * H
REC = 6 + PIX


# ------------------------------------------------------------------ WAD, gamma

def read_playpal(wad):
    with open(wad, "rb") as f:
        data = f.read()
    ident, n, diroff = struct.unpack_from("<4sii", data, 0)
    for i in range(n):
        pos, size, name = struct.unpack_from("<ii8s", data, diroff + 16 * i)
        if name.rstrip(b"\0").upper() == b"PLAYPAL":
            return data[pos:pos + size]
    raise SystemExit("no PLAYPAL in %s" % wad)


def read_gamma():
    try:
        src = open(VIDEO_C).read()
        body = src[src.index("gammatable[5][256]"):]
        body = body[body.index("{") + 1:]
        nums = [int(x) for x in re.findall(r"\d+", body[:body.index("};")])]
        if len(nums) == 5 * 256:
            return [nums[i * 256:(i + 1) * 256] for i in range(5)]
    except (OSError, ValueError):
        pass
    print("warning: gammatable not found in %s, gamma ignored" % VIDEO_C,
          file=sys.stderr)
    return [list(range(256))] * 5


class Colours:
    def __init__(self, wad):
        self.playpal = read_playpal(wad)
        self.gamma = read_gamma()
        self.cache = {}

    def palette(self, pal, gamma):
        key = (pal, gamma)
        if key not in self.cache:
            p = self.playpal[pal * 768:(pal + 1) * 768]
            g = self.gamma[min(gamma, 4)]
            self.cache[key] = bytes(g[c] for c in p)
        return self.cache[key]


# ------------------------------------------------------------------ PNG

def write_png(path, rgb, w, h, scale=1):
    """rgb: bytes of w*h*3."""
    rows = []
    for y in range(h):
        row = rgb[y * w * 3:(y + 1) * w * 3]
        if scale > 1:
            row = b"".join(row[x * 3:x * 3 + 3] * scale for x in range(w))
        line = b"\0" + row
        rows.extend([line] * scale)
    raw = b"".join(rows)

    def chunk(tag, body):
        c = struct.pack(">I", len(body)) + tag + body
        return c + struct.pack(">I", zlib.crc32(tag + body) & 0xffffffff)

    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", w * scale, h * scale, 8, 2, 0, 0, 0))
    png += chunk(b"IDAT", zlib.compress(raw, 9))
    png += chunk(b"IEND", b"")
    with open(path, "wb") as f:
        f.write(png)


def frame_rgb(colours, rec):
    pal = colours.palette(rec[4], rec[5])
    pix = rec[6:]
    out = bytearray(PIX * 3)
    for i, c in enumerate(pix):
        out[i * 3:i * 3 + 3] = pal[c * 3:c * 3 + 3]
    return bytes(out)


def diff_rgb(colours, a, b):
    base = frame_rgb(colours, a)
    out = bytearray(base[i] // 3 for i in range(len(base)))
    pa, pb = a[6:], b[6:]
    for i in range(PIX):
        if pa[i] != pb[i]:
            out[i * 3:i * 3 + 3] = b"\xff\x00\xff"
    return bytes(out)


# ------------------------------------------------------------------ frames

def records(path):
    with open(path, "rb") as f:
        while True:
            rec = f.read(REC)
            if not rec:
                return
            if len(rec) != REC:
                print("%s: truncated record (%d bytes)" % (path, len(rec)),
                      file=sys.stderr)
                return
            yield rec


def counter(rec):
    return struct.unpack_from("<I", rec, 0)[0]


def describe(rec):
    return "frame %d (palette %d, gamma %d)" % (counter(rec), rec[4], rec[5])


def compare(colours, exp_path, act_path, out, scale):
    n = 0
    ea, ab = records(exp_path), records(act_path)
    while True:
        a = next(ea, None)
        b = next(ab, None)
        if a is None or b is None:
            if a is None and b is None:
                print("identical: %d frames" % n)
                return 0
            longer, rec = ("oracle", a) if b is None else ("elm", b)
            print("identical for %d frames, then only %s goes on (%s)"
                  % (n, longer, describe(rec)))
            return 1
        if a != b:
            report(colours, a, b, n, out, scale)
            return 1
        n += 1


def report(colours, a, b, index, out, scale):
    print("first difference at record %d: oracle %s, elm %s"
          % (index, describe(a), describe(b)))
    for name, off in (("counter", None), ("palette", 4), ("gamma", 5)):
        va = counter(a) if off is None else a[off]
        vb = counter(b) if off is None else b[off]
        if va != vb:
            print("  %s differs: oracle %d, elm %d" % (name, va, vb))
    pa, pb = a[6:], b[6:]
    xs, ys, count = [], [], 0
    for i in range(PIX):
        if pa[i] != pb[i]:
            count += 1
            xs.append(i % W)
            ys.append(i // W)
    if count:
        print("  %d pixels differ, bounding box x %d..%d, y %d..%d (%dx%d)"
              % (count, min(xs), max(xs), min(ys), max(ys),
                 max(xs) - min(xs) + 1, max(ys) - min(ys) + 1))
        i = ys[0] * W + xs[0]
        print("  first at (%d,%d): oracle index %d, elm index %d"
              % (xs[0], ys[0], pa[i], pb[i]))
    else:
        print("  pixels identical")
    os.makedirs(out, exist_ok=True)
    write_png(os.path.join(out, "expected.png"), frame_rgb(colours, a), W, H, scale)
    write_png(os.path.join(out, "actual.png"), frame_rgb(colours, b), W, H, scale)
    write_png(os.path.join(out, "diff.png"), diff_rgb(colours, a, b), W, H, scale)
    print("  wrote %s/{expected,actual,diff}.png" % out)


def extract(colours, path, wanted, out, scale):
    wanted = set(wanted)
    os.makedirs(out, exist_ok=True)
    for rec in records(path):
        c = counter(rec)
        if c in wanted:
            p = os.path.join(out, "frame%d.png" % c)
            write_png(p, frame_rgb(colours, rec), W, H, scale)
            print("%s: %s" % (p, describe(rec)))
            wanted.discard(c)
            if not wanted:
                break
    for c in sorted(wanted):
        print("frame %d not in %s" % (c, path), file=sys.stderr)
    return 2 if wanted else 0


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


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog=__doc__)
    ap.add_argument("--wad", required=True, help="IWAD (PLAYPAL, and passed to both runs)")
    ap.add_argument("--files", nargs=2, metavar=("ORACLE_FRAMES", "ELM_FRAMES"))
    ap.add_argument("--extract", nargs="+", metavar=("FRAMES", "N"),
                    help="write OUT/frame<N>.png for the given frame counters")
    ap.add_argument("--oracle", default=DEFAULT_ORACLE)
    ap.add_argument("--elm", default=DEFAULT_ELM)
    ap.add_argument("--elm-args", help='Elm runner arguments before the game '
                    'arguments (default "-full -iwad WAD")')
    ap.add_argument("--out", help="directory for PNGs and run outputs "
                    "(default: a new temporary directory)")
    ap.add_argument("--scale", type=int, default=2, help="PNG pixel scale (default 2)")
    ap.add_argument("game", nargs="*", help="game arguments, after --")
    args = ap.parse_args()

    colours = Colours(args.wad)
    out = args.out or tempfile.mkdtemp(prefix="doom-frames-")

    if args.extract:
        if len(args.extract) < 2:
            ap.error("--extract FRAMES N [N ...]")
        sys.exit(extract(colours, args.extract[0],
                         [int(n) for n in args.extract[1:]], out, args.scale))

    if args.files:
        for p in args.files:
            if not os.path.exists(p):
                print("missing %s" % p, file=sys.stderr)
                sys.exit(2)
        sys.exit(compare(colours, args.files[0], args.files[1], out, args.scale))

    os.makedirs(out, exist_ok=True)
    ofr = os.path.join(out, "oracle.frames")
    efr = os.path.join(out, "elm.frames")
    elm_pre = (shlex.split(args.elm_args) if args.elm_args is not None
               else ["-full", "-iwad", args.wad])
    orc = run("oracle", [args.oracle, "-full", "-iwad", args.wad] + args.game
              + ["-frames", ofr], os.path.join(out, "oracle.err"))
    erc = run("elm", [args.elm] + elm_pre + args.game + ["-frames", efr],
              os.path.join(out, "elm.err"))
    if not os.path.exists(ofr) or not os.path.exists(efr):
        print("a frame file is missing", file=sys.stderr)
        sys.exit(2)
    status = compare(colours, ofr, efr, out, args.scale)
    if (orc != 0 or erc != 0) and status == 0:
        status = 2
    sys.exit(status)


if __name__ == "__main__":
    main()
