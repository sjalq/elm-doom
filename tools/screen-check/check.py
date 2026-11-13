#!/usr/bin/env python3
"""Run intermission and finale scenarios on the original (./harness) and the Elm port
(./screen-check), and compare every frame pixel for pixel and the sounds and music.

    python3 check.py --wad freedoom1.wad --wad2 freedoom2.wad [--only NAME] [--png]

--png writes out/NAME-TIC-c.png / -elm.png for the first differing frame (needs
PLAYPAL; no dependencies beyond the standard library).
"""

import argparse
import os
import struct
import subprocess
import sys
import zlib

W, H = 320, 200

# name, wad ("1" or "2"), arguments shared by both programs
SCENARIOS = [
    # single player, counting through every stat, then the map with the arrow
    ("e1-sp", "1", "-tics 600 -press 300 -wi 0 2 3 0 50 40 5 2625 0 -plyr 0 37 21 3 4100 0 0 0 0"),
    ("e1-sp-skip", "1", "-tics 300 -press 5,40,200 -wi 0 7 8 1 12 9 0 1050 0 -plyr 0 12 9 0 99999 0 0 0 0"),
    ("e2-sp", "1", "-tics 700 -press 420 -wi 1 4 5 1 120 80 9 3150 0 -plyr 0 100 80 9 7000 0 0 0 0"),
    ("e2-secret", "1", "-tics 400 -press 3,40 -wi 1 8 5 1 20 20 2 1000 0 -plyr 0 3 1 2 1300 0 0 0 0"),
    ("e3-sp", "1", "-tics 500 -press 5,60 -wi 2 0 1 0 10 10 0 3150 0 -plyr 0 7 10 0 222 0 0 0 0"),
    ("e4-sp", "1", "-tics 500 -press 5,60 -wi 3 2 3 0 60 30 3 4200 0 -plyr 0 60 15 1 3600 0 0 0 0"),
    ("e1-sucks", "1", "-tics 400 -press 2,30 -wi 0 1 2 0 0 0 0 0 0 -plyr 0 0 0 0 130000 0 0 0 0"),
    # net game: two and four players, with and without frags
    ("e1-net", "1", "-tics 700 -press 600 -net -wi 0 3 4 0 40 30 4 2000 1 "
     "-plyr 0 20 10 1 3000 0 3 1 0 -plyr 1 15 20 3 3000 2 0 0 0 -plyr 2 5 0 0 3000 0 1 -1 0"),
    ("e2-net-nofrags", "1", "-tics 500 -press 450 -net -wi 1 0 1 0 40 30 4 2000 0 "
     "-plyr 0 20 10 1 3000 0 0 0 0 -plyr 3 15 20 3 3000 0 0 0 0"),
    # deathmatch
    ("e1-dm", "1", "-tics 400 -press 300 -dm -net -wi 0 4 5 0 1 1 1 0 2 "
     "-plyr 0 0 0 0 0 0 5 2 0 -plyr 1 0 0 0 0 3 1 0 7 -plyr 2 0 0 0 0 12 0 0 0 -plyr 3 0 0 0 0 1 1 1 -2"),
    # Doom II
    ("map01-sp", "2", "-tics 400 -press 250 -wi 0 0 1 0 30 20 2 1050 0 -plyr 0 30 18 1 1500 0 0 0 0"),
    ("map30-sp", "2", "-tics 300 -press 5,40 -wi 0 29 30 0 30 20 2 6300 0 -plyr 0 1 1 1 9999 0 0 0 0"),
    ("map15-dm", "2", "-tics 300 -press 100 -dm -net -wi 0 14 30 1 1 1 1 0 0 "
     "-plyr 0 0 0 0 0 0 2 0 0 -plyr 1 0 0 0 0 4 0 0 0"),
    # finales
    ("fin-e1", "1", "-tics 2200 -finale 1 8"),
    ("fin-e2", "1", "-tics 2200 -finale 2 8"),
    ("fin-e3", "1", "-tics 3000 -finale 3 8"),
    ("fin-e4", "1", "-tics 2100 -finale 4 8"),
    ("fin-map06", "2", "-tics 300 -press 200 -finale 1 6"),
    ("fin-map15", "2", "-tics 120 -finale 1 15"),
    ("fin-map31", "2", "-tics 200 -finale 1 31"),
    ("fin-map30-cast", "2",
     "-tics 3000 -press 60,61,62,900 -key 400,401,700,1200,1500,1900,2300,2600,2900 -finale 1 30"),
    # the episode maps' animations: Freedoom's WIAeeaaff are 1x1 placeholders, so
    # these use a copy of freedoom1.wad ("A") whose WIA pictures are the big digits
    # (WINUM<frame>), visible and different for every frame
    ("anim-e1", "A", "-tics 400 -press 30,150,300 -wi 0 2 3 0 50 40 5 2625 0 -plyr 0 37 21 3 4100 0 0 0 0"),
    ("anim-e1-secret", "A", "-tics 300 -press 3,40 -wi 0 8 3 1 10 10 1 1000 0 -plyr 0 3 1 1 300 0 0 0 0"),
    ("anim-e2-next8", "A", "-tics 400 -press 5,60,250 -wi 1 6 7 0 10 10 1 1000 0 -plyr 0 3 1 1 300 0 0 0 0"),
    ("anim-e2-next4", "A", "-tics 300 -press 5,60 -wi 1 3 4 0 10 10 1 1000 0 -plyr 0 3 1 1 300 0 0 0 0"),
    ("anim-e3", "A", "-tics 400 -press 30,150,300 -wi 2 4 5 1 10 10 1 1000 0 -plyr 0 3 1 1 300 0 0 0 0"),
    ("anim-e1-net", "A", "-tics 400 -press 300 -net -wi 0 3 4 0 40 30 4 2000 1 "
     "-plyr 0 20 10 1 3000 0 3 1 0 -plyr 1 15 20 3 3000 2 0 0 0"),
    ("anim-e3-dm", "A", "-tics 300 -press 200 -dm -net -wi 2 4 5 0 1 1 1 0 1 "
     "-plyr 0 0 0 0 0 0 5 2 0 -plyr 1 0 0 0 0 3 1 0 0"),
]


def run(cmd):
    r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0:
        sys.exit("failed: %s\n%s%s" % (" ".join(cmd), r.stdout, r.stderr))
    if False:
        print(r.stdout.strip())


def norm_sounds(text):
    """The music calls as "tic music name looping", repeats of the music playing
    dropped (S_ChangeMusic does nothing then); names without the D_ prefix."""
    out, playing = [], None
    for line in text.splitlines():
        parts = line.split()
        if len(parts) >= 3 and parts[1] == "music":
            name = parts[2].lower()
            if name.startswith("d_"):
                name = name[2:]
            if name == playing:
                continue
            playing = name
            out.append("%s music %s %s" % (parts[0], name, parts[3] if len(parts) > 3 else "1"))
        elif len(parts) >= 3:
            out.append("%s sfx %s" % (parts[0], parts[2].lower()))
    return out


def png(path, pixels, palette):
    rows = b"".join(b"\0" + bytes(c for i in pixels[y * W:(y + 1) * W] for c in palette[i]) for y in range(H))

    def chunk(t, d):
        return struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d) & 0xFFFFFFFF)

    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", W, H, 8, 2, 0, 0, 0))
                + chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))


def lump(wad, wanted):
    with open(wad, "rb") as f:
        data = f.read()
    n, off = struct.unpack("<II", data[4:12])
    found = None
    for i in range(n):
        pos, size, name = struct.unpack("<II8s", data[off + 16 * i: off + 16 * i + 16])
        if name.rstrip(b"\0") == wanted:
            found = data[pos:pos + size]
    return found


def anim_wad(wad):
    """A copy of the WAD with every WIAeeaaff lump replaced by WINUM<ff+1>."""
    path = "out/anim-%s" % os.path.basename(wad)
    if os.path.exists(path):
        return path
    with open(wad, "rb") as f:
        data = f.read()
    n, off = struct.unpack("<II", data[4:12])
    entries = [struct.unpack("<II8s", data[off + 16 * i: off + 16 * i + 16]) for i in range(n)]
    lumps = [(name, data[pos:pos + size]) for pos, size, name in entries]
    digits = {name.rstrip(b"\0"): body for name, body in lumps}
    out, directory = bytearray(b"IWAD" + b"\0" * 8), []
    for name, body in lumps:
        bare = name.rstrip(b"\0")
        if bare.startswith(b"WIA") and len(bare) == 8:
            body = digits[b"WINUM%d" % (int(bare[6:8]) + 1)]
        directory.append(struct.pack("<II8s", len(out), len(body), name))
        out += body
    out[4:12] = struct.pack("<II", n, len(out))
    out += b"".join(directory)
    with open(path, "wb") as f:
        f.write(out)
    return path


def playpal(wad):
    raw = lump(wad, b"PLAYPAL")
    if raw is None:
        return [(k, k, k) for k in range(256)]
    return [tuple(raw[3 * k:3 * k + 3]) for k in range(256)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--wad", required=True)
    ap.add_argument("--wad2", required=True)
    ap.add_argument("--only")
    ap.add_argument("--png", action="store_true")
    a = ap.parse_args()
    os.makedirs("out", exist_ok=True)
    failed = 0
    for name, which, args in SCENARIOS:
        if a.only and name not in a.only.split(","):
            continue
        wad = {"1": a.wad, "2": a.wad2, "A": a.wad}[which]
        if which == "A" and os.path.exists(wad):
            wad = anim_wad(wad)
        if not os.path.exists(wad):
            print("%-16s skipped (no %s)" % (name, wad))
            continue
        argv = args.split()
        run(["./harness", "-iwad", wad, "-out", "out/%s-c" % name] + argv)
        run(["./screen-check", "-iwad", wad, "-out", "out/%s-elm" % name] + argv)
        with open("out/%s-c.frames" % name, "rb") as f:
            cf = f.read()
        with open("out/%s-elm.frames" % name, "rb") as f:
            ef = f.read()
        with open("out/%s-c.sounds" % name) as f:
            cs = norm_sounds(f.read())
        with open("out/%s-elm.sounds" % name) as f:
            es = norm_sounds(f.read())
        problems = []
        if len(cf) != len(ef):
            problems.append("%d frames vs %d" % (len(cf) // (W * H), len(ef) // (W * H)))
        frames = min(len(cf), len(ef)) // (W * H)
        diffs = 0
        first = None
        for t in range(frames):
            a0, b0 = cf[t * W * H:(t + 1) * W * H], ef[t * W * H:(t + 1) * W * H]
            if a0 != b0:
                n = sum(1 for x, y in zip(a0, b0) if x != y)
                diffs += 1
                if first is None:
                    first = (t, n)
                    if a.png:
                        pal = playpal(wad)
                        png("out/%s-%05d-c.png" % (name, t), a0, pal)
                        png("out/%s-%05d-elm.png" % (name, t), b0, pal)
        if diffs:
            problems.append("%d frames differ, first tic %d (%d pixels)" % (diffs, first[0], first[1]))
        if cs != es:
            for i, (x, y) in enumerate(zip(cs + ["<end>"] * len(es), es + ["<end>"] * len(cs))):
                if x != y:
                    problems.append("sounds differ at #%d: C %r, Elm %r" % (i, x, y))
                    break
        if problems:
            failed += 1
            print("%-16s FAIL %s" % (name, "; ".join(problems)))
        else:
            print("%-16s ok   %d frames identical, %d sounds" % (name, frames, len(cs)))
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
