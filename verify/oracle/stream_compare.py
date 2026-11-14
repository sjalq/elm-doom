#!/usr/bin/env python3
"""Compare the oracle's and the Elm runner's frames and sound logs without dumps.

    stream_compare.py --wad WAD [--oracle BIN] [--elm BIN] -- GAME ARGS...

Runs
    BIN_ORACLE -full -iwad WAD GAME ARGS... -savedir T/so -frames T/o.fifo -soundlog T/o.snd
    BIN_ELM    -full -iwad WAD GAME ARGS... -savedir T/se -frames T/e.fifo -soundlog T/e.snd
side by side (each with its own empty -savedir), the frame records going
through FIFOs and compared as they arrive, so a whole title loop (~25000
frames, 1.6 GB) is checked with nothing written to disk. Record format:
compare_frames.py. Reports the frames compared, how many differ and the
first that does (counter, palette/gamma of each, differing pixels); then the
sound logs as compare_sounds.py does. Locate a difference with
compare_frames.py run mode and -framesfrom/-framesto around that counter.

A dummy writer keeps each FIFO open, so the Elm runner (which opens its
-frames file again for every batch it appends) never ends the stream early;
the end of a stream is its program's exit.

Exit status: 0 identical, 1 they differ, 2 a program failed.
No third-party modules.
"""

import argparse
import fcntl
import os
import select
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
REC = 6 + 320 * 200


def open_fifo(path):
    r = os.open(path, os.O_RDONLY | os.O_NONBLOCK)
    w = os.open(path, os.O_WRONLY)
    fcntl.fcntl(r, fcntl.F_SETFL, fcntl.fcntl(r, fcntl.F_GETFL) & ~os.O_NONBLOCK)
    return r, w


def read_record(fd, proc):
    b = bytearray()
    while len(b) < REC:
        ready, _, _ = select.select([fd], [], [], 1.0)
        if ready:
            b += os.read(fd, REC - len(b))
        elif proc.poll() is not None:
            ready, _, _ = select.select([fd], [], [], 0)
            if not ready:
                break
    return bytes(b)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--wad", required=True)
    ap.add_argument("--oracle", default=os.path.join(HERE, "doom-oracle"))
    ap.add_argument("--elm", default=os.path.join(HERE, "..", "..", "hosts", "headless", "doom-headless"))
    ap.add_argument("game", nargs=argparse.REMAINDER)
    a = ap.parse_args()
    game = a.game[1:] if a.game[:1] == ["--"] else a.game
    t = tempfile.mkdtemp(prefix="stream-compare-")
    try:
        procs, fds = [], []
        for tag, binary in (("o", a.oracle), ("e", a.elm)):
            fifo = os.path.join(t, tag + ".fifo")
            os.mkfifo(fifo)
            os.mkdir(os.path.join(t, "s" + tag))
            fds.append(open_fifo(fifo)[0])
            procs.append(subprocess.Popen(
                [binary, "-full", "-iwad", a.wad] + game +
                ["-savedir", os.path.join(t, "s" + tag), "-frames", fifo,
                 "-soundlog", os.path.join(t, tag + ".snd")],
                stdout=subprocess.DEVNULL, stderr=open(os.path.join(t, tag + ".err"), "w")))
        n, differing, first = 0, 0, None
        while True:
            x = read_record(fds[0], procs[0])
            y = read_record(fds[1], procs[1])
            if not x and not y:
                break
            if len(x) != REC or len(y) != REC:
                print("one stream stops early, after %d frames (oracle %d bytes, elm %d bytes left)"
                      % (n, len(x), len(y)))
                first = first or "length"
                break
            if x != y:
                differing += 1
                if first is None:
                    px = sum(1 for i in range(6, REC) if x[i] != y[i])
                    first = ("frame %d (elm %d): palette/gamma oracle %d/%d, elm %d/%d, %d pixels differ"
                             % (int.from_bytes(x[:4], "little"), int.from_bytes(y[:4], "little"),
                                x[4], x[5], y[4], y[5], px))
            n += 1
        for fd, p in zip(fds, procs):
            while read_record(fd, p):
                pass
            p.wait()
        print("frames compared: %d, differing: %d" % (n, differing))
        if first:
            print("first difference:", first)
        rc = 0 if first is None else 1
        if procs[0].returncode or procs[1].returncode:
            print("exit codes: oracle %d, elm %d" % (procs[0].returncode, procs[1].returncode))
            for tag in "oe":
                print(open(os.path.join(t, tag + ".err")).read()[-400:])
            rc = 2
        s = subprocess.run([sys.executable, os.path.join(HERE, "compare_sounds.py"), "--files",
                            os.path.join(t, "o.snd"), os.path.join(t, "e.snd")],
                           capture_output=True, text=True)
        print("sound:", s.stdout.strip().replace("\n", " | "))
        if s.returncode and not rc:
            rc = 1
        return rc
    finally:
        shutil.rmtree(t, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
