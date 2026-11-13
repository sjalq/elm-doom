#!/usr/bin/env python3
"""Generate a random but sane single-player ticcmd stream for doom-oracle.

    gen_ticcmds.py --seed N --tics T [--skill 0-4] [--episode E] [--map M]
                   [-o out.ticcmds] [--lmp out.lmp]

The ticcmd file (doom-oracle -ticcmds) is a text header line
"skill episode map" (skill 0..4 as in a demo header, sk_medium = 2) followed
by 4 bytes per tic: forwardmove, sidemove (signed bytes), angleturn high
byte, buttons. --lmp also (or instead) writes the same input as a v1.9 demo
lump file, which both doom-oracle -playdemo and doom-headless accept.

Input comes in bursts of 4..70 tics, each one of: walk/run forward or back,
strafe, turn, walk while turning, stand still, with fire and use held or
tapped at random and an occasional weapon change. Moves stay inside what a
real keyboard produces (|forward| <= 50, |side| <= 40, turns of 512 or 1280
angle units: the demo format keeps only angleturn's high byte), never set
BT_SPECIAL (pause/save) and never produce the demo
end marker (forwardmove -128). The same seed always gives the same stream.
"""

import argparse
import random
import sys

BT_ATTACK = 1
BT_USE = 2
BT_CHANGE = 4
BT_WEAPONSHIFT = 3
DEMO_VERSION = 109
DEMOMARKER = 0x80


def burst(rng):
    """One burst of identical-ish ticcmds: (length, generator of cmds)."""
    length = rng.randint(4, 70)
    kind = rng.choice(["forward", "forward", "back", "strafe", "turn",
                       "walkturn", "walkturn", "still"])
    speed = rng.choice([0, 1])  # walk or run
    fwd = [25, 50][speed]
    side = [24, 40][speed]
    turn = rng.choice([640, 1280]) // 256 * rng.choice([-1, 1])
    hold_fire = rng.random() < 0.3
    hold_use = rng.random() < 0.08
    change = rng.random() < 0.06
    weapon = rng.randint(0, 6)

    cmds = []
    for t in range(length):
        f = s = a = 0
        if kind == "forward":
            f = fwd
        elif kind == "back":
            f = -fwd
        elif kind == "strafe":
            s = side if turn > 0 else -side
        elif kind == "turn":
            a = turn
        elif kind == "walkturn":
            f = fwd
            a = turn
        b = 0
        if hold_fire or rng.random() < 0.05:
            b |= BT_ATTACK
        if (hold_use and t % 8 == 0) or rng.random() < 0.03:
            b |= BT_USE
        if change and t == 0:
            b |= BT_CHANGE | (weapon << BT_WEAPONSHIFT)
        cmds.append((f, s, a, b))
    return cmds


def generate(seed, tics):
    rng = random.Random(seed)
    cmds = []
    while len(cmds) < tics:
        cmds.extend(burst(rng))
    return cmds[:tics]


def pack(cmds):
    out = bytearray()
    for f, s, a, b in cmds:
        assert -50 <= f <= 50 and -40 <= s <= 40 and 0 <= b < 0x80
        out += bytes([f & 0xFF, s & 0xFF, a & 0xFF, b])
    return bytes(out)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--tics", type=int, required=True)
    ap.add_argument("--skill", type=int, default=2, choices=range(5))
    ap.add_argument("--episode", type=int, default=1)
    ap.add_argument("--map", type=int, default=1)
    ap.add_argument("-o", "--output", help="ticcmd file (default: stdout unless --lmp)")
    ap.add_argument("--lmp", help="also write the stream as a v1.9 demo lump file")
    args = ap.parse_args()

    body = pack(generate(args.seed, args.tics))
    header = ("%d %d %d\n" % (args.skill, args.episode, args.map)).encode()

    if args.lmp:
        demo = bytes([DEMO_VERSION, args.skill, args.episode, args.map,
                      0, 0, 0, 0, 0, 1, 0, 0, 0]) + body + bytes([DEMOMARKER])
        with open(args.lmp, "wb") as fh:
            fh.write(demo)
    if args.output:
        with open(args.output, "wb") as fh:
            fh.write(header + body)
    elif not args.lmp:
        sys.stdout.buffer.write(header + body)


if __name__ == "__main__":
    main()
