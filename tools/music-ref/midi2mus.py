#!/usr/bin/env python3
"""Make a MUS lump out of a MIDI music lump, to test the MUS path (mus2mid.c and
its port) with Freedoom, whose music is all MIDI. Not a careful converter: it
merges the tracks, follows tempo changes, rounds times to MUS's 140 ticks a
second, and keeps notes, volumes, instruments, the volume and pan controllers,
pitch bends and all-notes-off, so that every MUS event kind the songs use turns up.

    midi2mus.py <wad> <lump> <out.mus>
"""
import struct
import sys


def lump(wad, name):
    data = open(wad, 'rb').read()
    n, d = struct.unpack('<ii', data[4:12])
    for i in range(n):
        pos, size, nm = struct.unpack('<ii8s', data[d + 16 * i:d + 16 * i + 16])
        if nm.rstrip(b'\0').decode().upper() == name.upper():
            return data[pos:pos + size]
    raise SystemExit('no lump ' + name)


def vlq(b, p):
    v = 0
    while True:
        c = b[p]
        p += 1
        v = (v << 7) | (c & 0x7f)
        if not c & 0x80:
            return v, p


def events(midi):
    assert midi[:4] == b'MThd'
    ntracks, division = struct.unpack('>HH', midi[10:14])
    p = 14
    out = []
    for t in range(ntracks):
        assert midi[p:p + 4] == b'MTrk'
        length = struct.unpack('>I', midi[p + 4:p + 8])[0]
        q, end = p + 8, p + 8 + length
        tick, status, order = 0, 0, 0
        while q < end:
            dt, q = vlq(midi, q)
            tick += dt
            if midi[q] & 0x80:
                status = midi[q]
                q += 1
            if status == 0xff:
                kind = midi[q]
                ln, q = vlq(midi, q + 1)
                out.append((tick, t, order, 'meta', kind, midi[q:q + ln]))
                q += ln
            elif status in (0xf0, 0xf7):
                ln, q = vlq(midi, q)
                q += ln
            else:
                k = status & 0xf0
                n = 1 if k in (0xc0, 0xd0) else 2
                out.append((tick, t, order, 'ch', status, midi[q:q + n]))
                q += n
            order += 1
        p = end
    out.sort(key=lambda e: (e[0], e[1], e[2]))
    return out, division


def main():
    wad, name, path = sys.argv[1:4]
    evs, division = events(lump(wad, name))
    tempo, last_tick, us = 500000, 0, 0.0
    timed = []
    for tick, _, _, kind, a, b in evs:
        us += (tick - last_tick) * tempo / division
        last_tick = tick
        if kind == 'meta':
            if a == 0x51:
                tempo = (b[0] << 16) | (b[1] << 8) | b[2]
            continue
        timed.append((round(us * 140 / 1e6), a, b))
    chmap = {9: 15}
    score = bytearray()
    lastvol = {}
    pending = []

    def channel(c):
        if c not in chmap:
            chmap[c] = len([k for k in chmap if k != 9])
        return chmap[c]

    for t, status, data in timed:
        k, c = status & 0xf0, channel(status & 0x0f)
        if k == 0x90 and data[1] > 0:
            if lastvol.get(c) == data[1]:
                pending.append((t, bytes([0x10 | c, data[0]])))
            else:
                lastvol[c] = data[1]
                pending.append((t, bytes([0x10 | c, data[0] | 0x80, data[1]])))
        elif k == 0x80 or k == 0x90:
            pending.append((t, bytes([0x00 | c, data[0]])))
        elif k == 0xc0:
            pending.append((t, bytes([0x40 | c, 0, data[0]])))
        elif k == 0xb0 and data[0] in (7, 10):
            pending.append((t, bytes([0x40 | c, 3 if data[0] == 7 else 4, data[1]])))
        elif k == 0xb0 and data[0] == 123:
            pending.append((t, bytes([0x30 | c, 11])))
        elif k == 0xe0:
            pending.append((t, bytes([0x20 | c, ((data[1] << 7) | data[0]) >> 6])))
    for i, (t, ev) in enumerate(pending):
        nxt = pending[i + 1][0] if i + 1 < len(pending) else t
        delay = nxt - t
        if delay > 0:
            ev = bytes([ev[0] | 0x80]) + ev[1:]
            d = []
            while True:
                d.insert(0, delay & 0x7f)
                delay >>= 7
                if not delay:
                    break
            ev += bytes([b | 0x80 for b in d[:-1]] + [d[-1]])
        score += ev
    score += bytes([0x60])
    header = b'MUS\x1a' + struct.pack('<HHHHHH', len(score), 16, len(chmap), 0, 0, 0)
    open(path, 'wb').write(header + score)


main()
