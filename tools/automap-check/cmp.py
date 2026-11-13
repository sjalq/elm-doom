"""Compare C (53760-byte) and Elm (64000-byte) automap dumps; write PNGs of both."""
import os, sys, struct, zlib

cdir, edir, pngdir, wad = sys.argv[1:5]
os.makedirs(pngdir, exist_ok=True)

data = open(wad, 'rb').read()
n, off = struct.unpack('<ii', data[4:12])
pal = None
for i in range(n):
    p, s, name = struct.unpack('<ii8s', data[off + 16 * i: off + 16 * i + 16])
    if name.rstrip(b'\0') == b'PLAYPAL':
        pal = data[p:p + 768]


def png(path, pix, w, h):
    raw = b''.join(b'\0' + b''.join(pal[c * 3:c * 3 + 3] for c in pix[y * w:(y + 1) * w]) for y in range(h))
    def chunk(t, d):
        return struct.pack('>I', len(d)) + t + d + struct.pack('>I', zlib.crc32(t + d) & 0xffffffff)
    open(path, 'wb').write(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, 8, 2, 0, 0, 0))
                           + chunk(b'IDAT', zlib.compress(raw)) + chunk(b'IEND', b''))


cfiles = sorted(f for f in os.listdir(cdir) if f.endswith('.raw'))
efiles = sorted(f for f in os.listdir(edir) if f.endswith('.raw'))
if cfiles != efiles:
    print('file sets differ:', sorted(set(cfiles) ^ set(efiles)))
bad = 0
for f in sorted(set(cfiles) & set(efiles)):
    c = open(os.path.join(cdir, f), 'rb').read()[:53760]
    e = open(os.path.join(edir, f), 'rb').read()[:53760]
    d = [i for i in range(53760) if c[i] != e[i]]
    if d:
        bad += 1
        print(f, len(d), 'differ, first at', d[0] % 320, d[0] // 320)
        png(os.path.join(pngdir, f[:-4] + '_c.png'), c, 320, 168)
        png(os.path.join(pngdir, f[:-4] + '_elm.png'), e, 320, 168)
    else:
        print(f, 'identical', sum(1 for x in c if x))
        png(os.path.join(pngdir, f[:-4] + '.png'), c, 320, 168)
print('frames', len(set(cfiles) & set(efiles)), 'differing', bad)
