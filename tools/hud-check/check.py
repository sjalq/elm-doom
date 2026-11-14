#!/usr/bin/env python3
"""Status bar and heads-up messages against linuxdoom-1.10, frame by frame.

    python3 check.py WAD [DEMO1 DEMO2 ... or demo files] [--clev]

Builds a C harness from ../../verify/oracle/src plus harness.c (in build/oracle-hud):
after every tic ST_Drawer draws onto the persistent screens[0] and HU_Drawer onto
a cleared one, with a script of cheat keys, full-screen and automap stretches.
Then runs ./hud-check (build it with: eco make src/HudCheck.elm --output=hud-check)
on the same demos and compares the per-frame lines (gametic, palette, status bar
hash, messages hash) and the music requests. Exit status 1 on any difference.
"""

import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ORACLE = os.path.join(HERE, '..', '..', 'verify', 'oracle')
BUILD = os.path.join(HERE, 'build', 'oracle-hud')


def patch(path, old, new):
    text = open(path).read()
    if new in text:
        return
    if old not in text:
        sys.exit('check.py: cannot patch ' + path)
    open(path, 'w').write(text.replace(old, new, 1))


def build_oracle():
    if os.path.exists(BUILD):
        shutil.rmtree(BUILD)
    shutil.copytree(os.path.join(ORACLE, 'src'), os.path.join(BUILD, 'src'))
    shutil.copy(os.path.join(ORACLE, 'Makefile'), BUILD)
    shutil.copy(os.path.join(HERE, 'harness.c'), os.path.join(BUILD, 'src'))
    src = os.path.join(BUILD, 'src')
    patch(os.path.join(src, 'd_main.c'),
          '\tM_Ticker ();\n\tG_Ticker ();\n\tgametic++;\n\tmaketic++;',
          '\tM_Ticker ();\n\t{ extern void Harness_BeforeTic (void); Harness_BeforeTic (); }\n'
          '\tG_Ticker ();\n\tgametic++;\n\tmaketic++;\n'
          '\t{ extern void Harness_AfterTic (void); Harness_AfterTic (); }')
    with open(os.path.join(src, 'st_stuff.c'), 'a') as f:
        f.write('\nint st_palette_harness (void) { return st_palette; }\n')
    patch(os.path.join(src, 's_sound.c'),
          '  int\t\t\tlooping )\n{\n    musicinfo_t*\tmusic;\n    char\t\tnamebuf[9];\n',
          '  int\t\t\tlooping )\n{\n    musicinfo_t*\tmusic;\n    char\t\tnamebuf[9];\n'
          '    { extern void Harness_Music (int); Harness_Music (musicnum); }\n')
    subprocess.run(['make'], cwd=BUILD, check=True, stdout=subprocess.DEVNULL)
    os.makedirs(os.path.join(BUILD, 'frames'), exist_ok=True)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    clev = '--clev' in sys.argv
    if not args:
        sys.exit(__doc__)
    wad = os.path.abspath(args[0])
    demos = args[1:] or ['DEMO1', 'DEMO2', 'DEMO3', 'DEMO4']
    build_oracle()
    out = os.path.join(HERE, 'out')
    os.makedirs(out, exist_ok=True)
    failed = False
    for demo in demos:
        if os.path.exists(demo):
            demo = os.path.abspath(demo)
        name = os.path.basename(demo)
        c_out = os.path.join(BUILD, name + '.txt')
        e_out = os.path.join(out, name + '.txt')
        flags = ['-clev'] if clev else []
        subprocess.run(['./doom-oracle', '-iwad', wad, '-playdemo', demo, '-hudout', c_out] + flags,
                       cwd=BUILD, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        with open(e_out, 'w') as f:
            subprocess.run([os.path.join(HERE, 'hud-check'), wad, demo, out] + (['--clev'] if clev else []),
                           stdout=f, check=True)
        c = [l.strip().lower() for l in open(c_out)]
        e = [l.strip().lower() for l in open(e_out)]
        bad = [(a, b) for a, b in zip(c, e) if a != b]
        if len(c) != len(e):
            bad.append(('%d lines' % len(c), '%d lines' % len(e)))
        print('%s: %d frames, %d differing' % (demo, len(c), len(bad)))
        for a, b in bad[:10]:
            print('  C:   ' + a + '\n  Elm: ' + b)
        failed = failed or bool(bad)
    sys.exit(1 if failed else 0)


if __name__ == '__main__':
    main()
