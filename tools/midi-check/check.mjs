// Doom.Audio.Midi against Chocolate Doom's mus2mid (as doom/browser/oplmusic.js
// has it; music-ref checks that against the C) for every music lump of the WADs
// given: each lump converted alone, and all of a WAD's in turn with one converter
// (mus2mid's note volumes carry over from song to song). Run by check.sh.
//
//   node check.mjs WAD...
import fs from 'fs';
import path from 'path';
import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import { loadOplMusic, wadLumps } from '../music-ref/oplmusic-at.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const { Elm } = createRequire(import.meta.url)(path.join(here, 'midi-check.js'));
const { mus2mid, mus2midState } = await loadOplMusic();

const fresh = () => {
  mus2midState.channelvelocities.fill(127);
  mus2midState.queuedtime = 0;
};
const same = (a, b) => (a === null || b === null ? a === b : a.length === b.length && a.every((x, i) => x === b[i]));

function elm(lumps) {
  return new Promise((resolve) => {
    const app = Elm.MidiCheck.init({ flags: lumps.map((l) => Array.from(l)) });
    app.ports.result.subscribe(resolve);
  });
}

let failed = 0;
for (const wadPath of process.argv.slice(2)) {
  const wad = wadLumps(new Uint8Array(fs.readFileSync(wadPath)));
  const names = [...wad.keys()].filter((n) => n.startsWith('D_'));
  const lumps = names.map((n) => wad.get(n));
  const js = (lump) => {
    const midi = mus2mid(lump);
    return midi ? Array.from(midi) : null;
  };
  const jsFresh = lumps.map((l) => (fresh(), js(l)));
  fresh();
  const jsSequential = lumps.map(js);
  const got = await elm(lumps);
  let bytes = 0;
  let converted = 0;
  const bad = [];
  names.forEach((name, i) => {
    if (jsFresh[i]) {
      converted++;
      bytes += jsFresh[i].length;
    }
    if (!same(got.fresh[i], jsFresh[i]) || !same(got.sequential[i], jsSequential[i])) bad.push(name);
  });
  const changed = names.filter((_, i) => !same(jsFresh[i], jsSequential[i])).length;
  console.log(
    `${path.basename(wadPath)}: ${names.length} music lumps (${converted} converted, ${bytes} MIDI bytes, ` +
      `${changed} different when converted in turn): ` +
      (bad.length ? `DIFFER: ${bad.join(' ')}` : 'identical, alone and in turn'),
  );
  if (bad.length) failed = 1;
}
process.exitCode = failed;
