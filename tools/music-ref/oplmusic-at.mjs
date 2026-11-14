// doom/browser/oplmusic.js at another output rate, for the tools that check other
// ports against it (synth-ref.mjs, ../midi-check). The browser plays at the chip's
// own rate (49716 Hz) and resamples, so the rate is a constant of the module: this
// loads a copy of it with the rate and Chocolate's buffer size at that rate put in,
// and with mus2mid and its state (which carries over between conversions, as the C's
// statics do) exported too.
//
//   const { Music, mus2mid, mus2midState, BUFFER_FRAMES } = await loadOplMusic(48000);
import fs from 'fs';
import path from 'path';
import { fileURLToPath, pathToFileURL } from 'url';

const browser = path.join(path.dirname(fileURLToPath(import.meta.url)), '../../browser');

// Chocolate's GetSliceSize(): the largest power of two of frames within 28 ms
export function sliceFrames(rate) {
  const limit = Math.floor((rate * 28) / 1000);
  let n = 1;
  while (n * 2 <= limit) n *= 2;
  return n;
}

export async function loadOplMusic(rate = 49716) {
  let src = fs.readFileSync(path.join(browser, 'oplmusic.js'), 'utf8');
  const swap = (from, to) => {
    if (!src.includes(from)) throw new Error(`oplmusic.js has no "${from}"`);
    src = src.replace(from, to);
  };
  swap("from './opl3.js'", `from '${pathToFileURL(path.join(browser, 'opl3.js')).href}'`);
  swap('export const OPL_RATE = 49716;', `export const OPL_RATE = ${rate};`);
  swap('export const BUFFER_FRAMES = 1024;', `export const BUFFER_FRAMES = ${sliceFrames(rate)};`);
  src += '\nexport { mus2mid, mus2midState };\n';
  return import('data:text/javascript;base64,' + Buffer.from(src).toString('base64'));
}

// A WAD's lumps by (upper case) name; the last of a name wins, as in the engine.
export function wadLumps(buf) {
  const v = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
  const n = v.getInt32(4, true);
  const dir = v.getInt32(8, true);
  const out = new Map();
  for (let i = 0; i < n; i++) {
    const at = dir + 16 * i;
    let name = '';
    for (let k = 0; k < 8; k++) {
      const c = v.getUint8(at + 8 + k);
      if (!c) break;
      name += String.fromCharCode(c);
    }
    const pos = v.getInt32(at, true);
    out.set(name.toUpperCase(), new Uint8Array(buf.buffer, buf.byteOffset + pos, v.getInt32(at + 4, true)));
  }
  return out;
}
