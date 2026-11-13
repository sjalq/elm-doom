// Check the JS OPL3 alone: replay a register trace from music-ref
// (MUSIC_REF_TRACE) through doom/browser/opl3.js and compare its output with
// music-ref's, sample by sample.
//
//   node chip-replay.mjs <trace> <ref.raw>
import fs from 'fs';
import { Opl3 } from '../../browser/opl3.js';

const [tracePath, refPath] = process.argv.slice(2);
const trace = fs.readFileSync(tracePath);
const refBuf = fs.readFileSync(refPath);
const ref = new Int16Array(refBuf.buffer, refBuf.byteOffset, refBuf.byteLength / 2);
const frames = ref.length / 2;

const chip = new Opl3(49716);
const out = new Int16Array(frames * 2);
let t = 0;
for (let f = 0; f < frames; f++) {
  while (t + 7 <= trace.length && trace.readUInt32LE(t) === f) {
    chip.writeRegBuffered(trace.readUInt16LE(t + 4), trace[t + 6]);
    t += 7;
  }
  chip.generateStream(out, f * 2, 1);
}

let first = -1;
let maxDiff = 0;
let differing = 0;
for (let i = 0; i < ref.length; i++) {
  const d = Math.abs(ref[i] - out[i]);
  if (d) {
    differing++;
    if (first < 0) first = i;
    if (d > maxDiff) maxDiff = d;
  }
}
if (first < 0) {
  console.log(`identical: ${frames} frames`);
} else {
  const f = first >> 1;
  console.log(`differ: first at frame ${f} (${first & 1 ? 'right' : 'left'}), ${differing} samples, max abs diff ${maxDiff}`);
  console.log('ref', Array.from(ref.slice(first - (first & 1) - 8, first - (first & 1) + 8)));
  console.log('js ', Array.from(out.slice(first - (first & 1) - 8, first - (first & 1) + 8)));
  process.exitCode = 1;
}
