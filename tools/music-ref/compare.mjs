// Compare two raw 16-bit renders sample by sample: max abs difference and the
// first differing sample.  node compare.mjs <a.raw> <b.raw>
import fs from 'fs';

const [aPath, bPath] = process.argv.slice(2);
const read = (p) => {
  const b = fs.readFileSync(p);
  return new Int16Array(b.buffer, b.byteOffset, b.byteLength >> 1);
};
const a = read(aPath);
const b = read(bPath);
const n = Math.min(a.length, b.length);
let first = -1;
let max = 0;
let count = 0;
for (let i = 0; i < n; i++) {
  const d = Math.abs(a[i] - b[i]);
  if (d) {
    count++;
    if (first < 0) first = i;
    if (d > max) max = d;
  }
}
const frames = n >> 1;
if (a.length !== b.length) console.log(`lengths differ: ${a.length >> 1} and ${b.length >> 1} frames`);
if (first < 0) {
  console.log(`identical: ${frames} frames, max abs diff 0`);
} else {
  console.log(`DIFFER: first at frame ${first >> 1} (${first & 1 ? 'right' : 'left'}, ${((first >> 1) / 49716).toFixed(3)} s), ${count} samples differ, max abs diff ${max}`);
  process.exitCode = 1;
}
