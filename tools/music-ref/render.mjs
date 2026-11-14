// Render a Doom music lump with the browser's music code (hosts/browser/oplmusic.js,
// the module the AudioWorklet runs), the way music-ref renders it with Chocolate
// Doom's C sources, to compare them sample by sample.
//
//   node render.mjs <wad> <lump or file> <seconds> <out.raw> [volume] [looping]
//
// Same arguments (including the script of later actions) and output as music-ref:
// interleaved 16-bit stereo at 49716 Hz.
import fs from 'fs';
import { Music, BUFFER_FRAMES, OPL_RATE } from '../../hosts/browser/oplmusic.js';

const [wadPath, lumpName, secondsArg, outPath, volumeArg, loopingArg, script] = process.argv.slice(2);
if (!outPath) {
  console.error('usage: node render.mjs <wad> <lump> <seconds> <out.raw> [volume] [looping]');
  process.exit(2);
}

function lumps(path) {
  const buf = fs.readFileSync(path);
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

const wad = lumps(wadPath);
const song = wad.get(lumpName.toUpperCase()) || (fs.existsSync(lumpName) ? new Uint8Array(fs.readFileSync(lumpName)) : null);
if (!song) {
  console.error(`render: no lump ${lumpName}`);
  process.exit(1);
}
const music = new Music(wad.get('GENMIDI'));
if (!music.init()) process.exit(1);
music.setMusicVolume(127);
music.setMusicVolume(volumeArg === undefined ? 64 : +volumeArg);
const handle = music.registerSong(song);
if (!handle) {
  console.error(`render: couldn't register ${lumpName}`);
  process.exit(1);
}
music.playSong(handle, loopingArg === undefined ? true : loopingArg !== '0');

// the script: "buffer:action[:arg[:arg]]", comma separated (see music-ref)
const actions = (script || '').split(',').filter(Boolean).map((item) => item.split(':'));
function runScript(buffer) {
  for (const [at, action, a1, a2] of actions) {
    if (+at !== buffer) continue;
    if (action === 'pause') music.pauseSong();
    else if (action === 'resume') music.resumeSong();
    else if (action === 'stop') music.stopSong();
    else if (action === 'volume') music.setMusicVolume(+a1);
    else if (action === 'play') {
      music.stopSong();
      const lump = wad.get(a1.toUpperCase());
      const h = lump ? music.registerSong(lump) : null;
      music.playSong(h, a2 === undefined ? true : a2 !== '0');
    }
  }
}

const want = Math.floor(+secondsArg * OPL_RATE);
const buffers = Math.ceil(want / BUFFER_FRAMES);
const out = new Int16Array(buffers * BUFFER_FRAMES * 2);
const buffer = new Int16Array(BUFFER_FRAMES * 2);
for (let b = 0; b < buffers; b++) {
  runScript(b);
  music.deviceBuffer(buffer);
  out.set(buffer, b * BUFFER_FRAMES * 2);
}
fs.writeFileSync(outPath, Buffer.from(out.buffer, 0, want * 4));
