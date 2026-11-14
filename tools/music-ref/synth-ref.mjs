// The reference for host/synth (Elm OS's Rust port of the same music code): render a
// song with the browser's music code (doom/browser/oplmusic.js) at any output rate,
// the way Chocolate Doom does at that mixing rate, and save the MIDI files it played
// so the Rust synthesizer can play the very same ones. host/synth/tests/reference.rs
// runs this and compares every sample (see there for how to run it).
//
//   node synth-ref.mjs <rate> <seconds> <outdir> <wad or bank> <lump or file> [volume] [looping] [script]
//
// <wad or bank>: a WAD (its GENMIDI lump is the bank, and songs are its lumps), or a
// DMX OP2 bank file (and the song a file). A MUS song is converted to MIDI by
// oplmusic.js's mus2mid, as Chocolate converts it when the song is registered.
// volume (default 64), looping (default 1) and the script of later calls are as for
// render.mjs and music-ref; buffers are Chocolate's slice size at <rate>.
//
// Writes <outdir>/ref.raw (interleaved 16-bit little-endian stereo, the first
// floor(seconds * rate) frames), <outdir>/bank.op2, and <outdir>/song<N>.mid for the
// song (N = 0) and each script "play" in order (N = 1, 2, ...): every play converts
// the lump again, as Chocolate does, and mus2mid keeps state from one conversion to
// the next. ./oplmusic-at.mjs gives the module at <rate>, with mus2mid.
import fs from 'fs';
import path from 'path';
import { loadOplMusic, sliceFrames, wadLumps } from './oplmusic-at.mjs';

const [rateArg, secondsArg, outDir, bankPath, songName, volumeArg, loopingArg, script] = process.argv.slice(2);
if (!songName) {
  console.error('usage: node synth-ref.mjs <rate> <seconds> <outdir> <wad or bank> <lump or file> [volume] [looping] [script]');
  process.exit(2);
}
const rate = +rateArg;
const { Music, mus2mid } = await loadOplMusic(rate);

const bankFile = new Uint8Array(fs.readFileSync(bankPath));
const magic = String.fromCharCode(...bankFile.slice(0, 4));
const wad = magic === 'IWAD' || magic === 'PWAD' ? wadLumps(bankFile) : null;
const bank = wad ? wad.get('GENMIDI') : bankFile;
const songBytes = (name) => (wad ? wad.get(name.toUpperCase()) : null) || (fs.existsSync(name) ? new Uint8Array(fs.readFileSync(name)) : null);

fs.mkdirSync(outDir, { recursive: true });
fs.writeFileSync(path.join(outDir, 'bank.op2'), bank);

// I_OPL_RegisterSong's choice: MIDI as it is, anything else converted from MUS
let songs = 0;
function midiFor(name) {
  const data = songBytes(name);
  const n = songs++;
  if (!data) return null;
  const isMid = data.length > 4 && data[0] === 0x4d && data[1] === 0x54 && data[2] === 0x68 && data[3] === 0x64;
  const midi = isMid ? data : mus2mid(data);
  if (midi) fs.writeFileSync(path.join(outDir, `song${n}.mid`), midi);
  return midi;
}

const music = new Music(bank);
if (!music.init()) process.exit(1);
music.setMusicVolume(127);
music.setMusicVolume(volumeArg === undefined ? 64 : +volumeArg);
const first = midiFor(songName);
music.playSong(first ? music.registerSong(first) : null, loopingArg === undefined ? true : loopingArg !== '0');

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
      const midi = midiFor(a1);
      music.playSong(midi ? music.registerSong(midi) : null, a2 === undefined ? true : a2 !== '0');
    }
  }
}

const frames = sliceFrames(rate);
const want = Math.floor(+secondsArg * rate);
const buffers = Math.ceil(want / frames);
const out = new Int16Array(buffers * frames * 2);
const buffer = new Int16Array(frames * 2);
for (let b = 0; b < buffers; b++) {
  runScript(b);
  music.deviceBuffer(buffer);
  out.set(buffer, b * frames * 2);
}
fs.writeFileSync(path.join(outDir, 'ref.raw'), Buffer.from(out.buffer, 0, want * 4));
