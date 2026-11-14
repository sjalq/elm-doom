// Doom's music in the browser, as an AudioWorklet: Chocolate Doom's OPL music
// path (./oplmusic.js, a sample-exact port of i_oplmusic.c, mus2mid.c,
// midifile.c and its OPL driver, on ./opl3.js, a sample-exact port of Nuked OPL3),
// played the way its SDL audio thread plays it: 1024-frame buffers at the chip's
// own 49716 Hz, with the game's music calls made between buffers. Only the last
// step is the browser's: resampling to the AudioContext's rate (cubic Hermite
// interpolation), and converting the 16-bit samples to floats.
// tools/music-ref checks the 49716 Hz output against the C sources.
//
// Part of the Doom port in Elm-OS, GPL-2.0-or-later.

import { Music, BUFFER_FRAMES, OPL_RATE } from './oplmusic.js';

class OplProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.music = null;
    this.handle = null;
    this.buffer = new Int16Array(BUFFER_FRAMES * 2);
    this.at = BUFFER_FRAMES; // next frame of `buffer` to read; past the end: none yet
    // the last four input frames, for the interpolation (left, right)
    this.hist = new Float32Array(8);
    this.step = OPL_RATE / sampleRate; // input frames per output frame
    this.frac = 0;
    this.peak = 0;
    this.port.onmessage = (e) => this.message(e.data);
  }

  message(m) {
    if (m.type === 'genmidi') {
      this.music = new Music(new Uint8Array(m.buffer));
      this.music.init();
      return;
    }
    const music = this.music;
    if (!music) return;
    switch (m.type) {
      case 'play': // S_ChangeMusic: stop, unregister, register, play
        music.stopSong();
        this.handle = music.registerSong(new Uint8Array(m.buffer));
        music.playSong(this.handle, m.looping);
        break;
      case 'stop': music.stopSong(); break;
      case 'pause': music.pauseSong(); break;
      case 'resume': music.resumeSong(); break;
      case 'volume': music.setMusicVolume(m.value); break;
      case 'report': // for tests: what the player is doing
        this.port.postMessage({ playing: music.isPlaying(), volume: music.currentMusicVolume, peak: this.peak });
        this.peak = 0;
        break;
    }
  }

  // the next 49716 Hz frame into the history
  pull() {
    if (this.at >= BUFFER_FRAMES) {
      if (this.music) this.music.deviceBuffer(this.buffer);
      else this.buffer.fill(0);
      this.at = 0;
    }
    const h = this.hist;
    h[0] = h[2]; h[1] = h[3];
    h[2] = h[4]; h[3] = h[5];
    h[4] = h[6]; h[5] = h[7];
    h[6] = this.buffer[this.at * 2] / 32768;
    h[7] = this.buffer[this.at * 2 + 1] / 32768;
    this.at++;
  }

  process(inputs, outputs) {
    const out = outputs[0];
    const left = out[0];
    const right = out[1] || out[0];
    const h = this.hist;
    for (let i = 0; i < left.length; i++) {
      this.frac += this.step;
      while (this.frac >= 1) {
        this.pull();
        this.frac -= 1;
      }
      // Catmull-Rom between the middle two of the last four frames
      const t = this.frac;
      const t2 = t * t;
      const t3 = t2 * t;
      const a = -0.5 * t3 + t2 - 0.5 * t;
      const b = 1.5 * t3 - 2.5 * t2 + 1;
      const c = -1.5 * t3 + 2 * t2 + 0.5 * t;
      const d = 0.5 * t3 - 0.5 * t2;
      const l = a * h[0] + b * h[2] + c * h[4] + d * h[6];
      const r = a * h[1] + b * h[3] + c * h[5] + d * h[7];
      left[i] = l;
      if (right !== left) right[i] = r;
      const m = Math.abs(l) > Math.abs(r) ? Math.abs(l) : Math.abs(r);
      if (m > this.peak) this.peak = m;
    }
    return true;
  }
}

registerProcessor('doom-opl', OplProcessor);
