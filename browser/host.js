// The browser host for Doom in Elm: loads the WAD, draws the Elm program's frames,
// plays its sounds and music, and turns the keyboard and mouse into Doom's events.
// Plain modern JavaScript, no build step. Used by index.html (the game) and
// soundtest.html (a page that exercises the host without a game).
//
// Part of the Doom port in Elm-OS, under the GNU GPL version 2 or later.

// v_video.c's gammatable: the five gamma correction levels
export const GAMMA = [
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255],
  [2,4,5,7,8,10,11,12,14,15,16,18,19,20,21,23,24,25,26,27,29,30,31,32,33,34,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,52,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,163,164,165,166,167,168,169,170,171,172,173,174,175,175,176,177,178,179,180,181,182,183,184,185,186,186,187,188,189,190,191,192,193,194,195,196,196,197,198,199,200,201,202,203,204,205,205,206,207,208,209,210,211,212,213,214,214,215,216,217,218,219,220,221,222,222,223,224,225,226,227,228,229,230,230,231,232,233,234,235,236,237,237,238,239,240,241,242,243,244,245,245,246,247,248,249,250,251,252,252,253,254,255],
  [4,7,9,11,13,15,17,19,21,22,24,26,27,29,30,32,33,35,36,38,39,40,42,43,45,46,47,48,50,51,52,54,55,56,57,59,60,61,62,63,65,66,67,68,69,70,72,73,74,75,76,77,78,79,80,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,133,134,135,136,137,138,139,140,141,142,143,144,144,145,146,147,148,149,150,151,152,153,153,154,155,156,157,158,159,160,160,161,162,163,164,165,166,166,167,168,169,170,171,172,172,173,174,175,176,177,178,178,179,180,181,182,183,183,184,185,186,187,188,188,189,190,191,192,193,193,194,195,196,197,197,198,199,200,201,201,202,203,204,205,206,206,207,208,209,210,210,211,212,213,213,214,215,216,217,217,218,219,220,221,221,222,223,224,224,225,226,227,228,228,229,230,231,231,232,233,234,235,235,236,237,238,238,239,240,241,241,242,243,244,244,245,246,247,247,248,249,250,251,251,252,253,254,254,255],
  [8,12,16,19,22,24,27,29,31,34,36,38,40,41,43,45,47,49,50,52,53,55,57,58,60,61,63,64,65,67,68,70,71,72,74,75,76,77,79,80,81,82,84,85,86,87,88,90,91,92,93,94,95,96,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,135,136,137,138,139,140,141,142,143,143,144,145,146,147,148,149,150,150,151,152,153,154,155,155,156,157,158,159,160,160,161,162,163,164,165,165,166,167,168,169,169,170,171,172,173,173,174,175,176,176,177,178,179,180,180,181,182,183,183,184,185,186,186,187,188,189,189,190,191,192,192,193,194,195,195,196,197,197,198,199,200,200,201,202,202,203,204,205,205,206,207,207,208,209,210,210,211,212,212,213,214,214,215,216,216,217,218,219,219,220,221,221,222,223,223,224,225,225,226,227,227,228,229,229,230,231,231,232,233,233,234,235,235,236,237,237,238,238,239,240,240,241,242,242,243,244,244,245,246,246,247,247,248,249,249,250,251,251,252,253,253,254,254,255],
  [16,23,28,32,36,39,42,45,48,50,53,55,57,60,62,64,66,68,69,71,73,75,76,78,80,81,83,84,86,87,89,90,92,93,94,96,97,98,100,101,102,103,105,106,107,108,109,110,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,128,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,143,144,145,146,147,148,149,150,150,151,152,153,154,155,155,156,157,158,159,159,160,161,162,163,163,164,165,166,166,167,168,169,169,170,171,172,172,173,174,175,175,176,177,177,178,179,180,180,181,182,182,183,184,184,185,186,187,187,188,189,189,190,191,191,192,193,193,194,195,195,196,196,197,198,198,199,200,200,201,202,202,203,203,204,205,205,206,207,207,208,208,209,210,210,211,211,212,213,213,214,214,215,216,216,217,217,218,219,219,220,220,221,221,222,223,223,224,224,225,225,226,227,227,228,228,229,229,230,230,231,232,232,233,233,234,234,235,235,236,236,237,237,238,239,239,240,240,241,241,242,242,243,243,244,244,245,245,246,246,247,247,248,248,249,249,250,250,251,251,252,252,253,254,254,255,255]
];

// doomdef.h's key codes
const KEY = {
  RIGHTARROW: 0xae, LEFTARROW: 0xac, UPARROW: 0xad, DOWNARROW: 0xaf,
  ESCAPE: 27, ENTER: 13, TAB: 9, BACKSPACE: 127, PAUSE: 0xff,
  EQUALS: 0x3d, MINUS: 0x2d, RSHIFT: 0x80 + 0x36, RCTRL: 0x80 + 0x1d, RALT: 0x80 + 0x38,
};
const F_KEYS = [0x80 + 0x3b, 0x80 + 0x3c, 0x80 + 0x3d, 0x80 + 0x3e, 0x80 + 0x3f, 0x80 + 0x40,
  0x80 + 0x41, 0x80 + 0x42, 0x80 + 0x43, 0x80 + 0x44, 0x80 + 0x57, 0x80 + 0x58];

const CODE_KEYS = {
  ArrowRight: KEY.RIGHTARROW, ArrowLeft: KEY.LEFTARROW, ArrowUp: KEY.UPARROW, ArrowDown: KEY.DOWNARROW,
  Escape: KEY.ESCAPE, Enter: KEY.ENTER, NumpadEnter: KEY.ENTER, Tab: KEY.TAB,
  Backspace: KEY.BACKSPACE, Delete: KEY.BACKSPACE, Pause: KEY.PAUSE,
  Equal: KEY.EQUALS, NumpadEqual: KEY.EQUALS, Minus: KEY.MINUS, NumpadSubtract: KEY.MINUS,
  ShiftLeft: KEY.RSHIFT, ShiftRight: KEY.RSHIFT, ControlLeft: KEY.RCTRL, ControlRight: KEY.RCTRL,
  AltLeft: KEY.RALT, AltRight: KEY.RALT,
  // keys that type nothing (the menu gives them the character 0, as DOS Doom does)
  CapsLock: 0x80 + 0x3a, ScrollLock: 0x80 + 0x46, NumLock: 0x80 + 0x45,
  Space: 32, Comma: 44, Period: 46, Slash: 47, Semicolon: 59, Quote: 39,
  BracketLeft: 91, BracketRight: 93, Backslash: 92, Backquote: 96,
};

// i_video.c's xlatekey, for a browser KeyboardEvent: the key's unshifted meaning
// (KeyboardEvent.code, so Shift+1 is still '1'), letters in lower case.
export function doomKey(e) {
  if (e.code in CODE_KEYS) return CODE_KEYS[e.code];
  let m = /^F([0-9]+)$/.exec(e.code);
  if (m && +m[1] >= 1 && +m[1] <= 12) return F_KEYS[+m[1] - 1];
  m = /^Key([A-Z])$/.exec(e.code);
  if (m) return m[1].toLowerCase().charCodeAt(0);
  m = /^(?:Digit|Numpad)([0-9])$/.exec(e.code);
  if (m) return m[1].charCodeAt(0);
  if (e.key && e.key.length === 1) {
    const c = e.key.toLowerCase().charCodeAt(0);
    if (c >= 32 && c < 127) return c;
  }
  return null;
}

// ---------------------------------------------------------------------------
// The WAD

export function readWad(buffer) {
  const view = new DataView(buffer);
  const magic = String.fromCharCode(...new Uint8Array(buffer, 0, 4));
  if (magic !== 'IWAD' && magic !== 'PWAD') throw new Error('not a WAD file');
  const count = view.getInt32(4, true);
  const dir = view.getInt32(8, true);
  const lumps = new Map();
  for (let i = 0; i < count; i++) {
    const at = dir + i * 16;
    const pos = view.getInt32(at, true);
    const size = view.getInt32(at + 4, true);
    let name = '';
    for (let k = 0; k < 8; k++) {
      const c = view.getUint8(at + 8 + k);
      if (c === 0) break;
      name += String.fromCharCode(c);
    }
    lumps.set(name.toUpperCase(), { pos, size });
  }
  return {
    buffer,
    has: (name) => lumps.has(name),
    lump(name) {
      const l = lumps.get(name);
      return l ? new Uint8Array(buffer, l.pos, l.size) : null;
    },
  };
}

// ---------------------------------------------------------------------------
// The screen: 320x200 palette indices, through PLAYPAL and the gamma table, on a
// canvas the page stretches to 4:3 with square-edged pixels.

export class Screen {
  constructor(canvas, wad) {
    this.canvas = canvas;
    canvas.width = 320;
    canvas.height = 200;
    this.ctx = canvas.getContext('2d', { alpha: false });
    this.image = this.ctx.createImageData(320, 200);
    this.rgba = new Uint32Array(this.image.data.buffer);
    this.playpal = wad.lump('PLAYPAL');
    this.luts = new Map();
  }

  lut(palette, gamma) {
    const key = palette * 8 + gamma;
    let lut = this.luts.get(key);
    if (!lut) {
      lut = new Uint32Array(256);
      const g = GAMMA[gamma] || GAMMA[0];
      const base = (palette % 14) * 768;
      for (let i = 0; i < 256; i++) {
        const r = g[this.playpal[base + i * 3]];
        const gr = g[this.playpal[base + i * 3 + 1]];
        const b = g[this.playpal[base + i * 3 + 2]];
        lut[i] = (0xff000000 | (b << 16) | (gr << 8) | r) >>> 0;
      }
      this.luts.set(key, lut);
    }
    return lut;
  }

  // pixels: 320 columns, left to right, of 200 palette indices from the top
  draw(pixels, palette, gamma) {
    const lut = this.lut(palette, gamma);
    const rgba = this.rgba;
    for (let x = 0; x < 320; x++) {
      const col = pixels[x];
      for (let y = 0, o = x; y < 200; y++, o += 320) rgba[o] = lut[col[y]];
    }
    this.ctx.putImageData(this.image, 0, 0);
  }
}

// ---------------------------------------------------------------------------
// Audio: started by the first key press or click (browsers keep audio off until
// then). Sound effects as i_sound.c mixes them, music through the OPL worklet.

export class Audio {
  constructor(wad) {
    this.wad = wad;
    this.ctx = null;
    this.starting = null;
    this.buffers = new Map();
    this.channels = [];
    this.music = null;
    this.pendingMusic = null; // the song asked for before audio could start
    this.musicVolume = 64;
    this.sfxVolume = 64;
    this.pitchShift = /[?&]pitch\b/.test(location.search);
  }

  start() {
    if (!this.starting) this.starting = this.open();
    return this.starting;
  }

  async open() {
    const ctx = new AudioContext();
    this.ctx = ctx;
    await ctx.resume();
    try {
      await ctx.audioWorklet.addModule('opl.js');
      this.music = new AudioWorkletNode(ctx, 'doom-opl', { numberOfInputs: 0, outputChannelCount: [2] });
      this.music.connect(ctx.destination);
      const genmidi = this.wad.lump('GENMIDI');
      if (genmidi) this.music.port.postMessage({ type: 'genmidi', buffer: genmidi.slice().buffer });
      this.music.port.postMessage({ type: 'volume', value: this.musicVolume });
      if (this.pendingMusic) this.music.port.postMessage(this.pendingMusic);
    } catch (err) {
      console.error('music unavailable:', err);
    }
  }

  // a DMX sound lump: format 3, the rate, the sample count, then 8-bit unsigned
  // samples (the 16 padding bytes each end included, as Doom plays them)
  buffer(name) {
    let b = this.buffers.get(name);
    if (b === undefined) {
      b = null;
      const lump = this.wad.lump(name);
      if (lump && lump.length > 8) {
        const view = new DataView(lump.buffer, lump.byteOffset, lump.byteLength);
        const rate = view.getUint16(2, true) || 11025;
        const count = Math.min(view.getUint32(4, true), lump.length - 8);
        if (count > 0) {
          b = this.ctx.createBuffer(1, count, rate);
          const data = b.getChannelData(0);
          for (let i = 0; i < count; i++) data[i] = (lump[8 + i] - 128) / 128;
        }
      }
      this.buffers.set(name, b);
    }
    return b;
  }

  stopChannel(n) {
    const ch = this.channels[n];
    if (ch) {
      this.channels[n] = null;
      try { ch.source.stop(); } catch (e) { /* already ended */ }
      ch.merger.disconnect();
    }
  }

  // addsfx's stereo: separation 0..255 (128 the middle) and volume 0..127 to left
  // and right volumes 0..127
  static gains(volume, separation) {
    const vol = Math.max(0, Math.min(127, volume));
    let sep = separation + 1;
    const left = vol - ((vol * sep * sep) >> 16);
    sep -= 257;
    const right = vol - ((vol * sep * sep) >> 16);
    return [Math.max(0, left) / 127, Math.max(0, right) / 127];
  }

  // I_SetChannels's steptable: pitch 128 is normal speed, 64 steps an octave. DOS
  // Doom v1.9's DMX ignored the pitch (s_sound.c still draws it from M_Random), so
  // it is only used when pitchShift is on (?pitch in the page's address).
  rate(pitch) {
    return this.pitchShift ? Math.pow(2, (pitch - 128) / 64) : 1;
  }

  startSound(c) {
    if (!this.ctx) return;
    this.stopChannel(c.channel);
    const buffer = this.buffer(c.lump);
    if (!buffer) return;
    const ctx = this.ctx;
    const source = ctx.createBufferSource();
    source.buffer = buffer;
    source.playbackRate.value = this.rate(c.pitch);
    const left = ctx.createGain();
    const right = ctx.createGain();
    const merger = ctx.createChannelMerger(2);
    const [l, r] = Audio.gains(c.volume, c.separation);
    left.gain.value = l;
    right.gain.value = r;
    source.connect(left).connect(merger, 0, 0);
    source.connect(right).connect(merger, 0, 1);
    merger.connect(ctx.destination);
    const entry = { source, left, right, merger };
    source.onended = () => {
      merger.disconnect();
      if (this.channels[c.channel] === entry) this.channels[c.channel] = null;
    };
    this.channels[c.channel] = entry;
    source.start();
  }

  updateSound(c) {
    const ch = this.channels[c.channel];
    if (!ch) return;
    const [l, r] = Audio.gains(c.volume, c.separation);
    ch.left.gain.value = l;
    ch.right.gain.value = r;
    ch.source.playbackRate.value = this.rate(c.pitch);
  }

  musicMessage(m) {
    if (m.type === 'play') this.pendingMusic = m;
    else if (m.type === 'stop') this.pendingMusic = null;
    if (this.music) this.music.port.postMessage(m);
  }

  // one of Doom.Audio.Command, as the Elm host encodes it
  command(c) {
    switch (c.cmd) {
      case 'start': this.startSound(c); break;
      case 'update': this.updateSound(c); break;
      case 'stop': this.stopChannel(c.channel); break;
      case 'music': this.musicMessage({ type: 'play', events: c.events, looping: c.looping }); break;
      case 'musicstop': this.musicMessage({ type: 'stop' }); break;
      case 'musicpause': this.musicMessage({ type: 'pause' }); break;
      case 'musicresume': this.musicMessage({ type: 'resume' }); break;
      case 'musicvolume':
        // DMX's 0..127 (S_SetMusicVolume sends 127 first, then the setting)
        this.musicVolume = Math.max(0, Math.min(127, c.value));
        this.musicMessage({ type: 'volume', value: this.musicVolume });
        break;
      case 'sfxvolume': this.sfxVolume = c.value; break;
    }
  }
}

// ---------------------------------------------------------------------------
// Input: key presses (repeats included: DOS Doom's keyboard handler saw the BIOS's
// repeated key-downs, which the menus rely on) and, once the canvas has captured
// the pointer, the mouse as i_video.c reports it (movement times 4, forwards up).

export function attachInput(canvas, send, onGesture) {
  window.addEventListener('keydown', (e) => {
    if (e.metaKey) return; // leave the browser's own shortcuts alone
    onGesture();
    const k = doomKey(e);
    if (k === null) return;
    e.preventDefault();
    send({ kind: 'keydown', data1: k, data2: 0, data3: 0 });
  });
  window.addEventListener('keyup', (e) => {
    const k = doomKey(e);
    if (k === null) return;
    e.preventDefault();
    send({ kind: 'keyup', data1: k, data2: 0, data3: 0 });
  });

  let buttons = 0;
  // DOS's button bits: 1 left, 2 right, 4 middle
  const bit = (b) => (b === 0 ? 1 : b === 2 ? 2 : b === 1 ? 4 : 0);
  const locked = () => document.pointerLockElement === canvas;
  canvas.addEventListener('click', () => {
    onGesture();
    if (!locked() && canvas.requestPointerLock) canvas.requestPointerLock();
  });
  canvas.addEventListener('contextmenu', (e) => e.preventDefault());
  document.addEventListener('mousedown', (e) => {
    if (!locked()) return;
    buttons |= bit(e.button);
    send({ kind: 'mouse', data1: buttons, data2: 0, data3: 0 });
  });
  document.addEventListener('mouseup', (e) => {
    if (!locked()) return;
    buttons &= ~bit(e.button);
    send({ kind: 'mouse', data1: buttons, data2: 0, data3: 0 });
  });
  document.addEventListener('mousemove', (e) => {
    if (!locked()) return;
    const dx = Math.round(e.movementX) * 4;
    const dy = -Math.round(e.movementY) * 4;
    if (dx || dy) send({ kind: 'mouse', data1: buttons, data2: dx, data3: dy });
  });
  document.addEventListener('pointerlockchange', () => {
    if (!locked() && buttons) {
      buttons = 0;
      send({ kind: 'mouse', data1: 0, data2: 0, data3: 0 });
    }
  });
}

// ---------------------------------------------------------------------------
// Starting: fetch the WAD next to the page, or take one dropped on it, then start
// the Elm program with it and wire everything up.

export async function fetchWad(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`could not load ${url} (${res.status})`);
  return res.arrayBuffer();
}

export function onDrop(target, handle) {
  target.addEventListener('dragover', (e) => e.preventDefault());
  target.addEventListener('drop', async (e) => {
    e.preventDefault();
    const file = e.dataTransfer.files[0];
    if (file) handle(await file.arrayBuffer(), file.name);
  });
}

// everything saved under "doom:" in localStorage, for the Elm program's flags
export function stored() {
  const out = {};
  try {
    for (let i = 0; i < localStorage.length; i++) {
      const k = localStorage.key(i);
      if (k.startsWith('doom:')) out[k.slice(5)] = localStorage.getItem(k);
    }
  } catch (e) { /* storage off */ }
  return out;
}

// Wire an Elm program (with the ports below) to the page:
//   in:  wad (a File with the WAD), event ({kind, data1, data2, data3})
//   out: frame ({pixels, palette, gamma}), audio (a list of commands),
//        status (a message to show), store ({key, value}: value null deletes)
export function run(app, canvas, buffer, status) {
  const wad = readWad(buffer);
  const screen = new Screen(canvas, wad);
  const audio = new Audio(wad);
  const timing = { frames: 0, drawMs: 0, intervals: [] };
  let last = 0;
  window.doomTiming = timing;

  app.ports.frame.subscribe((f) => {
    const t0 = performance.now();
    screen.draw(f.pixels, f.palette, f.gamma);
    const t1 = performance.now();
    timing.drawMs += t1 - t0;
    timing.frames++;
    if (last) timing.intervals.push(t0 - last);
    if (timing.intervals.length > 600) timing.intervals.shift();
    last = t0;
  });
  app.ports.audio.subscribe((cmds) => {
    for (const c of cmds) audio.command(c);
  });
  if (app.ports.status) app.ports.status.subscribe((s) => status(s));
  if (app.ports.store) {
    app.ports.store.subscribe(({ key, value }) => {
      try {
        if (value === null) localStorage.removeItem('doom:' + key);
        else localStorage.setItem('doom:' + key, value);
      } catch (e) { /* storage off: saves last as long as the page */ }
    });
  }
  attachInput(canvas, (e) => app.ports.event.send(e), () => audio.start());
  app.ports.wad.send(new File([buffer], 'doom.wad'));
  window.doomHost = { app, wad, screen, audio };
  return window.doomHost;
}
