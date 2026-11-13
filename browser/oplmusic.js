// Doom's music as Chocolate Doom plays it through an emulated OPL: a sample-exact
// port of its src/i_oplmusic.c (the DMX library's OPL music driver, Doom 1.9
// version), src/midifile.c and src/mus2mid.c (songs are turned into MIDI and
// played from there), opl/opl.c (register writes, the chip detection and
// initialisation sequence), opl/opl_queue.c (the timer callback heap) and
// opl/opl_sdl.c (how the audio thread runs the emulator between callbacks), all
// by Simon Howard and the Chocolate Doom authors, GPL-2.0-or-later. The emulator
// is ./opl3.js (Nuked OPL3).
//
// A `Music` is the audio device's view of all that: `deviceBuffer()` gives the
// next 1024 stereo frames at 49716 Hz, as SDL's audio thread would get them, and
// the music module's calls (register, play, pause, stop, volume) are made between
// buffers. doom/tools/music-ref renders the same thing with the C sources.
//
// Part of the Doom port in Elm-OS, GPL-2.0-or-later.

import { Opl3 } from './opl3.js';

export const OPL_RATE = 49716;
// Chocolate's GetSliceSize() at 49716 Hz with its default 28 ms slice
export const BUFFER_FRAMES = 1024;
const OPL_SECOND = 1000000;

// i_oplmusic.c's frequency_curve, copied (the OPL F-number for each 1/32 semitone)
const frequencyCurve = new Uint16Array([
  0x133, 0x133, 0x134, 0x134, 0x135, 0x136, 0x136, 0x137, 0x137, 0x138, 0x138, 0x139, 0x139, 0x13a, 0x13b, 0x13b,
  0x13c, 0x13c, 0x13d, 0x13d, 0x13e, 0x13f, 0x13f, 0x140, 0x140, 0x141, 0x142, 0x142, 0x143, 0x143, 0x144, 0x144,
  0x145, 0x146, 0x146, 0x147, 0x147, 0x148, 0x149, 0x149, 0x14a, 0x14a, 0x14b, 0x14c, 0x14c, 0x14d, 0x14d, 0x14e,
  0x14f, 0x14f, 0x150, 0x150, 0x151, 0x152, 0x152, 0x153, 0x153, 0x154, 0x155, 0x155, 0x156, 0x157, 0x157, 0x158,
  0x158, 0x159, 0x15a, 0x15a, 0x15b, 0x15b, 0x15c, 0x15d, 0x15d, 0x15e, 0x15f, 0x15f, 0x160, 0x161, 0x161, 0x162,
  0x162, 0x163, 0x164, 0x164, 0x165, 0x166, 0x166, 0x167, 0x168, 0x168, 0x169, 0x16a, 0x16a, 0x16b, 0x16c, 0x16c,
  0x16d, 0x16e, 0x16e, 0x16f, 0x170, 0x170, 0x171, 0x172, 0x172, 0x173, 0x174, 0x174, 0x175, 0x176, 0x176, 0x177,
  0x178, 0x178, 0x179, 0x17a, 0x17a, 0x17b, 0x17c, 0x17c, 0x17d, 0x17e, 0x17e, 0x17f, 0x180, 0x181, 0x181, 0x182,
  0x183, 0x183, 0x184, 0x185, 0x185, 0x186, 0x187, 0x188, 0x188, 0x189, 0x18a, 0x18a, 0x18b, 0x18c, 0x18d, 0x18d,
  0x18e, 0x18f, 0x18f, 0x190, 0x191, 0x192, 0x192, 0x193, 0x194, 0x194, 0x195, 0x196, 0x197, 0x197, 0x198, 0x199,
  0x19a, 0x19a, 0x19b, 0x19c, 0x19d, 0x19d, 0x19e, 0x19f, 0x1a0, 0x1a0, 0x1a1, 0x1a2, 0x1a3, 0x1a3, 0x1a4, 0x1a5,
  0x1a6, 0x1a6, 0x1a7, 0x1a8, 0x1a9, 0x1a9, 0x1aa, 0x1ab, 0x1ac, 0x1ad, 0x1ad, 0x1ae, 0x1af, 0x1b0, 0x1b0, 0x1b1,
  0x1b2, 0x1b3, 0x1b4, 0x1b4, 0x1b5, 0x1b6, 0x1b7, 0x1b8, 0x1b8, 0x1b9, 0x1ba, 0x1bb, 0x1bc, 0x1bc, 0x1bd, 0x1be,
  0x1bf, 0x1c0, 0x1c0, 0x1c1, 0x1c2, 0x1c3, 0x1c4, 0x1c4, 0x1c5, 0x1c6, 0x1c7, 0x1c8, 0x1c9, 0x1c9, 0x1ca, 0x1cb,
  0x1cc, 0x1cd, 0x1ce, 0x1ce, 0x1cf, 0x1d0, 0x1d1, 0x1d2, 0x1d3, 0x1d3, 0x1d4, 0x1d5, 0x1d6, 0x1d7, 0x1d8, 0x1d8,
  0x1d9, 0x1da, 0x1db, 0x1dc, 0x1dd, 0x1de, 0x1de, 0x1df, 0x1e0, 0x1e1, 0x1e2, 0x1e3, 0x1e4, 0x1e5, 0x1e5, 0x1e6,
  0x1e7, 0x1e8, 0x1e9, 0x1ea, 0x1eb, 0x1ec, 0x1ed, 0x1ed, 0x1ee, 0x1ef, 0x1f0, 0x1f1, 0x1f2, 0x1f3, 0x1f4, 0x1f5,
  0x1f6, 0x1f6, 0x1f7, 0x1f8, 0x1f9, 0x1fa, 0x1fb, 0x1fc, 0x1fd, 0x1fe, 0x1ff, 0x200, 0x201, 0x201, 0x202, 0x203,
  0x204, 0x205, 0x206, 0x207, 0x208, 0x209, 0x20a, 0x20b, 0x20c, 0x20d, 0x20e, 0x20f, 0x210, 0x210, 0x211, 0x212,
  0x213, 0x214, 0x215, 0x216, 0x217, 0x218, 0x219, 0x21a, 0x21b, 0x21c, 0x21d, 0x21e, 0x21f, 0x220, 0x221, 0x222,
  0x223, 0x224, 0x225, 0x226, 0x227, 0x228, 0x229, 0x22a, 0x22b, 0x22c, 0x22d, 0x22e, 0x22f, 0x230, 0x231, 0x232,
  0x233, 0x234, 0x235, 0x236, 0x237, 0x238, 0x239, 0x23a, 0x23b, 0x23c, 0x23d, 0x23e, 0x23f, 0x240, 0x241, 0x242,
  0x244, 0x245, 0x246, 0x247, 0x248, 0x249, 0x24a, 0x24b, 0x24c, 0x24d, 0x24e, 0x24f, 0x250, 0x251, 0x252, 0x253,
  0x254, 0x256, 0x257, 0x258, 0x259, 0x25a, 0x25b, 0x25c, 0x25d, 0x25e, 0x25f, 0x260, 0x262, 0x263, 0x264, 0x265,
  0x266, 0x267, 0x268, 0x269, 0x26a, 0x26c, 0x26d, 0x26e, 0x26f, 0x270, 0x271, 0x272, 0x273, 0x275, 0x276, 0x277,
  0x278, 0x279, 0x27a, 0x27b, 0x27d, 0x27e, 0x27f, 0x280, 0x281, 0x282, 0x284, 0x285, 0x286, 0x287, 0x288, 0x289,
  0x28b, 0x28c, 0x28d, 0x28e, 0x28f, 0x290, 0x292, 0x293, 0x294, 0x295, 0x296, 0x298, 0x299, 0x29a, 0x29b, 0x29c,
  0x29e, 0x29f, 0x2a0, 0x2a1, 0x2a2, 0x2a4, 0x2a5, 0x2a6, 0x2a7, 0x2a9, 0x2aa, 0x2ab, 0x2ac, 0x2ae, 0x2af, 0x2b0,
  0x2b1, 0x2b2, 0x2b4, 0x2b5, 0x2b6, 0x2b7, 0x2b9, 0x2ba, 0x2bb, 0x2bd, 0x2be, 0x2bf, 0x2c0, 0x2c2, 0x2c3, 0x2c4,
  0x2c5, 0x2c7, 0x2c8, 0x2c9, 0x2cb, 0x2cc, 0x2cd, 0x2ce, 0x2d0, 0x2d1, 0x2d2, 0x2d4, 0x2d5, 0x2d6, 0x2d8, 0x2d9,
  0x2da, 0x2dc, 0x2dd, 0x2de, 0x2e0, 0x2e1, 0x2e2, 0x2e4, 0x2e5, 0x2e6, 0x2e8, 0x2e9, 0x2ea, 0x2ec, 0x2ed, 0x2ee,
  0x2f0, 0x2f1, 0x2f2, 0x2f4, 0x2f5, 0x2f6, 0x2f8, 0x2f9, 0x2fb, 0x2fc, 0x2fd, 0x2ff, 0x300, 0x302, 0x303, 0x304,
  0x306, 0x307, 0x309, 0x30a, 0x30b, 0x30d, 0x30e, 0x310, 0x311, 0x312, 0x314, 0x315, 0x317, 0x318, 0x31a, 0x31b,
  0x31c, 0x31e, 0x31f, 0x321, 0x322, 0x324, 0x325, 0x327, 0x328, 0x329, 0x32b, 0x32c, 0x32e, 0x32f, 0x331, 0x332,
  0x334, 0x335, 0x337, 0x338, 0x33a, 0x33b, 0x33d, 0x33e, 0x340, 0x341, 0x343, 0x344, 0x346, 0x347, 0x349, 0x34a,
  0x34c, 0x34d, 0x34f, 0x350, 0x352, 0x353, 0x355, 0x357, 0x358, 0x35a, 0x35b, 0x35d, 0x35e, 0x360, 0x361, 0x363,
  0x365, 0x366, 0x368, 0x369, 0x36b, 0x36c, 0x36e, 0x370, 0x371, 0x373, 0x374, 0x376, 0x378, 0x379, 0x37b, 0x37c,
  0x37e, 0x380, 0x381, 0x383, 0x384, 0x386, 0x388, 0x389, 0x38b, 0x38d, 0x38e, 0x390, 0x392, 0x393, 0x395, 0x397,
  0x398, 0x39a, 0x39c, 0x39d, 0x39f, 0x3a1, 0x3a2, 0x3a4, 0x3a6, 0x3a7, 0x3a9, 0x3ab, 0x3ac, 0x3ae, 0x3b0, 0x3b1,
  0x3b3, 0x3b5, 0x3b7, 0x3b8, 0x3ba, 0x3bc, 0x3bd, 0x3bf, 0x3c1, 0x3c3, 0x3c4, 0x3c6, 0x3c8, 0x3ca, 0x3cb, 0x3cd,
  0x3cf, 0x3d1, 0x3d2, 0x3d4, 0x3d6, 0x3d8, 0x3da, 0x3db, 0x3dd, 0x3df, 0x3e1, 0x3e3, 0x3e4, 0x3e6, 0x3e8, 0x3ea,
  0x3ec, 0x3ed, 0x3ef, 0x3f1, 0x3f3, 0x3f5, 0x3f6, 0x3f8, 0x3fa, 0x3fc, 0x3fe, 0x36c,
]);

const volumeMappingTable = [
  0, 1, 3, 5, 6, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 23,
  25, 26, 27, 29, 30, 32, 33, 34, 36, 37, 39, 41, 43, 45, 47, 49,
  50, 52, 54, 55, 57, 59, 60, 61, 63, 64, 66, 67, 68, 69, 71, 72,
  73, 74, 75, 76, 77, 79, 80, 81, 82, 83, 84, 84, 85, 86, 87, 88,
  89, 90, 91, 92, 92, 93, 94, 95, 96, 96, 97, 98, 99, 99, 100, 101,
  101, 102, 103, 103, 104, 105, 105, 106, 107, 107, 108, 109, 109, 110, 110, 111,
  112, 112, 113, 113, 114, 114, 115, 115, 116, 117, 117, 118, 118, 119, 119, 120,
  120, 121, 121, 122, 122, 123, 123, 123, 124, 124, 125, 125, 126, 126, 127, 127,
];

const voiceOperators = [
  [0x00, 0x01, 0x02, 0x08, 0x09, 0x0a, 0x10, 0x11, 0x12],
  [0x03, 0x04, 0x05, 0x0b, 0x0c, 0x0d, 0x13, 0x14, 0x15],
];

const OPL_NUM_VOICES = 9;
const OPL_NUM_OPERATORS = 21;
const OPL_REG_WAVEFORM_ENABLE = 0x01;
const OPL_REG_TIMER1 = 0x02;
const OPL_REG_TIMER2 = 0x03;
const OPL_REG_TIMER_CTRL = 0x04;
const OPL_REG_FM_MODE = 0x08;
const OPL_REG_NEW = 0x105;
const OPL_REGS_TREMOLO = 0x20;
const OPL_REGS_LEVEL = 0x40;
const OPL_REGS_ATTACK = 0x60;
const OPL_REGS_SUSTAIN = 0x80;
const OPL_REGS_WAVEFORM = 0xe0;
const OPL_REGS_FREQ_1 = 0xa0;
const OPL_REGS_FREQ_2 = 0xb0;
const OPL_REGS_FEEDBACK = 0xc0;

const GENMIDI_NUM_INSTRS = 128;
const GENMIDI_FLAG_FIXED = 0x0001;
const GENMIDI_FLAG_2VOICE = 0x0004;
const MIDI_CHANNELS_PER_TRACK = 16;
const MAXMIDLENGTH = 96 * 1024;

const MIDI_EVENT_NOTE_OFF = 0x80;
const MIDI_EVENT_NOTE_ON = 0x90;
const MIDI_EVENT_AFTERTOUCH = 0xa0;
const MIDI_EVENT_CONTROLLER = 0xb0;
const MIDI_EVENT_PROGRAM_CHANGE = 0xc0;
const MIDI_EVENT_CHAN_AFTERTOUCH = 0xd0;
const MIDI_EVENT_PITCH_BEND = 0xe0;
const MIDI_EVENT_SYSEX = 0xf0;
const MIDI_EVENT_SYSEX_SPLIT = 0xf7;
const MIDI_EVENT_META = 0xff;
const MIDI_CONTROLLER_VOLUME_MSB = 0x07;
const MIDI_CONTROLLER_PAN = 0x0a;
const MIDI_CONTROLLER_ALL_NOTES_OFF = 0x7b;
const MIDI_META_END_OF_TRACK = 0x2f;
const MIDI_META_SET_TEMPO = 0x51;

const OPL_DOOM1_1_666 = 0;
const OPL_DOOM2_1_666 = 1;
const OPL_DOOM_1_9 = 2;

// ---------------------------------------------------------------------------
// opl_queue.c: a binary min-heap of timer callbacks

const MAX_OPL_QUEUE = 64;

class CallbackQueue {
  constructor() {
    this.entries = [];
    for (let i = 0; i < MAX_OPL_QUEUE; i++) this.entries.push({ callback: null, data: null, time: 0 });
    this.num = 0;
  }

  isEmpty() { return this.num === 0; }

  clear() { this.num = 0; }

  push(callback, data, time) {
    if (this.num >= MAX_OPL_QUEUE) return; // "Exceeded maximum callbacks"
    let id = this.num++;
    while (id > 0) {
      const parent = Math.floor((id - 1) / 2);
      if (time >= this.entries[parent].time) break;
      Object.assign(this.entries[id], this.entries[parent]);
      id = parent;
    }
    const e = this.entries[id];
    e.callback = callback;
    e.data = data;
    e.time = time;
  }

  pop() {
    if (this.num <= 0) return null;
    const result = { callback: this.entries[0].callback, data: this.entries[0].data };
    this.num--;
    const entry = { ...this.entries[this.num] };
    let i = 0;
    for (;;) {
      const c1 = i * 2 + 1;
      const c2 = i * 2 + 2;
      let next;
      if (c1 < this.num && this.entries[c1].time < entry.time) {
        next = c2 < this.num && this.entries[c2].time < this.entries[c1].time ? c2 : c1;
      } else if (c2 < this.num && this.entries[c2].time < entry.time) {
        next = c2;
      } else {
        break;
      }
      Object.assign(this.entries[i], this.entries[next]);
      i = next;
    }
    Object.assign(this.entries[i], entry);
    return result;
  }

  peek() { return this.num > 0 ? this.entries[0].time : 0; }

  // OPL_Queue_AdjustCallbacks: float arithmetic, as in C (offset / factor)
  adjust(time, factor) {
    for (let i = 0; i < this.num; i++) {
      const offset = this.entries[i].time - time;
      this.entries[i].time = time + Math.trunc(Math.fround(Math.fround(offset) / factor));
    }
  }
}

// ---------------------------------------------------------------------------
// midifile.c

class Reader {
  constructor(bytes) {
    this.bytes = bytes;
    this.pos = 0;
  }

  byte() {
    if (this.pos >= this.bytes.length) return -1;
    return this.bytes[this.pos++];
  }

  // ReadVariableLength: at most four bytes
  vlq() {
    let result = 0;
    for (let i = 0; i < 4; i++) {
      const b = this.byte();
      if (b < 0) return -1;
      result = ((result << 7) | (b & 0x7f)) >>> 0;
      if ((b & 0x80) === 0) return result;
    }
    return -1;
  }

  sequence(n) {
    if (this.pos + n > this.bytes.length) return null;
    const r = this.bytes.slice(this.pos, this.pos + n);
    this.pos += n;
    return r;
  }
}

function readEvent(r, state) {
  const event = { deltaTime: 0, type: 0, channel: 0, param1: 0, param2: 0, metaType: 0, data: null };
  const delta = r.vlq();
  if (delta < 0) return null;
  event.deltaTime = delta;
  let eventType = r.byte();
  if (eventType < 0) return null;
  if ((eventType & 0x80) === 0) {
    eventType = state.last;
    r.pos -= 1;
  } else {
    state.last = eventType;
  }
  const channelEvent = (two) => {
    event.type = eventType & 0xf0;
    event.channel = eventType & 0x0f;
    const a = r.byte();
    if (a < 0) return null;
    event.param1 = a;
    if (two) {
      const b = r.byte();
      if (b < 0) return null;
      event.param2 = b;
    }
    return event;
  };
  switch (eventType & 0xf0) {
    case MIDI_EVENT_NOTE_OFF:
    case MIDI_EVENT_NOTE_ON:
    case MIDI_EVENT_AFTERTOUCH:
    case MIDI_EVENT_CONTROLLER:
    case MIDI_EVENT_PITCH_BEND:
      return channelEvent(true);
    case MIDI_EVENT_PROGRAM_CHANGE:
    case MIDI_EVENT_CHAN_AFTERTOUCH:
      return channelEvent(false);
    default:
      break;
  }
  switch (eventType) {
    case MIDI_EVENT_SYSEX:
    case MIDI_EVENT_SYSEX_SPLIT: {
      event.type = eventType;
      const len = r.vlq();
      if (len < 0) return null;
      event.data = r.sequence(len);
      return event.data ? event : null;
    }
    case MIDI_EVENT_META: {
      event.type = MIDI_EVENT_META;
      const t = r.byte();
      if (t < 0) return null;
      event.metaType = t;
      const len = r.vlq();
      if (len < 0) return null;
      event.data = r.sequence(len);
      return event.data ? event : null;
    }
    default:
      return null;
  }
}

// MIDI_LoadFile, from the file's bytes
function loadMidi(bytes) {
  const r = new Reader(bytes);
  const header = r.sequence(14);
  if (!header) return null;
  const id = String.fromCharCode(header[0], header[1], header[2], header[3]);
  const size = ((header[4] << 24) | (header[5] << 16) | (header[6] << 8) | header[7]) >>> 0;
  if (id !== 'MThd' || size !== 6) return null;
  const format = (header[8] << 8) | header[9];
  const numTracks = (header[10] << 8) | header[11];
  let division = (header[12] << 8) | header[13];
  if ((format !== 0 && format !== 1) || numTracks < 1) return null;
  const tracks = [];
  for (let t = 0; t < numTracks; t++) {
    const th = r.sequence(8);
    if (!th) return null;
    if (String.fromCharCode(th[0], th[1], th[2], th[3]) !== 'MTrk') return null;
    const events = [];
    const state = { last: 0 };
    for (;;) {
      const e = readEvent(r, state);
      if (!e) return null;
      events.push(e);
      if (e.type === MIDI_EVENT_META && e.metaType === MIDI_META_END_OF_TRACK) break;
    }
    tracks.push(events);
  }
  // MIDI_GetFileTimeDivision: SMPTE divisions as frames a second times ticks
  if (division & 0x8000) {
    const s = division - 0x10000;
    division = -Math.trunc(s / 256) * (s & 0xff);
  }
  return { tracks, division };
}

// ---------------------------------------------------------------------------
// mus2mid.c. Its channel velocities and queued time are file-level statics that
// carry over from one conversion to the next, so they are kept here too.

const mus2midState = { channelvelocities: new Array(16).fill(127), queuedtime: 0 };
const controllerMap = [0x00, 0x20, 0x01, 0x07, 0x0a, 0x0b, 0x5b, 0x5d, 0x40, 0x43, 0x78, 0x7b, 0x7e, 0x7f, 0x79];

function mus2mid(mus) {
  const st = mus2midState;
  const out = [];
  let tracksize = 0;
  const channelMap = new Array(16).fill(-1);
  const writeTime = (time) => {
    let buffer = time & 0x7f;
    while ((time >>>= 7) !== 0) {
      buffer = (buffer << 8) >>> 0;
      buffer = (buffer | ((time & 0x7f) | 0x80)) >>> 0;
    }
    for (;;) {
      out.push(buffer & 0xff);
      ++tracksize;
      if ((buffer & 0x80) !== 0) {
        buffer >>>= 8;
      } else {
        st.queuedtime = 0;
        return;
      }
    }
  };
  const event3 = (a, b, c) => {
    writeTime(st.queuedtime);
    out.push(a & 0xff, b & 0xff, c & 0xff);
    tracksize += 3;
  };
  const controllerValued = (channel, control, value) => {
    let v = value & 0xff;
    if (v & 0x80) v = 0x7f;
    event3(0xb0 | channel, control & 0x7f, v);
  };
  const allocate = () => {
    let max = -1;
    for (let i = 0; i < 16; i++) if (channelMap[i] > max) max = channelMap[i];
    let result = max + 1;
    if (result === 9) ++result;
    return result;
  };
  const midiChannel = (musChannel) => {
    if (musChannel === 15) return 9;
    if (channelMap[musChannel] === -1) {
      channelMap[musChannel] = allocate();
      controllerValued(channelMap[musChannel], 0x7b, 0);
    }
    return channelMap[musChannel];
  };

  if (mus.length < 14) return null;
  const scorestart = mus[6] | (mus[7] << 8);
  let pos = scorestart;
  if (pos > mus.length) return null; // mem_fseek past the end
  const read = () => (pos < mus.length ? mus[pos++] : -1);

  // the MIDI header, with a placeholder for the track length
  out.push(0x4d, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 0, 0, 1, 0, 0x46, 0x4d, 0x54, 0x72, 0x6b, 0, 0, 0, 0);
  let hitscoreend = false;
  while (!hitscoreend) {
    while (!hitscoreend) {
      const descriptor = read();
      if (descriptor < 0) return null;
      const channel = midiChannel(descriptor & 0x0f);
      switch (descriptor & 0x70) {
        case 0x00: { // release key
          const key = read();
          if (key < 0) return null;
          event3(0x80 | channel, key & 0x7f, 0);
          break;
        }
        case 0x10: { // press key
          const key = read();
          if (key < 0) return null;
          if (key & 0x80) {
            const v = read();
            if (v < 0) return null;
            st.channelvelocities[channel] = v & 0x7f;
          }
          event3(0x90 | channel, key & 0x7f, st.channelvelocities[channel] & 0x7f);
          break;
        }
        case 0x20: { // pitch wheel
          const key = read();
          if (key < 0) break;
          const wheel = (key * 64) << 16 >> 16;
          event3(0xe0 | channel, wheel & 0x7f, (wheel >> 7) & 0x7f);
          break;
        }
        case 0x30: { // system event
          const number = read();
          if (number < 0) return null;
          if (number < 10 || number > 14) return null;
          controllerValued(channel, controllerMap[number], 0);
          break;
        }
        case 0x40: { // change controller
          const number = read();
          if (number < 0) return null;
          const value = read();
          if (value < 0) return null;
          if (number === 0) {
            writeTime(st.queuedtime);
            out.push(0xc0 | channel, value & 0x7f);
            tracksize += 2;
          } else {
            if (number < 1 || number > 9) return null;
            controllerValued(channel, controllerMap[number], value);
          }
          break;
        }
        case 0x60:
          hitscoreend = true;
          break;
        default:
          return null;
      }
      if (descriptor & 0x80) break;
    }
    if (!hitscoreend) {
      let timedelay = 0;
      for (;;) {
        const w = read();
        if (w < 0) return null;
        timedelay = (timedelay * 128 + (w & 0x7f)) >>> 0;
        if ((w & 0x80) === 0) break;
      }
      st.queuedtime = (st.queuedtime + timedelay) >>> 0;
    }
  }
  writeTime(st.queuedtime);
  out.push(0xff, 0x2f, 0x00);
  tracksize += 3;
  out[18] = (tracksize >>> 24) & 0xff;
  out[19] = (tracksize >>> 16) & 0xff;
  out[20] = (tracksize >>> 8) & 0xff;
  out[21] = tracksize & 0xff;
  return Uint8Array.from(out);
}

// ---------------------------------------------------------------------------
// GENMIDI

function parseGenmidi(bytes) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const op = (at) => ({
    tremolo: bytes[at], attack: bytes[at + 1], sustain: bytes[at + 2],
    waveform: bytes[at + 3], scale: bytes[at + 4], level: bytes[at + 5],
  });
  const instruments = [];
  for (let i = 0; i < 175; i++) {
    const at = 8 + i * 36;
    const voice = (v) => {
      const b = at + 4 + v * 16;
      return { modulator: op(b), feedback: bytes[b + 6], carrier: op(b + 7), baseNoteOffset: view.getInt16(b + 14, true) };
    };
    instruments.push({
      index: i,
      flags: view.getUint16(at, true),
      fineTuning: bytes[at + 2],
      fixedNote: bytes[at + 3],
      voices: [voice(0), voice(1)],
    });
  }
  return instruments;
}

// ---------------------------------------------------------------------------
// The device and the music module

export class Music {
  // genmidi: the GENMIDI lump's bytes
  constructor(genmidi) {
    // opl_sdl.c
    this.chip = null;
    this.queue = new CallbackQueue();
    this.currentTime = 0;
    this.paused = 0;
    this.pauseOffset = 0;
    this.registerNum = 0;
    this.timer1 = { rate: 12500, enabled: 0, value: 0, expire: 0 };
    this.timer2 = { rate: 3125, enabled: 0, value: 0, expire: 0 };
    this.opl3mode = 0;
    this.initStageRegWrites = 1;
    // what the device has played (OPL_Detect's delays play buffers)
    this.pending = [];

    // i_oplmusic.c
    this.genmidi = genmidi;
    this.drvVer = OPL_DOOM_1_9;
    this.initialized = false;
    this.startMusicVolume = 0;
    this.currentMusicVolume = 0;
    this.voices = [];
    this.voiceFreeList = [];
    this.voiceAllocedList = [];
    this.channels = [];
    for (let i = 0; i < MIDI_CHANNELS_PER_TRACK; i++) {
      this.channels.push({ num: i, instrument: null, volume: 0, volumeBase: 0, pan: 0, bend: 0 });
    }
    this.tracks = null;
    this.numTracks = 0;
    this.runningTracks = 0;
    this.songLooping = false;
    this.ticksPerBeat = 0;
    this.usPerBeat = 0;
  }

  // ---- opl_sdl.c

  driverInit() {
    this.paused = 0;
    this.pauseOffset = 0;
    this.queue = new CallbackQueue();
    this.currentTime = 0;
    this.chip = new Opl3(OPL_RATE);
    this.opl3mode = 0;
  }

  advanceTime(nsamples) {
    const us = Math.floor((nsamples * OPL_SECOND) / OPL_RATE);
    this.currentTime += us;
    if (this.paused) this.pauseOffset += us;
    while (!this.queue.isEmpty() && this.currentTime >= this.queue.peek() + this.pauseOffset) {
      const c = this.queue.pop();
      if (!c) break;
      c.callback.call(this, c.data);
    }
  }

  // OPL_Mix_Callback: fill one device buffer (out: Int16Array of
  // BUFFER_FRAMES * 2), running callbacks at their times
  mix(out) {
    out.fill(0);
    let filled = 0;
    const frames = BUFFER_FRAMES;
    while (filled < frames) {
      let nsamples;
      if (this.paused || this.queue.isEmpty()) {
        nsamples = frames - filled;
      } else {
        const next = this.queue.peek() + this.pauseOffset;
        nsamples = (next - this.currentTime) * OPL_RATE;
        nsamples = Math.floor((nsamples + OPL_SECOND - 1) / OPL_SECOND);
        if (nsamples > frames - filled) nsamples = frames - filled;
      }
      this.chip.generateStream(out, filled * 2, nsamples);
      filled += nsamples;
      this.advanceTime(nsamples);
    }
  }

  // the next buffer the device plays
  deviceBuffer(out) {
    if (this.pending.length) {
      out.set(this.pending.shift());
      return;
    }
    this.mix(out);
  }

  calculateEndTime(timer) {
    if (timer.enabled) {
      const tics = 0x100 - timer.value;
      timer.expire = this.currentTime + Math.floor((tics * OPL_SECOND) / timer.rate);
    }
  }

  writeRegisterSdl(reg, value) {
    switch (reg) {
      case OPL_REG_TIMER1:
        this.timer1.value = value;
        this.calculateEndTime(this.timer1);
        break;
      case OPL_REG_TIMER2:
        this.timer2.value = value;
        this.calculateEndTime(this.timer2);
        break;
      case OPL_REG_TIMER_CTRL:
        if (value & 0x80) {
          this.timer1.enabled = 0;
          this.timer2.enabled = 0;
        } else {
          if ((value & 0x40) === 0) {
            this.timer1.enabled = (value & 0x01) !== 0 ? 1 : 0;
            this.calculateEndTime(this.timer1);
          }
          if ((value & 0x20) === 0) {
            this.timer2.enabled = (value & 0x02) !== 0 ? 1 : 0;
            this.calculateEndTime(this.timer2);
          }
        }
        break;
      case OPL_REG_NEW:
        this.opl3mode = value & 0x01;
        this.chip.writeRegBuffered(reg, value);
        break;
      default:
        this.chip.writeRegBuffered(reg, value);
        break;
    }
  }

  readStatus() {
    let result = 0;
    if (this.timer1.enabled && this.currentTime > this.timer1.expire) result |= 0x80 | 0x40;
    if (this.timer2.enabled && this.currentTime > this.timer2.expire) result |= 0x80 | 0x20;
    return result;
  }

  setCallback(us, callback, data) {
    this.queue.push(callback, data, this.currentTime - this.pauseOffset + us);
  }

  // ---- opl.c

  // OPL_WriteRegister: the port reads in between only time the real chip
  writeRegister(reg, value) {
    this.registerNum = reg & 0x100 ? (reg & 0xff) | 0x100 : reg & 0xff;
    this.writeRegisterSdl(this.registerNum, value & 0xff);
  }

  // OPL_Delay: the caller waits while the device plays buffers
  delay(us) {
    const state = { finished: false };
    this.setCallback(us, () => { state.finished = true; }, null);
    while (!state.finished) {
      const buf = new Int16Array(BUFFER_FRAMES * 2);
      this.mix(buf);
      this.pending.push(buf);
    }
  }

  detect() {
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x60);
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x80);
    const result1 = this.readStatus();
    this.writeRegister(OPL_REG_TIMER1, 0xff);
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x21);
    this.delay(1000);
    const result2 = this.readStatus();
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x60);
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x80);
    if ((result1 & 0xe0) === 0x00 && (result2 & 0xe0) === 0xc0) {
      return this.readStatus() === 0x00 ? 'opl3' : 'opl2';
    }
    return 'none';
  }

  oplInit() {
    this.driverInit();
    this.initStageRegWrites = 1;
    this.detect();
    const result = this.detect();
    this.initStageRegWrites = 0;
    return result;
  }

  initRegisters(opl3) {
    for (let r = OPL_REGS_LEVEL; r <= OPL_REGS_LEVEL + OPL_NUM_OPERATORS; ++r) this.writeRegister(r, 0x3f);
    for (let r = OPL_REGS_ATTACK; r <= OPL_REGS_WAVEFORM + OPL_NUM_OPERATORS; ++r) this.writeRegister(r, 0x00);
    for (let r = 1; r < OPL_REGS_LEVEL; ++r) this.writeRegister(r, 0x00);
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x60);
    this.writeRegister(OPL_REG_TIMER_CTRL, 0x80);
    this.writeRegister(OPL_REG_WAVEFORM_ENABLE, 0x20);
    if (opl3) {
      this.writeRegister(OPL_REG_NEW, 0x01);
      for (let r = OPL_REGS_LEVEL; r <= OPL_REGS_LEVEL + OPL_NUM_OPERATORS; ++r) this.writeRegister(r | 0x100, 0x3f);
      for (let r = OPL_REGS_ATTACK; r <= OPL_REGS_WAVEFORM + OPL_NUM_OPERATORS; ++r) this.writeRegister(r | 0x100, 0x00);
      for (let r = 1; r < OPL_REGS_LEVEL; ++r) this.writeRegister(r | 0x100, 0x00);
    }
    this.writeRegister(OPL_REG_FM_MODE, 0x40);
    if (opl3) this.writeRegister(OPL_REG_NEW, 0x01);
  }

  // ---- i_oplmusic.c: voices

  getFreeVoice() {
    if (this.voiceFreeList.length === 0) return null;
    const result = this.voiceFreeList.shift();
    this.voiceAllocedList.push(result);
    return result;
  }

  releaseVoice(index) {
    if (index >= this.voiceAllocedList.length) {
      // Doom 2 1.666 crash emulation
      this.voiceAllocedList.length = 0;
      this.voiceFreeList.length = 0;
      return;
    }
    const voice = this.voiceAllocedList[index];
    this.voiceKeyOff(voice);
    voice.channel = null;
    voice.note = 0;
    const doubleVoice = voice.currentInstrVoice !== 0;
    this.voiceAllocedList.splice(index, 1);
    this.voiceFreeList.push(voice);
    if (doubleVoice && this.drvVer < OPL_DOOM_1_9) this.releaseVoice(index);
  }

  loadOperatorData(operator, data, maxLevel) {
    let level = data.scale;
    level |= maxLevel ? 0x3f : data.level;
    this.writeRegister(OPL_REGS_LEVEL + operator, level);
    this.writeRegister(OPL_REGS_TREMOLO + operator, data.tremolo);
    this.writeRegister(OPL_REGS_ATTACK + operator, data.attack);
    this.writeRegister(OPL_REGS_SUSTAIN + operator, data.sustain);
    this.writeRegister(OPL_REGS_WAVEFORM + operator, data.waveform);
    return level;
  }

  setVoiceInstrument(voice, instr, instrVoice) {
    if (voice.currentInstr === instr && voice.currentInstrVoice === instrVoice) return;
    voice.currentInstr = instr;
    voice.currentInstrVoice = instrVoice;
    const data = instr.voices[instrVoice];
    const modulating = (data.feedback & 0x01) === 0;
    voice.carVolume = this.loadOperatorData(voice.op2 | voice.array, data.carrier, true);
    voice.modVolume = this.loadOperatorData(voice.op1 | voice.array, data.modulator, !modulating);
    this.writeRegister((OPL_REGS_FEEDBACK + voice.index) | voice.array, data.feedback | voice.regPan);
    voice.priority = 0x0f - (data.carrier.attack >> 4) + 0x0f - (data.carrier.sustain & 0x0f);
  }

  setVoiceVolume(voice, volume) {
    voice.noteVolume = volume;
    const oplVoice = voice.currentInstr.voices[voice.currentInstrVoice];
    const midiVolume = 2 * (volumeMappingTable[voice.channel.volume] + 1);
    const fullVolume = (volumeMappingTable[voice.noteVolume] * midiVolume) >>> 9;
    const carVolume = (0x3f - fullVolume) >>> 0;
    if (carVolume !== (voice.carVolume & 0x3f)) {
      voice.carVolume = (carVolume | (voice.carVolume & 0xc0)) >>> 0;
      this.writeRegister((OPL_REGS_LEVEL + voice.op2) | voice.array, voice.carVolume);
      if ((oplVoice.feedback & 0x01) !== 0 && oplVoice.modulator.level !== 0x3f) {
        let modVolume = oplVoice.modulator.level;
        if (modVolume < carVolume) modVolume = carVolume;
        modVolume |= voice.modVolume & 0xc0;
        if (modVolume !== voice.modVolume) {
          voice.modVolume = modVolume;
          this.writeRegister((OPL_REGS_LEVEL + voice.op1) | voice.array, modVolume | (oplVoice.modulator.scale & 0xc0));
        }
      }
    }
  }

  setVoicePan(voice, pan) {
    voice.regPan = pan;
    const oplVoice = voice.currentInstr.voices[voice.currentInstrVoice];
    this.writeRegister((OPL_REGS_FEEDBACK + voice.index) | voice.array, oplVoice.feedback | pan);
  }

  initVoices() {
    this.voiceFreeList = [];
    this.voiceAllocedList = [];
    this.voices = [];
    for (let i = 0; i < this.numOplVoices; ++i) {
      const v = {
        index: i % OPL_NUM_VOICES,
        op1: voiceOperators[0][i % OPL_NUM_VOICES],
        op2: voiceOperators[1][i % OPL_NUM_VOICES],
        array: Math.floor(i / OPL_NUM_VOICES) << 8,
        currentInstr: null,
        currentInstrVoice: 0,
        channel: null,
        key: 0,
        note: 0,
        freq: 0,
        noteVolume: 0,
        carVolume: 0,
        modVolume: 0,
        regPan: 0,
        priority: 0,
      };
      this.voices.push(v);
      this.voiceFreeList.push(v);
    }
  }

  voiceKeyOff(voice) {
    this.writeRegister((OPL_REGS_FREQ_2 + voice.index) | voice.array, voice.freq >>> 8);
  }

  trackChannelForEvent(event) {
    let n = event.channel;
    if (n === 9) n = 15;
    else if (n === 15) n = 9;
    return this.channels[n];
  }

  keyOffEvent(event) {
    const channel = this.trackChannelForEvent(event);
    const key = event.param1;
    for (let i = 0; i < this.voiceAllocedList.length; i++) {
      const v = this.voiceAllocedList[i];
      if (v.channel === channel && v.key === key) {
        this.releaseVoice(i);
        i--;
      }
    }
  }

  replaceExistingVoice() {
    const list = this.voiceAllocedList;
    let result = 0;
    for (let i = 0; i < list.length; i++) {
      if (list[i].currentInstrVoice !== 0 || list[i].channel.num >= list[result].channel.num) result = i;
    }
    this.releaseVoice(result);
  }

  replaceExistingVoiceDoom1() {
    const list = this.voiceAllocedList;
    let result = 0;
    for (let i = 0; i < list.length; i++) {
      if (list[i].channel.num > list[result].channel.num) result = i;
    }
    this.releaseVoice(result);
  }

  replaceExistingVoiceDoom2(channel) {
    const list = this.voiceAllocedList;
    let result = 0;
    let priority = 0x8000;
    for (let i = 0; i < list.length - 3; i++) {
      if (list[i].priority < priority && list[i].channel.num >= channel.num) {
        priority = list[i].priority;
        result = i;
      }
    }
    this.releaseVoice(result);
  }

  frequencyForVoice(voice) {
    let note = voice.note;
    const gm = voice.currentInstr.voices[voice.currentInstrVoice];
    if ((voice.currentInstr.flags & GENMIDI_FLAG_FIXED) === 0) note += gm.baseNoteOffset;
    while (note < 0) note += 12;
    while (note > 95) note -= 12;
    let freqIndex = 64 + 32 * note + voice.channel.bend;
    if (voice.currentInstrVoice !== 0) freqIndex += (voice.currentInstr.fineTuning >> 1) - 64;
    if (freqIndex < 0) freqIndex = 0;
    if (freqIndex < 284) return frequencyCurve[freqIndex];
    const sub = (freqIndex - 284) % (12 * 32);
    let octave = Math.floor((freqIndex - 284) / (12 * 32));
    if (octave >= 7) octave = 7;
    return frequencyCurve[sub + 284] | (octave << 10);
  }

  updateVoiceFrequency(voice) {
    const freq = this.frequencyForVoice(voice);
    if (voice.freq !== freq) {
      this.writeRegister((OPL_REGS_FREQ_1 + voice.index) | voice.array, freq & 0xff);
      this.writeRegister((OPL_REGS_FREQ_2 + voice.index) | voice.array, (freq >>> 8) | 0x20);
      voice.freq = freq;
    }
  }

  voiceKeyOn(channel, instrument, instrumentVoice, note, key, volume) {
    if (!this.opl3modeMusic && this.drvVer === OPL_DOOM1_1_666) instrumentVoice = 0;
    const voice = this.getFreeVoice();
    if (voice === null) return;
    voice.channel = channel;
    voice.key = key;
    voice.note = (instrument.flags & GENMIDI_FLAG_FIXED) !== 0 ? instrument.fixedNote : note;
    voice.regPan = channel.pan;
    this.setVoiceInstrument(voice, instrument, instrumentVoice);
    this.setVoiceVolume(voice, volume);
    voice.freq = 0;
    this.updateVoiceFrequency(voice);
  }

  keyOnEvent(event) {
    let note = event.param1;
    const key = event.param1;
    const volume = event.param2;
    if (volume <= 0) {
      this.keyOffEvent(event);
      return;
    }
    const channel = this.trackChannelForEvent(event);
    let instrument;
    if (event.channel === 9) {
      if (key < 35 || key > 81) return;
      instrument = this.instruments[GENMIDI_NUM_INSTRS + key - 35];
      note = 60;
    } else {
      instrument = channel.instrument;
    }
    const doubleVoice = (instrument.flags & GENMIDI_FLAG_2VOICE) !== 0;
    switch (this.drvVer) {
      case OPL_DOOM1_1_666: {
        let voicenum = (doubleVoice ? 1 : 0) + 1;
        if (!this.opl3modeMusic) voicenum = 1;
        while (this.voiceAllocedList.length > this.numOplVoices - voicenum) this.replaceExistingVoiceDoom1();
        if (doubleVoice) this.voiceKeyOn(channel, instrument, 1, note, key, volume);
        this.voiceKeyOn(channel, instrument, 0, note, key, volume);
        break;
      }
      case OPL_DOOM2_1_666:
        if (this.voiceAllocedList.length === this.numOplVoices) this.replaceExistingVoiceDoom2(channel);
        if (this.voiceAllocedList.length === this.numOplVoices - 1 && doubleVoice) this.replaceExistingVoiceDoom2(channel);
        if (doubleVoice) this.voiceKeyOn(channel, instrument, 1, note, key, volume);
        this.voiceKeyOn(channel, instrument, 0, note, key, volume);
        break;
      default:
        if (this.voiceFreeList.length === 0) this.replaceExistingVoice();
        this.voiceKeyOn(channel, instrument, 0, note, key, volume);
        if (doubleVoice) this.voiceKeyOn(channel, instrument, 1, note, key, volume);
        break;
    }
  }

  programChangeEvent(event) {
    const channel = this.trackChannelForEvent(event);
    channel.instrument = this.instruments[event.param1];
  }

  setChannelVolume(channel, volume, clipStart) {
    channel.volumeBase = volume;
    if (volume > this.currentMusicVolume) volume = this.currentMusicVolume;
    if (clipStart && volume > this.startMusicVolume) volume = this.startMusicVolume;
    channel.volume = volume;
    for (let i = 0; i < this.numOplVoices; ++i) {
      if (this.voices[i].channel === channel) this.setVoiceVolume(this.voices[i], this.voices[i].noteVolume);
    }
  }

  setChannelPan(channel, pan) {
    if (this.opl3modeMusic) {
      const regPan = pan >= 96 ? 0x10 : pan <= 48 ? 0x20 : 0x30;
      if (channel.pan !== regPan) {
        channel.pan = regPan;
        for (let i = 0; i < this.numOplVoices; i++) {
          if (this.voices[i].channel === channel) this.setVoicePan(this.voices[i], regPan);
        }
      }
    }
  }

  allNotesOff(channel) {
    for (let i = 0; i < this.voiceAllocedList.length; i++) {
      if (this.voiceAllocedList[i].channel === channel) {
        this.releaseVoice(i);
        i--;
      }
    }
  }

  controllerEvent(event) {
    const channel = this.trackChannelForEvent(event);
    const param = event.param2;
    switch (event.param1) {
      case MIDI_CONTROLLER_VOLUME_MSB: this.setChannelVolume(channel, param, true); break;
      case MIDI_CONTROLLER_PAN: this.setChannelPan(channel, param); break;
      case MIDI_CONTROLLER_ALL_NOTES_OFF: this.allNotesOff(channel); break;
      default: break;
    }
  }

  pitchBendEvent(event) {
    const channel = this.trackChannelForEvent(event);
    channel.bend = event.param2 - 64;
    const updated = [];
    const notUpdated = [];
    for (const v of this.voiceAllocedList) {
      if (v.channel === channel) {
        this.updateVoiceFrequency(v);
        updated.push(v);
      } else {
        notUpdated.push(v);
      }
    }
    this.voiceAllocedList = notUpdated.concat(updated);
  }

  metaSetTempo(tempo) {
    const factor = Math.fround(Math.fround(this.usPerBeat) / Math.fround(tempo));
    this.queue.adjust(this.currentTime, factor);
    this.usPerBeat = tempo;
  }

  metaEvent(event) {
    if (event.metaType === MIDI_META_SET_TEMPO && event.data.length === 3) {
      this.metaSetTempo((event.data[0] << 16) | (event.data[1] << 8) | event.data[2]);
    }
  }

  processEvent(event) {
    switch (event.type) {
      case MIDI_EVENT_NOTE_OFF: this.keyOffEvent(event); break;
      case MIDI_EVENT_NOTE_ON: this.keyOnEvent(event); break;
      case MIDI_EVENT_CONTROLLER: this.controllerEvent(event); break;
      case MIDI_EVENT_PROGRAM_CHANGE: this.programChangeEvent(event); break;
      case MIDI_EVENT_PITCH_BEND: this.pitchBendEvent(event); break;
      case MIDI_EVENT_META: this.metaEvent(event); break;
      default: break;
    }
  }

  restartSong() {
    this.runningTracks = this.numTracks;
    this.startMusicVolume = this.currentMusicVolume;
    for (let i = 0; i < this.numTracks; ++i) {
      this.tracks[i].position = 0;
      this.scheduleTrack(this.tracks[i]);
    }
    for (let i = 0; i < MIDI_CHANNELS_PER_TRACK; ++i) this.initChannel(this.channels[i]);
  }

  trackTimerCallback(track) {
    if (track.position >= track.events.length) return;
    const event = track.events[track.position++];
    this.processEvent(event);
    if (event.type === MIDI_EVENT_META && event.metaType === MIDI_META_END_OF_TRACK) {
      --this.runningTracks;
      if (this.runningTracks <= 0 && this.songLooping) this.setCallback(5000, Music.prototype.restartSong, null);
      return;
    }
    this.scheduleTrack(track);
  }

  scheduleTrack(track) {
    const nticks = track.position < track.events.length ? track.events[track.position].deltaTime : 0;
    const us = Math.floor((nticks * this.usPerBeat) / this.ticksPerBeat);
    this.setCallback(us, Music.prototype.trackTimerCallback, track);
  }

  initChannel(channel) {
    channel.instrument = this.instruments[0];
    channel.volume = this.currentMusicVolume;
    channel.volumeBase = 100;
    if (channel.volume > channel.volumeBase) channel.volume = channel.volumeBase;
    channel.pan = 0x30;
    channel.bend = 0;
  }

  // ---- the music module (music_opl_module)

  // I_OPL_InitMusic
  init() {
    const chipType = this.oplInit();
    if (chipType === 'none') return false;
    this.opl3modeMusic = 0; // DMXOPTION -opl3 is not set
    this.numOplVoices = OPL_NUM_VOICES;
    this.initRegisters(this.opl3modeMusic);
    this.instruments = parseGenmidi(this.genmidi);
    this.initVoices();
    this.tracks = null;
    this.numTracks = 0;
    this.initialized = true;
    return true;
  }

  // I_OPL_SetMusicVolume (0..127)
  setMusicVolume(volume) {
    if (this.currentMusicVolume === volume) return;
    this.currentMusicVolume = volume;
    for (let i = 0; i < MIDI_CHANNELS_PER_TRACK; ++i) {
      if (i === 15) this.setChannelVolume(this.channels[i], volume, false);
      else this.setChannelVolume(this.channels[i], this.channels[i].volumeBase, false);
    }
  }

  // I_OPL_RegisterSong: a handle, or null if the lump can't be played
  registerSong(data) {
    if (!this.initialized) return null;
    const isMid = data.length > 4 && data[0] === 0x4d && data[1] === 0x54 && data[2] === 0x68 && data[3] === 0x64;
    const midi = isMid && data.length < MAXMIDLENGTH ? data : mus2mid(data);
    return midi ? loadMidi(midi) : null;
  }

  // I_OPL_PlaySong
  playSong(file, looping) {
    if (!this.initialized || file === null) return;
    this.numTracks = file.tracks.length;
    this.tracks = file.tracks.map((events) => ({ events, position: 0 }));
    this.runningTracks = this.numTracks;
    this.songLooping = looping;
    this.ticksPerBeat = file.division;
    this.usPerBeat = 500 * 1000;
    this.startMusicVolume = this.currentMusicVolume;
    for (let i = 0; i < this.numTracks; ++i) this.scheduleTrack(this.tracks[i]);
    for (let i = 0; i < MIDI_CHANNELS_PER_TRACK; ++i) this.initChannel(this.channels[i]);
    this.paused = 0;
  }

  pauseSong() {
    if (!this.initialized) return;
    this.paused = 1;
    for (let i = 0; i < this.numOplVoices; ++i) {
      const v = this.voices[i];
      if (v.channel !== null && v.currentInstr.index < GENMIDI_NUM_INSTRS) this.voiceKeyOff(v);
    }
  }

  resumeSong() {
    if (!this.initialized) return;
    this.paused = 0;
  }

  stopSong() {
    if (!this.initialized) return;
    this.queue.clear();
    for (let i = 0; i < MIDI_CHANNELS_PER_TRACK; ++i) this.allNotesOff(this.channels[i]);
    this.tracks = null;
    this.numTracks = 0;
  }

  isPlaying() {
    return this.initialized && this.numTracks > 0;
  }
}
