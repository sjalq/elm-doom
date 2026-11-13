// Doom's music in the browser: an OPL2 (YM3812) emulator and the DMX music driver
// that played Doom's MUS songs on it, as an AudioWorklet.
//
// The chip: written for this project after the design of Nuked-OPL3 by Nuke.YKT
// (GPL-2.0-or-later, https://github.com/nukeykt/Nuked-OPL3), cut down to the OPL2
// subset Doom uses: 9 two-operator channels, the log-sine and exponent tables, the
// envelope generator with its rate counter, the phase generator with vibrato,
// tremolo, feedback, key scaling. The chip runs at its own 49716 Hz and is
// resampled to the AudioContext's rate.
//
// The driver: DMX's OPL music player as Chocolate Doom's i_oplmusic.c (GPL-2.0-or-
// later) reconstructs it for Doom 1.9: GENMIDI instruments (two-operator patches,
// double-voice instruments, fixed notes, the base note offset and fine tuning of the
// second voice), DMX's frequency curve (32 steps a semitone, octaves looping) and
// volume curve, 9 voices handed out from a free list and taken back by DMX's
// priority rule when they run out, MUS channel 15 played through GENMIDI's
// percussion instruments.
//
// This file is part of the Doom port in Elm-OS and is under the GNU GPL, version 2
// or (at your option) any later version.

const OPL_RATE = 49716;
const MUS_RATE = 140;

// ---------------------------------------------------------------------------
// The chip

const logsinrom = new Uint16Array(256);
const exprom = new Uint16Array(256);
for (let i = 0; i < 256; i++) {
  logsinrom[i] = Math.round(-Math.log2(Math.sin(((i + 0.5) * Math.PI) / 512)) * 256);
  exprom[i] = Math.round(Math.pow(2, (255 - i) / 256) * 1024);
}
const kslrom = [0, 32, 40, 45, 48, 51, 53, 56, 56, 58, 59, 60, 61, 62, 63, 64];
const kslshift = [8, 1, 2, 0];
const mt = [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30];
const egIncStep = [
  [0, 0, 0, 0],
  [1, 0, 0, 0],
  [1, 0, 1, 0],
  [1, 1, 1, 0],
];
// register offset (low 5 bits) to operator slot, -1 where there is none
const adSlot = [0, 1, 2, 3, 4, 5, -1, -1, 6, 7, 8, 9, 10, 11, -1, -1, 12, 13, 14, 15, 16, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1];
const chSlot = [0, 1, 2, 6, 7, 8, 12, 13, 14];

const EG_ATTACK = 0, EG_DECAY = 1, EG_SUSTAIN = 2, EG_RELEASE = 3;

function calcExp(level) {
  if (level > 0x1fff) level = 0x1fff;
  return ((exprom[level & 0xff] << 1) >> (level >> 8));
}

// the four OPL2 waveforms: phase (10 bits used) and envelope (9 bits) to output
function waveform(wf, phase, env) {
  phase &= 0x3ff;
  let out;
  let neg = 0;
  switch (wf) {
    case 0:
      if (phase & 0x200) neg = 0xffff;
      out = phase & 0x100 ? logsinrom[(phase & 0xff) ^ 0xff] : logsinrom[phase & 0xff];
      break;
    case 1:
      if (phase & 0x200) out = 0x1000;
      else out = phase & 0x100 ? logsinrom[(phase & 0xff) ^ 0xff] : logsinrom[phase & 0xff];
      break;
    case 2:
      out = phase & 0x100 ? logsinrom[(phase & 0xff) ^ 0xff] : logsinrom[phase & 0xff];
      break;
    default:
      out = phase & 0x100 ? 0x1000 : logsinrom[phase & 0xff];
      break;
  }
  let v = calcExp(out + (env << 3));
  if (neg) v = ~v; // C's x ^ 0xffff on an int16: -x - 1
  return v;
}

class Slot {
  constructor(chip, channel) {
    this.chip = chip;
    this.channel = channel;
    this.out = 0;
    this.fbmod = 0;
    this.prout = 0;
    this.egRout = 0x1ff;
    this.egOut = 0x1ff;
    this.egGen = EG_RELEASE;
    this.egKsl = 0;
    this.key = 0;
    this.pgReset = 0;
    this.pgPhase = 0;
    this.pgPhaseOut = 0;
    this.am = 0; this.vib = 0; this.type = 0; this.ksr = 0; this.mult = 0;
    this.ksl = 0; this.tl = 0; this.ar = 0; this.dr = 0; this.sl = 0; this.rr = 0; this.wf = 0;
    this.modFrom = null; // the slot whose output modulates this one, or null for feedback
  }

  updateKsl() {
    const ch = this.channel;
    let ksl = (kslrom[ch.fnum >> 6] << 2) - ((0x08 - ch.block) << 5);
    this.egKsl = ksl < 0 ? 0 : ksl;
  }

  calcFb() {
    const fb = this.channel.fb;
    this.fbmod = fb !== 0 ? (this.prout + this.out) >> (9 - fb) : 0;
    this.prout = this.out;
  }

  envelope() {
    const chip = this.chip;
    let egOut = this.egRout + (this.tl << 2) + (this.egKsl >> kslshift[this.ksl]) + (this.am ? chip.tremolo : 0);
    this.egOut = egOut > 0x1ff ? 0x1ff : egOut;

    let reset = 0;
    let regRate = 0;
    if (this.key && this.egGen === EG_RELEASE) {
      reset = 1;
      regRate = this.ar;
    } else {
      switch (this.egGen) {
        case EG_ATTACK: regRate = this.ar; break;
        case EG_DECAY: regRate = this.dr; break;
        case EG_SUSTAIN: if (!this.type) regRate = this.rr; break;
        case EG_RELEASE: regRate = this.rr; break;
      }
    }
    this.pgReset = reset;
    const ks = this.channel.ksv >> ((this.ksr ^ 1) << 1);
    const nonzero = regRate !== 0;
    const rate = ks + (regRate << 2);
    let rateHi = rate >> 2;
    const rateLo = rate & 3;
    if (rateHi & 0x10) rateHi = 0x0f;
    const egShift = rateHi + chip.egAdd;
    let shift = 0;
    if (nonzero) {
      if (rateHi < 12) {
        if (chip.egState) {
          switch (egShift) {
            case 12: shift = 1; break;
            case 13: shift = (rateLo >> 1) & 1; break;
            case 14: shift = rateLo & 1; break;
          }
        }
      } else {
        shift = (rateHi & 3) + egIncStep[rateLo][chip.egTimerLo];
        if (shift & 4) shift = 3;
        if (!shift) shift = chip.egState;
      }
    }
    let egRout = this.egRout;
    let egInc = 0;
    let egOff = 0;
    if (reset && rateHi === 0x0f) egRout = 0;
    if ((this.egRout & 0x1f8) === 0x1f8) egOff = 1;
    if (this.egGen !== EG_ATTACK && !reset && egOff) egRout = 0x1ff;
    switch (this.egGen) {
      case EG_ATTACK:
        if (!this.egRout) this.egGen = EG_DECAY;
        else if (this.key && shift > 0 && rateHi !== 0x0f) egInc = (~this.egRout) >> (4 - shift);
        break;
      case EG_DECAY:
        if ((this.egRout >> 4) === this.sl) this.egGen = EG_SUSTAIN;
        else if (!egOff && !reset && shift > 0) egInc = 1 << (shift - 1);
        break;
      case EG_SUSTAIN:
      case EG_RELEASE:
        if (!egOff && !reset && shift > 0) egInc = 1 << (shift - 1);
        break;
    }
    this.egRout = (egRout + egInc) & 0x1ff;
    if (reset) this.egGen = EG_ATTACK;
    if (!this.key) this.egGen = EG_RELEASE;
  }

  phase() {
    const chip = this.chip;
    let fnum = this.channel.fnum;
    if (this.vib) {
      let range = (fnum >> 7) & 7;
      const vibpos = chip.vibpos;
      if (!(vibpos & 3)) range = 0;
      else if (vibpos & 1) range >>= 1;
      range >>= chip.vibshift;
      if (vibpos & 4) range = -range;
      fnum += range;
    }
    const basefreq = (fnum << this.channel.block) >> 1;
    const phase = (this.pgPhase >>> 9) & 0xffff;
    if (this.pgReset) this.pgPhase = 0;
    this.pgPhase = (this.pgPhase + ((basefreq * mt[this.mult]) >> 1)) >>> 0;
    this.pgPhaseOut = phase;
  }

  generate() {
    // the first operator is modulated by its own feedback, the second by the first
    // (FM) or by nothing (AM)
    const mod = this.second ? (this.modFrom ? this.modFrom.out : 0) : this.fbmod;
    const wf = this.chip.waveSelect ? this.wf : 0;
    this.out = waveform(wf, this.pgPhaseOut + mod, this.egOut);
  }
}

class Channel {
  constructor() {
    this.fnum = 0;
    this.block = 0;
    this.ksv = 0;
    this.fb = 0;
    this.con = 0;
    this.slots = [];
  }
}

class Opl2 {
  constructor() {
    this.channels = [];
    this.slots = new Array(18);
    for (let c = 0; c < 9; c++) {
      const ch = new Channel();
      const a = new Slot(this, ch);
      const b = new Slot(this, ch);
      ch.slots = [a, b];
      this.slots[chSlot[c]] = a;
      this.slots[chSlot[c] + 3] = b;
      this.channels.push(ch);
      this.setupAlg(ch);
    }
    this.timer = 0;
    this.egTimer = 0;
    this.egTimerRem = 0;
    this.egState = 0;
    this.egAdd = 0;
    this.egTimerLo = 0;
    this.tremoloPos = 0;
    this.tremolo = 0;
    this.tremoloShift = 4;
    this.vibpos = 0;
    this.vibshift = 1;
    this.nts = 0;
    this.waveSelect = 0;
  }

  setupAlg(ch) {
    // FM: the first operator modulates the second, which is heard; AM: both heard
    ch.slots[0].modFrom = null;
    ch.slots[1].modFrom = ch.con ? null : ch.slots[0];
    ch.slots[1].second = true;
  }

  updateKsv(ch) {
    ch.ksv = (ch.block << 1) | ((ch.fnum >> (9 - this.nts)) & 1);
    ch.slots[0].updateKsl();
    ch.slots[1].updateKsl();
  }

  write(reg, data) {
    reg &= 0xff;
    data &= 0xff;
    const high = reg & 0xf0;
    const slotNum = adSlot[reg & 0x1f];
    switch (high) {
      case 0x00:
        if (reg === 0x01) this.waveSelect = (data >> 5) & 1;
        else if (reg === 0x08) this.nts = (data >> 6) & 1;
        break;
      case 0x20:
      case 0x30:
        if (slotNum >= 0) {
          const s = this.slots[slotNum];
          s.am = (data >> 7) & 1;
          s.vib = (data >> 6) & 1;
          s.type = (data >> 5) & 1;
          s.ksr = (data >> 4) & 1;
          s.mult = data & 0x0f;
        }
        break;
      case 0x40:
      case 0x50:
        if (slotNum >= 0) {
          const s = this.slots[slotNum];
          s.ksl = (data >> 6) & 3;
          s.tl = data & 0x3f;
          s.updateKsl();
        }
        break;
      case 0x60:
      case 0x70:
        if (slotNum >= 0) {
          const s = this.slots[slotNum];
          s.ar = (data >> 4) & 0x0f;
          s.dr = data & 0x0f;
        }
        break;
      case 0x80:
      case 0x90:
        if (slotNum >= 0) {
          const s = this.slots[slotNum];
          s.sl = (data >> 4) & 0x0f;
          if (s.sl === 0x0f) s.sl = 0x1f;
          s.rr = data & 0x0f;
        }
        break;
      case 0xe0:
      case 0xf0:
        if (slotNum >= 0) this.slots[slotNum].wf = data & 3;
        break;
      case 0xa0:
        if ((reg & 0x0f) < 9) {
          const ch = this.channels[reg & 0x0f];
          ch.fnum = (ch.fnum & 0x300) | data;
          this.updateKsv(ch);
        }
        break;
      case 0xb0:
        if (reg === 0xbd) {
          this.tremoloShift = (data >> 7) & 1 ? 2 : 4;
          this.vibshift = (data >> 6) & 1 ? 0 : 1;
        } else if ((reg & 0x0f) < 9) {
          const ch = this.channels[reg & 0x0f];
          ch.fnum = (ch.fnum & 0xff) | ((data & 3) << 8);
          ch.block = (data >> 2) & 7;
          this.updateKsv(ch);
          const on = (data >> 5) & 1;
          ch.slots[0].key = on;
          ch.slots[1].key = on;
        }
        break;
      case 0xc0:
        if ((reg & 0x0f) < 9) {
          const ch = this.channels[reg & 0x0f];
          ch.fb = (data >> 1) & 7;
          ch.con = data & 1;
          this.setupAlg(ch);
        }
        break;
    }
  }

  // one sample at 49716 Hz, as a signed 16-bit-range integer
  generate() {
    const slots = this.slots;
    for (let i = 0; i < 18; i++) {
      const s = slots[i];
      if (!s.second) s.calcFb();
      s.envelope();
      s.phase();
      s.generate();
    }
    let mix = 0;
    const chs = this.channels;
    for (let c = 0; c < 9; c++) {
      const ch = chs[c];
      mix += ch.con ? ch.slots[0].out + ch.slots[1].out : ch.slots[1].out;
    }
    if (mix > 32767) mix = 32767;
    else if (mix < -32768) mix = -32768;

    if ((this.timer & 0x3f) === 0x3f) this.tremoloPos = (this.tremoloPos + 1) % 210;
    this.tremolo = this.tremoloPos < 105 ? this.tremoloPos >> this.tremoloShift : (210 - this.tremoloPos) >> this.tremoloShift;
    if ((this.timer & 0x3ff) === 0x3ff) this.vibpos = (this.vibpos + 1) & 7;
    this.timer = (this.timer + 1) & 0xffff;
    if (this.egState) {
      const low = this.egTimer % 8192;
      let shift = 0;
      while (shift < 13 && ((low >> shift) & 1) === 0) shift++;
      this.egAdd = shift > 12 ? 0 : shift + 1;
      this.egTimerLo = this.egTimer % 4;
    }
    if (this.egTimerRem || this.egState) {
      if (this.egTimer >= 0xfffffffff) {
        this.egTimer = 0;
        this.egTimerRem = 1;
      } else {
        this.egTimer++;
        this.egTimerRem = 0;
      }
    }
    this.egState ^= 1;
    return mix;
  }
}

// ---------------------------------------------------------------------------
// GENMIDI: "#OPL_II#", then 175 instruments of 36 bytes (128 melodic, then the
// percussion for notes 35 to 81)

function parseGenmidi(buffer) {
  const bytes = new Uint8Array(buffer);
  const view = new DataView(buffer);
  const op = (at) => ({
    tremolo: bytes[at], attack: bytes[at + 1], sustain: bytes[at + 2],
    waveform: bytes[at + 3], scale: bytes[at + 4], level: bytes[at + 5],
  });
  const instruments = [];
  for (let i = 0; i < 175; i++) {
    const at = 8 + i * 36;
    const voice = (v) => {
      const b = at + 4 + v * 16;
      return {
        modulator: op(b),
        feedback: bytes[b + 6],
        carrier: op(b + 7),
        baseNoteOffset: view.getInt16(b + 14, true),
      };
    };
    instruments.push({
      flags: view.getUint16(at, true),
      fineTuning: bytes[at + 2],
      fixedNote: bytes[at + 3],
      voices: [voice(0), voice(1)],
    });
  }
  return instruments;
}

const FLAG_FIXED = 0x01;
const FLAG_2VOICE = 0x04;

// ---------------------------------------------------------------------------
// The DMX driver

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

// DMX's frequency curve: an OPL F-number (block 0) for each 1/32 of a semitone,
// from two semitones below MIDI note 0. Index 284 onwards is one octave that the
// driver loops through, adding the octave as the block.
const frequencyCurve = new Uint16Array(668);
for (let i = 0; i < 668; i++) {
  const note = (i - 64) / 32;
  const hz = 440 * Math.pow(2, (note - 69) / 12) * 2;
  frequencyCurve[i] = Math.round((hz * 1048576) / OPL_RATE);
}

const voiceOp1 = [0x00, 0x01, 0x02, 0x08, 0x09, 0x0a, 0x10, 0x11, 0x12];
const voiceOp2 = [0x03, 0x04, 0x05, 0x0b, 0x0c, 0x0d, 0x13, 0x14, 0x15];

class MusPlayer {
  constructor(opl) {
    this.opl = opl;
    this.instruments = null;
    this.musicVolume = 64;
    this.voices = [];
    for (let i = 0; i < 9; i++) {
      this.voices.push({
        index: i, op1: voiceOp1[i], op2: voiceOp2[i],
        channel: -1, key: 0, note: 0, noteVolume: 0, freq: 0,
        instr: null, instrVoice: 0, carVolume: 0, modVolume: 0, priority: 0,
      });
    }
    this.events = null;
    this.playing = false;
    this.paused = false;
    this.looping = false;
    this.initRegisters();
    this.resetSong();
  }

  initRegisters() {
    const w = (r, v) => this.opl.write(r, v);
    for (let r = 0x40; r <= 0x40 + 0x15; r++) w(r, 0x3f);
    for (let r = 0x60; r <= 0xf5; r++) w(r, 0x00);
    for (let r = 0x01; r < 0x40; r++) w(r, 0x00);
    w(0x04, 0x60);
    w(0x04, 0x80);
    w(0x01, 0x20);
    w(0x08, 0x40);
  }

  resetSong() {
    this.channels = [];
    for (let c = 0; c < 16; c++) {
      this.channels.push({
        instrument: 0,
        volumeBase: 100,
        volume: Math.min(100, this.musicVolume),
        bend: 0,
        lastVolume: 127,
      });
    }
    this.pos = 0;
    this.wait = 0;
    this.freeList = this.voices.slice();
    this.allocated = [];
    for (const v of this.voices) {
      this.opl.write(0xb0 + v.index, v.freq >> 8);
      v.channel = -1;
      v.instr = null;
    }
  }

  setGenmidi(buffer) {
    this.instruments = parseGenmidi(buffer);
  }

  play(events, looping) {
    this.stop();
    this.events = events;
    this.looping = looping;
    this.resetSong();
    this.playing = true;
  }

  stop() {
    this.playing = false;
    this.events = null;
    for (let i = this.allocated.length - 1; i >= 0; i--) this.releaseVoice(i);
  }

  setVolume(v) {
    this.musicVolume = v;
    for (let c = 0; c < 16; c++) this.setChannelVolume(c, this.channels[c].volumeBase);
  }

  // -- voices

  keyOff(voice) {
    this.opl.write(0xb0 + voice.index, voice.freq >> 8);
  }

  releaseVoice(i) {
    const voice = this.allocated[i];
    this.keyOff(voice);
    voice.channel = -1;
    voice.note = 0;
    this.allocated.splice(i, 1);
    this.freeList.push(voice);
  }

  loadOperator(op, data, maxLevel) {
    let level = data.scale;
    level |= maxLevel ? 0x3f : data.level;
    this.opl.write(0x40 + op, level);
    this.opl.write(0x20 + op, data.tremolo);
    this.opl.write(0x60 + op, data.attack);
    this.opl.write(0x80 + op, data.sustain);
    this.opl.write(0xe0 + op, data.waveform);
    return level;
  }

  setVoiceInstrument(voice, instr, instrVoice) {
    if (voice.instr === instr && voice.instrVoice === instrVoice) return;
    voice.instr = instr;
    voice.instrVoice = instrVoice;
    const data = instr.voices[instrVoice];
    const modulating = (data.feedback & 1) === 0;
    voice.carVolume = this.loadOperator(voice.op2, data.carrier, true);
    voice.modVolume = this.loadOperator(voice.op1, data.modulator, !modulating);
    this.opl.write(0xc0 + voice.index, data.feedback | 0x30);
    voice.priority = 0x0f - (data.carrier.attack >> 4) + 0x0f - (data.carrier.sustain & 0x0f);
  }

  setVoiceVolume(voice, volume) {
    voice.noteVolume = volume;
    const data = voice.instr.voices[voice.instrVoice];
    const midiVolume = 2 * (volumeMappingTable[this.channels[voice.channel].volume] + 1);
    const fullVolume = (volumeMappingTable[voice.noteVolume] * midiVolume) >> 9;
    const carVolume = 0x3f - fullVolume;
    if (carVolume !== (voice.carVolume & 0x3f)) {
      voice.carVolume = carVolume | (voice.carVolume & 0xc0);
      this.opl.write(0x40 + voice.op2, voice.carVolume);
      if ((data.feedback & 1) !== 0 && data.modulator.level !== 0x3f) {
        let modVolume = data.modulator.level;
        if (modVolume < carVolume) modVolume = carVolume;
        modVolume |= voice.modVolume & 0xc0;
        if (modVolume !== voice.modVolume) {
          voice.modVolume = modVolume;
          this.opl.write(0x40 + voice.op1, modVolume | (data.modulator.scale & 0xc0));
        }
      }
    }
  }

  frequencyFor(voice) {
    const instr = voice.instr;
    const gm = instr.voices[voice.instrVoice];
    let note = voice.note;
    if ((instr.flags & FLAG_FIXED) === 0) note += gm.baseNoteOffset;
    while (note < 0) note += 12;
    while (note > 95) note -= 12;
    let index = 64 + 32 * note + this.channels[voice.channel].bend;
    if (voice.instrVoice !== 0) index += (instr.fineTuning >> 1) - 64;
    if (index < 0) index = 0;
    if (index < 284) return frequencyCurve[index];
    const sub = (index - 284) % (12 * 32);
    let octave = Math.floor((index - 284) / (12 * 32));
    if (octave >= 7) octave = 7;
    return frequencyCurve[sub + 284] | (octave << 10);
  }

  updateFrequency(voice) {
    const freq = this.frequencyFor(voice);
    if (voice.freq !== freq) {
      this.opl.write(0xa0 + voice.index, freq & 0xff);
      this.opl.write(0xb0 + voice.index, (freq >> 8) | 0x20);
      voice.freq = freq;
    }
  }

  voiceKeyOn(channel, instr, instrVoice, note, key, volume) {
    const voice = this.freeList.shift();
    if (!voice) return;
    this.allocated.push(voice);
    voice.channel = channel;
    voice.key = key;
    voice.note = (instr.flags & FLAG_FIXED) !== 0 ? instr.fixedNote : note;
    this.setVoiceInstrument(voice, instr, instrVoice);
    this.setVoiceVolume(voice, volume);
    voice.freq = 0;
    this.updateFrequency(voice);
  }

  replaceExistingVoice(channel) {
    let result = 0;
    let priority = 0x8000;
    for (let i = 0; i < this.allocated.length - 3; i++) {
      const v = this.allocated[i];
      if (v.priority < priority && v.channel >= channel) {
        priority = v.priority;
        result = i;
      }
    }
    if (this.allocated.length > 0) this.releaseVoice(result);
  }

  // -- events

  noteOn(c, key, volume) {
    if (!this.instruments) return;
    if (volume <= 0) {
      this.noteOff(c, key);
      return;
    }
    let instr;
    let note = key;
    if (c === 15) {
      if (key < 35 || key > 81) return;
      instr = this.instruments[128 + key - 35];
      note = 60;
    } else {
      instr = this.instruments[this.channels[c].instrument];
    }
    const double = (instr.flags & FLAG_2VOICE) !== 0;
    if (this.freeList.length === 0) this.replaceExistingVoice(c);
    this.voiceKeyOn(c, instr, 0, note, key, volume);
    if (double) this.voiceKeyOn(c, instr, 1, note, key, volume);
  }

  noteOff(c, key) {
    for (let i = 0; i < this.allocated.length; i++) {
      const v = this.allocated[i];
      if (v.channel === c && v.key === key) {
        this.releaseVoice(i);
        i--;
      }
    }
  }

  allNotesOff(c) {
    for (let i = 0; i < this.allocated.length; i++) {
      if (this.allocated[i].channel === c) {
        this.releaseVoice(i);
        i--;
      }
    }
  }

  setChannelVolume(c, volume) {
    const ch = this.channels[c];
    ch.volumeBase = volume;
    ch.volume = Math.min(volume, this.musicVolume);
    for (const v of this.allocated) if (v.channel === c) this.setVoiceVolume(v, v.noteVolume);
  }

  pitchBend(c, bend) {
    this.channels[c].bend = (bend >> 1) - 64;
    for (const v of this.allocated) if (v.channel === c) this.updateFrequency(v);
  }

  controller(c, number, value) {
    switch (number) {
      case 0: this.channels[c].instrument = value & 0x7f; break;
      case 3: this.setChannelVolume(c, value & 0x7f); break;
      default: break; // pan needs OPL3; modulation, expression and the rest DMX ignores
    }
  }

  system(c, number) {
    switch (number) {
      case 10:
      case 11:
        this.allNotesOff(c);
        break;
      case 14:
        this.channels[c].bend = 0;
        this.setChannelVolume(c, 100);
        break;
    }
  }

  // run the events due now; returns false when the song is over
  tick() {
    if (!this.playing || this.paused) return;
    if (this.wait > 0) {
      this.wait--;
      if (this.wait > 0) return;
    }
    const ev = this.events;
    while (this.playing) {
      if (this.pos + 5 > ev.length) {
        this.endOfScore();
        return;
      }
      const type = ev[this.pos], c = ev[this.pos + 1], a = ev[this.pos + 2], b = ev[this.pos + 3], delay = ev[this.pos + 4];
      this.pos += 5;
      switch (type) {
        case 0: this.noteOff(c, a); break;
        case 1: {
          const ch = this.channels[c];
          if (b >= 0) ch.lastVolume = b;
          this.noteOn(c, a, ch.lastVolume);
          break;
        }
        case 2: this.pitchBend(c, a); break;
        case 3: this.system(c, a); break;
        case 4: this.controller(c, a, b); break;
        case 6: this.endOfScore(); return;
        default: break;
      }
      if (delay > 0) {
        this.wait = delay;
        return;
      }
    }
  }

  endOfScore() {
    if (this.looping) {
      const events = this.events;
      for (let i = this.allocated.length - 1; i >= 0; i--) this.releaseVoice(i);
      this.pos = 0;
      this.wait = 0;
      this.events = events;
    } else {
      this.stop();
    }
  }
}

// ---------------------------------------------------------------------------
// The worklet

class OplProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.opl = new Opl2();
    this.player = new MusPlayer(this.opl);
    this.step = OPL_RATE / sampleRate; // chip samples per output sample
    this.frac = 0;
    this.prev = 0;
    this.cur = 0;
    this.untilTick = 0; // chip samples until the next MUS tick
    this.tickLength = OPL_RATE / MUS_RATE;
    this.gain = 1 / 16384;
    this.peak = 0;
    this.port.onmessage = (e) => this.message(e.data);
  }

  message(m) {
    switch (m.type) {
      case 'genmidi': this.player.setGenmidi(m.buffer); break;
      case 'play': this.player.play(Int16Array.from(m.events), m.looping); this.untilTick = 0; break;
      case 'stop': this.player.stop(); break;
      case 'pause':
        this.player.paused = true;
        for (const v of this.player.allocated) this.player.keyOff(v);
        break;
      case 'resume': this.player.paused = false; break;
      case 'volume': this.player.setVolume(m.value); break;
      case 'report':
        // for tests: what the player is doing
        this.port.postMessage({
          playing: this.player.playing, pos: this.player.pos, events: this.player.events ? this.player.events.length : 0,
          voices: this.player.allocated.length, volume: this.player.musicVolume, instruments: !!this.player.instruments,
          peak: this.peak,
        });
        this.peak = 0;
        break;
    }
  }

  chipSample() {
    if (this.untilTick <= 0) {
      this.player.tick();
      this.untilTick += this.tickLength;
    }
    this.untilTick -= 1;
    return this.opl.generate();
  }

  process(inputs, outputs) {
    const out = outputs[0];
    const left = out[0];
    const right = out[1] || out[0];
    for (let i = 0; i < left.length; i++) {
      this.frac += this.step;
      while (this.frac >= 1) {
        this.prev = this.cur;
        this.cur = this.chipSample();
        this.frac -= 1;
      }
      const s = (this.prev + (this.cur - this.prev) * this.frac) * this.gain;
      left[i] = s;
      if (Math.abs(s) > this.peak) this.peak = Math.abs(s);
      if (right !== left) right[i] = s;
    }
    return true;
  }
}

registerProcessor('doom-opl', OplProcessor);
