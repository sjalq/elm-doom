// Nuked OPL3 in JavaScript: a sample-exact port of the emulator Chocolate Doom
// ships (opl/opl3.c: Nuked-OPL3 1.8, cfedb09, by Nuke.YKT, in Tony Gies's
// bit-exact "fast" fork), with the same integer arithmetic: every value is kept in
// the width the C source gives it (uint16 phases, int16 outputs, uint32 phase
// accumulators). The fork's caches and fast paths (which it proves give the same
// output) are left out: every slot runs the plain upstream path each sample.
//
// Nuked OPL3 is free software under the GNU Lesser General Public License 2.1 or
// later; this port is part of the Doom port in Elm-OS, GPL-2.0-or-later (which the
// LGPL allows).

// Nuked OPL3's ROM tables (exprom is stored shifted left by one, as in the fork)
const logsinrom = new Uint16Array([
  0x859, 0x6c3, 0x607, 0x58b, 0x52e, 0x4e4, 0x4a6, 0x471, 0x443, 0x41a, 0x3f5, 0x3d3, 0x3b5, 0x398, 0x37e, 0x365,
  0x34e, 0x339, 0x324, 0x311, 0x2ff, 0x2ed, 0x2dc, 0x2cd, 0x2bd, 0x2af, 0x2a0, 0x293, 0x286, 0x279, 0x26d, 0x261,
  0x256, 0x24b, 0x240, 0x236, 0x22c, 0x222, 0x218, 0x20f, 0x206, 0x1fd, 0x1f5, 0x1ec, 0x1e4, 0x1dc, 0x1d4, 0x1cd,
  0x1c5, 0x1be, 0x1b7, 0x1b0, 0x1a9, 0x1a2, 0x19b, 0x195, 0x18f, 0x188, 0x182, 0x17c, 0x177, 0x171, 0x16b, 0x166,
  0x160, 0x15b, 0x155, 0x150, 0x14b, 0x146, 0x141, 0x13c, 0x137, 0x133, 0x12e, 0x129, 0x125, 0x121, 0x11c, 0x118,
  0x114, 0x10f, 0x10b, 0x107, 0x103, 0x0ff, 0x0fb, 0x0f8, 0x0f4, 0x0f0, 0x0ec, 0x0e9, 0x0e5, 0x0e2, 0x0de, 0x0db,
  0x0d7, 0x0d4, 0x0d1, 0x0cd, 0x0ca, 0x0c7, 0x0c4, 0x0c1, 0x0be, 0x0bb, 0x0b8, 0x0b5, 0x0b2, 0x0af, 0x0ac, 0x0a9,
  0x0a7, 0x0a4, 0x0a1, 0x09f, 0x09c, 0x099, 0x097, 0x094, 0x092, 0x08f, 0x08d, 0x08a, 0x088, 0x086, 0x083, 0x081,
  0x07f, 0x07d, 0x07a, 0x078, 0x076, 0x074, 0x072, 0x070, 0x06e, 0x06c, 0x06a, 0x068, 0x066, 0x064, 0x062, 0x060,
  0x05e, 0x05c, 0x05b, 0x059, 0x057, 0x055, 0x053, 0x052, 0x050, 0x04e, 0x04d, 0x04b, 0x04a, 0x048, 0x046, 0x045,
  0x043, 0x042, 0x040, 0x03f, 0x03e, 0x03c, 0x03b, 0x039, 0x038, 0x037, 0x035, 0x034, 0x033, 0x031, 0x030, 0x02f,
  0x02e, 0x02d, 0x02b, 0x02a, 0x029, 0x028, 0x027, 0x026, 0x025, 0x024, 0x023, 0x022, 0x021, 0x020, 0x01f, 0x01e,
  0x01d, 0x01c, 0x01b, 0x01a, 0x019, 0x018, 0x017, 0x017, 0x016, 0x015, 0x014, 0x014, 0x013, 0x012, 0x011, 0x011,
  0x010, 0x00f, 0x00f, 0x00e, 0x00d, 0x00d, 0x00c, 0x00c, 0x00b, 0x00a, 0x00a, 0x009, 0x009, 0x008, 0x008, 0x007,
  0x007, 0x007, 0x006, 0x006, 0x005, 0x005, 0x005, 0x004, 0x004, 0x004, 0x003, 0x003, 0x003, 0x002, 0x002, 0x002,
  0x002, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
]);
const exprom = new Uint16Array([
  0xff4, 0xfea, 0xfde, 0xfd4, 0xfc8, 0xfbe, 0xfb4, 0xfa8, 0xf9e, 0xf92, 0xf88, 0xf7e, 0xf72, 0xf68, 0xf5c, 0xf52,
  0xf48, 0xf3e, 0xf32, 0xf28, 0xf1e, 0xf14, 0xf08, 0xefe, 0xef4, 0xeea, 0xee0, 0xed4, 0xeca, 0xec0, 0xeb6, 0xeac,
  0xea2, 0xe98, 0xe8e, 0xe84, 0xe7a, 0xe70, 0xe66, 0xe5c, 0xe52, 0xe48, 0xe3e, 0xe34, 0xe2a, 0xe20, 0xe16, 0xe0c,
  0xe04, 0xdfa, 0xdf0, 0xde6, 0xddc, 0xdd2, 0xdca, 0xdc0, 0xdb6, 0xdac, 0xda4, 0xd9a, 0xd90, 0xd88, 0xd7e, 0xd74,
  0xd6a, 0xd62, 0xd58, 0xd50, 0xd46, 0xd3c, 0xd34, 0xd2a, 0xd22, 0xd18, 0xd10, 0xd06, 0xcfe, 0xcf4, 0xcec, 0xce2,
  0xcda, 0xcd0, 0xcc8, 0xcbe, 0xcb6, 0xcae, 0xca4, 0xc9c, 0xc92, 0xc8a, 0xc82, 0xc78, 0xc70, 0xc68, 0xc60, 0xc56,
  0xc4e, 0xc46, 0xc3c, 0xc34, 0xc2c, 0xc24, 0xc1c, 0xc12, 0xc0a, 0xc02, 0xbfa, 0xbf2, 0xbea, 0xbe0, 0xbd8, 0xbd0,
  0xbc8, 0xbc0, 0xbb8, 0xbb0, 0xba8, 0xba0, 0xb98, 0xb90, 0xb88, 0xb80, 0xb78, 0xb70, 0xb68, 0xb60, 0xb58, 0xb50,
  0xb48, 0xb40, 0xb38, 0xb32, 0xb2a, 0xb22, 0xb1a, 0xb12, 0xb0a, 0xb02, 0xafc, 0xaf4, 0xaec, 0xae4, 0xade, 0xad6,
  0xace, 0xac6, 0xac0, 0xab8, 0xab0, 0xaa8, 0xaa2, 0xa9a, 0xa92, 0xa8c, 0xa84, 0xa7c, 0xa76, 0xa6e, 0xa68, 0xa60,
  0xa58, 0xa52, 0xa4a, 0xa44, 0xa3c, 0xa36, 0xa2e, 0xa28, 0xa20, 0xa18, 0xa12, 0xa0c, 0xa04, 0x9fe, 0x9f6, 0x9f0,
  0x9e8, 0x9e2, 0x9da, 0x9d4, 0x9ce, 0x9c6, 0x9c0, 0x9b8, 0x9b2, 0x9ac, 0x9a4, 0x99e, 0x998, 0x990, 0x98a, 0x984,
  0x97c, 0x976, 0x970, 0x96a, 0x962, 0x95c, 0x956, 0x950, 0x948, 0x942, 0x93c, 0x936, 0x930, 0x928, 0x922, 0x91c,
  0x916, 0x910, 0x90a, 0x904, 0x8fc, 0x8f6, 0x8f0, 0x8ea, 0x8e4, 0x8de, 0x8d8, 0x8d2, 0x8cc, 0x8c6, 0x8c0, 0x8ba,
  0x8b4, 0x8ae, 0x8a8, 0x8a2, 0x89c, 0x896, 0x890, 0x88a, 0x884, 0x87e, 0x878, 0x872, 0x86c, 0x866, 0x860, 0x85a,
  0x854, 0x850, 0x84a, 0x844, 0x83e, 0x838, 0x832, 0x82c, 0x828, 0x822, 0x81c, 0x816, 0x810, 0x80c, 0x806, 0x800,
]);

const RSM_FRAC = 10;
const WRITEBUF_SIZE = 1024;
const WRITEBUF_DELAY = 2;

const CH_2OP = 0;
const CH_4OP = 1;
const CH_4OP2 = 2;
const CH_DRUM = 3;

const EGK_NORM = 0x01;
const EGK_DRUM = 0x02;

const EG_ATTACK = 0;
const EG_DECAY = 1;
const EG_SUSTAIN = 2;
const EG_RELEASE = 3;

const mt = [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30];
const kslrom = [0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64];
const kslshift = [8, 1, 2, 0];
const egIncStep = [
  [0, 0, 0, 0],
  [1, 0, 0, 0],
  [1, 0, 1, 0],
  [1, 1, 1, 0],
];
const adSlot = [
  0, 1, 2, 3, 4, 5, -1, -1, 6, 7, 8, 9, 10, 11, -1, -1,
  12, 13, 14, 15, 16, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
];
const chSlot = [0, 1, 2, 6, 7, 8, 12, 13, 14, 18, 19, 20, 24, 25, 26, 30, 31, 32];

// OPL3_BuildWfTable: the eight waveforms as one log-sine table, the sign in bit 15
const logsinWf = [];
for (let w = 0; w < 8; w++) logsinWf.push(new Uint16Array(1024));
for (let p = 0; p < 1024; p++) {
  const c2 = p & 0x100 ? logsinrom[(p & 0xff) ^ 0xff] : logsinrom[p & 0xff];
  const c6 = p & 0x200 ? 0x8000 : 0;
  const c5 = c6 ? 0x1000 : p & 0x80 ? logsinrom[((p ^ 0xff) << 1) & 0xff] : logsinrom[(p << 1) & 0xff];
  logsinWf[0][p] = c6 | c2;
  logsinWf[1][p] = c6 ? 0x1000 : c2;
  logsinWf[2][p] = c2;
  logsinWf[3][p] = p & 0x100 ? 0x1000 : logsinrom[p & 0xff];
  logsinWf[4][p] = ((p & 0x300) === 0x100 ? 0x8000 : 0) | c5;
  logsinWf[5][p] = c5;
  logsinWf[6][p] = c6;
  logsinWf[7][p] = (c6 | ((c6 ? (p & 0x1ff) ^ 0x1ff : p) << 3)) & 0xffff;
}

const int16 = (x) => (x << 16) >> 16;

// a slot's modulation input: nothing, its own feedback, or another slot's output
const MOD_ZERO = 0;
const MOD_FB = 1;
const MOD_OUT = 2;

class Slot {
  constructor(chip, num) {
    this.chip = chip;
    this.slotNum = num;
    this.channel = null;
    this.modKind = MOD_ZERO;
    this.modSlot = null;
    this.am = 0; // trem points at the chip's tremolo (1) or zeromod (0)
    this.pgReset = 0;
    this.pgPhase = 0;
    this.pgInc = 0;
    this.out = 0;
    this.fbmod = 0;
    this.prout = 0;
    this.egRout = 0x1ff;
    this.egOut = 0x1ff;
    this.egTlKsl = 0;
    this.pgPhaseOut = 0;
    this.key = 0;
    this.egGen = EG_RELEASE;
    this.regVib = 0;
    this.regMult = 0;
    this.regWf = 0;
    this.egKsl = 0;
    this.egKs = 0;
    this.regType = 0;
    this.regKsr = 0;
    this.regKsl = 0;
    this.regTl = 0;
    this.regAr = 0;
    this.regDr = 0;
    this.regSl = 0;
    this.regRr = 0;
    this.egRates = [0, 0, 0, 0];
    this.egRateHi = [0, 0, 0, 0];
    this.egRateLo = [0, 0, 0, 0];
    this.pgIncVib = new Uint32Array(8);
  }

  mod() {
    switch (this.modKind) {
      case MOD_FB: return this.fbmod;
      case MOD_OUT: return this.modSlot.out;
      default: return 0;
    }
  }

  // ---- envelope generator

  updateKsl() {
    let ksl = (kslrom[this.channel.fNum >> 6] << 2) - ((0x08 - this.channel.block) << 5);
    ksl = int16(ksl);
    if (ksl < 0) ksl = 0;
    this.egKsl = ksl & 0xff;
    this.egTlKsl = ((this.regTl << 2) + (this.egKsl >> kslshift[this.regKsl])) & 0xffff;
  }

  updateRate() {
    this.egKs = (this.channel.ksv >> ((this.regKsr ^ 1) << 1)) & 0xff;
    for (let ii = 0; ii < 4; ii++) {
      const rate = (this.egKs + (this.egRates[ii] << 2)) & 0xff;
      let rateHi = rate >> 2;
      if (rateHi & 0x10) rateHi = 0x0f;
      this.egRateHi[ii] = rateHi;
      this.egRateLo[ii] = rate & 0x03;
    }
  }

  envelopeCalc() {
    const chip = this.chip;
    let regRate;
    let reset = 0;
    this.egOut = (this.egRout + this.egTlKsl + (this.am ? chip.tremolo : 0)) & 0xffff;
    if (this.key && this.egGen === EG_RELEASE) {
      reset = 1;
      regRate = this.egRates[0];
    } else {
      regRate = this.egRates[this.egGen];
    }
    this.pgReset = reset;
    const nonzero = regRate !== 0;
    let rateHi;
    let rateLo;
    if (reset) {
      rateHi = this.egRateHi[0];
      rateLo = this.egRateLo[0];
    } else {
      rateHi = this.egRateHi[this.egGen];
      rateLo = this.egRateLo[this.egGen];
    }
    const egShift = (rateHi + chip.egAdd) & 0xff;
    let shift = 0;
    if (nonzero) {
      if (rateHi < 12) {
        if (chip.egState) {
          switch (egShift) {
            case 12: shift = 1; break;
            case 13: shift = (rateLo >> 1) & 0x01; break;
            case 14: shift = rateLo & 0x01; break;
            default: break;
          }
        }
      } else {
        shift = ((rateHi & 0x03) + egIncStep[rateLo][chip.egTimerLo]) & 0xff;
        if (shift & 0x04) shift = 0x03;
        if (!shift) shift = chip.egState;
      }
    }
    let egRout = this.egRout;
    let egInc = 0;
    let egOff = 0;
    if (reset && rateHi === 0x0f) egRout = 0x00;
    if ((this.egRout & 0x1f8) === 0x1f8) egOff = 1;
    if (this.egGen !== EG_ATTACK && !reset && egOff) egRout = 0x1ff;
    switch (this.egGen) {
      case EG_ATTACK:
        if (!this.egRout) {
          this.egGen = EG_DECAY;
        } else if (this.key && shift > 0 && rateHi !== 0x0f) {
          egInc = int16(~this.egRout >> (4 - shift));
        }
        break;
      case EG_DECAY:
        if ((this.egRout >> 4) === this.regSl) {
          this.egGen = EG_SUSTAIN;
        } else if (!egOff && !reset && shift > 0) {
          egInc = 1 << (shift - 1);
        }
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

  // ---- phase generator

  updateInc() {
    const ch = this.channel;
    const basefreq = ((ch.fNum << ch.block) >>> 0) >>> 1;
    this.pgInc = ((basefreq * mt[this.regMult]) >>> 1) >>> 0;
    for (let vibpos = 0; vibpos < 8; vibpos++) {
      let fNum = ch.fNum;
      let range = (fNum >> 7) & 7;
      if (!(vibpos & 3)) range = 0;
      else if (vibpos & 1) range >>= 1;
      range >>= this.chip.vibshift;
      if (vibpos & 4) range = -range;
      fNum = (fNum + range) & 0xffff;
      this.pgIncVib[vibpos] = ((((fNum << ch.block) >>> 0) >>> 1) * mt[this.regMult]) >>> 1;
    }
  }

  phaseGenerate() {
    const chip = this.chip;
    const phaseinc = this.regVib ? this.pgIncVib[chip.vibpos] : this.pgInc;
    const phase = (this.pgPhase >>> 9) & 0xffff;
    if (this.pgReset) this.pgPhase = 0;
    this.pgPhase = (this.pgPhase + phaseinc) >>> 0;
    this.pgPhaseOut = phase;
    switch (this.slotNum) {
      case 13: // hh
        chip.rmHhBit2 = (phase >> 2) & 1;
        chip.rmHhBit3 = (phase >> 3) & 1;
        chip.rmHhBit7 = (phase >> 7) & 1;
        chip.rmHhBit8 = (phase >> 8) & 1;
        if (chip.rhy & 0x20) {
          const rmXor = (chip.rmHhBit2 ^ chip.rmHhBit7) | (chip.rmHhBit3 ^ chip.rmTcBit5) | (chip.rmTcBit3 ^ chip.rmTcBit5);
          this.pgPhaseOut = rmXor << 9;
          if (rmXor ^ (chip.noiseHh & 1)) this.pgPhaseOut |= 0xd0;
          else this.pgPhaseOut |= 0x34;
        }
        break;
      case 16: // sd
        if (chip.rhy & 0x20) {
          this.pgPhaseOut = (chip.rmHhBit8 << 9) | ((chip.rmHhBit8 ^ (chip.noiseSd & 1)) << 8);
        }
        break;
      case 17: // tc
        if (chip.rhy & 0x20) {
          chip.rmTcBit3 = (phase >> 3) & 1;
          chip.rmTcBit5 = (phase >> 5) & 1;
          const rmXor = (chip.rmHhBit2 ^ chip.rmHhBit7) | (chip.rmHhBit3 ^ chip.rmTcBit5) | (chip.rmTcBit3 ^ chip.rmTcBit5);
          this.pgPhaseOut = (rmXor << 9) | 0x80;
        }
        break;
      default:
        break;
    }
  }

  // ---- registers

  write20(data) {
    this.am = (data >> 7) & 0x01;
    this.regVib = (data >> 6) & 0x01;
    this.regType = (data >> 5) & 0x01;
    this.egRates[2] = this.regType ? 0 : this.regRr;
    this.regKsr = (data >> 4) & 0x01;
    this.regMult = data & 0x0f;
    this.updateRate();
    this.updateInc();
  }

  write40(data) {
    this.regKsl = (data >> 6) & 0x03;
    this.regTl = data & 0x3f;
    this.updateKsl();
  }

  write60(data) {
    this.regAr = (data >> 4) & 0x0f;
    this.regDr = data & 0x0f;
    this.egRates[0] = this.regAr;
    this.egRates[1] = this.regDr;
    this.updateRate();
  }

  write80(data) {
    this.regSl = (data >> 4) & 0x0f;
    if (this.regSl === 0x0f) this.regSl = 0x1f;
    this.regRr = data & 0x0f;
    this.egRates[2] = this.regType ? 0 : this.regRr;
    this.egRates[3] = this.regRr;
    this.updateRate();
  }

  writeE0(data) {
    this.regWf = data & 0x07;
    if (this.chip.newm === 0) this.regWf &= 0x03;
  }

  // ---- output

  generate() {
    const phase = (this.pgPhaseOut + this.mod()) & 0xffff;
    const wf = logsinWf[this.regWf][phase & 0x3ff];
    const neg = wf & 0x8000 ? 0xffff : 0;
    let level = (wf & 0x7fff) + ((this.egOut << 3) & 0xffff);
    if (level > 0x1fff) level = 0x1fff;
    this.out = int16((exprom[level & 0xff] >> (level >> 8)) ^ neg);
  }

  calcFb(fb) {
    if (fb !== 0) this.fbmod = int16((this.prout + this.out) >> (0x09 - fb));
    else this.fbmod = 0;
    this.prout = this.out;
  }

  process(fb) {
    this.calcFb(fb);
    this.envelopeCalc();
    this.phaseGenerate();
    this.generate();
  }
}

class Channel {
  constructor(chip, num) {
    this.chip = chip;
    this.chNum = num;
    this.slotz = [null, null];
    this.pair = null;
    // what the channel's output sums: slots, or null for zeromod
    this.out = [null, null, null, null];
    this.outCnt = 0;
    this.chtype = CH_2OP;
    this.fNum = 0;
    this.block = 0;
    this.fb = 0;
    this.con = 0;
    this.alg = 0;
    this.ksv = 0;
    this.cha = 0;
    this.chb = 0;
    this.chc = 0;
    this.chd = 0;
  }

  setOut(a, b, c, d, cnt) {
    this.out[0] = a;
    this.out[1] = b;
    this.out[2] = c;
    this.out[3] = d;
    this.outCnt = cnt;
  }

  setupAlg() {
    const s0 = this.slotz[0];
    const s1 = this.slotz[1];
    const fb = (s) => { s.modKind = MOD_FB; s.modSlot = null; };
    const from = (s, src) => { s.modKind = MOD_OUT; s.modSlot = src; };
    const zero = (s) => { s.modKind = MOD_ZERO; s.modSlot = null; };
    if (this.chtype === CH_DRUM) {
      if (this.chNum === 7 || this.chNum === 8) {
        zero(s0);
        zero(s1);
        return;
      }
      if ((this.alg & 0x01) === 0) {
        fb(s0);
        from(s1, s0);
      } else {
        fb(s0);
        zero(s1);
      }
      return;
    }
    if (this.alg & 0x08) return;
    if (this.alg & 0x04) {
      const p = this.pair;
      p.setOut(null, null, null, null, 0);
      switch (this.alg & 0x03) {
        case 0x00:
          fb(p.slotz[0]); from(p.slotz[1], p.slotz[0]); from(s0, p.slotz[1]); from(s1, s0);
          this.setOut(s1, null, null, null, 1);
          break;
        case 0x01:
          fb(p.slotz[0]); from(p.slotz[1], p.slotz[0]); zero(s0); from(s1, s0);
          this.setOut(p.slotz[1], s1, null, null, 2);
          break;
        case 0x02:
          fb(p.slotz[0]); zero(p.slotz[1]); from(s0, p.slotz[1]); from(s1, s0);
          this.setOut(p.slotz[0], s1, null, null, 2);
          break;
        case 0x03:
          fb(p.slotz[0]); zero(p.slotz[1]); from(s0, p.slotz[1]); zero(s1);
          this.setOut(p.slotz[0], s0, s1, null, 3);
          break;
      }
    } else if ((this.alg & 0x01) === 0) {
      fb(s0);
      from(s1, s0);
      this.setOut(s1, null, null, null, 1);
    } else {
      fb(s0);
      zero(s1);
      this.setOut(s0, s1, null, null, 2);
    }
  }

  updateAlg() {
    this.alg = this.con;
    if (this.chip.newm) {
      if (this.chtype === CH_4OP) {
        this.pair.alg = 0x04 | (this.con << 1) | this.pair.con;
        this.alg = 0x08;
        this.pair.setupAlg();
      } else if (this.chtype === CH_4OP2) {
        this.alg = 0x04 | (this.pair.con << 1) | this.con;
        this.pair.alg = 0x08;
        this.setupAlg();
      } else {
        this.setupAlg();
      }
    } else {
      this.setupAlg();
    }
  }

  frequencyChanged() {
    for (const s of this.slotz) {
      s.updateKsl();
      s.updateRate();
      s.updateInc();
    }
  }

  writeA0(data) {
    if (this.chip.newm && this.chtype === CH_4OP2) return;
    this.fNum = (this.fNum & 0x300) | data;
    this.ksv = ((this.block << 1) | ((this.fNum >> (0x09 - this.chip.nts)) & 0x01)) & 0xff;
    this.frequencyChanged();
    if (this.chip.newm && this.chtype === CH_4OP) {
      this.pair.fNum = this.fNum;
      this.pair.ksv = this.ksv;
      this.pair.frequencyChanged();
    }
  }

  writeB0(data) {
    if (this.chip.newm && this.chtype === CH_4OP2) return;
    this.fNum = (this.fNum & 0xff) | ((data & 0x03) << 8);
    this.block = (data >> 2) & 0x07;
    this.ksv = ((this.block << 1) | ((this.fNum >> (0x09 - this.chip.nts)) & 0x01)) & 0xff;
    this.frequencyChanged();
    if (this.chip.newm && this.chtype === CH_4OP) {
      this.pair.fNum = this.fNum;
      this.pair.block = this.block;
      this.pair.ksv = this.ksv;
      this.pair.frequencyChanged();
    }
  }

  writeC0(data) {
    this.fb = (data & 0x0e) >> 1;
    this.con = data & 0x01;
    this.updateAlg();
    if (this.chip.newm) {
      this.cha = (data >> 4) & 0x01 ? 0xffff : 0;
      this.chb = (data >> 5) & 0x01 ? 0xffff : 0;
      this.chc = (data >> 6) & 0x01 ? 0xffff : 0;
      this.chd = (data >> 7) & 0x01 ? 0xffff : 0;
    } else {
      this.cha = this.chb = 0xffff;
      this.chc = this.chd = 0;
    }
  }

  keyOn() {
    if (this.chip.newm) {
      if (this.chtype === CH_4OP) {
        this.slotz[0].key |= EGK_NORM;
        this.slotz[1].key |= EGK_NORM;
        this.pair.slotz[0].key |= EGK_NORM;
        this.pair.slotz[1].key |= EGK_NORM;
      } else if (this.chtype === CH_2OP || this.chtype === CH_DRUM) {
        this.slotz[0].key |= EGK_NORM;
        this.slotz[1].key |= EGK_NORM;
      }
    } else {
      this.slotz[0].key |= EGK_NORM;
      this.slotz[1].key |= EGK_NORM;
    }
  }

  keyOff() {
    if (this.chip.newm) {
      if (this.chtype === CH_4OP) {
        this.slotz[0].key &= ~EGK_NORM;
        this.slotz[1].key &= ~EGK_NORM;
        this.pair.slotz[0].key &= ~EGK_NORM;
        this.pair.slotz[1].key &= ~EGK_NORM;
      } else if (this.chtype === CH_2OP || this.chtype === CH_DRUM) {
        this.slotz[0].key &= ~EGK_NORM;
        this.slotz[1].key &= ~EGK_NORM;
      }
    } else {
      this.slotz[0].key &= ~EGK_NORM;
      this.slotz[1].key &= ~EGK_NORM;
    }
  }
}

const clip = (s) => (s > 32767 ? 32767 : s < -32768 ? -32768 : s);

export class Opl3 {
  constructor(samplerate) {
    this.reset(samplerate);
  }

  // OPL3_Reset
  reset(samplerate) {
    this.slot = [];
    for (let i = 0; i < 36; i++) this.slot.push(new Slot(this, i));
    this.channel = [];
    for (let c = 0; c < 18; c++) this.channel.push(new Channel(this, c));
    this.timer = 0;
    this.egTimer = 0;
    this.egTimerrem = 0;
    this.egState = 0;
    this.egAdd = 0;
    this.egTimerLo = 0;
    this.newm = 0;
    this.nts = 0;
    this.rhy = 0;
    this.vibpos = 0;
    this.vibshift = 1;
    this.tremolo = 0;
    this.tremolopos = 0;
    this.tremoloshift = 4;
    this.noise = 1;
    this.noiseHh = 0;
    this.noiseSd = 0;
    this.mixbuff = [0, 0, 0, 0];
    this.rmHhBit2 = 0;
    this.rmHhBit3 = 0;
    this.rmHhBit7 = 0;
    this.rmHhBit8 = 0;
    this.rmTcBit3 = 0;
    this.rmTcBit5 = 0;
    this.rateratio = Math.floor((samplerate * (1 << RSM_FRAC)) / 49716);
    this.samplecnt = 0;
    this.oldsamples = [0, 0, 0, 0];
    this.samples = [0, 0, 0, 0];
    this.writebufSamplecnt = 0;
    this.writebufCur = 0;
    this.writebufLast = 0;
    this.writebufLasttime = 0;
    this.wbTime = new Float64Array(WRITEBUF_SIZE);
    this.wbReg = new Uint16Array(WRITEBUF_SIZE);
    this.wbData = new Uint8Array(WRITEBUF_SIZE);
    for (let c = 0; c < 18; c++) {
      const ch = this.channel[c];
      const s = chSlot[c];
      ch.slotz[0] = this.slot[s];
      ch.slotz[1] = this.slot[s + 3];
      this.slot[s].channel = ch;
      this.slot[s + 3].channel = ch;
      if (c % 9 < 3) ch.pair = this.channel[c + 3];
      else if (c % 9 < 6) ch.pair = this.channel[c - 3];
      ch.setOut(null, null, null, null, 0);
      ch.chtype = CH_2OP;
      ch.cha = 0xffff;
      ch.chb = 0xffff;
    }
    for (let c = 0; c < 18; c++) this.channel[c].setupAlg();
  }

  updateRhythm(data) {
    this.rhy = data & 0x3f;
    const ch6 = this.channel[6];
    const ch7 = this.channel[7];
    const ch8 = this.channel[8];
    if (this.rhy & 0x20) {
      ch6.setOut(ch6.slotz[1], ch6.slotz[1], null, null, 2);
      ch7.setOut(ch7.slotz[0], ch7.slotz[0], ch7.slotz[1], ch7.slotz[1], 4);
      ch8.setOut(ch8.slotz[0], ch8.slotz[0], ch8.slotz[1], ch8.slotz[1], 4);
      for (let n = 6; n < 9; n++) this.channel[n].chtype = CH_DRUM;
      ch6.setupAlg();
      ch7.setupAlg();
      ch8.setupAlg();
      const key = (slot, on) => { if (on) slot.key |= EGK_DRUM; else slot.key &= ~EGK_DRUM; };
      key(ch7.slotz[0], this.rhy & 0x01); // hh
      key(ch8.slotz[1], this.rhy & 0x02); // tc
      key(ch8.slotz[0], this.rhy & 0x04); // tom
      key(ch7.slotz[1], this.rhy & 0x08); // sd
      key(ch6.slotz[0], this.rhy & 0x10); // bd
      key(ch6.slotz[1], this.rhy & 0x10);
    } else {
      for (let n = 6; n < 9; n++) {
        const ch = this.channel[n];
        ch.chtype = CH_2OP;
        ch.setupAlg();
        ch.slotz[0].key &= ~EGK_DRUM;
        ch.slotz[1].key &= ~EGK_DRUM;
      }
    }
  }

  set4Op(data) {
    for (let bit = 0; bit < 6; bit++) {
      let chnum = bit;
      if (bit >= 3) chnum += 9 - 3;
      if ((data >> bit) & 0x01) {
        this.channel[chnum].chtype = CH_4OP;
        this.channel[chnum + 3].chtype = CH_4OP2;
        this.channel[chnum].updateAlg();
      } else {
        this.channel[chnum].chtype = CH_2OP;
        this.channel[chnum + 3].chtype = CH_2OP;
        this.channel[chnum].updateAlg();
        this.channel[chnum + 3].updateAlg();
      }
    }
  }

  // OPL3_WriteReg
  writeReg(reg, v) {
    const high = (reg >> 8) & 0x01;
    const regm = reg & 0xff;
    v &= 0xff;
    const slotAt = () => (adSlot[regm & 0x1f] >= 0 ? this.slot[18 * high + adSlot[regm & 0x1f]] : null);
    switch (regm & 0xf0) {
      case 0x00:
        if (high) {
          switch (regm & 0x0f) {
            case 0x04: this.set4Op(v); break;
            case 0x05: this.newm = v & 0x01; break;
          }
        } else if ((regm & 0x0f) === 0x08) {
          this.nts = (v >> 6) & 0x01;
        }
        break;
      case 0x20:
      case 0x30: { const s = slotAt(); if (s) s.write20(v); break; }
      case 0x40:
      case 0x50: { const s = slotAt(); if (s) s.write40(v); break; }
      case 0x60:
      case 0x70: { const s = slotAt(); if (s) s.write60(v); break; }
      case 0x80:
      case 0x90: { const s = slotAt(); if (s) s.write80(v); break; }
      case 0xe0:
      case 0xf0: { const s = slotAt(); if (s) s.writeE0(v); break; }
      case 0xa0:
        if ((regm & 0x0f) < 9) this.channel[9 * high + (regm & 0x0f)].writeA0(v);
        break;
      case 0xb0:
        if (regm === 0xbd && !high) {
          this.tremoloshift = ((((v >> 7) ^ 1) << 1) + 2) & 0xff;
          const vibshift = ((v >> 6) & 0x01) ^ 1;
          if (this.vibshift !== vibshift) {
            this.vibshift = vibshift;
            for (const s of this.slot) s.updateInc();
          }
          this.updateRhythm(v);
        } else if ((regm & 0x0f) < 9) {
          const ch = this.channel[9 * high + (regm & 0x0f)];
          ch.writeB0(v);
          if (v & 0x20) ch.keyOn();
          else ch.keyOff();
        }
        break;
      case 0xc0:
        if ((regm & 0x0f) < 9) this.channel[9 * high + (regm & 0x0f)].writeC0(v);
        break;
    }
  }

  // OPL3_WriteRegBuffered: writes land WRITEBUF_DELAY samples apart
  writeRegBuffered(reg, v) {
    const last = this.writebufLast;
    if (this.wbReg[last] & 0x200) {
      this.writeReg(this.wbReg[last] & 0x1ff, this.wbData[last]);
      this.writebufCur = (last + 1) % WRITEBUF_SIZE;
      this.writebufSamplecnt = this.wbTime[last];
    }
    this.wbReg[last] = reg | 0x200;
    this.wbData[last] = v & 0xff;
    let time1 = this.writebufLasttime + WRITEBUF_DELAY;
    const time2 = this.writebufSamplecnt;
    if (time1 < time2) time1 = time2;
    this.wbTime[last] = time1;
    this.writebufLasttime = time1;
    this.writebufLast = (last + 1) % WRITEBUF_SIZE;
  }

  // the channel's output for one side: slots at or past `delayFrom` are heard
  // as they were the sample before (OPL_QUIRK_CHANNELSAMPLEDELAY)
  accum(ch, delayFrom) {
    let accm = 0;
    for (let k = 0; k < ch.outCnt; k++) {
      const s = ch.out[k];
      const v = s === null ? 0 : s.slotNum >= delayFrom ? s.prout : s.out;
      accm = int16(accm + v);
    }
    return accm;
  }

  // OPL3_Generate4Ch: one sample at 49716 Hz into buf4 (left, right, and the
  // OPL3's second pair of outputs)
  generate4Ch(buf4) {
    buf4[1] = clip(this.mixbuff[1]);
    buf4[3] = clip(this.mixbuff[3]);

    // the noise LFSR, 36 steps at once
    {
      const s = this.noise >>> 0;
      const f0to8 = (s ^ (s >>> 14)) & 0x1ff;
      const f9to17 = ((s >>> 9) ^ f0to8) & 0x1ff;
      const f18to22 = ((s >>> 18) ^ f9to17) & 0x1f;
      const f23to31 = (f0to8 ^ ((f9to17 >>> 5) | (f18to22 << 4))) >>> 0;
      const f32to35 = (f9to17 ^ f23to31) & 0x0f;
      this.noiseHh = (s >>> 13) & 1;
      this.noiseSd = (s >>> 16) & 1;
      this.noise = (((f9to17 >>> 4) & 0x1f) | (f18to22 << 5) | (f23to31 << 10) | (f32to35 << 19)) >>> 0;
    }

    for (let c = 0; c < 18; c++) {
      const ch = this.channel[c];
      ch.slotz[0].process(ch.fb);
      ch.slotz[1].process(ch.fb);
    }

    let mix0 = 0;
    let mix1 = 0;
    for (let c = 0; c < 18; c++) {
      const ch = this.channel[c];
      if (!ch.outCnt || !(ch.cha | ch.chc)) continue;
      const accm = this.accum(ch, 15);
      mix0 += int16(accm & ch.cha);
      mix1 += int16(accm & ch.chc);
    }
    this.mixbuff[0] = mix0;
    this.mixbuff[2] = mix1;
    buf4[0] = clip(this.mixbuff[0]);
    buf4[2] = clip(this.mixbuff[2]);

    if ((this.timer & 0x3f) === 0x3f) {
      this.tremolopos++;
      if (this.tremolopos === 210) this.tremolopos = 0;
    }
    if (this.tremolopos < 105) this.tremolo = this.tremolopos >> this.tremoloshift;
    else this.tremolo = (210 - this.tremolopos) >> this.tremoloshift;
    if ((this.timer & 0x3ff) === 0x3ff) this.vibpos = (this.vibpos + 1) & 7;
    this.timer = (this.timer + 1) & 0xffff;

    if (this.egState) {
      const low = this.egTimer % 8192;
      if (!low) {
        this.egAdd = 0;
      } else {
        let shift = 0;
        while (((low >> shift) & 1) === 0) shift++;
        this.egAdd = shift + 1;
      }
      this.egTimerLo = this.egTimer % 4;
    }
    if (this.egTimerrem || this.egState) {
      if (this.egTimer === 0xfffffffff) {
        this.egTimer = 0;
        this.egTimerrem = 1;
      } else {
        this.egTimer++;
        this.egTimerrem = 0;
      }
    }
    this.egState ^= 1;

    // OPL3_MixRight: heard at the next sample
    let r0 = 0;
    let r1 = 0;
    for (let c = 0; c < 18; c++) {
      const ch = this.channel[c];
      if (!ch.outCnt || !(ch.chb | ch.chd)) continue;
      const accm = this.accum(ch, 33);
      r0 += int16(accm & ch.chb);
      r1 += int16(accm & ch.chd);
    }
    this.mixbuff[1] = r0;
    this.mixbuff[3] = r1;

    for (;;) {
      const cur = this.writebufCur;
      if (!(this.wbTime[cur] <= this.writebufSamplecnt)) break;
      if (!(this.wbReg[cur] & 0x200)) break;
      this.wbReg[cur] &= 0x1ff;
      this.writeReg(this.wbReg[cur], this.wbData[cur]);
      this.writebufCur = (cur + 1) % WRITEBUF_SIZE;
    }
    this.writebufSamplecnt++;
  }

  // OPL3_Generate4ChResampled
  generate4ChResampled(buf4) {
    while (this.samplecnt >= this.rateratio) {
      this.oldsamples[0] = this.samples[0];
      this.oldsamples[1] = this.samples[1];
      this.oldsamples[2] = this.samples[2];
      this.oldsamples[3] = this.samples[3];
      this.generate4Ch(this.samples);
      this.samplecnt -= this.rateratio;
    }
    const r = this.rateratio;
    const n = this.samplecnt;
    for (let i = 0; i < 4; i++) {
      buf4[i] = int16(Math.trunc((this.oldsamples[i] * (r - n) + this.samples[i] * n) / r));
    }
    this.samplecnt += 1 << RSM_FRAC;
  }

  // OPL3_GenerateStream: numsamples stereo frames into out (Int16Array), from at
  generateStream(out, at, numsamples) {
    const four = this._four || (this._four = [0, 0, 0, 0]);
    for (let i = 0; i < numsamples; i++) {
      this.generate4ChResampled(four);
      out[at + i * 2] = four[0];
      out[at + i * 2 + 1] = four[1];
    }
  }
}
