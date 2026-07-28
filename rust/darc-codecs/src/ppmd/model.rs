//! The PPMII model, ported from `Compression/PPMD/Model.cpp`.
//!
//! # Memory layout, which is the whole difficulty
//!
//! The C works in raw pointers into the suballocator's heap; here every
//! pointer is a byte offset into [`SubAllocator`]'s `Vec<u8>`. The C already
//! stores intra-heap references as 32-bit offsets (`CTX_REF`/`STATE_REF`, added
//! for 64-bit portability) so that `PPM_CONTEXT` stays exactly `UNIT_SIZE` = 12
//! bytes, and this port keeps those on-heap layouts byte for byte:
//!
//! ```text
//! PPM_CONTEXT @ c        STATE @ s
//!   c+0  NumStats  u8      s+0  Symbol     u8
//!   c+1  Flags     u8      s+1  Freq       u8
//!   c+2  SummFreq  u16     s+2  Successor  u32   (CTX_REF)
//!   c+4  Stats     u32
//!   c+8  Suffix    u32
//! ```
//!
//! **`oneState()` is a union, not a field.** The C writes
//! `return (STATE&) SummFreq`, so a single-symbol context stores its state
//! *overlapping* `SummFreq` and `Stats`: symbol at `c+2`, freq at `c+3`,
//! successor at `c+4..c+8` — i.e. exactly on top of `Stats`. Getting this wrong
//! would not crash, it would silently corrupt one field with another.
//!
//! Refs are 1-based (`ref = offset + 1`, 0 = NULL) so that `pText == HeapStart`
//! can be stored in a `Successor` without being mistaken for NULL.
//!
//! # No seam
//!
//! There is no stage boundary inside this file at which a partial port could be
//! compared against the C: the coder, the model and the allocator branch on
//! each other. See [`super::suballoc`] for the measurement that establishes it.

use super::coder::{RangeCoder, TOP};
use super::stream::PrimeStream;
use super::suballoc::SubAllocator;

const UP_FREQ: usize = 5;
const INT_BITS: u32 = 7;
const PERIOD_BITS: u32 = 7;
pub const TOT_BITS: u32 = INT_BITS + PERIOD_BITS;
const INTERVAL: u32 = 1 << INT_BITS;
const BIN_SCALE: u32 = 1 << TOT_BITS;
const MAX_FREQ: u32 = 124;
const O_BOUND: i32 = 9;
const MAX_O: usize = 128;

/// `PPMdSignature` from PPMdType.h, stamped into `DummySEE2Cont`.
const PPMD_SIGNATURE: u32 = 0x84AC_AF8F;

const INIT_BIN_ESC: [u16; 8] = [
    0x3CDD, 0x1F3F, 0x59BF, 0x48F3, 0x64A1, 0x5ABC, 0x6632, 0x6051,
];

/// Tabulated escapes for an exponential symbol distribution.
const EXP_ESCAPE: [u8; 16] = [25, 14, 9, 7, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2];

/// `MR_METHOD`: what to do when the model runs out of memory.
pub const MRM_RESTART: i32 = 0;
pub const MRM_CUT_OFF: i32 = 1;
pub const MRM_FREEZE: i32 = 2;

#[inline]
fn get_mean(summ: u32, shift: u32, round: u32) -> u32 {
    (summ + (1 << (shift - round))) >> shift
}

/// SEE-context for PPM-contexts with masked symbols.
#[derive(Clone, Copy, Default)]
struct See2Context {
    summ: u16,
    shift: u8,
    count: u8,
}

impl See2Context {
    fn init(&mut self, init_val: u32) {
        self.shift = (PERIOD_BITS - 4) as u8;
        self.summ = (init_val << self.shift) as u16;
        self.count = 7;
    }
    fn get_mean(&mut self) -> u32 {
        let ret = (self.summ >> self.shift) as u32;
        self.summ = self.summ.wrapping_sub(ret as u16);
        ret + (ret == 0) as u32
    }
    fn update(&mut self) {
        if (self.shift as u32) < PERIOD_BITS {
            self.count -= 1;
            if self.count == 0 {
                self.summ = self.summ.wrapping_add(self.summ);
                self.count = 3 << self.shift;
                self.shift += 1;
            }
        }
    }
}

pub struct Model {
    pub sa: SubAllocator,
    pub rc: RangeCoder,

    see2cont: [[See2Context; 32]; 24],
    dummy_see2cont: See2Context,
    bin_summ: [[u16; 64]; 25],

    ns2bs_indx: [u8; 256],
    q_table: [u8; 260],
    char_mask: [u8; 256],

    /// Offsets, 0 = NULL. State offsets are always inside the units area, so
    /// 0 is never a legitimate state and doubles as the null sentinel exactly
    /// as the C's NULL pointer does.
    found_state: usize,
    max_context: usize,

    init_esc: u32,
    order_fall: i32,
    run_length: i32,
    init_rl: i32,
    max_order: i32,
    num_masked: u32,
    prev_success: u32,
    esc_count: u8,
    print_count: u8,
    mr_method: i32,
}

impl Model {
    pub fn new() -> Self {
        let mut m = Model {
            sa: SubAllocator::new(),
            rc: RangeCoder::new(),
            see2cont: [[See2Context::default(); 32]; 24],
            dummy_see2cont: See2Context::default(),
            bin_summ: [[0u16; 64]; 25],
            ns2bs_indx: [0; 256],
            q_table: [0; 260],
            char_mask: [0; 256],
            found_state: 0,
            max_context: 0,
            init_esc: 0,
            order_fall: 0,
            run_length: 0,
            init_rl: 0,
            max_order: 0,
            num_masked: 0,
            prev_success: 0,
            esc_count: 0,
            print_count: 0,
            mr_method: MRM_RESTART,
        };
        m.startup();
        m
    }

    /// `PPMD_STARTUP`: the constant tables. The suballocator builds its own
    /// index tables in `SubAllocator::new`, which is the same code.
    fn startup(&mut self) {
        self.ns2bs_indx[0] = 0;
        self.ns2bs_indx[1] = 2;
        for i in 2..11 {
            self.ns2bs_indx[i] = 4;
        }
        for i in 11..256 {
            self.ns2bs_indx[i] = 6;
        }
        for i in 0..UP_FREQ {
            self.q_table[i] = i as u8;
        }
        let mut m = UP_FREQ;
        let mut k = 1usize;
        let mut step = 1usize;
        for i in UP_FREQ..260 {
            self.q_table[i] = m as u8;
            k -= 1;
            if k == 0 {
                step += 1;
                k = step;
                m += 1;
            }
        }
        // `(unsigned int&) DummySEE2Cont = PPMdSignature` -- the signature is
        // written over the whole 4-byte struct, so Summ/Shift/Count take its
        // bytes. Little-endian, matching every target this builds for.
        let b = PPMD_SIGNATURE.to_le_bytes();
        self.dummy_see2cont.summ = u16::from_le_bytes([b[0], b[1]]);
        self.dummy_see2cont.shift = b[2];
        self.dummy_see2cont.count = b[3];
    }

    // --- heap field accessors --------------------------------------------

    #[inline]
    fn rd8(&self, at: usize) -> u8 {
        self.sa.heap()[at]
    }
    #[inline]
    fn wr8(&mut self, at: usize, v: u8) {
        self.sa.heap_mut()[at] = v;
    }
    #[inline]
    fn rd16(&self, at: usize) -> u16 {
        let h = self.sa.heap();
        u16::from_le_bytes([h[at], h[at + 1]])
    }
    #[inline]
    fn wr16(&mut self, at: usize, v: u16) {
        self.sa.heap_mut()[at..at + 2].copy_from_slice(&v.to_le_bytes());
    }
    #[inline]
    fn rd32(&self, at: usize) -> u32 {
        let h = self.sa.heap();
        u32::from_le_bytes([h[at], h[at + 1], h[at + 2], h[at + 3]])
    }
    #[inline]
    fn wr32(&mut self, at: usize, v: u32) {
        self.sa.heap_mut()[at..at + 4].copy_from_slice(&v.to_le_bytes());
    }

    /// Ref (1-based) from offset. The C's `RPCTX`/`RPSTAT`/`BREF`.
    #[inline]
    fn rref(off: usize) -> u32 {
        (off + 1) as u32
    }
    /// Offset from ref; 0 stays 0 and callers must not dereference it.
    #[inline]
    fn roff(r: u32) -> usize {
        if r == 0 { 0 } else { (r - 1) as usize }
    }

    // context fields
    #[inline]
    fn ns(&self, c: usize) -> u8 { self.rd8(c) }
    #[inline]
    fn set_ns(&mut self, c: usize, v: u8) { self.wr8(c, v) }
    #[inline]
    fn flags(&self, c: usize) -> u8 { self.rd8(c + 1) }
    #[inline]
    fn set_flags(&mut self, c: usize, v: u8) { self.wr8(c + 1, v) }
    #[inline]
    fn summ_freq(&self, c: usize) -> u32 { self.rd16(c + 2) as u32 }
    #[inline]
    fn set_summ_freq(&mut self, c: usize, v: u32) { self.wr16(c + 2, v as u16) }
    #[inline]
    fn stats(&self, c: usize) -> u32 { self.rd32(c + 4) }
    #[inline]
    fn set_stats(&mut self, c: usize, v: u32) { self.wr32(c + 4, v) }
    #[inline]
    fn suffix(&self, c: usize) -> u32 { self.rd32(c + 8) }
    #[inline]
    fn set_suffix(&mut self, c: usize, v: u32) { self.wr32(c + 8, v) }
    /// The union: a single-symbol context's state overlaps SummFreq/Stats.
    #[inline]
    fn one_state(c: usize) -> usize { c + 2 }

    // state fields
    #[inline]
    fn sym(&self, s: usize) -> u8 { self.rd8(s) }
    #[inline]
    fn set_sym(&mut self, s: usize, v: u8) { self.wr8(s, v) }
    #[inline]
    fn freq(&self, s: usize) -> u32 { self.rd8(s + 1) as u32 }
    #[inline]
    fn set_freq(&mut self, s: usize, v: u32) { self.wr8(s + 1, v as u8) }
    #[inline]
    fn succ(&self, s: usize) -> u32 { self.rd32(s + 2) }
    #[inline]
    fn set_succ(&mut self, s: usize, v: u32) { self.wr32(s + 2, v) }

    /// `SWAP`: the C swaps `(WORD&)` and `Successor` separately, which together
    /// are the whole 6-byte state.
    fn swap_states(&mut self, a: usize, b: usize) {
        for i in 0..6 {
            let t = self.rd8(a + i);
            let u = self.rd8(b + i);
            self.wr8(a + i, u);
            self.wr8(b + i, t);
        }
    }

    /// `StateCpy`.
    fn state_cpy(&mut self, dst: usize, src: usize) {
        for i in 0..6 {
            let v = self.rd8(src + i);
            self.wr8(dst + i, v);
        }
    }

    /// `GE_UNITS(r)`: does this ref point into the unit-storage region?
    #[inline]
    fn ge_units(&self, r: u32) -> bool {
        r != 0 && Self::roff(r) >= self.sa.units_start
    }

    // --- model construction ----------------------------------------------

    /// `StartModelRare`.
    pub fn start_model_rare(&mut self, max_order: i32, mr_method: i32) {
        self.char_mask = [0; 256];
        self.esc_count = 1;
        self.print_count = 1;
        if max_order < 2 {
            // Solid mode: keep the tree, just recompute OrderFall.
            self.order_fall = self.max_order;
            let mut pc = self.max_context;
            while self.suffix(pc) != 0 {
                self.order_fall -= 1;
                pc = Self::roff(self.suffix(pc));
            }
            return;
        }
        self.max_order = max_order;
        self.order_fall = max_order;
        self.mr_method = mr_method;
        self.sa.init();
        self.init_rl = -(if max_order < 12 { max_order } else { 12 }) - 1;
        self.run_length = self.init_rl;

        let mc = self.sa.alloc_context();
        self.max_context = mc;
        self.set_suffix(mc, 0);
        self.set_ns(mc, 255);
        self.set_summ_freq(mc, 255 + 2);
        let stats = self.sa.alloc_units(256 / 2);
        self.set_stats(mc, Self::rref(stats));
        self.prev_success = 0;
        for i in 0..256usize {
            let p = stats + i * 6;
            self.set_sym(p, i as u8);
            self.set_freq(p, 1);
            self.set_succ(p, 0);
        }

        let mut i = 0usize;
        for m in 0..25usize {
            while self.q_table[i] as usize == m {
                i += 1;
            }
            for k in 0..8usize {
                self.bin_summ[m][k] =
                    (BIN_SCALE - INIT_BIN_ESC[k] as u32 / (i as u32 + 1)) as u16;
            }
            for k in (8..64).step_by(8) {
                for j in 0..8 {
                    self.bin_summ[m][k + j] = self.bin_summ[m][j];
                }
            }
        }
        let mut i = 0usize;
        for m in 0..24usize {
            while self.q_table[i + 3] as usize == m + 3 {
                i += 1;
            }
            self.see2cont[m][0].init(2 * i as u32 + 5);
            for k in 1..32 {
                self.see2cont[m][k] = self.see2cont[m][0];
            }
        }
    }

    /// `PPM_CONTEXT::refresh`.
    fn refresh(&mut self, c: usize, old_nu: usize, scale: bool) {
        let sc = scale as u32;
        let mut i = self.ns(c) as u32;
        let n = ((i + 2) >> 1) as usize;
        let p0 = self.sa.shrink_units(Self::roff(self.stats(c)), old_nu, n);
        self.set_stats(c, Self::rref(p0));
        let mut p = p0;
        let f = self.flags(c);
        self.set_flags(
            c,
            (f & (0x10 + 0x04 * sc as u8)) + 0x08 * (self.sym(p) >= 0x40) as u8,
        );
        let mut esc_freq = self.summ_freq(c) as i64 - self.freq(p) as i64;
        let nf = (self.freq(p) + sc) >> sc;
        self.set_freq(p, nf);
        let mut summ = nf;
        while i > 0 {
            p += 6;
            esc_freq -= self.freq(p) as i64;
            let nf = (self.freq(p) + sc) >> sc;
            self.set_freq(p, nf);
            summ += nf;
            let fl = self.flags(c) | 0x08 * (self.sym(p) >= 0x40) as u8;
            self.set_flags(c, fl);
            i -= 1;
        }
        let esc = ((esc_freq + sc as i64) >> sc) as u32;
        self.set_summ_freq(c, summ + esc);
    }

    /// `PPM_CONTEXT::cutOff`. Returns 0 for the C's NULL.
    fn cut_off(&mut self, c: usize, order: i32) -> usize {
        if self.ns(c) == 0 {
            let p = Self::one_state(c);
            if self.ge_units(self.succ(p)) {
                if order < self.max_order {
                    let s = Self::roff(self.succ(p));
                    let r = self.cut_off(s, order + 1);
                    self.set_succ(p, if r == 0 { 0 } else { Self::rref(r) });
                } else {
                    self.set_succ(p, 0);
                }
                if self.succ(p) == 0 && order > O_BOUND {
                    self.sa.special_free_unit(c);
                    return 0;
                }
                return c;
            } else {
                self.sa.special_free_unit(c);
                return 0;
            }
        }
        let tmp = ((self.ns(c) as usize) + 2) >> 1;
        let s_base = self.sa.move_units_up(Self::roff(self.stats(c)), tmp);
        self.set_stats(c, Self::rref(s_base));
        let ns = self.ns(c) as i32;
        let mut i = ns;
        let mut pi = ns;
        while pi >= 0 {
            let p = s_base + pi as usize * 6;
            if !self.ge_units(self.succ(p)) {
                self.set_succ(p, 0);
                let other = s_base + i as usize * 6;
                self.swap_states(p, other);
                i -= 1;
            } else if order < self.max_order {
                let s = Self::roff(self.succ(p));
                let r = self.cut_off(s, order + 1);
                self.set_succ(p, if r == 0 { 0 } else { Self::rref(r) });
            } else {
                self.set_succ(p, 0);
            }
            pi -= 1;
        }
        if i != ns && order != 0 {
            self.set_ns(c, i as u8);
            let p = s_base;
            if i < 0 {
                self.sa.free_units(p, tmp);
                self.sa.special_free_unit(c);
                return 0;
            } else if i == 0 {
                let fl = (self.flags(c) & 0x10) + 0x08 * (self.sym(p) >= 0x40) as u8;
                self.set_flags(c, fl);
                let os = Self::one_state(c);
                self.state_cpy(os, p);
                self.sa.free_units(p, tmp);
                let f = (self.freq(os) + 11) >> 3;
                self.set_freq(os, f);
            } else {
                let scale = self.summ_freq(c) > 16 * i as u32;
                self.refresh(c, tmp, scale);
            }
        }
        c
    }

    /// `PPM_CONTEXT::removeBinConts`. Returns 0 for NULL.
    fn remove_bin_conts(&mut self, c: usize, order: i32) -> usize {
        if self.ns(c) == 0 {
            let p = Self::one_state(c);
            if self.ge_units(self.succ(p)) && order < self.max_order {
                let s = Self::roff(self.succ(p));
                let r = self.remove_bin_conts(s, order + 1);
                self.set_succ(p, if r == 0 { 0 } else { Self::rref(r) });
            } else {
                self.set_succ(p, 0);
            }
            let suf = Self::roff(self.suffix(c));
            if self.succ(p) == 0 && (self.ns(suf) == 0 || self.flags(suf) == 0xFF) {
                self.sa.free_units(c, 1);
                return 0;
            }
            return c;
        }
        let s_base = Self::roff(self.stats(c));
        let mut pi = self.ns(c) as i32;
        while pi >= 0 {
            let p = s_base + pi as usize * 6;
            if self.ge_units(self.succ(p)) && order < self.max_order {
                let s = Self::roff(self.succ(p));
                let r = self.remove_bin_conts(s, order + 1);
                self.set_succ(p, if r == 0 { 0 } else { Self::rref(r) });
            } else {
                self.set_succ(p, 0);
            }
            pi -= 1;
        }
        c
    }

    /// `RestoreModelRare`.
    fn restore_model_rare(&mut self, pc1: usize, min_context: usize, f_successor: usize) {
        self.sa.p_text = 0;
        let mut pc = self.max_context;
        while pc != pc1 {
            let ns = self.ns(pc);
            if ns == 1 {
                self.set_ns(pc, 0);
                let st = Self::roff(self.stats(pc));
                let fl = (self.flags(pc) & 0x10) + 0x08 * (self.sym(st) >= 0x40) as u8;
                self.set_flags(pc, fl);
                let os = Self::one_state(pc);
                self.state_cpy(os, st);
                self.sa.special_free_unit(st);
                let f = (self.freq(os) + 11) >> 3;
                self.set_freq(os, f);
            } else {
                self.set_ns(pc, ns - 1);
                let nu = ((self.ns(pc) as usize) + 3) >> 1;
                self.refresh(pc, nu, false);
            }
            pc = Self::roff(self.suffix(pc));
        }
        while pc != min_context {
            if self.ns(pc) == 0 {
                let os = Self::one_state(pc);
                let f = self.freq(os);
                self.set_freq(os, f - (f >> 1));
            } else {
                let sf = self.summ_freq(pc) + 4;
                self.set_summ_freq(pc, sf);
                if sf > 128 + 4 * self.ns(pc) as u32 {
                    let nu = ((self.ns(pc) as usize) + 2) >> 1;
                    self.refresh(pc, nu, true);
                }
            }
            pc = Self::roff(self.suffix(pc));
        }
        if self.mr_method > MRM_FREEZE {
            self.max_context = f_successor;
            // `GlueCount += !(BList[1].Stamp & 1)`
            self.sa.glue_count += (!self.sa.blist_stamp(1) & 1) as u32;
        } else if self.mr_method == MRM_FREEZE {
            while self.suffix(self.max_context) != 0 {
                self.max_context = Self::roff(self.suffix(self.max_context));
            }
            let mc = self.max_context;
            self.remove_bin_conts(mc, 0);
            self.mr_method += 1;
            self.sa.glue_count = 0;
            self.order_fall = self.max_order;
        } else if self.mr_method == MRM_RESTART
            // Both sides in 32 bits, as in the C: SubAllocatorSize is a DWORD
            // and GetUsedMemory returns one, and the used figure can have
            // wrapped past zero by the time this is asked.
            || self.sa.get_used_memory() < (self.sa.sub_allocator_size as u32 >> 1)
        {
            let (mo, mrm) = (self.max_order, self.mr_method);
            self.start_model_rare(mo, mrm);
            self.esc_count = 0;
            self.print_count = 0xFF;
        } else {
            while self.suffix(self.max_context) != 0 {
                self.max_context = Self::roff(self.suffix(self.max_context));
            }
            loop {
                let mc = self.max_context;
                self.cut_off(mc, 0);
                self.sa.expand_text_area();
                if self.sa.get_used_memory() <= 3 * (self.sa.sub_allocator_size as u32 >> 2) {
                    break;
                }
            }
            self.sa.glue_count = 0;
            self.order_fall = self.max_order;
        }
    }

    /// `ReduceOrder`. Returns 0 for NULL.
    fn reduce_order(&mut self, mut p: usize, pc_in: usize) -> usize {
        let mut ps = [0usize; MAX_O];
        let mut n = 0usize;
        let mut pc = pc_in;
        let pc1 = pc_in;
        let up_branch = Self::rref(self.sa.p_text);
        let sym = self.sym(self.found_state);

        ps[n] = self.found_state;
        n += 1;
        self.set_succ(self.found_state, up_branch);
        self.order_fall += 1;

        let mut entered = false;
        if p != 0 {
            pc = Self::roff(self.suffix(pc));
            entered = true;
        }
        loop {
            if !entered {
                if self.suffix(pc) == 0 {
                    if self.mr_method > MRM_FREEZE {
                        while n > 0 {
                            n -= 1;
                            self.set_succ(ps[n], Self::rref(pc));
                        }
                        self.sa.p_text = 1;
                        self.order_fall = 1;
                    }
                    return pc;
                }
                pc = Self::roff(self.suffix(pc));
                if self.ns(pc) != 0 {
                    p = Self::roff(self.stats(pc));
                    if self.sym(p) != sym {
                        loop {
                            let t = self.sym(p + 6);
                            p += 6;
                            if t == sym {
                                break;
                            }
                        }
                    }
                    let tmp = 2 * (self.freq(p) < MAX_FREQ - 9) as u32;
                    let f = self.freq(p) + tmp;
                    self.set_freq(p, f);
                    let sf = self.summ_freq(pc) + tmp;
                    self.set_summ_freq(pc, sf);
                } else {
                    p = Self::one_state(pc);
                    let f = self.freq(p) + (self.freq(p) < 32) as u32;
                    self.set_freq(p, f);
                }
            }
            entered = false;
            // LOOP_ENTRY
            if self.succ(p) != 0 {
                break;
            }
            ps[n] = p;
            n += 1;
            self.set_succ(p, up_branch);
            self.order_fall += 1;
        }
        if self.mr_method > MRM_FREEZE {
            pc = Self::roff(self.succ(p));
            while n > 0 {
                n -= 1;
                self.set_succ(ps[n], Self::rref(pc));
            }
            self.sa.p_text = 1;
            self.order_fall = 1;
            return pc;
        } else if self.succ(p) <= up_branch {
            let p1 = self.found_state;
            self.found_state = p;
            let r = self.create_successors(false, 0, pc);
            self.set_succ(p, if r == 0 { 0 } else { Self::rref(r) });
            self.found_state = p1;
        }
        if self.order_fall == 1 && pc1 == self.max_context {
            let s = self.succ(p);
            self.set_succ(self.found_state, s);
            self.sa.p_text -= 1;
        }
        Self::roff(self.succ(p))
    }

    /// `PPM_CONTEXT::rescale`.
    fn rescale(&mut self, c: usize) {
        let s_base = Self::roff(self.stats(c));
        let mut p = self.found_state;
        while p != s_base {
            self.swap_states(p, p - 6);
            p -= 6;
        }
        let f = self.freq(p) + 4;
        self.set_freq(p, f);
        let sf = self.summ_freq(c) + 4;
        self.set_summ_freq(c, sf);
        // `UINT EscFreq` in the C -- UNSIGNED, unlike the `int EscFreq` in
        // refresh(). It genuinely goes "negative" here and wraps, and the
        // later `EscFreq >> 1` is then a LOGICAL shift of a huge value. Using a
        // signed type keeps it negative and shifts arithmetically, which is the
        // same for small values and diverges exactly when it wraps.
        let mut esc_freq: u32 = self.summ_freq(c).wrapping_sub(self.freq(p));
        let adder =
            (self.order_fall != 0 || self.mr_method > MRM_FREEZE) as u32;
        let nf = (self.freq(p) + adder) >> 1;
        self.set_freq(p, nf);
        let mut summ = nf;
        let mut i = self.ns(c) as u32;
        while i > 0 {
            p += 6;
            esc_freq = esc_freq.wrapping_sub(self.freq(p));
            let nf = (self.freq(p) + adder) >> 1;
            self.set_freq(p, nf);
            summ += nf;
            if self.freq(p) > self.freq(p - 6) {
                // Bubble the state back into frequency order.
                let mut tmp = [0u8; 6];
                for k in 0..6 {
                    tmp[k] = self.rd8(p + k);
                }
                let tmp_freq = tmp[1] as u32;
                let mut p1 = p;
                loop {
                    self.state_cpy(p1, p1 - 6);
                    p1 -= 6;
                    if !(tmp_freq > self.freq(p1 - 6)) {
                        break;
                    }
                }
                for k in 0..6 {
                    self.wr8(p1 + k, tmp[k]);
                }
            }
            i -= 1;
        }
        self.set_summ_freq(c, summ);
        // `if (p->Freq == 0)`, and it must be a genuine RE-READ of the heap --
        // the bubble sort above may have moved a different state into `p`.
        //
        // This line is the one place where the C's compiler flags decide the
        // compressed format. StateCpy/SWAP type-pun through `(WORD&)`
        // references, violating strict aliasing, so a compiler allowed to
        // assume those WORD writes cannot alias this BYTE read will reuse the
        // value ASSIGNED to p->Freq in the loop's last iteration instead of
        // reloading the slot. Measured on the `dominant` corpus shape, all of
        // orders 3/4/10/16:
        //
        //     clang -O1                        reuses the assigned value
        //     clang -O2                        reuses the assigned value
        //     clang -O0                        re-reads
        //     clang -O1 -fno-strict-aliasing   re-reads
        //
        // Compression/PPMD/makefile passes `-fno-strict-aliasing`, so every
        // real -mppmd archive was written by a build that RE-READS, and that is
        // what this port has to reproduce. Isolated by flag: -fno-strict-
        // aliasing alone flips it; -fomit-frame-pointer -funroll-loops do not.
        // ppmd-check.sh builds its oracle with the makefile's flag set for
        // exactly this reason -- pinning only the -O level is not enough.
        if self.freq(p) == 0 {
            let mut i = 0u32;
            loop {
                i += 1;
                p -= 6;
                if self.freq(p) != 0 {
                    break;
                }
            }
            esc_freq = esc_freq.wrapping_add(i);
            let old_nu = ((self.ns(c) as usize) + 2) >> 1;
            let new_ns = self.ns(c) as u32 - i;
            self.set_ns(c, new_ns as u8);
            if new_ns == 0 {
                let mut tmp = [0u8; 6];
                for k in 0..6 {
                    tmp[k] = self.rd8(s_base + k);
                }
                let mut tf = (2 * tmp[1] as u32).wrapping_add(esc_freq).wrapping_sub(1) / esc_freq;
                if tf > MAX_FREQ / 3 {
                    tf = MAX_FREQ / 3;
                }
                tmp[1] = tf as u8;
                self.sa.free_units(s_base, old_nu);
                let os = Self::one_state(c);
                for k in 0..6 {
                    self.wr8(os + k, tmp[k]);
                }
                let fl = (self.flags(c) & 0x10) + 0x08 * (tmp[0] >= 0x40) as u8;
                self.set_flags(c, fl);
                self.found_state = os;
                return;
            }
            let nn = ((new_ns as usize) + 2) >> 1;
            let nb = self.sa.shrink_units(s_base, old_nu, nn);
            self.set_stats(c, Self::rref(nb));
            let fl = self.flags(c) & !0x08;
            self.set_flags(c, fl);
            let mut i = new_ns;
            let mut p = nb;
            let fl = self.flags(c) | 0x08 * (self.sym(p) >= 0x40) as u8;
            self.set_flags(c, fl);
            while i > 0 {
                p += 6;
                let fl = self.flags(c) | 0x08 * (self.sym(p) >= 0x40) as u8;
                self.set_flags(c, fl);
                i -= 1;
            }
            let esc = esc_freq - (esc_freq >> 1);
            let sf = self.summ_freq(c) + esc;
            self.set_summ_freq(c, sf);
            let fl = self.flags(c) | 0x04;
            self.set_flags(c, fl);
            self.found_state = nb;
            return;
        }
        let esc = esc_freq - (esc_freq >> 1);
        let sf = self.summ_freq(c) + esc;
        self.set_summ_freq(c, sf);
        let fl = self.flags(c) | 0x04;
        self.set_flags(c, fl);
        self.found_state = s_base;
    }

    /// `CreateSuccessors`. Returns 0 for NULL.
    fn create_successors(&mut self, skip: bool, mut p: usize, pc_in: usize) -> usize {
        let mut pc = pc_in;
        let up_branch = self.succ(self.found_state);
        let mut ps = [0usize; MAX_O];
        let mut n = 0usize;
        let sym = self.sym(self.found_state);

        let mut no_loop = false;
        if !skip {
            ps[n] = self.found_state;
            n += 1;
            if self.suffix(pc) == 0 {
                no_loop = true;
            }
        }
        if !no_loop {
            let mut entered = p != 0;
            if entered {
                pc = Self::roff(self.suffix(pc));
            }
            loop {
                if !entered {
                    pc = Self::roff(self.suffix(pc));
                    if self.ns(pc) != 0 {
                        p = Self::roff(self.stats(pc));
                        if self.sym(p) != sym {
                            loop {
                                let t = self.sym(p + 6);
                                p += 6;
                                if t == sym {
                                    break;
                                }
                            }
                        }
                        let tmp = (self.freq(p) < MAX_FREQ - 9) as u32;
                        let f = self.freq(p) + tmp;
                        self.set_freq(p, f);
                        let sf = self.summ_freq(pc) + tmp;
                        self.set_summ_freq(pc, sf);
                    } else {
                        p = Self::one_state(pc);
                        let suf = Self::roff(self.suffix(pc));
                        let inc = ((self.ns(suf) == 0) as u32) & ((self.freq(p) < 24) as u32);
                        let f = self.freq(p) + inc;
                        self.set_freq(p, f);
                    }
                }
                entered = false;
                // LOOP_ENTRY
                if self.succ(p) != up_branch {
                    pc = Self::roff(self.succ(p));
                    break;
                }
                ps[n] = p;
                n += 1;
                if self.suffix(pc) == 0 {
                    break;
                }
            }
        }
        // NO_LOOP
        if n == 0 {
            return pc;
        }
        // Build the template context in a local 12-byte buffer, as the C does
        // with its stack `PPM_CONTEXT ct`.
        let mut ct = [0u8; 12];
        ct[0] = 0; // NumStats
        let mut fl = 0x10 * (sym >= 0x40) as u8;
        // ct.oneState() lives at offset 2 of the buffer, the SummFreq union.
        let text_sym = self.rd8(Self::roff(up_branch));
        ct[2] = text_sym;
        ct[4..8].copy_from_slice(&(up_branch + 1).to_le_bytes());
        fl |= 0x08 * (text_sym >= 0x40) as u8;
        ct[1] = fl;

        let one_freq;
        if self.ns(pc) != 0 {
            let mut q = Self::roff(self.stats(pc));
            if self.sym(q) != text_sym {
                loop {
                    let t = self.sym(q + 6);
                    q += 6;
                    if t == text_sym {
                        break;
                    }
                }
            }
            let cf = self.freq(q) - 1;
            let s0 = self.summ_freq(pc) - self.ns(pc) as u32 - cf;
            one_freq = 1 + if 2 * cf <= s0 {
                (5 * cf > s0) as u32
            } else {
                (cf + 2 * s0 - 3) / s0
            };
        } else {
            one_freq = self.freq(Self::one_state(pc));
        }
        ct[3] = one_freq as u8;

        loop {
            let pc1 = self.sa.alloc_context();
            if pc1 == 0 {
                return 0;
            }
            for k in 0..12 {
                self.wr8(pc1 + k, ct[k]);
            }
            self.set_suffix(pc1, Self::rref(pc));
            pc = pc1;
            n -= 1;
            self.set_succ(ps[n], Self::rref(pc));
            if n == 0 {
                break;
            }
        }
        pc
    }

    /// `UpdateModel`.
    fn update_model(&mut self, min_context: usize) {
        let mut p = 0usize;
        let mut pc1 = self.max_context;
        let f_freq = self.freq(self.found_state);
        let f_symbol = self.sym(self.found_state);
        let mut f_successor = Self::roff(self.succ(self.found_state));
        let had_successor = self.succ(self.found_state) != 0;
        let pc = Self::roff(self.suffix(min_context));

        if f_freq < MAX_FREQ / 4 && self.suffix(min_context) != 0 {
            if self.ns(pc) != 0 {
                p = Self::roff(self.stats(pc));
                if self.sym(p) != f_symbol {
                    loop {
                        let s = self.sym(p + 6);
                        p += 6;
                        if s == f_symbol {
                            break;
                        }
                    }
                    if self.freq(p) >= self.freq(p - 6) {
                        self.swap_states(p, p - 6);
                        p -= 6;
                    }
                }
                let cf = 2 * (self.freq(p) < MAX_FREQ - 9) as u32;
                let f = self.freq(p) + cf;
                self.set_freq(p, f);
                let sf = self.summ_freq(pc) + cf;
                self.set_summ_freq(pc, sf);
            } else {
                p = Self::one_state(pc);
                let f = self.freq(p) + (self.freq(p) < 32) as u32;
                self.set_freq(p, f);
            }
        }

        if self.order_fall == 0 && had_successor {
            let r = self.create_successors(true, p, min_context);
            if r == 0 {
                self.restore_model_rare(pc1, min_context, f_successor);
                return;
            }
            self.set_succ(self.found_state, Self::rref(r));
            self.max_context = r;
            return;
        }

        self.wr8(self.sa.p_text, f_symbol);
        self.sa.p_text += 1;
        let mut successor = self.sa.p_text;
        if self.sa.p_text >= self.sa.units_start {
            self.restore_model_rare(pc1, min_context, f_successor);
            return;
        }

        if had_successor {
            if f_successor < self.sa.units_start {
                f_successor = self.create_successors(false, p, min_context);
            }
        } else {
            f_successor = self.reduce_order(p, min_context);
        }
        if f_successor == 0 {
            self.restore_model_rare(pc1, min_context, f_successor);
            return;
        }
        self.order_fall -= 1;
        if self.order_fall == 0 {
            successor = f_successor;
            if self.max_context != min_context {
                self.sa.p_text -= 1;
            }
        } else if self.mr_method > MRM_FREEZE {
            successor = f_successor;
            self.sa.p_text = 0;
            self.order_fall = 0;
        }

        let ns = self.ns(min_context) as u32;
        let s0 = self.summ_freq(min_context) - ns - f_freq;
        let flag = 0x08 * (f_symbol >= 0x40) as u8;
        while pc1 != min_context {
            let ns1 = self.ns(pc1) as u32;
            if ns1 != 0 {
                if (ns1 & 1) != 0 {
                    let np = self
                        .sa
                        .expand_units(Self::roff(self.stats(pc1)), ((ns1 + 1) >> 1) as usize);
                    if np == 0 {
                        self.restore_model_rare(pc1, min_context, f_successor);
                        return;
                    }
                    self.set_stats(pc1, Self::rref(np));
                }
                let sf = self.summ_freq(pc1) + (3 * ns1 + 1 < ns) as u32;
                self.set_summ_freq(pc1, sf);
            } else {
                let np = self.sa.alloc_units(1);
                if np == 0 {
                    self.restore_model_rare(pc1, min_context, f_successor);
                    return;
                }
                let os = Self::one_state(pc1);
                self.state_cpy(np, os);
                self.set_stats(pc1, Self::rref(np));
                let f = self.freq(np);
                if f < MAX_FREQ / 4 - 1 {
                    self.set_freq(np, f + f);
                } else {
                    self.set_freq(np, MAX_FREQ - 4);
                }
                let sf = self.freq(np) + self.init_esc + (ns > 2) as u32;
                self.set_summ_freq(pc1, sf);
            }
            let mut cf = 2 * f_freq * (self.summ_freq(pc1) + 6);
            let sf2 = s0 + self.summ_freq(pc1);
            if cf < 6 * sf2 {
                cf = 1 + (cf > sf2) as u32 + (cf >= 4 * sf2) as u32;
                let s = self.summ_freq(pc1) + 4;
                self.set_summ_freq(pc1, s);
            } else {
                cf = 4 + (cf > 9 * sf2) as u32
                    + (cf > 12 * sf2) as u32
                    + (cf > 15 * sf2) as u32;
                let s = self.summ_freq(pc1) + cf;
                self.set_summ_freq(pc1, s);
            }
            let new_ns = self.ns(pc1) + 1;
            self.set_ns(pc1, new_ns);
            let np = Self::roff(self.stats(pc1)) + new_ns as usize * 6;
            self.set_succ(np, Self::rref(successor));
            self.set_sym(np, f_symbol);
            self.set_freq(np, cf);
            let fl = self.flags(pc1) | flag;
            self.set_flags(pc1, fl);

            pc1 = Self::roff(self.suffix(pc1));
        }
        self.max_context = f_successor;
    }
}

// --- symbol coding --------------------------------------------------------

impl Model {
    /// The binary-context SEE slot. `RunLength >> 26` is an ARITHMETIC shift of
    /// a signed int in the C, so a negative run length sets bit 5 -- using a
    /// logical shift here would pick the wrong half of the table.
    #[inline]
    fn bin_summ_idx(&self, c: usize) -> (usize, usize) {
        let rs = Self::one_state(c);
        let suf = Self::roff(self.suffix(c));
        let indx = self.ns2bs_indx[self.ns(suf) as usize] as usize
            + self.prev_success as usize
            + self.flags(c) as usize;
        let row = self.q_table[(self.freq(rs) - 1) as usize] as usize;
        let col = indx + ((self.run_length >> 26) & 0x20) as usize;
        (row, col)
    }

    /// `PPM_CONTEXT::encodeBinSymbol`.
    fn encode_bin_symbol(&mut self, c: usize, symbol: i32) {
        let (row, col) = self.bin_summ_idx(c);
        let rs = Self::one_state(c);
        let bs = self.bin_summ[row][col] as u32;
        if self.sym(rs) as i32 == symbol {
            self.found_state = rs;
            let f = self.freq(rs) + (self.freq(rs) < 196) as u32;
            self.set_freq(rs, f);
            self.rc.low_count = 0;
            self.rc.high_count = bs;
            self.bin_summ[row][col] =
                (bs + INTERVAL - get_mean(bs, PERIOD_BITS, 2)) as u16;
            self.prev_success = 1;
            self.run_length += 1;
        } else {
            self.rc.low_count = bs;
            let nb = bs - get_mean(bs, PERIOD_BITS, 2);
            self.bin_summ[row][col] = nb as u16;
            self.rc.high_count = BIN_SCALE;
            self.init_esc = EXP_ESCAPE[(nb >> 10) as usize] as u32;
            let s = self.sym(rs) as usize;
            self.char_mask[s] = self.esc_count;
            self.num_masked = 0;
            self.prev_success = 0;
            self.found_state = 0;
        }
    }

    /// `PPM_CONTEXT::decodeBinSymbol`.
    fn decode_bin_symbol(&mut self, c: usize) {
        let (row, col) = self.bin_summ_idx(c);
        let rs = Self::one_state(c);
        let bs = self.bin_summ[row][col] as u32;
        if self.rc.get_current_shift_count(TOT_BITS) < bs {
            self.found_state = rs;
            let f = self.freq(rs) + (self.freq(rs) < 196) as u32;
            self.set_freq(rs, f);
            self.rc.low_count = 0;
            self.rc.high_count = bs;
            self.bin_summ[row][col] =
                (bs + INTERVAL - get_mean(bs, PERIOD_BITS, 2)) as u16;
            self.prev_success = 1;
            self.run_length += 1;
        } else {
            self.rc.low_count = bs;
            let nb = bs - get_mean(bs, PERIOD_BITS, 2);
            self.bin_summ[row][col] = nb as u16;
            self.rc.high_count = BIN_SCALE;
            self.init_esc = EXP_ESCAPE[(nb >> 10) as usize] as u32;
            let s = self.sym(rs) as usize;
            self.char_mask[s] = self.esc_count;
            self.num_masked = 0;
            self.prev_success = 0;
            self.found_state = 0;
        }
    }

    /// `PPM_CONTEXT::update1`.
    fn update1(&mut self, c: usize, mut p: usize) {
        self.found_state = p;
        let f = self.freq(p) + 4;
        self.set_freq(p, f);
        let sf = self.summ_freq(c) + 4;
        self.set_summ_freq(c, sf);
        if self.freq(p) > self.freq(p - 6) {
            self.swap_states(p, p - 6);
            p -= 6;
            self.found_state = p;
            if self.freq(p) > MAX_FREQ {
                self.rescale(c);
            }
        }
    }

    /// `PPM_CONTEXT::encodeSymbol1`.
    fn encode_symbol1(&mut self, c: usize, symbol: i32) {
        let mut p = Self::roff(self.stats(c));
        self.rc.scale = self.summ_freq(c);
        if self.sym(p) as i32 == symbol {
            self.rc.high_count = self.freq(p);
            self.prev_success = (2 * self.rc.high_count >= self.rc.scale) as u32;
            self.found_state = p;
            let f = self.freq(p) + 4;
            self.set_freq(p, f);
            let sf = self.summ_freq(c) + 4;
            self.set_summ_freq(c, sf);
            self.run_length += self.prev_success as i32;
            if self.freq(p) > MAX_FREQ {
                self.rescale(c);
            }
            self.rc.low_count = 0;
            return;
        }
        let mut lo_cnt = self.freq(p);
        let mut i = self.ns(c) as i32;
        self.prev_success = 0;
        loop {
            p += 6;
            if self.sym(p) as i32 == symbol {
                break;
            }
            lo_cnt += self.freq(p);
            i -= 1;
            if i == 0 {
                self.rc.low_count = lo_cnt;
                let s = self.sym(p) as usize;
                self.char_mask[s] = self.esc_count;
                let mut j = self.ns(c) as u32;
                self.num_masked = j;
                self.found_state = 0;
                while j > 0 {
                    p -= 6;
                    let s = self.sym(p) as usize;
                    self.char_mask[s] = self.esc_count;
                    j -= 1;
                }
                self.rc.high_count = self.rc.scale;
                return;
            }
        }
        self.rc.low_count = lo_cnt;
        self.rc.high_count = lo_cnt + self.freq(p);
        self.update1(c, p);
    }

    /// `PPM_CONTEXT::decodeSymbol1`.
    fn decode_symbol1(&mut self, c: usize) {
        let mut p = Self::roff(self.stats(c));
        self.rc.scale = self.summ_freq(c);
        let mut hi_cnt = self.freq(p);
        let count = self.rc.get_current_count();
        if count < hi_cnt {
            self.rc.high_count = hi_cnt;
            self.prev_success = (2 * hi_cnt >= self.rc.scale) as u32;
            self.found_state = p;
            hi_cnt += 4;
            self.set_freq(p, hi_cnt);
            let sf = self.summ_freq(c) + 4;
            self.set_summ_freq(c, sf);
            self.run_length += self.prev_success as i32;
            if hi_cnt > MAX_FREQ {
                self.rescale(c);
            }
            self.rc.low_count = 0;
            return;
        }
        let mut i = self.ns(c) as i32;
        self.prev_success = 0;
        loop {
            p += 6;
            hi_cnt += self.freq(p);
            if hi_cnt > count {
                break;
            }
            i -= 1;
            if i == 0 {
                self.rc.low_count = hi_cnt;
                let s = self.sym(p) as usize;
                self.char_mask[s] = self.esc_count;
                let mut j = self.ns(c) as u32;
                self.num_masked = j;
                self.found_state = 0;
                while j > 0 {
                    p -= 6;
                    let s = self.sym(p) as usize;
                    self.char_mask[s] = self.esc_count;
                    j -= 1;
                }
                self.rc.high_count = self.rc.scale;
                return;
            }
        }
        self.rc.high_count = hi_cnt;
        self.rc.low_count = hi_cnt - self.freq(p);
        self.update1(c, p);
    }

    /// `PPM_CONTEXT::update2`.
    fn update2(&mut self, c: usize, p: usize) {
        self.found_state = p;
        let f = self.freq(p) + 4;
        self.set_freq(p, f);
        let sf = self.summ_freq(c) + 4;
        self.set_summ_freq(c, sf);
        if self.freq(p) > MAX_FREQ {
            self.rescale(c);
        }
        self.esc_count = self.esc_count.wrapping_add(1);
        self.run_length = self.init_rl;
    }

    /// `PPM_CONTEXT::makeEscFreq2`. Returns the SEE slot as (row, col), or
    /// `None` for the dummy context used when NumStats is 0xFF.
    fn make_esc_freq2(&mut self, c: usize) -> Option<(usize, usize)> {
        let ns = self.ns(c) as u32;
        if ns != 0xFF {
            let t = self.ns(Self::roff(self.suffix(c))) as u32;
            let row = self.q_table[(ns + 2) as usize] as usize - 3;
            let mut col = (self.summ_freq(c) > 11 * (ns + 1)) as usize;
            col += 2 * ((2 * ns < t + self.num_masked) as usize) + self.flags(c) as usize;
            self.rc.scale = self.see2cont[row][col].get_mean();
            Some((row, col))
        } else {
            self.rc.scale = 1;
            None
        }
    }

    #[inline]
    fn see_summ_add(&mut self, slot: Option<(usize, usize)>, v: u32) {
        match slot {
            Some((r, c)) => {
                self.see2cont[r][c].summ = self.see2cont[r][c].summ.wrapping_add(v as u16)
            }
            None => {
                self.dummy_see2cont.summ = self.dummy_see2cont.summ.wrapping_add(v as u16)
            }
        }
    }

    #[inline]
    fn see_update(&mut self, slot: Option<(usize, usize)>) {
        match slot {
            Some((r, c)) => self.see2cont[r][c].update(),
            None => self.dummy_see2cont.update(),
        }
    }

    /// `PPM_CONTEXT::encodeSymbol2`.
    fn encode_symbol2(&mut self, c: usize, symbol: i32) {
        let slot = self.make_esc_freq2(c);
        let mut lo_cnt = 0u32;
        let mut i = self.ns(c) as i32 - self.num_masked as i32;
        let mut p = Self::roff(self.stats(c)) - 6;
        loop {
            loop {
                p += 6;
                let s = self.sym(p) as usize;
                if self.char_mask[s] != self.esc_count {
                    break;
                }
            }
            let s = self.sym(p) as usize;
            self.char_mask[s] = self.esc_count;
            if s as i32 == symbol {
                // SYMBOL_FOUND
                self.rc.low_count = lo_cnt;
                lo_cnt += self.freq(p);
                self.rc.high_count = lo_cnt;
                let mut p1 = p;
                i -= 1;
                while i != 0 {
                    loop {
                        p1 += 6;
                        let s1 = self.sym(p1) as usize;
                        if self.char_mask[s1] != self.esc_count {
                            break;
                        }
                    }
                    lo_cnt += self.freq(p1);
                    i -= 1;
                }
                self.rc.scale += lo_cnt;
                self.see_update(slot);
                self.update2(c, p);
                return;
            }
            lo_cnt += self.freq(p);
            i -= 1;
            if i == 0 {
                break;
            }
        }
        self.rc.low_count = lo_cnt;
        self.rc.scale += lo_cnt;
        self.rc.high_count = self.rc.scale;
        let sc = self.rc.scale;
        self.see_summ_add(slot, sc);
        self.num_masked = self.ns(c) as u32;
    }

    /// `PPM_CONTEXT::decodeSymbol2`.
    fn decode_symbol2(&mut self, c: usize) {
        let slot = self.make_esc_freq2(c);
        let mut hi_cnt = 0u32;
        let n = self.ns(c) as i32 - self.num_masked as i32;
        let mut i = n;
        let mut ps = [0usize; 256];
        let mut np = 0usize;
        let mut p = Self::roff(self.stats(c)) - 6;
        loop {
            loop {
                p += 6;
                let s = self.sym(p) as usize;
                if self.char_mask[s] != self.esc_count {
                    break;
                }
            }
            hi_cnt += self.freq(p);
            ps[np] = p;
            np += 1;
            i -= 1;
            if i == 0 {
                break;
            }
        }
        self.rc.scale += hi_cnt;
        let count = self.rc.get_current_count();
        if count < hi_cnt {
            let mut k = 0usize;
            let mut acc = 0u32;
            let mut q = ps[0];
            loop {
                acc += self.freq(q);
                if acc > count {
                    break;
                }
                k += 1;
                q = ps[k];
            }
            self.rc.high_count = acc;
            self.rc.low_count = acc - self.freq(q);
            self.see_update(slot);
            self.update2(c, q);
        } else {
            self.rc.low_count = hi_cnt;
            self.rc.high_count = self.rc.scale;
            self.num_masked = self.ns(c) as u32;
            for k in 0..n as usize {
                let s = self.sym(ps[k]) as usize;
                self.char_mask[s] = self.esc_count;
            }
            let sc = self.rc.scale;
            self.see_summ_add(slot, sc);
        }
    }

    /// `ClearMask`.
    fn clear_mask(&mut self) {
        self.esc_count = 1;
        self.char_mask = [0; 256];
        self.print_count = self.print_count.wrapping_add(1);
    }

    /// `EncodeFile`.
    pub fn encode_file(
        &mut self,
        encoded: &mut PrimeStream,
        decoded: &mut PrimeStream,
        max_order: i32,
        mr_method: i32,
    ) {
        self.rc.init_encoder();
        self.start_model_rare(max_order, mr_method);
        loop {
            let mut min_context = self.max_context;
            let ns = self.ns(min_context);
            let c = decoded.get();
            if ns != 0 {
                self.encode_symbol1(min_context, c);
                self.rc.encode_symbol();
            } else {
                self.encode_bin_symbol(min_context, c);
                self.rc.shift_encode_symbol(TOT_BITS);
            }
            let mut stop = false;
            while self.found_state == 0 {
                self.rc.encode_normalize(encoded);
                loop {
                    self.order_fall += 1;
                    if self.suffix(min_context) == 0 {
                        stop = true;
                        break;
                    }
                    min_context = Self::roff(self.suffix(min_context));
                    if self.ns(min_context) as u32 != self.num_masked {
                        break;
                    }
                }
                if stop {
                    break;
                }
                self.encode_symbol2(min_context, c);
                self.rc.encode_symbol();
            }
            if stop {
                break;
            }
            if self.order_fall == 0 && self.ge_units(self.succ(self.found_state)) {
                self.max_context = Self::roff(self.succ(self.found_state));
            } else {
                self.update_model(min_context);
                if self.esc_count == 0 {
                    self.clear_mask();
                }
            }
            self.rc.encode_normalize(encoded);
            if decoded.error() < 0 || encoded.error() < 0 {
                return;
            }
        }
        self.rc.flush_encoder(encoded);
    }

    /// `DecodeFile`.
    pub fn decode_file(
        &mut self,
        decoded: &mut PrimeStream,
        encoded: &mut PrimeStream,
        max_order: i32,
        mr_method: i32,
    ) {
        self.rc.init_decoder(encoded);
        self.start_model_rare(max_order, mr_method);
        let mut min_context = self.max_context;
        let mut ns = self.ns(min_context);
        loop {
            if ns != 0 {
                self.decode_symbol1(min_context);
            } else {
                self.decode_bin_symbol(min_context);
            }
            self.rc.remove_subrange();
            let mut stop = false;
            while self.found_state == 0 {
                self.rc.decode_normalize(encoded);
                loop {
                    self.order_fall += 1;
                    if self.suffix(min_context) == 0 {
                        stop = true;
                        break;
                    }
                    min_context = Self::roff(self.suffix(min_context));
                    if self.ns(min_context) as u32 != self.num_masked {
                        break;
                    }
                }
                if stop {
                    break;
                }
                self.decode_symbol2(min_context);
                self.rc.remove_subrange();
            }
            if stop {
                break;
            }
            let sym = self.sym(self.found_state);
            decoded.put(sym);
            if self.order_fall == 0 && self.ge_units(self.succ(self.found_state)) {
                self.max_context = Self::roff(self.succ(self.found_state));
            } else {
                self.update_model(min_context);
                if self.esc_count == 0 {
                    self.clear_mask();
                }
            }
            min_context = self.max_context;
            ns = self.ns(min_context);
            self.rc.decode_normalize(encoded);
            if decoded.error() < 0 || encoded.error() < 0 {
                return;
            }
        }
    }
}
