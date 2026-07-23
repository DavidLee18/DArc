//! The shared arithmetic decoder behind GRZip's two entropy coders, ported from
//! `Compression/GRZip/MTF_Ari.c` (`GRZip_MTF_Ari_Decode` :404) and
//! `WFC_Ari.c` (`GRZip_WFC_Ari_Decode` :498).
//!
//! Those two C files are near-duplicates: identical range coder, identical
//! model stack, identical run-length ladder. They differ in exactly one step --
//! how a decoded rank becomes a character. MTF uses a move-to-front list; WFC
//! uses a weighted-frequency-count list. That step is the `SymbolList` trait
//! here, so the ~150 lines around it are transcribed once rather than twice.
//!
//! This is the entropy stage that follows the BWT or ST4. It decodes a
//! move-to-front rank, then a run length for that symbol, both through a binary
//! arithmetic coder driven by a stack of adaptive context models.
//!
//! ## How a symbol is spelled
//!
//! 1. **Rank 0-3** from a quaternary model that mixes three tables: a global
//!    one, one keyed on the previous character, and one keyed on the recent
//!    rank and run-length history.
//! 2. If the rank is 3, the real rank is **escaped**: a unary group number
//!    (0-6) then a binary position within that group, which together index
//!    `GRNUM_TO_GRBEGIN`. Group 6 with position 127 is the **end marker** --
//!    the only exit from the loop.
//! 3. The rank drives a **move-to-front list**, giving the character.
//! 4. A **log2 run length**, again unary, then that many bits of the run's
//!    remainder, offset by `LOG2_RLE_SIZE`.
//!
//! Every model is a frequency in `[0, MODEL_MAX_FREQ]` updated by shifting
//! toward or away from the maximum -- `x += (MAX-x)>>k` on a zero bit,
//! `x -= x>>k` on a one. The shift constants differ per model and are what tune
//! adaptation speed; they must match exactly or the two sides diverge.
//!
//! ## Bounds
//!
//! The C gained several bounds in an earlier hardening pass, all reproduced
//! here: an over-read counter so a truncated block stops instead of spinning on
//! zero bytes, a rank-search limit so a corrupt frequency cannot walk past the
//! five-entry models, a `Log2RunSize` ceiling, and a run length checked against
//! the remaining output. The one exit is the end marker, so without them a
//! corrupt block never terminates.

use super::{GrzError, GRZ_UNEXPECTED_EOF};

pub const MAX_BYTE: usize = 256;
pub const RANGE_TOP: u32 = 1 << 24;
pub const MODEL_NUM_BITS: u32 = 11;
pub const MODEL_MAX_FREQ: u32 = 1 << MODEL_NUM_BITS;

const M_LOG2RLE_SHIFT_0: u32 = 6;
const M_LOG2RLE_SHIFT_1: u32 = 3;
const M_LOG2RLE_SHIFT_2: u32 = 6;
const M_L1_SHIFT_0: u32 = 4;
const M_L1_SHIFT_1: u32 = 6;
const M_L2_SHIFT: u32 = 7;

const MODEL_L0_0_MAX_FREQ: u32 = 58;
const MODEL_L0_1_MAX_FREQ: u32 = 62;
const MODEL_L0_2_MAX_FREQ: u32 = 204;

/// `GRZ_Log2MaxBlockSize` (libGRZip.h:55).
pub const LOG2_MAX_BLOCK: usize = 23;

/// `WFCMTF_GrNum2GrBegin` (WFC_MTF.h:90).
pub const GRNUM_TO_GRBEGIN: [u32; 7] = [3, 5, 9, 17, 33, 65, 129];

/// `WFCMTF_Log2RLESize` (WFC_MTF.h:92) -- the base run length for each log2
/// bucket.
pub const LOG2_RLE_SIZE: [u32; LOG2_MAX_BLOCK + 1] = [
    1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072,
    262144, 524288, 1048576, 2097152, 4194304, 8388608,
];

/// How far past the compressed data the decoder may read before giving up.
/// The C counts the same and checks it at the top of the symbol loop.
pub const MAX_OVERREAD: u32 = 64;

struct Rc<'a> {
    input: &'a [u8],
    pos: usize,
    code: u32,
    range: u32,
    overread: u32,
}

impl<'a> Rc<'a> {
    /// `ARI_InTgtByte`: past the end, yield zero and count it.
    #[inline]
    fn byte(&mut self) -> u32 {
        if self.pos < self.input.len() {
            let b = self.input[self.pos] as u32;
            self.pos += 1;
            b
        } else {
            self.overread += 1;
            0
        }
    }

    fn new(input: &'a [u8]) -> Self {
        let mut rc = Rc { input, pos: 0, code: 0, range: u32::MAX, overread: 0 };
        // Five bytes, four of them followed by a shift.
        for _ in 0..4 {
            rc.code |= rc.byte();
            rc.code <<= 8;
        }
        rc.code |= rc.byte();
        rc
    }

    /// `ARI_GetFreq`: divides `range` in place, then scales the code by it.
    #[inline]
    fn get_freq(&mut self, tot: u32) -> u32 {
        self.range /= tot.max(1);
        if self.range == 0 {
            0
        } else {
            self.code / self.range
        }
    }

    /// `ARI_Decode`. The `TotFreq` argument the C takes is assigned and never
    /// used -- the division already happened in `get_freq` -- so it is absent.
    #[inline]
    fn decode(&mut self, freq: u32, cum: u32) {
        self.code = self.code.wrapping_sub(cum.wrapping_mul(self.range));
        self.range = self.range.wrapping_mul(freq);
        while self.range < RANGE_TOP {
            let b = self.byte();
            self.code = (self.code << 8) | b;
            self.range <<= 8;
        }
    }

    #[inline]
    fn decode_0(&mut self, f: u32) {
        self.decode(f, 0);
    }

    #[inline]
    fn decode_1(&mut self, f: u32) {
        self.decode(MODEL_MAX_FREQ - f, f);
    }
}

#[inline]
fn up0(v: &mut u32, k: u32) {
    *v += (MODEL_MAX_FREQ - *v) >> k;
}

#[inline]
fn up1(v: &mut u32, k: u32) {
    *v -= *v >> k;
}

/// The one step that differs between the two coders: turn a rank into a
/// character, updating whatever list discipline the coder maintains.
pub trait SymbolList {
    fn pick(&mut self, rank: usize) -> u8;
}

/// The decoder body. `out_size` is the capacity of `out`; the return is how
/// many bytes were written.
pub fn decode<L: SymbolList>(
    input: &[u8],
    out: &mut [u8],
    out_size: usize,
    list: &mut L,
) -> Result<usize, GrzError> {
    let cap = out_size.min(out.len());
    let mut rc = Rc::new(input);

    let mut m_l0_0 = [1u32, 1, 1, 1, 4];
    let mut m_l0_1 = vec![0u32; MAX_BYTE * 5];
    let mut m_l0_2 = vec![0u32; 4 * MAX_BYTE * 5];
    let half = MODEL_MAX_FREQ >> 1;
    let mut m_l1_0 = [half; 8];
    // The C initialises only rows 0..6 (`for (i=0;i<7;i++)`), leaving row 7 as
    // uninitialised stack. CtxL1 is a group number in 0..6, so row 7 is never
    // read; initialising it too is unreachable rather than divergent.
    let mut m_l1_1 = [[half; 8]; 8];
    let mut m_l2_0 = [[half; 128]; 8];
    let mut m_rle_0 = [[half; LOG2_MAX_BLOCK + 1]; 64];
    let mut m_rle_2 = [[half; LOG2_MAX_BLOCK + 1]; LOG2_MAX_BLOCK + 1];
    let mut m_rle_1 = vec![half; MAX_BYTE * (LOG2_MAX_BLOCK + 1)];

    let mut ctx_rle: usize = 0;
    let mut ctx_l0: usize = 0;
    let mut ctx_l1: usize = 0;
    let mut ch: usize = 0;
    let mut op = 0usize;

    loop {
        // The only exit is the end marker, so a corrupt stream would otherwise
        // spin forever on zero bytes.
        if rc.overread > MAX_OVERREAD {
            return Err(GRZ_UNEXPECTED_EOF);
        }

        let pred = ch;
        let v = pred * 5; // Model_L0_1[PredChar]
        let u = (4 * ctx_l0 + (ctx_rle & 3)) * 5; // Model_L0_2[...]

        let total = m_l0_0[4] + m_l0_2[u + 4] + m_l0_1[v + 4];
        let frq = rc.get_freq(total);
        // Bounded at 4: a corrupt frequency could otherwise walk past the
        // five-entry models. A valid stream always stops by rank 4.
        let mut cum = 0u32;
        let mut rank = 0usize;
        while frq >= cum && rank < 4 {
            cum += m_l0_0[rank] + m_l0_2[u + rank] + m_l0_1[v + rank];
            rank += 1;
        }
        rank -= 1;
        cum -= m_l0_0[rank] + m_l0_2[u + rank] + m_l0_1[v + rank];

        rc.decode(m_l0_0[rank] + m_l0_2[u + rank] + m_l0_1[v + rank], cum);

        m_l0_0[rank] += 2;
        m_l0_2[u + rank] += 2;
        m_l0_1[v + rank] += 2;
        m_l0_0[4] += 2;
        m_l0_2[u + 4] += 2;
        m_l0_1[v + 4] += 2;

        // Update_Model_L0: halve each model that has outgrown its ceiling. Note
        // the global one rounds up, the two context ones do not.
        if m_l0_0[4] > MODEL_L0_0_MAX_FREQ {
            let mut sum = 0;
            for i in 0..4 {
                m_l0_0[i] = (m_l0_0[i] + 1) >> 1;
                sum += m_l0_0[i];
            }
            m_l0_0[4] = sum;
        }
        if m_l0_1[v + 4] > MODEL_L0_1_MAX_FREQ {
            let mut sum = 0;
            for i in 0..4 {
                m_l0_1[v + i] >>= 1;
                sum += m_l0_1[v + i];
            }
            m_l0_1[v + 4] = sum;
        }
        if m_l0_2[u + 4] > MODEL_L0_2_MAX_FREQ {
            let mut sum = 0;
            for i in 0..4 {
                m_l0_2[u + i] >>= 1;
                sum += m_l0_2[u + i];
            }
            m_l0_2[u + 4] = sum;
        }

        let mut wrank = rank;
        if rank == 3 {
            // Escape: a unary group number, then a binary position inside it.
            let mut grnum = 0usize;
            let mut grpos = 0u32;
            while grnum != 6 {
                let p = (m_l1_0[grnum] + m_l1_1[ctx_l1][grnum]) >> 1;
                if rc.get_freq(MODEL_MAX_FREQ) < p {
                    rc.decode_0(p);
                    up0(&mut m_l1_0[grnum], M_L1_SHIFT_0);
                    up0(&mut m_l1_1[ctx_l1][grnum], M_L1_SHIFT_1);
                    break;
                }
                rc.decode_1(p);
                up1(&mut m_l1_0[grnum], M_L1_SHIFT_0);
                up1(&mut m_l1_1[ctx_l1][grnum], M_L1_SHIFT_1);
                grnum += 1;
            }
            ctx_l1 = grnum;

            let mut ctx_l2 = 1usize;
            for _ in 0..=grnum {
                let p = m_l2_0[grnum][ctx_l2];
                if rc.get_freq(MODEL_MAX_FREQ) < p {
                    rc.decode_0(p);
                    up0(&mut m_l2_0[grnum][ctx_l2], M_L2_SHIFT);
                    ctx_l2 <<= 1;
                    grpos <<= 1;
                } else {
                    rc.decode_1(p);
                    up1(&mut m_l2_0[grnum][ctx_l2], M_L2_SHIFT);
                    ctx_l2 = (ctx_l2 << 1) | 1;
                    grpos = (grpos << 1) | 1;
                }
            }
            // The end marker, and the loop's only exit.
            if grnum == 6 && grpos == 127 {
                break;
            }
            wrank = (GRNUM_TO_GRBEGIN[grnum] + grpos) as usize;
        }

        wrank = (wrank + 1) & 0xFF;
        ch = list.pick(wrank) as usize;
        wrank = wrank.wrapping_sub(1) & 0xFF;

        let clamped = wrank.min(3);
        ctx_l0 = ((ctx_l0 << 2) | clamped) & 0xFF;

        // Log2 of the run length, unary.
        let rle0_row = ctx_rle + 16 * clamped;
        let u1 = ch * (LOG2_MAX_BLOCK + 1);
        let mut log2_run = 0usize;
        loop {
            let p = (m_rle_0[rle0_row][log2_run] + m_rle_1[u1 + log2_run]) >> 1;
            if rc.get_freq(MODEL_MAX_FREQ) < p {
                rc.decode_0(p);
                up0(&mut m_rle_0[rle0_row][log2_run], M_LOG2RLE_SHIFT_0);
                up0(&mut m_rle_1[u1 + log2_run], M_LOG2RLE_SHIFT_1);
                break;
            }
            rc.decode_1(p);
            up1(&mut m_rle_0[rle0_row][log2_run], M_LOG2RLE_SHIFT_0);
            up1(&mut m_rle_1[u1 + log2_run], M_LOG2RLE_SHIFT_1);
            log2_run += 1;
            // A run cannot outlast the block, so a valid stream never gets here.
            if log2_run > LOG2_MAX_BLOCK {
                return Err(GRZ_UNEXPECTED_EOF);
            }
        }

        ctx_rle = if log2_run < 2 {
            (ctx_rle << 1) & 0xF
        } else {
            ((ctx_rle << 1) | 1) & 0xF
        };

        // The run's remaining bits, most significant first.
        let mut run: u32 = 0;
        for i in 0..log2_run {
            let p = m_rle_2[log2_run][i];
            if rc.get_freq(MODEL_MAX_FREQ) < p {
                rc.decode_0(p);
                up0(&mut m_rle_2[log2_run][i], M_LOG2RLE_SHIFT_2);
                run <<= 1;
            } else {
                rc.decode_1(p);
                up1(&mut m_rle_2[log2_run][i], M_LOG2RLE_SHIFT_2);
                run = (run << 1) | 1;
            }
        }
        run += LOG2_RLE_SIZE[log2_run];

        if run as usize > cap - op {
            return Err(GRZ_UNEXPECTED_EOF);
        }
        for _ in 0..run {
            out[op] = ch as u8;
            op += 1;
        }
    }

    Ok(op)
}
