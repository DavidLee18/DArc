//! Price tables and the optimal parser (`GetOptimum`) — a port of the dynamic
//! program at the heart of `LzmaEnc.c` (`GetOptimum` `:1219`, `Backward` `:1167`,
//! `ReadMatchDistances` `:1079`, and the `Fill*Prices` / `LenPriceEnc_UpdateTables`
//! machinery). Driven by [`crate::encoder`].
//!
//! ## Bit-exactness hazards (see CLAUDE.md)
//! - Tie-breaks are strict `<` (first candidate wins on equal price).
//! - Prices are rebuilt on the exact C cadence (`matchPriceCount >= 64`,
//!   `repLenEncCounter <= 0`); the optimal parse depends on those exact values.
//! - `opt[]` prices reset to infinity after each call (maintained across calls).

// Price fills mirror the C's explicit index loops.
#![allow(clippy::needless_range_loop)]

use crate::price::ProbPrices;
use crate::state::{
    LEN_NUM_HIGH_SYMBOLS, LEN_NUM_LOW_SYMBOLS, LEN_NUM_SYMBOLS_TOTAL, MATCH_LEN_MIN,
    NUM_ALIGN_BITS, NUM_FULL_DISTANCES, NUM_LEN_TO_POS_STATES, NUM_POS_SLOT_BITS,
    START_POS_MODEL_INDEX,
};

pub const NUM_OPTS: usize = 1 << 11;
pub const INFINITY_PRICE: u32 = 1 << 30;
pub const MARK_LIT: u32 = 0xFFFF_FFFF;
pub const NUM_POS_STATES_MAX: usize = 16;
pub const DIST_TABLE_SIZE_MAX: usize = 64; // kDicLogSizeMax * 2
const ALIGN_TABLE_SIZE: usize = 1 << NUM_ALIGN_BITS;
const END_POS_MODEL_INDEX: u32 = 14;
const NUM_BIT_PRICE_SHIFT_BITS: u32 = 4;

/// `min(len - 2, 3)` — `GetLenToPosState`.
#[inline]
pub fn len_to_pos_state(len: u32) -> usize {
    if len < NUM_LEN_TO_POS_STATES + 1 {
        (len - 2) as usize
    } else {
        (NUM_LEN_TO_POS_STATES - 1) as usize
    }
}

/// Map a 0-based distance to its 6-bit pos slot (`GetPosSlot`).
#[inline]
pub fn get_pos_slot(dist: u32) -> u32 {
    if dist < START_POS_MODEL_INDEX {
        return dist;
    }
    let n = 31 - dist.leading_zeros();
    (n << 1) | ((dist >> (n - 1)) & 1)
}

/// One `opt[]` cell (`COptimal`).
#[derive(Clone, Copy)]
pub struct Optimal {
    pub price: u32,
    pub state: u32,
    pub extra: u32, // 0: normal; 1: LIT:MATCH; >1: MATCH(extra-1):LIT:REP0
    pub len: u32,
    pub dist: u32,
    pub reps: [u32; 4],
}

impl Default for Optimal {
    fn default() -> Self {
        Optimal {
            price: INFINITY_PRICE,
            state: 0,
            extra: 0,
            len: 0,
            dist: 0,
            reps: [0; 4],
        }
    }
}

/// Length-coder price table (`CLenPriceEnc`).
pub struct LenPriceEnc {
    pub table_size: u32,
    pub prices: [[u32; LEN_NUM_SYMBOLS_TOTAL as usize]; NUM_POS_STATES_MAX],
}

impl LenPriceEnc {
    pub fn new() -> Self {
        LenPriceEnc {
            table_size: 0,
            prices: [[0; LEN_NUM_SYMBOLS_TOTAL as usize]; NUM_POS_STATES_MAX],
        }
    }

    #[inline]
    pub fn price(&self, pos_state: usize, len: u32) -> u32 {
        self.prices[pos_state][(len - MATCH_LEN_MIN) as usize]
    }
}

/// All distance/align/length price tables.
pub struct Prices {
    pub pp: ProbPrices,
    pub dist_table_size: u32,
    pub align_prices: [u32; ALIGN_TABLE_SIZE],
    pub pos_slot_prices: [[u32; DIST_TABLE_SIZE_MAX]; NUM_LEN_TO_POS_STATES as usize],
    pub distances_prices: [[u32; NUM_FULL_DISTANCES as usize]; NUM_LEN_TO_POS_STATES as usize],
    pub len_enc: LenPriceEnc,
    pub rep_len_enc: LenPriceEnc,
    pub match_price_count: u32,
    pub rep_len_enc_counter: i32,
}

impl Prices {
    pub fn new(dist_table_size: u32, num_fast_bytes: u32) -> Self {
        let mut len_enc = LenPriceEnc::new();
        let mut rep_len_enc = LenPriceEnc::new();
        len_enc.table_size = num_fast_bytes + 1 - MATCH_LEN_MIN;
        rep_len_enc.table_size = num_fast_bytes + 1 - MATCH_LEN_MIN;
        Prices {
            pp: ProbPrices::new(),
            dist_table_size,
            align_prices: [0; ALIGN_TABLE_SIZE],
            pos_slot_prices: [[0; DIST_TABLE_SIZE_MAX]; NUM_LEN_TO_POS_STATES as usize],
            distances_prices: [[0; NUM_FULL_DISTANCES as usize]; NUM_LEN_TO_POS_STATES as usize],
            len_enc,
            rep_len_enc,
            match_price_count: 0,
            rep_len_enc_counter: REP_LEN_COUNT,
        }
    }
}

pub const REP_LEN_COUNT: i32 = 64;

// ---- literal price helpers (LitEnc_GetPrice / LitEnc_Matched_GetPrice) ----

pub fn lit_get_price(probs: &[u16], sym: u32, pp: &ProbPrices) -> u32 {
    let mut price = 0u32;
    let mut sym = sym | 0x100;
    loop {
        let bit = sym & 1;
        sym >>= 1;
        price += pp.price(probs[sym as usize], bit);
        if sym < 2 {
            break;
        }
    }
    price
}

pub fn lit_matched_get_price(probs: &[u16], sym: u32, match_byte: u32, pp: &ProbPrices) -> u32 {
    let mut price = 0u32;
    let mut offs = 0x100u32;
    let mut sym = sym | 0x100;
    let mut mb = match_byte;
    loop {
        mb <<= 1;
        let idx = (offs + (mb & offs) + (sym >> 8)) as usize;
        price += pp.price(probs[idx], (sym >> 7) & 1);
        sym <<= 1;
        offs &= !(mb ^ sym);
        if sym >= 0x10000 {
            break;
        }
    }
    price
}

// ---- price table fills ----

/// `SetPrices_3`: fill 8 low/mid length prices.
fn set_prices_3(probs: &[u16], start_price: u32, prices: &mut [u32], pp: &ProbPrices) {
    let mut i = 0;
    while i < 8 {
        let mut price = start_price;
        price += pp.price(probs[1], (i >> 2) as u32);
        price += pp.price(probs[2 + (i >> 2)], ((i >> 1) & 1) as u32);
        let prob = probs[4 + (i >> 1)];
        prices[i] = price + pp.price_0(prob);
        prices[i + 1] = price + pp.price_1(prob);
        i += 2;
    }
}

/// `LenPriceEnc_UpdateTables`, using the split low/mid/high length-prob layout
/// from [`crate::encoder`]'s `LenEnc` (`choice`/`choice2` are the shared bits).
#[allow(clippy::too_many_arguments)]
pub fn len_price_update(
    le: &mut LenPriceEnc,
    num_pos_states: usize,
    choice: u16,
    choice2: u16,
    low: &[[u16; 8]; NUM_POS_STATES_MAX],
    mid: &[[u16; 8]; NUM_POS_STATES_MAX],
    high: &[u16],
    pp: &ProbPrices,
) {
    let b = pp.price_1(choice);
    let a = pp.price_0(choice);
    let c = b + pp.price_0(choice2);
    let low_syms = LEN_NUM_LOW_SYMBOLS as usize;
    for ps in 0..num_pos_states {
        set_prices_3(&low[ps], a, &mut le.prices[ps][0..low_syms], pp);
        set_prices_3(&mid[ps], c, &mut le.prices[ps][low_syms..low_syms * 2], pp);
    }

    let table_size = le.table_size as usize;
    let low2 = low_syms * 2;
    if table_size > low2 {
        let bb = b + pp.price_1(choice2);
        let half_high = (1 << (LEN_NUM_HIGH_SYMBOLS.trailing_zeros() - 1)) as usize; // 128
        let mut i = (table_size - low2 + 1) >> 1;
        while i != 0 {
            i -= 1;
            let mut sym = i + half_high;
            let mut price = bb;
            loop {
                let bit = (sym & 1) as u32;
                sym >>= 1;
                price += pp.price(high[sym], bit);
                if sym < 2 {
                    break;
                }
            }
            let prob = high[i + half_high];
            le.prices[0][low2 + i * 2] = price + pp.price_0(prob);
            le.prices[0][low2 + i * 2 + 1] = price + pp.price_1(prob);
        }
        for ps in 1..num_pos_states {
            let (head, tail) = le.prices.split_at_mut(ps);
            tail[0][low2..table_size].copy_from_slice(&head[0][low2..table_size]);
        }
    }
}

impl Prices {
    /// `FillAlignPrices`.
    pub fn fill_align_prices(&mut self, pos_align_encoder: &[u16]) {
        for i in 0..ALIGN_TABLE_SIZE / 2 {
            let mut price = 0u32;
            let mut sym = i as u32;
            let mut m = 1usize;
            for _ in 0..3 {
                let bit = sym & 1;
                sym >>= 1;
                price += self.pp.price(pos_align_encoder[m], bit);
                m = (m << 1) + bit as usize;
            }
            let prob = pos_align_encoder[m];
            self.align_prices[i] = price + self.pp.price_0(prob);
            self.align_prices[i + 8] = price + self.pp.price_1(prob);
        }
    }

    /// `FillDistancesPrices`.
    pub fn fill_distances_prices(&mut self, pos_slot_encoder: &[[u16; 64]], pos_encoders: &[u16]) {
        self.match_price_count = 0;
        let mut temp_prices = [0u32; NUM_FULL_DISTANCES as usize];

        let mut i = (START_POS_MODEL_INDEX / 2) as usize;
        while i < (NUM_FULL_DISTANCES / 2) as usize {
            let pos_slot = get_pos_slot(i as u32);
            let mut footer_bits = (pos_slot >> 1) - 1;
            let base = ((2 | (pos_slot & 1)) << footer_bits) as usize;
            let probs = &pos_encoders[base * 2..];
            let mut price = 0u32;
            let mut m = 1usize;
            let mut sym = i as u32;
            let offset = 1usize << footer_bits;
            let dest = base + i;
            if footer_bits != 0 {
                loop {
                    let bit = sym & 1;
                    sym >>= 1;
                    price += self.pp.price(probs[m], bit);
                    m = (m << 1) + bit as usize;
                    footer_bits -= 1;
                    if footer_bits == 0 {
                        break;
                    }
                }
            }
            let prob = probs[m];
            temp_prices[dest] = price + self.pp.price_0(prob);
            temp_prices[dest + offset] = price + self.pp.price_1(prob);
            i += 1;
        }

        for lps in 0..NUM_LEN_TO_POS_STATES as usize {
            let dist_table_size2 = ((self.dist_table_size + 1) >> 1) as usize;
            let probs = &pos_slot_encoder[lps];

            for slot in 0..dist_table_size2 {
                let mut sym = (slot + (1 << (NUM_POS_SLOT_BITS - 1)) as usize) as u32;
                let mut price = 0u32;
                for _ in 0..5 {
                    let bit = sym & 1;
                    sym >>= 1;
                    price += self.pp.price(probs[sym as usize], bit);
                }
                let prob = probs[slot + (1 << (NUM_POS_SLOT_BITS - 1)) as usize];
                self.pos_slot_prices[lps][slot * 2] = price + self.pp.price_0(prob);
                self.pos_slot_prices[lps][slot * 2 + 1] = price + self.pp.price_1(prob);
            }

            let mut delta =
                ((END_POS_MODEL_INDEX / 2 - 1) - NUM_ALIGN_BITS) << NUM_BIT_PRICE_SHIFT_BITS;
            let mut slot = (END_POS_MODEL_INDEX / 2) as usize;
            while slot < dist_table_size2 {
                self.pos_slot_prices[lps][slot * 2] += delta;
                self.pos_slot_prices[lps][slot * 2 + 1] += delta;
                delta += 1 << NUM_BIT_PRICE_SHIFT_BITS;
                slot += 1;
            }

            let dp = &mut self.distances_prices[lps];
            dp[0] = self.pos_slot_prices[lps][0];
            dp[1] = self.pos_slot_prices[lps][1];
            dp[2] = self.pos_slot_prices[lps][2];
            dp[3] = self.pos_slot_prices[lps][3];
            let mut d = 4usize;
            while d < NUM_FULL_DISTANCES as usize {
                let slot_price = self.pos_slot_prices[lps][get_pos_slot(d as u32) as usize];
                dp[d] = slot_price + temp_prices[d];
                dp[d + 1] = slot_price + temp_prices[d + 1];
                d += 2;
            }
        }
    }
}
