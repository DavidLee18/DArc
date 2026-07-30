//! Range *encoder* — a port of the `CRangeEnc` primitives in `LzmaEnc.c`:
//! `RangeEnc_Init` (`:660`), `RangeEnc_ShiftLow` (`:685`), `RangeEnc_FlushData`
//! (`:717`), the `RC_BIT` / `RC_NORM` macros (`:724-778`), the inlined direct-bits
//! loop (`:2133`), and the bit-tree encoders (`RcTree_*`, `:900`).
//!
//! ## Bit-exactness notes
//! - `low` is 64-bit; `shift_low` extracts the carry from bit 32 (`LzmaEnc.c:685`).
//! - Renormalization is a single shift per coded bit (`RC_NORM` is an `if`, not a
//!   loop): the probability floor (~31/2048) guarantees one `<< 8` restores
//!   `range >= TOP_VALUE`.
//! - `flush` is exactly **5** `shift_low` calls. Off-by-one corrupts every tail.
//! - The output's first byte is always `0x00` (the initial `cache`), matching
//!   `LzmaEnc_MemEncode`.
//!
//! Unlike the C code, output goes straight to a growable `Vec<u8>`; the 64 KiB
//! `RC_BUF_SIZE` staging buffer and `RangeEnc_FlushStream` are I/O batching that
//! does not affect the emitted bytes, so they are omitted.

use crate::state::{BIT_MODEL_TOTAL, NUM_BIT_MODEL_TOTAL_BITS, NUM_MOVE_BITS, TOP_VALUE};

/// LZMA range encoder writing to an in-memory buffer.
pub struct RangeEncoder {
    low: u64,
    range: u32,
    cache: u8,
    cache_size: u64,
    out: Vec<u8>,
}

impl RangeEncoder {
    /// `RangeEnc_Init` (`LzmaEnc.c:660`).
    pub fn new() -> Self {
        RangeEncoder {
            low: 0,
            range: 0xFFFF_FFFF,
            cache: 0,
            cache_size: 0,
            out: Vec::new(),
        }
    }

    /// `RangeEnc_ShiftLow` (`LzmaEnc.c:685`): emit the next settled byte, counting
    /// pending `0xFF` carry bytes in `cache_size`.
    fn shift_low(&mut self) {
        let low = self.low as u32;
        let high = (self.low >> 32) as u32;
        self.low = u64::from(low << 8);
        if low < 0xFF00_0000 || high != 0 {
            self.out
                .push((u32::from(self.cache).wrapping_add(high)) as u8);
            self.cache = (low >> 24) as u8;
            if self.cache_size != 0 {
                let byte = (0xFFu32.wrapping_add(high)) as u8;
                loop {
                    self.out.push(byte);
                    self.cache_size -= 1;
                    if self.cache_size == 0 {
                        break;
                    }
                }
            }
            return;
        }
        self.cache_size += 1;
    }

    /// `RC_NORM` (`LzmaEnc.c:724`): renormalize after a coded bit.
    #[inline]
    fn normalize(&mut self) {
        if self.range < TOP_VALUE {
            self.range <<= 8;
            self.shift_low();
        }
    }

    /// `RC_BIT` (`LzmaEnc.c:744`): encode `bit` under `prob` and adapt `prob`.
    #[inline]
    pub fn encode_bit(&mut self, prob: &mut u16, bit: u32) {
        let ttt = u32::from(*prob);
        let new_bound = (self.range >> NUM_BIT_MODEL_TOTAL_BITS) * ttt;
        if bit == 0 {
            self.range = new_bound;
            *prob = (ttt + ((BIT_MODEL_TOTAL - ttt) >> NUM_MOVE_BITS)) as u16;
        } else {
            self.low += u64::from(new_bound);
            self.range -= new_bound;
            *prob = (ttt - (ttt >> NUM_MOVE_BITS)) as u16;
        }
        self.normalize();
    }

    /// `RangeEnc_EncodeDirectBits` (inlined at `LzmaEnc.c:2133`): encode the top
    /// `num_bits` of `value` with no probability model, MSB first.
    pub fn encode_direct_bits(&mut self, value: u32, num_bits: u32) {
        debug_assert!(num_bits > 0);
        let mut n = num_bits;
        loop {
            self.range >>= 1;
            n -= 1;
            // low += range & (0 - bit), branchless like the C source.
            let bit = (value >> n) & 1;
            self.low += u64::from(self.range & 0u32.wrapping_sub(bit));
            self.normalize();
            if n == 0 {
                break;
            }
        }
    }

    /// `RcTree_Encode` (forward, MSB first): encode `num_bits` of `sym` through the
    /// bit-tree `probs`, which is indexed from 1.
    pub fn encode_tree(&mut self, probs: &mut [u16], num_bits: u32, sym: u32) {
        let mut m: u32 = 1;
        let mut i = num_bits;
        while i != 0 {
            i -= 1;
            let bit = (sym >> i) & 1;
            self.encode_bit(&mut probs[m as usize], bit);
            m = (m << 1) | bit;
        }
    }

    /// `RcTree_ReverseEncode` (`LzmaEnc.c:900`): encode `num_bits` of `sym` LSB
    /// first through the bit-tree `probs`, indexed from 1.
    pub fn encode_tree_reverse(&mut self, probs: &mut [u16], num_bits: u32, sym: u32) {
        let mut m: u32 = 1;
        let mut s = sym;
        for _ in 0..num_bits {
            let bit = s & 1;
            s >>= 1;
            self.encode_bit(&mut probs[m as usize], bit);
            m = (m << 1) | bit;
        }
    }

    /// `RangeEnc_FlushData` (`LzmaEnc.c:717`): flush with exactly 5 `shift_low`
    /// calls and return the completed stream.
    pub fn finish(mut self) -> Vec<u8> {
        for _ in 0..5 {
            self.shift_low();
        }
        self.out
    }
}

impl Default for RangeEncoder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_stream_flushes_to_five_zeros() {
        // A fresh encoder, flushed with no symbols, emits five 0x00 bytes: the
        // initial cache (0) propagated through 5 shift_low calls. This is the
        // canonical "first byte is always 0" property of LZMA streams.
        let rc = RangeEncoder::new();
        assert_eq!(rc.finish(), vec![0, 0, 0, 0, 0]);
    }

    #[test]
    fn output_always_starts_with_zero_byte() {
        let mut rc = RangeEncoder::new();
        rc.encode_direct_bits(0b1011, 4);
        let out = rc.finish();
        assert_eq!(out[0], 0x00);
        assert!(out.len() >= 5);
    }
}
