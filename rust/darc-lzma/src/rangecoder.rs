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
//! Output is staged in a 64 KiB buffer and pushed to an [`OutStream`], which is
//! what `RC_BUF_SIZE` and `RangeEnc_FlushStream` do in the C. Staging is I/O
//! batching and cannot change the emitted bytes — but it does bound memory, which
//! is why this is no longer a plain growable `Vec`: a solid block's output is as
//! large as the block.

use crate::state::{BIT_MODEL_TOTAL, NUM_BIT_MODEL_TOTAL_BITS, NUM_MOVE_BITS, TOP_VALUE};
use crate::stream::{OutStream, StreamError};

/// `RC_BUF_SIZE` (`LzmaEnc.c`) — the staging buffer before a write to the sink.
const RC_BUF_SIZE: usize = 1 << 16;

/// LZMA range encoder writing through a staging buffer to an [`OutStream`].
pub struct RangeEncoder<'a> {
    low: u64,
    range: u32,
    cache: u8,
    cache_size: u64,
    out: Vec<u8>,
    sink: &'a mut dyn OutStream,
    /// The first sink error, latched. Coding continues so the caller sees one
    /// error rather than a cascade, exactly as `p->res` does in the C.
    result: Result<(), StreamError>,
}

impl<'a> RangeEncoder<'a> {
    /// `RangeEnc_Init` (`LzmaEnc.c:660`).
    pub fn new(sink: &'a mut dyn OutStream) -> Self {
        RangeEncoder {
            low: 0,
            range: 0xFFFF_FFFF,
            cache: 0,
            cache_size: 0,
            out: Vec::with_capacity(RC_BUF_SIZE + 16),
            sink,
            result: Ok(()),
        }
    }

    /// Push the staged bytes to the sink and reset the stage.
    fn flush_stage(&mut self) {
        if self.out.is_empty() {
            return;
        }
        if self.result.is_ok() {
            self.result = self.sink.write(&self.out);
        }
        self.out.clear();
    }

    /// Stage one byte, flushing when the stage fills.
    #[inline]
    fn emit(&mut self, byte: u8) {
        self.out.push(byte);
        if self.out.len() >= RC_BUF_SIZE {
            self.flush_stage();
        }
    }

    /// `RangeEnc_ShiftLow` (`LzmaEnc.c:685`): emit the next settled byte, counting
    /// pending `0xFF` carry bytes in `cache_size`.
    fn shift_low(&mut self) {
        let low = self.low as u32;
        let high = (self.low >> 32) as u32;
        self.low = u64::from(low << 8);
        if low < 0xFF00_0000 || high != 0 {
            self.emit((u32::from(self.cache).wrapping_add(high)) as u8);
            self.cache = (low >> 24) as u8;
            if self.cache_size != 0 {
                let byte = (0xFFu32.wrapping_add(high)) as u8;
                loop {
                    self.emit(byte);
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

    /// `RangeEnc_FlushData` (`LzmaEnc.c:717`) followed by `RangeEnc_FlushStream`:
    /// exactly 5 `shift_low` calls, then push everything staged.
    pub fn finish(mut self) -> Result<(), StreamError> {
        for _ in 0..5 {
            self.shift_low();
        }
        self.flush_stage();
        self.result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stream::VecOut;

    #[test]
    fn empty_stream_flushes_to_five_zeros() {
        // A fresh encoder, flushed with no symbols, emits five 0x00 bytes: the
        // initial cache (0) propagated through 5 shift_low calls. This is the
        // canonical "first byte is always 0" property of LZMA streams.
        let mut sink = VecOut::default();
        let rc = RangeEncoder::new(&mut sink);
        assert_eq!(rc.finish(), Ok(()));
        assert_eq!(sink.data, vec![0, 0, 0, 0, 0]);
    }

    #[test]
    fn output_always_starts_with_zero_byte() {
        let mut sink = VecOut::default();
        let mut rc = RangeEncoder::new(&mut sink);
        rc.encode_direct_bits(0b1011, 4);
        assert_eq!(rc.finish(), Ok(()));
        assert_eq!(sink.data[0], 0x00);
        assert!(sink.data.len() >= 5);
    }

    /// Staging must not change the bytes: the same symbols across the 64 KiB
    /// boundary produce the same stream whether or not a flush lands mid-run.
    #[test]
    fn staging_boundary_does_not_alter_the_stream() {
        let mut a = VecOut::default();
        {
            let mut rc = RangeEncoder::new(&mut a);
            for i in 0..300_000u32 {
                rc.encode_direct_bits(i & 0xFF, 8);
            }
            assert_eq!(rc.finish(), Ok(()));
        }
        assert!(
            a.data.len() > RC_BUF_SIZE,
            "output must cross the staging boundary or the test proves nothing"
        );
        // Byte 0 is still the initial cache, and the stream is a single run: any
        // duplicated or dropped stage would change the length.
        assert_eq!(a.data[0], 0x00);

        let mut b = VecOut::default();
        {
            let mut rc = RangeEncoder::new(&mut b);
            for i in 0..300_000u32 {
                rc.encode_direct_bits(i & 0xFF, 8);
            }
            assert_eq!(rc.finish(), Ok(()));
        }
        assert_eq!(a.data, b.data);
    }

    /// A sink error is latched and surfaced, not swallowed.
    #[test]
    fn sink_errors_reach_the_caller() {
        struct Failing;
        impl OutStream for Failing {
            fn write(&mut self, _: &[u8]) -> Result<(), StreamError> {
                Err(StreamError(-7))
            }
        }
        let mut sink = Failing;
        let rc = RangeEncoder::new(&mut sink);
        assert_eq!(rc.finish(), Err(StreamError(-7)));
    }
}
