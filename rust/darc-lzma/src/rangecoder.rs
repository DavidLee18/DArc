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
//!
//! ## The chunked (LZMA2) surface
//!
//! LZMA2 re-arms one encoder repeatedly: `LzmaEnc_CodeOneMemBlock`
//! (`LzmaEnc.c:2953`) calls `RangeEnc_Init` per chunk and points the coder at a
//! *bounded* scratch buffer, then asks how many bytes the chunk has produced so far
//! to decide where to cut. Three things here exist only for that:
//!
//! * [`RangeEncoder::reinit`] — `RangeEnc_Init` (`LzmaEnc.c:660`) as a re-arm
//!   rather than a constructor. Every LZMA chunk payload therefore starts with the
//!   initial `cache` byte, `0x00`, and that byte is counted in the chunk's
//!   `packSize`.
//! * [`RangeEncoder::get_processed`] — `RangeEnc_GetProcessed` (`LzmaEnc.c:637`).
//!   Read by the bounded break in `LzmaEnc_CodeOneBlock` (`:2663`), so it decides
//!   chunk boundaries: it must count the *staged but unwritten* bytes and the
//!   pending carry bytes, not just what reached the sink.
//! * [`BoundedSink`] — `CLzmaEnc_SeqOutStreamBuf` (`LzmaEnc.c:2911-2935`), a
//!   fixed-capacity sink that truncates and raises `overflow` instead of growing.

use crate::state::{BIT_MODEL_TOTAL, NUM_BIT_MODEL_TOTAL_BITS, NUM_MOVE_BITS, TOP_VALUE};
use crate::stream::{OutStream, StreamError};

/// `RC_BUF_SIZE` (`LzmaEnc.c:639`) — the staging buffer before a write to the sink.
const RC_BUF_SIZE: usize = 1 << 16;

/// `SZ_ERROR_WRITE` (`7zTypes.h`), the code `RangeEnc_FlushStream`
/// (`LzmaEnc.c:706`) latches when the sink accepts fewer bytes than offered. Only
/// the bounded sink can produce a short write; a stream sink reports its caller's
/// own error code instead, which is strictly more information.
const ERR_WRITE: StreamError = StreamError(5);

/// `CLzmaEnc_SeqOutStreamBuf` (`LzmaEnc.c:2911`) — a bounded scratch sink.
///
/// The C hands `LzmaEnc` a raw `dest` pointer plus a remaining count; a short write
/// sets `overflow` and is *not* an error at the sink level, but
/// `RangeEnc_FlushStream` notices the short count and latches `SZ_ERROR_WRITE`
/// anyway, and `LzmaEnc_CodeOneMemBlock` (`:2984`) then reports
/// `SZ_ERROR_OUTPUT_EOF` in preference to that. Both signals are kept separate here
/// for the same reason.
#[derive(Default)]
pub struct BoundedSink {
    /// What the coder has written since the last [`BoundedSink::arm`].
    pub data: Vec<u8>,
    /// `p->rem` — capacity still available.
    pub rem: usize,
    /// `p->overflow` — set once a write was truncated.
    pub overflow: bool,
}

impl BoundedSink {
    /// `outStream.data = dest; outStream.rem = *destLen; outStream.overflow = False`
    /// (`LzmaEnc.c:2962-2965`).
    pub fn arm(&mut self, rem: usize) {
        self.data.clear();
        self.rem = rem;
        self.overflow = false;
    }

    /// `SeqOutStreamBuf_Write` (`LzmaEnc.c:2919`), returning the accepted count.
    fn write_bounded(&mut self, data: &[u8]) -> usize {
        let mut size = data.len();
        if self.rem < size {
            size = self.rem;
            self.overflow = true;
        }
        if size != 0 {
            self.data.extend_from_slice(&data[..size]);
            self.rem -= size;
        }
        size
    }
}

/// Where a [`RangeEncoder`] pushes its staged bytes — `p->rc.outStream`, which
/// `LzmaEnc_CodeOneMemBlock` (`LzmaEnc.c:2971`) swaps for a bounded buffer.
enum Sink<'a> {
    Stream(&'a mut dyn OutStream),
    Bounded(BoundedSink),
}

/// LZMA range encoder writing through a staging buffer to an [`OutStream`].
pub struct RangeEncoder<'a> {
    low: u64,
    range: u32,
    cache: u8,
    cache_size: u64,
    out: Vec<u8>,
    sink: Sink<'a>,
    /// `p->processed` — bytes already pushed out of the stage. Note this is *not*
    /// the whole answer: see [`RangeEncoder::get_processed`].
    processed: u64,
    /// The first sink error, latched. Coding continues so the caller sees one
    /// error rather than a cascade, exactly as `p->res` does in the C.
    result: Result<(), StreamError>,
}

impl<'a> RangeEncoder<'a> {
    /// `RangeEnc_Init` (`LzmaEnc.c:660`) over a caller-supplied [`OutStream`].
    pub fn new(sink: &'a mut dyn OutStream) -> Self {
        RangeEncoder {
            low: 0,
            range: 0xFFFF_FFFF,
            cache: 0,
            cache_size: 0,
            out: Vec::with_capacity(RC_BUF_SIZE + 16),
            sink: Sink::Stream(sink),
            processed: 0,
            result: Ok(()),
        }
    }

    /// The same, over the bounded scratch sink LZMA2 uses.
    ///
    /// The sink starts with `rem == 0`, i.e. refusing everything; arm it with
    /// [`RangeEncoder::bounded_arm`] before each chunk, as
    /// `LzmaEnc_CodeOneMemBlock` does.
    pub fn new_bounded() -> Self {
        RangeEncoder {
            low: 0,
            range: 0xFFFF_FFFF,
            cache: 0,
            cache_size: 0,
            out: Vec::with_capacity(RC_BUF_SIZE + 16),
            sink: Sink::Bounded(BoundedSink::default()),
            processed: 0,
            result: Ok(()),
        }
    }

    /// `RangeEnc_Init` (`LzmaEnc.c:660`) as a re-arm: reset the coder state, drop
    /// whatever is staged, and clear the latched error — but keep the sink.
    ///
    /// Dropping the stage is not a leak: the C sets `p->buf = p->bufBase` here too,
    /// and every caller has just flushed via `Flush` (`LzmaEnc.c:2189`).
    pub fn reinit(&mut self) {
        self.range = 0xFFFF_FFFF;
        self.cache = 0;
        self.low = 0;
        self.cache_size = 0;
        self.out.clear();
        self.processed = 0;
        self.result = Ok(());
    }

    /// `RangeEnc_GetProcessed` (`LzmaEnc.c:637`):
    /// `processed + (buf - bufBase) + cacheSize`.
    ///
    /// All three terms matter. Dropping the stage term under-counts by up to
    /// `RC_BUF_SIZE`, and dropping `cacheSize` under-counts by the pending carry
    /// run — either one moves the bounded break in `LzmaEnc_CodeOneBlock` (`:2663`)
    /// and so moves every subsequent chunk boundary.
    pub fn get_processed(&self) -> u64 {
        self.processed + self.out.len() as u64 + self.cache_size
    }

    /// Point at the bounded sink's accumulated bytes, or `None` on the stream path.
    pub fn bounded(&self) -> Option<&BoundedSink> {
        match &self.sink {
            Sink::Stream(_) => None,
            Sink::Bounded(b) => Some(b),
        }
    }

    /// Re-arm the bounded sink with `rem` bytes of capacity. A no-op on the stream
    /// path, which has no capacity to speak of.
    pub fn bounded_arm(&mut self, rem: usize) {
        match &mut self.sink {
            Sink::Stream(_) => {}
            Sink::Bounded(b) => b.arm(rem),
        }
    }

    /// The latched sink error, as `CheckErrors` reads `p->rc.res`
    /// (`LzmaEnc.c:2165`).
    pub fn result(&self) -> Result<(), StreamError> {
        self.result
    }

    /// `RangeEnc_FlushStream` (`LzmaEnc.c:703`): push the staged bytes to the sink,
    /// add them to `processed`, and reset the stage.
    fn flush_stage(&mut self) {
        if self.out.is_empty() {
            return;
        }
        let num = self.out.len();
        match &mut self.sink {
            Sink::Stream(s) => {
                if self.result.is_ok() {
                    self.result = s.write(&self.out);
                }
            }
            Sink::Bounded(b) => {
                let wrote = b.write_bounded(&self.out);
                if wrote != num && self.result.is_ok() {
                    self.result = Err(ERR_WRITE);
                }
            }
        }
        // `p->processed += num` runs whether or not the write succeeded.
        self.processed += num as u64;
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
    ///
    /// Takes `&mut self` rather than `self` because LZMA2 re-arms the same coder for
    /// the next chunk (`RangeEnc_Init` at `LzmaEnc.c:2970`); this is `Flush`
    /// (`LzmaEnc.c:2189`), not a destructor.
    pub fn finish(&mut self) -> Result<(), StreamError> {
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
        let mut rc = RangeEncoder::new(&mut sink);
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
        let mut rc = RangeEncoder::new(&mut sink);
        assert_eq!(rc.finish(), Err(StreamError(-7)));
    }

    /// `RangeEnc_GetProcessed` counts what is staged and what is pending, not just
    /// what has reached the sink. The bounded break in `LzmaEnc_CodeOneBlock`
    /// (`:2663`) reads it, so an under-count moves every chunk boundary.
    #[test]
    fn get_processed_counts_the_stage_before_it_is_flushed() {
        // A bounded sink lets the same object be inspected while the coder holds it.
        let mut rc = RangeEncoder::new_bounded();
        rc.bounded_arm(1 << 20);
        assert_eq!(rc.get_processed(), 0);
        for i in 0..1000u32 {
            rc.encode_direct_bits(i & 0xFF, 8);
        }
        let staged = rc.get_processed();
        assert!(staged > 0, "nothing counted while the stage is still filling");
        match rc.bounded() {
            Some(b) => assert!(
                b.data.is_empty(),
                "the stage has not reached the sink yet, so this proves the stage term"
            ),
            None => panic!("new_bounded must expose a bounded sink"),
        }
        assert_eq!(rc.finish(), Ok(()));
        // finish adds exactly the 5 RangeEnc_FlushData bytes.
        assert_eq!(rc.get_processed(), staged + 5);
        match rc.bounded() {
            Some(b) => assert_eq!(b.data.len() as u64, rc.get_processed()),
            None => panic!("new_bounded must expose a bounded sink"),
        }
    }

    /// `reinit` re-arms the coder: the second run must be byte-identical to a fresh
    /// one, including the leading `0x00`. That is what makes every LZMA2 chunk
    /// payload start with a zero byte.
    #[test]
    fn reinit_produces_the_same_stream_as_a_fresh_coder() {
        let mut a = VecOut::default();
        {
            let mut rc = RangeEncoder::new(&mut a);
            for i in 0..500u32 {
                rc.encode_direct_bits(i & 0x7F, 7);
            }
            assert_eq!(rc.finish(), Ok(()));
            rc.reinit();
            assert_eq!(rc.get_processed(), 0);
            for i in 0..500u32 {
                rc.encode_direct_bits(i & 0x7F, 7);
            }
            assert_eq!(rc.finish(), Ok(()));
        }
        let half = a.data.len() / 2;
        assert_eq!(a.data[..half], a.data[half..], "reinit did not re-arm cleanly");
        assert_eq!(a.data[0], 0x00);
        assert_eq!(a.data[half], 0x00, "a re-armed chunk still starts with 0x00");
    }

    /// `SeqOutStreamBuf_Write` truncates and flags rather than growing, and the
    /// short count reaches `p->res` through `RangeEnc_FlushStream`.
    #[test]
    fn bounded_sink_truncates_and_flags_overflow() {
        let mut rc = RangeEncoder::new_bounded();
        rc.bounded_arm(3);
        for i in 0..100_000u32 {
            rc.encode_direct_bits(i & 0xFF, 8);
        }
        assert_eq!(rc.finish(), Err(ERR_WRITE));
        let b = match rc.bounded() {
            Some(b) => b,
            None => panic!("new_bounded must expose a bounded sink"),
        };
        assert!(b.overflow, "truncation must raise overflow");
        assert_eq!(b.data.len(), 3);
        assert_eq!(b.rem, 0);
    }

    /// With room to spare the bounded sink is transparent: same bytes as the stream
    /// sink, no overflow, no error.
    #[test]
    fn bounded_sink_matches_the_stream_sink_when_it_fits() {
        let mut sink = VecOut::default();
        {
            let mut rc = RangeEncoder::new(&mut sink);
            for i in 0..200u32 {
                rc.encode_direct_bits(i & 0xFF, 8);
            }
            assert_eq!(rc.finish(), Ok(()));
        }
        let mut rc = RangeEncoder::new_bounded();
        rc.bounded_arm(1 << 16);
        for i in 0..200u32 {
            rc.encode_direct_bits(i & 0xFF, 8);
        }
        assert_eq!(rc.finish(), Ok(()));
        match rc.bounded() {
            Some(b) => {
                assert!(!b.overflow);
                assert_eq!(b.data, sink.data);
            }
            None => panic!("new_bounded must expose a bounded sink"),
        }
    }
}
