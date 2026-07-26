//! The binary range decoder, ported from
//! `Compression/BSC/libbsc/coder/common/rangecoder.h`.
//!
//! QLFC codes every decision as a binary symbol against a 12-bit probability,
//! so this is the layer every one of BSC's entropy coders sits on. Only the
//! decoder half is ported; the encoder is not on the decode path.
//!
//! The coder is 32-bit with 16-bit renormalisation: it reads `unsigned short`
//! at a time, and `InitDecoder` primes `ari_code` with three of them (48 bits
//! shifted into a 32-bit register, so the first 16 fall out again -- that is
//! the C's behaviour and reproducing it exactly is what matters, not whether it
//! looks redundant).
//!
//! ## Widths
//!
//! `ari_code` and `ari_range` are `unsigned int` and every operation on them
//! wraps at 32 bits. The subtraction in `DecodeBit` relies on that, so all of
//! it is `wrapping_*` here -- Rust would panic in debug where C wraps silently.
//!
//! Reads past the end of the compressed data return zero rather than running
//! off the buffer. The C reads unchecked (`*ari_input++`), which on a truncated
//! block walks into whatever follows; a decoder reached through `arc t` on an
//! untrusted archive must not.

/// The default probability shift: probabilities are 12-bit (0..4096).
const P_DEFAULT: u32 = 12;

pub struct RangeDecoder<'a> {
    input: &'a [u8],
    pos: usize,
    code: u32,
    range: u32,
    /// Shorts requested past the end of the input; a truncated block would
    /// otherwise read stale memory.
    overrun: usize,
}

impl<'a> RangeDecoder<'a> {
    /// `InitDecoder`.
    pub fn new(input: &'a [u8]) -> Self {
        let mut d = RangeDecoder { input, pos: 0, code: 0, range: 0xffff_ffff, overrun: 0 };
        // Three 16-bit reads shifted into a 32-bit accumulator, exactly as the
        // C does it.
        for _ in 0..3 {
            let s = d.input_short();
            d.code = (d.code << 16) | s;
        }
        d
    }

    /// `InputShort`: the stream is read as little-endian 16-bit words.
    #[inline]
    fn input_short(&mut self) -> u32 {
        if self.pos + 2 <= self.input.len() {
            let v = u16::from_le_bytes([self.input[self.pos], self.input[self.pos + 1]]);
            self.pos += 2;
            v as u32
        } else {
            self.overrun += 1;
            self.pos += 2;
            0
        }
    }

    /// How far past the compressed data this decoder has had to read. The
    /// caller treats a large value as a truncated block.
    pub fn overrun(&self) -> usize {
        self.overrun
    }

    #[inline]
    fn renormalize(&mut self) {
        if self.range < 0x10000 {
            self.range <<= 16;
            let s = self.input_short();
            self.code = (self.code << 16) | s;
        }
    }

    /// `DecodeBit(probability)` with the default 12-bit scale.
    #[inline]
    pub fn decode_bit_p(&mut self, probability: u32) -> u32 {
        self.decode_bit_shift(probability, P_DEFAULT)
    }

    /// `DecodeBit<P>(probability)`.
    #[inline]
    pub fn decode_bit_shift(&mut self, probability: u32, p: u32) -> u32 {
        self.renormalize();
        let range = (self.range >> p).wrapping_mul(probability);
        let bit = (self.code >= range) as u32;
        if bit != 0 {
            self.range = self.range.wrapping_sub(range);
            self.code = self.code.wrapping_sub(range);
        } else {
            self.range = range;
        }
        bit
    }

    /// `PeakBit<P>`: the same test without consuming the symbol.
    #[inline]
    pub fn peak_bit(&mut self, probability: u32, p: u32) -> u32 {
        self.renormalize();
        (self.code >= (self.range >> p).wrapping_mul(probability)) as u32
    }

    /// `DecodeBit0<P>` / `DecodeBit1<P>`: commit a symbol whose value is
    /// already known, as after a `PeakBit`.
    #[inline]
    pub fn decode_bit0(&mut self, probability: u32, p: u32) {
        self.range = (self.range >> p).wrapping_mul(probability);
    }

    #[inline]
    pub fn decode_bit1(&mut self, probability: u32, p: u32) {
        let range = (self.range >> p).wrapping_mul(probability);
        self.code = self.code.wrapping_sub(range);
        self.range = self.range.wrapping_sub(range);
    }

    /// `DecodeBit()`: an equiprobable bit (probability 2048 of 4096).
    #[inline]
    pub fn decode_bit(&mut self) -> u32 {
        self.decode_bit_p(2048)
    }

    /// `DecodeByte`.
    pub fn decode_byte(&mut self) -> u32 {
        let mut byte = 0u32;
        for _ in 0..8 {
            byte = byte.wrapping_add(byte).wrapping_add(self.decode_bit());
        }
        byte
    }

    /// `DecodeWord`.
    pub fn decode_word(&mut self) -> u32 {
        let mut word = 0u32;
        for _ in 0..32 {
            word = word.wrapping_add(word).wrapping_add(self.decode_bit());
        }
        word
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Init consumes exactly three 16-bit words and leaves `code` holding the
    /// last two -- the first is shifted out of the 32-bit register. Pinning
    /// this because it looks like a bug and is not.
    #[test]
    fn init_shifts_three_shorts_into_a_32_bit_code() {
        let input = [0x11, 0x11, 0x22, 0x22, 0x33, 0x33, 0, 0];
        let d = RangeDecoder::new(&input);
        assert_eq!(d.pos, 6, "three shorts consumed");
        assert_eq!(d.code, 0x2222_3333, "first short shifted out");
        assert_eq!(d.range, 0xffff_ffff);
    }

    /// A probability of 0 makes every bit a 1, and 4096 (the full scale) makes
    /// every bit a 0 -- the two ends of the 12-bit scale.
    #[test]
    fn extreme_probabilities_are_deterministic() {
        let input = [0xffu8; 64];
        let mut d = RangeDecoder::new(&input);
        for _ in 0..16 {
            assert_eq!(d.decode_bit_p(0), 1, "probability 0 => range 0 => code >= 0");
        }
        let zeros = [0u8; 64];
        let mut d = RangeDecoder::new(&zeros);
        for _ in 0..16 {
            assert_eq!(d.decode_bit_p(4096), 0, "full-scale probability => code < range");
        }
    }

    /// Reading past the end must yield zeros and be counted, not walk off the
    /// buffer -- the C reads unchecked here.
    #[test]
    fn reads_past_the_end_are_bounded_and_counted() {
        let short_input = [1u8, 2];
        let mut d = RangeDecoder::new(&short_input);
        assert!(d.overrun() > 0, "init alone over-reads a 2-byte input");
        for _ in 0..1000 {
            let _ = d.decode_bit();
        }
        // Still alive, still counting -- no panic, no out-of-bounds.
        assert!(d.overrun() > 0);
    }

    /// decode_bit0/1 must leave the coder in the same state as decode_bit would
    /// have, given the bit peak_bit predicted.
    #[test]
    fn peak_then_commit_matches_a_plain_decode() {
        let input: Vec<u8> = (0..64u8).map(|i| i.wrapping_mul(37)).collect();
        for prob in [100u32, 1024, 2048, 3000, 4000] {
            let mut a = RangeDecoder::new(&input);
            let mut b = RangeDecoder::new(&input);
            for _ in 0..20 {
                let bit = b.peak_bit(prob, 12);
                if bit != 0 { b.decode_bit1(prob, 12) } else { b.decode_bit0(prob, 12) }
                assert_eq!(a.decode_bit_p(prob), bit, "prob {prob}");
                assert_eq!((a.code, a.range), (b.code, b.range), "state diverged at prob {prob}");
            }
        }
    }
}


/// The encoder half of libbsc's `RangeCoder` (`coder/common/rangecoder.h:123`),
/// the mirror of [`RangeDecoder`] above.
///
/// ## The `low`/`carry` union
///
/// The C keeps `low` as a union of a `unsigned long long` and a
/// `{ low32, carry }` pair, so `ari.low += range` overflowing bit 31 lands in
/// `carry` for the shift step to pick up. That aliasing is little-endian-only,
/// which DArc is (`FREEARC_INTEL_BYTE_ORDER` is mandatory). Here `low` is a
/// plain u64 and `carry` is read as `low >> 32`, which is the same value
/// without depending on layout.
///
/// ## Output is 16 bits at a time
///
/// `OutputShort` writes a `unsigned short`, so the coded stream is a sequence
/// of little-endian 16-bit words, not bytes -- and `CheckEOB` compares *short*
/// pointers against `output + outputSize - 16`. Getting that unit wrong yields
/// a stream that decodes for a while and then diverges.
pub struct RangeEncoder<'a> {
    out: &'a mut [u8],
    /// Index in 16-bit units, matching the C's `unsigned short *`.
    pos: usize,
    eob: usize,
    low: u64,
    ffnum: u32,
    cache: u32,
    range: u32,
}

impl<'a> RangeEncoder<'a> {
    /// `InitEncoder(output, outputSize)`.
    pub fn new(out: &'a mut [u8]) -> Self {
        // outputEOB = (unsigned short *)(output + outputSize - 16); a byte
        // offset converted to a short index, and it may sit before the start
        // for a very small buffer, which CheckEOB then reports immediately.
        let eob = out.len().saturating_sub(16) / 2;
        RangeEncoder { out, pos: 0, eob, low: 0, ffnum: 0, cache: 0, range: 0xffff_ffff }
    }

    /// `CheckEOB()`: the encoders poll this and give up with
    /// `LIBBSC_NOT_COMPRESSIBLE` rather than overrun.
    pub fn check_eob(&self) -> bool {
        self.pos >= self.eob
    }

    fn output_short(&mut self, s: u16) {
        let at = self.pos * 2;
        if at + 2 <= self.out.len() {
            self.out[at..at + 2].copy_from_slice(&s.to_le_bytes());
        }
        // Past the end the C would write anyway; the counter still advances so
        // the returned length and CheckEOB agree with it. The bounds test above
        // is the only divergence, and it only engages after the encoder has
        // already decided the block does not fit.
        self.pos += 1;
    }

    /// `ShiftLow()` and `ShiftLowSlow()`, which differ only in whether the fast
    /// path applies; folded into one function with the same branch structure.
    fn shift_low(&mut self) -> u32 {
        let low32 = self.low as u32;
        let carry = (self.low >> 32) as u32;

        if self.ffnum == 0 && low32 < 0xffff_0000 {
            self.output_short((self.cache.wrapping_add(carry)) as u16);
            self.cache = low32 >> 16;
            self.low = ((low32 << 16) as u64) & 0xffff_ffff; // clears carry
            return self.range << 16;
        }

        // ShiftLowSlow
        if low32 < 0xffff_0000 || carry != 0 {
            self.output_short((self.cache.wrapping_add(carry)) as u16);
            if self.ffnum != 0 {
                let s = carry.wrapping_sub(1) as u16;
                while self.ffnum != 0 {
                    self.output_short(s);
                    self.ffnum -= 1;
                }
            }
            self.cache = low32 >> 16;
            // ari.u.carry = 0, leaving low32 untouched until the shift below.
            self.low &= 0xffff_ffff;
        } else {
            self.ffnum += 1;
        }
        // `ari.u.low32 <<= 16` -- the low half only; carry keeps whatever it
        // holds, which the branch above may have just cleared.
        let carry_now = self.low & 0xffff_ffff_0000_0000;
        self.low = carry_now | (((low32 << 16) as u64) & 0xffff_ffff);

        self.range << 16
    }

    /// `FinishEncoder()`: returns the coded length in BYTES.
    pub fn finish(&mut self) -> usize {
        if self.range < 0x10000 {
            self.shift_low();
        }
        self.shift_low();
        self.shift_low();
        self.shift_low();
        self.pos * 2
    }

    pub fn encode_bit0_p(&mut self, probability: u32, p: u32) {
        if self.range < 0x10000 {
            self.range = self.shift_low();
        }
        self.range = (self.range >> p) * probability;
    }

    pub fn encode_bit1_p(&mut self, probability: u32, p: u32) {
        if self.range < 0x10000 {
            self.range = self.shift_low();
        }
        let range = (self.range >> p) * probability;
        self.low += range as u64;
        self.range -= range;
    }

    /// `EncodeBit(bit, probability)`. The C writes it branchlessly with
    /// `(~bit + 1u) & range`, which is `bit ? range : 0`; spelled as the
    /// condition here since the masking is an optimisation, not semantics.
    pub fn encode_bit_p(&mut self, bit: u32, probability: u32, p: u32) {
        if bit != 0 {
            self.encode_bit1_p(probability, p);
        } else {
            self.encode_bit0_p(probability, p);
        }
    }

    /// `EncodeBit(bit)` with no model: probability 2048 at P = 12, i.e. even.
    pub fn encode_bit(&mut self, bit: u32) {
        self.encode_bit_p(bit, 2048, 12);
    }

    pub fn encode_byte(&mut self, byte: u32) {
        for bit in (0..8).rev() {
            self.encode_bit(byte & (1 << bit));
        }
    }

    pub fn encode_word(&mut self, word: u32) {
        for bit in (0..32).rev() {
            self.encode_bit(word & (1 << bit));
        }
    }
}

#[cfg(test)]
mod encoder_tests {
    use super::*;

    /// The encoder's only real obligation: the already-verified decoder must
    /// read back what it wrote. Model-free bits and words only -- the modelled
    /// paths are exercised by the QLFC harness, where the C is the oracle.
    #[test]
    fn round_trips_words_and_bits() {
        let words = [0u32, 1, 0xffff_ffff, 12345, 0x8000_0000];
        let bits = [1u32, 0, 1, 1, 0, 0, 0, 1, 1, 0];

        let mut buf = vec![0u8; 4096];
        let n = {
            let mut e = RangeEncoder::new(&mut buf);
            for w in words {
                e.encode_word(w);
            }
            for b in bits {
                e.encode_bit(b);
            }
            e.finish()
        };
        assert!(n > 0 && n <= buf.len());

        let mut d = RangeDecoder::new(&buf);
        for w in words {
            assert_eq!(d.decode_word(), w, "word round-trip");
        }
        for b in bits {
            assert_eq!(d.decode_bit(), b, "bit round-trip");
        }
    }

    #[test]
    fn reports_eob_before_overrunning_a_small_buffer() {
        let mut buf = vec![0u8; 32];
        let mut e = RangeEncoder::new(&mut buf);
        for _ in 0..64 {
            e.encode_word(0xdead_beef);
        }
        assert!(e.check_eob(), "a full buffer must report EOB");
    }
}
