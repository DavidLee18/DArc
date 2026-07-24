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
