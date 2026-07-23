//! The four LZ77 decoders, ported from `Compression/Tornado/LZ77_Coder.cpp`
//! (`LZ77_ByteDecoder` :96, `LZ77_BitDecoder` :348, `LZ77_Decoder<D>` :519).
//!
//! All four present the same four-call interface to the output loop:
//! `is_literal` -> `getchar`, or `is_literal` -> `getlen` -> `getdist`. The
//! order matters: `getlen` must be called before `getdist`, because the byte
//! decoder reads the distance *while* decoding the length and stashes it, and
//! the generic decoder consumes extra bits for the length first.
//!
//! What differs between them is only how a symbol is spelled:
//!
//! * **Byte** -- LZSS flag words: one 32-bit word carries two bits for each of
//!   16 following elements. `00` is a literal, and `01`/`10`/`11` select one of
//!   three match encodings of increasing reach.
//! * **Bit** -- a 9-bit code; below 256 it is a literal, otherwise its upper
//!   bits index a length code and its lower five a distance code, each followed
//!   by that code's extra bits.
//! * **Huffman / arith** -- one symbol out of `CODES`, where 0..255 are
//!   literals and the rest pack (length code, distance code) into a single
//!   number, plus the repeat-distance and repeat-char codes.
//!
//! The last of those carries the `prevdists` ring: four recently used
//! distances, addressable as short codes. Getting its update order wrong does
//! not corrupt the current match -- it corrupts every later one that refers
//! back to it, which is why it is transcribed literally here.

use super::range::ArithDecoder;
use super::huffman::HuffmanDecoder;
use super::stream::{InputBitStream, InputByteStream};
use super::vle::Tables;
use super::{CODES, EOB_CODE, LEN_CODES, REPCHAR, REPDIST_CODES};
use core::ffi::c_int;

/// Signals in the length space (Tornado.cpp uses these to end the stream and to
/// mark data tables).
pub const IMPOSSIBLE_LEN: u32 = 1 << 30;
pub const IMPOSSIBLE_DIST: u32 = 1 << 30;

/// What every back-end must provide to the output loop.
pub trait Lz77Decoder {
    fn is_literal(&mut self) -> bool;
    fn getchar(&mut self) -> u8;
    /// Must be called before `getdist`.
    fn getlen(&mut self, minlen: u32) -> u32;
    fn getdist(&mut self) -> u32;
    fn error(&self) -> Option<c_int>;
}

// ---------------------------------------------------------------------------
// Byte-aligned decoder (BYTECODER)
// ---------------------------------------------------------------------------

pub struct ByteDecoder<'a> {
    bytes: InputByteStream<'a>,
    flags: u32,
    flagpos: u32,
    dist: u32,
}

impl<'a> ByteDecoder<'a> {
    pub fn new(bytes: InputByteStream<'a>) -> Self {
        ByteDecoder { bytes, flags: 0, flagpos: 1, dist: 0 }
    }
}

impl Lz77Decoder for ByteDecoder<'_> {
    fn is_literal(&mut self) -> bool {
        // A fresh flag word every 16 elements; two bits each, consumed low-first.
        self.flagpos -= 1;
        if self.flagpos != 0 {
            self.flags >>= 2;
        } else {
            self.flagpos = 16;
            self.flags = self.bytes.get32();
        }
        (self.flags & 3) == 0
    }

    fn getchar(&mut self) -> u8 {
        self.bytes.getc() as u8
    }

    fn getlen(&mut self, minlen: u32) -> u32 {
        let len;
        match self.flags & 3 {
            1 => {
                let x = self.bytes.get16();
                len = x >> 12;
                self.dist = x % (1 << 12);
            }
            2 => {
                let x = self.bytes.get24();
                len = x >> 18;
                self.dist = x % (1 << 18);
            }
            _ => {
                // Case 3, the escape ladder: 255 means a wider distance
                // follows, and 254 (checked after that substitution, exactly as
                // the C does) means a wider length follows.
                let mut l = self.bytes.get8();
                if l == 255 {
                    self.dist = self.bytes.get8() << 24;
                    l = self.bytes.get8();
                } else {
                    self.dist = 0;
                }
                if l == 254 {
                    l = (self.bytes.get24() << 8).wrapping_add(self.bytes.get8());
                }
                self.dist = self.dist.wrapping_add(self.bytes.get24());
                len = l;
            }
        }
        minlen.wrapping_add(len)
    }

    fn getdist(&mut self) -> u32 {
        self.dist
    }

    fn error(&self) -> Option<c_int> {
        self.bytes.error()
    }
}

// ---------------------------------------------------------------------------
// Bit-aligned decoder (BITCODER)
// ---------------------------------------------------------------------------

pub struct BitDecoder<'a> {
    bits: InputBitStream<'a>,
    t: Tables,
    x: u32,
}

impl<'a> BitDecoder<'a> {
    pub fn new(bits: InputBitStream<'a>) -> Self {
        BitDecoder { bits, t: Tables::new(), x: 0 }
    }
}

impl Lz77Decoder for BitDecoder<'_> {
    fn is_literal(&mut self) -> bool {
        self.x = self.bits.getbits(9);
        self.x < 256
    }

    fn getchar(&mut self) -> u8 {
        self.x as u8
    }

    fn getlen(&mut self, minlen: u32) -> u32 {
        // Upper bits select the length code; the byte decoder's 8-entry table.
        let lcode = ((self.x >> 5).wrapping_sub(8)) as usize & 7;
        let lbits = self.t.lc_extra[lcode];
        let lbase = self.t.lc_base[lcode];
        minlen
            .wrapping_add(lbase)
            .wrapping_add(self.bits.getbits(lbits as i32))
    }

    fn getdist(&mut self) -> u32 {
        let dcode = (self.x & 31) as usize;
        let dbits = self.t.dc_extra[dcode];
        let dbase = self.t.dc_base[dcode];
        dbase.wrapping_add(self.bits.getbits(dbits as i32))
    }

    fn error(&self) -> Option<c_int> {
        self.bits.error()
    }
}

// ---------------------------------------------------------------------------
// Generic decoder over an entropy back-end (HUFCODER / ARICODER)
// ---------------------------------------------------------------------------

/// The entropy layer under `LZ77_Decoder`: one symbol, or n raw bits.
pub trait SymbolDecoder {
    fn decode(&mut self) -> usize;
    fn getbits(&mut self, n: u32) -> u32;
    fn error(&self) -> Option<c_int>;
}

pub struct HufBackend<'a> {
    bits: InputBitStream<'a>,
    huf: HuffmanDecoder,
}

impl<'a> HufBackend<'a> {
    pub fn new(bits: InputBitStream<'a>) -> Self {
        HufBackend { bits, huf: HuffmanDecoder::new(CODES, EOB_CODE) }
    }
}

impl SymbolDecoder for HufBackend<'_> {
    fn decode(&mut self) -> usize {
        self.huf.decode(&mut self.bits)
    }
    fn getbits(&mut self, n: u32) -> u32 {
        if n == 0 {
            0
        } else {
            self.bits.getbits(n as i32)
        }
    }
    fn error(&self) -> Option<c_int> {
        self.bits.error()
    }
}

impl SymbolDecoder for ArithDecoder<'_> {
    fn decode(&mut self) -> usize {
        ArithDecoder::decode(self)
    }
    fn getbits(&mut self, n: u32) -> u32 {
        ArithDecoder::getbits(self, n)
    }
    fn error(&self) -> Option<c_int> {
        ArithDecoder::error(self)
    }
}

pub struct GenericDecoder<D: SymbolDecoder> {
    d: D,
    t: Tables,
    x: usize,
    /// `prevdists[]` with its moving cursor. The C keeps 128 slots and slides
    /// the last few to the front on overflow purely for speed; a small ring of
    /// exactly REPDIST_CODES entries is the same thing observably, and it
    /// cannot run off the end.
    prev: [u32; REPDIST_CODES],
}

impl<D: SymbolDecoder> GenericDecoder<D> {
    pub fn new(d: D) -> Self {
        GenericDecoder { d, t: Tables::new(), x: 0, prev: [0; REPDIST_CODES] }
    }

    /// `prevdist[-1]` is the most recent distance; `[-2]` the one before it.
    #[inline]
    fn recent(&self, back: usize) -> u32 {
        self.prev[REPDIST_CODES - back]
    }

    /// Move the entry `back` places from the end to the end, sliding the ones
    /// after it down -- the C's explicit swap chains in `getdist`.
    #[inline]
    fn promote(&mut self, back: usize) -> u32 {
        let idx = REPDIST_CODES - back;
        let d = self.prev[idx];
        for i in idx..REPDIST_CODES - 1 {
            self.prev[i] = self.prev[i + 1];
        }
        self.prev[REPDIST_CODES - 1] = d;
        d
    }

    #[inline]
    fn push(&mut self, dist: u32) {
        for i in 0..REPDIST_CODES - 1 {
            self.prev[i] = self.prev[i + 1];
        }
        self.prev[REPDIST_CODES - 1] = dist;
    }
}

impl<D: SymbolDecoder> Lz77Decoder for GenericDecoder<D> {
    fn is_literal(&mut self) -> bool {
        self.x = self.d.decode();
        self.x < 256
    }

    fn getchar(&mut self) -> u8 {
        self.x as u8
    }

    fn getlen(&mut self, minlen: u32) -> u32 {
        if self.x == REPCHAR {
            return 1;
        }
        let lcode = self.x % LEN_CODES;
        let lbits = self.t.lc2_extra[lcode];
        let lbase = self.t.lc2_base[lcode];
        let len = lbase.wrapping_add(self.d.getbits(lbits));
        // Lengths above 100 are the escape window: 101..104 encode the four
        // table/EOF signals, anything higher is a real length shifted by 4.
        if len > 100 {
            if len <= 104 {
                len - 100 + IMPOSSIBLE_LEN
            } else {
                len - 4 + minlen
            }
        } else {
            len + minlen
        }
    }

    fn getdist(&mut self) -> u32 {
        if self.x == REPCHAR {
            return self.recent(1);
        }
        let dcode = (self.x - 256) / LEN_CODES;
        // The first REPDIST_CODES distance codes are "reuse a recent distance";
        // the C reaches them by letting dcode go negative.
        if dcode < REPDIST_CODES {
            return match dcode {
                0 => self.recent(1),
                1 => self.promote(2),
                2 => self.promote(3),
                _ => self.promote(4),
            };
        }
        let dcode = dcode - REPDIST_CODES;
        // Only 0..31 are real distance codes. A symbol at or above REPBOTH
        // yields 32, and the C reads `xextra_bits[32]` -- past the part its
        // constructor ever fills, so uninitialised. That is unreachable from a
        // valid stream: REPBOTH and the seven spare codes are declared but the
        // encoder never emits them, and EOB and REPCHAR are handled before
        // this. Clamping keeps a corrupt stream deterministic; the resulting
        // distance is then rejected by the output loop's `dist > bufsize` check
        // rather than silently copying from nowhere.
        let dcode = dcode.min(31);
        let dbits = self.t.dc_extra[dcode];
        let dbase = self.t.dc_base[dcode];
        let dist = dbase.wrapping_add(self.d.getbits(dbits)).wrapping_add(1);
        self.push(dist);
        dist
    }

    fn error(&self) -> Option<c_int> {
        self.d.error()
    }
}
