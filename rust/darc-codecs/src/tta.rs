//! TTA lossless-audio decoder, ported from `Compression/MM/tta.cpp`,
//! `entropy.cpp` and `filters.cpp` (`tta_decompress` and everything it reaches).
//!
//! TTA is a multimedia (audio) filter: an order-1 fixed predictor, three
//! cascaded adaptive hybrid filters, and an adaptive Rice entropy coder. The
//! archiver reaches it as `-mtta`. Only the decoder is ported here -- the same
//! decode-first order used for REP and Dict -- because a Rust build must *read*
//! every existing `-mtta` archive before it may write one. The 1,117-line
//! `mmdet.cpp` detector is not needed: it drives *encoder* autodetection only.
//!
//! ## Integer width is the whole ballgame
//!
//! This codec was the source of ten separate LP64 width bugs in this repo. The
//! fixes settled on exact widths, and the port must reproduce them exactly or
//! it decodes archives to the wrong bytes:
//!
//! * The bit array is addressed as 32-bit words (`tta_word` = `u32`).
//! * The adaptive filter state is 32-bit *by design* (`tta_i32` = `i32`) and
//!   relies on 32-bit wraparound to keep its cascading differences bounded --
//!   so every filter operation here is a `wrapping_*` on `i32`. In C this is
//!   plain `int` arithmetic that wraps; Rust would panic in debug, and using
//!   `i64` (as the pre-fix C accidentally did on LP64) makes the state run away
//!   and frames stop round-tripping.
//! * The sample buffers and running Rice sums are `long`/`unsigned long`, which
//!   on the LP64 build that wrote these archives are 64-bit -- so `i64`/`u64`
//!   here, not `i32`/`u32`.
//!
//! ## The stream
//!
//! `header[4]` = (level, raw_data*2+is_float, num_chan, word_size), then a
//! 4-byte `offset` and that many verbatim header bytes, then a sequence of
//! blocks until EOF. Each block: `bytes_read` (uncompressed byte count); then
//! for the entropy path a `compressed size` and the bit array (or, if that size
//! is zero, `bytes_read` stored bytes); then `bytes_read % (num_chan*byte_size)`
//! trailing bytes copied verbatim. `level==0` means the whole payload is stored.
//!
//! Every length pulled from the stream is untrusted (this runs on `arc t`), so
//! each is bounded before it drives an allocation or a copy, mirroring the C
//! hardening. A corrupt Rice run that never terminates is bounded by the read
//! buffer's last whole word, exactly as `get_unary` was fixed to do.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO,
                 FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, OK};
use core::ffi::c_int;
use crate::mmdet;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;
const NOMEM: c_int = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY as c_int;

// A hard ceiling on any single length read from the stream, matching the
// `> (1<<30)` guards the C decoder applies to bytes_read and bit_array_size.
const MAX_LEN: u64 = 1 << 30;
const MB: usize = 1 << 20;

// ---------------------------------------------------------------------------
// Stream reads. The archiver's read callback fills a request fully mid-stream
// and returns 0 at EOF; a partial read is a truncated/corrupt stream. This
// mirrors the READ / READ4 / READ4_OR_EOF macros in Compression.h.
// ---------------------------------------------------------------------------

fn read_exact(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.read(buf) {
        n if n as usize == buf.len() => Ok(()),
        n if n >= 0 => Err(IO), // short read where the format demands a full one
        n => Err(n),
    }
}

fn read_u32(io: &Io) -> Result<u32, c_int> {
    let mut b = [0u8; 4];
    read_exact(io, &mut b)?;
    Ok(u32::from_le_bytes(b))
}

/// `READ4_OR_EOF`: `Ok(None)` on a clean end of stream (zero bytes), `Ok(Some)`
/// on a full word, and `Err(IO)` on a partial word (a frame header cut short).
fn read_u32_or_eof(io: &Io) -> Result<Option<u32>, c_int> {
    let mut b = [0u8; 4];
    match io.read(&mut b) {
        0 => Ok(None),
        4 => Ok(Some(u32::from_le_bytes(b))),
        n if n >= 0 => Err(IO),
        n => Err(n),
    }
}

// ---------------------------------------------------------------------------
// Entropy tables (entropy.cpp). bit_shift has 40 entries; shift_16 is the view
// starting at index 4 (`shift_16 = bit_shift + 4`). Values fit in u32 but are
// `unsigned long` in C, hence u64 here so the Rice-sum arithmetic matches LP64.
// ---------------------------------------------------------------------------

const BIT_MASK32: [u64; 33] = [
    0x00000000, 0x00000001, 0x00000003, 0x00000007, 0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff, 0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff, 0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff,
];

const BIT_SHIFT: [u64; 40] = [
    0x00000001, 0x00000002, 0x00000004, 0x00000008, 0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800, 0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000, 0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000, 0x10000000, 0x20000000, 0x40000000, 0x80000000,
    0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000,
];

#[inline]
fn shift_16(i: usize) -> u64 {
    BIT_SHIFT[i + 4]
}

/// `ENC`/`DEC` map signed samples to the unsigned values the Rice coder sees.
/// `DEC(x) = (x&1) ? (x+1)>>1 : (-x)>>1`, the shift arithmetic on `long`.
///
/// Wrapping add/neg because a corrupt frame (reachable via `arc t`) can drive
/// `x` to any 64-bit value, and `-i64::MIN` / `i64::MAX + 1` would panic in a
/// debug build -- an unwind across the C ABI. C's `long` here simply wraps in
/// two's complement, which is what a hostile stream would produce anyway.
#[inline]
fn dec(x: i64) -> i64 {
    if x & 1 != 0 {
        x.wrapping_add(1) >> 1
    } else {
        x.wrapping_neg() >> 1
    }
}

// ---------------------------------------------------------------------------
// Bit reader (init_bit_array_read / get_binary / get_unary).
// ---------------------------------------------------------------------------

/// Reads little-endian 32-bit words out of a byte buffer, MSB-to-LSB within the
/// running bit position, matching the C reader that casts the buffer to
/// `tta_word*` on a little-endian target.
struct BitReader {
    /// The frame's bytes, padded up to a whole word plus one spare word so the
    /// final partial word can be fetched in bounds. calloc-zeroed padding, as
    /// `init_bit_array_read` allocates.
    words: Vec<u32>,
    /// Number of whole (or final partial) words the *real* data covers; reads
    /// past this return zero, as the fixed C bounds do.
    word_count: usize,
    bits: u64,
}

impl BitReader {
    fn new(data: &[u8]) -> Self {
        let size = data.len();
        // ((size + 3) & ~3) + 4 bytes, zero-padded -> that many /4 words.
        let padded = ((size + 3) & !3) + 4;
        let mut bytes = vec![0u8; padded];
        bytes[..size].copy_from_slice(data);
        let words = bytes
            .chunks_exact(4)
            .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect();
        // bit_array_read_words = (size + 3) >> 2
        let word_count = (size + 3) >> 2;
        BitReader { words, word_count, bits: 0 }
    }

    #[inline]
    fn word(&self, pos: usize) -> u32 {
        self.words[pos]
    }

    /// `get_binary`: read `bits` bits as an unsigned value.
    fn get_binary(&mut self, bits: u64) -> u64 {
        let fbit = self.bits & 0x1f;
        let rbit = 32 - fbit;
        let pos = (self.bits >> 5) as usize;

        if pos >= self.word_count {
            return 0;
        }
        let value: u64;
        if bits <= rbit {
            value = ((self.word(pos) as u64) >> fbit) & BIT_MASK32[bits as usize];
        } else {
            if pos + 1 >= self.word_count {
                return 0;
            }
            let lo = ((self.word(pos) as u64) >> fbit) & BIT_MASK32[rbit as usize];
            let hi = ((self.word(pos + 1) as u64) & BIT_MASK32[(bits - rbit) as usize]) << rbit;
            value = lo | hi;
        }
        self.bits += bits;
        value
    }

    /// `get_unary`: count a run of set bits terminated by a clear bit, bounded
    /// by the last whole word so a corrupt non-terminating run cannot walk off
    /// the buffer.
    fn get_unary(&mut self) -> u64 {
        let mut fbit = self.bits & 0x1f;
        let rbit = 32 - fbit;
        let mut pos = (self.bits >> 5) as usize;
        let end = self.word_count;

        let mut value: u64 = 0;
        if pos >= end {
            return 0;
        }

        if ((self.word(pos) as u64) >> fbit) == BIT_MASK32[rbit as usize] {
            value += rbit;
            fbit = 0;
            while pos + 1 < end && {
                pos += 1;
                self.word(pos) as u64 == BIT_MASK32[32]
            } {
                value += 32;
            }
        }
        let mut mask: u64 = 1u64 << fbit;
        while pos < end && ((self.word(pos) as u64) & mask) != 0 {
            value += 1;
            mask <<= 1;
        }

        self.bits += value + 1;
        value
    }
}

/// `decode_frame`: adaptive Rice decode of `len` samples into `data`.
fn decode_frame(br: &mut BitReader, data: &mut [i64]) {
    let mut k0: usize = 10;
    let mut k1: usize = 10;
    let mut sum0: u64 = shift_16(k0);
    let mut sum1: u64 = shift_16(k1);

    for slot in data.iter_mut() {
        // decode Rice unsigned
        let mut unary = br.get_unary();
        if unary == 50 {
            unary = br.get_binary(32);
        }

        let (depth, k) = match unary {
            0 => (0, k0),
            _ => {
                unary -= 1;
                (1, k1)
            }
        };

        let mut value: i64 = if k != 0 {
            let binary = br.get_binary(k as u64);
            ((unary << k).wrapping_add(binary)) as i64
        } else {
            unary as i64
        };

        if depth == 1 {
            sum1 = sum1.wrapping_add((value as u64).wrapping_sub(sum1 >> 4));
            if k1 > 0 && sum1 < shift_16(k1) {
                k1 -= 1;
            } else if k1 < 32 && sum1 > shift_16(k1 + 1) {
                k1 += 1;
            }
            value = value.wrapping_add(BIT_SHIFT[k0] as i64);
            // fall through to the depth-0 sum0 update, matching the "no break!"
        }
        sum0 = sum0.wrapping_add((value as u64).wrapping_sub(sum0 >> 4));
        if k0 > 0 && sum0 < shift_16(k0) {
            k0 -= 1;
        } else if k0 < 32 && sum0 > shift_16(k0 + 1) {
            k0 += 1;
        }

        *slot = dec(value);
    }
}

// ---------------------------------------------------------------------------
// Filters (filters.cpp). All state is exactly 32-bit and wraps; every op below
// is a wrapping i32 op for that reason.
// ---------------------------------------------------------------------------

const MAX_ORDER: usize = 32;
const BUF_SIZE: usize = 256;

// flt_set[stage][level-1][byte_size-1] = {order, shift, mode}
const FLT_SET: [[[[i32; 3]; 4]; 3]; 3] = [
    [
        [[8, 10, 0], [8, 9, 0], [8, 10, 0], [8, 12, 1]],
        [[8, 10, 0], [8, 11, 0], [8, 10, 0], [16, 12, 1]],
        [[8, 10, 0], [8, 11, 0], [8, 10, 0], [32, 12, 1]],
    ],
    [
        [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]],
        [[16, 10, 1], [16, 9, 1], [16, 10, 1], [0, 0, 0]],
        [[32, 11, 1], [32, 11, 1], [32, 11, 1], [0, 0, 0]],
    ],
    [
        [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]],
        [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]],
        [[0, 0, 0], [16, 9, 1], [16, 10, 1], [0, 0, 0]],
    ],
];

/// `PREDICTOR1(x, k)`: the order-1 fixed predictor, done in unsigned 32-bit so
/// the wrap is defined and the final `>> k` is arithmetic on `i32`.
#[inline]
fn predictor1(x: i64, k: u32) -> i64 {
    let ux = x as u32;
    ((((ux << k).wrapping_sub(ux)) as i32) >> k) as i64
}

/// The adaptive hybrid filter state, `fltst`. `px`/`pl` are indices into
/// `dx`/`dl` rather than pointers; the sliding window is reproduced by advancing
/// them and memcpy-compacting when they reach the end.
struct Filter {
    order: usize,
    mode: i32,
    shift: i32,
    round: i32,
    qm: [i32; MAX_ORDER],
    dx: [i32; BUF_SIZE],
    dl: [i32; BUF_SIZE],
    px: usize, // index into dx
    pl: usize, // index into dl
}

impl Filter {
    fn new(order: i32, shift: i32, mode: i32) -> Self {
        Filter {
            order: order as usize,
            mode,
            shift,
            round: if shift > 0 { 1i32 << (shift - 1) } else { 0 },
            qm: [0; MAX_ORDER],
            dx: [0; BUF_SIZE],
            dl: [0; BUF_SIZE],
            px: 0,
            pl: 0,
        }
    }

    /// `filter_decompress`: transform one sample in place.
    fn decompress(&mut self, sample: &mut i64) {
        let order = self.order;

        // sum = round + dot(pl[0..order], qm[0..order]); all wrapping i32.
        let mut sum: i32 = self.round;
        for j in 0..order {
            sum = sum.wrapping_add(self.dl[self.pl + j].wrapping_mul(self.qm[j]));
        }

        // out = in + (sum >> shift), truncated to i32 (as C stores into tta_i32).
        let inv = *sample;
        let out: i32 = (inv as i32).wrapping_add(sum >> self.shift);

        // pl[order] = out
        self.dl[self.pl + order] = out;

        if self.mode == 0 {
            // adaptive polynomial predictors: pl[order-1-n] = pl[order-n] - pl[order-1-n]
            let base = self.pl + order - 1;
            for n in 0..3 {
                self.dl[base - n] = self.dl[base + 1 - n].wrapping_sub(self.dl[base - n]);
            }
        }

        // qm[n] += / -= px[n], by the sign of the *input* residual.
        if inv < 0 {
            for j in 0..order {
                self.qm[j] = self.qm[j].wrapping_add(self.dx[self.px + j]);
            }
        } else if inv > 0 {
            for j in 0..order {
                self.qm[j] = self.qm[j].wrapping_sub(self.dx[self.px + j]);
            }
        }

        // px[order-n] = sign-bucket of pl[order-n]
        let pxo = self.px + order;
        let plo = self.pl + order;
        self.dx[pxo] = ((self.dl[plo] >> 28) & 8) - 4;
        self.dx[pxo - 1] = ((self.dl[plo - 1] >> 29) & 4) - 2;
        self.dx[pxo - 2] = ((self.dl[plo - 2] >> 29) & 4) - 2;
        self.dx[pxo - 3] = ((self.dl[plo - 3] >> 30) & 2) - 1;

        // slide the windows, compacting at the end of dx/dl
        if self.px + order == BUF_SIZE - 1 {
            self.dx.copy_within(self.px + 1..self.px + 1 + order, 0);
            self.px = 0;
        } else {
            self.px += 1;
        }
        if self.pl + order == BUF_SIZE - 1 {
            self.dl.copy_within(self.pl + 1..self.pl + 1 + order, 0);
            self.pl = 0;
        } else {
            self.pl += 1;
        }

        *sample = out as i64;
    }
}

/// `filters_decompress`: three cascaded filters (highest stage first on decode)
/// then the fixed order-1 predictor undone.
/// Bytes per sample word: TTA supports 1-3 byte integers and 4-byte floats, and
/// nothing else.
///
/// Exhaustive four variants with **no** `Option` at the use sites, unlike MM's
/// `WordBytes`. The difference is where the value comes from: MM's width is an
/// unvalidated header byte, so an unfiltered width is ordinary input. TTA rejects
/// anything outside 1..=4 *before* the filtered path -- decoding at `:545-549`
/// and `:568`, encoding at `:1141-1143`, which falls back to the stored path --
/// and `FLT_SET[..][byte_size - 1]` would index out of bounds anyway. So the
/// conversion happens at those guards, where an invalid width already has a real
/// handler, and the filters downstream cannot be handed one.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SampleBytes {
    /// 8-bit integer samples.
    One,
    /// 16-bit integer.
    Two,
    /// 24-bit integer.
    Three,
    /// 32-bit float. TTA refuses 32-bit *integers*, so this variant is float-only.
    Four,
}

impl SampleBytes {
    /// `None` for a width TTA does not support. Callers turn that into `Err(BAD)`
    /// when decoding, or into the stored path when encoding.
    fn from_byte_size(n: usize) -> Option<SampleBytes> {
        match n {
            1 => Some(SampleBytes::One),
            2 => Some(SampleBytes::Two),
            3 => Some(SampleBytes::Three),
            4 => Some(SampleBytes::Four),
            _ => None,
        }
    }

    /// The width in bytes, 1..=4. The single source of truth for the byte
    /// arithmetic that used to read a separate `byte_size` parameter.
    fn width(self) -> usize {
        match self {
            SampleBytes::One => 1,
            SampleBytes::Two => 2,
            SampleBytes::Three => 3,
            SampleBytes::Four => 4,
        }
    }

    /// Index into `FLT_SET`'s innermost dimension, which the C spells
    /// `byte_size - 1`.
    fn flt_index(self) -> usize {
        self.width() - 1
    }
}

fn filters_decompress(data: &mut [i64], level: usize, bytes: SampleBytes) {
    let f1 = FLT_SET[0][level - 1][bytes.flt_index()];
    let f2 = FLT_SET[1][level - 1][bytes.flt_index()];
    let f3 = FLT_SET[2][level - 1][bytes.flt_index()];
    let mut fst1 = Filter::new(f1[0], f1[1], f1[2]);
    let mut fst2 = Filter::new(f2[0], f2[1], f2[2]);
    let mut fst3 = Filter::new(f3[0], f3[1], f3[2]);

    let mut last: i64 = 0;
    for p in data.iter_mut() {
        if fst3.order != 0 {
            fst3.decompress(p);
        }
        if fst2.order != 0 {
            fst2.decompress(p);
        }
        if fst1.order != 0 {
            fst1.decompress(p);
        }

        // `filters.cpp:297`: cases 1-4 and no default, so this is total in the C
        // too. Not `tta.cpp:133/:183` -- those are the read/write interleave
        // switches, a different pair; the predictors live in filters.cpp, which
        // #98 deleted, so they are cited from the pinned reference.
        match bytes {
            SampleBytes::One => *p += predictor1(last, 4),
            SampleBytes::Two => *p += predictor1(last, 5),
            SampleBytes::Three => *p += predictor1(last, 5),
            SampleBytes::Four => *p += last,
        }
        last = *p;
    }
}

// ---------------------------------------------------------------------------
// Channel recombination (combine_int / combine_float) and output (write_wave).
// ---------------------------------------------------------------------------

/// `combine_int`: undo the inter-channel decorrelation done at encode time.
fn combine_int(frame_len: usize, num_chan: usize, buffer: &mut [Vec<i64>]) {
    if num_chan > 1 {
        let n = num_chan - 1;
        for i in 0..frame_len {
            buffer[n][i] += buffer[n - 1][i] / 2;
            for j in (1..=n).rev() {
                buffer[j - 1][i] = buffer[j][i] - buffer[j - 1][i];
            }
        }
    }
}

const SWAP16_LOW16: u32 = 0xffff;

/// `SWAP16`: reverse the low 16 bits.
#[inline]
fn swap16(x: u32) -> u32 {
    (x & SWAP16_LOW16).reverse_bits() >> 16
}

/// `combine_float`: rebuild 32-bit floats from the split hi/lo channels.
fn combine_float(frame_len: usize, num_chan: usize, buffer: &mut [Vec<i64>]) {
    for i in 0..frame_len {
        for j in 0..num_chan {
            let negative = (buffer[j + num_chan][i] as u32) & 0x80000000;
            let mut data_hi = buffer[j][i] as u32;
            let data_lo = (buffer[j + num_chan][i].unsigned_abs() as u32).wrapping_sub(1);
            data_hi = data_hi.wrapping_add(0x3F80);
            buffer[j][i] = ((data_hi << 16) | swap16(data_lo) | negative) as i32 as i64;
        }
    }
}

/// `write_wave`: interleave the per-channel samples back into `byte_size`-wide
/// little-endian words and emit them.
fn write_wave(
    io: &Io,
    bytes: SampleBytes,
    num_chan: usize,
    frame_len: usize,
    buffer: &[Vec<i64>],
) -> Result<(), c_int> {
    let mut out = vec![0u8; frame_len * num_chan * bytes.width()];
    for i in 0..frame_len {
        for n in 0..num_chan {
            let v = buffer[n][i];
            let base = (i * num_chan + n) * bytes.width();
            // Exhaustive: no arm for a width TTA rejected upstream. The C's
            // `switch` (tta.cpp:183) has cases 1-4 and no default for the same
            // reason -- those are the only widths that reach it.
            match bytes {
                SampleBytes::One => out[base] = (v + 0x80) as u8,
                SampleBytes::Two => {
                    out[base..base + 2].copy_from_slice(&(v as i16).to_le_bytes())
                }
                SampleBytes::Three => {
                    let u = v as u32;
                    out[base] = u as u8;
                    out[base + 1] = (u >> 8) as u8;
                    out[base + 2] = (u >> 16) as u8;
                }
                SampleBytes::Four => {
                    out[base..base + 4].copy_from_slice(&(v as i32).to_le_bytes())
                }
            }
        }
    }
    io.write_all(&out)
}

// ---------------------------------------------------------------------------
// The decoder proper (tta_decompress).
// ---------------------------------------------------------------------------

/// Decode a TTA stream. Entry point matching `tta_decompress`.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    // TTA header
    let mut header = [0u8; 4];
    read_exact(io, &mut header)?;
    let level = header[0] as usize;
    let is_float = (header[1] % 2) as usize;
    let raw_data = header[1] / 2;
    let num_chan = header[2] as usize;
    let word_size = header[3] as usize;
    let byte_size = (word_size + 7) / 8;

    if level > 3
        || raw_data > 1
        || (is_float != 0 && byte_size != 4)
        || (is_float == 0 && byte_size >= 4)
    {
        return Err(BAD);
    }

    // level 0 => the payload is stored; copy input to output verbatim.
    if level == 0 {
        let mut buf = vec![0u8; MB];
        loop {
            let n = io.read(&mut buf);
            if n <= 0 {
                return if n < 0 { Err(n) } else { Ok(()) };
            }
            io.write_all(&buf[..n as usize])?;
        }
    }

    // num_chan / byte_size are now guaranteed non-zero for the sample paths:
    // byte_size >= 1 (word_size <= 255) and a stored/level-0 stream already
    // returned. num_chan can still be zero from a corrupt header, which would
    // make the sample-set size zero -- reject rather than divide by it.
    if num_chan == 0 || byte_size == 0 {
        return Err(BAD);
    }
    // Classify HERE, at the guard, rather than at each filter: this is the one
    // place an unsupported width has a real answer (reject the stream). The
    // checks above already narrow it to 1..=4, so this cannot fail today -- but
    // expressing it as a conversion means the filters downstream cannot be
    // handed a width at all, instead of being trusted not to receive one.
    let bytes = SampleBytes::from_byte_size(byte_size).ok_or(BAD)?;
    let sample_set = num_chan * bytes.width(); // bytes per (all-channels) sample

    // Copy the verbatim original-data header.
    let offset = read_u32(io)? as usize;
    if offset > MAX_LEN as usize {
        return Err(BAD);
    }
    if offset > 0 {
        let mut buf = vec![0u8; offset];
        read_exact(io, &mut buf)?;
        io.write_all(&buf)?;
    }

    let rows = num_chan << is_float;

    loop {
        // Frame header is optional (EOF ends the stream); everything after it
        // in the frame is mandatory.
        let bytes_read = match read_u32_or_eof(io)? {
            None => return Ok(()),
            Some(v) => v as u64,
        };
        if bytes_read > MAX_LEN {
            return Err(BAD);
        }
        let bytes_read = bytes_read as usize;
        let frame_len = bytes_read / sample_set;

        // Read the block body.
        let mut buffer: Vec<Vec<i64>>;
        if raw_data == 0 {
            let bit_array_size = read_u32(io)? as u64;
            if bit_array_size > MAX_LEN {
                return Err(BAD);
            }
            if bit_array_size == 0 {
                // stored block: copy bytes_read bytes through
                let mut buf = vec![0u8; bytes_read];
                read_exact(io, &mut buf)?;
                io.write_all(&buf)?;
                continue;
            }
            let mut packed = vec![0u8; bit_array_size as usize];
            read_exact(io, &mut packed)?;

            buffer = alloc2d(rows, frame_len)?;
            let mut br = BitReader::new(&packed);
            for row in buffer.iter_mut() {
                decode_frame(&mut br, row);
                filters_decompress(row, level, bytes);
            }
        } else {
            // raw_data == 1: the predictor residuals were dumped as raw `long`
            // values. On the LP64 build that wrote these archives `long` is 8
            // bytes, so each sample is a little-endian i64. (This path is
            // architecture-dependent in the C, a known quirk; match the writer.)
            buffer = alloc2d(rows, frame_len)?;
            let mut raw = vec![0u8; frame_len.checked_mul(8).ok_or(BAD)?];
            for row in buffer.iter_mut() {
                read_exact(io, &mut raw)?;
                for (slot, chunk) in row.iter_mut().zip(raw.chunks_exact(8)) {
                    // `chunks_exact(8)` yields exactly 8 bytes, so this cannot
                    // fail; expressed as an error return rather than a panic
                    // because this runs under a C caller.
                    let word: [u8; 8] = chunk.try_into().map_err(|_| BAD)?;
                    *slot = i64::from_le_bytes(word);
                }
                filters_decompress(row, level, bytes);
            }
        }

        if is_float != 0 {
            combine_float(frame_len, num_chan, &mut buffer);
        } else {
            combine_int(frame_len, num_chan, &mut buffer);
        }

        if frame_len != 0 {
            write_wave(io, bytes, num_chan, frame_len, &buffer)?;
        }

        // Trailing bytes that do not fill a whole sample-set, copied verbatim.
        let rest = bytes_read % sample_set;
        if rest != 0 {
            let mut buf = vec![0u8; rest];
            read_exact(io, &mut buf)?;
            io.write_all(&buf)?;
        }
    }
}

/// `malloc2d(num, len)`: `num` rows of `len` zeroed `i64`s. The C allocates one
/// block; here each row is its own Vec, which the callers never rely on being
/// contiguous. Guards the total against absurd sizes from a corrupt header.
fn alloc2d(num: usize, len: usize) -> Result<Vec<Vec<i64>>, c_int> {
    // The filters index up to pl+order (< len+MAX_ORDER) only inside dl/dx, not
    // this buffer, so len alone is the row length. Bound the product.
    if num != 0 && len > (isize::MAX as usize) / 8 / num {
        return Err(NOMEM);
    }
    Ok(vec![vec![0i64; len]; num])
}

// ---------------------------------------------------------------------------
// Encoder -- the port of `tta_compress` (tta.cpp:294) and the encode halves of
// entropy.cpp and filters.cpp.
//
// Every stage is the exact mirror of the decoder above, and the two differ in
// ways that are easy to get backwards, so they are spelled out where they occur:
// `filter_compress` subtracts where `filter_decompress` adds, stores the INPUT
// into the history where decode stores the OUTPUT, and tests the sign of the
// OUTPUT where decode tests the input.
// ---------------------------------------------------------------------------

/// `BASE_SIZE`/`STEP_SIZE` (entropy.h:34-35) are byte counts for C's realloc
/// policy. The growth schedule is not observable -- only the bits written are --
/// so this grows a word vector instead, which cannot leave an uninitialised
/// word behind. (In C that is safe only by an argument about `fbit`: a freshly
/// grown word is always first touched with fbit == 0, where `*s &= bit_mask32[0]`
/// clears it.)
struct BitWriter {
    words: Vec<u32>,
    bits: u64,
}

impl BitWriter {
    fn new() -> Self {
        BitWriter {
            words: vec![0u32; (1 << 20) / 4],
            bits: 0,
        }
    }

    /// `get_len`: the byte length, rounding a partial byte up.
    fn len(&self) -> usize {
        ((self.bits >> 3) + if self.bits & 7 != 0 { 1 } else { 0 }) as usize
    }

    #[inline]
    fn reserve(&mut self, word: usize) {
        if word + 2 > self.words.len() {
            self.words.resize(word + 2 + (1 << 18), 0);
        }
    }

    /// `put_binary` (entropy.cpp:115).
    #[inline]
    fn put_binary(&mut self, value: u64, bits: u64) {
        let fbit = self.bits & 0x1F;
        let rbit = 32 - fbit;
        let pos = (self.bits >> 5) as usize;
        self.reserve(pos);

        // Clear everything at or above the current bit, then lay the field in.
        self.words[pos] &= BIT_MASK32[fbit as usize] as u32;
        self.words[pos] |= ((value & BIT_MASK32[bits as usize]) << fbit) as u32;
        if bits > rbit {
            self.words[pos + 1] = (value >> rbit) as u32;
        }
        self.bits += bits;
    }

    /// `put_unary` (entropy.cpp:134): `value` one-bits then a zero.
    #[inline]
    fn put_unary(&mut self, value: u64) {
        let fbit = self.bits & 0x1F;
        let rbit = 32 - fbit;
        let mut pos = (self.bits >> 5) as usize;
        self.reserve(pos + (value >> 5) as usize + 1);

        self.words[pos] &= BIT_MASK32[fbit as usize] as u32;
        if value < rbit {
            self.words[pos] |= (BIT_MASK32[value as usize] << fbit) as u32;
        } else {
            let mut unary = value;
            self.words[pos] |= (BIT_MASK32[rbit as usize] << fbit) as u32;
            pos += 1;
            unary -= rbit;
            // `> 32`, not `>= 32`: the loop leaves a remainder of exactly 32 to
            // the tail assignment below, which writes a full word of ones.
            while unary > 32 {
                self.words[pos] = BIT_MASK32[32] as u32;
                pos += 1;
                unary -= 32;
            }
            if unary != 0 {
                self.words[pos] = BIT_MASK32[unary as usize] as u32;
            }
        }
        self.bits += value + 1;
    }

    /// The bytes to emit: the word vector reinterpreted little-endian, which is
    /// what C's `(tta_word*)bit_array_write` aliasing produces on every target
    /// this builds for.
    fn bytes(&self) -> Vec<u8> {
        let n = self.len();
        let mut out = Vec::with_capacity(n);
        for w in self.words.iter() {
            if out.len() >= n {
                break;
            }
            out.extend_from_slice(&w.to_le_bytes());
        }
        out.truncate(n);
        out
    }
}

/// `ENC` (entropy.h:37): fold a signed residual onto the unsigned line.
#[inline]
fn enc(x: i64) -> i64 {
    if x > 0 {
        (x << 1) - 1
    } else {
        (-x) << 1
    }
}

/// `encode_frame` (entropy.cpp:216): adaptive Rice coding with two parameter
/// tracks, exactly mirroring `decode_frame`'s adaptation -- including the clamp
/// at 32, which both sides must apply or they fall out of lock-step.
fn encode_frame(bw: &mut BitWriter, data: &[i64]) {
    let mut k0: u64 = 10;
    let mut k1: u64 = 10;
    let mut sum0: u64 = shift_16(k0 as usize);
    let mut sum1: u64 = shift_16(k1 as usize);

    for &sample in data {
        let mut value = enc(sample) as u64;
        let mut k = k0;

        sum0 = sum0.wrapping_add(value.wrapping_sub(sum0 >> 4));
        if k0 > 0 && sum0 < shift_16(k0 as usize) {
            k0 -= 1;
        } else if k0 < 32 && sum0 > shift_16(k0 as usize + 1) {
            k0 += 1;
        }

        let unary;
        if value >= BIT_SHIFT[k as usize] {
            value -= BIT_SHIFT[k as usize];
            k = k1;

            sum1 = sum1.wrapping_add(value.wrapping_sub(sum1 >> 4));
            if k1 > 0 && sum1 < shift_16(k1 as usize) {
                k1 -= 1;
            } else if k1 < 32 && sum1 > shift_16(k1 as usize + 1) {
                k1 += 1;
            }

            unary = 1 + (value >> k);
        } else {
            unary = 0;
        }

        // An escape at 50: longer runs go out as a 50-run plus a 32-bit literal.
        if unary >= 50 {
            bw.put_unary(50);
            bw.put_binary(unary, 32);
        } else {
            bw.put_unary(unary);
        }
        if k != 0 {
            bw.put_binary(value & BIT_MASK32[k as usize], k);
        }
    }
}

impl Filter {
    /// `filter_compress` (filters.cpp:64). Three differences from `decompress`,
    /// all of them silent if transposed:
    ///   * `out = in - (sum >> shift)`, where decode adds;
    ///   * the history takes the INPUT, where decode stores the output;
    ///   * the adaptation tests the sign of the OUTPUT, where decode tests the
    ///     input.
    /// In both directions the history holds the original-domain sample and the
    /// sign test looks at the residual-domain one -- they are the same rule
    /// seen from opposite sides.
    fn compress(&mut self, sample: &mut i64) {
        let order = self.order;

        let mut sum: i32 = self.round;
        for j in 0..order {
            sum = sum.wrapping_add(self.dl[self.pl + j].wrapping_mul(self.qm[j]));
        }

        let inv = *sample;
        let out: i32 = (inv as i32).wrapping_sub(sum >> self.shift);

        self.dl[self.pl + order] = inv as i32;

        if self.mode == 0 {
            let base = self.pl + order - 1;
            for n in 0..3 {
                self.dl[base - n] = self.dl[base + 1 - n].wrapping_sub(self.dl[base - n]);
            }
        }

        if out < 0 {
            for j in 0..order {
                self.qm[j] = self.qm[j].wrapping_add(self.dx[self.px + j]);
            }
        } else if out > 0 {
            for j in 0..order {
                self.qm[j] = self.qm[j].wrapping_sub(self.dx[self.px + j]);
            }
        }

        let pxo = self.px + order;
        let plo = self.pl + order;
        self.dx[pxo] = ((self.dl[plo] >> 28) & 8) - 4;
        self.dx[pxo - 1] = ((self.dl[plo - 1] >> 29) & 4) - 2;
        self.dx[pxo - 2] = ((self.dl[plo - 2] >> 29) & 4) - 2;
        self.dx[pxo - 3] = ((self.dl[plo - 3] >> 30) & 2) - 1;

        if self.px + order == BUF_SIZE - 1 {
            self.dx.copy_within(self.px + 1..self.px + 1 + order, 0);
            self.px = 0;
        } else {
            self.px += 1;
        }
        if self.pl + order == BUF_SIZE - 1 {
            self.dl.copy_within(self.pl + 1..self.pl + 1 + order, 0);
            self.pl = 0;
        } else {
            self.pl += 1;
        }

        *sample = out as i64;
    }
}

/// `filters_compress` (filters.cpp:243): the fixed order-1 predictor first, then
/// the three adaptive stages in ASCENDING order -- decode runs them descending.
fn filters_compress(data: &mut [i64], level: usize, bytes: SampleBytes) {
    let f1 = FLT_SET[0][level - 1][bytes.flt_index()];
    let f2 = FLT_SET[1][level - 1][bytes.flt_index()];
    let f3 = FLT_SET[2][level - 1][bytes.flt_index()];
    let mut fst1 = Filter::new(f1[0], f1[1], f1[2]);
    let mut fst2 = Filter::new(f2[0], f2[1], f2[2]);
    let mut fst3 = Filter::new(f3[0], f3[1], f3[2]);

    let mut last: i64 = 0;
    for v in data.iter_mut() {
        let tmp = *v;
        // `filters.cpp:260`, the mirror of the decoder's `:297`: cases 1-4, no
        // default.
        match bytes {
            SampleBytes::One => *v = v.wrapping_sub(predictor1(last, 4)),
            SampleBytes::Two | SampleBytes::Three => *v = v.wrapping_sub(predictor1(last, 5)),
            SampleBytes::Four => *v = v.wrapping_sub(last),
        }
        last = tmp;

        if fst1.order != 0 {
            fst1.compress(v);
        }
        if fst2.order != 0 {
            fst2.compress(v);
        }
        if fst3.order != 0 {
            fst3.compress(v);
        }
    }
}

/// `split_int` (tta.cpp:227): de-interleave, then inter-channel decorrelation.
fn split_int(data: &[i64], frame_len: usize, num_chan: usize, buffer: &mut [Vec<i64>]) {
    for i in 0..frame_len {
        for j in 0..num_chan {
            buffer[j][i] = data[i * num_chan + j];
        }
    }
    if num_chan > 1 {
        let n = num_chan - 1;
        for i in 0..frame_len {
            for j in 0..n {
                buffer[j][i] = buffer[j + 1][i].wrapping_sub(buffer[j][i]);
            }
            buffer[n][i] = buffer[n][i].wrapping_sub(buffer[n - 1][i] / 2);
        }
    }
}

/// `split_float` (tta.cpp:258): split each IEEE-754 single into an exponent-ish
/// high half and a byte-swapped mantissa, doubling the channel count.
fn split_float(data: &[i64], frame_len: usize, num_chan: usize, buffer: &mut [Vec<i64>]) {
    for i in 0..frame_len {
        for j in 0..num_chan {
            let t = data[i * num_chan + j] as u64;
            // C computes this as `unsigned long negative = (t & 0x80000000)? -1:1`
            // -- i.e. all-ones on LP64 -- and multiplies, so the effect is a
            // two's-complement negation of the 64-bit product.
            let negative: i64 = if t & 0x8000_0000 != 0 { -1 } else { 1 };
            let data_hi = (t & 0x7FFF_0000) >> 16;
            let data_lo = t & 0x0000_FFFF;

            buffer[j][i] = (data_hi as i64).wrapping_sub(0x3F80);
            buffer[j + num_chan][i] =
                ((swap16(data_lo as u32) as i64).wrapping_add(1)).wrapping_mul(negative);
        }
    }
}

/// `read_wave` (tta.cpp:118): fill `data` with sign-extended samples, keep the
/// trailing partial sample in `rest`, and hand back the raw bytes so the
/// incompressible path can store them verbatim.
///
/// Returns the byte count read, or the callback's error.
#[allow(clippy::too_many_arguments)]
fn read_wave(
    io: &Io,
    data: &mut [i64],
    rest: &mut Vec<u8>,
    prev: &[u8],
    bytes: SampleBytes,
    num_chan: usize,
    len: usize,
) -> Result<(usize, Vec<u8>), c_int> {
    let sample = num_chan * bytes.width();
    let wanted = len * sample;
    let mut buffer = vec![0u8; (len + 2) * sample];

    let use_prev = prev.len().min(wanted);
    buffer[..use_prev].copy_from_slice(&prev[..use_prev]);

    // Note the asymmetry in C: it copies min(prevsize,wanted) bytes but reads at
    // offset `prevsize`. They differ only when prevsize > wanted, and in that
    // case no read happens at all, so the offset is unused.
    let mut bytes_read = if wanted <= prev.len() {
        0
    } else {
        let n = io.read(&mut buffer[prev.len()..wanted]);
        if n < 0 {
            return Err(n);
        }
        n as usize
    };
    bytes_read += use_prev;

    let rest_bytes = bytes_read % sample;
    rest.clear();
    rest.extend_from_slice(&buffer[bytes_read - rest_bytes..bytes_read]);

    let elements = (bytes_read / sample) * num_chan;
    // Exhaustive: the C's `switch` (tta.cpp:133) has cases 1-4 and no default,
    // because a width outside that range never reaches here.
    match bytes {
        SampleBytes::One => {
            for i in 0..elements {
                data[i] = buffer[i] as i64 - 0x80;
            }
        }
        SampleBytes::Two => {
            for i in 0..elements {
                data[i] = i16::from_le_bytes([buffer[2 * i], buffer[2 * i + 1]]) as i64;
            }
        }
        SampleBytes::Three => {
            // Three bytes sign-extended through 32 bits. C dereferenced a `long`
            // here once, which on LP64 read five bytes past each sample.
            for i in 0..elements {
                let q = &buffer[i * 3..i * 3 + 3];
                let t = (q[0] as u32) | ((q[1] as u32) << 8) | ((q[2] as u32) << 16);
                data[i] = (((t << 8) as i32) >> 8) as i64;
            }
        }
        SampleBytes::Four => {
            for i in 0..elements {
                data[i] = i32::from_le_bytes([
                    buffer[4 * i],
                    buffer[4 * i + 1],
                    buffer[4 * i + 2],
                    buffer[4 * i + 3],
                ]) as i64;
            }
        }
    }
    Ok((bytes_read, buffer))
}

/// TTA's OWN candidate sets (tta.cpp:49,52) -- NOT mmdet's.
///
/// `mmdet.cpp` has file-static `channels[] = {1,2,3,4}` / `bitvalues[] =
/// {8,16,24,32}`, and `tta.cpp` has its own file-static arrays of the same
/// names holding {1,2} and {8,16}. Both call `autodetect_by_entropy`, so which
/// set it sees depends purely on which translation unit the caller is in. The
/// difference is not cosmetic: on 32-bit table data the wider set wins with
/// `1 channel x 32 bits`, which TTA then REFUSES (`byte_size >= 4` and not
/// float) and stores, while the narrow set picks `2 x 16` and compresses it
/// 6.7x. Passing mmdet's arrays here stored a 32 KB file that the C compressed
/// to 4,763 bytes.
const TTA_CHANNELS: [c_int; 2] = [1, 2];
const TTA_BITVALUES: [c_int; 2] = [8, 16];

/// `tta_compress` (tta.cpp:294).
#[allow(clippy::too_many_arguments)]
fn encode(
    io: &Io,
    level: c_int,
    skip_header: bool,
    is_float_in: c_int,
    num_chan_in: c_int,
    word_size_in: c_int,
    offset_in: c_int,
    raw_data: c_int,
) -> Result<(), c_int> {
    // Samples per chunk. Not part of the format -- each block records its own
    // byte count -- but the decoder sizes its buffers from the recorded count,
    // so keeping it identical keeps the stream identical.
    const FRAME_SIZE: usize = 1 << 18;

    let level = level.min(3);
    let mut is_float = is_float_in != 0;
    let mut num_chan = num_chan_in;
    let mut word_size = word_size_in;
    let mut offset = offset_in;

    // `prev` holds the megabyte read for autodetection; it is consumed by the
    // first frames before any further reads happen.
    let mut prev: Vec<u8> = Vec::new();
    let mut detected = true;

    if level == 0 {
        detected = false;
    } else if is_float || num_chan != 0 || word_size != 0 {
        if num_chan == 0 {
            num_chan = 1;
        }
        if word_size == 0 {
            // NOTE: 4 and 1, not 32 and 8 -- unlike mm_compress, whose identical
            // looking line means BITS. Here the value is fed to (word_size+7)/8
            // all the same, so `4` yields byte_size 1 for floats. Transliterated,
            // not corrected: it is what the C writes into the header.
            word_size = if is_float { 4 } else { 1 };
        }
    } else {
        prev = vec![0u8; MB];
        let n = io.read(&mut prev);
        if n <= 0 {
            return if n < 0 { Err(n) } else { Ok(()) };
        }
        prev.truncate(n as usize);

        let wav = if skip_header {
            None
        } else {
            mmdet::autodetect_wav_header(&prev)
        };
        // TTA uses a LOOSER entropy threshold than MM: 0.50 against MM's 0.80.
        let d = wav.or_else(|| {
            mmdet::autodetect_by_entropy(&prev, &TTA_CHANNELS, &TTA_BITVALUES, 0.50)
        });
        match d {
            Some(d) => {
                is_float = d.is_float;
                num_chan = d.num_chan;
                word_size = d.word_size;
                offset = d.offset;
            }
            None => detected = false,
        }
    }

    let byte_size = ((word_size + 7) / 8) as usize;

    // TTA handles neither 32-bit integers nor non-32-bit floats.
    if detected && ((is_float && byte_size != 4) || (!is_float && byte_size >= 4)) {
        detected = false;
    }
    // A width TTA has no filter for falls back to storing, exactly like an
    // undetected stream. Folded into the same guard so there is one place that
    // decides, and note this also covers byte_size 0, which the checks above do
    // not reject for integers.
    if detected && SampleBytes::from_byte_size(byte_size).is_none() {
        detected = false;
    }

    if !detected {
        // Header of 0, then the input verbatim.
        io.write_all(&0u32.to_le_bytes())?;
        loop {
            if !prev.is_empty() {
                io.write_all(&prev)?;
            }
            prev.resize(MB, 0);
            let n = io.read(&mut prev);
            if n < 0 {
                return Err(n);
            }
            if n == 0 {
                return Ok(());
            }
            prev.truncate(n as usize);
        }
    }

    // Past the early return, so the width is one TTA filters. `ok_or` rather than
    // an unwrap or an `unreachable!()`: it is a real error path, so a future
    // reordering of the guards above fails loudly instead of panicking across the
    // FFI boundary.
    let bytes = SampleBytes::from_byte_size(byte_size).ok_or(BAD)?;

    let num_chan = num_chan as usize;
    io.write_all(&[
        level as u8,
        (raw_data * 2) as u8 + is_float as u8,
        num_chan as u8,
        word_size as u8,
    ])?;

    // The original file header, passed through uncompressed. If autodetection
    // never ran there is nothing buffered, so it has to be read now.
    let offset = offset.max(0) as usize;
    if offset > 0 && prev.is_empty() {
        prev = vec![0u8; offset];
        read_exact(io, &mut prev)?;
    }
    io.write_all(&(offset as u32).to_le_bytes())?;
    let head = offset.min(prev.len());
    io.write_all(&prev[..head])?;
    let mut prev_pos = head; // the rest of `prev` still feeds the frames

    let rows = num_chan << is_float as usize;
    let mut data = vec![0i64; num_chan * FRAME_SIZE];
    let mut buffer = alloc2d(rows, FRAME_SIZE)?;
    let mut rest: Vec<u8> = Vec::new();

    loop {
        let (bytes_read, origdata) = read_wave(
            io,
            &mut data,
            &mut rest,
            &prev[prev_pos.min(prev.len())..],
            bytes,
            num_chan,
            FRAME_SIZE,
        )?;
        let sample = num_chan * bytes.width();
        let frame_len = bytes_read / sample;

        if bytes_read >= prev.len() - prev_pos.min(prev.len()) {
            prev.clear();
            prev_pos = 0;
        } else {
            prev_pos += bytes_read;
        }
        if bytes_read == 0 {
            return Ok(());
        }

        io.write_all(&(bytes_read as u32).to_le_bytes())?;

        if is_float {
            split_float(&data, frame_len, num_chan, &mut buffer);
        } else {
            split_int(&data, frame_len, num_chan, &mut buffer);
        }

        let mut bw = BitWriter::new();
        for row in buffer.iter_mut().take(rows) {
            filters_compress(&mut row[..frame_len], level as usize, bytes);

            if raw_data == 0 {
                encode_frame(&mut bw, &row[..frame_len]);
            } else {
                if raw_data == 2 {
                    for v in row[..frame_len].iter_mut() {
                        *v = if *v >= 0 { v.wrapping_mul(2) } else { v.wrapping_neg().wrapping_mul(2).wrapping_sub(1) };
                    }
                }
                // C writes frame_len*sizeof(long) bytes straight out of the
                // buffer -- EIGHT bytes per sample on LP64, four on Win32. This
                // path is a debugging aid (`:r1`/`:r2`), never produced by the
                // archiver's defaults, and its stream was never portable across
                // word sizes. Matched to the LP64 build this replaces.
                let mut out = Vec::with_capacity(frame_len * 8);
                for &v in row[..frame_len].iter() {
                    out.extend_from_slice(&v.to_le_bytes());
                }
                io.write_all(&out)?;
            }
        }

        if raw_data == 0 {
            let size = bw.len();
            if size >= bytes_read {
                // The coded frame came out no smaller than the input: store it.
                io.write_all(&0u32.to_le_bytes())?;
                io.write_all(&origdata[..bytes_read])?;
                continue; // NB: skips the `rest` write -- origdata already has it
            }
            io.write_all(&(size as u32).to_le_bytes())?;
            io.write_all(&bw.bytes())?;
        }
        io.write_all(&rest)?;
    }
}

/// `tta_compress`.
#[allow(clippy::too_many_arguments)]
pub fn compress(
    io: &Io,
    level: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    raw_data: c_int,
) -> c_int {
    match encode(
        io,
        level,
        skip_header != 0,
        is_float,
        num_chan,
        word_size,
        offset,
        raw_data,
    ) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

#[cfg(test)]
mod rice_tests {
    use super::*;

    /// Drive the Rice parameter to its ceiling and back.
    ///
    /// The clamp at 32 cannot be reached from the differential corpus: it needs
    /// `sum0 > 2^31`, i.e. residuals near 2^30, and audio-shaped input never
    /// produces those. It is still load-bearing, and in a way the C's own
    /// symptom hid -- `shift_16` SATURATES (`BIT_SHIFT[36] == BIT_SHIFT[37] ==
    /// 0x80000000`), so the threshold for 32 -> 33 is identical to the one for
    /// 31 -> 32. Once the parameter reaches the top it would climb forever,
    /// indexing `BIT_MASK32[33]` and beyond off the end of a 33-entry table.
    ///
    /// Removing the clamp leaves the whole differential test green, so this is
    /// the only thing standing between that edit and an out-of-bounds index.
    #[test]
    fn rice_parameter_saturates_instead_of_running_off_the_table() {
        // Residuals large enough to push both adaptive tracks to the ceiling,
        // then small ones so they wind back down.
        let mut data: Vec<i64> = Vec::new();
        for _ in 0..600 {
            data.push(1 << 30);
        }
        for i in 0..600 {
            data.push(if i % 2 == 0 { 3 } else { -4 });
        }

        let mut bw = BitWriter::new();
        encode_frame(&mut bw, &data);
        let bytes = bw.bytes();
        assert!(!bytes.is_empty());

        // And the decoder must recover exactly what went in -- the two sides
        // adapt in lock-step only if both clamp.
        let mut br = BitReader::new(&bytes);
        let mut out = vec![0i64; data.len()];
        decode_frame(&mut br, &mut out);
        assert_eq!(out, data, "encode/decode disagree at the Rice ceiling");
    }

    /// The 50-run escape: anything longer goes out as a 50 unary run plus a
    /// 32-bit literal, and the reader has to take the same branch.
    #[test]
    fn long_unary_runs_take_the_escape_and_round_trip() {
        let data: Vec<i64> = (0..400).map(|i| (i as i64 % 7) * (1 << 20)).collect();
        let mut bw = BitWriter::new();
        encode_frame(&mut bw, &data);
        let bytes = bw.bytes();
        let mut br = BitReader::new(&bytes);
        let mut out = vec![0i64; data.len()];
        decode_frame(&mut br, &mut out);
        assert_eq!(out, data);
    }

    /// `put_unary` writes `value` ones then a zero, across word boundaries at
    /// every starting bit offset. The word-spanning branch is the one with the
    /// `> 32` loop bound.
    #[test]
    fn unary_round_trips_at_every_bit_offset() {
        for pad in 0..33u64 {
            for value in [0u64, 1, 31, 32, 33, 63, 64, 65, 100] {
                let mut bw = BitWriter::new();
                if pad > 0 {
                    bw.put_binary(0, pad);
                }
                bw.put_unary(value);
                let bytes = bw.bytes();
                let mut br = BitReader::new(&bytes);
                if pad > 0 {
                    br.get_binary(pad);
                }
                assert_eq!(br.get_unary(), value, "pad={pad} value={value}");
            }
        }
    }
}
