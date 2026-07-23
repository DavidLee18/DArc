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
fn filters_decompress(data: &mut [i64], level: usize, byte_size: usize) {
    let f1 = FLT_SET[0][level - 1][byte_size - 1];
    let f2 = FLT_SET[1][level - 1][byte_size - 1];
    let f3 = FLT_SET[2][level - 1][byte_size - 1];
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

        match byte_size {
            1 => *p += predictor1(last, 4),
            2 => *p += predictor1(last, 5),
            3 => *p += predictor1(last, 5),
            4 => *p += last,
            _ => {}
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
    byte_size: usize,
    num_chan: usize,
    frame_len: usize,
    buffer: &[Vec<i64>],
) -> Result<(), c_int> {
    let mut out = vec![0u8; frame_len * num_chan * byte_size];
    for i in 0..frame_len {
        for n in 0..num_chan {
            let v = buffer[n][i];
            let base = (i * num_chan + n) * byte_size;
            match byte_size {
                1 => out[base] = (v + 0x80) as u8,
                2 => out[base..base + 2].copy_from_slice(&(v as i16).to_le_bytes()),
                3 => {
                    let u = v as u32;
                    out[base] = u as u8;
                    out[base + 1] = (u >> 8) as u8;
                    out[base + 2] = (u >> 16) as u8;
                }
                4 => out[base..base + 4].copy_from_slice(&(v as i32).to_le_bytes()),
                _ => {}
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
    let sample_set = num_chan * byte_size; // bytes per (all-channels) sample

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
                filters_decompress(row, level, byte_size);
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
                    *slot = i64::from_le_bytes(chunk.try_into().unwrap());
                }
                filters_decompress(row, level, byte_size);
            }
        }

        if is_float != 0 {
            combine_float(frame_len, num_chan, &mut buffer);
        } else {
            combine_int(frame_len, num_chan, &mut buffer);
        }

        if frame_len != 0 {
            write_wave(io, byte_size, num_chan, frame_len, &buffer)?;
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
