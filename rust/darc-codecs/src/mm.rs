//! MM multimedia preprocessor decoder, ported from `Compression/MM/mm.cpp`
//! (`mm_decompress` and the four `undiff` routines it reaches).
//!
//! MM is not a compressor: it is a *filter* that subtracts each multimedia
//! sample from the previous sample of the same channel, so that whatever runs
//! after it in the `+`-chain sees small deltas instead of raw waveform. The
//! archiver reaches it as `-mmm`. Decoding is the inverse -- a running sum per
//! channel -- which is why this module is a fraction of the size of `tta.rs`
//! despite the two codecs sharing a directory.
//!
//! Both directions are ported, decoder first -- the order used for REP, Dict,
//! LZP and TTA: a Rust build must *read* every existing `-mmm` archive before it
//! may write one. The encoder lives at the bottom of this file and its
//! autodetection in `mmdet.rs`, which is the bulk of it: the detector chooses
//! `num_chan`/`word_size`/`offset`, and those travel in the stream header, so
//! it decides archive bytes rather than merely compression ratio.
//!
//! The C `mm_compress` is excluded under `DARC_RUST`, but `mmdet.cpp` is NOT
//! deleted: `tta.cpp` calls its two autodetect entry points directly, so it
//! survives until TTA's encoder is ported too.
//!
//! ## The stream
//!
//! A single flags byte, then either nothing more (`0` = the payload is stored
//! verbatim) or a two-byte `num_chan`/`word_size` pair. After that comes a
//! 4-byte `offset` and that many bytes of the original file header, copied
//! through untouched -- this is what lets the filter start on a sample
//! boundary for e.g. a `.wav` whose 44-byte header would otherwise skew every
//! channel. Then zero padding up to a multiple of the sample size, then the
//! deltas, to end of stream.
//!
//! Bit 0 of the flags byte is the diff, bit 1 the byte reordering (`:r1`), which
//! this port completed in both directions -- it could previously be written but
//! never read. Bit 2 was `reorder_words`, whose C implementation is
//! `return buf;`, and stays rejected rather than accepted as a no-op.
//!
//! ## Widths, and why little-endian is exact
//!
//! Every accumulator is the width of one sample -- `u8`, `u16`, 24-bit, `u32` --
//! and all of them wrap. In C that is unsigned arithmetic on `uint16`/`uint32`
//! (and plain `char` for the 8-bit case, where the add is mod 256 either way);
//! here every one is a `wrapping_add`, because a Rust `+` would panic in debug
//! on input that the format not only permits but produces constantly.
//!
//! The C reads samples by casting the buffer to `uint16*`/`uint32*`, i.e. in
//! *native* order, so an MM stream is only portable across builds of the same
//! byte order. Every DArc target is little-endian, and the format's own
//! accessors (`value24`/`value32`, Common.h:285) are defined little-endian for
//! the non-Intel path, so `from_le_bytes` here is exact rather than merely
//! equivalent.

use crate::ffi::{
    Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_GENERAL, FREEARC_ERRCODE_IO, OK,
};
use crate::mmdet;
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;
const GENERAL: c_int = FREEARC_ERRCODE_GENERAL as c_int;

/// `BUFFER_SIZE` (Compression.h:38). Part of the format, not a tuning knob:
/// the encoder emits its second and later blocks at `roundDown(BUFFER_SIZE,N)`,
/// and the decoder must ask for exactly the same amount.
const BUFFER_SIZE: usize = 64 << 10;

/// Bytes per channel word, as MM's differencing filters understand it.
///
/// Derived from the header's `word_size` byte -- `(word_size + 7) / 8` -- so it
/// spans **0..=32**, not 1..=4: `word_size` is a single header byte, so a crafted
/// stream can ask for any width. Only 1..=4 name a filter, and the C's
/// `switch (byte_size)` has cases 1-4 and NO default (`mm.cpp:207` encoding,
/// `:311` decoding), so every other width passes the data through unchanged.
///
/// `Option<WordBytes>` rather than a fifth `Unknown` variant, for the reason
/// settled in #107: "not a filtered width" is a parse outcome, not a width, and
/// an `Unknown` variant would be dead on any path that computes the value itself.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum WordBytes {
    /// 8-bit samples.
    One,
    /// 16-bit.
    Two,
    /// 24-bit, whose base stride is 4 rather than 3 -- see `base_stride`.
    Three,
    /// 32-bit.
    Four,
}

impl WordBytes {
    /// Classify a `byte_size`. `None` means no filter applies, which is the C's
    /// fall-through rather than an error.
    fn from_byte_size(n: usize) -> Option<WordBytes> {
        match n {
            1 => Some(WordBytes::One),
            2 => Some(WordBytes::Two),
            3 => Some(WordBytes::Three),
            4 => Some(WordBytes::Four),
            _ => None,
        }
    }
}

/// The largest reorder block the encoder can emit: `BUFSIZE` in `mm.cpp`, the
/// size of its first read. Bounds the buffer growth in the reorder path so a
/// declared length cannot choose the allocation size.
const MAX_REORDER_BLOCK: usize = 1 << 20;

/// The header offset is a 32-bit field read into a C `int`, so the C decoder
/// happily takes a value that makes `malloc` fail or go negative. No encoder
/// can emit more than the 1 MB first-block size, so anything past this is
/// corrupt; rejecting it keeps a hostile stream from driving an allocation.
const MAX_OFFSET: u32 = 1 << 30;

// ---------------------------------------------------------------------------
// Stream helpers, mirroring the READ / READ4 / WRITE macros at
// Compression.h:83-130. Note WRITE is the *lenient* one -- it fails only on a
// negative return, because the write callback is documented to either take
// everything or fail -- while READ demands the exact count it asked for.
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

/// A 32-bit length, or `None` at a clean end of stream.
///
/// Distinguishing "no more blocks" from "a truncated length" matters here: the
/// reorder path reads an explicit block length, so a stream that simply ends is
/// normal while one that ends mid-length is corrupt.
fn read_u32_or_eof(io: &Io) -> Result<Option<u32>, c_int> {
    let mut b = [0u8; 4];
    let mut got = 0usize;
    while got < 4 {
        let n = io.read(&mut b[got..]);
        if n < 0 {
            return Err(n);
        }
        if n == 0 {
            return if got == 0 { Ok(None) } else { Err(BAD) };
        }
        got += n as usize;
    }
    Ok(Some(u32::from_le_bytes(b)))
}

fn write_out(io: &Io, buf: &[u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.write(buf) {
        n if n < 0 => Err(n),
        _ => Ok(()),
    }
}

/// `roundDown`/`roundUp` (Common.h:494-503). The `b > 1` guard is theirs, and
/// it matters: it is the reason a stream declaring `word_size == 0` divides by
/// nothing and simply copies through instead of trapping.
fn round_down(a: usize, b: usize) -> usize {
    if b > 1 {
        a / b * b
    } else {
        a
    }
}

fn round_up(a: usize, b: usize) -> usize {
    if b > 1 {
        round_down(a - 1, b) + b
    } else {
        a
    }
}

// ---------------------------------------------------------------------------
// The inverse filters. Each walks the buffer in samples of `n` channels and
// replaces every element with a running per-channel sum. `base` holds those
// sums across chunk boundaries -- and across the whole stream, which is why it
// is threaded through the read loop rather than being local to one call.
//
// The trailing bytes of a partial sample are left untouched, exactly as the
// pointer conditions in C do (`p+N <= buf+bufsize`).
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Forward filters -- diff1..diff4 (`mm.cpp:19-77`), the exact mirrors of the
// undiff family below.
//
// Each replaces every element with its difference from the previous element in
// the SAME channel, so `base` carries one running value per channel and
// persists across calls for the whole stream. The order matters and is easy to
// get subtly wrong: the C does `x = p[i]; p[i] -= base[i]; base[i] = x;` -- the
// ORIGINAL value becomes the new base, not the difference just written.
//
// Every arithmetic op wraps. In C these are unsigned types where overflow is
// defined; a Rust `-` would panic in debug, so `wrapping_sub` is required for
// equivalence rather than as a style choice.
//
// The loop bound mirrors the C pointer condition `p + N <= buf + bufsize`, so a
// trailing partial element is left untouched -- not rounded up, not padded.
// ---------------------------------------------------------------------------

/// Inverse of `reorder_bytes` (`mm.cpp:80`).
///
/// The forward transform is a byte transpose: with `x = n * width` bytes per
/// sample and `r = len / x` whole samples, it sends `buf[i + j*x]` to
/// `out[i*r + j]`, gathering the k-th byte of every sample into one run. That
/// is what makes it pay -- the high bytes of a slowly-varying signal become long
/// similar runs for the entropy coder. The trailing `len % x` bytes are copied
/// straight through, not transposed.
///
/// **This transform is why the reorder flag needs a length prefix in the
/// stream.** Unlike diff/undiff, which are streaming and carry their state in
/// `base`, the permutation is parameterised by the block length: invert it with
/// a different `len` and every byte lands in the wrong place. The encoder's
/// blocks are not a fixed size -- the first is up to 1 MB (`BUFSIZE`) minus the
/// header offset, later ones are `roundDown(BUFFER_SIZE, N)` = 64 KB, and the
/// last is whatever remains -- and none of that is recoverable from the old
/// stream. That, not the missing inverse, is why the flag was never finished.
fn unreorder_bytes(buf: &mut [u8], n: usize, width: usize) {
    let x = n * width;
    if x == 0 || buf.len() < x {
        return; // nothing transposed; the whole block is "tail"
    }
    let len = buf.len();
    let r = len / x;
    let mut out = vec![0u8; len];
    for i in 0..x {
        for j in 0..r {
            out[i + j * x] = buf[i * r + j];
        }
    }
    let tail = len - (len % x);
    out[tail..].copy_from_slice(&buf[tail..]);
    buf.copy_from_slice(&out);
}

fn diff1(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0;
    while p + n <= buf.len() {
        for i in 0..n {
            let x = buf[p + i];
            buf[p + i] = x.wrapping_sub(base[i]);
            base[i] = x;
        }
        p += n;
    }
}

fn diff2(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 16-bit words
    while (p + n) * 2 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 2;
            let x = u16::from_le_bytes([buf[at], buf[at + 1]]);
            let prev = u16::from_le_bytes([base[i * 2], base[i * 2 + 1]]);
            buf[at..at + 2].copy_from_slice(&x.wrapping_sub(prev).to_le_bytes());
            base[i * 2..i * 2 + 2].copy_from_slice(&x.to_le_bytes());
        }
        p += n;
    }
}

fn diff3(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in bytes
    while p + n * 3 <= buf.len() {
        for i in 0..n {
            let at = p + i * 3;
            let b = i * 4;
            let x = u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], 0]);
            let prev = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]]);
            // setvalue24 writes three bytes; the fourth is not ours to touch.
            buf[at..at + 3].copy_from_slice(&x.wrapping_sub(prev).to_le_bytes()[..3]);
            base[b..b + 4].copy_from_slice(&x.to_le_bytes());
        }
        p += n * 3;
    }
}

fn diff4(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 32-bit words
    while (p + n) * 4 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 4;
            let b = i * 4;
            let x = u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]]);
            let prev = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]]);
            buf[at..at + 4].copy_from_slice(&x.wrapping_sub(prev).to_le_bytes());
            base[b..b + 4].copy_from_slice(&x.to_le_bytes());
        }
        p += n;
    }
}

fn undiff1(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0;
    while p + n <= buf.len() {
        for i in 0..n {
            base[i] = base[i].wrapping_add(buf[p + i]);
            buf[p + i] = base[i];
        }
        p += n;
    }
}

fn undiff2(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 16-bit words
    while (p + n) * 2 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 2;
            let acc = u16::from_le_bytes([base[i * 2], base[i * 2 + 1]])
                .wrapping_add(u16::from_le_bytes([buf[at], buf[at + 1]]));
            base[i * 2..i * 2 + 2].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 2].copy_from_slice(&acc.to_le_bytes());
        }
        p += n;
    }
}

/// 24-bit samples, with 32-bit accumulators -- `base` is a `uint32*` in C even
/// here, hence the 4-byte stride. C reads each sample with `value24`, a masked
/// 32-bit load that touches one byte past the last sample (which is what the
/// `+1` on mm.cpp's `malloc` is for); reading three bytes explicitly gives the
/// same value with no over-read to justify.
fn undiff3(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in bytes
    while p + n * 3 <= buf.len() {
        for i in 0..n {
            let at = p + i * 3;
            let b = i * 4;
            let acc = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]])
                .wrapping_add(u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], 0]));
            base[b..b + 4].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 3].copy_from_slice(&acc.to_le_bytes()[..3]);
        }
        p += n * 3;
    }
}

fn undiff4(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 32-bit words
    while (p + n) * 4 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 4;
            let b = i * 4;
            let acc = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]])
                .wrapping_add(u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]]));
            base[b..b + 4].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 4].copy_from_slice(&acc.to_le_bytes());
        }
        p += n;
    }
}

// ---------------------------------------------------------------------------

/// `mm_decompress`.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    let mut header = [0u8; 3];
    read_exact(io, &mut header[..1])?;
    let mut buf = vec![0u8; BUFFER_SIZE];

    // Autodetection found nothing at encode time, so the payload was stored.
    if header[0] == 0 {
        return copy_to_eof(io, &mut buf);
    }

    // Bit 1 is byte reordering. Bit 2 was `reorder_words`, which never existed
    // -- the C function is `return buf;` -- so it is rejected rather than
    // treated as a no-op: a flag that means nothing should not be accepted.
    // Anything above is reserved.
    if (header[0] & !0b11) != 0 {
        return Err(BAD);
    }
    let reorder = header[0] & 0b10 != 0;
    read_exact(io, &mut header[1..3])?;

    // The original file header, copied through untouched. C reads it into one
    // `malloc(offset)`; chunking it moves the same bytes without letting a
    // declared length pick the allocation size.
    let offset = read_u32(io)?;
    if offset > MAX_OFFSET {
        return Err(BAD);
    }
    let mut left = offset as usize;
    while left > 0 {
        let want = left.min(buf.len());
        read_exact(io, &mut buf[..want])?;
        write_out(io, &buf[..want])?;
        left -= want;
    }

    let num_chan = header[1] as usize;
    let word_size = header[2] as usize;
    let byte_size = (word_size + 7) / 8; // bytes per channel word, 0..=32
    let sample = num_chan * byte_size; // `N`: bytes per sample

    // With no channels but a nonzero word size, C's `p += N` never advances and
    // the undiff loop spins forever. Nothing can be bug-compatible with a hang,
    // so treat it as the corrupt header it is. (`word_size == 0` is *not* this
    // case: it makes `byte_size` zero, which C handles as a plain copy below.)
    if num_chan == 0 && byte_size != 0 {
        return Err(BAD);
    }

    // Padding the encoder wrote to align the deltas to a sample boundary. It
    // reads into `base` in C as well -- the bytes are zeros, so the running sums
    // stay zero, and a stream that puts something else there seeds them exactly
    // as C would.
    let base_stride = if byte_size == 3 { 4 } else { byte_size };
    let mut base = vec![0u8; num_chan * base_stride];
    let before = 3 + 4 + offset as usize;
    let pad = round_up(before, sample) - before;
    read_exact(io, &mut base[..pad])?;

    let chunk = round_down(BUFFER_SIZE, sample);
    loop {
        // With reordering the block length is explicit: the permutation is
        // parameterised by it, and the encoder's blocks are not a fixed size.
        // Without it the stream is self-describing by chunk size, as before.
        let got = if reorder {
            let n = match read_u32_or_eof(io)? {
                Some(n) => n as usize,
                None => return Ok(()),
            };
            // The encoder's FIRST reorder block is up to BUFSIZE (1 MB) minus
            // the header offset, while later ones are roundDown(BUFFER_SIZE,N)
            // = 64 KB. So the decoder's fixed 64 KB buffer is not enough and
            // has to grow -- bounded, so a hostile length cannot pick the
            // allocation size.
            if n > MAX_REORDER_BLOCK {
                return Err(BAD);
            }
            if n > buf.len() {
                buf.resize(n, 0);
            }
            read_exact(io, &mut buf[..n])?;
            n as c_int
        } else {
            io.read(&mut buf[..chunk])
        };
        if got < 0 {
            return Err(got);
        }
        if got == 0 {
            return Ok(());
        }
        let data = &mut buf[..got as usize];
        // Undo the transpose before the deltas: the encoder diffed first, then
        // reordered, so decode runs the inverse order.
        if reorder {
            unreorder_bytes(data, num_chan, byte_size);
        }
        match WordBytes::from_byte_size(byte_size) {
            Some(WordBytes::One) => undiff1(data, num_chan, &mut base),
            Some(WordBytes::Two) => undiff2(data, num_chan, &mut base),
            Some(WordBytes::Three) => undiff3(data, num_chan, &mut base),
            Some(WordBytes::Four) => undiff4(data, num_chan, &mut base),
            // No filter exists for 0, or for words wider than 32 bits. The C's
            // `switch` (mm.cpp:311) has no default either, so the data passes
            // through. `byte_size` derives from a header byte, so a crafted
            // stream reaches this: it must stay a quiet no-op, not a panic.
            None => {}
        }
        write_out(io, data)?;
    }
}

fn copy_to_eof(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    loop {
        let got = io.read(buf);
        if got < 0 {
            return Err(got);
        }
        if got == 0 {
            return Ok(());
        }
        write_out(io, &buf[..got as usize])?;
    }
}

// ---------------------------------------------------------------------------
// Encoder -- the port of `mm_compress` (mm.cpp:181).
//
// The filter itself is four `diff` routines and a header. Almost all of the
// work is in deciding WHAT to filter: `mmdet` picks num_chan/word_size/offset,
// and those three go into the stream header, so a detector that disagrees with
// the C detector writes a different archive rather than a slightly worse one.
// ---------------------------------------------------------------------------

/// `BUFSIZE` (mm.cpp:183): the size of the first read, and so the largest block
/// the filter ever emits. Later blocks are `roundDown(BUFFER_SIZE, N)`.
const BUFSIZE: usize = 1 << 20;

/// Forward byte transpose (`reorder_bytes`, mm.cpp:94). See `unreorder_bytes`
/// for why the block length has to travel in the stream.
fn reorder_bytes(buf: &mut [u8], n: usize, width: usize) {
    let x = n * width;
    if x == 0 || buf.len() < x {
        return;
    }
    let len = buf.len();
    let r = len / x;
    let mut out = vec![0u8; len];
    for i in 0..x {
        for j in 0..r {
            out[i * r + j] = buf[i + j * x];
        }
    }
    let tail = len - (len % x);
    out[tail..].copy_from_slice(&buf[tail..]);
    buf.copy_from_slice(&out);
}

fn write_u32(io: &Io, v: u32) -> Result<(), c_int> {
    write_out(io, &v.to_le_bytes())
}

fn encode(
    io: &Io,
    mode: c_int,
    skip_header: bool,
    is_float_in: c_int,
    num_chan_in: c_int,
    word_size_in: c_int,
    offset_in: c_int,
    reorder: c_int,
) -> Result<(), c_int> {
    let mut buf = vec![0u8; BUFSIZE + 1]; // +1 for diff3's 24-bit reads in C
    let got = io.read(&mut buf[..BUFSIZE]);
    if got <= 0 {
        // C falls straight to `finished` and returns the read's own result, so
        // an empty input produces an EMPTY stream -- not even a flags byte.
        return if got < 0 { Err(got) } else { Ok(()) };
    }
    let bytes = got as usize;

    // How much to examine, and which model sets to try. Modes 1-2 are the
    // "fast" settings and look at less data with fewer candidate models.
    let check_bytes = core::cmp::min(
        if mode <= 2 {
            64 * 1024
        } else {
            core::cmp::max(64 * 1024, bytes / 2)
        },
        bytes,
    );
    let (use_channels, use_bitvalues): (&[c_int], &[c_int]) = if mode <= 2 {
        (&mmdet::FAST_CHANNELS, &mmdet::FAST_BITVALUES)
    } else {
        (&mmdet::CHANNELS, &mmdet::BITVALUES)
    };

    let mut is_float = is_float_in != 0;
    let mut num_chan = num_chan_in;
    let mut word_size = word_size_in;
    let mut offset = offset_in;

    if is_float || num_chan != 0 || word_size != 0 {
        // Caller pinned the model; fill in whichever half they left out.
        if num_chan == 0 {
            num_chan = 1;
        }
        if word_size == 0 {
            word_size = if is_float { 32 } else { 8 };
        }
    } else {
        let detected = (if skip_header {
            None
        } else {
            mmdet::autodetect_wav_header(&buf[..bytes])
        })
        .or_else(|| {
            // The entropy analyser looks at a window in the MIDDLE of the
            // block, not the start: file headers and leading silence are the
            // least representative part of a media file.
            let start = (bytes - check_bytes) / 2;
            mmdet::autodetect_by_entropy(
                &buf[start..start + check_bytes],
                use_channels,
                use_bitvalues,
                0.80,
            )
        });
        match detected {
            Some(d) => {
                is_float = d.is_float;
                num_chan = d.num_chan;
                word_size = d.word_size;
                offset = d.offset;
            }
            // Neither detector recognised it: store the data intact.
            None => {
                is_float = false;
                num_chan = 0;
                word_size = 0;
                offset = 0;
            }
        }
    }
    // is_float only ever picks a default word size. It is NOT recorded in the
    // stream -- float samples are filtered as 32-bit integers.
    let _ = is_float;

    if offset > bytes as c_int {
        offset = bytes as c_int; // else we would write a negative length
    }
    let offset = offset as usize;
    let byte_size = ((word_size + 7) / 8) as usize;
    let chan = num_chan as usize;
    let n = chan * byte_size; // bytes per sample

    // Don't filter a trailing partial sample in the first block; it is carried
    // into the second one instead.
    let mut blk = round_down(bytes - offset, n);
    let mut rest = bytes - offset - blk;

    // Room for the previous value of every channel. 24-bit samples accumulate
    // in 32 bits, hence the 4.
    let mut base = vec![0u8; chan * if byte_size == 3 { 4 } else { byte_size }];

    if n == 0 {
        // A single '\0' flags byte, then the data verbatim.
        write_out(io, &[0u8])?;
    } else {
        // bit0 = diff, bit1 = reorder_bytes. num_chan is written as ONE byte,
        // so a WAV claiming 256 channels wraps to 0 here -- the C does the same
        // and the format has no room for more.
        write_out(io, &[(1 + reorder * 2) as u8, num_chan as u8, word_size as u8])?;
        write_u32(io, offset as u32)?;
        write_out(io, &buf[..offset])?;
        // Pad to a multiple of the sample size so the filter starts aligned.
        // `base` is still all zeros and is what C writes these from.
        let sofar = 3 + 4 + offset;
        let pad = round_up(sofar, n) - sofar;
        write_out(io, &base[..pad])?;
    }

    let mut ptr = offset; // first block skips the copied-through header
    loop {
        {
            let data = &mut buf[ptr..ptr + blk];
            match WordBytes::from_byte_size(byte_size) {
                Some(WordBytes::One) => diff1(data, chan, &mut base),
                Some(WordBytes::Two) => diff2(data, chan, &mut base),
                Some(WordBytes::Three) => diff3(data, chan, &mut base),
                Some(WordBytes::Four) => diff4(data, chan, &mut base),
                // Verified against the pinned C: `switch (byte_size)` has cases
                // 1-4 and NO default at `mm.cpp:207`, so a width outside 1..=4
                // passes through unchanged there too.
                //
                // (An earlier revision of this comment cited MM/tta.cpp:133,:183.
                // Those are TTA's switches, which document tta.rs; MM's own are in
                // mm.cpp.)
                None => {}
            }
            // reorder_words (:r2) is not resurrected: the C is `return buf;`.
            if n != 0 && reorder == 1 {
                reorder_bytes(data, chan, byte_size);
            }
        }
        // The length prefix goes with the flag, and the flag only exists on the
        // filtered path -- the stored path's header is a bare '\0' with nowhere
        // to record it, and its decoder copies straight to EOF. Emitting a
        // prefix there would corrupt the stream.
        if n != 0 && reorder != 0 {
            write_u32(io, blk as u32)?;
        }
        write_out(io, &buf[ptr..ptr + blk])?;

        // Carry whatever did not make a whole sample into the next block.
        buf.copy_within(ptr + blk..ptr + blk + rest, 0);

        let chunk = if n > 1 {
            round_down(BUFFER_SIZE, n)
        } else {
            BUFFER_SIZE
        };
        // A sample larger than the chunk size leaves nothing to read into. C
        // passes the resulting negative count to the callback and gets an error
        // back; here it would be a panic across the FFI boundary, so it is
        // caught. Only reachable from a WAV header claiming thousands of
        // channels.
        if chunk <= rest {
            return Err(GENERAL);
        }
        buf.resize(chunk + 1, 0);
        ptr = 0;

        let got = io.read(&mut buf[rest..chunk]);
        if got < 0 {
            return Err(got);
        }
        blk = got as usize + rest;
        rest = 0;
        if blk == 0 {
            break;
        }
    }
    Ok(())
}

/// `mm_compress`.
pub fn compress(
    io: &Io,
    mode: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    reorder: c_int,
) -> c_int {
    match encode(
        io,
        mode,
        skip_header != 0,
        is_float,
        num_chan,
        word_size,
        offset,
        reorder,
    ) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

#[cfg(test)]
mod diff_tests {
    /// `byte_size` is `(word_size + 7) / 8` over a header BYTE, so it spans
    /// 0..=32 and nothing validates it down to 1..=4.
    ///
    /// The *behaviour* of the unclassified widths is already covered, by
    /// `tests/mm.rs`'s `zero_word_size_copies_through_as_c_does` and
    /// `oversized_word_size_copies_through` -- both fail if the `None` arm is
    /// given work to do, which I checked rather than assumed. What this adds is
    /// the mapping itself: that 1..=4 and *only* 1..=4 classify, out to
    /// `usize::MAX`, which no stream-level test pins.
    ///
    /// Worth knowing: `mm-check.sh` reaches none of this. It builds only streams
    /// an encoder would emit, so `byte_size` is always 1..=4 there.
    #[test]
    fn word_bytes_classifies_only_the_four_filtered_widths() {
        use super::WordBytes;
        assert_eq!(WordBytes::from_byte_size(1), Some(WordBytes::One));
        assert_eq!(WordBytes::from_byte_size(2), Some(WordBytes::Two));
        assert_eq!(WordBytes::from_byte_size(3), Some(WordBytes::Three));
        assert_eq!(WordBytes::from_byte_size(4), Some(WordBytes::Four));
        // 0 is `word_size == 0`; 5..=32 are the wider words a crafted header can
        // declare. Both are plain-copy territory in the C, which has no default.
        assert_eq!(WordBytes::from_byte_size(0), None);
        for n in 5..=32 {
            assert_eq!(WordBytes::from_byte_size(n), None, "byte_size {n}");
        }
        // And well past anything a header can produce, so the guard is total.
        for n in [33usize, 64, 255, usize::MAX] {
            assert_eq!(WordBytes::from_byte_size(n), None, "byte_size {n}");
        }
    }

    use super::*;

    /// diff then undiff must be the identity for every width and channel count,
    /// INCLUDING inputs with a trailing partial element -- the loop bound leaves
    /// those untouched, and an off-by-one there would corrupt the tail.
    #[test]
    fn diff_and_undiff_are_inverse() {
        for width in 1..=4usize {
            for n in 1..=4usize {
                for extra in 0..width * n {
                    let len = width * n * 7 + extra;
                    let orig: Vec<u8> = (0..len).map(|i| (i * 31 + 7) as u8).collect();
                    let mut buf = orig.clone();
                    let mut enc_base = [0u8; 64];
                    match width {
                        1 => diff1(&mut buf, n, &mut enc_base),
                        2 => diff2(&mut buf, n, &mut enc_base),
                        3 => diff3(&mut buf, n, &mut enc_base),
                        _ => diff4(&mut buf, n, &mut enc_base),
                    }
                    let mut dec_base = [0u8; 64];
                    match width {
                        1 => undiff1(&mut buf, n, &mut dec_base),
                        2 => undiff2(&mut buf, n, &mut dec_base),
                        3 => undiff3(&mut buf, n, &mut dec_base),
                        _ => undiff4(&mut buf, n, &mut dec_base),
                    }
                    assert_eq!(buf, orig, "width={width} n={n} extra={extra}");
                }
            }
        }
    }

    /// The base must become the ORIGINAL value, not the difference just stored.
    /// Getting that backwards still round-trips, so a round-trip test alone
    /// cannot catch it -- pin the encoded bytes.
    #[test]
    fn base_tracks_the_original_not_the_difference() {
        let mut buf = vec![10u8, 30, 60, 100];
        let mut base = [0u8; 64];
        diff1(&mut buf, 1, &mut base);
        assert_eq!(buf, vec![10, 20, 30, 40]);
        assert_eq!(base[0], 100, "base must hold the last ORIGINAL value");
    }

    /// Wrapping is required, not stylistic: a Rust `-` would panic in debug on
    /// any decreasing sequence, which is most real signal data.
    #[test]
    fn decreasing_input_wraps_rather_than_panics() {
        let mut buf = vec![5u8, 1, 250, 3];
        let mut base = [0u8; 64];
        diff1(&mut buf, 1, &mut base);
        assert_eq!(buf[1], 252);
    }
}
