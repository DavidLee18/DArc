//! Multimedia autodetection -- the port of `Compression/MM/mmdet.cpp`.
//!
//! This decides `num_chan`, `word_size` and `offset`, and those three go
//! straight into the MM stream header. It is therefore **archive-byte-visible**:
//! a detector that picks a different model produces a different archive, not
//! merely a worse one.
//!
//! ## What is ported, and what is not
//!
//! `mmdet.cpp` is 1,144 lines, but `autodetect_by_entropy` reaches only two of
//! its scans: `run_order0()` and `diff_run`. `Model::run` for 16/24/32-bit, and
//! the LZP/ROLZ/LZ77 models, are called only from that file's own `main()` --
//! a standalone research tool that is not built into the archiver. They are not
//! ported. Confirmed by sabotage: flipping the 24-bit non-diff scan from signed
//! to unsigned reads changed no output anywhere, because nothing calls it.
//!
//! ## Arithmetic transliterated rather than tidied
//!
//! `calc_results` divides `total / count` as INTEGERS before taking the
//! logarithm, and `result` truncates `xbits / 8` before adding it and then
//! truncates the sum. Both are the original's and both are kept -- but neither
//! is load-bearing in the way it first appears: for the distributions this sees,
//! the truncation shifts the estimate by well under 0.01%, and every threshold
//! it feeds is compared with far more margin than that. Sabotaging either one
//! changes no archive byte across the whole differential corpus, including
//! inputs built to sit on the order-0 gate. They are preserved because matching
//! the C exactly is free here, not because a rounding difference is known to
//! matter.
//!
//! The C is compiled into `mm.cpp` by `#include`, and is also called by
//! `tta.cpp`, which is why the C file survives this port: TTA's encoder still
//! needs it.

use core::ffi::c_int;

/// Counter slots per channel.
const STATSIZE: usize = 1024;

/// Channel counts autodetection will try, in order. Zero-terminated in C.
pub const CHANNELS: [c_int; 4] = [1, 2, 3, 4];
/// Word sizes autodetection will try, in order.
pub const BITVALUES: [c_int; 4] = [8, 16, 24, 32];
/// The reduced sets used by the fast modes (`mode <= 2`).
pub const FAST_CHANNELS: [c_int; 2] = [1, 3];
pub const FAST_BITVALUES: [c_int; 2] = [8, 16];

/// Read an unsigned 24-bit little-endian value.
#[inline]
fn unsigned24(b: &[u8]) -> u32 {
    (b[0] as u32) | ((b[1] as u32) << 8) | ((b[2] as u32) << 16)
}

#[inline]
fn i16le(b: &[u8]) -> i16 {
    i16::from_le_bytes([b[0], b[1]])
}

#[inline]
fn u32le(b: &[u8]) -> u32 {
    u32::from_le_bytes([b[0], b[1], b[2], b[3]])
}

#[inline]
fn u16le(b: &[u8]) -> u16 {
    u16::from_le_bytes([b[0], b[1]])
}

/// One candidate model, and the estimate of how well it would compress.
///
/// `stats` is dropped by `calc_results` in C (`FreeAndNil`) before the struct is
/// ever copied into `best_model`, so the copy never carries the table. Here the
/// table simply is not a field of the result: `finish` consumes it.
#[derive(Clone, Copy)]
pub struct ModelResult {
    pub channels: c_int,
    pub bitwidth: c_int,
    pub offset: c_int,
    pub result: i64,
}

struct Model {
    channels: usize,
    bitwidth: c_int,
    offset: c_int,
    /// Bits used for the small values, as a real number.
    bits: f64,
    /// Extra bits for values outside a single byte. `unsigned long` in C.
    xbits: u64,
    stats: Vec<i64>, // channels * STATSIZE, row-major
}

impl Model {
    fn start(channels: usize, bitwidth: c_int, offset: c_int) -> Model {
        Model {
            channels,
            bitwidth,
            offset,
            bits: 0.0,
            // "additional space probably required for huffman tables (very
            // estimated)" -- an int product in C, small enough never to overflow
            // for the channel counts and widths this tries.
            xbits: (channels as u64) * 128 * (bitwidth as u64),
            stats: vec![0i64; channels * STATSIZE],
        }
    }

    /// Slot an unsigned value and charge the extra bits its range costs.
    ///
    /// The largest value that can arrive is `2^32-1` (from `count` on a 32-bit
    /// difference), which lands at `768 + 255 = 1023` -- exactly the last slot.
    #[inline]
    fn ucount(&mut self, channel: usize, x: u64) {
        let base = channel * STATSIZE;
        if x < (1 << 8) {
            self.stats[base + x as usize] += 1;
        } else if x < (1 << 16) {
            self.stats[base + 256 + (x >> 8) as usize] += 1;
            self.xbits += 8;
        } else if x < (1 << 24) {
            self.stats[base + 512 + (x >> 16) as usize] += 1;
            self.xbits += 16;
        } else {
            self.stats[base + 768 + (x >> 24) as usize] += 1;
            self.xbits += 24;
        }
    }

    /// Zigzag a signed value onto the unsigned slots.
    #[inline]
    fn count(&mut self, channel: usize, x: i64) {
        let z = if x >= 0 {
            (x as u64).wrapping_mul(2)
        } else {
            ((-x) as u64).wrapping_mul(2) - 1
        };
        self.ucount(channel, z);
    }

    /// Estimated output size in bytes, under an order-0 arithmetic coder.
    ///
    /// `total / count` really is integer division in the original, before the
    /// logarithm -- so a symbol occurring more than half the time contributes
    /// `log2(1) = 0` bits. Likewise `xbits / 8` truncates before the addition.
    /// Kept because matching the C exactly costs nothing here; see the module
    /// comment for why neither turns out to change an archive byte.
    fn finish(self) -> ModelResult {
        let mut bits = 0.0f64;
        for n in 0..self.channels {
            let row = &self.stats[n * STATSIZE..(n + 1) * STATSIZE];
            let total: i64 = row.iter().sum();
            for &c in row.iter() {
                if c != 0 {
                    bits += (c as f64) * ((total / c) as f64).ln() / (2.0f64).ln();
                }
            }
        }
        let result = (bits / 8.0 + (self.xbits / 8) as f64) as i64;
        ModelResult {
            channels: self.channels as c_int,
            bitwidth: self.bitwidth,
            offset: self.offset,
            result,
        }
    }
}

// ---------------------------------------------------------------------------
// The scans. Each mirrors the C loop bound exactly, rewritten from pointer
// arithmetic into byte offsets: the C `p + K <= bufend` is a bound on the
// pointer AFTER K elements of the pointee type, so the byte bound depends on
// that type, and the `_diff` variants advance by one sample while requiring two
// to be in range.
// ---------------------------------------------------------------------------

/// The order-0 baseline, `Model::run(1, 8, 0)`.
///
/// This is the ONLY non-diff scan autodetection reaches. The 16/24/32-bit arms
/// of C's `Model::run` exist for `mmdet.cpp`'s own `main()` and are not ported;
/// see the module comment.
fn run_order0(m: &mut Model, buf: &[u8]) {
    for &b in buf {
        m.ucount(0, b as u64);
    }
}

fn diff_scan(m: &mut Model, buf: &[u8], n: usize, bitwidth: c_int) {
    let len = buf.len();
    match bitwidth {
        8 => {
            // Differences are taken in `unsigned char`, so they wrap at 8 bits
            // -- matching diff1 in mm.cpp, which is what this estimates.
            let mut b = 0;
            while b + 2 * n <= len {
                for i in 0..n {
                    let d = buf[b + n + i].wrapping_sub(buf[b + i]);
                    m.ucount(i, d as u64);
                }
                b += n;
            }
        }
        16 => {
            // C promotes both shorts to int and subtracts there: NO wrap at 16
            // bits, unlike diff2. Preserved as written.
            let mut b = 0;
            while b + 4 * n <= len {
                for i in 0..n {
                    let hi = i16le(&buf[b + 2 * (n + i)..]) as i64;
                    let lo = i16le(&buf[b + 2 * i..]) as i64;
                    m.count(i, hi - lo);
                }
                b += 2 * n;
            }
        }
        24 => {
            // UNSIGNED here, signed in the non-diff scan. That asymmetry is the
            // original's.
            let mut b = 0;
            while b + 6 * n <= len {
                for i in 0..n {
                    let hi = unsigned24(&buf[b + 3 * n + 3 * i..]) as i64;
                    let lo = unsigned24(&buf[b + 3 * i..]) as i64;
                    m.count(i, hi - lo);
                }
                b += 3 * n;
            }
        }
        32 => {
            // Taken unsigned and cast back, so it wraps at 32 bits the way
            // diff4 does -- and so it stays inside what `count` can slot.
            let mut b = 0;
            while b + 8 * n <= len {
                for i in 0..n {
                    let hi = u32le(&buf[b + 4 * (n + i)..]);
                    let lo = u32le(&buf[b + 4 * i..]);
                    m.count(i, hi.wrapping_sub(lo) as i32 as i64);
                }
                b += 4 * n;
            }
        }
        _ => unreachable!("diff_scan called with bitwidth {bitwidth}"),
    }
}

fn run_model(buf: &[u8], channels: usize, bitwidth: c_int, offset: c_int, diff: bool) -> ModelResult {
    let mut m = Model::start(channels, bitwidth, offset);
    let off = offset as usize;
    let tail = if off <= buf.len() { &buf[off..] } else { &buf[..0] };
    if diff {
        diff_scan(&mut m, tail, channels, bitwidth);
    } else {
        // Only ever reached as run_order0() -- channels 1, bitwidth 8.
        run_order0(&mut m, tail);
    }
    m.finish()
}

// ---------------------------------------------------------------------------
// WAV header recognition
// ---------------------------------------------------------------------------

const WAVE_FORMAT_PCM: u16 = 1;
const WAVE_FORMAT_PCM2: u16 = 0xFFFE;
const WAVE_FORMAT_IEEE_FLOAT: u16 = 3;

const RIFF_SIGN: u32 = 0x4646_4952;
const WAVE_SIGN: u32 = 0x4556_4157;
const FMT_SIGN: u32 = 0x2074_6D66;
const DATA_SIGN: u32 = 0x6174_6164;

const MAX_BPS: u16 = 32;

/// `sizeof(wave_hdr_t)` -- five u32, then u16,u16,u32,u32,u16,u16. All fields
/// are naturally aligned at those offsets, so the struct is 36 bytes with no
/// padding, and the C code indexes the file through it directly.
const WAVE_HDR_SIZE: usize = 36;
/// `sizeof(subchunk_hdr)`.
const SUBCHUNK_HDR_SIZE: usize = 8;

/// What autodetection concluded.
pub struct Detected {
    pub is_float: bool,
    pub num_chan: c_int,
    pub word_size: c_int,
    pub offset: c_int,
}

/// Recognise a RIFF/WAVE header and locate the sample data.
///
/// Note there is NO check that `BitsPerSample` is one of 8/16/24/32, and
/// `NumChannels` is a 16-bit field that `mm_compress` later stores in a single
/// header BYTE. Both are the original's behaviour and both are preserved --
/// the encoder must agree with the C encoder, defects included.
pub fn autodetect_wav_header(buf: &[u8]) -> Option<Detected> {
    let size = buf.len();
    if size < WAVE_HDR_SIZE {
        return None;
    }
    let chunk_id = u32le(&buf[0..]);
    let chunk_size = u32le(&buf[4..]);
    let format = u32le(&buf[8..]);
    let subchunk1_id = u32le(&buf[12..]);
    let subchunk1_size = u32le(&buf[16..]);
    let audio_format = u16le(&buf[20..]);
    let num_channels = u16le(&buf[22..]);
    let bits_per_sample = u16le(&buf[34..]);

    if chunk_id != RIFF_SIGN
        || format != WAVE_SIGN
        || subchunk1_id != FMT_SIGN
        || subchunk1_size > chunk_size
        || num_channels == 0
        || bits_per_sample > MAX_BPS
    {
        return None;
    }

    let is_float = match audio_format {
        WAVE_FORMAT_IEEE_FLOAT => true,
        WAVE_FORMAT_PCM | WAVE_FORMAT_PCM2 => false,
        _ => return None,
    };

    // p points just past the headers, at the samples.
    let mut p = WAVE_HDR_SIZE;

    if subchunk1_size > 16 {
        let extra = (subchunk1_size - 16) as u64;
        if extra >= (size - p) as u64 {
            return None;
        }
        p += extra as usize;
    }

    if SUBCHUNK_HDR_SIZE >= size - p {
        return None;
    }

    // Skip any subchunks before `data`. Each check keeps p inside the buffer
    // with at least a subchunk header to spare, so the read at the top of the
    // loop is always in range.
    while u32le(&buf[p..]) != DATA_SIGN {
        let skip = SUBCHUNK_HDR_SIZE as u64 + u32le(&buf[p + 4..]) as u64;
        if skip + SUBCHUNK_HDR_SIZE as u64 >= (size - p) as u64 {
            return None;
        }
        p += skip as usize;
    }
    p += SUBCHUNK_HDR_SIZE;

    Some(Detected {
        is_float,
        num_chan: num_channels as c_int,
        word_size: bits_per_sample as c_int,
        offset: p as c_int,
    })
}

// ---------------------------------------------------------------------------
// Entropy-based autodetection
// ---------------------------------------------------------------------------

/// The three selection rules, applied to one candidate in iteration order.
///
/// They are three INDEPENDENT `if`s in the original, not a chain, and they are
/// not mutually exclusive: a candidate can be adopted by the first and then
/// examined again by the third within the same call. Turning them into
/// `else if` changes the winner, so the shape is preserved exactly.
///
/// This is split out because the differential corpus cannot reach it. Real
/// candidates come out near-tied -- the runner-up scores within 0.1% of the
/// best on every input measured -- so the +/-5% bands admit or reject the same
/// model whatever their exact value, and perturbing 0.95 or 1.05 changes no
/// archive byte. The bands still have to be right, hence the unit tests below,
/// which construct the near-ties that real data does not.
fn consider(best: &mut ModelResult, m: ModelResult, model0: i64) {
    // Better, and not narrower than what we have.
    if m.result < best.result && m.bitwidth >= best.bitwidth {
        *best = m;
    }
    // Prefer a WIDER word if it is still close enough, and actually beats
    // order-0.
    if m.bitwidth > best.bitwidth
        && (m.result as f64) < best.result as f64 * 1.05
        && (m.result as f64) < model0 as f64 * 0.95
    {
        *best = m;
    }
    // ...and the opposite: a narrower word that wins outright by 5%.
    if (m.result as f64) < best.result as f64 * 0.95 {
        *best = m;
    }
}

/// Try every (channels, wordsize, offset) model and pick the best, or give up.
///
/// The three selection rules run in sequence and are NOT mutually exclusive --
/// a model can be adopted by the first and then again by the third within one
/// iteration. Written as three independent `if`s because that is what the
/// original does, and collapsing them into `else if` changes the winner.
pub fn autodetect_by_entropy(
    buf: &[u8],
    channels: &[c_int],
    bitvalues: &[c_int],
    min_entropy: f64,
) -> Option<Detected> {
    if buf.len() < 500 {
        return None; // not enough data to decide
    }

    // Order-0 baseline: if a plain order-0 coder already shrinks this a lot,
    // it is not multimedia.
    let model0 = run_model(buf, 1, 8, 0, false).result;
    if (model0 as f64) < buf.len() as f64 * min_entropy {
        return None;
    }

    // C writes `Model best_model; best_model.result = LONG_MAX;` -- and Model's
    // constructor sets only buf/bufsize, so `bitwidth` starts as whatever was on
    // the stack. It does not matter: on the first candidate, either rule 1 fires
    // (garbage width <= m's) or rule 3 does (anything beats LONG_MAX*0.95), so
    // `best` becomes that candidate either way. Zero is the honest stand-in.
    let mut best = ModelResult {
        channels: 0,
        bitwidth: 0,
        offset: 0,
        result: i64::MAX, // LONG_MAX; `long` is 64-bit on every build here
    };

    for &n in channels {
        for &bits in bitvalues {
            let mut offset = 0;
            while offset * 8 < bits {
                consider(&mut best, run_model(buf, n as usize, bits, offset, true), model0);
                offset += 1;
            }
        }
    }

    // Only worth filtering if it beats order-0 by at least 5%.
    if (best.result as f64) < model0 as f64 * 0.95 {
        Some(Detected {
            is_float: false,
            num_chan: best.channels,
            word_size: best.bitwidth,
            offset: best.offset,
        })
    } else {
        None
    }
}


#[cfg(test)]
mod selection_tests {
    use super::*;

    fn m(channels: c_int, bitwidth: c_int, result: i64) -> ModelResult {
        ModelResult { channels, bitwidth, offset: 0, result }
    }
    fn seed() -> ModelResult {
        m(0, 0, i64::MAX)
    }

    /// The very first candidate must always be adopted, whatever it scores.
    /// C relies on LONG_MAX plus an uninitialised `bitwidth` for this; the port
    /// uses 0, and the two must agree.
    #[test]
    fn first_candidate_is_always_adopted() {
        for width in [8, 16, 24, 32] {
            let mut best = seed();
            consider(&mut best, m(1, width, 900_000), 1_000_000);
            assert_eq!(best.bitwidth, width);
            assert_eq!(best.result, 900_000);
        }
    }

    /// Rule 1 is `>=`, not `>`: an equally wide model that scores better wins.
    /// With `>` it would be rejected here and rule 3 would not save it either,
    /// because 990 is not 5% better than 1000.
    #[test]
    fn equal_width_and_better_score_wins() {
        let mut best = m(1, 16, 1000);
        consider(&mut best, m(2, 16, 990), 10_000);
        assert_eq!(best.channels, 2, "the >= in rule 1 is load-bearing");
    }

    /// Rule 1 refuses to go narrower even for a better score...
    #[test]
    fn narrower_is_refused_unless_much_better() {
        let mut best = m(1, 16, 1000);
        consider(&mut best, m(1, 8, 960), 10_000); // 4% better: not enough
        assert_eq!(best.bitwidth, 16);
    }

    /// ...until rule 3's 5% margin is cleared.
    #[test]
    fn narrower_wins_at_five_percent() {
        let mut best = m(1, 16, 1000);
        consider(&mut best, m(1, 8, 940), 10_000); // 6% better
        assert_eq!(best.bitwidth, 8, "rule 3 must override the width preference");
    }

    /// Rule 2 takes a WIDER model that is merely close, not better -- but only
    /// if it also beats order-0 by 5%. Both halves are checked, because a
    /// corpus cannot reach either.
    #[test]
    fn wider_wins_inside_the_five_percent_band() {
        let mut best = m(1, 8, 1000);
        consider(&mut best, m(1, 16, 1040), 2000); // 4% worse, but wider
        assert_eq!(best.bitwidth, 16);

        let mut best = m(1, 8, 1000);
        consider(&mut best, m(1, 16, 1060), 2000); // 6% worse: outside the band
        assert_eq!(best.bitwidth, 8);

        // Wider and inside the band, but does not beat order-0 by 5%.
        let mut best = m(1, 8, 1000);
        consider(&mut best, m(1, 16, 1040), 1050);
        assert_eq!(best.bitwidth, 8, "rule 2's order-0 condition is load-bearing");
    }

    /// The rules are three independent `if`s. A candidate adopted by rule 1 is
    /// still tested by rules 2 and 3 in the same call -- which is harmless only
    /// because `best` has become the candidate itself by then. Collapsing them
    /// into `else if` is what this pins against.
    #[test]
    fn rules_are_independent_not_chained() {
        let mut best = m(1, 16, 1000);
        consider(&mut best, m(2, 16, 500), 10_000);
        // Rule 1 adopts it; rule 3 then compares 500 < 500*0.95, which is false.
        assert_eq!(best.channels, 2);
        assert_eq!(best.result, 500);
    }
}
