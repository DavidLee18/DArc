//! Record-structure de-interleaving, ported from
//! `Compression/GRZip/Rec_Flt.c` (`GRZip_Rec_Decode` :208).
//!
//! Reached only from the recursive `Mode == -2` block, which splits its input
//! into 2 or 4 sub-blocks, decodes each independently, and then recombines them
//! here. Four modes:
//!
//! * **1 / 2** -- plain byte de-interleave for 2- and 4-byte records: the input
//!   holds all the first bytes, then all the second bytes, and so on.
//! * **3 / 4** -- the same, but the records are 16- or 32-bit values that were
//!   *delta-coded* with a zigzag sign map, so each is a difference from its
//!   predecessor rather than a literal.
//!
//! The zigzag is `Delta&1 ? !(Delta>>1) : Delta>>1` -- note the C uses `~`, a
//! bitwise complement, not a negation. On unsigned values those agree only
//! because the sum is taken modulo the word size, so this reproduces the
//! complement exactly rather than "fixing" it to `-(x+1)`.
//!
//! Every mode reads exactly `Size` bytes and writes exactly `Size`, so the
//! caller's buffers bound everything; the slicing here is checked regardless.

/// `GRZip_Rec_Decode`/`_Encode`'s `Mode`: which de-interleave a recursive block
/// used.
///
/// Exactly four variants, with **no `Unknown`**. The first version of this type
/// (#106) had one, and it was a mistake worth recording: `test` returns only
/// 0..=4 and the encode call site filters 0, so `Unknown` was unreachable on the
/// encode path and carried a documented no-op body -- a can't-happen arm
/// reintroduced by the very refactor meant to delete them (#105 removed three).
///
/// "Not one of the four" is a **parse failure**, not a mode, so it is
/// `Option<RecMode>`: see [`RecMode::from_stream`]. The C is four independent
/// `if (Mode==n)` tests with no `else` (Rec_Flt.c:211/:234/:262/:269), which in
/// Rust is exactly "no mode, so no transform".
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum RecMode {
    /// Mode 1 -- 2-way byte de-interleave.
    Interleave2,
    /// Mode 2 -- 4-way byte de-interleave.
    Interleave4,
    /// Mode 3 -- 16-bit delta records.
    Delta16,
    /// Mode 4 -- 32-bit delta records.
    Delta32,
}

impl RecMode {
    /// Classify a mode byte read from the compressed stream.
    ///
    /// `None` for anything outside 1..=4, which is the C's fall-through: with no
    /// `else` on those four `if`s, an unrecognised mode leaves the output
    /// untouched. Returning `Option` rather than an `Unknown` variant keeps that
    /// case out of the enum, where it would be dead on the encode path.
    ///
    /// A crafted archive can carry any value here, so this must never panic.
    pub fn from_stream(mode: i32) -> Option<RecMode> {
        match mode {
            1 => Some(RecMode::Interleave2),
            2 => Some(RecMode::Interleave4),
            3 => Some(RecMode::Delta16),
            4 => Some(RecMode::Delta32),
            _ => None,
        }
    }

    /// The mode number as stored in the block header. Inverse of
    /// [`RecMode::from_stream`] on 1..=4.
    pub fn to_i32(self) -> i32 {
        match self {
            RecMode::Interleave2 => 1,
            RecMode::Interleave4 => 2,
            RecMode::Delta16 => 3,
            RecMode::Delta32 => 4,
        }
    }

    /// How many sub-blocks a recognised mode splits into: 2 for modes 1 and 3,
    /// 4 for modes 2 and 4.
    pub fn parts(self) -> usize {
        match self {
            RecMode::Interleave2 | RecMode::Delta16 => 2,
            RecMode::Interleave4 | RecMode::Delta32 => 4,
        }
    }
}

/// The sub-block count for a **raw** mode byte, recognised or not.
///
/// Separate from [`RecMode::parts`] on purpose. The C computes the count as
/// `Mode & 1` before it ever checks whether `Mode` names a transform, so an
/// unrecognised mode still yields 2 or 4 -- the same byte read two different
/// ways. The block walk must consume exactly as many sub-blocks as the C would,
/// even when the transform then declines to run, so this rule lives on the raw
/// value where the C has it rather than being smuggled into a classified mode.
pub fn parts_from_stream(mode: i32) -> usize {
    if mode & 1 == 1 {
        2
    } else {
        4
    }
}

/// `GRZip_Rec_Decode`. `size` is both the input and output length.
pub fn decode(input: &[u8], size: usize, out: &mut [u8], mode: RecMode) {
    if size == 0 || input.len() < size || out.len() < size {
        return;
    }
    match mode {
        RecMode::Delta16 => {
            // 16-bit delta records. The low byte of each comes from the first
            // half of the input, the high byte from `NumRecords` bytes later.
            let n = size >> 1;
            let mut pred: u16 = 0;
            for i in 0..n {
                let mut delta = input[i] as u16;
                delta = (delta << 8) | input[i + n] as u16;
                delta = if delta & 1 != 0 { !(delta >> 1) } else { delta >> 1 };
                let code = delta.wrapping_add(pred);
                pred = code;
                out[i * 2..i * 2 + 2].copy_from_slice(&code.to_le_bytes());
            }
            // Trailing bytes that did not fill a whole record.
            let mut i = 2 * n;
            let mut p = n;
            while i < size {
                out[i] = input[p + n];
                i += 1;
                p += 1;
            }
        }
        RecMode::Delta32 => {
            // 32-bit delta records, assembled most-significant byte first from
            // four equally spaced planes.
            let n = size >> 2;
            let (p1, p2, p3) = (n, 2 * n, 3 * n);
            let mut pred: u32 = 0;
            for i in 0..n {
                let mut delta = input[i] as u32;
                delta = (delta << 8) | input[i + p3] as u32;
                delta = (delta << 8) | input[i + p2] as u32;
                delta = (delta << 8) | input[i + p1] as u32;
                delta = if delta & 1 != 0 { !(delta >> 1) } else { delta >> 1 };
                let code = delta.wrapping_add(pred);
                pred = code;
                out[i * 4..i * 4 + 4].copy_from_slice(&code.to_le_bytes());
            }
            let mut i = 4 * n;
            let mut p = n;
            while i < size {
                out[i] = input[p + p3];
                i += 1;
                p += 1;
            }
        }
        RecMode::Interleave2 => {
            let mut p = 0;
            for step in 0..2 {
                let mut i = step;
                while i < size {
                    out[i] = input[p];
                    p += 1;
                    i += 2;
                }
            }
        }
        RecMode::Interleave4 => {
            let mut p = 0;
            for step in 0..4 {
                let mut i = step;
                while i < size {
                    out[i] = input[p];
                    p += 1;
                    i += 4;
                }
            }
        }
    }
}

/// `GRZip_Rec_Test` (Rec_Flt.c:44): decide whether the block looks like fixed-
/// width records, and if so which of the four filters to apply.
///
/// Two stages. First an entropy comparison: the order-0 cost of the whole block
/// against the cost of splitting bytes into 2 or 4 positional buckets, with the
/// bar set 7% below the flat cost (`MinEntropy *= 0.93`) and a further 5% for
/// the 2-way case. Then, if a de-interleave looks worthwhile, a second test
/// asks whether the records are better as DELTAS -- comparing the sum of the
/// values against the sum of their zigzagged differences.
///
/// Two pieces of arithmetic are transliterated rather than corrected:
///
/// * `MinCode*(Size>>1)` in the 16-bit branch is `int * int`, and overflows for
///   any large block (65535 * 4M is well past 2^31). C leaves that undefined;
///   in practice it wraps, and the comparison it feeds decides the mode -- so
///   the wrap is reproduced with `wrapping_mul` on `i32`.
/// * The 32-bit branch is `uint32 * int`, which the usual conversions make
///   UNSIGNED, so it wraps defined at 32 bits and then widens. Same expression,
///   different type, different result. Both are matched exactly.
///
/// (`memset(Freq0,0,Rec_MaxByte*sizeof(Rec_MaxByte))` in the C takes `sizeof`
/// of a *value* rather than a type. It is right only by coincidence -- the
/// macro is an `int` literal and the arrays are `sint32` -- so there is nothing
/// to port, but it is worth not copying the idiom.)
pub fn test(input: &[u8], size: usize) -> Option<RecMode> {
    const MAX_BYTE: usize = 256;
    if size == 0 || input.len() < size {
        return None;
    }

    let mut freq0 = [0i32; MAX_BYTE];
    let mut freq2 = [[0i32; MAX_BYTE]; 2];
    let mut freq4 = [[0i32; MAX_BYTE]; 4];

    let (mut pos2, mut pos4) = (0usize, 0usize);
    for &c in input[..size].iter() {
        freq0[c as usize] += 1;
        freq2[pos2][c as usize] += 1;
        freq4[pos4][c as usize] += 1;
        pos2 = (pos2 + 1) & 1;
        pos4 = (pos4 + 1) & 3;
    }

    let sz = size as f64;
    let mut min_entropy = 0.0f64;
    for &f in freq0.iter() {
        if f != 0 {
            min_entropy -= f as f64 * (f as f64 / sz).log10();
        }
    }
    min_entropy *= 0.93;
    let mut min = 0i32;

    let mut entropy = 0.0f64;
    for row in freq2.iter() {
        for &f in row.iter() {
            if f != 0 {
                entropy -= f as f64 * (2.0 * f as f64 / sz).log10();
            }
        }
    }
    if entropy < min_entropy {
        min_entropy = 0.95 * entropy;
        min = 1;
    }

    entropy = 0.0;
    for row in freq4.iter() {
        for &f in row.iter() {
            if f != 0 {
                entropy -= f as f64 * (4.0 * f as f64 / sz).log10();
            }
        }
    }
    if entropy < min_entropy {
        min = 2;
    }

    if min == 1 {
        let n = size >> 1;
        let (mut sum, mut sum_delta) = (0i64, 0i64);
        let mut min_code: u16 = 0xFFFF;
        let mut pred: u16 = 0;
        for i in 0..n {
            let code = u16::from_le_bytes([input[2 * i], input[2 * i + 1]]);
            if code < min_code {
                min_code = code;
            }
            sum += code as i64;
            let mut delta = code.wrapping_sub(pred);
            pred = code;
            delta = if delta & 0x8000 != 0 {
                ((!delta) << 1) | 1
            } else {
                delta << 1
            };
            sum_delta += delta as i64;
        }
        // int * int, and it overflows -- see the note above.
        let prod = (min_code as i32).wrapping_mul(n as i32) as i64;
        if sum - prod > sum_delta + (sum_delta >> 4) {
            min = 3;
        }
    }

    if min == 2 {
        let n = size >> 2;
        let (mut sum, mut sum_delta) = (0i64, 0i64);
        let mut min_code: u32 = 0xFFFF_FFFF;
        let mut pred: u32 = 0;
        for i in 0..n {
            let code = u32::from_le_bytes([
                input[4 * i],
                input[4 * i + 1],
                input[4 * i + 2],
                input[4 * i + 3],
            ]);
            if code < min_code {
                min_code = code;
            }
            sum += code as i64;
            let mut delta = code.wrapping_sub(pred);
            pred = code;
            delta = if delta & 0x8000_0000 != 0 {
                ((!delta) << 1) | 1
            } else {
                delta << 1
            };
            sum_delta += delta as i64;
        }
        // uint32 * int -> UNSIGNED, so this wraps defined and then widens.
        let prod = min_code.wrapping_mul(n as u32) as i64;
        if sum - prod > sum_delta + (sum_delta >> 4) {
            min = 4;
        }
    }

    // `min` is 0 (no filter worth applying) or 1..=4. Classifying here means no
    // raw mode integer escapes this module on the encode path, so the encoder
    // cannot reach the decode-only "unrecognised" case at all.
    RecMode::from_stream(min)
}

/// `GRZip_Rec_Encode` (Rec_Flt.c:137). Inverse of `decode`.
///
/// Modes 3 and 4 write the delta's bytes into separate planes, and the plane
/// order is not the obvious one: mode 4 emits byte3 first, then bytes 0, 1, 2,
/// because the C shifts `Delta` twice before storing the top byte at offset 0.
pub fn encode(input: &[u8], size: usize, out: &mut [u8], mode: RecMode) {
    if size == 0 || input.len() < size || out.len() < size {
        return;
    }
    match mode {
        RecMode::Delta16 => {
            let n = size >> 1;
            let mut pred: u16 = 0;
            for i in 0..n {
                let code = u16::from_le_bytes([input[2 * i], input[2 * i + 1]]);
                let mut delta = code.wrapping_sub(pred);
                pred = code;
                delta = if delta & 0x8000 != 0 {
                    ((!delta) << 1) | 1
                } else {
                    delta << 1
                };
                out[i + n] = (delta & 0xFF) as u8;
                out[i] = (delta >> 8) as u8;
            }
            // Whatever did not make a whole record follows the low-byte plane.
            let mut o = n;
            for i in (2 * n)..size {
                out[o + n] = input[i];
                o += 1;
            }
        }
        RecMode::Delta32 => {
            let n = size >> 2;
            let (p1, p2, p3) = (n, 2 * n, 3 * n);
            let mut pred: u32 = 0;
            for i in 0..n {
                let code = u32::from_le_bytes([
                    input[4 * i],
                    input[4 * i + 1],
                    input[4 * i + 2],
                    input[4 * i + 3],
                ]);
                let mut delta = code.wrapping_sub(pred);
                pred = code;
                delta = if delta & 0x8000_0000 != 0 {
                    ((!delta) << 1) | 1
                } else {
                    delta << 1
                };
                out[i + p1] = (delta & 0xFF) as u8;
                delta >>= 8;
                out[i + p2] = (delta & 0xFF) as u8;
                delta >>= 8;
                out[i + p3] = (delta & 0xFF) as u8;
                out[i] = (delta >> 8) as u8;
            }
            let mut o = n;
            for i in (4 * n)..size {
                out[o + p3] = input[i];
                o += 1;
            }
        }
        RecMode::Interleave2 => {
            let mut o = 0;
            for step in 0..2 {
                let mut i = step;
                while i < size {
                    out[o] = input[i];
                    o += 1;
                    i += 2;
                }
            }
        }
        RecMode::Interleave4 => {
            let mut o = 0;
            for step in 0..4 {
                let mut i = step;
                while i < size {
                    out[o] = input[i];
                    o += 1;
                    i += 4;
                }
            }
        }
    }
}

#[cfg(test)]
mod rec_tests {
    use super::*;

    /// The 16-bit product `MinCode*(Size>>1)` is `int * int` in C and overflows
    /// for high values over a large block. Reproduced with `wrapping_mul`, and
    /// pinned here as well as in the corpus, because it decides the mode.
    #[test]
    fn sixteen_bit_product_wraps_like_the_c() {
        let n = 60000usize;
        let min_code: u16 = 0xF000;
        let wrapped = (min_code as i32).wrapping_mul((n) as i32) as i64;
        let widened = (min_code as i64) * (n as i64);
        assert_ne!(wrapped, widened, "this block size must actually overflow");
        assert!(wrapped < 0, "the signed product wraps negative here");
    }

    /// [`parts_from_stream`] cannot be reached with an unrecognised mode from any
    /// valid archive, so no differential harness covers that case -- a sabotage
    /// probe replacing the rule with `=> 4` went undetected by grzip-check (#106).
    /// It is still load-bearing: the block walk consumes this many sub-blocks
    /// before the transform is even looked up, so a wrong count here misparses a
    /// corrupt archive instead of leaving the C's untouched buffer.
    #[test]
    fn part_count_follows_the_low_bit_for_any_mode() {
        // The C computes `Mode & 1` before checking whether Mode names anything.
        for m in [1, 3, 5, 7, 77, -1, 0x7fff_ffff] {
            assert_eq!(parts_from_stream(m), 2, "odd mode {m}");
        }
        for m in [0, 2, 4, 6, 78, -2, 0x7fff_fffe] {
            assert_eq!(parts_from_stream(m), 4, "even mode {m}");
        }
        // The four recognised modes agree with the raw rule.
        for m in [1, 2, 3, 4] {
            let classified = RecMode::from_stream(m).expect("1..=4 are recognised");
            assert_eq!(classified.parts(), parts_from_stream(m), "mode {m}");
        }
    }

    /// An unrecognised mode must yield no transform at all -- the C's four `if`s
    /// with no `else`. Since #106's `Unknown` variant was removed this is
    /// structural: `decode`/`encode` take a `RecMode`, so there is no way to *ask*
    /// for the unrecognised case. What remains testable is that classification
    /// rejects those values, which is what the call sites branch on.
    #[test]
    fn unrecognised_modes_do_not_classify() {
        for m in [0, 5, 6, 77, -1, -9, i32::MIN, i32::MAX] {
            assert!(RecMode::from_stream(m).is_none(), "mode {m} must not classify");
        }
        for m in [1, 2, 3, 4] {
            assert!(RecMode::from_stream(m).is_some(), "mode {m} must classify");
        }
    }

    /// The header stores the mode number, so the round trip must be exact.
    #[test]
    fn to_i32_inverts_from_stream() {
        for m in [1, 2, 3, 4] {
            assert_eq!(RecMode::from_stream(m).unwrap().to_i32(), m);
        }
    }

    #[test]
    fn deinterleave_modes_round_trip() {
        for (mode, width) in [(RecMode::Interleave2, 2usize), (RecMode::Interleave4, 4usize)] {
            for extra in 0..width {
                let size = width * 50 + extra;
                let orig: Vec<u8> = (0..size).map(|i| (i * 31 + 7) as u8).collect();
                let mut enc = vec![0u8; size];
                encode(&orig, size, &mut enc, mode);
                let mut dec = vec![0u8; size];
                decode(&enc, size, &mut dec, mode);
                assert_eq!(dec, orig, "mode={mode:?} extra={extra}");
            }
        }
    }
}
