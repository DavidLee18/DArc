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

/// `GRZip_Rec_Decode`. `size` is both the input and output length.
pub fn decode(input: &[u8], size: usize, out: &mut [u8], mode: i32) {
    if size == 0 || input.len() < size || out.len() < size {
        return;
    }
    match mode {
        3 => {
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
        4 => {
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
        1 => {
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
        2 => {
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
        // Verified against the pinned C and deliberately NOT `unreachable!()`.
        // `GRZip_Rec_Decode` is four independent `if (Mode==n)` statements --
        // 3, 4, 1, 2 at Rec_Flt.c:211, :234, :262, :269 -- with no `else` and no
        // `default`. Any other mode leaves the output buffer untouched.
        //
        // That matters because on the decode side `mode` is read from the
        // compressed stream, so a corrupt or crafted archive can carry any
        // value. The C tolerates it silently; a panic here would abort across
        // the FFI boundary on input the C merely shrugs at, which is worse than
        // the fall-through. So this is a no-op on purpose, matching the C.
        _ => {}
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
pub fn test(input: &[u8], size: usize) -> i32 {
    const MAX_BYTE: usize = 256;
    if size == 0 || input.len() < size {
        return 0;
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

    min
}

/// `GRZip_Rec_Encode` (Rec_Flt.c:137). Inverse of `decode`.
///
/// Modes 3 and 4 write the delta's bytes into separate planes, and the plane
/// order is not the obvious one: mode 4 emits byte3 first, then bytes 0, 1, 2,
/// because the C shifts `Delta` twice before storing the top byte at offset 0.
pub fn encode(input: &[u8], size: usize, out: &mut [u8], mode: i32) {
    if size == 0 || input.len() < size || out.len() < size {
        return;
    }
    match mode {
        3 => {
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
        4 => {
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
        1 => {
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
        2 => {
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
        // Same as `decode` above: `GRZip_Rec_Encode` is four independent
        // `if (Mode==n)` tests (Rec_Flt.c:140, :162, :188, :195) with no `else`
        // and no `default`, so an out-of-range mode writes nothing. Here the
        // mode comes from `test()` rather than from a stream, so it is in range
        // in practice -- but keeping the two directions symmetrical is worth
        // more than a guard that would only ever fire on a caller bug.
        _ => {}
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

    /// Modes 1 and 2 are pure de-interleaves and must invert exactly.
    #[test]
    fn deinterleave_modes_round_trip() {
        for (mode, width) in [(1, 2usize), (2, 4usize)] {
            for extra in 0..width {
                let size = width * 50 + extra;
                let orig: Vec<u8> = (0..size).map(|i| (i * 31 + 7) as u8).collect();
                let mut enc = vec![0u8; size];
                encode(&orig, size, &mut enc, mode);
                let mut dec = vec![0u8; size];
                decode(&enc, size, &mut dec, mode);
                assert_eq!(dec, orig, "mode={mode} extra={extra}");
            }
        }
    }
}
