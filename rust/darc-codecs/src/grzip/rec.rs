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
        _ => {}
    }
}
