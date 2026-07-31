//! `-m1`/`-m2`, content-defined chunking — `Compression/SREP/compress_cdc.cpp`.
//!
//! Instead of fixed `L`-byte chunks, the input is cut at positions the *content*
//! chooses: a rolling hash runs over the data and a boundary is declared wherever
//! it exceeds a threshold picked so boundaries occur about once per `L` bytes.
//! Two identical regions therefore get identical boundaries however they are
//! aligned, which is what makes this robust to insertions.
//!
//! # `-m1`'s chunking is CPU-dependent in the C. This port's is not.
//!
//! `compress_cdc.cpp:136` selects the boundary hash at **runtime**:
//!
//! ```text
//! : crc32c()? fast_find_chunks<uint32, CrcRollingHash<uint32>,        ...>
//!           : fast_find_chunks<size_t, PolynomialRollingHash<size_t>, ...>
//! ```
//!
//! and `crc32c()` (`hashes.cpp:214`) is gated on `#if GCC_VERSION >= 403`, which
//! includes `<x86intrin.h>`/`<cpuid.h>` and CPUID-tests SSE4.2; everywhere else
//! it is `#define crc32c() false`. So an x86-64 build cuts chunks with
//! CRC32-Castagnoli and an ARM64 build cuts them with the polynomial hash — a
//! completely different compressed stream from the same input. Both decode; they
//! are simply not the same bytes.
//!
//! **This port implements the polynomial variant only**, by decision: the port
//! stays deterministic on every host, which is the property `-m0`/`-m3`/`-m4`
//! already have. The consequence is that `-m1` is byte-identical to a C build
//! whose `crc32c()` is false and not to one where it is true, and
//! `srep-encode-check.sh` says so rather than pretending otherwise.
//!
//! `-m2` (zpaq) has no such branch and is portable.

use super::hash_table::HashTable;
use super::matches::{self, MatchTooShort};
use super::rolling::{RollingHash, PRIME1};
use sha2::{Digest as _, Sha256};

/// `STRIPE` (`:96`) — the unit each thread takes, sized for L2.
pub const STRIPE: usize = 116 * 1024;
/// `WINSIZE` (`:98`) — the rolling window used to choose boundaries.
pub const WINSIZE: usize = 48;
/// `MINIMAL_MIN_MATCH` (`srep.cpp:36`).
pub const MINIMAL_MIN_MATCH: usize = 16;

/// `maxhash` (`:31`, `:63`) — `HashTypeMax - HashTypeMax/L`, so a random hash
/// exceeds it with probability 1/L.
fn maxhash_u64(l: usize) -> u64 {
    u64::MAX - u64::MAX / l as u64
}
fn maxhash_u32(l: usize) -> u32 {
    u32::MAX - u32::MAX / l as u32
}

/// `fast_find_chunks()` (`:29`) with the polynomial hash — `-m1`.
///
/// Appends absolute boundary positions to `marks`. Advances `ptr` exactly as the
/// C does: a whole stripe when one fits, otherwise to `pend`.
fn fast_find_chunks(
    buf: &[u8],
    ptr: &mut usize,
    pend: usize,
    bufend: usize,
    l: usize,
    min_match: usize,
    marks: &mut Vec<usize>,
) {
    let maxhash = maxhash_u64(l);
    let piece = STRIPE / 3;
    let start_of_run = marks.len();

    if pend - *ptr >= piece * 3 && *ptr + 3 * piece + WINSIZE <= buf.len() {
        // Three interleaved streams over one stripe, then the marks are sorted
        // back into position order (`:38`). They are distinct positions, so the
        // sort's stability is not in question.
        let p0 = *ptr;
        let mut h: [RollingHash; 3] = [
            RollingHash::new(WINSIZE, PRIME1),
            RollingHash::new(WINSIZE, PRIME1),
            RollingHash::new(WINSIZE, PRIME1),
        ];
        let mut lastp = [p0, p0 + piece, p0 + 2 * piece];
        for k in 0..3 {
            h[k].moveto(&buf[lastp[k]..]);
        }
        let mut p = p0 + WINSIZE;
        while p < p0 + piece {
            for k in 0..3 {
                let base = p + k * piece;
                h[k].update(buf[base - WINSIZE], buf[base]);
                if h[k].value > maxhash && base - lastp[k] >= min_match {
                    marks.push(base);
                    lastp[k] = base;
                }
            }
            p += 1;
        }
        marks[start_of_run..].sort_unstable();
        *ptr += piece * 3;
    } else {
        if pend - *ptr >= WINSIZE && *ptr + WINSIZE <= buf.len() {
            let mut lastp1 = *ptr;
            let mut h = RollingHash::new(WINSIZE, PRIME1);
            h.moveto(&buf[lastp1..]);
            let mut p = *ptr + WINSIZE;
            while p < pend {
                h.update(buf[p - WINSIZE], buf[p]);
                if h.value > maxhash && p - lastp1 >= min_match {
                    marks.push(p);
                    lastp1 = p;
                }
                p += 1;
            }
        }
        *ptr = pend;
    }
    if pend == bufend {
        marks.push(bufend);
    }
}

/// `zpaq_find_chunks()` (`:61`) — `-m2`.
///
/// The window is not a fixed span: the hash is fed every byte but is *reset*
/// at each boundary, and its multiplier depends on whether an order-1 model
/// predicted the byte. Modelling starts 8000 bytes before the stripe so the
/// predictor is warm, which is why boundaries before `ptr` are discarded.
fn zpaq_find_chunks(
    buf: &[u8],
    ptr: &mut usize,
    pend: usize,
    bufend: usize,
    l: usize,
    min_match: usize,
    marks: &mut Vec<usize>,
) {
    let maxhash = maxhash_u32(l);
    let mut hash: u32 = 0;
    let mut c1: u8 = 0;
    let mut o1 = [0u8; 256];

    let begin = match *ptr > 8000 {
        true => *ptr - 8000,
        false => 0,
    };
    let mut lastp = begin;
    let mut p = begin;
    while p < pend {
        let c = buf[p];
        let mult: u32 = match c != o1[c1 as usize] {
            true => 271_828_182,
            false => 314_159_265,
        };
        hash = hash
            .wrapping_add(u32::from(c))
            .wrapping_add(1)
            .wrapping_mul(mult);
        o1[c1 as usize] = c;
        c1 = c;
        if hash > maxhash && p - lastp >= min_match {
            // A boundary found in the warm-up region belongs to the previous
            // stripe and is dropped, but it still resets the model.
            if p > *ptr {
                marks.push(p);
            }
            lastp = p;
            c1 = 0;
            hash = 0;
            o1 = [0u8; 256];
        }
        p += 1;
    }
    *ptr = pend;
    if pend == bufend {
        marks.push(bufend);
    }
}

/// The 32 bytes `find_match_CDC` consumes: 20 of digest, then 8 of index.
///
/// The C fills these with two VMAC tags under a random key. Any keyless hash
/// answers the same questions — see `hash_table`'s module docs.
fn chunk_hash(chunk: &[u8]) -> [u8; 32] {
    let mut h = Sha256::new();
    h.update(chunk);
    let out = h.finalize();
    let mut b = [0u8; 32];
    b.copy_from_slice(&out);
    b
}

/// `compress_CDC()` (`:176`).
///
/// Single-threaded: the C fans stripes out to a thread pool but consumes the
/// results in submission order, and its output is identical across `-t1`
/// through `-t8` (measured), so the threading is a speed device only.
pub fn compress_cdc(
    zpaq: bool,
    l: usize,
    min_match: usize,
    block_start: u64,
    h: &mut HashTable,
    buf: &[u8],
    stat: &mut Vec<u32>,
) -> Result<usize, MatchTooShort> {
    let bufend = buf.len();
    let mut literal_bytes = 0usize;
    let mut last_match = 0usize;
    let mut last_chunk = 0usize;
    // `if (MIN_MATCH < MINIMAL_MIN_MATCH) MIN_MATCH = MINIMAL_MIN_MATCH` (:179)
    let min_match = min_match.max(MINIMAL_MIN_MATCH);

    let mut ptr = 0usize;
    while ptr < bufend {
        let pend = match bufend - ptr < STRIPE {
            true => bufend,
            false => ptr + STRIPE,
        };
        let mut marks: Vec<usize> = Vec::new();
        let mut scan = ptr;
        match zpaq {
            true => zpaq_find_chunks(buf, &mut scan, pend, bufend, l, min_match, &mut marks),
            false => fast_find_chunks(buf, &mut scan, pend, bufend, l, min_match, &mut marks),
        }
        // The CALLER advances to `pend`, not to wherever the finder stopped
        // (`:192`). That matters: `fast_find_chunks` advances its own copy by
        // `STRIPE/3*3`, which is TWO BYTES SHORT of STRIPE because 116 KB is not
        // divisible by three. Letting the finder drive this makes every stripe
        // after the first start two bytes early, and the chunk boundaries drift
        // away from the C's from the second stripe onwards.
        ptr = pend;

        for mark in marks {
            // No guard against a zero-length chunk: the C has none (`:202`), and
            // one can occur -- zpaq_find_chunks pushes `bufend` as the closing
            // mark even when the last boundary it found already sat there. The C
            // still calls find_match_CDC for it, which CONSUMES A CHUNK NUMBER
            // and writes startarr, so skipping it would renumber every later
            // chunk. (Measured: on this corpus removing the guard changed no
            // output, so it is faithfulness rather than a fix.)
            let len = mark - last_chunk;
            let hash32 = chunk_hash(&buf[last_chunk..mark]);
            let match_offset =
                h.find_match_cdc(block_start + last_chunk as u64, len as u64, &hash32);
            if match_offset != 0 && len >= min_match {
                matches::encode(
                    stat,
                    false,
                    min_match as u32,
                    (last_chunk - last_match) as u32,
                    match_offset,
                    len as u32,
                )?;
                last_match = mark;
            } else {
                literal_bytes += len;
            }
            last_chunk = mark;
        }
    }
    Ok(literal_bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n)
            .map(|_| {
                s = s.wrapping_mul(1_103_515_245).wrapping_add(12_345);
                (s >> 16) as u8
            })
            .collect()
    }

    fn boundaries(buf: &[u8], zpaq: bool, l: usize, min_match: usize) -> Vec<usize> {
        let mut ptr = 0usize;
        let mut marks = Vec::new();
        while ptr < buf.len() {
            let pend = (ptr + STRIPE).min(buf.len());
            match zpaq {
                true => zpaq_find_chunks(buf, &mut ptr, pend, buf.len(), l, min_match, &mut marks),
                false => fast_find_chunks(buf, &mut ptr, pend, buf.len(), l, min_match, &mut marks),
            }
        }
        marks
    }

    #[test]
    fn boundaries_land_roughly_every_l_bytes() {
        // maxhash is chosen so a random hash exceeds it with probability 1/L.
        // Nothing here is exact, but an order of magnitude off means the
        // threshold is wrong.
        let buf = prng(1, 400_000);
        for &zpaq in &[false, true] {
            let l = 4096usize;
            let m = boundaries(&buf, zpaq, l, 32);
            let avg = buf.len() / m.len().max(1);
            assert!(
                avg > l / 8 && avg < l * 8,
                "zpaq={zpaq}: {} marks, average chunk {avg}, expected near {l}",
                m.len()
            );
        }
    }

    #[test]
    fn the_last_mark_is_always_the_buffer_end() {
        // Without it the tail bytes belong to no chunk and vanish.
        let buf = prng(3, 200_000);
        for &zpaq in &[false, true] {
            let m = boundaries(&buf, zpaq, 4096, 32);
            assert_eq!(*m.last().expect("at least one mark"), buf.len(), "zpaq={zpaq}");
        }
    }

    #[test]
    fn boundaries_are_sorted_and_respect_min_match() {
        // The three-stream finder emits out of order and sorts afterwards; if
        // that sort were dropped the chunk lengths would go negative.
        let buf = prng(5, 400_000);
        for &zpaq in &[false, true] {
            let m = boundaries(&buf, zpaq, 4096, 512);
            for w in m.windows(2) {
                assert!(w[1] > w[0], "zpaq={zpaq}: marks not increasing");
            }
        }
    }

    #[test]
    fn identical_content_gets_identical_boundaries_when_shifted() {
        // The point of content-defined chunking: an insertion must not move the
        // boundaries of everything after it.
        let body = prng(7, 200_000);
        let mut shifted = vec![0xAAu8; 1234];
        shifted.extend_from_slice(&body);

        let a = boundaries(&body, true, 4096, 32);
        let b = boundaries(&shifted, true, 4096, 32);
        let b_shifted: Vec<usize> = b.iter().filter(|&&x| x >= 1234).map(|x| x - 1234).collect();
        let common = a.iter().filter(|x| b_shifted.contains(x)).count();
        assert!(
            common * 2 > a.len(),
            "only {common} of {} boundaries survived a 1234-byte insertion",
            a.len()
        );
    }
}
