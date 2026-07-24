//! Inverse Schindler sort-transform (ST3..ST8), ported from
//! `Compression/BSC/libbsc/st/st.cpp` (`bsc_st_decode` :1491). This is the
//! alternative block sorter to the BWT: the mode byte's low bits select it
//! (`BLOCKSORTER_ST3..ST8`), and a block coded with it must invert here.
//!
//! Only the serial path is on the format's decode surface; the OpenMP and CUDA
//! fan-outs above it are throughput and produce identical output.
//!
//! ## The three reconstruct cases
//!
//! `bsc_unst_reconstruct_serial` (:1239) picks one of three by size and by
//! whether any single symbol's count reached `0x800000` (`failBack`):
//!
//! * **case 1** (`n < 0x800000`): links carry a 23-bit absolute position plus a
//!   fail-back bit in bit 23; the symbol rides in the top byte.
//! * **case 2** (`n >= 0x800000`, no fail-back): links carry a 23-bit *relative*
//!   offset and the walk rebases through `count`.
//! * **case 3** (fail-back): a symbol's run is too long for 23 bits, so the link
//!   uses bit 31 and the symbol is recovered by a `count`-boundary search with a
//!   10-bit fastbits table.
//!
//! Typical BSC blocks are well under `0x800000` (8 MiB), so case 1 is the common
//! path; cases 2 and 3 are ported for completeness because a large-block archive
//! is still an archive this decoder must read.
//!
//! ## Bounds
//!
//! The reconstruct walk chases stored links; on corrupt input a link can point
//! anywhere. Every `P` index and every `fastbits`/`count` search is bounded, so
//! bad input under `arc t` is `LIBBSC_DATA_CORRUPT`, not a panic.

use super::{LIBBSC_BAD_PARAMETER, LIBBSC_DATA_CORRUPT, LIBBSC_NO_ERROR};

const ALPHABET_SIZE: usize = 256;
const ST_NUM_FASTBITS: u32 = 10;

/// `bsc_unst_sort_serial` (:1014): rebuild the link array `P` and report
/// `failBack`. `count` is left holding the prefix-sum bucket starts.
fn unst_sort(t: &[u8], p: &mut [u32], count: &mut [u32], bucket: &mut [u32], n: usize, k: u32) -> bool {
    let mut index = [0u32; ALPHABET_SIZE];
    let mut group = [-1i32; ALPHABET_SIZE];
    let mut fail_back = false;

    for &b in &t[..n] {
        count[b as usize] += 1;
    }
    let mut sum: i32 = 0;
    for c in 0..ALPHABET_SIZE {
        if count[c] >= 0x800000 {
            fail_back = true;
        }
        let tmp = sum;
        sum += count[c] as i32;
        count[c] = tmp as u32;
        if count[c] as i32 != sum {
            let base = c << 8;
            for i in count[c] as usize..sum as usize {
                bucket[base + t[i] as usize] += 1;
            }
        }
    }

    // Transpose the bigram matrix (lower triangle against upper).
    for c in 0..ALPHABET_SIZE {
        for d in 0..c {
            bucket.swap((d << 8) | c, (c << 8) | d);
        }
    }

    if k == 3 {
        let mut sum: i32 = 0;
        for w in 0..ALPHABET_SIZE * ALPHABET_SIZE {
            if bucket[w] > 0 {
                p[sum as usize] = 1;
                sum += bucket[w] as i32;
            }
        }
        return fail_back;
    }

    index.copy_from_slice(&count[..ALPHABET_SIZE]);
    group = [-1i32; ALPHABET_SIZE];
    let mut sum: i32 = 0;
    for w in 0..ALPHABET_SIZE * ALPHABET_SIZE {
        let tmp = sum;
        sum += bucket[w] as i32;
        bucket[w] = tmp as u32;
        for i in bucket[w] as usize..sum as usize {
            let c = t[i] as usize;
            if group[c] != w as i32 {
                group[c] = w as i32;
                p[index[c] as usize] = 0x80000000;
            }
            index[c] += 1;
        }
    }

    let mut mask0: u32 = 0x80000000;
    let mut mask1: u32 = 0x40000000;
    for _round in 4..k {
        index.copy_from_slice(&count[..ALPHABET_SIZE]);
        group = [-1i32; ALPHABET_SIZE];
        let mut g: i32 = 0;
        for i in 0..n {
            if p[i] & mask0 != 0 {
                g = i as i32;
            }
            let c = t[i] as usize;
            if group[c] != g {
                group[c] = g;
                p[index[c] as usize] += mask1;
            }
            index[c] += 1;
        }
        mask0 >>= 1;
        mask1 >>= 1;
    }

    fail_back
}

/// Bounded `P` read for the reconstruct walk.
#[inline]
fn at(p: &[u32], i: usize) -> Result<u32, i32> {
    p.get(i).copied().ok_or(LIBBSC_DATA_CORRUPT)
}

/// case 1 (`bsc_unst_reconstruct_case1_serial` :1095): absolute 23-bit links,
/// fail-back bit at 0x800000, symbol in the top byte.
fn reconstruct_case1(t: &mut [u8], p: &mut [u32], count: &[u32], n: usize, start: usize) -> Result<(), i32> {
    let mut index = [0u32; ALPHABET_SIZE];
    index.copy_from_slice(&count[..ALPHABET_SIZE]);
    let mut group = [-1i32; ALPHABET_SIZE];

    let mut g: i32 = 0;
    for i in 0..n {
        if p[i] > 0 {
            g = i as i32;
        }
        let c = t[i] as usize;
        if group[c] < g {
            group[c] = i as i32;
            p[i] = ((c as u32) << 24) | index[c];
        } else {
            let gc = group[c] as usize;
            p[i] = ((c as u32) << 24) | 0x800000 | group[c] as u32;
            p[gc] += 1;
        }
        index[c] += 1;
    }

    let mut pc = start;
    for i in (0..n).rev() {
        let mut u = at(p, pc)?;
        if u & 0x800000 != 0 {
            pc = (u & 0x7fffff) as usize;
            u = at(p, pc)?;
        }
        t[i] = (u >> 24) as u8;
        p[pc] = p[pc].wrapping_sub(1);
        pc = (u & 0x7fffff) as usize;
    }
    Ok(())
}

/// case 2 (`:1132`): relative 23-bit links, rebased through `count` on the walk.
fn reconstruct_case2(t: &mut [u8], p: &mut [u32], count: &[u32], n: usize, start: usize) -> Result<(), i32> {
    let mut index = [0u32; ALPHABET_SIZE];
    let mut group = [-1i32; ALPHABET_SIZE];

    let mut g: i32 = 0;
    for i in 0..n {
        if p[i] > 0 {
            g = i as i32;
        }
        let c = t[i] as usize;
        if group[c] < g {
            group[c] = i as i32;
            p[i] = ((c as u32) << 24) | index[c];
        } else {
            let gc = group[c] as usize;
            p[i] = ((c as u32) << 24) | 0x800000 | (i as u32 - group[c] as u32);
            p[gc] += 1;
        }
        index[c] += 1;
    }

    let mut pc = start;
    for i in (0..n).rev() {
        let mut u = at(p, pc)?;
        if u & 0x800000 != 0 {
            pc = pc.checked_sub((u & 0x7fffff) as usize).ok_or(LIBBSC_DATA_CORRUPT)?;
            u = at(p, pc)?;
        }
        let c = (u >> 24) as usize;
        t[i] = c as u8;
        p[pc] = p[pc].wrapping_sub(1);
        pc = (u & 0x7fffff) as usize + count[c] as usize;
    }
    Ok(())
}

/// `bsc_unst_search` (:1170): the smallest boundary index whose `count` exceeds
/// `v`. Bounded so corrupt input cannot walk past the alphabet.
#[inline]
fn search(mut idx: usize, boundaries: &[u32], v: u32) -> Result<usize, i32> {
    while boundaries.get(idx).copied().ok_or(LIBBSC_DATA_CORRUPT)? <= v {
        idx += 1;
    }
    Ok(idx)
}

/// case 3 (`:1177`): fail-back links use bit 31; the symbol is recovered by a
/// fastbits-accelerated `count`-boundary search.
fn reconstruct_case3(t: &mut [u8], p: &mut [u32], count: &[u32], n: usize, start_in: usize) -> Result<(), i32> {
    let mut fastbits = [0u8; 1 << ST_NUM_FASTBITS];
    let mut index = [0u32; ALPHABET_SIZE];
    index.copy_from_slice(&count[..ALPHABET_SIZE]);
    let mut group = [-1i32; ALPHABET_SIZE];

    let mut g: i32 = 0;
    for i in 0..n {
        if p[i] > 0 {
            g = i as i32;
        }
        let c = t[i] as usize;
        if group[c] < g {
            group[c] = i as i32;
            p[i] = index[c];
        } else {
            let gc = group[c] as usize;
            p[i] = 0x80000000 | group[c] as u32;
            p[gc] += 1;
        }
        index[c] += 1;
    }

    let mut shift: u32 = 0;
    while ((n - 1) >> shift) >= (1 << ST_NUM_FASTBITS) {
        shift += 1;
    }

    // Rebuild `index` as the per-symbol upper boundary, and fill fastbits.
    let mut v: usize = 0;
    for c in 0..ALPHABET_SIZE {
        index[c] = if c + 1 < ALPHABET_SIZE { count[c + 1] } else { n as u32 };
        if count[c] != index[c] {
            let lim = ((index[c] - 1) >> shift) as usize;
            while v <= lim && v < fastbits.len() {
                fastbits[v] = c as u8;
                v += 1;
            }
        }
    }

    let mut start = start_in;
    let ps = at(p, start)?;
    if ps & 0x80000000 != 0 {
        start = (ps & 0x7fffffff) as usize;
    }

    t[0] = search(fastbits[start >> shift] as usize, &index, start as u32)? as u8;
    let ps = at(p, start)?;
    p[start] = ps.wrapping_sub(1);
    start = ps as usize;

    let mut pc = start;
    for i in (1..n).rev() {
        let mut u = at(p, pc)?;
        if u & 0x80000000 != 0 {
            pc = (u & 0x7fffffff) as usize;
            u = at(p, pc)?;
        }
        t[i] = search(fastbits[pc >> shift] as usize, &index, pc as u32)? as u8;
        p[pc] = p[pc].wrapping_sub(1);
        pc = u as usize;
    }
    Ok(())
}

/// `bsc_st_decode` (:1491): invert an ST-`k` block in place. Returns
/// `LIBBSC_NO_ERROR` or a negative libbsc code.
pub fn st_decode(t: &mut [u8], n: usize, k: u32, index: i32) -> i32 {
    if index < 0 || index as usize >= n {
        return LIBBSC_BAD_PARAMETER;
    }
    if k < 3 || k > 8 {
        return LIBBSC_BAD_PARAMETER;
    }
    if n <= 1 {
        return LIBBSC_NO_ERROR;
    }

    let mut p = vec![0u32; n];
    let mut bucket = vec![0u32; ALPHABET_SIZE * ALPHABET_SIZE];
    let mut count = [0u32; ALPHABET_SIZE];

    let fail_back = unst_sort(&t[..n], &mut p, &mut count, &mut bucket, n, k);

    let start = index as usize;
    let r = if n < 0x800000 {
        reconstruct_case1(t, &mut p, &count, n, start)
    } else if !fail_back {
        reconstruct_case2(t, &mut p, &count, n, start)
    } else {
        reconstruct_case3(t, &mut p, &count, n, start)
    };

    match r {
        Ok(()) => LIBBSC_NO_ERROR,
        Err(e) => e,
    }
}
