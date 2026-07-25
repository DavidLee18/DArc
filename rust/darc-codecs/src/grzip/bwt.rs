//! Inverse Burrows-Wheeler transform, ported from `Compression/GRZip/BWT.c`
//! (`GRZip_BWT_Decode` :1052, `GRZip_FastBWT_Decode` :952).
//!
//! (`GRZip_StrongBWT_Decode` :403, `GRZip_FastBWT_Decode` :952.)
//!
//! The encoder picks between two forward transforms and records which by
//! setting `StrongBWT_Flag` in the stored first-byte position. **They are
//! different transforms and each needs its own inverse.** Both walk the same
//! LF mapping -- pair every byte with its occurrence index, take cumulative
//! counts as first-column offsets, follow the chain filling the output
//! backwards -- but the strong variant carries a sentinel row, so its table has
//! `Size+1` entries with a gap at `FBP`, its prefix sum starts at 1, and its
//! walk is anchored at 0 rather than at the stored position.
//!
//! An earlier version of this file ran one inverse for both, on the assumption
//! that the flag only reinterpreted `FBP`. Every BWT block decoded to garbage
//! while every ST4 block passed, which is precisely the signal that located it
//! -- and it survived a local round-trip test, because that test exercised this
//! file's own forward transform rather than the C's.
//!
//! `FBP` arrives from the block header and was never checked in the C until an
//! earlier hardening pass; out of range it indexed off the end of the table.
//! The bounds here are that pass's, not new.

use super::{GrzError, GRZ_CRC_ERROR};

/// `BWT_MaxByte` / `BWT_MaxWord` (BWT.c:42-43).
const MAX_BYTE: usize = 256;
const MAX_WORD: usize = 65536;

/// `StrongBWT_Flag` (BWT.c:70), set in the stored FBP to select the strong
/// variant.
const STRONG_BWT_FLAG: i32 = 0x4000_0000;

/// `GRZip_BWT_Decode` (:1052): the flag picks which inverse runs.
///
/// These are **not** the same transform with different bounds, which is what an
/// earlier version of this file assumed -- it ran one inverse for both and
/// decoded every BWT block to garbage while the ST4 blocks passed, which is
/// exactly the signal that found it. The strong variant carries a sentinel:
/// its table has Size+1 entries with a gap at FBP, its prefix sum starts at 1
/// rather than 0, and its walk starts from 0 rather than from the stored
/// position.
pub fn decode(buf: &mut [u8], size: usize, fbp: i32) -> Result<(), GrzError> {
    if size == 0 || buf.len() < size {
        return Err(GRZ_CRC_ERROR);
    }
    if (fbp & STRONG_BWT_FLAG) == 0 {
        // Fast variant: FBP indexes a table of exactly `size` entries.
        if fbp < 0 || fbp as usize >= size {
            return Err(GRZ_CRC_ERROR);
        }
        invert_fast(buf, size, fbp as usize);
    } else {
        let real = fbp & !STRONG_BWT_FLAG;
        // The C accepts FBP == Size here (the table has Size+1 entries), but
        // FBP == 0 leaves T[0] unwritten and the walk starts by reading it, so
        // only 1..=Size is actually decodable.
        if real < 1 || real as usize > size {
            return Err(GRZ_CRC_ERROR);
        }
        invert_strong(buf, size, real as usize);
    }
    Ok(())
}

/// `GRZip_FastBWT_Decode` (:952). The standard LF-mapping walk: pair each byte
/// with its occurrence index, take cumulative counts as first-column offsets,
/// then follow the chain from `fbp`, filling the output backwards.
fn invert_fast(buf: &mut [u8], size: usize, mut fbp: usize) {
    let mut count = [0u32; 256];
    let mut t = vec![0u32; size];

    for i in 0..size {
        let c = buf[i] as usize;
        t[i] = (count[c] << 8) | c as u32;
        count[c] += 1;
    }
    let mut sum: u32 = 0;
    for c in count.iter_mut() {
        sum += *c;
        *c = sum - *c;
    }

    for i in (0..size).rev() {
        let u = t[fbp];
        let c = (u & 0xFF) as u8;
        fbp = ((u >> 8) + count[c as usize]) as usize;
        buf[i] = c;
        if fbp >= size {
            // Inconsistent permutation, i.e. a corrupt block. The caller's CRC
            // rejects the partial output; stopping beats panicking on an index.
            break;
        }
    }
}

/// `GRZip_StrongBWT_Decode` (:403). Same walk, but over a Size+1 table whose
/// slot `fbp` is deliberately skipped -- the sentinel row -- with the prefix sum
/// biased by one to account for it, and the walk anchored at 0.
fn invert_strong(buf: &mut [u8], size: usize, fbp: usize) {
    let mut count = [0u32; 256];
    let mut t = vec![0u32; size + 1];

    for i in 0..fbp {
        let c = buf[i] as usize;
        t[i] = (count[c] << 8) | c as u32;
        count[c] += 1;
    }
    for i in fbp..size {
        let c = buf[i] as usize;
        t[i + 1] = (count[c] << 8) | c as u32;
        count[c] += 1;
    }
    // Sum starts at 1, not 0: the sentinel occupies the first row.
    let mut sum: u32 = 1;
    for c in count.iter_mut() {
        sum += *c;
        *c = sum - *c;
    }

    let mut at = 0usize;
    for i in (0..size).rev() {
        let u = t[at];
        let c = (u & 0xFF) as u8;
        at = ((u >> 8) + count[c as usize]) as usize;
        buf[i] = c;
        if at > size {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A forward BWT is easy to write directly, so the inverse can be pinned
    /// without the C: build every rotation, sort, take the last column and the
    /// row index of the original, then invert and require the original back.
    fn forward(data: &[u8]) -> (Vec<u8>, usize) {
        let n = data.len();
        let mut rows: Vec<usize> = (0..n).collect();
        rows.sort_by(|&a, &b| {
            for k in 0..n {
                let x = data[(a + k) % n];
                let y = data[(b + k) % n];
                if x != y {
                    return x.cmp(&y);
                }
            }
            a.cmp(&b)
        });
        let last: Vec<u8> = rows.iter().map(|&r| data[(r + n - 1) % n]).collect();
        let fbp = rows.iter().position(|&r| r == 0).unwrap();
        (last, fbp)
    }

    #[test]
    fn inverse_undoes_a_forward_transform() {
        for case in [
            &b"banana"[..],
            &b"the quick brown fox jumps over the lazy dog"[..],
            &b"aaaaaaaaaaaaaaaa"[..],
            &b"ab"[..],
            &b"a"[..],
            &b"mississippi river"[..],
        ] {
            let (mut last, fbp) = forward(case);
            let n = last.len();
            invert_fast(&mut last, n, fbp);
            assert_eq!(&last[..], case, "inverse BWT mismatch for {case:?}");
        }
    }

    #[test]
    fn out_of_range_fbp_is_rejected() {
        let mut buf = vec![1u8, 2, 3, 4];
        assert!(decode(&mut buf, 4, -1).is_err());
        assert!(decode(&mut buf, 4, 4).is_err());
        assert!(decode(&mut buf, 4, i32::MAX).is_err());
        // Strong flag with an in-range position is accepted; 0 is not, because
        // it would leave the walk's first table slot unwritten.
        assert!(decode(&mut buf, 4, STRONG_BWT_FLAG | 2).is_ok());
        assert!(decode(&mut buf, 4, STRONG_BWT_FLAG).is_err());
        assert!(decode(&mut buf, 4, STRONG_BWT_FLAG | 5).is_err());
    }

    #[test]
    fn empty_or_oversized_size_is_rejected() {
        let mut buf = vec![1u8, 2, 3, 4];
        assert!(decode(&mut buf, 0, 0).is_err());
        assert!(decode(&mut buf, 99, 0).is_err());
    }
}

// ---------------------------------------------------------------------------
// Encoder: the "strong" sort (BWT.c:268).
//
// An induced-sorting BWT. The first pass buckets every position by its two-byte
// context; then, taking single-byte buckets in order of increasing frequency, it
// sorts each `S,j` sub-bucket by suffix and *induces* the rest by copying from
// already-placed neighbours. Sorting the rarest first is what keeps the induced
// half doing most of the work.
//
// `Group[i]` packs two things: the high byte is `Input[i]`, and the low 24 bits
// are the position's current rank. That is why the block size is capped just
// under 2^23 -- ranks have to fit alongside the byte.
// ---------------------------------------------------------------------------

/// `BWT_MinQSort` (:44) -- below this span the ternary sort defers to a shell
/// sort.
const MIN_QSORT: i32 = 20;
/// `BWT_MedTrh` (:45) -- above this span the pivot is a median of medians.
const MED_TRH: i32 = 96;
/// `StrongBWT_TSortStackSize` (:72). Also the size of the array the ternary
/// sort uses as its explicit stack, which is why it is far larger than the
/// 65536 entries the bucket pass needs.
const TSORT_STACK: usize = 3 * 65536;
/// `StrongBWT_CMask` / `StrongBWT_SMask` (:74-75) -- the bucket array's low bits
/// are a position, the top bit marks a bucket already induced.
const CMASK: i32 = 0x3FFF_FFFF;
const SMASK: i32 = 0x4000_0000;

fn med3(a: u32, b: u32, c: u32) -> u32 {
    let (mut ma, mut mb) = (a, b);
    if ma > mb {
        core::mem::swap(&mut ma, &mut mb);
    }
    if mb > c {
        mb = c;
        if ma > mb {
            mb = ma;
        }
    }
    mb
}

/// `StrongBWT_SimpleCMP` (:104): compare two positions by walking their ranks
/// two apart until they differ.
///
/// `Group[Size] = 0` terminates this for valid input. The bound is explicit
/// because the walk steps by TWO and can therefore skip past that terminator;
/// in C that reads off the end of the allocation.
#[inline]
fn simple_cmp(group: &[u32], mut i1: usize, mut i2: usize) -> bool {
    let n = group.len();
    if i1 >= n || i2 >= n {
        return false;
    }
    let (mut c1, mut c2) = (group[i1], group[i2]);
    while c1 == c2 {
        i1 += 2;
        i2 += 2;
        if i1 >= n || i2 >= n {
            return false;
        }
        c1 = group[i1];
        c2 = group[i2];
    }
    c1 > c2
}

/// `StrongBWT_ShellSort` (:113). Sorts `Index[lo..=hi]` by the rank `d` deep,
/// then writes each position's new rank back into `Group`.
fn shell_sort(index: &mut [i32], group: &mut [u32], lo: i32, hi: i32, d: i32) {
    if lo == hi {
        let k = index[lo as usize] as usize;
        group[k] = (group[k] & 0xFF00_0000) | lo as u32;
        return;
    }
    let n = hi - lo + 1;
    let mut h = 1i32;
    while h < n {
        h = h * 3 + 1;
    }
    loop {
        h /= 3;
        let mut i = lo + h;
        while i <= hi {
            let s = index[i as usize] + d;
            let mut j = i;
            while simple_cmp(group, (index[(j - h) as usize] + d) as usize, s as usize) {
                index[j as usize] = index[(j - h) as usize];
                j -= h;
                if j < h + lo {
                    break;
                }
            }
            index[j as usize] = s - d;
            i += 1;
        }
        if h == 1 {
            break;
        }
    }
    for p in lo..=hi {
        let k = index[p as usize] as usize;
        group[k] = (group[k] & 0xFF00_0000) | p as u32;
    }
}

/// `StrongBWT_ShellSortDeph2` (:143): the same, specialised to depth 2, with
/// the gap sequence 13, 4, 1 unrolled.
fn shell_sort_depth2(index: &mut [i32], group: &mut [u32], lo: i32, hi: i32) {
    if hi == lo {
        let k = index[lo as usize] as usize;
        group[k] = (group[k] & 0xFF00_0000) | lo as u32;
        return;
    }
    for gap in [13i32, 4, 1] {
        if gap != 1 && hi - lo <= gap {
            continue;
        }
        let mut i = lo + gap;
        while i <= hi {
            let s = index[i as usize] + 2;
            let mut j = i - gap;
            while simple_cmp(group, (index[j as usize] + 2) as usize, s as usize) {
                index[(j + gap) as usize] = index[j as usize];
                j -= gap;
                if j < lo {
                    break;
                }
            }
            index[(j + gap) as usize] = s - 2;
            i += 1;
        }
    }
    for p in lo..=hi {
        let k = index[p as usize] as usize;
        group[k] = (group[k] & 0xFF00_0000) | p as u32;
    }
}

/// `StrongBWT_TernarySort` (:191): three-way radix quicksort on the rank at
/// depth `d`, with an explicit stack.
fn ternary_sort(index: &mut [i32], group: &mut [u32], lo0: i32, hi0: i32, stack: &mut [i32]) {
    let mut sp = 0usize;
    macro_rules! push {
        ($l:expr, $h:expr, $d:expr) => {
            if sp + 3 <= stack.len() {
                stack[sp] = $l;
                stack[sp + 1] = $h;
                stack[sp + 2] = $d;
                sp += 3;
            }
        };
    }
    push!(lo0, hi0, 2);
    while sp != 0 {
        sp -= 3;
        let (mut lo, mut hi, d) = (stack[sp], stack[sp + 1], stack[sp + 2]);
        if hi < lo {
            continue;
        }
        if hi - lo < MIN_QSORT || sp >= TSORT_STACK {
            shell_sort(index, group, lo, hi, d);
            continue;
        }
        let at = |ix: &[i32], p: i32| group[(ix[p as usize] + d) as usize];
        let med = if hi - lo < MED_TRH {
            med3(at(index, (lo + hi) >> 1), at(index, lo), at(index, hi))
        } else {
            let m = (hi + lo) >> 1;
            let n = (hi - lo) >> 3;
            let m1 = med3(at(index, lo), at(index, lo + n), at(index, lo + n + n));
            let m2 = med3(at(index, m - n), at(index, m), at(index, m + n));
            let m3 = med3(at(index, hi - n - n), at(index, hi - n), at(index, hi));
            med3(m1, m2, m3)
        };

        let (mut a, mut b, mut c, mut dd) = (lo, lo, hi, hi);
        loop {
            loop {
                if b > c {
                    break;
                }
                let v = group[(index[b as usize] + d) as usize];
                if v == med {
                    index.swap(a as usize, b as usize);
                    a += 1;
                    b += 1;
                    continue;
                }
                if v > med {
                    break;
                }
                b += 1;
            }
            loop {
                if b > c {
                    break;
                }
                let v = group[(index[c as usize] + d) as usize];
                if v == med {
                    index.swap(c as usize, dd as usize);
                    dd -= 1;
                    c -= 1;
                    continue;
                }
                if v < med {
                    break;
                }
                c -= 1;
            }
            if b > c {
                break;
            }
            index.swap(b as usize, c as usize);
            b += 1;
            c -= 1;
        }

        if dd < a {
            push!(lo, hi, d + 2);
            continue;
        }

        // Move the two equal-to-pivot runs into the middle.
        let mut vswap = |ix: &mut [i32], mut s1: i32, mut s2: i32, mut num: i32| {
            while num != 0 {
                ix.swap(s1 as usize, s2 as usize);
                s1 += 1;
                s2 += 1;
                num -= 1;
            }
        };
        let n = (a - lo).min(b - a);
        vswap(index, lo, b - n, n);
        let m = (hi - dd).min(dd - c);
        vswap(index, b, hi - m + 1, m);

        let n2 = lo + b - a;
        let m2 = hi - (dd - c);
        push!(m2 + 1, hi, d);
        push!(n2, m2, d + 2);
        push!(lo, n2 - 1, d);
        // `lo`/`hi` are reassigned at the top of the loop.
        let _ = (&mut lo, &mut hi);
    }
}

/// `GRZip_StrongBWT_Encode` (:268). Returns the first-byte position.
///
/// The bucket array does double duty: the low 30 bits are a position and
/// `SMASK` marks a single-byte bucket whose induction has already run. The
/// ternary sort's stack shares `BGroups`, which is why that allocation is
/// `TSORT_STACK + 3` rather than the 65537 the bucket pass needs.
pub fn strong_encode(input: &[u8], size: usize, output: &mut [u8]) -> Result<i32, GrzError> {
    if size < 2 || input.len() < size || output.len() < size {
        return Err(GRZ_CRC_ERROR);
    }
    let mut group = vec![0u32; size + 1];
    let mut index = vec![0i32; size + 1];
    let mut buckets = vec![0i32; MAX_WORD + 1];
    let mut bgroups = vec![0i32; TSORT_STACK + 3];

    // Histogram over two-byte contexts, walking backwards so the wrap at the
    // end of the block is seeded from the last byte.
    let lb = input[size - 1];
    let mut w: usize = (lb as usize) << 8;
    buckets[w] += 1;
    for i in (0..=size - 2).rev() {
        w = (w >> 8) | ((input[i] as usize) << 8);
        buckets[w] += 1;
    }

    bgroups[0] = buckets[0];
    for i in 1..=MAX_WORD {
        buckets[i] += buckets[i - 1];
        bgroups[i] = buckets[i];
    }

    w = (lb as usize) << 8;
    for i in (0..=size - 2).rev() {
        w = (w >> 8) | ((input[i] as usize) << 8);
        group[i] = bgroups[w] as u32 | ((input[i] as u32) << 24);
        index[buckets[w] as usize] = i as i32;
        buckets[w] -= 1;
    }
    group[size] = 0;
    let lbw = (lb as usize) << 8;
    group[size - 1] = buckets[lbw] as u32 | ((lb as u32) << 24);
    index[0] = size as i32;
    index[buckets[lbw] as usize] = size as i32 - 1;
    buckets[lbw] -= 1;

    // Single-byte buckets, ordered by increasing frequency: sorting the rarest
    // first leaves the most work to induction.
    let bfreq = |bk: &[i32], b: usize| bk[(b + 1) << 8] - bk[b << 8];
    let mut run_order: Vec<u8> = (0..MAX_BYTE as u32).map(|i| i as u8).collect();
    let mut big_done = [false; MAX_BYTE];
    let mut h = 364usize;
    loop {
        h /= 3;
        for i in h..MAX_BYTE {
            let s = run_order[i];
            let mut j = i;
            while bfreq(&buckets, run_order[j - h] as usize) > bfreq(&buckets, s as usize) {
                run_order[j] = run_order[j - h];
                j -= h;
                if j < h {
                    break;
                }
            }
            run_order[j] = s;
        }
        if h == 1 {
            break;
        }
    }

    let mut hh = 0usize;
    while hh < MAX_BYTE && bfreq(&buckets, run_order[hh] as usize) == 0 {
        hh += 1;
    }

    let mut copy_start = [0i32; MAX_BYTE];
    let mut copy_end = [0i32; MAX_BYTE];

    while hh < MAX_BYTE {
        let s = run_order[hh] as usize;
        for j in 0..MAX_BYTE {
            if j != s {
                let k = (s << 8) | j;
                let mut lo = buckets[k] + 1;
                let hi = buckets[k + 1] & CMASK;
                if lbw == k {
                    lo += 1;
                }
                if lo < SMASK && hi >= lo {
                    if hi - lo > MIN_QSORT {
                        ternary_sort(&mut index, &mut group, lo, hi, &mut bgroups);
                    } else {
                        shell_sort_depth2(&mut index, &mut group, lo, hi);
                    }
                }
            }
        }

        for j in 0..MAX_BYTE {
            copy_start[j] = (buckets[(j << 8) + s] & CMASK) + 1;
            if lbw == (j << 8) + s {
                copy_start[j] += 1;
            }
            buckets[(j << 8) + s] |= SMASK;
            copy_end[j] = buckets[(j << 8) + s + 1] & CMASK;
        }

        // Induce forwards from the sorted bucket...
        let mut j = (buckets[s << 8] & CMASK) + 1;
        while j < copy_start[s] {
            let k = index[j as usize] - 1;
            if k >= 0 {
                let c = (group[k as usize] >> 24) as usize;
                if !big_done[c] {
                    group[k as usize] = (group[k as usize] & 0xFF00_0000) | copy_start[c] as u32;
                    index[copy_start[c] as usize] = k;
                    copy_start[c] += 1;
                }
            }
            j += 1;
        }
        // ...and backwards.
        let mut j = buckets[(s + 1) << 8] & CMASK;
        while j > copy_end[s] {
            let k = index[j as usize] - 1;
            if k >= 0 {
                let c = (group[k as usize] >> 24) as usize;
                if !big_done[c] {
                    group[k as usize] = (group[k as usize] & 0xFF00_0000) | copy_end[c] as u32;
                    index[copy_end[c] as usize] = k;
                    copy_end[c] -= 1;
                }
            }
            j -= 1;
        }
        big_done[s] = true;
        hh += 1;
    }

    // Emit: each position's rank says where its PRECEDING byte belongs.
    let mut last = (group[0] >> 24) as u8;
    let first = group[0] & 0xFF_FFFF;
    for i in 1..=size {
        let ps = group[i] & 0xFF_FFFF;
        let c = (group[i] >> 24) as u8;
        if ps < first {
            output[ps as usize] = last;
        } else {
            output[(ps - 1) as usize] = last;
        }
        last = c;
    }
    Ok(first as i32)
}

#[cfg(test)]
mod strong_tests {
    use super::*;

    /// Encode then decode must be the identity. The C differential says which
    /// bytes differ; a failing round-trip says the fault is on this side of the
    /// boundary and localises it to one half. That is what caught the range
    /// coder's shift-low truncation in the MTF stage.
    #[test]
    fn strong_bwt_round_trips() {
        let cases: Vec<Vec<u8>> = vec![
            b"banana".repeat(4),
            b"the quick brown fox jumps over the lazy dog. ".repeat(10),
            vec![b'A'; 200],
            (0..500u32).map(|i| (i % 7) as u8).collect(),
            (0..1000u32).map(|i| ((i * 37) % 251) as u8).collect(),
            // Long runs and a highly skewed alphabet stress the induction half.
            (0..300u32).flat_map(|i| core::iter::repeat((i % 3) as u8).take(9)).collect(),
        ];
        for (n, input) in cases.iter().enumerate() {
            let size = input.len();
            let mut coded = vec![0u8; size];
            let fbp = strong_encode(input, size, &mut coded).expect("encode");
            // The dispatcher ORs in the flag; do the same so `decode` picks the
            // matching inverse.
            let mut round = coded.clone();
            decode(&mut round, size, fbp | STRONG_BWT_FLAG).expect("decode");
            assert_eq!(round, *input, "case {n}");
        }
    }
}
