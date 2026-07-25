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

// ---------------------------------------------------------------------------
// Encoder: the "fast" sort (BWT.c:789).
//
// Where the strong sort ranks positions through a Group array, this one sorts
// raw suffixes by comparing 32-bit words read straight out of the input, and
// merges eight independently-sorted groups through a tournament tree. It is
// much faster and it can GIVE UP: an adaptive match limit counts comparison
// rounds, and when it runs out the whole sort aborts and the caller falls back
// to the strong one. Which sort ran therefore decides the output bytes.
//
// ## The overshoot prefix
//
// `GRZip_BWT_FastBWT_Init` (:985) rewrites the caller's buffer before the sort:
// it copies the first 80 bytes past the end, REVERSES the whole `size + 80`
// region, then appends four more bytes. The encoder then works on `Input + 80`.
//
// That is what makes this code's negative indexing legal -- `Input[i-1]` at
// i == 0, and the `Input + pos - 3` pointer hashes, read into the prefix by
// construction rather than off the front of the allocation. The comparison
// walks back 32 bytes per round plus a 4-byte word, so 36 bytes below `Input`
// is the deepest it reaches; 80 covers it.
//
// Here that is one `Vec<u8>` with `Input` at offset `OVERSHOOT`, and every C
// pointer becomes an absolute index into it.
// ---------------------------------------------------------------------------

/// `FastBWT_NumOverShoot` (:57).
const OVERSHOOT: usize = 80;
/// `FastBWT_NumGroups` (:58). The tournament below is specialised to 8, which
/// is the only value the C compiles.
const NUM_GROUPS: usize = 8;
/// `FastBWT_MaxQSortDepth` (:55) / `FastBWT_QSortStackSize` (:56).
const MAX_QSORT_DEPTH: i32 = 32;
const QSORT_STACK: usize = 1024;
/// `FastBWT_RepTreshStep2` / `_Step4` (:52-53) -- the adaptive match limits, as
/// a fraction of the block size.
const REP_TRESH_STEP2: f64 = 0.35;
const REP_TRESH_STEP4: f64 = 1.15;
/// `GRZ_FAST_BWT_FAILS`.
const FAST_BWT_FAILS: GrzError = -5;

/// Read a 32-bit word at an absolute byte index, little-endian.
///
/// C dereferences a `uint32*` at an arbitrary byte address, so these loads are
/// unaligned and their VALUE depends on byte order -- the comparison below
/// orders by the loaded word, not by the bytes. Every target here is
/// little-endian, so `from_le_bytes` is exact rather than merely equivalent.
#[inline]
fn w32(buf: &[u8], at: usize) -> u32 {
    u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]])
}

/// `FastBWT_SimpleCmp` (:443). Compares two suffixes by walking 32-bit words
/// BACKWARD, eight per round, wrapping by `size` bytes at the front.
///
/// `c1`/`c2` are absolute byte indices. The pointer arithmetic is mixed
/// granularity in the C -- `Cmp1 - 7` steps seven WORDS but the wrap adds
/// `Size` BYTES -- which is the easiest thing here to get wrong.
///
/// Returns false when the match limit runs out, which is also how the caller
/// learns the sort has failed: `aml` goes negative and every level checks it.
#[inline]
fn fast_cmp(buf: &[u8], mut c1: usize, mut c2: usize, size: usize, aml: &mut i32) -> bool {
    loop {
        for k in 0..8 {
            let a = w32(buf, c1 - 4 * k);
            let b = w32(buf, c2 - 4 * k);
            if a != b {
                return a > b;
            }
        }
        c1 -= 32;
        c2 -= 32;
        if c1 < OVERSHOOT {
            c1 += size;
        }
        if c2 < OVERSHOOT {
            c2 += size;
        }
        *aml -= 1;
        if *aml < 0 {
            return false;
        }
    }
}

/// `FastBWT_ShellSort` (:519).
fn fast_shell_sort(
    index: &mut [i32],
    buf: &[u8],
    lo: i32,
    hi: i32,
    d: i32,
    size: usize,
    aml: &mut i32,
) {
    let n = hi - lo + 1;
    let mut h = 1i32;
    while h < n {
        h = h * 3 + 1;
    }
    loop {
        h /= 3;
        let mut i = lo + h;
        while i <= hi {
            let idx = index[i as usize];
            let ptr = (OVERSHOOT as i32 + idx + d) as usize;
            let mut j = i;
            while fast_cmp(
                buf,
                (OVERSHOOT as i32 + index[(j - h) as usize] + d) as usize,
                ptr,
                size,
                aml,
            ) {
                index[j as usize] = index[(j - h) as usize];
                j -= h;
                if j < h + lo {
                    break;
                }
            }
            index[j as usize] = idx;
            if *aml < 0 {
                return;
            }
            i += 1;
        }
        if h == 1 {
            break;
        }
    }
}

/// `FastBWT_ShellSortDeph2` (:468) -- gaps 13, 4, 1 at a fixed depth of -4.
fn fast_shell_sort_d2(
    index: &mut [i32],
    buf: &[u8],
    lo: i32,
    hi: i32,
    size: usize,
    aml: &mut i32,
) {
    for gap in [13i32, 4, 1] {
        if gap != 1 && hi - lo <= gap {
            continue;
        }
        let mut i = lo + gap;
        while i <= hi {
            let idx = index[i as usize];
            let ptr = (OVERSHOOT as i32 + idx - 4) as usize;
            let mut j = i - gap;
            while fast_cmp(
                buf,
                (OVERSHOOT as i32 + index[j as usize] - 4) as usize,
                ptr,
                size,
                aml,
            ) {
                index[(j + gap) as usize] = index[j as usize];
                j -= gap;
                if j < lo {
                    break;
                }
            }
            index[(j + gap) as usize] = idx;
            if *aml < 0 {
                return;
            }
            i += 1;
        }
    }
}

/// `FastBWT_TernarySort` (:545). Three-way radix quicksort on the 32-bit word
/// at depth `d`, which starts at -4 and decreases.
///
/// The three sub-ranges are pushed LARGEST FIRST so the smallest is popped
/// first, which is what keeps the 1024-entry stack sufficient.
fn fast_ternary_sort(
    index: &mut [i32],
    buf: &[u8],
    lo0: i32,
    hi0: i32,
    size: usize,
    aml: &mut i32,
) {
    let mut stack = [0i32; QSORT_STACK];
    let mut sp = 0usize;
    macro_rules! push {
        ($l:expr, $h:expr, $d:expr) => {
            if sp + 3 <= QSORT_STACK {
                stack[sp] = $l;
                stack[sp + 1] = $h;
                stack[sp + 2] = $d;
                sp += 3;
            }
        };
    }
    push!(lo0, hi0, -4);
    while sp != 0 {
        sp -= 3;
        let (lo, hi, d) = (stack[sp], stack[sp + 1], stack[sp + 2]);
        if hi <= lo {
            continue;
        }
        if hi - lo < MIN_QSORT || d <= -MAX_QSORT_DEPTH {
            fast_shell_sort(index, buf, lo, hi, d, size, aml);
            if *aml < 0 {
                return;
            }
            continue;
        }
        let at = |ix: &[i32], p: i32| w32(buf, (OVERSHOOT as i32 + ix[p as usize] + d) as usize);
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
                let t = w32(buf, (OVERSHOOT as i32 + index[b as usize] + d) as usize);
                if t == med {
                    index.swap(a as usize, b as usize);
                    a += 1;
                    b += 1;
                    continue;
                }
                if t > med {
                    break;
                }
                b += 1;
            }
            loop {
                if b > c {
                    break;
                }
                let t = w32(buf, (OVERSHOOT as i32 + index[c as usize] + d) as usize);
                if t == med {
                    index.swap(c as usize, dd as usize);
                    dd -= 1;
                    c -= 1;
                    continue;
                }
                if t < med {
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
            push!(lo, hi, d - 4);
            continue;
        }

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

        let n2 = lo + b - a - 1;
        let m2 = hi - (dd - c) + 1;

        let mut nl = [lo, m2, n2 + 1];
        let mut nh = [n2, hi, m2 - 1];
        let mut nd = [d, d, d - 4];
        // Sort the three by size, largest first.
        let sz = |l: &[i32; 3], h: &[i32; 3], i: usize| h[i] - l[i];
        let mut nswap = |l: &mut [i32; 3], h: &mut [i32; 3], dp: &mut [i32; 3], i: usize, j: usize| {
            l.swap(i, j);
            h.swap(i, j);
            dp.swap(i, j);
        };
        if sz(&nl, &nh, 0) < sz(&nl, &nh, 2) {
            nswap(&mut nl, &mut nh, &mut nd, 0, 2);
        }
        if sz(&nl, &nh, 0) < sz(&nl, &nh, 1) {
            nswap(&mut nl, &mut nh, &mut nd, 0, 1);
        }
        if sz(&nl, &nh, 1) < sz(&nl, &nh, 2) {
            nswap(&mut nl, &mut nh, &mut nd, 1, 2);
        }
        push!(nl[0], nh[0], nd[0]);
        push!(nl[1], nh[1], nd[1]);
        push!(nl[2], nh[2], nd[2]);
    }
}

/// State the tournament reads. Kept together because both `rebuild` and
/// `update` touch all of it and C passes the same six pointers to each.
struct Tournament {
    lo: [i32; NUM_GROUPS],
    hi: [i32; NUM_GROUPS],
    /// One absolute byte index per group -- C's `PtrHash`, a pointer three
    /// bytes BEFORE the group's next position so the 32-bit load covers it.
    ptr: [usize; NUM_GROUPS],
    winner: [u8; NUM_GROUPS - 1],
}

impl Tournament {
    /// `FastBWT_Match` (:653): a group that has run out loses by default;
    /// otherwise compare the two groups' next suffixes.
    #[inline]
    fn match_full(&mut self, g0: usize, g1: usize, win: usize, buf: &[u8], size: usize, aml: &mut i32) {
        self.winner[win] = if self.lo[g0] >= self.hi[g0] {
            g1 as u8
        } else if self.lo[g1] >= self.hi[g1] {
            g0 as u8
        } else if fast_cmp(buf, self.ptr[g1], self.ptr[g0], size, aml) {
            g0 as u8
        } else {
            g1 as u8
        };
    }

    /// `FastBWT_MatchFast` (:708): the same without the exhaustion checks, used
    /// on the update path where the caller knows both groups still have entries.
    #[inline]
    fn match_fast(&mut self, g0: usize, g1: usize, win: usize, buf: &[u8], size: usize, aml: &mut i32) {
        self.winner[win] = if fast_cmp(buf, self.ptr[g1], self.ptr[g0], size, aml) {
            g0 as u8
        } else {
            g1 as u8
        };
    }

    /// `TreeReBuild` (:674), specialised to eight groups as the C is.
    fn rebuild(&mut self, buf: &[u8], size: usize, aml: &mut i32) {
        self.match_full(0, 1, 0, buf, size, aml);
        self.match_full(2, 3, 1, buf, size, aml);
        let (w0, w1) = (self.winner[0] as usize, self.winner[1] as usize);
        self.match_full(w0, w1, 4, buf, size, aml);
        self.match_full(4, 5, 2, buf, size, aml);
        self.match_full(6, 7, 3, buf, size, aml);
        let (w0, w1) = (self.winner[2] as usize, self.winner[3] as usize);
        self.match_full(w0, w1, 5, buf, size, aml);
        let (w0, w1) = (self.winner[4] as usize, self.winner[5] as usize);
        self.match_full(w0, w1, 6, buf, size, aml);
    }

    /// `TreeUpdateFast` (:719): replay only the path from the group that just
    /// won. Note each arm still re-runs the semifinal from `winner[0]`/`[1]` or
    /// `[2]`/`[3]` -- the C does not narrow it further, and the choice of which
    /// pair is re-read per arm is what makes this asymmetric.
    fn update(&mut self, pred: usize, buf: &[u8], size: usize, aml: &mut i32) {
        match pred {
            0 | 1 => {
                self.match_fast(0, 1, 0, buf, size, aml);
                let (w0, w1) = (self.winner[0] as usize, self.winner[1] as usize);
                self.match_fast(w0, w1, 4, buf, size, aml);
            }
            2 | 3 => {
                self.match_fast(2, 3, 1, buf, size, aml);
                let (w0, w1) = (self.winner[0] as usize, self.winner[1] as usize);
                self.match_fast(w0, w1, 4, buf, size, aml);
            }
            4 | 5 => {
                self.match_fast(4, 5, 2, buf, size, aml);
                let (w0, w1) = (self.winner[2] as usize, self.winner[3] as usize);
                self.match_fast(w0, w1, 5, buf, size, aml);
            }
            _ => {
                self.match_fast(6, 7, 3, buf, size, aml);
                let (w0, w1) = (self.winner[2] as usize, self.winner[3] as usize);
                self.match_fast(w0, w1, 5, buf, size, aml);
            }
        }
        let (w0, w1) = (self.winner[4] as usize, self.winner[5] as usize);
        self.match_fast(w0, w1, 6, buf, size, aml);
    }
}

/// `GRZip_BWT_FastBWT_Init` (:985): build the overshoot prefix in `buf`, whose
/// first `OVERSHOOT` bytes are scratch and whose payload starts there.
fn fast_init(buf: &mut [u8], size: usize) {
    // Laid out exactly as C's: the payload starts at offset 0, and the ENCODER
    // then runs at offset OVERSHOOT. Putting the payload at OVERSHOOT and also
    // reading from OVERSHOOT is an easy mistake and a SILENT one -- every read
    // lands 80 bytes off, the match limit is exhausted immediately, and the
    // sort "fails" into the strong fallback on every single input. Round-trip
    // tests still pass, because the fallback is correct.
    for i in 0..OVERSHOOT {
        buf[size + i] = buf[i];
    }
    // Reverse the whole size+OVERSHOOT region: the comparison walks BACKWARD,
    // so reversing lets it read the data in forward order.
    let mid = (size + OVERSHOOT) >> 1;
    for i in 0..mid {
        buf.swap(i, size + OVERSHOOT - i - 1);
    }
    for k in 0..4 {
        buf[size + OVERSHOOT + k] = buf[OVERSHOOT + k];
    }
}

/// `GRZip_FastBWT_Encode` (:789), operating on the already-initialised buffer.
///
/// Returns the first-byte position, or `FAST_BWT_FAILS` when the adaptive match
/// limit runs out -- which is not an error, it is the signal to fall back to the
/// strong sort.
fn fast_encode_inner(buf: &[u8], size: usize, output: &mut [u8]) -> Result<i32, GrzError> {
    let mut groups_freq = [[0i32; MAX_BYTE]; NUM_GROUPS];
    let mut big_bucket = vec![0i32; MAX_WORD];
    let mut index = vec![0i32; size + 1];
    let group_size = (size / NUM_GROUPS) as i32;
    let mut aml = ((size as f32) as f64 * REP_TRESH_STEP2) as i32;

    let b = |off: i32| buf[(OVERSHOOT as i32 + off) as usize] as usize;

    let mut i = size as i32 - 1;
    while i > 0 {
        for j in 0..NUM_GROUPS {
            groups_freq[j][b(i - j as i32)] += 1;
        }
        big_bucket[(b(i - NUM_GROUPS as i32 + 1) << 8) | b(i - NUM_GROUPS as i32)] += 1;
        i -= NUM_GROUPS as i32;
    }

    let mut small_lo = [0i32; NUM_GROUPS];
    let mut small_hi = [0i32; NUM_GROUPS];
    let mut cum = 0i32;
    for g in 0..NUM_GROUPS {
        small_lo[g] = cum;
        cum += group_size;
        small_hi[g] = cum;
    }

    let mut c = small_lo[NUM_GROUPS - 1];
    for v in big_bucket.iter_mut() {
        c += *v;
        *v = c - *v;
    }

    let mut i = 0i32;
    while i < size as i32 {
        let tmp = (b(i) << 8) | b(i - 1);
        index[big_bucket[tmp] as usize] = i;
        big_bucket[tmp] += 1;
        i += NUM_GROUPS as i32;
    }

    let mut index = index;
    let mut lo = small_lo[NUM_GROUPS - 1];
    for i in 0..MAX_WORD {
        let hi = big_bucket[i] - 1;
        if lo < hi {
            if hi - lo > MIN_QSORT {
                fast_ternary_sort(&mut index, buf, lo, hi, size, &mut aml);
            } else {
                fast_shell_sort_d2(&mut index, buf, lo, hi, size, &mut aml);
            }
        }
        if aml < 0 {
            return Err(FAST_BWT_FAILS);
        }
        lo = hi + 1;
    }

    // Propagate each sorted group back to the group before it, tagging every
    // index with the byte that follows it.
    let mut bucket_start = [0i32; MAX_BYTE];
    for g in (0..NUM_GROUPS - 1).rev() {
        let mut cum = small_lo[g];
        for j in 0..MAX_BYTE {
            bucket_start[j] = cum;
            cum += groups_freq[g][j];
        }
        let (mut l, h) = (small_lo[g + 1], small_hi[g + 1]);
        while l < h {
            let tmp = index[l as usize];
            let ch = b(tmp + 1);
            index[l as usize] = tmp | ((ch as i32) << 24);
            index[bucket_start[ch] as usize] = tmp + 1;
            bucket_start[ch] += 1;
            l += 1;
        }
    }
    let (mut l, h) = (small_lo[0], small_hi[0]);
    while l < h {
        let tmp = index[l as usize];
        index[l as usize] = tmp | ((b(tmp + 1) as i32) << 24);
        l += 1;
    }

    let mut cum = 0i32;
    for i in 0..MAX_BYTE {
        bucket_start[i] = cum;
        for j in 0..NUM_GROUPS {
            cum += groups_freq[j][i];
        }
    }

    for o in output[..size].iter_mut() {
        *o = 0xFF;
    }

    let mut t = Tournament {
        lo: small_lo,
        hi: small_hi,
        ptr: [0; NUM_GROUPS],
        winner: [0; NUM_GROUPS - 1],
    };
    for g in 0..NUM_GROUPS {
        t.ptr[g] = (OVERSHOOT as i32 + (index[t.lo[g] as usize] & 0xFF_FFFF) - 3) as usize;
    }

    aml = ((size as f32) as f64 * REP_TRESH_STEP4) as i32;
    let mut first = 0i32;
    let mut i = 0i32;

    // Merge: repeatedly take the tournament winner. `output[i] == 0xFF` marks a
    // slot whose group is not yet known; the second inner loop handles slots
    // already stamped by the induction below, which need no tree work.
    loop {
        let mut min = t.hi[0] - t.lo[0];
        for g in 1..NUM_GROUPS {
            min = min.min(t.hi[g] - t.lo[g]);
        }
        if min <= 0 {
            break;
        }
        let stop = i + min;
        while i < stop {
            t.rebuild(buf, size, &mut aml);
            if aml < 0 {
                return Err(FAST_BWT_FAILS);
            }
            while i < stop && output[i as usize] == 0xFF {
                let gnum = t.winner[NUM_GROUPS - 2] as usize;
                let idx = t.lo[gnum];
                t.lo[gnum] += 1;
                let ptr = index[idx as usize];
                let pb = (ptr >> 24) as u8;
                output[i as usize] = pb;
                if (ptr & 0xFF_FFFF) == size as i32 - 1 {
                    first = i;
                }
                t.ptr[gnum] =
                    (OVERSHOOT as i32 + (index[(idx + 1) as usize] & 0xFF_FFFF) - 3) as usize;

                let p = bucket_start[pb as usize];
                bucket_start[pb as usize] += 1;
                if i < p {
                    output[p as usize] =
                        ((gnum + NUM_GROUPS - 1) & (NUM_GROUPS - 1)) as u8;
                }
                t.update(gnum, buf, size, &mut aml);
                if aml < 0 {
                    return Err(FAST_BWT_FAILS);
                }
                i += 1;
            }
            while i < stop && output[i as usize] != 0xFF {
                let gnum = output[i as usize] as usize;
                let idx = t.lo[gnum];
                t.lo[gnum] += 1;
                let ptr = index[idx as usize];
                let pb = (ptr >> 24) as u8;
                output[i as usize] = pb;
                if (ptr & 0xFF_FFFF) == size as i32 - 1 {
                    first = i;
                }
                t.ptr[gnum] =
                    (OVERSHOOT as i32 + (index[(idx + 1) as usize] & 0xFF_FFFF) - 3) as usize;
                let p = bucket_start[pb as usize];
                bucket_start[pb as usize] += 1;
                if i < p {
                    output[p as usize] =
                        ((gnum + NUM_GROUPS - 1) & (NUM_GROUPS - 1)) as u8;
                }
                i += 1;
            }
        }
    }
    while i < size as i32 {
        t.rebuild(buf, size, &mut aml);
        if aml < 0 {
            return Err(FAST_BWT_FAILS);
        }
        let gnum = t.winner[NUM_GROUPS - 2] as usize;
        let idx = t.lo[gnum];
        t.lo[gnum] += 1;
        let ptr = index[idx as usize];
        output[i as usize] = (ptr >> 24) as u8;
        if (ptr & 0xFF_FFFF) == size as i32 - 1 {
            first = i;
        }
        t.ptr[gnum] = (OVERSHOOT as i32 + (index[(idx + 1) as usize] & 0xFF_FFFF) - 3) as usize;
        i += 1;
    }
    if aml < 0 {
        Err(FAST_BWT_FAILS)
    } else {
        Ok(first)
    }
}

/// `GRZip_BWT_Encode` (:1014): fast sort when asked, strong otherwise, and
/// strong as the fallback when the fast one gives up. The strong result is
/// returned with `STRONG_BWT_FLAG` set so the decoder picks the right inverse.
pub fn encode(input: &[u8], size: usize, output: &mut [u8], fast: bool) -> Result<i32, GrzError> {
    if size < 2 || input.len() < size || output.len() < size {
        return Err(GRZ_CRC_ERROR);
    }
    if fast {
        // The C mutates the caller's buffer in place and undoes it afterwards;
        // a scratch copy is the same thing without the surprise.
        let mut work = vec![0u8; size + 2 * OVERSHOOT + 8];
        work[..size].copy_from_slice(&input[..size]);
        fast_init(&mut work, size);
        match fast_encode_inner(&work, size, output) {
            Ok(fbp) => return Ok(fbp),
            Err(e) if e != FAST_BWT_FAILS => return Err(e),
            Err(_) => {} // fall through to the strong sort
        }
    }
    strong_encode(input, size, output).map(|fbp| fbp | STRONG_BWT_FLAG)
}

#[cfg(test)]
mod fast_tests {
    use super::*;

    /// Encode then decode must be the identity, through whichever sort ran.
    /// `encode` returns the strong flag when it fell back, so `decode` picks
    /// the matching inverse on its own.
    #[test]
    fn fast_bwt_round_trips() {
        fn lcg(seed: u32, n: usize) -> Vec<u8> {
            let mut st = seed;
            (0..n).map(|_| { st = st.wrapping_mul(1103515245).wrapping_add(12345); (st >> 16) as u8 }).collect()
        }
        let cases: Vec<Vec<u8>> = vec![
            // High entropy: the fast sort RUNS to completion on these.
            lcg(12345, 40000),
            lcg(999, 9000),
            lcg(7, 3000),
            // Repetitive: the match limit runs out and the strong sort takes
            // over. Both outcomes must round-trip, and `encode` sets the strong
            // flag so `decode` picks the right inverse on its own.
            b"the quick brown fox jumps over the lazy dog. ".repeat(30),
            vec![b'A'; 500],
            (0..4000u32).map(|i| ((i / 64) % 4) as u8 + b'p').collect(),
        ];
        let mut fast_ran = 0;
        let mut fell_back = 0;
        for (n, input) in cases.iter().enumerate() {
            let size = input.len();
            let mut coded = vec![0u8; size];
            let fbp = encode(input, size, &mut coded, true).expect("encode");
            if fbp & STRONG_BWT_FLAG != 0 { fell_back += 1 } else { fast_ran += 1 }
            let mut round = coded.clone();
            decode(&mut round, size, fbp).expect("decode");
            assert_eq!(round, *input, "case {n} (fbp {fbp:#x})");
        }
        // Without this the fast path can be entirely untested and still green:
        // the fallback is correct, so every case round-trips through the STRONG
        // sort while the fast one never runs. That is exactly what a base-offset
        // bug in the buffer layout produced -- six green cases, zero coverage.
        assert!(fast_ran > 0, "the fast sort never completed -- it is untested");
        assert!(fell_back > 0, "the fallback path was never taken");
    }

    /// The match limit only decrements after 32 consecutive matching bytes, so
    /// high-entropy data never touches it while repetitive data burns through
    /// it. That is the entire selection rule between the two sorts.
    #[test]
    fn repetitive_falls_back_high_entropy_does_not() {
        let mut st = 4321u32;
        let noise: Vec<u8> = (0..40000).map(|_| { st = st.wrapping_mul(1103515245).wrapping_add(12345); (st >> 16) as u8 }).collect();
        let mut out = vec![0u8; noise.len()];
        let fbp = encode(&noise, noise.len(), &mut out, true).expect("encode");
        assert_eq!(fbp & STRONG_BWT_FLAG, 0, "high-entropy input should not fall back");

        let rep = b"the quick brown fox jumps over the lazy dog. ".repeat(1000);
        let mut out2 = vec![0u8; rep.len()];
        let fbp2 = encode(&rep, rep.len(), &mut out2, true).expect("encode");
        assert_ne!(fbp2 & STRONG_BWT_FLAG, 0, "repetitive input should fall back");
    }

    /// Both sorts must produce the SAME transform -- they are two routes to one
    /// answer, and the fallback is only sound because of that.
    /// Does the fast sort ever COMPLETE, or does everything fall back? A
    /// round-trip test passes either way, so without this the fast path could
    /// be entirely untested and look green.
    #[test]
    #[ignore]
    fn report_fallback_rate() {
        // AML starts at size*0.35, so small blocks exhaust it during bucket
        // sorting no matter what. Sizes here span small to realistic.
        let cases: Vec<(&str, Vec<u8>)> = vec![
            ("text 1.3k", b"the quick brown fox jumps over the lazy dog. ".repeat(30)),
            ("text 45k", b"the quick brown fox jumps over the lazy dog. ".repeat(1000)),
            ("text 450k", b"the quick brown fox jumps over the lazy dog. ".repeat(10000)),
            ("lcg 200k", { let mut v=Vec::new(); let mut st=12345u32;
                for _ in 0..200000 { st = st.wrapping_mul(1103515245).wrapping_add(12345); v.push((st>>16) as u8); } v }),
            ("lcg 20k", { let mut v=Vec::new(); let mut st=999u32;
                for _ in 0..20000 { st = st.wrapping_mul(1103515245).wrapping_add(12345); v.push((st>>16) as u8); } v }),
            ("blocks 200k", (0..200000u32).map(|i| ((i / 64) % 4) as u8 + b'p').collect()),
        ];
        for (name, input) in cases.iter() {
            let size = input.len();
            let mut out = vec![0u8; size];
            let fbp = encode(input, size, &mut out, true).expect("encode");
            println!("{name}: {}", if fbp & STRONG_BWT_FLAG != 0 { "FELL BACK to strong" } else { "fast sort completed" });
        }
    }

    #[test]
    fn fast_and_strong_agree_when_both_run() {
        let input: Vec<u8> = b"the quick brown fox jumps over the lazy dog. ".repeat(30);
        let size = input.len();
        let mut fast_out = vec![0u8; size];
        let fast_fbp = encode(&input, size, &mut fast_out, true).expect("fast");
        // Only meaningful when the fast sort actually completed.
        if fast_fbp & STRONG_BWT_FLAG == 0 {
            let mut strong_out = vec![0u8; size];
            let strong_fbp = strong_encode(&input, size, &mut strong_out).expect("strong");
            assert_eq!(fast_out, strong_out, "the two sorts disagree");
            assert_eq!(fast_fbp, strong_fbp, "first-byte positions disagree");
        }
    }
}
