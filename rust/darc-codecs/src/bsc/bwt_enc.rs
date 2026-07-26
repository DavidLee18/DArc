//! Forward Burrows-Wheeler transform -- the last block sorter BSC needs.
//!
//! # Why this is not a port of libsais
//!
//! The C reaches this through `libsais_bwt_aux`, and libsais is 8,532 lines:
//! SIMD induction, OpenMP partitioning, context pooling, 16-bit and 64-bit
//! variants. Almost none of that is *semantics*; it is engineering to make one
//! particular answer arrive quickly.
//!
//! The answer is unique. Every suffix of a string has a distinct length, so no
//! two suffixes are equal, so the suffix array is totally ordered with no ties
//! to break -- and the BWT and its primary index are functions of that array.
//! Any correct construction therefore produces the same bytes as libsais.
//!
//! That is a claim worth testing rather than believing, so it was tested before
//! any of this was written: a `qsort`-with-`memcmp` suffix array -- the
//! slowest correct construction there is -- reproduces `libsais_bwt`'s output
//! and index exactly across 21 inputs, including the shapes where a tie-break
//! would show up (`banana`, `mississippi`, `abcabcabc`*1000, `ab`*4000,
//! all-zeros, single-byte, full alphabet, already-sorted). A second experiment
//! confirmed the sampled-index convention the same way. Had either come back
//! false, this module would have had to mirror libsais's structure instead.
//!
//! So what is implemented here is SA-IS: linear time, ~250 lines, and the
//! algorithm libsais is itself an optimised implementation of.
//!
//! # The two output conventions, both verified against the C
//!
//! `libsais_bwt` does not write the BWT array as-is. With `p` the position
//! where `SA[p] == 0`, it writes `T[n-1]` first, then the BWT with the entry at
//! `p` omitted, and returns `p + 1`.
//!
//! `libsais_bwt_aux` additionally fills `I[j] = (position i with SA[i] == j*r)
//! + 1` for `j` in `0 ..= (n-1)/r`, where `I[0]` is the primary index. DArc
//! always takes this path: libbsc.cpp's `indexes[256]` is a stack array, so the
//! pointer is never null.

/// Marker for an unfilled suffix-array slot. `n` never reaches `u32::MAX`
/// because a BSC block is bounded well below 4 GB.
const EMPTY: u32 = u32::MAX;

/// True where the suffix at `i` is S-type (smaller than the suffix at `i+1`).
fn build_types(t: &[u32]) -> Vec<bool> {
    let n = t.len();
    let mut ty = vec![false; n];
    // The sentinel is the smallest suffix, hence S-type.
    ty[n - 1] = true;
    for i in (0..n - 1).rev() {
        ty[i] = match t[i].cmp(&t[i + 1]) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
            // Equal: the type propagates from the right.
            std::cmp::Ordering::Equal => ty[i + 1],
        };
    }
    ty
}

/// An LMS ("leftmost S") position: S-type with an L-type immediately before it.
#[inline]
fn is_lms(ty: &[bool], i: usize) -> bool {
    i > 0 && ty[i] && !ty[i - 1]
}

fn bucket_sizes(t: &[u32], k: usize) -> Vec<u32> {
    let mut b = vec![0u32; k];
    for &c in t {
        b[c as usize] += 1;
    }
    b
}

/// Exclusive prefix sums: the first slot of each bucket.
fn bucket_heads(bkt: &[u32]) -> Vec<i64> {
    let mut h = Vec::with_capacity(bkt.len());
    let mut sum = 0i64;
    for &c in bkt {
        h.push(sum);
        sum += c as i64;
    }
    h
}

/// Inclusive prefix sums minus one: the last slot of each bucket.
fn bucket_tails(bkt: &[u32]) -> Vec<i64> {
    let mut tl = Vec::with_capacity(bkt.len());
    let mut sum = 0i64;
    for &c in bkt {
        sum += c as i64;
        tl.push(sum - 1);
    }
    tl
}

/// The induced-sort core: given the LMS suffixes already in place, fill in
/// every L-type left to right, then every S-type right to left.
fn induce(t: &[u32], sa: &mut [u32], ty: &[bool], bkt: &[u32]) {
    let n = t.len();

    let mut heads = bucket_heads(bkt);
    for i in 0..n {
        let v = sa[i];
        // `v > 0` skips both EMPTY and position 0, which has no predecessor.
        if v != EMPTY && v > 0 {
            let j = (v - 1) as usize;
            if !ty[j] {
                let c = t[j] as usize;
                sa[heads[c] as usize] = j as u32;
                heads[c] += 1;
            }
        }
    }

    let mut tails = bucket_tails(bkt);
    for i in (0..n).rev() {
        let v = sa[i];
        if v != EMPTY && v > 0 {
            let j = (v - 1) as usize;
            if ty[j] {
                let c = t[j] as usize;
                sa[tails[c] as usize] = j as u32;
                tails[c] -= 1;
            }
        }
    }
}

/// Are the LMS substrings starting at `a` and `b` equal, symbol and type alike?
fn lms_substr_eq(t: &[u32], ty: &[bool], a: usize, b: usize) -> bool {
    let n = t.len();
    // The sentinel's LMS substring is the sentinel alone, equal only to itself.
    if a == n - 1 || b == n - 1 {
        return a == b;
    }
    let mut d = 0usize;
    loop {
        let a_end = d > 0 && is_lms(ty, a + d);
        let b_end = d > 0 && is_lms(ty, b + d);
        if a_end && b_end {
            return true;
        }
        if a_end != b_end {
            return false;
        }
        if t[a + d] != t[b + d] || ty[a + d] != ty[b + d] {
            return false;
        }
        d += 1;
        // Safe: the sentinel at n-1 is LMS, so one side always terminates.
    }
}

/// Suffix array of `t` by SA-IS. `t` must end with a unique `0` sentinel that
/// occurs nowhere else, and every symbol must be below `k`.
fn sais(t: &[u32], k: usize) -> Vec<u32> {
    let n = t.len();
    if n == 0 {
        return Vec::new();
    }
    if n == 1 {
        return vec![0];
    }

    let ty = build_types(t);
    let bkt = bucket_sizes(t, k);
    let lms: Vec<u32> = (1..n).filter(|&i| is_lms(&ty, i)).map(|i| i as u32).collect();
    let n1 = lms.len();

    // Round one: LMS suffixes dropped into their buckets in arbitrary order.
    // Inducing from them sorts the LMS *substrings*, which is enough to name.
    let mut sa = vec![EMPTY; n];
    {
        let mut tails = bucket_tails(&bkt);
        for &p in lms.iter().rev() {
            let c = t[p as usize] as usize;
            sa[tails[c] as usize] = p;
            tails[c] -= 1;
        }
    }
    induce(t, &mut sa, &ty, &bkt);

    // Name the LMS substrings in the order that sort put them.
    let mut sorted_lms = Vec::with_capacity(n1);
    for i in 0..n {
        let v = sa[i];
        if v != EMPTY && v > 0 && is_lms(&ty, v as usize) {
            sorted_lms.push(v);
        }
    }

    let mut names = vec![0u32; n];
    let mut cur = 0u32;
    if !sorted_lms.is_empty() {
        names[sorted_lms[0] as usize] = 0;
        let mut prev = sorted_lms[0] as usize;
        for &pos in &sorted_lms[1..] {
            let pos = pos as usize;
            if !lms_substr_eq(t, &ty, prev, pos) {
                cur += 1;
            }
            names[pos] = cur;
            prev = pos;
        }
    }
    let name_count = cur as usize + 1;

    // The reduced string: one name per LMS position, in TEXT order.
    let s1: Vec<u32> = lms.iter().map(|&p| names[p as usize]).collect();

    // If names are not yet unique the substring order is not the suffix order,
    // so recurse. The reduced string ends with the sentinel's name, which is 0
    // and unique, so it satisfies this function's own precondition.
    let sa1 = if name_count < n1 {
        sais(&s1, name_count)
    } else {
        let mut r = vec![0u32; n1];
        for (i, &name) in s1.iter().enumerate() {
            r[name as usize] = i as u32;
        }
        r
    };

    // Round two: place the LMS suffixes in their true order, then induce again.
    let mut sa = vec![EMPTY; n];
    {
        let mut tails = bucket_tails(&bkt);
        for i in (0..n1).rev() {
            let p = lms[sa1[i] as usize];
            let c = t[p as usize] as usize;
            sa[tails[c] as usize] = p;
            tails[c] -= 1;
        }
    }
    induce(t, &mut sa, &ty, &bkt);
    sa
}

/// Suffix array of a byte string, without sentinel, as `u32` positions.
fn suffix_array(input: &[u8]) -> Vec<u32> {
    let n = input.len();
    // Shift by one so 0 can serve as a sentinel no real byte can collide with.
    let mut t: Vec<u32> = Vec::with_capacity(n + 1);
    t.extend(input.iter().map(|&b| b as u32 + 1));
    t.push(0);
    let mut sa = sais(&t, 257);
    // sa[0] is the sentinel suffix; drop it.
    sa.remove(0);
    sa
}

/// `libsais_bwt` (`libsais.c:7110`): the BWT of `input` into `output`, with the
/// entry for the position-0 suffix omitted and `input[n-1]` written first.
///
/// Returns `p + 1` where `SA[p] == 0`, matching the C, or `n` for `n <= 1`.
pub fn bwt_encode(input: &[u8], output: &mut [u8]) -> i32 {
    let n = input.len();
    if n <= 1 {
        if n == 1 {
            output[0] = input[0];
        }
        return n as i32;
    }

    let sa = suffix_array(input);
    let mut p = 0usize;
    let mut l = vec![0u8; n];
    for i in 0..n {
        let s = sa[i] as usize;
        if s == 0 {
            p = i;
        }
        l[i] = input[(s + n - 1) % n];
    }

    output[0] = input[n - 1];
    let mut k = 1usize;
    for i in 0..n {
        if i == p {
            continue;
        }
        output[k] = l[i];
        k += 1;
    }
    (p + 1) as i32
}

/// `libsais_bwt_aux` (`libsais.c:7136`): as [`bwt_encode`], and additionally
/// `i[j] = (position with SA == j*r) + 1`. Returns 0 on success, matching the C
/// -- the primary index comes back in `i[0]`, not in the return value.
///
/// `r` must be a power of two of at least 2, and `i` must hold at least
/// `(n-1)/r + 1` entries.
pub fn bwt_aux_encode(input: &[u8], output: &mut [u8], r: usize, i_out: &mut [i32]) -> i32 {
    let n = input.len();
    if r < 2 || (r & (r - 1)) != 0 {
        return -1; // LIBBSC_BAD_PARAMETER
    }
    if n <= 1 {
        if n == 1 {
            output[0] = input[0];
        }
        i_out[0] = n as i32;
        return 0;
    }

    let sa = suffix_array(input);
    let mut p = 0usize;
    let mut l = vec![0u8; n];
    // pos_of[s] is where suffix s landed; the sampled indexes read it directly.
    let mut pos_of = vec![0u32; n];
    for idx in 0..n {
        let s = sa[idx] as usize;
        pos_of[s] = idx as u32;
        if s == 0 {
            p = idx;
        }
        l[idx] = input[(s + n - 1) % n];
    }

    let count = (n - 1) / r;
    for j in 0..=count {
        i_out[j] = pos_of[j * r] as i32 + 1;
    }

    output[0] = input[n - 1];
    let mut k = 1usize;
    for idx in 0..n {
        if idx == p {
            continue;
        }
        output[k] = l[idx];
        k += 1;
    }
    0
}

/// `bsc_bwt_encode` (`bwt.cpp:178`): the aux transform plus the `mod` the C
/// derives from `n`, writing the sampled indexes the caller stores in the
/// block. Returns the primary index, or a negative libbsc error.
pub fn bsc_bwt_encode(
    input: &mut [u8],
    n: usize,
    num_indexes: &mut u8,
    indexes: &mut [i32],
) -> i32 {
    if n <= 1 {
        *num_indexes = 0;
        return n as i32;
    }

    // bwt.cpp:193 -- the largest power of two at or below n/8, halved.
    let mut m = n / 8;
    m |= m >> 1;
    m |= m >> 2;
    m |= m >> 4;
    m |= m >> 8;
    m |= m >> 16;
    m >>= 1;

    let src = input[..n].to_vec();
    let mut i_arr = vec![0i32; 256];
    let rc = bwt_aux_encode(&src, &mut input[..n], m + 1, &mut i_arr);
    if rc != 0 {
        return rc;
    }

    // The C only publishes the sampled indexes when the aux call succeeded.
    *num_indexes = ((n - 1) / (m + 1)) as u8;
    let index = i_arr[0];
    for t in 0..*num_indexes as usize {
        indexes[t] = i_arr[t + 1] - 1;
    }
    index
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The suffix array by brute force, for cross-checking SA-IS on small
    /// inputs. Deliberately the dumbest correct construction.
    fn naive_sa(input: &[u8]) -> Vec<u32> {
        let n = input.len();
        let mut sa: Vec<u32> = (0..n as u32).collect();
        sa.sort_by(|&a, &b| input[a as usize..].cmp(&input[b as usize..]));
        sa
    }

    #[test]
    fn sais_matches_brute_force() {
        let cases: Vec<Vec<u8>> = vec![
            b"banana".to_vec(),
            b"mississippi".to_vec(),
            b"abracadabra".to_vec(),
            b"aaaaaaaa".to_vec(),
            b"ab".repeat(50),
            b"abc".repeat(37),
            (0u8..=255).collect(),
            vec![0u8; 100],
            b"the quick brown fox".to_vec(),
        ];
        for c in cases {
            assert_eq!(suffix_array(&c), naive_sa(&c), "SA differs for {:?}", &c[..c.len().min(16)]);
        }
    }

    #[test]
    fn sais_matches_brute_force_on_random() {
        let mut s: u32 = 12345;
        for trial in 0..200 {
            let n = 1 + (trial * 7) % 300;
            // A small alphabet makes repeats -- and thus recursion -- likely.
            let alpha = 1 + (trial % 4) as u8;
            let data: Vec<u8> = (0..n)
                .map(|_| {
                    s = s.wrapping_mul(1103515245).wrapping_add(12345);
                    ((s >> 16) as u8) % alpha
                })
                .collect();
            assert_eq!(suffix_array(&data), naive_sa(&data), "SA differs, trial {trial}");
        }
    }

    #[test]
    fn bwt_round_trips_through_a_naive_inverse() {
        for case in [b"banana".to_vec(), b"mississippi".to_vec(), b"abracadabra".to_vec()] {
            let n = case.len();
            let mut out = vec![0u8; n];
            let index = bwt_encode(&case, &mut out) as usize;

            // Undo the C's packing: reinsert the omitted entry to get the BWT.
            let mut l = Vec::with_capacity(n);
            l.extend_from_slice(&out[1..index]);
            l.push(out[0]);
            l.extend_from_slice(&out[index..]);

            // Textbook inverse BWT from the full L column and primary index.
            let mut counts = [0usize; 256];
            for &b in &l {
                counts[b as usize] += 1;
            }
            let mut starts = [0usize; 256];
            let mut sum = 0;
            for c in 0..256 {
                starts[c] = sum;
                sum += counts[c];
            }
            // LF(i): the row of the suffix one character to the left. Note this
            // is the mapping itself, not its inverse -- walking the inverse
            // rebuilds the string rotated by one.
            let mut lf = vec![0usize; n];
            let mut seen = [0usize; 256];
            for i in 0..n {
                let c = l[i] as usize;
                lf[i] = starts[c] + seen[c];
                seen[c] += 1;
            }
            // Row `index - 1` is the suffix starting at 0: the original string.
            let mut p = index - 1;
            let mut rebuilt = vec![0u8; n];
            for i in (0..n).rev() {
                rebuilt[i] = l[p];
                p = lf[p];
            }
            assert_eq!(rebuilt, case, "round trip failed");
        }
    }
}
