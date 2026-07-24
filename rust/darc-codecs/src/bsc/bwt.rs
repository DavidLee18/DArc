//! Inverse Burrows-Wheeler transform, ported from libsais' `libsais_unbwt`
//! (`Compression/BSC/libbsc/bwt/libsais/libsais.c`), driven through
//! `bsc_bwt_decode` (`bwt.cpp:285`).
//!
//! This is the biPSI (bigram-PSI) inverse BWT with a 17-bit "fastbits" lookup
//! that libsais uses to skip most of the linear scan for each character. It is
//! the single-threaded path; the OpenMP fan-out and the CUDA path above it are
//! throughput only and never on the format's decode surface (BSC builds here
//! without either).
//!
//! ## Why the strided multi-block walk has to be ported, not just `decode_1`
//!
//! For a block of at least 64 KiB the encoder stores, past the primary index,
//! an array of `num_indexes` auxiliary checkpoints (`libbsc.cpp:303,320`); the
//! decoder then splits the output into `num_indexes + 1` regions of `r = mod+1`
//! bytes and walks them in lockstep (`libsais_unbwt_decode`). Those checkpoints
//! are redundant -- the primary index alone inverts the block -- but the C uses
//! them whenever they are present, and its output is identical either way. So
//! reading real archives means reproducing both: `r == n` (single index, small
//! blocks) and `r < n` (aux indices, blocks >= 64 KiB). The corpus straddles
//! 64 KiB so both are exercised.
//!
//! ## Little-endian is baked in
//!
//! The decode writes each character *pair* as `bswap16(c)` stored native
//! (`libsais.c:7735`). DArc is `FREEARC_INTEL_BYTE_ORDER`, so that is: high byte
//! of the bigram first, low byte second. Reproduced with explicit byte writes so
//! it does not silently depend on the host's endianness.
//!
//! ## Every index is bounded
//!
//! This runs under `arc t` on attacker-supplied data. libsais trusts its input
//! (the checked adler32 upstream is the real guard); here the fastbits scan, the
//! PSI follow and every output write are range-checked, so corrupt input is
//! `LIBBSC_DATA_CORRUPT`, never a panic across the C ABI.

use super::{LIBBSC_BAD_PARAMETER, LIBBSC_DATA_CORRUPT, LIBBSC_NO_ERROR};

const ALPHABET_SIZE: usize = 256;
const UNBWT_FASTBITS: u32 = 17;

/// `n >> shift` must not exceed `1 << UNBWT_FASTBITS`; `shift` is the smallest
/// that achieves it (`libsais.c:7569`).
fn compute_shift(n: usize) -> u32 {
    let mut shift = 0u32;
    while (n >> shift) > (1usize << UNBWT_FASTBITS) {
        shift += 1;
    }
    shift
}

/// The auxiliary-index stride `mod + 1` (`bwt.cpp:297-302`): `n/8` rounded down
/// to a power of two.
fn aux_stride(n: usize) -> usize {
    let mut m = n / 8;
    m |= m >> 1;
    m |= m >> 2;
    m |= m >> 4;
    m |= m >> 8;
    m |= m >> 16;
    m >>= 1;
    m + 1
}

/// Prefix-sum `bucket1` (starting at 1), and for each present character fill its
/// 256-wide slice of `bucket2` with the histogram of the characters that precede
/// it in `T` -- split at `index` exactly as `libsais_unbwt_compute_bigram_histogram_single`
/// does -- then transpose (`libsais.c:7485`).
fn compute_bigram_histogram(t: &[u8], bucket1: &mut [u32; ALPHABET_SIZE], bucket2: &mut [u32], index: usize) {
    let mut sum: usize = 1;
    for c in 0..ALPHABET_SIZE {
        let prev = sum;
        sum += bucket1[c] as usize;
        bucket1[c] = prev as u32;
        if prev != sum {
            let base = c << 8;
            let bp = &mut bucket2[base..base + ALPHABET_SIZE];

            let hi = index.min(sum);
            if hi > prev {
                for &b in &t[prev..hi] {
                    bp[b as usize] += 1;
                }
            }

            let lo = (index + 1).max(prev);
            if sum > lo {
                for &b in &t[lo - 1..sum - 1] {
                    bp[b as usize] += 1;
                }
            }
        }
    }
    transpose_bucket2(bucket2);
}

/// Transpose the 256x256 `bucket2` matrix in place. libsais does this cache-blocked
/// for speed; the plain transpose is identical (`libsais.c:7444`).
fn transpose_bucket2(bucket2: &mut [u32]) {
    for a in 0..ALPHABET_SIZE {
        for d in (a + 1)..ALPHABET_SIZE {
            bucket2.swap((a << 8) | d, (d << 8) | a);
        }
    }
}

/// Prefix-sum `bucket2` and fill the `fastbits` acceleration table: `fastbits[v]`
/// is the largest bigram whose running total's top bits are `<= v`
/// (`libsais.c:7510`).
fn calculate_fastbits(bucket2: &mut [u32], fastbits: &mut [u16], lastc: usize, shift: u32) {
    let mut v: usize = 0;
    let mut w: usize = 0;
    let mut sum: usize = 1;
    for c in 0..ALPHABET_SIZE {
        if c == lastc {
            sum += 1;
        }
        for _d in 0..ALPHABET_SIZE {
            let prev = sum;
            sum += bucket2[w] as usize;
            bucket2[w] = prev as u32;
            if prev != sum {
                let lim = (sum - 1) >> shift;
                // The C proves v stays in bounds on valid data; the guard only
                // stops a panic on corrupt input.
                while v <= lim && v < fastbits.len() {
                    fastbits[v] = w as u16;
                    v += 1;
                }
            }
            w += 1;
        }
    }
}

/// Build the PSI array `P`: `P[bucket2[w]++] = i` for each position, keyed on the
/// bigram `w` formed by the character at `i` and its predecessor across the
/// `index` split (`libsais_unbwt_calculate_biPSI`, `libsais.c:7528`). Returns
/// `false` on any out-of-range index (corrupt input).
fn calculate_bipsi(
    t: &[u8],
    p_arr: &mut [u32],
    bucket1: &mut [u32; ALPHABET_SIZE],
    bucket2: &mut [u32],
    index: usize,
    n: usize,
) -> bool {
    let step = |t: &[u8], p_arr: &mut [u32], bucket1: &mut [u32; ALPHABET_SIZE], bucket2: &mut [u32], i: usize, c: usize| -> bool {
        let p = bucket1[c] as usize;
        bucket1[c] += 1;
        if index != p {
            // t >> (bits-1): -1 (i.e. p-1) when index < p, else 0 (p).
            let off = if index < p { p.wrapping_sub(1) } else { p };
            if off >= n {
                return false;
            }
            let w = ((t[off] as usize) << 8) + c;
            let dst = bucket2[w] as usize;
            bucket2[w] += 1;
            if dst >= p_arr.len() {
                return false;
            }
            p_arr[dst] = i as u32;
        }
        true
    };

    let j = index.min(n);
    for i in 0..j {
        let c = t[i] as usize;
        if !step(t, p_arr, bucket1, bucket2, i, c) {
            return false;
        }
    }
    for i in (index + 1)..=n {
        let c = t[i - 1] as usize;
        if !step(t, p_arr, bucket1, bucket2, i, c) {
            return false;
        }
    }
    true
}

/// `libsais_unbwt_init_single` (`libsais.c:7563`): histogram, bigram histogram,
/// fastbits, then PSI. `freq` is always NULL on BSC's path.
fn init_single(t: &[u8], p_arr: &mut [u32], n: usize, index: usize, bucket2: &mut [u32], fastbits: &mut [u16]) -> bool {
    let mut bucket1 = [0u32; ALPHABET_SIZE];
    let lastc = t[0] as usize;
    let shift = compute_shift(n);

    for &b in &t[..n] {
        bucket1[b as usize] += 1;
    }

    compute_bigram_histogram(t, &mut bucket1, bucket2, index);
    calculate_fastbits(bucket2, fastbits, lastc, shift);
    calculate_bipsi(t, p_arr, &mut bucket1, bucket2, index, n)
}

/// One lockstep pass over `bases.len()` output regions: region `blk` starts its
/// PSI walk at `ps[blk]`, writes `k` bigrams at `bases[blk] + 2*i`, and leaves
/// its follow position back in `ps[blk]`. This is the shared body of
/// `libsais_unbwt_decode_1..8` (`libsais.c:7727`).
fn decode_walk(
    out: &mut [u8],
    p_arr: &[u32],
    bucket2: &[u32],
    fastbits: &[u16],
    shift: u32,
    bases: &[usize],
    ps: &mut [usize],
    k: usize,
    n: usize,
) -> Result<(), i32> {
    for blk in 0..bases.len() {
        let mut p = ps[blk];
        let base = bases[blk];
        for i in 0..k {
            if p >= p_arr.len() {
                return Err(LIBBSC_DATA_CORRUPT);
            }
            let mut c = fastbits[p >> shift] as usize;
            while bucket2[c] as usize <= p {
                c += 1;
                if c >= bucket2.len() {
                    return Err(LIBBSC_DATA_CORRUPT);
                }
            }
            p = p_arr[p] as usize;
            let o = base + 2 * i;
            // The last byte (n-1) is written separately as lastc, so a well-
            // formed walk never addresses byte n.
            if o + 1 >= n {
                return Err(LIBBSC_DATA_CORRUPT);
            }
            out[o] = (c >> 8) as u8;
            out[o + 1] = (c & 0xff) as u8;
        }
        ps[blk] = p;
    }
    Ok(())
}

/// `libsais_unbwt_decode` + `_decode_omp` for one thread (`libsais.c:7895,7956`):
/// peel the blocks in groups of eight, then handle the final 1..8 with the two-
/// phase split that lets the short last block stop early.
fn decode_all(t: &[u8], p_arr: &[u32], n: usize, r: usize, indexes: &[usize], bucket2: &[u32], fastbits: &[u16]) -> Result<Vec<u8>, i32> {
    let mut out = vec![0u8; n];
    let lastc = t[0];
    let shift = compute_shift(n);

    let blocks_total = 1 + (n - 1) / r;
    let remainder = n - r * (blocks_total - 1);
    let half_r = r >> 1;
    let half_rem = remainder >> 1;

    let mut ii = 0usize;
    let mut blocks = blocks_total;
    let mut offset = 0usize;

    while blocks > 8 {
        let bases: Vec<usize> = (0..8).map(|j| offset + j * r).collect();
        let mut ps: Vec<usize> = (0..8).map(|j| indexes[ii + j]).collect();
        decode_walk(&mut out, p_arr, bucket2, fastbits, shift, &bases, &mut ps, half_r, n)?;
        ii += 8;
        blocks -= 8;
        offset += 8 * r;
    }

    // Final group of `b` blocks. All `b` advance for the first `half_rem`
    // bigrams; then the last (short) block is done and the other `b-1` continue.
    let b = blocks;
    let bases: Vec<usize> = (0..b).map(|j| offset + j * r).collect();
    let mut ps: Vec<usize> = (0..b).map(|j| indexes[ii + j]).collect();
    decode_walk(&mut out, p_arr, bucket2, fastbits, shift, &bases, &mut ps, half_rem, n)?;
    if b > 1 {
        let bases2: Vec<usize> = (0..b - 1).map(|j| offset + 2 * half_rem + j * r).collect();
        let mut ps2: Vec<usize> = ps[..b - 1].to_vec();
        decode_walk(&mut out, p_arr, bucket2, fastbits, shift, &bases2, &mut ps2, half_r - half_rem, n)?;
    }

    out[n - 1] = lastc;
    Ok(out)
}

/// `libsais_unbwt_aux(T, U, P, n, NULL, r, I)` for one thread: allocate the
/// scratch, init, decode. `indexes[0]` is the primary index.
fn unbwt(t: &[u8], n: usize, r: usize, indexes: &[usize]) -> Result<Vec<u8>, i32> {
    let shift = compute_shift(n);
    let mut bucket2 = vec![0u32; ALPHABET_SIZE * ALPHABET_SIZE];
    let mut fastbits = vec![0u16; 1 + (n >> shift)];
    let mut p_arr = vec![0u32; n + 1];

    if !init_single(t, &mut p_arr, n, indexes[0], &mut bucket2, &mut fastbits) {
        return Err(LIBBSC_DATA_CORRUPT);
    }
    decode_all(t, &p_arr, n, r, indexes, &bucket2, &fastbits)
}

/// `bsc_bwt_decode` (`bwt.cpp:285`): validate, decide the aux vs single-index
/// path from `num_indexes`, invert `data` in place. Returns `LIBBSC_NO_ERROR`
/// or a negative libbsc error code.
pub fn bwt_decode(data: &mut [u8], n: usize, index: i32, num_indexes: u8, indexes: &[i32]) -> i32 {
    if index <= 0 || index as usize > n {
        return LIBBSC_BAD_PARAMETER;
    }
    if n <= 1 {
        return LIBBSC_NO_ERROR;
    }

    let stride = aux_stride(n);
    let expected = (n - 1) / stride;

    // Build the index list and stride exactly as bsc_bwt_decode chooses them.
    let (r, idx): (usize, Vec<usize>) = if num_indexes as usize == expected && !indexes.is_empty() {
        let mut v = Vec::with_capacity(num_indexes as usize + 1);
        v.push(index as usize);
        for t in 0..num_indexes as usize {
            v.push(indexes[t] as usize + 1);
        }
        (stride, v)
    } else {
        (n, vec![index as usize])
    };

    // libsais_unbwt_aux's own validation: r == n, or r a power of two >= 2; and
    // every index in (0, n].
    if r != n && (r < 2 || (r & (r - 1)) != 0) {
        return LIBBSC_BAD_PARAMETER;
    }
    let blocks = (n - 1) / r + 1;
    if idx.len() < blocks {
        return LIBBSC_BAD_PARAMETER;
    }
    for &i in &idx[..blocks] {
        if i == 0 || i > n {
            return LIBBSC_BAD_PARAMETER;
        }
    }

    match unbwt(&data[..n], n, r, &idx) {
        Ok(out) => {
            data[..n].copy_from_slice(&out);
            LIBBSC_NO_ERROR
        }
        Err(e) => e,
    }
}
