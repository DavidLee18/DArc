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
