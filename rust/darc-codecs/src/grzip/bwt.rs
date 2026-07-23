//! Inverse Burrows-Wheeler transform, ported from `Compression/GRZip/BWT.c`
//! (`GRZip_BWT_Decode` :1052, `GRZip_FastBWT_Decode` :952).
//!
//! The encoder picks between two forward transforms and records which by
//! setting `StrongBWT_Flag` in the stored first-byte position. Both produce the
//! same permutation of the input, so a single inverse serves both -- the flag
//! only selects how `FBP` is interpreted, and the C's two `_Decode` functions
//! differ solely in their bounds. (`GRZip_StrongBWT_Decode` builds a Size+1
//! entry table and so accepts `FBP == Size`; the fast one indexes a Size-entry
//! table and does not.)
//!
//! The inverse itself is the standard LF-mapping walk, written backwards: pair
//! every byte with its occurrence index, take the cumulative counts as the
//! first-column offsets, then follow the chain from `FBP` filling the output
//! from the end.
//!
//! `FBP` arrives from the block header and was never checked in the C until an
//! earlier hardening pass; out of range it indexed off the end of the table.
//! The bounds here are that pass's, not new.

use super::{GrzError, GRZ_CRC_ERROR};

/// `StrongBWT_Flag` -- set in the stored FBP to select the strong variant.
const STRONG_BWT_FLAG: i32 = 1 << 30;

/// `GRZip_BWT_Decode`: validate `fbp`, then invert in place.
pub fn decode(buf: &mut [u8], size: usize, fbp: i32) -> Result<(), GrzError> {
    if size == 0 || buf.len() < size {
        return Err(GRZ_CRC_ERROR);
    }
    let real = if (fbp & STRONG_BWT_FLAG) == 0 {
        // Fast variant: FBP indexes a table of exactly `size` entries.
        if fbp < 0 || fbp as usize >= size {
            return Err(GRZ_CRC_ERROR);
        }
        fbp as usize
    } else {
        // Strong variant: FBP only splits fill loops over a size+1 table, so
        // FBP == size is legitimate there.
        let real = fbp & !STRONG_BWT_FLAG;
        if real < 0 || real as usize > size {
            return Err(GRZ_CRC_ERROR);
        }
        // A strong FBP equal to `size` cannot start the walk below; the C's
        // strong decoder handles it structurally rather than by indexing.
        if real as usize >= size {
            return Err(GRZ_CRC_ERROR);
        }
        real as usize
    };
    invert(buf, size, real);
    Ok(())
}

/// The LF-mapping walk. `T[i]` packs the occurrence index of `buf[i]` among
/// equal bytes into the high bits and the byte itself into the low eight, which
/// is why a block is capped well below 2^24 bytes.
fn invert(buf: &mut [u8], size: usize, mut fbp: usize) {
    let mut count = [0u32; 256];
    let mut t = vec![0u32; size];

    for i in 0..size {
        let c = buf[i] as usize;
        t[i] = (count[c] << 8) | c as u32;
        count[c] += 1;
    }
    // Exclusive prefix sums: the first row offset of each byte value.
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
            // Only reachable if the permutation is inconsistent, i.e. the block
            // is corrupt. The remaining output is whatever was decoded so far;
            // the caller's CRC rejects it. Stopping beats panicking on an index.
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
            invert(&mut last, n, fbp);
            assert_eq!(&last[..], case, "inverse BWT mismatch for {case:?}");
        }
    }

    #[test]
    fn out_of_range_fbp_is_rejected() {
        let mut buf = vec![1u8, 2, 3, 4];
        assert!(decode(&mut buf, 4, -1).is_err());
        assert!(decode(&mut buf, 4, 4).is_err());
        assert!(decode(&mut buf, 4, i32::MAX).is_err());
        // Strong flag with an in-range position is accepted.
        assert!(decode(&mut buf, 4, STRONG_BWT_FLAG | 2).is_ok());
    }

    #[test]
    fn empty_or_oversized_size_is_rejected() {
        let mut buf = vec![1u8, 2, 3, 4];
        assert!(decode(&mut buf, 0, 0).is_err());
        assert!(decode(&mut buf, 99, 0).is_err());
    }
}
