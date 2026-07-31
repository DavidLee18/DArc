//! LZP: match-flag preprocessor, ported from Compression/LZP/C_LZP.cpp.
//!
//! LZP predicts the next bytes from a hash of recent context. When the
//! prediction is right it emits a flag byte and a length instead of the bytes
//! themselves. Its output feeds a later codec in the chain.
//!
//! Two things about the original shape this port has to preserve exactly:
//!
//!   * `lzpC(p)` reads the four bytes *before* p (`*(UINT*)(p-4)`), and the
//!     hash reads four bytes before that again. Every index below is offset
//!     accordingly rather than "fixed" to read forwards.
//!   * The encoder writes **two streams into one buffer**: literals forward
//!     from the start, match lengths backward from the end, meeting in the
//!     middle. Compression fails (returns 0) exactly when they meet.
//!
//! The hash table holds positions into the input (encoding) or the output
//! (decoding); the C version stores raw pointers, which is the same thing.

use crate::ffi::{Io, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

const LZP_MATCH_FLAG: u8 = 0xB5;

/// Rotate right, matching the portable `ROR` macro in the original.
#[inline]
fn ror(x: u32, y: i32) -> u32 {
    x.rotate_right((y & 31) as u32)
}

/// `lzpC(p)` -- the 32-bit word ending at `at`, i.e. bytes [at-4, at).
#[inline]
fn lzp_c(b: &[u8], at: usize) -> u32 {
    debug_assert!(at >= 4 && at <= b.len(), "lzpC out of range");
    if at < 4 || at > b.len() {
        return 0;
    }
    u32::from_le_bytes([b[at - 4], b[at - 3], b[at - 2], b[at - 1]])
}

/// `lzpH(c, p, HashMask)` -- context hash. Note the `lzpC(p-1)` term reads the
/// word ending one byte earlier.
#[inline]
fn lzp_h(c: u32, b: &[u8], at: usize, mask: u32) -> u32 {
    (c.wrapping_add(5u32.wrapping_mul(ror(c, 17)))
        .wrapping_add(3u32.wrapping_mul(lzp_c(b, at.wrapping_sub(1))))) & mask
}

/// Port of `LZPEncode`. Returns the packed length, or 0 when the data did not
/// compress -- the caller then stores the block instead.
pub fn encode(input: &[u8], out: &mut [u8], min_len: i32, hash_size: usize, barrier: i32, smallest_len: i32) -> usize {
    let size = input.len();
    if size < 32 {
        return 0;
    }
    let mask = (hash_size - 1) as u32;
    // The C version seeds every slot with `In+5`.
    let mut htable = vec![5usize; hash_size];

    // Header: the first 12 bytes pass through unchanged.
    out[..12].copy_from_slice(&input[..12]);
    let mut inp = 12usize;
    let mut outp = 12usize;
    let mut out_end = size; // grows downward
    let mut n1: u32 = 1;
    let mut n: u32 = 1;

    let mut i = lzp_c(input, inp);
    let mut k = lzp_h(i, input, inp, mask) as usize;

    loop {
        let p = htable[k];
        n -= 1;
        if n == 0 {
            htable[k] = inp;
            n = n1;
        }

        // Two distinct literal paths, and the difference is load-bearing.
        //
        // When the context hash does not match (`hash_miss`), the byte is
        // emitted raw even if it happens to equal LZP_MATCH_FLAG: the decoder
        // reaches the same state, sees i != lzpC(p), and short-circuits before
        // touching the backward stream, so no escape is needed.
        //
        // When the hash matched but no usable match was found, an emitted
        // 0xB5 is ambiguous and must be escaped with a 255 in the backward
        // stream. Merging the two paths and escaping in both writes one
        // spurious backward byte -- which is exactly what this port did, and
        // it showed up only as a one-byte difference on one input.
        let mut hash_miss = false;
        let mut literal = false;
        if i != lzp_c(input, p) {
            hash_miss = true;
        } else {
            let ml = if (inp - p) as i32 > barrier { smallest_len } else { min_len } as usize;
            if inp + ml <= size && lzp_c(input, p + ml) == lzp_c(input, inp + ml) {
                // Extend the match four bytes at a time, then one at a time.
                let mut m = 4usize;
                while inp + m <= size && lzp_c(input, p + m) == lzp_c(input, inp + m) {
                    m += 4;
                }
                m -= 4;
                while inp + m < size && input[inp + m] == input[p + m] {
                    m += 1;
                }
                if m < ml {
                    literal = true;
                } else {
                    htable[k] = inp;
                    if (inp - p) as u32 > (n1 + 1) * hash_size as u32 && n1 < 7 {
                        n1 += 1;
                    }
                    if outp >= out_end {
                        return 0;
                    }
                    out[outp] = LZP_MATCH_FLAG;
                    outp += 1;
                    k = m;
                    inp += m;
                    // Length goes into the backward stream, 254 at a time.
                    let mut rem = m - ml;
                    while rem >= 254 && outp < out_end {
                        out_end -= 1;
                        out[out_end] = 0;
                        rem -= 254;
                    }
                    if out_end == 0 {
                        return 0;
                    }
                    out_end -= 1;
                    out[out_end] = (rem + 1) as u8;
                    // Re-index the positions skipped over by the match.
                    loop {
                        let step = 2 * n1 as usize + 1;
                        if k < step {
                            break;
                        }
                        k -= step;
                        if k == 0 {
                            break;
                        }
                        let at = inp - k;
                        let h = lzp_h(lzp_c(input, at), input, at, mask) as usize;
                        htable[h] = at;
                    }
                }
            } else {
                literal = true;
            }
        }

        if hash_miss || literal {
            if outp >= out_end {
                return 0;
            }
            let c = input[inp];
            out[outp] = c;
            outp += 1;
            inp += 1;
            // Escape only on the MATCH_NOT_FOUND path; see above.
            if literal && c == LZP_MATCH_FLAG {
                if out_end == 0 {
                    return 0;
                }
                out_end -= 1;
                out[out_end] = 255;
            }
        }

        if inp >= size || outp >= out_end {
            break;
        }
        i = lzp_c(input, inp);
        k = lzp_h(i, input, inp, mask) as usize;
    }

    if outp >= out_end {
        return 0;
    }
    // Close the gap between the two streams.
    out.copy_within(out_end..size, outp);
    size - (out_end - outp)
}

/// The largest `HashSizeLog` any archive `arc` can write may carry.
///
/// The C bounds it in `LZP_METHOD::SetBlockSize` (`C_LZP.cpp:71`) as
/// `min(HashSizeLog, 1 + lb(BlockSize - 1))`, and `BlockSize` is a `MemSize`
/// (32-bit), so 32 is the ceiling that clamp can ever produce. Anything above it
/// came from something other than a stock `arc`.
const MAX_HASH_SIZE_LOG: c_int = 32;

/// `1 << HashSizeLog`, or an error if the archive's `HashSizeLog` is out of range.
///
/// **This is a security boundary, not tidiness.** `HashSizeLog` reaches here from
/// the method string in the archive's directory block: `parse_LZP` reads it with a
/// bare `parseInt` and no range check (`C_LZP.cpp:120`), and the only clamp is
/// `SetBlockSize`, which is on the *create* path — nothing calls it while
/// extracting. So the value is attacker-controlled, and `1usize << 99` is a panic
/// under this workspace's `overflow-checks = true` (`rust/Cargo.toml:74`), unwinding
/// out of an `extern "C"` frame into `unarc` and the SFX modules, which are compiled
/// `-D_NO_EXCEPTIONS`. That is undefined behaviour reachable from an archive.
///
/// A corrupt archive must produce a diagnosis, not a fault — the same reasoning
/// that made GRZip's rec mode an `Option` rather than an `unreachable!()`.
fn hash_size_from_log(hash_size_log: c_int) -> Result<usize, c_int> {
    if hash_size_log < 1 || hash_size_log > MAX_HASH_SIZE_LOG {
        return Err(crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    }
    Ok(1usize << hash_size_log)
}

/// Port of `LZPDecode`.
pub fn decode(input: &[u8], out: &mut Vec<u8>, min_len: i32, hash_size: usize, barrier: i32, smallest_len: i32) -> Result<usize, c_int> {
    let size = input.len();
    if size < 12 {
        return Err(crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    }
    let mask = (hash_size - 1) as u32;
    // `try_reserve`, not `vec![]`: at the top of the permitted range this is 32 GiB,
    // and an infallible allocation would abort the process rather than report that
    // the archive asked for too much. Same shape as `grzip::stream::run`.
    let mut htable: Vec<usize> = Vec::new();
    match htable.try_reserve_exact(hash_size) {
        Ok(()) => {}
        Err(_) => return Err(crate::ffi::FREEARC_ERRCODE_NOT_ENOUGH_MEMORY),
    }
    htable.resize(hash_size, 5usize);

    out.clear();
    out.extend_from_slice(&input[..12]);
    let mut inp = 12usize;
    let mut in_end = size; // consumed downward
    let mut n1: u32 = 1;
    let mut n: u32 = 1;

    let bad = crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    let mut i = lzp_c(out, out.len());
    let mut k = lzp_h(i, out, out.len(), mask) as usize;

    while inp < in_end {
        let p = htable[k];
        n -= 1;
        if n == 0 {
            htable[k] = out.len();
            n = n1;
        }
        let c = *input.get(inp).ok_or(bad)?;
        inp += 1;

        let is_match = c == LZP_MATCH_FLAG && i == lzp_c(out, p) && {
            // The C code consumes a byte from the backward stream as part of
            // this test; a 255 there means "this was a literal 0xB5".
            in_end = in_end.checked_sub(1).ok_or(bad)?;
            *input.get(in_end).ok_or(bad)? != 255
        };

        if !is_match {
            out.push(c);
        } else {
            htable[k] = out.len();
            if (out.len() - p) as u32 > (n1 + 1) * hash_size as u32 && n1 < 7 {
                n1 += 1;
            }
            let mut len = (if (out.len() - p) as i32 > barrier { smallest_len } else { min_len }) as usize - 1;
            while *input.get(in_end).ok_or(bad)? == 0 {
                len += 254;
                in_end = in_end.checked_sub(1).ok_or(bad)?;
            }
            len += *input.get(in_end).ok_or(bad)? as usize;
            let mut kk = 2 * n1 as usize + 2;
            let mut src = p;
            for _ in 0..len {
                kk -= 1;
                if kk == 0 {
                    kk = 2 * n1 as usize + 1;
                    let at = out.len();
                    let h = lzp_h(lzp_c(out, at), out, at, mask) as usize;
                    htable[h] = at;
                }
                let b = *out.get(src).ok_or(bad)?;
                out.push(b);
                src += 1;
            }
        }
        let at = out.len();
        i = lzp_c(out, at);
        k = lzp_h(i, out, at, mask) as usize;
    }
    Ok(out.len())
}

/// Port of `lzp_decompress`: block framing around `decode`.
#[allow(clippy::too_many_arguments)]
pub fn decompress(io: &Io, block_size: u32, min_len: c_int, hash_size_log: c_int, barrier: c_int, smallest_len: c_int) -> c_int {
    let hash_size = match hash_size_from_log(hash_size_log) {
        Ok(n) => n,
        Err(e) => return e,
    };
    let mut out: Vec<u8> = Vec::new();
    loop {
        let mut hdr = [0u8; 4];
        match io.read(&mut hdr) {
            0 => return OK,
            4 => {}
            n if n < 0 => return n,
            _ => return FREEARC_ERRCODE_IO,
        }
        let in_size = i32::from_le_bytes(hdr);
        if in_size < 0 {
            let n = (-(in_size as i64)) as usize;
            let mut raw = match crate::ffi::archive_sized_buffer(n, block_size) {
                Ok(b) => b,
                Err(e) => return e,
            };
            if io.read(&mut raw) as usize != n {
                return FREEARC_ERRCODE_IO;
            }
            // Propagate, do not substitute: a negative write is not
            // necessarily an error. See the note on Io::write.
            let w = io.write(&raw);
            if w < 0 {
                return w;
            }
            continue;
        }
        let n = in_size as usize;
        let mut packed = match crate::ffi::archive_sized_buffer(n, block_size) {
            Ok(b) => b,
            Err(e) => return e,
        };
        if n != 0 && io.read(&mut packed) as usize != n {
            return FREEARC_ERRCODE_IO;
        }
        match decode(&packed, &mut out, min_len, hash_size, barrier, smallest_len) {
            Ok(_) => {
                if !out.is_empty() {
                    let w = io.write(&out);
                    if w < 0 {
                        return w;
                    }
                }
            }
            Err(e) => return e,
        }
    }
}

/// Port of `lzp_compress`: block framing around `encode`.
///
/// A block that does not compress well enough is stored instead, flagged by a
/// negative length. The threshold test is `OutSize/MinCompression >= InSize/100`
/// in integer arithmetic, and it is reproduced exactly rather than rewritten as
/// a percentage -- the truncation is part of which blocks get stored.
#[allow(clippy::too_many_arguments)]
pub fn compress(io: &Io, block_size: u32, min_compression: c_int, min_len: c_int,
                hash_size_log: c_int, barrier: c_int, smallest_len: c_int) -> c_int {
    let block_size = block_size.max(1) as usize;
    // Bounded on the encode side too. The value is local rather than
    // attacker-supplied here, but the shift panics just the same, and a codec that
    // refuses in one direction and faults in the other is harder to reason about
    // than one that refuses in both.
    let hash_size = match hash_size_from_log(hash_size_log) {
        Ok(n) => n,
        Err(e) => return e,
    };
    let mut inbuf = vec![0u8; block_size];
    loop {
        let got = io.read(&mut inbuf);
        if got < 0 {
            return got;
        }
        if got == 0 {
            return OK;
        }
        let in_size = got as usize;
        let mut out = vec![0u8; in_size + 2];
        let out_size = encode(&inbuf[..in_size], &mut out, min_len, hash_size, barrier, smallest_len);

        let store = out_size == 0
            || (min_compression > 0 && out_size / min_compression as usize >= in_size / 100);
        if store {
            if io.write(&(-(in_size as i32)).to_le_bytes()) < 0 || io.write(&inbuf[..in_size]) < 0 {
                return FREEARC_ERRCODE_IO;
            }
        } else if io.write(&(out_size as u32).to_le_bytes()) < 0 || io.write(&out[..out_size]) < 0 {
            return FREEARC_ERRCODE_IO;
        }
    }
}

#[cfg(test)]
mod hash_size_tests {
    use super::*;

    /// The archive-supplied `HashSizeLog` must be refused, not shifted.
    ///
    /// `1usize << 99` panics under this workspace's `overflow-checks = true`, and
    /// that panic would unwind out of `lzp_decompress`'s `extern "C"` frame into
    /// `unarc`/SFX code compiled `-D_NO_EXCEPTIONS`. The value arrives from the
    /// method string stored in the archive's directory block, which `parse_LZP`
    /// reads with no range check (`C_LZP.cpp:120`); the only clamp is on the create
    /// path (`C_LZP.cpp:71`), so nothing bounds it while extracting.
    #[test]
    fn out_of_range_hash_size_log_is_refused_not_shifted() {
        for bad in [-1, 0, MAX_HASH_SIZE_LOG + 1, 64, 99, i32::MAX] {
            assert_eq!(
                hash_size_from_log(bad),
                Err(crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA),
                "HashSizeLog {bad} must be refused"
            );
        }
    }

    /// Everything a stock `arc` can write still works: `SetBlockSize` clamps to
    /// `1 + lb(BlockSize - 1)` and `BlockSize` is 32-bit, so 32 is its ceiling.
    #[test]
    fn every_hash_size_log_a_stock_arc_can_write_is_accepted() {
        for good in 1..=MAX_HASH_SIZE_LOG {
            assert_eq!(hash_size_from_log(good), Ok(1usize << good), "log {good}");
        }
        // The default (C_LZP.cpp:37) and the largest a 1 GiB block permits.
        assert_eq!(hash_size_from_log(18), Ok(1 << 18));
        assert_eq!(hash_size_from_log(30), Ok(1 << 30));
    }
}
