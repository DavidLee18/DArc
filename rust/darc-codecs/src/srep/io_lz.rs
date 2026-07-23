//! The I/O-LZ block decompressor, ported from
//! `Compression/SREP/decompress.cpp` (`decompress` :9).
//!
//! This is the v1/v2 strategy, and the one that gives SREP its whole reason to
//! exist. A match may point anywhere in the file, including into blocks decoded
//! long ago and already written out — so when its source lies before the
//! current block, the decoder **seeks back into the output file and re-reads
//! it**. That is how a dictionary larger than RAM works: the dictionary is the
//! output file itself.
//!
//! A block's compressed form is one buffer holding the stat array first and the
//! literal bytes after it, so the end of the stats is exactly the start of the
//! literals. Records and literals interleave strictly, as in textbook LZ77.
//!
//! Three things make a match invalid, all checked by the C and all reachable
//! from corrupt input: a literal run longer than the remaining literals, a
//! match that would overrun the output block, and `src >= dest` (a match must
//! point backwards).

use super::matches::{decode as decode_match, stats_per_match};

/// Somewhere already-written output can be read back from. For the real
/// decoder this is the output file; tests use a buffer.
pub trait Dictionary {
    /// Read exactly `buf.len()` bytes from absolute position `offset`.
    fn read_at(&mut self, offset: u64, buf: &mut [u8]) -> Result<(), Error>;
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// The C's single `return false` -- "broken compressed data".
    BadData,
    /// A read from the output file failed.
    Io,
}

/// `memcpy_lz_match` (srep.cpp:137): a forward byte-at-a-time copy, because the
/// regions may overlap and the overlap is *load-bearing* -- a match reaching
/// back fewer bytes than its length is how a repeated run is expressed, and it
/// only replicates correctly if each byte is copied before the next is read.
fn copy_overlapping(buf: &mut [u8], mut src: usize, mut dest: usize, len: usize) {
    for _ in 0..len {
        buf[dest] = buf[src];
        src += 1;
        dest += 1;
    }
}

/// `decompress`. `stat` is the block's match list, `lits` its literal bytes,
/// and `outbuf` the block's decompressed output, which must be exactly the
/// block's original size.
///
/// `block_start` is the block's absolute position in the decompressed file;
/// anything before it has already been written and is fetched through `dict`.
pub fn decompress_block(
    dict: &mut dyn Dictionary,
    round_matches: bool,
    l: u32,
    block_start: u64,
    stat: &[u32],
    lits: &[u8],
    outbuf: &mut [u8],
) -> Result<(), Error> {
    let per = stats_per_match(round_matches);
    let mut si = 0usize; // index into stat
    let mut ii = 0usize; // index into lits
    let mut oi = 0usize; // index into outbuf

    while stat.len() - si >= per {
        let basic_pos = block_start + oi as u64;
        let (rec, used) = decode_match(&stat[si..], false, round_matches, l, basic_pos)
            .ok_or(Error::BadData)?;
        si += used;
        let m = rec.lz_match;
        let lit_len = rec.lit_len as usize;

        // The C's three rejections, in its order.
        if lit_len > lits.len() - ii {
            return Err(Error::BadData);
        }
        if lit_len as u64 + m.len as u64 > (outbuf.len() - oi) as u64 {
            return Err(Error::BadData);
        }
        if m.src >= m.dest {
            return Err(Error::BadData);
        }

        // Literals first.
        outbuf[oi..oi + lit_len].copy_from_slice(&lits[ii..ii + lit_len]);
        ii += lit_len;
        oi += lit_len;

        let mut src = m.src;
        let mut len = m.len as usize;

        // Part of the match may live before this block, in output already
        // written to disk. Re-read it rather than keeping it in memory.
        if src < block_start {
            let bytes = core::cmp::min(len as u64, block_start - src) as usize;
            dict.read_at(src, &mut outbuf[oi..oi + bytes])?;
            oi += bytes;
            src += bytes as u64;
            len -= bytes;
        }

        // The rest is inside this block, and may overlap what we just wrote.
        if len > 0 {
            let from = (src - block_start) as usize;
            if from + len > outbuf.len() || oi + len > outbuf.len() {
                return Err(Error::BadData);
            }
            copy_overlapping(outbuf, from, oi, len);
            oi += len;
        }
    }

    // Whatever literals remain fill the block to its end; if the two do not
    // line up exactly, the block is corrupt.
    if lits.len() - ii != outbuf.len() - oi {
        return Err(Error::BadData);
    }
    let rest = lits.len() - ii;
    outbuf[oi..oi + rest].copy_from_slice(&lits[ii..ii + rest]);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A dictionary backed by the bytes decoded so far.
    struct MemDict(Vec<u8>);
    impl Dictionary for MemDict {
        fn read_at(&mut self, offset: u64, buf: &mut [u8]) -> Result<(), Error> {
            let at = offset as usize;
            if at + buf.len() > self.0.len() {
                return Err(Error::Io);
            }
            buf.copy_from_slice(&self.0[at..at + buf.len()]);
            Ok(())
        }
    }

    /// Build a non-rounded (v2) stat record.
    fn rec(lit_len: u32, offset: u64, len: u32, l: u32) -> Vec<u32> {
        vec![lit_len, offset as u32, (offset >> 32) as u32, len - l]
    }

    #[test]
    fn literals_only_fills_the_block() {
        let mut dict = MemDict(vec![]);
        let lits = b"hello world".to_vec();
        let mut out = vec![0u8; lits.len()];
        decompress_block(&mut dict, false, 32, 0, &[], &lits, &mut out).unwrap();
        assert_eq!(out, lits);
    }

    #[test]
    fn a_match_inside_the_block_copies_from_earlier_output() {
        let mut dict = MemDict(vec![]);
        let l = 4;
        // 6 literals "abcdef", then a match back 6 of length 6 -> "abcdef".
        let stat = rec(6, 6, 6, l);
        let lits = b"abcdef".to_vec();
        let mut out = vec![0u8; 12];
        decompress_block(&mut dict, false, l, 0, &stat, &lits, &mut out).unwrap();
        assert_eq!(&out[..], b"abcdefabcdef");
    }

    /// The overlapping copy is how a run is expressed: reaching back one byte
    /// with a length of many replicates it.
    #[test]
    fn an_overlapping_match_replicates() {
        let mut dict = MemDict(vec![]);
        let l = 4;
        let stat = rec(1, 1, 7, l); // literal "x", then match back 1, len 7
        let lits = b"x".to_vec();
        let mut out = vec![0u8; 8];
        decompress_block(&mut dict, false, l, 0, &stat, &lits, &mut out).unwrap();
        assert_eq!(&out[..], b"xxxxxxxx");
    }

    /// The property the whole design exists for: a match whose source is in an
    /// earlier block is fetched from output already written.
    #[test]
    fn a_match_before_the_block_is_read_back_from_the_dictionary() {
        let mut dict = MemDict(b"PREVIOUS-BLOCK-DATA".to_vec());
        let l = 4;
        let block_start = 19u64;
        // No literals, match back 19 bytes (to position 0) of length 8.
        let stat = rec(0, 19, 8, l);
        let mut out = vec![0u8; 8];
        decompress_block(&mut dict, false, l, block_start, &stat, &[], &mut out).unwrap();
        assert_eq!(&out[..], b"PREVIOUS");
    }

    /// And a match that straddles the boundary takes its first part from the
    /// file and its second from the current block.
    #[test]
    fn a_match_straddling_the_block_start_uses_both_sources() {
        let mut dict = MemDict(b"ABCD".to_vec());
        let l = 2;
        let block_start = 4u64;
        // literal "E" at pos 4, then a match at dest 5 reaching back 5 (to 0)
        // with length 6: "ABCDE" from before/at the boundary plus one more.
        let stat = rec(1, 5, 6, l);
        let lits = b"E".to_vec();
        let mut out = vec![0u8; 7];
        decompress_block(&mut dict, false, l, block_start, &stat, &lits, &mut out).unwrap();
        assert_eq!(&out[..], b"EABCDEA");
    }

    #[test]
    fn corrupt_records_are_rejected() {
        let l = 4;
        let mut dict = MemDict(vec![]);

        // Literal run longer than the literals available.
        let mut out = vec![0u8; 32];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &rec(99, 4, 4, l), b"ab", &mut out),
            Err(Error::BadData)
        );

        // Match longer than the block can hold.
        let mut out = vec![0u8; 8];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &rec(0, 4, 9999, l), b"", &mut out),
            Err(Error::BadData)
        );

        // src >= dest: a match must point backwards.
        let mut out = vec![0u8; 32];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &rec(0, 0, 8, l), b"", &mut out),
            Err(Error::BadData)
        );

        // Trailing literals that do not fill the block exactly.
        let mut out = vec![0u8; 32];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &[], b"short", &mut out),
            Err(Error::BadData)
        );
    }
}
