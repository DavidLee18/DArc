//! The Future-LZ block decompressor, ported from
//! `Compression/SREP/decompress.cpp` (`decompress_FUTURE_LZ` :299).
//!
//! This is the v3 and v4 strategy. Where I/O-LZ stores a match with its
//! destination and points backwards, Future-LZ stores it with its **source**
//! and points forwards: the record lives in the block containing the bytes to
//! be copied, and names a destination that may be many blocks away.
//!
//! That inverts the decoder's shape. A block is processed in three phases:
//!
//! 1. Read this block's records. Those whose destination also falls inside this
//!    block go straight onto the pending heap.
//! 2. Drain the heap of every match destined for this block, in destination
//!    order, interleaving the literal runs between them.
//! 3. Read the records again; those destined for *later* blocks go on the heap
//!    to wait.
//!
//! The heap is ordered by destination and outlives the block, which is the
//! whole mechanism: a match can sit in it across many blocks.
//!
//! ## What this deliberately does not port
//!
//! The C carries a chunked `MEMORY_MANAGER` plus a `VIRTUAL_MEMORY_MANAGER`
//! that spills to disk, so that a pending match holds a *copy* of its source
//! bytes. That is a performance structure, not a format one. Its own
//! `restore_match_data` shows why:
//!
//! ```text
//!   if (index != INVALID_INDEX)  mm.restore (index, ptr, len);
//!   else                         memcpy_lz_match (ptr, buf+(src-buf_start), len);
//! ```
//!
//! The saved copy is taken from a block that is written to the output file
//! immediately afterwards, so re-reading the file yields the same bytes. This
//! port therefore keeps no match data at all: a pending match is three numbers,
//! and its bytes come from the output file when the source is in an earlier
//! block, or from the current block's buffer when it is not. `maximum_save`,
//! which in the C chooses between those two sources, becomes irrelevant --
//! both paths produce identical output, which is why it is not a parameter
//! here.
//!
//! The `is_marking_point()` pseudo-matches (`len == 0`) exist only to mark
//! where the disk spill must restore, so with no spill they are never created.

use super::io_lz::{Dictionary, Error};
use super::matches::{decode as decode_match, stats_per_match};
use std::collections::BinaryHeap;

/// A match waiting for the block that contains its destination.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pending {
    pub dest: u64,
    pub src: u64,
    pub len: u32,
}

/// Ordered by destination, so the heap yields matches in the order the output
/// needs them. `BinaryHeap` is a max-heap, so the comparison is reversed here
/// rather than wrapping every element in `Reverse`.
impl Ord for Pending {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        other
            .dest
            .cmp(&self.dest)
            .then(other.src.cmp(&self.src))
            .then(other.len.cmp(&self.len))
    }
}
impl PartialOrd for Pending {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// The heap of matches not yet applied. It must persist across blocks.
pub type PendingHeap = BinaryHeap<Pending>;

fn copy_overlapping(buf: &mut [u8], mut src: usize, mut dest: usize, len: usize) {
    for _ in 0..len {
        buf[dest] = buf[src];
        src += 1;
        dest += 1;
    }
}

/// Walk a block's records. `basic_pos` chains: each record's position is the
/// *previous* record's source, not a running output offset.
fn for_each_record(
    stat: &[u32],
    round: bool,
    l: u32,
    block_start: u64,
    mut f: impl FnMut(u64, u64, u32) -> Result<(), Error>,
) -> Result<(), Error> {
    let per = stats_per_match(round);
    let mut si = 0usize;
    let mut block_pos = block_start;
    while stat.len() - si >= per {
        let (rec, used) =
            decode_match(&stat[si..], true, round, l, block_pos).ok_or(Error::BadData)?;
        si += used;
        let m = rec.lz_match;
        f(m.src, m.dest, m.len)?;
        block_pos = m.src;
    }
    Ok(())
}

/// `decompress_FUTURE_LZ`.
pub fn decompress_block(
    dict: &mut dyn Dictionary,
    round: bool,
    l: u32,
    block_start: u64,
    stat: &[u32],
    lits: &[u8],
    outbuf: &mut [u8],
    pending: &mut PendingHeap,
) -> Result<(), Error> {
    let block_end = block_start + outbuf.len() as u64;

    // Phase 1: records whose destination is inside this block.
    //
    // The validation is the C's, and it encodes the format's invariant: a
    // record lives in the block holding its source, so the source must fall
    // within this block and the match must fit inside it.
    let mut prev = block_start;
    for_each_record(stat, round, l, block_start, |src, dest, len| {
        if src < prev || src >= block_end || (len as u64) > block_end - src || dest <= src {
            return Err(Error::BadData);
        }
        if dest < block_end {
            pending.push(Pending { dest, src, len });
        }
        prev = src;
        Ok(())
    })?;

    // Phase 2: apply every pending match destined for this block, in
    // destination order, with the literal runs between them.
    let mut ii = 0usize;
    let mut oi = 0usize;
    while let Some(top) = pending.peek().copied() {
        if top.dest >= block_end {
            break;
        }
        pending.pop();

        // Literals run from where we are to the match's destination. A
        // destination behind the write position means the stream is corrupt.
        let dest_off = top.dest.checked_sub(block_start).ok_or(Error::BadData)?;
        if (dest_off as usize) < oi {
            return Err(Error::BadData);
        }
        let lit_len = dest_off as usize - oi;
        if ii + lit_len > lits.len() || oi + lit_len + top.len as usize > outbuf.len() {
            return Err(Error::BadData);
        }
        outbuf[oi..oi + lit_len].copy_from_slice(&lits[ii..ii + lit_len]);
        ii += lit_len;
        oi += lit_len;

        let len = top.len as usize;
        if top.src < block_start {
            // Source lies in a block already written out; the output file is
            // the dictionary.
            dict.read_at(top.src, &mut outbuf[oi..oi + len])?;
        } else {
            // Source is in this block. It precedes the destination, so it is
            // already decoded, and the copy may overlap.
            let from = (top.src - block_start) as usize;
            if from + len > outbuf.len() {
                return Err(Error::BadData);
            }
            copy_overlapping(outbuf, from, oi, len);
        }
        oi += len;
    }

    // Trailing literals must fill the block exactly.
    if lits.len() - ii != outbuf.len() - oi {
        return Err(Error::BadData);
    }
    let rest = lits.len() - ii;
    outbuf[oi..oi + rest].copy_from_slice(&lits[ii..ii + rest]);

    // Phase 3: records destined for later blocks wait on the heap. No data is
    // copied -- see the module note on why the C's memory manager is not needed.
    for_each_record(stat, round, l, block_start, |src, dest, len| {
        if dest >= block_end {
            pending.push(Pending { dest, src, len });
        }
        Ok(())
    })?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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

    /// Destination order is what the heap exists to impose, and it must hold
    /// across blocks, not just within one.
    #[test]
    fn the_heap_yields_matches_in_destination_order() {
        let mut h = PendingHeap::new();
        for d in [500u64, 100, 900, 300] {
            h.push(Pending { dest: d, src: 0, len: 4 });
        }
        let mut seen = vec![];
        while let Some(p) = h.pop() {
            seen.push(p.dest);
        }
        assert_eq!(seen, vec![100, 300, 500, 900]);
    }

    /// A record's position chains from the previous record's source, so a
    /// second record is relative to the first -- not to the output cursor.
    #[test]
    fn record_positions_chain_through_sources() {
        let l = 4;
        // Two records, non-rounded: (lit 10, offset 100, len 4) twice.
        let stat = vec![10u32, 100, 0, 0, 10, 100, 0, 0];
        let mut got = vec![];
        for_each_record(&stat, false, l, 1000, |src, dest, len| {
            got.push((src, dest, len));
            Ok(())
        })
        .unwrap();
        // First: src = 1000+10 = 1010, dest = 1010+100 = 1110.
        // Second: chains from src 1010, so src = 1010+10 = 1020, dest = 1120.
        assert_eq!(got, vec![(1010, 1110, 4), (1020, 1120, 4)]);
    }

    #[test]
    fn a_match_destined_for_this_block_is_applied_here() {
        let mut dict = MemDict(vec![]);
        let mut pending = PendingHeap::new();
        let l = 4;
        // src = 0+0 = 0, dest = 0+6 = 6, len 6: copy "abcdef" to offset 6.
        let stat = vec![0u32, 6, 0, 2]; // len = 2 + 4 = 6
        let lits = b"abcdef".to_vec();
        let mut out = vec![0u8; 12];
        decompress_block(&mut dict, false, l, 0, &stat, &lits, &mut out, &mut pending).unwrap();
        assert_eq!(&out[..], b"abcdefabcdef");
        assert!(pending.is_empty());
    }

    /// The defining behaviour: a record in block 0 whose destination is in
    /// block 1 waits on the heap and is applied when that block arrives.
    #[test]
    fn a_match_destined_for_a_later_block_waits_on_the_heap() {
        let mut pending = PendingHeap::new();
        let l = 4;

        // Block 0: 8 bytes of literals, one record pointing forward to dest 8.
        let stat = vec![0u32, 8, 0, 0]; // src 0, dest 8, len 4
        let lits0 = b"ABCDEFGH".to_vec();
        let mut out0 = vec![0u8; 8];
        {
            let mut dict = MemDict(vec![]);
            decompress_block(&mut dict, false, l, 0, &stat, &lits0, &mut out0, &mut pending)
                .unwrap();
        }
        assert_eq!(&out0[..], b"ABCDEFGH");
        assert_eq!(pending.len(), 1, "match must be waiting for block 1");

        // Block 1: no records of its own. The pending match copies 4 bytes from
        // absolute 0 -- which lives in block 0, already written to the file.
        let mut dict = MemDict(out0.clone());
        let mut out1 = vec![0u8; 6];
        decompress_block(&mut dict, false, l, 8, &[], b"xx", &mut out1, &mut pending).unwrap();
        assert_eq!(&out1[..], b"ABCDxx");
        assert!(pending.is_empty());
    }

    #[test]
    fn corrupt_records_are_rejected() {
        let l = 4;
        let mut dict = MemDict(vec![]);
        let mut pending = PendingHeap::new();
        let mut out = vec![0u8; 16];

        // dest <= src: a Future-LZ match must point forwards.
        let stat = vec![0u32, 0, 0, 0];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &stat, b"", &mut out, &mut pending),
            Err(Error::BadData)
        );

        // Source outside the block: the record must live with its source.
        let mut pending = PendingHeap::new();
        let stat = vec![999u32, 8, 0, 0];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &stat, b"", &mut out, &mut pending),
            Err(Error::BadData)
        );

        // Match longer than the source block can hold.
        let mut pending = PendingHeap::new();
        let stat = vec![0u32, 8, 0, 9999];
        assert_eq!(
            decompress_block(&mut dict, false, l, 0, &stat, b"", &mut out, &mut pending),
            Err(Error::BadData)
        );
    }
}
