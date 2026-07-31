//! The Future-LZ / Index-LZ second pass, from `srep.cpp:724-830`.
//!
//! `compress()` finds matches *destination-first*: each record says "at this
//! point in the output, copy from back there". Future-LZ inverts that — a match
//! is stored with the block containing its **source**, saying "when you reach
//! this point, remember these bytes; they will be needed later". That is what
//! lets a decoder work in one forward pass without seeking backwards.
//!
//! Turning one into the other cannot be done per block, because a match's source
//! is usually in an *earlier* block than the one that found it. So SREP makes two
//! passes: collect every match from every block, sort by source, then walk the
//! blocks again handing each one the matches that start inside it.
//!
//! # The two record encodings are different, and this is where they meet
//!
//! | | ROUND_MATCHES | base | words |
//! |---|---|---|---|
//! | internal `stat` from `compress()` | as derived (true for `-m3`) | `BASE_LEN` (512) | 3 |
//! | records **written** for Future-LZ | **always false** (`:788`) | `FUTURELZ_BASE_LEN` (**0**) | 4 |
//!
//! This is the resolution of the "`-m3f` rounds but writes version 3" question:
//! rounding governs the buffer `compress()` fills, and the file's records are
//! re-encoded here without it. `srep.cpp:667` even accounts for the size change,
//! adding one `STAT` word per match because the data was collected at 12 bytes
//! each and will be written at 16.

use super::matches::{self, LzMatch, MatchTooShort};

/// One block as `compress()` left it: the literal count, and the matches it
/// found, still destination-anchored in the internal encoding.
#[derive(Clone, Debug)]
pub struct Block {
    /// Absolute offset of the block in the input file.
    pub start: u64,
    /// Uncompressed length of the block.
    pub size: usize,
    /// `literal_bytes` — size minus the sum of match lengths.
    pub literal_bytes: usize,
    /// The `statbuf` `compress()` produced for this block.
    pub stat: Vec<u32>,
}

impl Block {
    /// `block->end`.
    pub fn end(&self) -> u64 {
        self.start + self.size as u64
    }
}

/// Steps 1 and 2 (`:739-757`) — decode every block's matches into absolute
/// positions and sort them by **source**.
///
/// The C decodes with `FUTURE_LZ = false` here even in Future-LZ mode: the
/// records being read are the ones `compress()` wrote, which are
/// destination-anchored. Only the records written in [`redistribute`] are
/// source-anchored.
pub fn collect_and_sort(blocks: &[Block], round_matches: bool, base_len: u32) -> Vec<LzMatch> {
    let mut out = Vec::new();
    for block in blocks {
        let mut block_pos = block.start;
        let mut rest = block.stat.as_slice();
        while let Some((rec, used)) =
            matches::decode(rest, false, round_matches, base_len, block_pos)
        {
            rest = &rest[used..];
            block_pos += u64::from(rec.lit_len) + u64::from(rec.lz_match.len);
            out.push(rec.lz_match);
        }
    }
    // `std::sort` by `order_by_LZ_match_src` (:85). Stability is not specified
    // there, and ties are matches sharing a source position.
    out.sort_by_key(|m| m.src);
    out
}

/// Step 3 (`:768-800`) — hand each block the matches whose source lies in it.
///
/// Returns one written `stat` array per block, in block order.
///
/// A match longer than a block is emitted **once per block it spans**, truncated
/// to the part inside each: the `saved_i` rewind is what makes the next block
/// re-examine it rather than skipping past.
pub fn redistribute(
    blocks: &[Block],
    sorted: &[LzMatch],
    futurelz_base_len: u32,
) -> Result<Vec<Vec<u32>>, MatchTooShort> {
    let mut out = Vec::with_capacity(blocks.len());
    let mut i = 0usize;

    for block in blocks {
        let mut stat = Vec::new();
        let mut block_pos = block.start;
        // "First match that should be checked in the next block."
        let mut saved_i = i;

        // The C terminates on a barrier element whose src is origsize; a bounds
        // check is the same condition without the sentinel.
        while i < sorted.len() && sorted[i].src < block.end() {
            let m = sorted[i];
            // Entirely owned by previous blocks -- nothing of it starts here.
            if m.src + u64::from(m.len) <= block.start {
                saved_i = i;
                i += 1;
                continue;
            }
            // Truncate to the part of the match that lies inside this block.
            let src = m.src.max(block.start);
            let len_front = u64::from(m.len) - (src - m.src);
            let len = len_front.min(block.end() - src);

            matches::encode(
                &mut stat,
                // Always false: written Future-LZ records are 4-word.
                false,
                futurelz_base_len,
                (src - block_pos) as u32,
                // The ORIGINAL offset, not the truncated one -- the destination
                // is still that far from the untruncated source.
                m.dest - m.src,
                len as u32,
            )?;
            block_pos = src;
            i += 1;
        }

        // Rewind, so a match spanning into the next block is seen again there.
        i = saved_i;
        out.push(stat);
    }
    Ok(out)
}

/// The literal bytes of a block (`:826-832`) — everything not covered by a match.
///
/// Walks the block's own destination-anchored records, copying `lit_len` bytes
/// and then skipping `lit_len + len`, and finally copies whatever tail remains.
pub fn literals(block: &Block, data: &[u8], round_matches: bool, base_len: u32) -> Vec<u8> {
    let mut out = Vec::with_capacity(block.literal_bytes);
    let mut in_pos = 0usize;
    let mut rest = block.stat.as_slice();
    // basic_pos is 0 here in the C: only lit_len and len are used.
    while let Some((rec, used)) = matches::decode(rest, false, round_matches, base_len, 0) {
        rest = &rest[used..];
        let lit = rec.lit_len as usize;
        if in_pos + lit > data.len() {
            break;
        }
        out.extend_from_slice(&data[in_pos..in_pos + lit]);
        in_pos += lit + rec.lz_match.len as usize;
        if in_pos > data.len() {
            return out;
        }
    }
    out.extend_from_slice(&data[in_pos.min(data.len())..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a block whose stat holds one destination-anchored match.
    fn block_with(
        start: u64,
        size: usize,
        round: bool,
        base: u32,
        recs: &[(u32, u64, u32)], // (lit_len, offset, len)
    ) -> Block {
        let mut stat = Vec::new();
        let mut matched = 0usize;
        for &(lit, off, len) in recs {
            matches::encode(&mut stat, round, base, lit, off, len).expect("encodes");
            matched += len as usize;
        }
        Block { start, size, literal_bytes: size - matched, stat }
    }

    #[test]
    fn matches_are_collected_into_absolute_positions_and_sorted_by_source() {
        let base = 512u32;
        // Two blocks, each with one match pointing back 1024 bytes.
        let b0 = block_with(0, 4096, true, base, &[(1024, 1024, 1024)]);
        let b1 = block_with(4096, 4096, true, base, &[(512, 2048, 512)]);
        let got = collect_and_sort(&[b0, b1], true, base);
        assert_eq!(got.len(), 2);
        // Sorted by src, and each src is dest minus the offset.
        assert!(got[0].src <= got[1].src, "not sorted by source");
        for m in &got {
            assert!(m.dest > m.src, "a match must point backwards in the input");
        }
    }

    #[test]
    fn a_match_is_given_to_the_block_containing_its_source() {
        let base = 512u32;
        // One match: source at 100 (block 0), destination at 5000 (block 1).
        let sorted = [LzMatch { src: 100, dest: 5000, len: 512 }];
        let blocks = [
            Block { start: 0, size: 4096, literal_bytes: 4096, stat: Vec::new() },
            Block { start: 4096, size: 4096, literal_bytes: 4096, stat: Vec::new() },
        ];
        let out = redistribute(&blocks, &sorted, 0).expect("redistributes");
        assert_eq!(out[0].len(), 4, "block 0 owns the source, 4-word record");
        assert!(out[1].is_empty(), "block 1 owns the destination, not the source");
        // And the written record is source-anchored: decoding with future_lz
        // recovers src = block_start + lit_len = 100.
        let (rec, _) = matches::decode(&out[0], true, false, 0, 0).expect("decodes");
        assert_eq!(rec.lz_match.src, 100);
        assert_eq!(rec.lz_match.dest, 5000);
        assert_eq!(rec.lz_match.len, 512);
        let _ = base;
    }

    #[test]
    fn a_match_spanning_two_blocks_is_emitted_in_both_truncated() {
        // Source 4000..4600 straddles the 4096 boundary, so each block gets the
        // part inside it. This is what the saved_i rewind exists for.
        let sorted = [LzMatch { src: 4000, dest: 9000, len: 600 }];
        let blocks = [
            Block { start: 0, size: 4096, literal_bytes: 4096, stat: Vec::new() },
            Block { start: 4096, size: 4096, literal_bytes: 4096, stat: Vec::new() },
        ];
        let out = redistribute(&blocks, &sorted, 0).expect("ok");
        assert_eq!(out[0].len(), 4, "front fragment");
        assert_eq!(out[1].len(), 4, "back fragment");

        let (a, _) = matches::decode(&out[0], true, false, 0, 0).expect("a");
        assert_eq!(a.lz_match.src, 4000);
        assert_eq!(a.lz_match.len, 96, "truncated at the block end");

        let (b, _) = matches::decode(&out[1], true, false, 0, 4096).expect("b");
        assert_eq!(b.lz_match.src, 4096, "truncated at the block start");
        assert_eq!(b.lz_match.len, 504);
        assert_eq!(a.lz_match.len + b.lz_match.len, 600, "no bytes lost");
    }

    #[test]
    fn fragments_shorter_than_base_len_are_why_futurelz_base_is_zero() {
        // A 1-byte fragment. With FUTURELZ_BASE_LEN = 512 this would be
        // rejected outright, which is precisely why srep.cpp:386 sets it to 0
        // for Future-LZ.
        let sorted = [LzMatch { src: 4095, dest: 9000, len: 600 }];
        let blocks = [
            Block { start: 0, size: 4096, literal_bytes: 4096, stat: Vec::new() },
            Block { start: 4096, size: 4096, literal_bytes: 4096, stat: Vec::new() },
        ];
        let out = redistribute(&blocks, &sorted, 0).expect("base 0 accepts a 1-byte fragment");
        let (a, _) = matches::decode(&out[0], true, false, 0, 0).expect("a");
        assert_eq!(a.lz_match.len, 1);

        // The same input with a nonzero base must be refused, not silently wrong.
        assert!(redistribute(&blocks, &sorted, 512).is_err());
    }

    #[test]
    fn literals_are_everything_a_match_does_not_cover() {
        let base = 512u32;
        // 2048-byte block: 512 literals, a 512-byte match, then a 1024 tail.
        let data: Vec<u8> = (0..2048u32).map(|i| (i % 251) as u8).collect();
        let b = block_with(0, 2048, true, base, &[(512, 512, 512)]);
        let lit = literals(&b, &data, true, base);
        assert_eq!(lit.len(), 2048 - 512, "one match removed");
        assert_eq!(&lit[..512], &data[..512], "leading literals");
        assert_eq!(&lit[512..], &data[1024..], "tail after the match");
    }

    #[test]
    fn a_block_with_no_matches_is_all_literal() {
        let data = vec![9u8; 1000];
        let b = Block { start: 0, size: 1000, literal_bytes: 1000, stat: Vec::new() };
        assert_eq!(literals(&b, &data, true, 512), data);
    }
}
