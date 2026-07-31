//! LZ match records, ported from `Compression/SREP/srep.cpp`
//! (`LZ_MATCH` :77, `DECODE_LZ_MATCH` :117, `STATS_PER_MATCH` :105).
//!
//! SREP separates its output into two streams: literal bytes, and an array of
//! 32-bit "stat" words describing matches. They interleave strictly, as in
//! textbook LZ77 -- each record is a literal run length followed by one match.
//!
//! A record is three or four words:
//!
//! ```text
//!   [0] literal run length
//!   [1] offset, low  32 bits   (divided by L when rounding)
//!   [2] offset, high 32 bits   -- ABSENT when rounding
//!   [3] (length - L) / L1
//! ```
//!
//! Rounding (format v1 only) is what removes the high word: with every offset
//! and length a multiple of `L`, they are stored divided by it and a 32-bit
//! quotient reaches far enough. That is why `STATS_PER_MATCH` is 3 there and 4
//! everywhere else -- the record *layout* changes with the format version, so
//! misreading the version desynchronises the whole stat array rather than
//! producing one wrong match.
//!
//! The other axis is `FUTURE_LZ`. Ordinarily a record's position is the match
//! *destination* and the source is derived by subtracting the offset. Under
//! Future-LZ it is the other way round: the record sits with its source block
//! and points forward to the destination. Same words, opposite meaning.

/// `LZ_MATCH` (:77). Absolute file positions, not window-relative.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct LzMatch {
    pub src: u64,
    pub dest: u64,
    pub len: u32,
}

/// `STATS_PER_MATCH` (:105).
pub const fn stats_per_match(round_matches: bool) -> usize {
    if round_matches {
        3
    } else {
        4
    }
}

/// One decoded record: the literal run that precedes the match, and the match.
#[derive(Clone, Copy, Debug)]
pub struct Record {
    pub lit_len: u32,
    pub lz_match: LzMatch,
}

/// Why `ENCODE_LZ_MATCH` can reject: the C calls `error(ERROR_COMPRESSION, ...)`
/// and exits the process.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MatchTooShort {
    pub len: u32,
    pub base_len: u32,
}

/// `ENCODE_LZ_MATCH` (:108) — append one record to `stat`.
///
/// `l` is the macro's `L` parameter, which every call site binds to **`BASE_LEN`,
/// not the chunk size**. `record_match` rounds the length by the chunk `L` and
/// then encodes it divided by `BASE_LEN`; conflating the two silently rescales
/// every offset in the stream.
///
/// The C aborts the process when `len < L` ("match len too small"). Returned as
/// an error here instead, so a caller bug surfaces as a refusal rather than as a
/// truncated subtraction.
pub fn encode(
    stat: &mut Vec<u32>,
    round_matches: bool,
    l: u32,
    lit_len: u32,
    match_offset: u64,
    match_len: u32,
) -> Result<(), MatchTooShort> {
    if match_len < l {
        return Err(MatchTooShort { len: match_len, base_len: l });
    }
    // `L1` — the same divisor `decode` reapplies as a multiplier.
    let l1 = match round_matches {
        true => u64::from(l),
        false => 1,
    };
    let scaled = match_offset / l1;

    stat.push(lit_len);
    stat.push(scaled as u32);
    // The high word exists only in the 4-word form; under ROUND_MATCHES the
    // offset is pre-divided by L and assumed to fit in 32 bits.
    if !round_matches {
        stat.push((scaled >> 32) as u32);
    }
    stat.push(((u64::from(match_len) - u64::from(l)) / l1) as u32);
    Ok(())
}

/// `DECODE_LZ_MATCH` (:117).
///
/// `basic_pos` is the current position in the decompressed file. Under
/// Future-LZ it anchors the match's source; otherwise it anchors its
/// destination.
///
/// Returns `None` if `stat` is too short for one record.
pub fn decode(
    stat: &[u32],
    future_lz: bool,
    round_matches: bool,
    l: u32,
    basic_pos: u64,
) -> Option<(Record, usize)> {
    let n = stats_per_match(round_matches);
    if stat.len() < n {
        return None;
    }
    // `L1` is the divisor the encoder applied: L when rounding, 1 otherwise.
    let l1 = if round_matches { l as u64 } else { 1 };

    let lit_len = stat[0];
    let mut offset = stat[1] as u64;
    let len_word;
    if round_matches {
        len_word = stat[2];
    } else {
        offset += (stat[2] as u64) << 32;
        len_word = stat[3];
    }
    offset = offset.wrapping_mul(l1);

    let mut m = LzMatch { src: 0, dest: 0, len: (len_word as u64).wrapping_mul(l1) as u32 + l };

    if !future_lz {
        m.dest = basic_pos.wrapping_add(lit_len as u64);
        // The destination is rounded down to a multiple of L before the offset
        // is applied -- only meaningful when rounding, where L1 is L.
        m.src = (m.dest / l1).wrapping_mul(l1).wrapping_sub(offset);
    } else {
        m.src = basic_pos.wrapping_add(lit_len as u64);
        m.dest = m.src.wrapping_add(offset);
    }

    Some((Record { lit_len, lz_match: m }, n))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The record layout changes width with the format version, so a decoder
    /// that guesses wrong desynchronises every following match rather than
    /// producing one bad one.
    #[test]
    fn record_width_depends_on_rounding() {
        assert_eq!(stats_per_match(true), 3);
        assert_eq!(stats_per_match(false), 4);
    }

    #[test]
    fn backward_match_derives_source_from_destination() {
        // Not rounded: offset is a full 64-bit value across two words.
        let l = 32;
        let stat = [10u32, 100, 0, 5]; // lit 10, offset 100, len (5*1)+32 = 37
        let (r, used) = decode(&stat, false, false, l, 1000).unwrap();
        assert_eq!(used, 4);
        assert_eq!(r.lit_len, 10);
        assert_eq!(r.lz_match.dest, 1010); // basic_pos + lit_len
        assert_eq!(r.lz_match.src, 910); // dest - offset
        assert_eq!(r.lz_match.len, 37);
    }

    #[test]
    fn future_lz_inverts_source_and_destination() {
        let l = 32;
        let stat = [10u32, 100, 0, 5];
        let (r, _) = decode(&stat, true, false, l, 1000).unwrap();
        // Same words, opposite meaning: the record's position is the SOURCE.
        assert_eq!(r.lz_match.src, 1010);
        assert_eq!(r.lz_match.dest, 1110); // src + offset
        assert_eq!(r.lz_match.len, 37);
    }

    #[test]
    fn rounded_records_scale_by_l_and_drop_the_high_word() {
        let l = 512;
        // Three words only. offset 2 -> 1024, len 3 -> 3*512 + 512 = 2048.
        let stat = [0u32, 2, 3];
        let (r, used) = decode(&stat, false, true, l, 4096).unwrap();
        assert_eq!(used, 3);
        assert_eq!(r.lz_match.len, 2048);
        assert_eq!(r.lz_match.dest, 4096);
        assert_eq!(r.lz_match.src, 4096 - 1024);
    }

    #[test]
    fn a_destination_not_on_an_l_boundary_is_rounded_down_first() {
        let l = 512;
        let stat = [7u32, 1, 0]; // lit 7, so dest = 4096+7, offset 512
        let (r, _) = decode(&stat, false, true, l, 4096).unwrap();
        assert_eq!(r.lz_match.dest, 4103);
        // dest/L*L = 4096, then minus 512.
        assert_eq!(r.lz_match.src, 3584);
    }

    #[test]
    fn a_short_stat_array_yields_nothing() {
        assert!(decode(&[1, 2], false, false, 32, 0).is_none());
        assert!(decode(&[1, 2, 3], false, false, 32, 0).is_none());
        assert!(decode(&[1, 2], false, true, 32, 0).is_none());
    }
}

#[cfg(test)]
mod encode_tests {
    use super::*;

    // decode() is byte-verified against the C by srep-check.sh across all four
    // format versions, so it is a real oracle for its own inverse -- not a
    // restatement of encode().

    #[test]
    fn encode_then_decode_recovers_the_record_future_lz() {
        // -m3f: Future-LZ, so basic_pos anchors the match SOURCE.
        let base_len = 32u32;
        for &round in &[false, true] {
            let l1 = match round { true => u64::from(base_len), false => 1 };
            for &(lit, off_units, len_units) in &[
                (0u32, 1u64, 1u64),
                (7, 3, 5),
                (1234, 1000, 2),
                (0, 65_536, 40),
            ] {
                let offset = off_units * l1;
                let len = (len_units * l1) as u32 + base_len;

                let mut stat = Vec::new();
                encode(&mut stat, round, base_len, lit, offset, len).expect("encodable");
                assert_eq!(stat.len(), stats_per_match(round), "record width");

                let basic_pos = 1_000_000u64;
                let (rec, used) = decode(&stat, true, round, base_len, basic_pos).expect("decodes");
                assert_eq!(used, stats_per_match(round));
                assert_eq!(rec.lit_len, lit);
                assert_eq!(rec.lz_match.len, len, "round={round}");
                // Future-LZ: src = basic_pos + lit_len, dest = src + offset.
                assert_eq!(rec.lz_match.src, basic_pos + u64::from(lit));
                assert_eq!(rec.lz_match.dest - rec.lz_match.src, offset, "round={round}");
            }
        }
    }

    #[test]
    fn the_high_offset_word_only_exists_when_not_rounding() {
        // A >32-bit offset survives only via the 4-word form; that is exactly
        // what STATS_PER_MATCH is about.
        let base_len = 64u32;
        let offset = (1u64 << 34) + 4096;
        let mut stat = Vec::new();
        encode(&mut stat, false, base_len, 0, offset, base_len + 128).expect("encodable");
        assert_eq!(stat.len(), 4);
        let (rec, _) = decode(&stat, true, false, base_len, 0).expect("decodes");
        assert_eq!(rec.lz_match.dest - rec.lz_match.src, offset);
    }

    #[test]
    fn a_match_shorter_than_base_len_is_refused_not_wrapped() {
        // The C exits the process here. The subtraction would underflow, which
        // under overflow-checks panics and under wrapping would encode a
        // gigantic length -- both worse than a refusal.
        let e = encode(&mut Vec::new(), false, 32, 0, 0, 31).unwrap_err();
        assert_eq!(e, MatchTooShort { len: 31, base_len: 32 });
        // Exactly BASE_LEN is the smallest legal match.
        assert!(encode(&mut Vec::new(), false, 32, 0, 0, 32).is_ok());
    }

    #[test]
    fn records_append_so_a_block_is_a_flat_stat_array() {
        let mut stat = Vec::new();
        encode(&mut stat, true, 32, 1, 32, 64).expect("ok");
        encode(&mut stat, true, 32, 2, 64, 96).expect("ok");
        assert_eq!(stat.len(), 6);
        let (a, used) = decode(&stat, true, true, 32, 0).expect("first");
        assert_eq!(a.lit_len, 1);
        let (b, _) = decode(&stat[used..], true, true, 32, 0).expect("second");
        assert_eq!(b.lit_len, 2);
    }
}
