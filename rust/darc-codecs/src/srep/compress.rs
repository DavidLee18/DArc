//! `compress<ACCELERATOR>`, ported from `Compression/SREP/compress.cpp:53`.
//!
//! The single-block match finder for the slow methods (`-m3`..`-m5`). It slides
//! an `L`-byte window over one block, probes the chunk table at every position,
//! and appends an `ENCODE_LZ_MATCH` record for each match it accepts.
//!
//! # `ACCELERATOR` is a template parameter in the C, and it changes the loop
//!
//! `srep.cpp:612-621` instantiates eight copies (0, 1, 2, 4, 8, 16, 32, 64) and
//! switches on the computed value. It is not a tuning knob bolted on top: with
//! `ACCELERATOR == 0` the loop hashes every byte with a single window, and with
//! any other value it hashes every `CYCLES`-th byte with a *narrower* window
//! (`hash1`, `L - OFFSET` bytes) and reconstructs the full-width `hash2` only
//! where the bit-array says a match is possible.
//!
//! Both paths must therefore find the *same* matches, which is why
//! `srep-encode-check.sh` sweeps all eight rather than trusting the default.
//! Here it is a runtime parameter: the C's templating is for speed, and the
//! branch structure is reproduced explicitly instead.
//!
//! # The input-match stream is inert without a dictionary
//!
//! `in_stat` carries matches found by the in-memory pass over a `-d` dictionary.
//! With no dictionary, `srep.cpp:605` writes a single **fence** record —
//! `lit_len = block_len + 1`, so its destination lands past the end of the block
//! and the "process next input match" branch never fires. The branch is ported
//! anyway, because it is on the path the moment `-d` is used and leaving it out
//! would make that a silent behaviour change rather than a missing feature.

use super::hash_table::{Chunk, HashTable, NOT_FOUND};
use super::matches::{self, MatchTooShort};
use super::rolling::{power, RollingHash, PRIME1};

/// Everything `compress()` takes that is not the block itself.
#[derive(Clone, Copy, Debug)]
pub struct Params {
    /// `ROUND_MATCHES` — round accepted lengths down to a multiple of `l`.
    pub round_matches: bool,
    /// `L` — the chunk size the table is indexed by.
    pub l: usize,
    /// `MIN_MATCH` — shorter candidates are rejected outright.
    pub min_match: usize,
    /// `BASE_LEN` — the divisor `ENCODE_LZ_MATCH` applies. **Not `l`.**
    pub base_len: u32,
    /// The `ACCELERATOR` template argument.
    pub accelerator: usize,
}

/// What one block produced.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Compressed {
    /// `literal_bytes` — block size minus the sum of accepted match lengths.
    pub literal_bytes: usize,
}

/// `record_match()` (`compress.cpp:8`).
///
/// Measures the candidate at `i` against chunk `k`, and if it is long enough
/// appends a record. Returns the new `last_match_end`, or `None` if the
/// candidate was too short to keep.
#[allow(clippy::too_many_arguments)]
fn record_match(
    p: &Params,
    block_start: u64,
    h: &HashTable,
    buf: &[u8],
    stat: &mut Vec<u32>,
    last_match_end: usize,
    literal_bytes: &mut usize,
    i: usize,
    k: Chunk,
) -> Result<Option<usize>, MatchTooShort> {
    let (mut mlen, add_len) = h.match_len(k, i, buf.len(), block_start, buf);
    if mlen < p.min_match {
        return Ok(None);
    }
    let match_start = i - add_len;
    if p.round_matches {
        mlen = mlen / p.l * p.l;
    }
    // `block_start + i - k*L` -- the distance back to the matched chunk.
    let match_offset = block_start + i as u64 - u64::from(k) * p.l as u64;

    matches::encode(
        stat,
        p.round_matches,
        p.base_len,
        (match_start - last_match_end) as u32,
        match_offset,
        mlen as u32,
    )?;

    *literal_bytes -= mlen;
    Ok(Some(match_start + mlen))
}

/// `compress<ACCELERATOR>()` (`compress.cpp:53`).
///
/// Appends records to `stat` and returns the block's literal-byte count.
pub fn compress(
    p: &Params,
    block_start: u64,
    h: &mut HashTable,
    buf: &[u8],
    in_stat: &[u32],
    stat: &mut Vec<u32>,
) -> Result<Compressed, MatchTooShort> {
    let block_size = buf.len();
    let l = p.l;
    let mut last_match_end = 0usize;
    let mut literal_bytes = block_size;

    let cycles = p.accelerator.max(1);
    let offset = cycles - 1;

    // Decode the first input match. Without a dictionary this is the fence.
    let mut instat = in_stat;
    let (mut match_start, mut match_len, mut match_offset) =
        match matches::decode(instat, false, p.round_matches, p.base_len, block_start) {
            Some((rec, used)) => {
                instat = &instat[used..];
                // Wrapping, because the FENCE record's derived values are
                // deliberately nonsense and are never read. Its src is
                // `dest/L1*L1 - offset`, and on a block shorter than BASE_LEN
                // that is `0 - 512`, which wraps in the C's unsigned Offset too.
                // The branch that would consume them is unreachable, since the
                // fence sets match_start past the end of the block. Under
                // overflow-checks a plain `-` panics here on any small input --
                // found by the round-trip test, not by reading.
                (
                    rec.lz_match.dest.wrapping_sub(block_start) as usize,
                    rec.lz_match.len as usize,
                    rec.lz_match.dest.wrapping_sub(rec.lz_match.src),
                )
            }
            // The C always has at least the fence record; with none, arrange for
            // the branch to be unreachable rather than reading past the slice.
            None => (block_size + 1, 0, 0),
        };

    // `if (2*L <= block_size)` -- a block too small to hold two chunks has no
    // match to find, and the loop bounds below would underflow.
    if 2 * l > block_size {
        return Ok(Compressed { literal_bytes });
    }

    let mut hash1 = RollingHash::new(l - offset, PRIME1);
    let mut hash2 = RollingHash::new(l, PRIME1);

    // PRIME1 powers for rebuilding hash2 from hash1 by adding the bytes just
    // before hash1's window start.
    let mut prime_powers = vec![0u64; cycles];
    prime_powers[cycles - 1] = power(PRIME1, (l - cycles) as u32);
    for j in (1..cycles).rev() {
        prime_powers[j - 1] = prime_powers[j].wrapping_mul(PRIME1);
    }

    // -- SPECIAL HANDLING FOR THE FIRST L BYTES (`:78`) ---------------------
    {
        let i = 0usize;
        hash1.moveto(buf);
        hash2.value = hash1.value;
        let mut j = 0usize;
        loop {
            if p.accelerator != 0 {
                h.mark_match_possibility(hash1.value);
            }
            if j == offset {
                break;
            }
            hash1.update(buf[j], buf[l - offset + j]);
            // "add OFFSET next bytes while removing zero bytes"
            hash2.update(0, buf[l - offset + j]);
            j += 1;
        }

        let k = h.find_match(buf, i, hash2.value);
        if k != NOT_FOUND {
            match record_match(
                p, block_start, h, buf, stat, last_match_end, &mut literal_bytes, i, k,
            )? {
                Some(end) => last_match_end = end,
                None => {}
            }
        }
        h.add_hash(((block_start + i as u64) / l as u64) as Chunk, hash2.value);
    }

    // -- MAIN CYCLE, one L-byte chunk per step (`:97`) ----------------------
    let mut i = 0usize;
    while i <= block_size - 2 * l {
        let mut saved_hash1 = 0u64;
        let next_chunk = i + l;

        // -- SUB-MAIN CYCLE (`:102`) ----------------------------------------
        while i < next_chunk {
            // Process the next input match once we have reached its start.
            if i >= match_start {
                // Encode it if enough of it survives after the overlap with our
                // own last match is removed.
                if match_start + match_len >= last_match_end + p.base_len as usize {
                    let clipped = match_start.max(last_match_end);
                    match_len -= clipped - match_start;
                    match_start = clipped;
                    let literal_len = match_start - last_match_end;
                    matches::encode(
                        stat,
                        p.round_matches,
                        p.base_len,
                        literal_len as u32,
                        match_offset,
                        match_len as u32,
                    )?;
                    last_match_end = match_start + match_len;
                    literal_bytes -= match_len;
                }
                let basic = block_start + (match_start + match_len) as u64;
                match matches::decode(instat, false, p.round_matches, p.base_len, basic) {
                    Some((rec, used)) => {
                        instat = &instat[used..];
                        // Wrapping for the same reason as the first decode above.
                        match_start = rec.lz_match.dest.wrapping_sub(block_start) as usize;
                        match_len = rec.lz_match.len as usize;
                        match_offset = rec.lz_match.dest.wrapping_sub(rec.lz_match.src);
                    }
                    None => match_start = block_size + 1,
                }
            }

            // Advance hash1 up to last_match_end, rounded down to X bytes.
            let x = cycles.max(4);
            let next_i = (next_chunk - cycles).min(last_match_end.saturating_sub(1));
            if next_i >= i + l / 2 {
                // Quick path for a large jump: rehash from scratch.
                i = next_i & !(x - 1);
                hash1.moveto(&buf[i + offset..]);
            } else {
                while i + x <= next_i {
                    hash1.update_n(&buf[i + offset..], x);
                    i += x;
                }
            }

            let lookahead = match p.accelerator {
                0 => 128usize,
                _ => 256,
            };
            let last_i = next_chunk.min(i + lookahead);
            let mut hashes1: Vec<u64> = Vec::with_capacity(lookahead);
            // (hash, position) pairs whose chunkarr slot is worth probing.
            let mut hashes2: Vec<(u64, usize)> = Vec::with_capacity(lookahead * 2);

            // PREFETCH bitarr AND UPDATE hash1 (`:138`)
            if p.accelerator != 0 {
                let mut i0 = i;
                while i0 < last_i {
                    hash1.update(buf[i0 + offset], buf[i0 + l]);
                    hashes1.push(hash1.value);
                    hash1.update_n(&buf[i0 + 1 + offset..], cycles - 1);
                    i0 += cycles;
                }
                saved_hash1 = match hashes1.last() {
                    Some(v) => *v,
                    None => saved_hash1,
                };
            }

            // CHECK bitarr AND CONDITIONALLY COLLECT CANDIDATES (`:153`)
            let mut h1i = 0usize;
            while i < last_i {
                match p.accelerator {
                    0 => {
                        // ACCELERATOR == 0: every byte, single window.
                        //
                        // The C runs the full X iterations regardless of last_i
                        // -- the `while (i < last_i)` above is the only guard, so
                        // it deliberately OVERSHOOTS by up to X-1 positions and
                        // the candidates it collects there are real. Breaking
                        // early here changed which matches were found.
                        for _ in 0..x {
                            if i + l >= buf.len() {
                                break;
                            }
                            hash1.update(buf[i], buf[i + l]);
                            i += 1;
                            if i >= last_match_end && i < match_start {
                                hashes2.push((hash1.value, i));
                            }
                        }
                    }
                    _ => {
                        let hsh1 = hashes1[h1i];
                        h1i += 1;
                        if h.check_match_possibility(hsh1) {
                            i += 1;
                            // Rebuild the full-width hash2 from the narrower
                            // hash1 by adding the CYCLES-1 leading bytes.
                            hash2.value = hsh1;
                            for j in 0..cycles - 1 {
                                hash2.value = hash2.value.wrapping_add(
                                    u64::from(buf[i + j]).wrapping_mul(prime_powers[j]),
                                );
                            }
                            if i >= last_match_end && i < match_start {
                                hashes2.push((hash2.value, i));
                            }
                            for _ in 0..cycles - 1 {
                                hash2.update(buf[i], buf[i + l]);
                                i += 1;
                                if i >= last_match_end && i < match_start {
                                    hashes2.push((hash2.value, i));
                                }
                            }
                        } else {
                            i += cycles;
                        }
                    }
                }
            }

            // CHECK chunkarr TRYING TO FIND A MATCH (`:187`)
            for (hsh, pos) in hashes2 {
                let k = h.find_match(buf, pos, hsh);
                if k != NOT_FOUND {
                    match record_match(
                        p, block_start, h, buf, stat, last_match_end, &mut literal_bytes, pos, k,
                    )? {
                        Some(end) => {
                            last_match_end = end;
                            // `goto match_found2` -- abandon the rest of this batch.
                            break;
                        }
                        None => {}
                    }
                }
            }
        }

        // -- SECOND SUB-MAIN CYCLE, replaying the last CYCLES bytes (`:196`) --
        if p.accelerator != 0 {
            hash1.value = saved_hash1;
            hash2.value = saved_hash1;
            h.mark_match_possibility(hash1.value);
            for j in 1..cycles {
                let base = i + j - cycles;
                hash1.update(buf[base + offset], buf[base + l]);
                h.mark_match_possibility(hash1.value);
                hash2.update(0, buf[base + l]);
            }
        }
        let hv = match p.accelerator {
            0 => hash1.value,
            _ => hash2.value,
        };
        h.add_hash(((block_start + i as u64) / l as u64) as Chunk, hv);
    }

    Ok(Compressed { literal_bytes })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::srep::hash_table::{Config, HashTable};

    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n)
            .map(|_| {
                s = s.wrapping_mul(1_103_515_245).wrapping_add(12_345);
                (s >> 16) as u8
            })
            .collect()
    }

    fn params(accel: usize, l: usize) -> Params {
        Params { round_matches: true, l, min_match: l, base_len: l as u32, accelerator: accel }
    }

    fn table(l: usize, accel: usize, filesize: u64) -> HashTable {
        HashTable::new(
            Config {
                l,
                compare_digests: true,
                precompute_digests: true,
                round_matches: true,
                // BITARR_ACCELERATOR = accel*8 (srep.cpp:505).
                bitarr_accelerator: (accel * 8) as u64,
            },
            filesize,
        )
    }

    /// The fence record srep.cpp:605 appends when there is no dictionary.
    fn fence(p: &Params, block_len: usize) -> Vec<u32> {
        let mut v = Vec::new();
        matches::encode(&mut v, p.round_matches, p.base_len,
                        block_len as u32 + 1, u64::from(p.base_len), p.base_len)
            .expect("fence encodes");
        v
    }

    #[test]
    fn a_duplicated_region_produces_matches_at_every_accelerator() {
        // The eight instantiations walk the block differently and must still
        // agree that a match exists. Proving they agree byte-for-byte is the
        // harness's job against the C; this proves none of them is inert.
        let l = 512usize;
        let half = prng(1, 8192);
        let buf: Vec<u8> = half.iter().chain(half.iter()).copied().collect();

        for accel in [0usize, 1, 2, 4, 8, 16, 32, 64] {
            let p = params(accel, l);
            let mut h = table(l, accel, buf.len() as u64);
            h.prepare_buffer(0, &buf);
            let mut stat = Vec::new();
            let out = compress(&p, 0, &mut h, &buf, &fence(&p, buf.len()), &mut stat)
                .expect("compresses");
            assert!(!stat.is_empty(), "accel={accel}: no match in a duplicated buffer");
            assert!(out.literal_bytes < buf.len(), "accel={accel}: literals did not shrink");
            assert!(out.literal_bytes > 0, "accel={accel}: claimed the whole block");
        }
    }

    #[test]
    fn incompressible_input_yields_no_records() {
        let l = 512usize;
        let buf = prng(9, 16384);
        for accel in [0usize, 4, 16] {
            let p = params(accel, l);
            let mut h = table(l, accel, buf.len() as u64);
            h.prepare_buffer(0, &buf);
            let mut stat = Vec::new();
            let out = compress(&p, 0, &mut h, &buf, &fence(&p, buf.len()), &mut stat).expect("ok");
            assert!(stat.is_empty(), "accel={accel}: invented a match in noise");
            assert_eq!(out.literal_bytes, buf.len(), "accel={accel}");
        }
    }

    #[test]
    fn a_block_smaller_than_two_chunks_is_returned_untouched() {
        // `if (2*L <= block_size)` -- the C skips the loop entirely, and the
        // bounds below it would underflow otherwise.
        let l = 512usize;
        let p = params(4, l);
        let buf = prng(3, l);
        let mut h = table(l, 4, 4096);
        let mut stat = Vec::new();
        let out = compress(&p, 0, &mut h, &buf, &fence(&p, buf.len()), &mut stat).expect("ok");
        assert!(stat.is_empty());
        assert_eq!(out.literal_bytes, buf.len());
    }

    #[test]
    fn the_fence_record_never_fires() {
        // Its destination is block_len+1, past the end. If the input-match
        // branch fired it would emit a record pointing outside the block.
        let l = 512usize;
        let p = params(0, l);
        let buf = prng(5, 4096);
        let mut h = table(l, 0, buf.len() as u64);
        h.prepare_buffer(0, &buf);
        let mut stat = Vec::new();
        compress(&p, 0, &mut h, &buf, &fence(&p, buf.len()), &mut stat).expect("ok");
        assert!(stat.is_empty(), "the fence was treated as a real match");
    }
}
