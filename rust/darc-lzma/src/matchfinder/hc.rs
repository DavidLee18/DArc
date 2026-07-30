//! Hash-chain match search — a port of `Hc_GetMatchesSpec` (`LzFind.c:880`) and
//! the bodies of `Hc4_MatchFinder_GetMatches` (`LzFind.c:1362`) /
//! `Hc5_MatchFinder_GetMatches` (`LzFind.c:1431`).
//!
//! This is the finder DArc actually ships with. `C_LZMA.cpp:253` defaults
//! `matchFinder = kHT4`, which `C_LZMA.cpp:106` maps to `btMode = 0,
//! numHashBytes = 5`, which `MatchFinder_CreateVTable` (`LzFind.c:1664`) resolves to
//! `Hc5_MatchFinder_GetMatches` / `Hc5_MatchFinder_Skip`. `kHC4` gives the same
//! functions with a 4-byte hash. Despite the name, `kHT4` is a five-byte hash
//! *chain*, not a four-byte hash table.
//!
//! ## `son` layout: one entry per slot, not two
//!
//! With `btMode == 0`, `MatchFinder_Create` (`LzFind.c:467`) sets
//! `numSons = cyclicBufferSize` and skips the `numSons <<= 1`. So `son` here is a
//! plain singly-linked chain indexed by the cyclic-buffer slot —
//! `son[cyclic_buffer_pos]`, **not** `son[cyclic_buffer_pos << 1]`. Every index in
//! this module is unshifted; passing a BT-sized `son` would still run, and would
//! still produce matches, just different ones. Nothing here can catch that, so the
//! allocation is the caller's contract (see the module docs' wiring notes).
//!
//! ## Bit-exactness hazards (see CLAUDE.md)
//!
//! - **`kEmptyHashValue = 0` is the chain terminator.** `Hc_GetMatchesSpec` tests
//!   `curMatch == 0` at the *top* of each iteration, before computing `delta`
//!   (`LzFind.c:920`). Reordering that after the `delta >= cyclicBufferSize` test
//!   would still terminate — `delta` would be `pos`, which is `>= cyclicBufferSize`
//!   only once `pos` grows past the dictionary — so early in the stream a
//!   0-terminated chain would be walked one step too far into slot
//!   `cyclic_buffer_pos - pos`. Keep the order.
//! - **`d2` / `d3` have no separate empty check.** An unset 2-/3-byte hash slot
//!   holds 0, so `d2 = pos - 0 = pos`, and the `SET_mmm` bound
//!   (`mmm = min(cyclicBufferSize, pos)`, `LzFind.c:1171`) rejects it because
//!   `pos < pos` is false. `mmm` *is* the sentinel test; dropping the `min(pos)` term
//!   would admit distances that reach before the start of the stream.
//! - **The chain is walked at most `cut_value` times**, `cut_value = mc`, and
//!   `LzmaEnc.c:99` halves the automatic `mc` when `btMode == 0`. A hash chain
//!   therefore searches half as deep as a tree at the same `fb` — see
//!   `MatchFinderKind::auto_mc` in `props.rs`.
//! - **The full-length hit is recorded unconditionally.** `LzFind.c:937` writes the
//!   `len == lenLimit` pair without re-testing `maxLen < len`, relying on the
//!   function's stated precondition `lenLimit > maxLen` (`LzFind.c:877`). Both
//!   prologues here uphold it: they return [`Prologue::PlantAndStop`] rather than
//!   `Search` whenever `max_len` reaches `len_limit`.
//! - **The byte scan starts at offset 0, not at `max_len`.** `cur[maxLen]` is only a
//!   filter (`LzFind.c:930`); the length that gets recorded comes from a scan
//!   beginning at `cur[0]` (`LzFind.c:932`). So a candidate can pass the filter and
//!   still yield `len == 0`, recording nothing. Starting the scan at `max_len` would
//!   report lengths that were never verified.
//! - **`Hc4` and `Hc5` differ from `Bt4` / `Bt5` in exactly two places** — the search
//!   function, and what the `maxLen == lenLimit` shortcut does instead of splicing a
//!   tree. See [`Prologue::PlantAndStop`].
//! - Distances are reported 0-based (`delta - 1`), as in the C `distances` array.

use super::hash::{CRC_SHIFT_1, CRC_SHIFT_2, HASH2_SIZE, HASH3_SIZE};
use super::Match;

/// `kEmptyHashValue` (`LzFind.c:17`). Both the "no such hash yet" value and the
/// chain terminator.
const EMPTY_HASH_VALUE: u32 = 0;

/// What the `Hc4` / `Hc5` short-match prologue decided.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Prologue {
    /// Fall through to [`get_matches_spec`] with this `max_len` — the C's
    /// `GET_MATCHES_FOOTER_HC(maxLen)` (`LzFind.c:1139`).
    Search { max_len: u32 },
    /// `maxLen == lenLimit`: nothing more can be found, so the C skips the search
    /// entirely and only links the current slot into the chain —
    /// `p->son[p->cyclicBufferPos] = curMatch; MOVE_POS_RET` (`LzFind.c:1421`,
    /// `LzFind.c:1495`).
    ///
    /// This is one of the two places `Hc4`/`Hc5` diverge from `Bt4`/`Bt5`, which
    /// instead call `SkipMatchesSpec` here to splice the tree (`LzFind.c:1278`,
    /// `LzFind.c:1352`). Calling the BT variant would walk and rewrite `son` as if
    /// it were a tree.
    PlantAndStop,
}

/// `HASH4_CALC` (`LzFind.c:49`) → `(h2, h3, hv)`, all ready to index `hash`
/// (`h2` directly, `h3` at `+ FIX3_HASH_SIZE`, `hv` at `+ FIX4_HASH_SIZE`).
///
/// Needs 4 readable bytes at `cur`, which `len_limit >= 4` guarantees.
#[inline]
pub(crate) fn hash4_calc(
    crc: &[u32; 256],
    input: &[u8],
    cur: usize,
    hash_mask: u32,
) -> (usize, usize, usize) {
    let temp = crc[input[cur] as usize] ^ input[cur + 1] as u32;
    let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
    let temp = temp ^ ((input[cur + 2] as u32) << 8);
    let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
    let hv = ((temp ^ (crc[input[cur + 3] as usize] << CRC_SHIFT_1)) & hash_mask) as usize;
    (h2, h3, hv)
}

/// `HASH5_CALC` (`LzFind.c:56`) → `(h2, h3, hv)`; `hv` indexes `hash` at
/// `+ FIX5_HASH_SIZE`.
///
/// Note that `h2` and `h3` fold in exactly the same bytes as [`hash4_calc`] — the
/// two finders share the 2- and 3-byte tables byte for byte. Only `hv` differs, by
/// one extra `crc[cur[4]] << 10` term. `hash4Mask` / `h4` are commented out in the C
/// (`LzFind.c:62`); there is no 4-byte table in the 5-byte finder.
///
/// Needs 5 readable bytes at `cur`, which `len_limit >= 5` guarantees.
#[inline]
pub(crate) fn hash5_calc(
    crc: &[u32; 256],
    input: &[u8],
    cur: usize,
    hash_mask: u32,
) -> (usize, usize, usize) {
    let temp = crc[input[cur] as usize] ^ input[cur + 1] as u32;
    let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
    let temp = temp ^ ((input[cur + 2] as u32) << 8);
    let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
    let temp = temp ^ (crc[input[cur + 3] as usize] << CRC_SHIFT_1);
    let hv = ((temp ^ (crc[input[cur + 4] as usize] << CRC_SHIFT_2)) & hash_mask) as usize;
    (h2, h3, hv)
}

/// `UPDATE_maxLen` (`LzFind.c:1144`): extend the match at back-distance `diff`
/// forward from offset `max_len`, stopping at `len_limit`.
#[inline]
fn update_max_len(len_limit: u32, diff: u32, input: &[u8], cur: usize, max_len: u32) -> u32 {
    let diff = diff as usize;
    let lim = cur + len_limit as usize;
    let mut c = cur + max_len as usize;
    while c != lim && input[c - diff] == input[c] {
        c += 1;
    }
    (c - cur) as u32
}

/// The 2-/3-byte hash probe shared by `Hc4`, `Hc5`, `Bt4` and `Bt5`
/// (`LzFind.c:1390-1415` and `LzFind.c:1247-1272`, which are the same code).
///
/// Pushes 0, 1 or 2 entries onto `out` and returns the back-distance the caller must
/// extend from (the C's re-assigned `d2`), or `None` when neither hash gives even a
/// one-byte match — the C's bare `break` out of the `for(;;)`, which leaves `maxLen`
/// at its initial value and any pushed pair at length 2.
///
/// The `len: 0` placeholder is deliberate and mirrors the C exactly: `distances[0]`
/// of the second pair is never written in the branch, only `distances[1]`; the length
/// arrives later via `distances[-2] = maxLen`. The caller **must** overwrite
/// `out.last()`'s `len` whenever this returns `Some`.
fn push_short_matches(
    d2: u32,
    d3: u32,
    mmm: u32,
    input: &[u8],
    cur: usize,
    out: &mut Vec<Match>,
) -> Option<u32> {
    match d2 < mmm && input[cur - d2 as usize] == input[cur] {
        true => {
            out.push(Match {
                len: 2,
                dist: d2 - 1,
            });
            match input[cur - d2 as usize + 2] == input[cur + 2] {
                // The 2-byte hit already covers cur[2]; keep d2 and extend it.
                true => Some(d2),
                false => match d3 < mmm && input[cur - d3 as usize] == input[cur] {
                    true => {
                        out.push(Match {
                            len: 0,
                            dist: d3 - 1,
                        });
                        Some(d3)
                    }
                    false => None,
                },
            }
        }
        false => match d3 < mmm && input[cur - d3 as usize] == input[cur] {
            true => {
                out.push(Match {
                    len: 0,
                    dist: d3 - 1,
                });
                Some(d3)
            }
            false => None,
        },
    }
}

/// `Hc4_MatchFinder_GetMatches` (`LzFind.c:1362`), everything between `SET_mmm` and
/// `GET_MATCHES_FOOTER_HC`.
///
/// `mmm` is `SET_mmm` (`LzFind.c:1171`): `min(cyclic_buffer_size, pos)`. `d2`/`d3`
/// are `pos - hash[..]` read *before* the hash slots are overwritten with `pos`.
/// Requires `len_limit >= 4`.
pub(crate) fn hc4_prologue(
    len_limit: u32,
    d2: u32,
    d3: u32,
    mmm: u32,
    input: &[u8],
    cur: usize,
    out: &mut Vec<Match>,
) -> Prologue {
    // maxLen = 3 (LzFind.c:1386): the 4-byte hash chain below only reports >= 4,
    // and UPDATE_maxLen starts its scan at cur[3].
    let max_len = 3u32;
    match push_short_matches(d2, d3, mmm, input, cur, out) {
        None => Prologue::Search { max_len },
        Some(diff) => {
            let max_len = update_max_len(len_limit, diff, input, cur, max_len);
            // distances[-2] = maxLen (LzFind.c:1418).
            let last = out.len() - 1;
            out[last].len = max_len;
            match max_len == len_limit {
                true => Prologue::PlantAndStop,
                false => Prologue::Search { max_len },
            }
        }
    }
}

/// `Hc5_MatchFinder_GetMatches` (`LzFind.c:1431`), everything between `SET_mmm` and
/// `GET_MATCHES_FOOTER_HC`. Requires `len_limit >= 5`.
///
/// Two differences from [`hc4_prologue`], both of which change emitted bytes:
///
/// 1. `maxLen` starts at **4**, so `UPDATE_maxLen` scans from `cur[4]`.
/// 2. Because of that, `cur[3]` has to be confirmed separately
///    (`LzFind.c:1488-1490`): the pair is first stamped with length **3**, and on a
///    mismatch at `cur[3]` the function bails out with that 3-byte match recorded
///    while still handing `max_len == 4` to the chain search. The recorded length and
///    the search's `max_len` disagree on purpose; "fixing" that to 3 makes the chain
///    re-report a 4-byte match the C suppresses.
pub(crate) fn hc5_prologue(
    len_limit: u32,
    d2: u32,
    d3: u32,
    mmm: u32,
    input: &[u8],
    cur: usize,
    out: &mut Vec<Match>,
) -> Prologue {
    let max_len = 4u32;
    match push_short_matches(d2, d3, mmm, input, cur, out) {
        None => Prologue::Search { max_len },
        Some(diff) => {
            let last = out.len() - 1;
            // distances[-2] = 3 (LzFind.c:1488), before cur[3] is known to match.
            out[last].len = 3;
            match input[cur - diff as usize + 3] == input[cur + 3] {
                false => Prologue::Search { max_len },
                true => {
                    let max_len = update_max_len(len_limit, diff, input, cur, max_len);
                    out[last].len = max_len;
                    match max_len == len_limit {
                        true => Prologue::PlantAndStop,
                        false => Prologue::Search { max_len },
                    }
                }
            }
        }
    }
}

/// `Hc_GetMatchesSpec` (`LzFind.c:880`): walk the hash chain from `cur_match`,
/// append `(len, dist0)` pairs of strictly increasing length to `out`, and link the
/// current slot into the chain.
///
/// The link happens first and unconditionally (`LzFind.c:914`), so
/// `son[cyclic_buffer_pos]` is written even when the chain is empty or the cut value
/// runs out. `son` has **one** entry per cyclic-buffer slot here (`btMode == 0`).
///
/// Preconditions, both from the C: `len_limit > max_len`, and `cur_match` is either
/// `kEmptyHashValue` or a strictly earlier `pos` value.
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_matches_spec(
    len_limit: u32,
    mut cur_match: u32,
    pos: u32,
    input: &[u8],
    cur: usize,
    son: &mut [u32],
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    mut cut_value: u32,
    out: &mut Vec<Match>,
    mut max_len: u32,
) {
    son[cyclic_buffer_pos as usize] = cur_match;

    loop {
        // Terminator test first — see the module docs. `delta` is only well-defined
        // (>= 1, no underflow) once `cur_match != 0`, because every stored value is a
        // strictly earlier `pos`.
        if cur_match == EMPTY_HASH_VALUE {
            break;
        }
        let delta = pos - cur_match;
        if delta >= cyclic_buffer_size {
            break;
        }

        // son[cyclicBufferPos - delta + (delta > cyclicBufferPos ? cyclicBufferSize : 0)]
        let slot = match delta > cyclic_buffer_pos {
            true => cyclic_buffer_pos + cyclic_buffer_size - delta,
            false => cyclic_buffer_pos - delta,
        } as usize;
        cur_match = son[slot];

        let diff = delta as usize;
        // Filter only: cur[maxLen] == cur[maxLen - delta] (LzFind.c:930).
        if input[cur + max_len as usize] == input[cur + max_len as usize - diff] {
            // Scan from offset 0, not from max_len.
            let mut len = 0usize;
            while input[cur + len] == input[cur + len - diff] {
                len += 1;
                if len == len_limit as usize {
                    // Written without re-testing maxLen < len, as in the C; the
                    // precondition len_limit > max_len makes the test redundant.
                    out.push(Match {
                        len: len_limit,
                        dist: delta - 1,
                    });
                    return;
                }
            }
            let len = len as u32;
            if max_len < len {
                max_len = len;
                out.push(Match {
                    len,
                    dist: delta - 1,
                });
            }
        }

        // C: `} while (--cutValue);` — decrement, then exit on zero. `wrapping_sub`
        // is not paranoia: it reproduces the C's behaviour for `cut_value == 0`
        // (wrap to 2^32-1 and keep going) instead of panicking in a debug build on an
        // input the C accepts. `mc` is never 0 in practice — `auto_mc` floors at 8 —
        // but that is a caller invariant, not something this function can see.
        cut_value = cut_value.wrapping_sub(1);
        if cut_value == 0 {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The 5-byte finder's `fixedHashSize` is the 4-byte one, `kFix5HashSize` being
    /// `#define`d to `kFix4HashSize` (`LzFind.c:29`). Asserted here as well as
    /// documented in [`super::hash`], because the failure mode — chain heads aliased
    /// onto the 3-byte table — still compresses and still round-trips.
    #[test]
    fn fix5_hash_size_is_fix4_hash_size() {
        use super::super::hash::{FIX4_HASH_SIZE, FIX5_HASH_SIZE};
        assert_eq!(FIX5_HASH_SIZE, HASH2_SIZE + HASH3_SIZE);
        assert_eq!(FIX5_HASH_SIZE, FIX4_HASH_SIZE);
    }

    fn crc_table() -> [u32; 256] {
        let mut crc = [0u32; 256];
        for (i, slot) in crc.iter_mut().enumerate() {
            let mut r = i as u32;
            for _ in 0..8 {
                r = (r >> 1) ^ (0xEDB8_8320u32 & 0u32.wrapping_sub(r & 1));
            }
            *slot = r;
        }
        crc
    }

    /// `HASH5_CALC` extends `HASH4_CALC`: the 2- and 3-byte hashes must come out
    /// identical, since the two finders share those tables.
    #[test]
    fn hash4_and_hash5_agree_on_h2_and_h3() {
        let crc = crc_table();
        let input: Vec<u8> = (0u32..64).map(|i| (i * 37 + 11) as u8).collect();
        for cur in 0..50usize {
            let (h2a, h3a, _) = hash4_calc(&crc, &input, cur, 0xFFFF);
            let (h2b, h3b, _) = hash5_calc(&crc, &input, cur, 0xFFFF);
            assert_eq!(h2a, h2b, "h2 diverged at {cur}");
            assert_eq!(h3a, h3b, "h3 diverged at {cur}");
        }
    }

    /// A three-link chain over a repeating pattern: the search must report strictly
    /// increasing lengths with 0-based distances, and must link the current slot.
    #[test]
    fn walks_a_chain_and_reports_increasing_lengths() {
        // Stream: "abcdX" "abcdY" "abcde" then the current position "abcde".
        let input: Vec<u8> = b"abcdXabcdYabcdeabcde".to_vec();
        let cur = 15usize; // the final "abcde"
        let pos = 16u32; // buf[cur] is stream byte pos-1
        let cyclic_buffer_size = 64u32;
        let cyclic_buffer_pos = pos;
        let mut son = vec![0u32; cyclic_buffer_size as usize];

        // Chain head = most recent candidate (offset 10, pos 11), then 5, then 0.
        let head = 11u32;
        son[(cyclic_buffer_pos - (pos - 11)) as usize] = 6; // 11 -> 6
        son[(cyclic_buffer_pos - (pos - 6)) as usize] = 1; // 6 -> 1
        son[(cyclic_buffer_pos - (pos - 1)) as usize] = EMPTY_HASH_VALUE;

        let mut out = Vec::new();
        get_matches_spec(
            5,
            head,
            pos,
            &input,
            cur,
            &mut son,
            cyclic_buffer_pos,
            cyclic_buffer_size,
            32,
            &mut out,
            3,
        );

        // offset 10 matches "abcde" fully (len 5 == len_limit) -> single pair, and the
        // walk returns immediately.
        assert_eq!(
            out,
            vec![Match {
                len: 5,
                dist: pos - head - 1
            }]
        );
        assert_eq!(son[cyclic_buffer_pos as usize], head, "slot was not linked");
    }

    /// `cut_value` bounds the walk. With a cut of 1 the chain is inspected exactly
    /// once, so the deeper (and here longer) candidate is never seen.
    #[test]
    fn cut_value_bounds_the_walk() {
        let input: Vec<u8> = b"abcdeXXXXXabcdYabcde".to_vec();
        let cur = 15usize;
        let pos = 16u32;
        let cyclic_buffer_size = 64u32;
        let cyclic_buffer_pos = pos;

        let run = |cut: u32| {
            let mut son = vec![0u32; cyclic_buffer_size as usize];
            son[(cyclic_buffer_pos - (pos - 11)) as usize] = 1; // 11 -> 1
            son[(cyclic_buffer_pos - (pos - 1)) as usize] = EMPTY_HASH_VALUE;
            let mut out = Vec::new();
            get_matches_spec(
                5,
                11,
                pos,
                &input,
                cur,
                &mut son,
                cyclic_buffer_pos,
                cyclic_buffer_size,
                cut,
                &mut out,
                3,
            );
            out
        };

        // Depth 1 sees only offset 10 ("abcdY" vs "abcde") -> len 4.
        assert_eq!(run(1), vec![Match { len: 4, dist: 4 }]);
        // Depth 2 also sees offset 0 ("abcde") -> len 5, the full limit.
        assert_eq!(
            run(2),
            vec![Match { len: 4, dist: 4 }, Match { len: 5, dist: 14 }]
        );
    }

    /// The `kEmptyHashValue` head terminates the walk without recording anything,
    /// while still linking the slot.
    #[test]
    fn empty_head_records_nothing_but_still_links() {
        let input: Vec<u8> = b"aaaaaaaaaaaaaaaa".to_vec();
        let mut son = vec![7u32; 64];
        let mut out = Vec::new();
        get_matches_spec(
            5,
            EMPTY_HASH_VALUE,
            9,
            &input,
            8,
            &mut son,
            9,
            64,
            32,
            &mut out,
            3,
        );
        assert!(out.is_empty());
        assert_eq!(son[9], EMPTY_HASH_VALUE);
    }

    /// `Hc5` stamps length 3 and bails when `cur[3]` mismatches, yet still asks the
    /// chain for matches from `max_len == 4`. The two disagreeing values are the
    /// point of the test.
    #[test]
    fn hc5_records_three_but_searches_from_four() {
        //                    0123456789
        let input: Vec<u8> = b"abcZ--abcQzz".to_vec();
        let cur = 6usize; // "abcQ"
        let mut out = Vec::new();
        // d2 = 6 (offset 0), d3 unusable.
        let got = hc5_prologue(5, 6, 6, 64, &input, cur, &mut out);
        assert_eq!(got, Prologue::Search { max_len: 4 });
        assert_eq!(out, vec![Match { len: 3, dist: 5 }]);
    }

    /// `Hc4` has no such confirmation step: it extends straight from `cur[3]`.
    #[test]
    fn hc4_extends_from_offset_three() {
        let input: Vec<u8> = b"abcde--abcdX".to_vec();
        let cur = 7usize; // "abcdX"
        let mut out = Vec::new();
        // d2 = 7 (offset 0) matches "abcd" then diverges at cur[4].
        let got = hc4_prologue(5, 7, 7, 64, &input, cur, &mut out);
        assert_eq!(got, Prologue::Search { max_len: 4 });
        assert_eq!(out, vec![Match { len: 4, dist: 6 }]);
    }

    /// Reaching `len_limit` in the prologue means the caller must plant and stop
    /// rather than search.
    #[test]
    fn full_length_prologue_asks_the_caller_to_plant_and_stop() {
        let input: Vec<u8> = b"abcde--abcde".to_vec();
        let cur = 7usize;
        let mut out = Vec::new();
        let got = hc4_prologue(5, 7, 7, 64, &input, cur, &mut out);
        assert_eq!(got, Prologue::PlantAndStop);
        assert_eq!(out, vec![Match { len: 5, dist: 6 }]);
    }

    /// Neither hash gives a byte-0 match: nothing is pushed and `max_len` stays at
    /// the finder's base (3 for Hc4, 4 for Hc5).
    #[test]
    fn no_short_match_leaves_max_len_at_the_base() {
        let input: Vec<u8> = b"zzzzzzzabcde".to_vec();
        let cur = 7usize;
        let mut out = Vec::new();
        // mmm = 1 rejects both distances outright.
        assert_eq!(
            hc4_prologue(5, 7, 7, 1, &input, cur, &mut out),
            Prologue::Search { max_len: 3 }
        );
        assert!(out.is_empty());
        assert_eq!(
            hc5_prologue(5, 7, 7, 1, &input, cur, &mut out),
            Prologue::Search { max_len: 4 }
        );
        assert!(out.is_empty());
    }
}
