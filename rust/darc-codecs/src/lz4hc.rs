//! LZ4 high-compression encoder, ported from the vendored `lz4hc.c` (lz4 v1.10.0).
//!
//! `lz4_flex` provides the fast encoder and the decoder but has **no
//! high-compression mode**, which is the one thing keeping `Compression/LZ4`
//! alive: `lz4hc.c` does `#include "lz4.c"` for shared internals, so the two
//! files are a unit and neither can be deleted while HC is wanted.
//!
//! ## What is reproduced
//!
//! HC is **encoder-only** and emits ordinary LZ4 blocks, so the standing
//! "format-valid is acceptable for standard formats" rule would have allowed an
//! encoder that merely decodes correctly. Every strategy ported exactly, so this
//! goes further and is **byte-identical to the C at all twelve levels** --
//! verified over 26 inputs per level by `rust/difftest/lz4hc-check.sh`, which
//! gates on that identity rather than on a size budget.
//!
//! All three strategies from the C's selection table are implemented:
//!
//! * [`compress_mid`] -- `LZ4MID_compressBlock`, levels 1-2.
//! * The hash chain -- `LZ4HC_compress_hashChain`, levels 3-9, including the
//!   three-match lookahead parser and `patternAnalysis`. DArc's `lz4:hc`
//!   keyword is level 9, so this is the path that normally gets used.
//! * [`compress_optimal`] -- `LZ4HC_compress_optimal`, levels 10-12, including
//!   `chainSwap`, which only this parser enables.
//!
//! Deliberately not ported, because DArc never reaches them:
//!
//! * **Dictionary and streaming paths.** `C_LZ4.cpp` calls the one-shot
//!   `LZ4_compress_HC`, so there is no external dictionary, no prefix carried
//!   between blocks, and `limit` is always `limitedOutput`. That removes every
//!   `extDict`/`dictCtx` branch from the search.
//! * **`favorDecSpeed`.** Reachable only via `LZ4_favorDecompressionSpeed()`,
//!   which DArc never calls; the one-shot entry point zeroes the context.
//!
//! ## Where byte-identity actually came from
//!
//! Three details decided it, none visible from the function signatures, and
//! each caught by only a couple of corpus inputs -- which is why those inputs
//! are load-bearing rather than filler:
//!
//! * `patternAnalysis` (levels 9+) short-circuits chains of a single repeated
//!   byte. Without it the output was still valid and only 0.08% larger -- small
//!   enough to have passed a ratio budget and hidden the difference. Caught only
//!   by `runs` and `skew`.
//! * `lz4mid` fills its hash tables using the `ipIndex` captured at the top of
//!   the loop, which its own catch-back leaves stale. Recomputing it is the
//!   obvious "fix" and makes the output diverge.
//! * `chainSwap` walks along a match looking for the position whose chain jumps
//!   furthest back. Its accelerating stride (`kTrigger`) is part of the result,
//!   not just a speed trick: changing it changes which matches are found.

/// Minimum length of a match the LZ4 format can encode.
const MINMATCH: i32 = 4;
/// The block format requires the last 5 bytes to be literals.
const LASTLITERALS: i32 = 5;
/// No match may start within the last 12 bytes.
const MFLIMIT: i32 = 12;
/// `lz4.c:249` -- shorter inputs are emitted as a single literal run.
const LZ4_MIN_LENGTH: i32 = MFLIMIT + 1;

const ML_BITS: u32 = 4;
const ML_MASK: i32 = (1 << ML_BITS) - 1;
const RUN_MASK: i32 = 15;
/// `lz4hc.c:76` -- the match length above which the parser stops trimming.
const OPTIMAL_ML: i32 = (ML_MASK - 1) + MINMATCH;

/// The largest offset an LZ4 block can encode: the offset field is 16 bits.
const DISTANCE_MAX: u32 = 65535;

const HASH_LOG: u32 = 15;
const HASHTABLE_SIZE: usize = 1 << HASH_LOG;
/// `LZ4HC_MAXD` -- the chain table is indexed by the low 16 bits of a position
/// (`DELTANEXTU16`, `lz4hc.c:228`).
const MAXD: usize = 1 << 16;

/// `LZ4HC_init_internal` (`lz4hc.c:242-259`) starts a fresh context at a 64 KB
/// index offset: `nextToUpdate = dictLimit = lowLimit = 64 KB`. That offset is
/// load-bearing, not cosmetic -- the hash table is zero-filled, so slot 0 means
/// "never written", and keeping every real position at or above 64 KB is what
/// makes an untouched slot compare below `lowest_match_index` and be rejected.
/// Reproducing the offset reproduces that behaviour exactly.
const BASE: u32 = 65536;

/// `HASH_FUNCTION` (`lz4hc.c:121`), a Knuth multiplicative hash of 4 bytes.
#[inline]
fn hash4(v: u32) -> usize {
    (v.wrapping_mul(2654435761) >> (32 - HASH_LOG)) as usize
}

/// Bounds-checked 4-byte little-endian read.
///
/// Every caller is already guarded by the parser's own limits, so `None` should
/// be unreachable. It returns `None` rather than a placeholder precisely so a
/// mistake cannot fabricate a match: two out-of-range reads yielding the same
/// sentinel would compare equal and invent a match that does not exist.
#[inline]
fn read32(src: &[u8], p: i32) -> Option<u32> {
    if p < 0 {
        return None;
    }
    let p = p as usize;
    src.get(p..p + 4).map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
}

/// Bounds-checked 2-byte read, used only by the speculative pre-filter.
#[inline]
fn read16(src: &[u8], p: i32) -> Option<u16> {
    if p < 0 {
        return None;
    }
    let p = p as usize;
    src.get(p..p + 2).map(|b| u16::from_le_bytes([b[0], b[1]]))
}

/// `LZ4_count` -- common bytes going forward from `a`/`b`, stopping at `limit`.
#[inline]
fn count_forward(src: &[u8], mut a: i32, mut b: i32, limit: i32) -> i32 {
    let start = a;
    while a < limit {
        match (src.get(a as usize), src.get(b as usize)) {
            (Some(x), Some(y)) if x == y => {
                a += 1;
                b += 1;
            }
            _ => break,
        }
    }
    a - start
}

/// `LZ4HC_countBack` (`lz4hc.c:202-220`) -- common bytes going *backwards*,
/// returned as a **negative** count. `m_min` is 0 here: with no external
/// dictionary the match can never precede the start of the block.
///
/// The C steps 4 bytes at a time; this is the byte-at-a-time equivalent, which
/// returns the same value.
#[inline]
fn count_back(src: &[u8], ip: i32, m: i32, i_min: i32) -> i32 {
    let mut back = 0i32;
    let min = core::cmp::max(i_min - ip, -m);
    while back > min {
        let (a, b) = (ip + back - 1, m + back - 1);
        match (src.get(a as usize), src.get(b as usize)) {
            (Some(x), Some(y)) if x == y => back -= 1,
            _ => break,
        }
    }
    back
}

/// `LZ4HC_countPattern` (`lz4hc.c:820`), reduced to a run of one byte value.
///
/// The caller only ever reaches this after the repeat test below has confirmed
/// all four bytes of the 32-bit pattern are equal, so the C's word-at-a-time
/// comparison against a replicated pattern degenerates to counting a byte run --
/// and `LZ4HC_rotatePattern` becomes a no-op, which is why it is absent here.
#[inline]
fn count_pattern(src: &[u8], start: i32, end: i32, byte: u8) -> i32 {
    let mut p = start;
    while p < end && src.get(p as usize) == Some(&byte) {
        p += 1;
    }
    p - start
}

/// `LZ4HC_reverseCountPattern` (`lz4hc.c:854`) -- the same run, counted
/// backwards from `start` and not passing `low`.
#[inline]
fn reverse_count_pattern(src: &[u8], start: i32, low: i32, byte: u8) -> i32 {
    let mut p = start;
    while p > low && src.get((p - 1) as usize) == Some(&byte) {
        p -= 1;
    }
    start - p
}

/// Whether the 32-bit sample is a repetition of a single byte.
///
/// The C writes this as `((pattern & 0xFFFF) == (pattern >> 16)) &
/// ((pattern & 0xFF) == (pattern >> 24))` (`lz4hc.c:992`). Those two together
/// force all four bytes equal: the first gives `b0==b2, b1==b3`, the second
/// `b0==b3`, and the pair collapses to `b0==b1==b2==b3`.
#[inline]
fn is_byte_repeat(pattern: u32) -> bool {
    (pattern & 0xFFFF) == (pattern >> 16) && (pattern & 0xFF) == (pattern >> 24)
}

/// Tri-state from `repeat_state_e` (`lz4hc.c:880`): the repeat test is run at
/// most once per search, and its answer cached for the rest of the chain walk.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Repeat {
    Untested,
    Not,
    Confirmed,
}

/// A candidate match: length, offset (distance back), and how far the match was
/// extended *backwards* from the search position (negative, `lz4hc.c:360`).
#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
struct Match {
    off: i32,
    len: i32,
    back: i32,
}

const NO_MATCH: Match = Match { off: 0, len: 0, back: 0 };

/// The hash-chain index: a hash table of most-recent positions plus a table of
/// 16-bit deltas linking each position to the previous one with the same hash.
struct HashChain {
    hash_table: Vec<u32>,
    chain_table: Vec<u16>,
    next_to_update: u32,
}

impl HashChain {
    fn new() -> Self {
        HashChain {
            hash_table: vec![0u32; HASHTABLE_SIZE],
            chain_table: vec![0u16; MAXD],
            next_to_update: BASE,
        }
    }

    /// `LZ4HC_Insert` (`lz4hc.c:781`) -- index every position up to, but not
    /// including, `pos`.
    fn insert(&mut self, src: &[u8], pos: i32) {
        let target = pos as u32 + BASE;
        let mut idx = self.next_to_update;
        while idx < target {
            let p = (idx - BASE) as i32;
            let Some(v) = read32(src, p) else { break };
            let h = hash4(v);
            let mut delta = idx - self.hash_table[h];
            if delta > DISTANCE_MAX {
                delta = DISTANCE_MAX;
            }
            self.chain_table[(idx & 0xFFFF) as usize] = delta as u16;
            self.hash_table[h] = idx;
            idx += 1;
        }
        self.next_to_update = target;
    }

    /// `LZ4HC_InsertAndGetWiderMatch` (`lz4hc.c:885`), reduced to the
    /// no-dictionary, no-`favorDecSpeed` case -- see the module header for why
    /// each of those drops out. (`favorDecSpeed` is reachable only through
    /// `LZ4_favorDecompressionSpeed()`, which DArc never calls; the one-shot
    /// `LZ4_compress_HC` zeroes the context, so it is always 0 here.)
    ///
    /// `i_low_limit` bounds how far back the match may be extended; passing
    /// `i_low_limit == ip` forbids backward extension entirely, which is what
    /// makes this double as `LZ4HC_InsertAndFindBestMatch` (`lz4hc.c:1117`).
    ///
    /// `chain_swap` is used only by the optimal parser (`lz4hc.c:1812`); the
    /// hash-chain parser passes 0 at every call site.
    fn get_wider_match(
        &mut self,
        src: &[u8],
        ip: i32,
        i_low_limit: i32,
        i_high_limit: i32,
        mut longest: i32,
        max_nb_attempts: i32,
        pattern_analysis: bool,
        chain_swap: bool,
    ) -> Match {
        let ip_index = ip as u32 + BASE;
        // `withinStartDistance` (lz4hc.c:901): while the whole block is still
        // within one 64 KB window of the start, nothing is out of reach.
        let lowest_match_index = if BASE + DISTANCE_MAX + 1 > ip_index {
            BASE
        } else {
            ip_index - DISTANCE_MAX
        };
        let look_back_length = ip - i_low_limit;
        let mut nb_attempts = max_nb_attempts;
        let mut offset = 0i32;
        let mut s_back = 0i32;
        let mut repeat = Repeat::Untested;
        let mut src_pattern_length = 0i32;
        // Persists across chain steps once chain_swap sets it, as in the C.
        let mut match_chain_pos = 0u32;

        let Some(pattern) = read32(src, ip) else { return NO_MATCH };

        self.insert(src, ip);
        let mut match_index = self.hash_table[hash4(pattern)];

        while match_index >= lowest_match_index && nb_attempts > 0 {
            nb_attempts -= 1;
            assert!(match_index < ip_index);
            let m = (match_index - BASE) as i32;

            // Speculative pre-filter (lz4hc.c:933): if the two bytes at the end
            // of the best match so far already disagree, this candidate cannot
            // beat it. When either probe falls outside the block the check is
            // skipped rather than failed -- skipping can only admit more
            // candidates to the full test below, never reject a real match.
            let probe_ip = i_low_limit + longest - 1;
            let probe_m = m - look_back_length + longest - 1;
            let filtered = match (read16(src, probe_ip), read16(src, probe_m)) {
                (Some(a), Some(b)) => a != b,
                _ => false,
            };

            let mut match_length = 0i32;
            if !filtered && read32(src, m) == Some(pattern) {
                let back = if look_back_length != 0 {
                    count_back(src, ip, m, i_low_limit)
                } else {
                    0
                };
                match_length =
                    MINMATCH + count_forward(src, ip + MINMATCH, m + MINMATCH, i_high_limit);
                match_length -= back; // `back` is negative: the match grew leftwards
                if match_length > longest {
                    longest = match_length;
                    offset = (ip_index - match_index) as i32;
                    s_back = back;
                }
            }

            // ---- Chain swap (lz4hc.c:964-985) ----
            //
            // Having tied the best length so far, look along the match for the
            // position whose chain jumps furthest back, and continue from there
            // instead. It scans with an accelerating stride that resets whenever
            // it finds a better jump, so a long match costs far fewer probes
            // than one step per byte. Only the optimal parser enables this.
            if chain_swap && match_length == longest {
                assert_eq!(look_back_length, 0); // search forward only
                if match_index + longest as u32 <= ip_index {
                    const K_TRIGGER: i32 = 4;
                    let mut distance_to_next_match = 1u32;
                    let end = longest - MINMATCH + 1;
                    let mut accel = 1i32 << K_TRIGGER;
                    let mut pos = 0i32;
                    while pos < end {
                        let candidate_dist = self.chain_table
                            [((match_index.wrapping_add(pos as u32)) & 0xFFFF) as usize]
                            as u32;
                        // `step = (accel++ >> kTrigger)` -- post-increment, so the
                        // stride uses the value from before this probe.
                        let step = accel >> K_TRIGGER;
                        accel += 1;
                        if candidate_dist > distance_to_next_match {
                            distance_to_next_match = candidate_dist;
                            match_chain_pos = pos as u32;
                            accel = 1 << K_TRIGGER;
                        }
                        pos += step;
                    }
                    if distance_to_next_match > 1 {
                        if distance_to_next_match > match_index {
                            break; // avoid overflow
                        }
                        match_index -= distance_to_next_match;
                        continue;
                    }
                }
            }

            // ---- Pattern analysis (lz4hc.c:987-1059) ----
            //
            // A chain delta of 1 means the previous position with this hash is
            // the byte immediately before, which is what a run of one repeated
            // byte looks like. Walking such a chain one step at a time burns
            // the whole attempt budget crossing the run; this jumps straight to
            // the useful end of it. The C enables it only for `nbSearches > 128`
            // -- levels 9 and up -- which is why levels 3..8 never take it.
            //
            // `LZ4HC_protectDictEnd` is omitted: it guards reading MINMATCH
            // bytes off the end of a dictionary, and with no dictionary it is
            // `(U32)((BASE-1) - matchIndex) >= 3`, which is always true for
            // every index at or above BASE.
            let dist_next_match = self.chain_table[(match_index & 0xFFFF) as usize] as u32;
            if pattern_analysis && dist_next_match == 1 && match_chain_pos == 0 {
                let match_candidate_idx = match_index - 1;
                if repeat == Repeat::Untested {
                    if is_byte_repeat(pattern) {
                        repeat = Repeat::Confirmed;
                        src_pattern_length =
                            count_pattern(src, ip + 4, i_high_limit, pattern as u8) + 4;
                    } else {
                        repeat = Repeat::Not;
                    }
                }
                if repeat == Repeat::Confirmed && match_candidate_idx >= lowest_match_index {
                    let mp = (match_candidate_idx - BASE) as i32;
                    if read32(src, mp) == Some(pattern) {
                        let byte = pattern as u8;
                        let forward_len = count_pattern(src, mp + 4, i_high_limit, byte) + 4;
                        let raw_back = reverse_count_pattern(src, mp, 0, byte);
                        // Clamp so the segment never starts below the window.
                        let back_len = (match_candidate_idx
                            - (match_candidate_idx - raw_back as u32).max(lowest_match_index))
                            as i32;
                        let segment_len = back_len + forward_len;

                        if segment_len >= src_pattern_length && forward_len <= src_pattern_length {
                            // The segment holds a whole source pattern: jump to
                            // where it ends, which may extend into a real match.
                            match_index = match_candidate_idx + forward_len as u32
                                - src_pattern_length as u32;
                        } else {
                            // Otherwise take the far end of the current segment.
                            match_index = match_candidate_idx - back_len as u32;
                            if look_back_length == 0 {
                                let max_ml = segment_len.min(src_pattern_length);
                                if longest < max_ml {
                                    if ip_index - match_index > DISTANCE_MAX {
                                        break;
                                    }
                                    longest = max_ml;
                                    offset = (ip_index - match_index) as i32;
                                }
                                let dist_to_next =
                                    self.chain_table[(match_index & 0xFFFF) as usize] as u32;
                                if dist_to_next > match_index {
                                    break; // avoid overflow
                                }
                                match_index -= dist_to_next;
                            }
                        }
                        continue; // skip the ordinary chain step
                    }
                }
            }

            // Step to the previous position sharing this hash. Note the index
            // is `matchIndex + matchChainPos`, not `matchIndex`: chain_swap may
            // have picked a different position along the match to follow.
            let step_dist =
                self.chain_table[((match_index.wrapping_add(match_chain_pos)) & 0xFFFF) as usize]
                    as u32;
            if step_dist > match_index {
                break; // would underflow; unreachable while match_index >= BASE
            }
            match_index -= step_dist;
        }

        Match { off: offset, len: longest, back: s_back }
    }
}

/// Output cursor over the destination buffer.
struct Out<'a> {
    buf: &'a mut [u8],
    pos: usize,
}

impl<'a> Out<'a> {
    #[inline]
    fn push(&mut self, b: u8) {
        self.buf[self.pos] = b;
        self.pos += 1;
    }
}

/// `LZ4HC_encodeSequence` (`lz4hc.c:268`). Returns `true` on output overflow,
/// which abandons the whole compression (`limitedOutput`).
///
/// Advances `ip` past the match and moves `anchor` to it, as the C does through
/// its `UPDATABLE` macro.
fn encode_sequence(
    src: &[u8],
    out: &mut Out,
    ip: &mut i32,
    anchor: &mut i32,
    match_length: i32,
    offset: i32,
) -> bool {
    assert!(match_length >= MINMATCH);
    assert!(offset > 0 && offset as u32 <= DISTANCE_MAX);

    let lit_len = (*ip - *anchor) as usize;

    // The C reserves the token byte before checking, so the check counts it.
    if out.pos + 1 + lit_len / 255 + lit_len + (2 + 1 + LASTLITERALS as usize) > out.buf.len() {
        return true;
    }

    let token_pos = out.pos;
    out.pos += 1;

    if lit_len >= RUN_MASK as usize {
        let mut len = lit_len - RUN_MASK as usize;
        out.buf[token_pos] = (RUN_MASK as u8) << ML_BITS;
        while len >= 255 {
            out.push(255);
            len -= 255;
        }
        out.push(len as u8);
    } else {
        out.buf[token_pos] = (lit_len as u8) << ML_BITS;
    }

    let a = *anchor as usize;
    out.buf[out.pos..out.pos + lit_len].copy_from_slice(&src[a..a + lit_len]);
    out.pos += lit_len;

    out.push(offset as u8);
    out.push((offset >> 8) as u8);

    let mut ml = (match_length - MINMATCH) as usize;
    if out.pos + ml / 255 + (1 + LASTLITERALS as usize) > out.buf.len() {
        return true;
    }
    if ml >= ML_MASK as usize {
        out.buf[token_pos] += ML_MASK as u8;
        ml -= ML_MASK as usize;
        while ml >= 510 {
            out.push(255);
            out.push(255);
            ml -= 510;
        }
        if ml >= 255 {
            ml -= 255;
            out.push(255);
        }
        out.push(ml as u8);
    } else {
        out.buf[token_pos] += ml as u8;
    }

    *ip += match_length;
    *anchor = *ip;
    false
}

// ---------------------------------------------------------------------------
// lz4opt -- the price-based optimal parser (levels 10-12)
// ---------------------------------------------------------------------------

/// `LZ4_OPT_NUM` (`lz4hc.c:77`) -- how far ahead the parser plans.
const OPT_NUM: usize = 1 << 12;
/// `TRAILING_LITERALS` (`lz4hc.c:1835`).
const TRAILING_LITERALS: usize = 3;

/// One position in the price table (`LZ4HC_optimal_t`, `lz4hc.c:1770`).
#[derive(Clone, Copy, Default)]
struct Opt {
    price: i32,
    off: i32,
    mlen: i32,
    litlen: i32,
}

/// `LZ4HC_literalsPrice` (`lz4hc.c:1778`) -- cost in bytes of a literal run,
/// including the extra length bytes once it exceeds `RUN_MASK`.
///
/// **Measured blind spot, recorded so it is not mistaken for coverage.** The
/// `1 + (litlen - RUN_MASK) / 255` term is the one part of this port the
/// differential harness cannot exercise. Established by sabotage, not assumed:
/// `/255`->`/254`, `>=`->`>`, and even multiplying the whole term by **10** all
/// leave every input byte-identical at every level, while changing
/// `sequence_price`'s token cost is caught on 5 inputs immediately. So the
/// pricing machinery *is* reached; this term simply never decides anything.
///
/// The cause is structural rather than a thin corpus: at any given position all
/// candidate paths share the same `llen`, so a constant added here cancels out
/// of every comparison. It could only fail to cancel in the literal-extension
/// step, where `baseLitlen` would have to land within `MINMATCH` of a 255
/// boundary above 269. Adding inputs with longer literal runs was tried (the
/// `priced` and `competing` corpus files) and did not move it.
///
/// Treat these three lines as verified by transcription against `lz4hc.c` only.
/// If they ever change, re-read the C -- the harness will not tell you.
#[inline]
fn literals_price(litlen: i32) -> i32 {
    let mut price = litlen;
    if litlen >= RUN_MASK {
        price += 1 + (litlen - RUN_MASK) / 255;
    }
    price
}

/// `LZ4HC_sequencePrice` (`lz4hc.c:1788`) -- a full sequence: token, 16-bit
/// offset, the literal run, and any extra match-length bytes.
#[inline]
fn sequence_price(litlen: i32, mlen: i32) -> i32 {
    assert!(mlen >= MINMATCH);
    let mut price = 1 + 2; // token + 16-bit offset
    price += literals_price(litlen);
    if mlen >= ML_MASK + MINMATCH {
        price += 1 + (mlen - (ML_MASK + MINMATCH)) / 255;
    }
    price
}

/// `LZ4MID_HASHLOG` -- one bit smaller than the hash-chain table, because
/// `lz4mid` keeps *two* tables inside the same allocation (`lz4hc.c:142`).
const MID_HASHLOG: u32 = HASH_LOG - 1;
const MID_HASHTABLE_SIZE: usize = 1 << MID_HASHLOG;

/// `LZ4MID_hash4` (`lz4hc.c:145`).
#[inline]
fn mid_hash4(v: u32) -> usize {
    (v.wrapping_mul(2654435761) >> (32 - MID_HASHLOG)) as usize
}

/// `LZ4MID_hash7` (`lz4hc.c:149`) -- hashes the low **56** bits, so the shift
/// by 8 is part of the hash, not an alignment fix.
#[inline]
fn mid_hash8(v: u64) -> usize {
    (((v << 8).wrapping_mul(58295818150454627)) >> (64 - MID_HASHLOG)) as usize
}

#[inline]
fn read64(src: &[u8], p: i32) -> Option<u64> {
    if p < 0 {
        return None;
    }
    let p = p as usize;
    // `get` already guarantees 8 bytes, but say so with `ok()` rather than an
    // `unwrap` -- the None simply cannot arise, and no panic path is emitted.
    src.get(p..p + 8)
        .and_then(|b| b.try_into().ok())
        .map(u64::from_le_bytes)
}

/// `LZ4MID_compressBlock` (`lz4hc.c:529`) -- the strategy the C selects for
/// levels 1-2, with the dictionary paths dropped.
///
/// It is not a shallower hash chain but a different shape: two hash tables (a
/// 4-byte and an 8-byte hash) holding a single most-recent position each, a
/// greedy match with a one-byte lookahead, and backward extension after the
/// fact. That dual hash is why it beats a 4-deep chain despite being faster --
/// clamping levels 1-2 onto the chain matcher measured ~0.9% *worse*.
fn compress_mid(src: &[u8], out: &mut Out) -> Option<usize> {
    let input_size = src.len() as i32;
    let iend = input_size;
    let mflimit = iend - MFLIMIT;
    let matchlimit = iend - LASTLITERALS;
    // `ilimit` (lz4hc.c:539) bounds where an 8-byte hash may still be read.
    let ilimit_idx = (iend - 8) as u32 + BASE;

    let mut hash4 = vec![0u32; MID_HASHTABLE_SIZE];
    let mut hash8 = vec![0u32; MID_HASHTABLE_SIZE];

    let mut ip = 0i32;
    let mut anchor = 0i32;

    macro_rules! addpos8 {
        ($p:expr, $idx:expr) => {
            match read64(src, $p) {
                Some(v) => {
                    hash8[mid_hash8(v)] = $idx;
                }
                None => {}
            }
        };
    }
    macro_rules! addpos4 {
        ($p:expr, $idx:expr) => {
            match read32(src, $p) {
                Some(v) => {
                    hash4[mid_hash4(v)] = $idx;
                }
                None => {}
            }
        };
    }

    'outer: while ip <= mflimit {
        let ip_index = ip as u32 + BASE;
        let mut match_length;
        let mut match_distance;

        // A zero-filled slot reads as index 0, which is more than
        // LZ4_DISTANCE_MAX below any real index -- that is what rejects
        // never-written slots, so no separate emptiness test is needed.
        'found: {
            // Long match first.
            match read64(src, ip) {
                Some(v) => {
                    let h8 = mid_hash8(v);
                    let pos8 = hash8[h8];
                    hash8[h8] = ip_index;
                    if ip_index - pos8 <= DISTANCE_MAX {
                        let m = (pos8 - BASE) as i32;
                        match_length = count_forward(src, ip, m, matchlimit);
                        if match_length >= MINMATCH {
                            match_distance = (ip_index - pos8) as i32;
                            break 'found;
                        }
                    }
                }
                None => {}
            }
            // Then a short match, with a one-byte lookahead for a longer one.
            match read32(src, ip) {
                Some(v) => {
                    let h4 = mid_hash4(v);
                    let pos4 = hash4[h4];
                    hash4[h4] = ip_index;
                    if ip_index - pos4 <= DISTANCE_MAX {
                        let m = (pos4 - BASE) as i32;
                        match_length = count_forward(src, ip, m, matchlimit);
                        if match_length >= MINMATCH {
                            match_distance = (ip_index - pos4) as i32;
                            match read64(src, ip + 1) {
                                Some(v2) => {
                                    let h8 = mid_hash8(v2);
                                    let pos8 = hash8[h8];
                                    let m2_distance = ip_index + 1 - pos8;
                                    if m2_distance <= DISTANCE_MAX && ip < mflimit {
                                        let m2 = (pos8 - BASE) as i32;
                                        let ml2 = count_forward(src, ip + 1, m2, matchlimit);
                                        if ml2 > match_length {
                                            hash8[h8] = ip_index + 1;
                                            ip += 1;
                                            match_length = ml2;
                                            match_distance = m2_distance as i32;
                                        }
                                    }
                                }
                                None => {}
                            }
                            break 'found;
                        }
                    }
                }
                None => {}
            }
            // No match: step forward, accelerating over incompressible data.
            ip += 1 + ((ip - anchor) >> 9);
            continue 'outer;
        }

        // Catch back (lz4hc.c:672): extend the match leftwards.
        while ip > anchor
            && ip > match_distance
            && src[(ip - 1) as usize] == src[(ip - match_distance - 1) as usize]
        {
            ip -= 1;
            match_length += 1;
        }

        // These use `ip_index` as captured at the TOP of the loop, which the
        // catch-back above (and the `ip += 1` lookahead before it) may have left
        // stale relative to `ip`. That is what the C does -- it never recomputes
        // `ipIndex` here (`lz4hc.c:677-679`) -- and reproducing the staleness is
        // required for identical output: recomputing it diverges on exactly the
        // repetitive inputs where catch-back actually moves `ip`.
        addpos8!(ip + 1, ip_index + 1);
        addpos8!(ip + 2, ip_index + 2);
        addpos4!(ip + 1, ip_index + 1);

        if encode_sequence(src, out, &mut ip, &mut anchor, match_length, match_distance) {
            return None;
        }

        // Fill the tables with the end of the match.
        let end_idx = ip as u32 + BASE;
        if end_idx - 2 < ilimit_idx {
            if ip > 5 {
                addpos8!(ip - 5, end_idx - 5);
            }
            addpos8!(ip - 3, end_idx - 3);
            addpos8!(ip - 2, end_idx - 2);
            addpos4!(ip - 2, end_idx - 2);
            addpos4!(ip - 1, end_idx - 1);
        }
    }

    Some(anchor as usize)
}

impl HashChain {
    /// `LZ4HC_FindLongerMatch` (`lz4hc.c:1801`) -- the optimal parser's only
    /// entry into the match finder. `iLowLimit == ip`, so it never searches
    /// past `ip` and `back` is always 0; `patternAnalysis` and `chainSwap` are
    /// both hardcoded on here, unlike the hash-chain parser.
    fn find_longer_match(
        &mut self,
        src: &[u8],
        ip: i32,
        i_high_limit: i32,
        min_len: i32,
        nb_searches: i32,
    ) -> Match {
        let md = self.get_wider_match(src, ip, ip, i_high_limit, min_len, nb_searches, true, true);
        if md.len <= min_len {
            return NO_MATCH;
        }
        // The `favorDecSpeed` shortcut that would clamp 19..36 to 18 is absent:
        // it is unreachable from DArc (see get_wider_match).
        md
    }
}

/// `LZ4HC_compress_optimal` (`lz4hc.c:1820`) -- levels 10-12.
///
/// Rather than committing to a match as it finds one, this prices every
/// position up to `OPT_NUM` ahead, keeps the cheapest way to reach each, then
/// walks the table backwards to recover the cheapest path and emits it. Prices
/// are in bytes of output, so the optimum it finds is a genuine size optimum
/// for the sequences it considered.
///
/// `full_update` is `cLevel >= LZ4HC_CLEVEL_MAX` (`lz4hc.c:1409`), i.e. level 12
/// only; it makes the parser search at positions it would otherwise skip.
///
/// Returns the final `anchor`, or `None` on output overflow.
#[allow(clippy::too_many_arguments)]
fn compress_optimal(
    src: &[u8],
    out: &mut Out,
    ctx: &mut HashChain,
    nb_searches: i32,
    mut sufficient_len: i32,
    full_update: bool,
) -> Option<i32> {
    let iend = src.len() as i32;
    let mflimit = iend - MFLIMIT;
    let matchlimit = iend - LASTLITERALS;

    if sufficient_len >= OPT_NUM as i32 {
        sufficient_len = OPT_NUM as i32 - 1;
    }

    // C sizes this OPT_NUM + TRAILING_LITERALS. The extra MINMATCH is slack for
    // the `opt[cur + MINMATCH]` lookahead in the full_update test: the C's own
    // invariants keep that in range, but an out-of-range index here would be a
    // panic across the C ABI rather than a stray read, so it is bought cheaply.
    let mut opt = vec![Opt::default(); OPT_NUM + TRAILING_LITERALS + MINMATCH as usize];

    let mut ip = 0i32;
    let mut anchor = 0i32;

    'main: while ip <= mflimit {
        let llen = ip - anchor;
        let first_match = ctx.find_longer_match(src, ip, matchlimit, MINMATCH - 1, nb_searches);
        if first_match.len == 0 {
            ip += 1;
            continue;
        }

        if first_match.len > sufficient_len {
            // Good enough: encode it immediately rather than pricing a path.
            if encode_sequence(src, out, &mut ip, &mut anchor, first_match.len, first_match.off) {
                return None;
            }
            continue;
        }

        // Seed prices: the first MINMATCH positions as pure literals ...
        for r_pos in 0..MINMATCH {
            let cost = literals_price(llen + r_pos);
            let o = &mut opt[r_pos as usize];
            o.mlen = 1;
            o.off = 0;
            o.litlen = llen + r_pos;
            o.price = cost;
        }
        // ... then every length of the first match.
        for mlen in MINMATCH..=first_match.len {
            let cost = sequence_price(llen, mlen);
            let o = &mut opt[mlen as usize];
            o.mlen = mlen;
            o.off = first_match.off;
            o.litlen = llen;
            o.price = cost;
        }
        let mut last_match_pos = first_match.len;
        for add_lit in 1..=TRAILING_LITERALS as i32 {
            let base = opt[last_match_pos as usize].price;
            let o = &mut opt[(last_match_pos + add_lit) as usize];
            o.mlen = 1;
            o.off = 0;
            o.litlen = add_lit;
            o.price = base + literals_price(add_lit);
        }

        // Extend the price table position by position.
        let mut best_mlen = 0i32;
        let mut best_off = 0i32;
        let mut cur = 0i32;
        let mut jumped = false;

        let mut c = 1i32;
        while c < last_match_pos {
            let cur_ptr = ip + c;
            if cur_ptr > mflimit {
                break;
            }
            // No point searching here if the next position is already at least
            // as cheap. Level 12 additionally allows a short match through when
            // cost rises sharply just after.
            if full_update {
                if opt[(c + 1) as usize].price <= opt[c as usize].price
                    && opt[(c + MINMATCH) as usize].price < opt[c as usize].price + 3
                {
                    c += 1;
                    continue;
                }
            } else if opt[(c + 1) as usize].price <= opt[c as usize].price {
                c += 1;
                continue;
            }

            let min_len = if full_update { MINMATCH - 1 } else { last_match_pos - c };
            let new_match = ctx.find_longer_match(src, cur_ptr, matchlimit, min_len, nb_searches);
            if new_match.len == 0 {
                c += 1;
                continue;
            }

            if new_match.len > sufficient_len || new_match.len + c >= OPT_NUM as i32 {
                best_mlen = new_match.len;
                best_off = new_match.off;
                last_match_pos = c + 1;
                cur = c;
                jumped = true;
                break;
            }

            // Before the match: price the leading literals.
            let base_litlen = opt[c as usize].litlen;
            for litlen in 1..MINMATCH {
                let price = opt[c as usize].price - literals_price(base_litlen)
                    + literals_price(base_litlen + litlen);
                let pos = (c + litlen) as usize;
                if price < opt[pos].price {
                    opt[pos].mlen = 1;
                    opt[pos].off = 0;
                    opt[pos].litlen = base_litlen + litlen;
                    opt[pos].price = price;
                }
            }

            // Then every length of the match found at this position.
            for ml in MINMATCH..=new_match.len {
                let pos = (c + ml) as usize;
                let (ll, price) = if opt[c as usize].mlen == 1 {
                    let ll = opt[c as usize].litlen;
                    let prev = if c > ll { opt[(c - ll) as usize].price } else { 0 };
                    (ll, prev + sequence_price(ll, ml))
                } else {
                    (0, opt[c as usize].price + sequence_price(0, ml))
                };
                // `- (int)favorDecSpeed` is `- 0` here.
                if pos as i32 > last_match_pos + TRAILING_LITERALS as i32
                    || price <= opt[pos].price
                {
                    if ml == new_match.len && last_match_pos < pos as i32 {
                        last_match_pos = pos as i32;
                    }
                    opt[pos].mlen = ml;
                    opt[pos].off = new_match.off;
                    opt[pos].litlen = ll;
                    opt[pos].price = price;
                }
            }

            for add_lit in 1..=TRAILING_LITERALS as i32 {
                let base = opt[last_match_pos as usize].price;
                let o = &mut opt[(last_match_pos + add_lit) as usize];
                o.mlen = 1;
                o.off = 0;
                o.litlen = add_lit;
                o.price = base + literals_price(add_lit);
            }

            c += 1;
        }

        if !jumped {
            best_mlen = opt[last_match_pos as usize].mlen;
            best_off = opt[last_match_pos as usize].off;
            cur = last_match_pos - best_mlen;
        }

        // Reverse traversal: rewrite each visited slot with the sequence that
        // the cheapest path actually takes from it.
        {
            let mut candidate_pos = cur;
            let mut selected_ml = best_mlen;
            let mut selected_off = best_off;
            loop {
                let next_ml = opt[candidate_pos as usize].mlen;
                let next_off = opt[candidate_pos as usize].off;
                opt[candidate_pos as usize].mlen = selected_ml;
                opt[candidate_pos as usize].off = selected_off;
                selected_ml = next_ml;
                selected_off = next_off;
                if next_ml > candidate_pos {
                    break; // first match to encode reached
                }
                if next_ml <= 0 {
                    break; // C asserts this cannot happen; do not spin if it does
                }
                candidate_pos -= next_ml;
            }
        }

        // Emit the recovered path in order.
        let mut r_pos = 0i32;
        while r_pos < last_match_pos {
            let ml = opt[r_pos as usize].mlen;
            let offset = opt[r_pos as usize].off;
            if ml == 1 {
                ip += 1;
                r_pos += 1;
                continue; // literal; several may run together
            }
            r_pos += ml;
            if encode_sequence(src, out, &mut ip, &mut anchor, ml, offset) {
                return None;
            }
        }
        continue 'main;
    }

    Some(anchor)
}

/// Search depth for a compression level, from `k_clTable` (`lz4hc.c:92-106`).
///
/// Which strategy and parameters a compression level selects, transcribed from
/// `k_clTable` (`lz4hc.c:92-106`). All three are ported, so every level DArc
/// accepts reproduces the C exactly.
///
/// A level below 1 means "default", which is 9; anything above 12 is clamped to
/// 12 (`lz4hc.c:112-114`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Strategy {
    /// Levels 1-2.
    Mid,
    /// Levels 3-9, carrying `nbSearches`.
    HashChain(i32),
    /// Levels 10-12, carrying `nbSearches`, `targetLength` and `fullUpdate`.
    Optimal(i32, i32, bool),
}

fn level_params(level: i32) -> Strategy {
    let level = if level < 1 { 9 } else { level.min(12) };
    match level {
        1 | 2 => Strategy::Mid,
        3 => Strategy::HashChain(4),
        4 => Strategy::HashChain(8),
        5 => Strategy::HashChain(16),
        6 => Strategy::HashChain(32),
        7 => Strategy::HashChain(64),
        8 => Strategy::HashChain(128),
        9 => Strategy::HashChain(256),
        10 => Strategy::Optimal(96, 64, false),
        11 => Strategy::Optimal(512, 128, false),
        // `fullUpdate` is `cLevel >= LZ4HC_CLEVEL_MAX` (lz4hc.c:1409), so only
        // level 12 gets it. targetLength is LZ4_OPT_NUM, clamped inside.
        _ => Strategy::Optimal(16384, OPT_NUM as i32, true),
    }
}

/// `LZ4_compressBound` (`lz4.h`) -- the exact bound `C_LZ4.cpp:66` sizes its
/// output buffer with.
pub fn compress_bound(n: usize) -> usize {
    n + n / 255 + 16
}

/// High-compression encode of one block, mirroring `LZ4_compress_HC`.
///
/// Returns the compressed length, or `0` when the result does not fit in `dst`
/// -- which `C_LZ4.cpp` treats as "store this block raw", not as an error.
pub fn compress_hc(src: &[u8], dst: &mut [u8], level: i32) -> usize {
    if src.len() > i32::MAX as usize {
        return 0;
    }
    let mut out = Out { buf: dst, pos: 0 };
    let input_size = src.len() as i32;
    let strategy = level_params(level);
    // Exhaustive on purpose: a wildcard here would silently give a new strategy
    // `max_nb_attempts = 0`, and `pattern_analysis` below is derived from it, so
    // the parse would change without any error. clippy::wildcard_enum_match_arm
    // is denied at the crate root to keep it that way.
    let max_nb_attempts = match strategy {
        Strategy::HashChain(n) => n,
        // The mid and optimal parsers do not use a hash-chain attempt budget;
        // lz4hc.c reaches their loops without consulting nbSearches this way.
        Strategy::Mid | Strategy::Optimal(..) => 0,
    };
    // `patternAnalysis` (lz4hc.c:1133) is tied to the search depth, not the
    // level number: "levels 9+". (The optimal parser hardcodes it on instead.)
    let pattern_analysis = max_nb_attempts > 128;

    let iend = input_size;
    let mflimit = iend - MFLIMIT;
    let matchlimit = iend - LASTLITERALS;

    let mut ip = 0i32;
    let mut anchor = 0i32;

    // Exhaustive on (long enough, which parser). This was an `if let` on a
    // tuple plus two `else if`s, which meant a NEW `Strategy` variant fell
    // silently into the hash-chain branch -- the same silent-fallback class as
    // the Tornado presets 7-11 bug. Now it is a compile error.
    match (input_size >= LZ4_MIN_LENGTH, strategy) {
        (true, Strategy::Mid) => {
            match compress_mid(src, &mut out) {
                Some(a) => anchor = a as i32,
                None => return 0,
            }
        }
        (true, Strategy::Optimal(nb, target, full)) => {
            let mut ctx = HashChain::new();
            match compress_optimal(src, &mut out, &mut ctx, nb, target, full) {
                Some(a) => anchor = a,
                None => return 0,
            }
        }
        (true, Strategy::HashChain(_)) => {
            let mut ctx = HashChain::new();

            'main: while ip <= mflimit {
                let mut m1 = ctx.get_wider_match(src, ip, ip, matchlimit, MINMATCH - 1, max_nb_attempts, pattern_analysis, false);
                if m1.len < MINMATCH {
                    ip += 1;
                    continue;
                }

                // Saved, in case the parser later decides it skipped too far.
                let mut start0 = ip;
                let mut m0 = m1;
                let mut start2 = ip;
                let mut m2 = NO_MATCH;
                let mut start3;
                let mut m3;

                // The C threads this section with `goto _Search2` / `goto _Search3`;
                // `state` reproduces those jumps exactly.
                let mut state = 2u8;
                loop {
                    if state == 2 {
                        // ---- _Search2 (lz4hc.c:1165) ----
                        if ip + m1.len <= mflimit {
                            start2 = ip + m1.len - 2;
                            m2 = ctx.get_wider_match(src, start2, ip, matchlimit, m1.len, max_nb_attempts, pattern_analysis, false);
                            start2 += m2.back;
                        } else {
                            m2 = NO_MATCH; // do not search further
                        }

                        if m2.len <= m1.len {
                            // No better match => encode ML1 immediately.
                            if encode_sequence(src, &mut out, &mut ip, &mut anchor, m1.len, m1.off) {
                                return 0;
                            }
                            continue 'main;
                        }

                        if start0 < ip && start2 < ip + m0.len {
                            // Squeezing ML1 between ML0 and ML2: restore Match1.
                            ip = start0;
                            m1 = m0;
                        }

                        if start2 - ip < 3 {
                            // First match too small: drop it.
                            ip = start2;
                            m1 = m2;
                            continue;
                        }
                        state = 3;
                        continue;
                    }

                    // ---- _Search3 (lz4hc.c:1198) ----
                    if start2 - ip < OPTIMAL_ML {
                        let mut new_ml = m1.len.min(OPTIMAL_ML);
                        if ip + new_ml > start2 + m2.len - MINMATCH {
                            new_ml = (start2 - ip) + m2.len - MINMATCH;
                        }
                        let correction = new_ml - (start2 - ip);
                        if correction > 0 {
                            start2 += correction;
                            m2.len -= correction;
                        }
                    }

                    if start2 + m2.len <= mflimit {
                        start3 = start2 + m2.len - 3;
                        m3 = ctx.get_wider_match(src, start3, start2, matchlimit, m2.len, max_nb_attempts, pattern_analysis, false);
                        start3 += m3.back;
                    } else {
                        start3 = start2;
                        m3 = NO_MATCH;
                    }

                    if m3.len <= m2.len {
                        // No better match => encode ML1 and ML2.
                        if start2 < ip + m1.len {
                            m1.len = start2 - ip;
                        }
                        if encode_sequence(src, &mut out, &mut ip, &mut anchor, m1.len, m1.off) {
                            return 0;
                        }
                        ip = start2;
                        if encode_sequence(src, &mut out, &mut ip, &mut anchor, m2.len, m2.off) {
                            return 0;
                        }
                        continue 'main;
                    }

                    if start3 < ip + m1.len + 3 {
                        if start3 >= ip + m1.len {
                            // Seq1 can be written now; Seq2 goes away and Seq3
                            // becomes the new Seq1.
                            if start2 < ip + m1.len {
                                let correction = ip + m1.len - start2;
                                start2 += correction;
                                m2.len -= correction;
                                if m2.len < MINMATCH {
                                    start2 = start3;
                                    m2 = m3;
                                }
                            }
                            if encode_sequence(src, &mut out, &mut ip, &mut anchor, m1.len, m1.off) {
                                return 0;
                            }
                            ip = start3;
                            m1 = m3;
                            start0 = start2;
                            m0 = m2;
                            state = 2;
                            continue;
                        }
                        start2 = start3;
                        m2 = m3;
                        continue; // state stays 3
                    }

                    // Three ascending matches; write the first.
                    if start2 < ip + m1.len {
                        if start2 - ip < OPTIMAL_ML {
                            if m1.len > OPTIMAL_ML {
                                m1.len = OPTIMAL_ML;
                            }
                            if ip + m1.len > start2 + m2.len - MINMATCH {
                                m1.len = (start2 - ip) + m2.len - MINMATCH;
                            }
                            let correction = m1.len - (start2 - ip);
                            if correction > 0 {
                                start2 += correction;
                                m2.len -= correction;
                            }
                        } else {
                            m1.len = start2 - ip;
                        }
                    }
                    if encode_sequence(src, &mut out, &mut ip, &mut anchor, m1.len, m1.off) {
                        return 0;
                    }

                    // ML2 becomes ML1, ML3 becomes ML2; look for a new ML3.
                    ip = start2;
                    m1 = m2;
                    start2 = start3;
                    m2 = m3;
                    // state stays 3
                }
            }
        }
        // Shorter than LZ4_MIN_LENGTH: no matches are emitted at all, and the
        // tail below stores the whole input as literals. Spelled out per
        // variant rather than `(false, _)` so a new strategy lands here too.
        (false, Strategy::Mid)
        | (false, Strategy::HashChain(_))
        | (false, Strategy::Optimal(..)) => {}
    }

    // ---- Encode last literals (lz4hc.c:1308) ----
    let last_run = (iend - anchor) as usize;
    let ll_add = (last_run + 255 - RUN_MASK as usize) / 255;
    if out.pos + 1 + ll_add + last_run > out.buf.len() {
        return 0;
    }
    if last_run >= RUN_MASK as usize {
        let mut acc = last_run - RUN_MASK as usize;
        out.push((RUN_MASK as u8) << ML_BITS);
        while acc >= 255 {
            out.push(255);
            acc -= 255;
        }
        out.push(acc as u8);
    } else {
        out.push((last_run as u8) << ML_BITS);
    }
    let a = anchor as usize;
    out.buf[out.pos..out.pos + last_run].copy_from_slice(&src[a..a + last_run]);
    out.pos += last_run;

    out.pos
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n)
            .map(|_| {
                s = s.wrapping_mul(1103515245).wrapping_add(12345);
                (s >> 16) as u8
            })
            .collect()
    }

    /// Every HC block must decode, through the same decoder DArc ships.
    fn round_trip(src: &[u8], level: i32) -> usize {
        let mut enc = vec![0u8; compress_bound(src.len())];
        let n = compress_hc(src, &mut enc, level);
        assert!(n > 0, "compress_hc returned 0 for {} bytes", src.len());
        let mut dec = vec![0u8; src.len()];
        let m = crate::lz4::decompress_block(&enc[..n], &mut dec).expect("decode HC block");
        assert_eq!(m, src.len(), "decoded length differs at level {level}");
        assert_eq!(dec, src, "decoded bytes differ at level {level}");
        n
    }

    #[test]
    fn round_trips_every_level() {
        let corpus: Vec<Vec<u8>> = vec![
            b"the quick brown fox jumps over the lazy dog. ".repeat(3000),
            b"\x5a".repeat(80_000),
            b"\x00\xff".repeat(40_000),
            prng(9, 200_000),
            (0..200_000u32).map(|i| (i % 256) as u8).collect(),
        ];
        for level in [1, 2, 3, 5, 9, 12] {
            for src in &corpus {
                round_trip(src, level);
            }
        }
    }

    /// Sizes around the parsing limits: below `LZ4_MIN_LENGTH`, straddling
    /// `MFLIMIT`, and just past the first indexable position.
    #[test]
    fn round_trips_boundary_sizes() {
        for len in [0usize, 1, 4, 12, 13, 14, 15, 16, 17, 63, 64, 65, 254, 255, 256, 4096] {
            let src: Vec<u8> = (0..len).map(|i| (i * 7 % 251) as u8).collect();
            round_trip(&src, 9);
            let runs: Vec<u8> = (0..len).map(|i| (i / 7 % 3) as u8).collect();
            round_trip(&runs, 9);
        }
    }

    /// Offsets are 16 bits, so a match further back than 65535 must not be
    /// emitted. An input longer than one window with a repeat exactly at the
    /// boundary is the case that catches an off-by-one there.
    #[test]
    fn round_trips_across_the_64k_window() {
        let mut src = prng(3, 70_000);
        let head: Vec<u8> = src[..2000].to_vec();
        src.extend_from_slice(&head);
        round_trip(&src, 9);
        round_trip(&src, 3);
    }

    /// The whole point of HC is that it compresses *better* than the fast
    /// encoder. A port that merely round-trips could be emitting all literals.
    #[test]
    fn beats_the_fast_encoder() {
        let src: Vec<u8> = b"compression algorithms rearrange data so that \
                             statistical redundancy can be removed by an entropy coder. "
            .repeat(900);
        let mut fast = vec![0u8; compress_bound(src.len())];
        let fast_n = crate::lz4::compress_block(&src, &mut fast).expect("fast encode");
        let hc_n = round_trip(&src, 9);
        assert!(
            hc_n < fast_n,
            "HC ({hc_n}) should beat the fast encoder ({fast_n})"
        );
    }

    /// A tight output buffer must return 0 ("store raw"), never write past the
    /// end and never panic.
    #[test]
    fn tight_output_buffer_returns_zero() {
        let src = prng(11, 50_000); // incompressible
        for cap in [0usize, 1, 8, 64, 1024, src.len() / 2] {
            let mut out = vec![0u8; cap];
            let n = compress_hc(&src, &mut out, 9);
            assert!(n <= cap, "wrote {n} into a {cap}-byte buffer");
        }
    }
}
