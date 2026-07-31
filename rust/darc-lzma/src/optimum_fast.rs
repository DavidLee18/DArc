// `GetOptimumFast` — the greedy/lazy parser used when `algo == 0`
// (`fastMode`). Included into `encoder.rs` (shares the private `Encoder`), the
// same way `optimum_dp.rs` is. A faithful port of LzmaEnc.c:1970-2095, plus the
// `ChangePair` macro at LzmaEnc.c:1966.
//
// Unlike `get_optimum`, this parser writes nothing into `self.opt` and never
// touches the price tables: it decides one op per call from the match finder's
// output alone, and returns its length with `back_res` set. `LzmaEnc_InitPrices`
// skips `FillDistancesPrices`/`FillAlignPrices` entirely in fast mode
// (LzmaEnc.c:2835), and `CodeOneBlock` skips the periodic price refresh
// (LzmaEnc.c:2635) — so the price tables must stay unread on this path, and
// nothing here reads them.
//
// Bit-exactness hazards, all of which a "reasonable" rewrite would get wrong:
//
//   * The rep scan (LzmaEnc.c:1994-2013) returns on the **first** rep index whose
//     length reaches `numFastBytes`, and otherwise keeps the longest with a
//     strict `>` (LzmaEnc.c:2008). Both facts encode a preference for the lower
//     rep index; scanning all four and taking a max with `>=` would pick a
//     different rep and change the encoded distance.
//   * That scan has **no** `len == LZMA_MATCH_LEN_MAX` early break. `GetOptimum`
//     does (LzmaEnc.c:1271, "21.03 : optimization") but `GetOptimumFast` does
//     not, and adding one here would skip the `len >= numFastBytes` return for a
//     later rep.
//   * `numAvail` is clamped to `LZMA_MATCH_LEN_MAX` (LzmaEnc.c:1989) *before* it
//     is used both as the rep-scan limit and in the `numAvail <= 2` cutoff
//     (LzmaEnc.c:2053). The unclamped value survives in `self.num_avail`; only
//     the clamped local is used below.
//   * `kMatchMinLen` is 2 throughout: `mainLen >= 2` / `repLen >= 2` /
//     `len1 >= 2` are all "is this a match at all", and `mainLen = 1` at
//     LzmaEnc.c:2040 is how a too-far 2-byte match is *demoted to a literal*
//     without touching `backRes` (still `MARK_LIT` from LzmaEnc.c:1985).
//   * The `repLen + 1 >= mainLen` / `+ 2` / `+ 3` ladder (LzmaEnc.c:2044-2046)
//     and the lazy-match ladder (LzmaEnc.c:2063-2066) use `>=` where a naive
//     reading expects `>`. They are transcribed verbatim. In particular
//     `Compression/LZMA/readme` claims a local modification to a
//     `len + 1 >= lenMain` comparison: that describes the *other*, uncompiled
//     C++ encoder under `Compression/LZMA/7zip/`, which the makefile never
//     builds. This port follows `7z24/LzmaEnc.c` only.
//   * The final rep re-scan (LzmaEnc.c:2073-2087) compares against
//     `limit = mainLen - 1` with `len >= limit` *before* the byte test, so a
//     `mainLen` of 2 gives `limit == 1` and returns a literal on the very first
//     iteration for any rep whose first two bytes match.

impl<'a> Encoder<'a> {
    /// `ChangePair` (LzmaEnc.c:1966): is `big_dist` more than 128x `small_dist`?
    /// Note the asymmetry — `>` on the shifted value, not `>=`.
    #[inline]
    fn change_pair(small_dist: u32, big_dist: u32) -> bool {
        (big_dist >> 7) > small_dist
    }

    /// `GetOptimumFast` (LzmaEnc.c:1970). Chooses one op, sets `self.back_res`
    /// (`MARK_LIT`, a rep index `0..4`, or `dist + NUM_REPS`), advances the match
    /// finder past the op's tail, and returns the op's length.
    fn get_optimum_fast(&mut self) -> u32 {
        // LzmaEnc.c:1976-1982. `read_match_distances` sets `self.num_pairs`, so
        // reading it afterwards covers both of the C's branches: the
        // `additionalOffset == 0` one (C's out-param) and the resumed one
        // (C's `numPairs = p->numPairs`, whose value came from the
        // `ReadMatchDistances(p, &p->numPairs)` at LzmaEnc.c:2057).
        let mut main_len = if self.additional_offset == 0 {
            self.read_match_distances()
        } else {
            self.longest_match_len
        };
        let mut num_pairs = self.num_pairs;

        // LzmaEnc.c:1984-1990.
        self.back_res = MARK_LIT;
        if self.num_avail < 2 {
            return 1;
        }
        // LzmaEnc.c:1988 keeps a disabled `mainLen < 2 && state == 0` early exit
        // ("18.06.notused"). Not ported — it is commented out in the C.
        let mut num_avail = self.num_avail;
        if num_avail > MATCH_LEN_MAX {
            num_avail = MATCH_LEN_MAX;
        }

        // LzmaEnc.c:1991 — `GetPointerToCurrentPos(..) - 1`, i.e. `parse_index`,
        // *not* `emit_index`. Taken after the `read_match_distances` above, which
        // may have slid the window.
        let data = self.parse_index();

        // LzmaEnc.c:1992-2013: scan the four rep distances.
        let mut rep_len = 0u32;
        let mut rep_index = 0usize;
        for i in 0..NUM_REPS {
            let r = self.reps[i] as usize;
            if self.win()[data] != self.win()[data - r]
                || self.win()[data + 1] != self.win()[data + 1 - r]
            {
                continue;
            }
            let mut len = 2usize;
            while (len as u32) < num_avail && self.win()[data + len] == self.win()[data + len - r] {
                len += 1;
            }
            let len = len as u32;
            if len >= self.num_fast_bytes {
                // LzmaEnc.c:2002-2007: good enough, take it and stop.
                self.back_res = i as u32;
                self.move_pos(len - 1);
                return len;
            }
            if len > rep_len {
                rep_index = i;
                rep_len = len;
            }
        }

        // LzmaEnc.c:2015-2020: the main match is long enough on its own.
        if main_len >= self.num_fast_bytes {
            self.back_res = self.matches[num_pairs as usize - 1] + NUM_REPS as u32;
            self.move_pos(main_len - 1);
            return main_len;
        }

        // LzmaEnc.c:2022-2041: trade length for a much nearer distance. Walking
        // back down the pair list one pair at a time, a shorter match is accepted
        // whenever its distance is under 1/128th of the current one.
        let mut main_dist = 0u32; // C: "for GCC", never read unless main_len >= 2
        if main_len >= 2 {
            main_dist = self.matches[num_pairs as usize - 1];
            while num_pairs > 2 {
                // `num_pairs` is even and > 2, so `- 4` / `- 3` are in range.
                if main_len != self.matches[num_pairs as usize - 4] + 1 {
                    break;
                }
                let dist2 = self.matches[num_pairs as usize - 3];
                if !Self::change_pair(dist2, main_dist) {
                    break;
                }
                num_pairs -= 2;
                main_len -= 1;
                main_dist = dist2;
            }
            // LzmaEnc.c:2039-2040: a 2-byte match beyond 128 is not worth coding.
            if main_len == 2 && main_dist >= 0x80 {
                main_len = 1;
            }
        }

        // LzmaEnc.c:2043-2051. The C nests two `if`s with the braces on the inner
        // one, so this is a single conjunction. All three `>=` are as written.
        if rep_len >= 2
            && (rep_len + 1 >= main_len
                || (rep_len + 2 >= main_len && main_dist >= (1 << 9))
                || (rep_len + 3 >= main_len && main_dist >= (1 << 15)))
        {
            self.back_res = rep_index as u32;
            self.move_pos(rep_len - 1);
            return rep_len;
        }

        // LzmaEnc.c:2053-2054. `back_res` is still `MARK_LIT`: the only writes
        // above (LzmaEnc.c:2004, :2017, :2048) all return immediately.
        if main_len < 2 || num_avail <= 2 {
            return 1;
        }

        // LzmaEnc.c:2056-2069: the lazy step. Look one byte ahead; if the match
        // starting there is better, emit a literal now and let the next call take
        // it. This second read leaves `additional_offset == 2`, which is why the
        // next entry to this function takes the `longest_match_len` branch.
        {
            let len1 = self.read_match_distances();
            self.longest_match_len = len1;

            if len1 >= 2 {
                let new_dist = self.matches[self.num_pairs as usize - 1];
                if (len1 >= main_len && new_dist < main_dist)
                    || (len1 == main_len + 1 && !Self::change_pair(main_dist, new_dist))
                    || (len1 > main_len + 1)
                    || (len1 + 1 >= main_len
                        && main_len >= 3
                        && Self::change_pair(new_dist, main_dist))
                {
                    return 1;
                }
            }
        }

        // LzmaEnc.c:2071 — recomputed, and again `- 1` (`parse_index`) even though
        // `additional_offset` is now 2. `emit_index` here would address the byte
        // one earlier and silently produce a parse whose matches the data does not
        // support.
        let data = self.parse_index();

        // LzmaEnc.c:2073-2087: if a rep at the *next* position runs nearly as far
        // as the main match, prefer a literal now.
        for i in 0..NUM_REPS {
            let r = self.reps[i] as usize;
            if self.win()[data] != self.win()[data - r]
                || self.win()[data + 1] != self.win()[data + 1 - r]
            {
                continue;
            }
            // main_len >= 2 (checked above), so `- 1` does not wrap.
            let limit = main_len - 1;
            let mut len = 2u32;
            loop {
                // LzmaEnc.c:2082 — the limit test precedes the byte test.
                if len >= limit {
                    return 1;
                }
                if self.win()[data + len as usize] != self.win()[data + len as usize - r] {
                    break;
                }
                len += 1;
            }
        }

        // LzmaEnc.c:2089-2094. `move_pos` is by `main_len - 2`, not `- 1`: the
        // lazy read at LzmaEnc.c:2057 already consumed one position. The C guards
        // the call with `mainLen != 2` and `move_pos` is a no-op at 0 anyway, but
        // the guard is kept so the shapes line up.
        self.back_res = main_dist + NUM_REPS as u32;
        if main_len != 2 {
            self.move_pos(main_len - 2);
        }
        main_len
    }
}
