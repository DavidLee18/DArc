//! Dict encoder, ported from Compression/Dict/dict.cpp (phases 1-7).
//!
//! This is the half where three of the defects fixed on the previous branch
//! lived: phase1 read past the front of the block, phase2 double-freed after a
//! zero-size realloc, and phase7's FindWord read one byte past the end. Porting
//! it replaces patched C with code where those cannot be expressed.
//!
//! Bit-exactness is mandatory -- the dictionary and the encoded text are the
//! archive format. Two things make that harder here than for Delta or LZP:
//!
//!   * phase1 is a trie mapped onto a closed-hash table, searched
//!     speculatively in WORD_STEP jumps and then refined backwards. The C is
//!     written with six `goto` labels; the control flow is reproduced here with
//!     an explicit enum rather than being "tidied", because which branch runs
//!     decides which words enter the dictionary.
//!   * phases 3, 4 and 6 sort with `qsort` on comparators that tie constantly,
//!     and the tie order decides which words fall either side of the one-byte /
//!     two-byte split. C output is byte-identical on macOS/ARM64 and
//!     Linux/x86-64, so two unrelated qsort implementations agree and the order
//!     within a tie does not reach the output; a stable sort is therefore safe.
//!     The differential harness is what confirms that, not this comment.

use crate::ffi::FREEARC_ERRCODE_GENERAL;
use core::ffi::c_int;

const MAX_WORD_LEN: usize = 254;
const USE_DICT2: u32 = 1;
const RESERVED_CHAR: u8 = b' ';
const WORD_STEP: usize = 4;
const DIRECT_CHARS: usize = 12;
const SCNT_MAX: i32 = i16::MAX as i32;
const UCHAR_MAX: usize = 255;

/// `MIN_VISITS_TO_HAVE_SON(len)`
#[inline]
fn min_visits(len: usize) -> i32 {
    if len > 10 { 2 } else { 5 }
}

/// `char_class_table`: control characters and space form one class, everything
/// else the other. Transcribed from the `#if 1` variant that is actually
/// compiled -- the file carries three alternatives.
#[inline]
fn char_class(c: u8) -> u8 {
    if c <= 32 { 0 } else { 1 }
}

#[inline]
fn allow_extend(c1: u8, c2: u8) -> bool {
    char_class(c1) == char_class(c2)
}

#[inline]
fn update_hash(hash: u32, c: u8) -> u32 {
    hash.wrapping_mul(137).wrapping_add(c as u32).wrapping_add(219)
}

#[inline]
fn rehash(hash: u32, c: u8) -> u32 {
    hash.wrapping_add((c as u32).wrapping_mul(256)).wrapping_add(317)
}

fn roundup_to_power_of_2(mut n: u32) -> u32 {
    let mut r = 1u32;
    while r < n {
        r = r.wrapping_mul(2);
        if r == 0 {
            return u32::MAX;
        }
    }
    n = r;
    n
}

#[derive(Clone, Copy, Default)]
struct Stats {
    count: i16,
    hash0: u16,
}

/// Mirrors the C `Word` union: `{hash, hash0}` while the dictionary is being
/// built, `{count, chr, chr2}` after phase2 prunes it. Kept as one struct with
/// both sets, since nothing reads the stale half.
#[derive(Clone, Copy, Default)]
struct Word {
    at: usize,   // offset of the word text in the input block
    len: u32,
    hash: u32,
    hash0: u32,
    count: i32,
    chr: u8,
    chr2: u8,
}

/// Outcome of the `SEARCH_IN_HASH` macro. The macro mutates `hash` as it
/// rehashes, so the updated value comes back with the verdict.
enum Search {
    Found(u32),
    LongChain(u32),
    Empty(u32),
}

/// The initial two-byte lookup is written as
///     SEARCH_IN_HASH (hash0, hash0, hash0, 0, found2, end);
/// where `hash`, `phash` and `c` are all the SAME variable. Because the macro
/// assigns `hash = rehash(hash, c)`, every rehash changes the comparison target
/// and the rehash input too -- they are not fixed at their entry values the way
/// a function call would fix them. Modelling this as an ordinary call silently
/// searches a different chain.
fn search_in_hash_aliased(scan: &[Stats], mask: u32, mut hash: u32) -> Search {
    let mut n = 13;
    loop {
        let h = scan[(hash & mask) as usize].hash0;
        if h == 0 {
            return Search::Empty(hash);
        }
        if h == hash as u16 {
            return Search::Found(hash);
        }
        n -= 1;
        if n == 0 {
            return Search::LongChain(hash);
        }
        hash = rehash(hash, hash as u8);
    }
}

fn search_in_hash(scan: &[Stats], mask: u32, mut hash: u32, phash: u32, c: u8) -> Search {
    let mut n = 13;
    loop {
        let h = scan[(hash & mask) as usize].hash0;
        if h == 0 {
            return Search::Empty(hash);
        }
        if h == phash as u16 {
            return Search::Found(hash);
        }
        n -= 1;
        if n == 0 {
            return Search::LongChain(hash);
        }
        hash = rehash(hash, c);
    }
}

impl Encoder {
    pub fn new() -> Self {
        Encoder { text: Vec::new(), hashmask: 0, hashbits: Vec::new(), codewords: Vec::new(), words: Vec::new(), scan: Vec::new(), mask: 0, max_words: 0,
                  char_counts: [0; 256], prefix_for_weak_chars: 0 }
    }
    pub fn prefix(&self) -> u8 { self.prefix_for_weak_chars }

    /// Words after phase2/phase3, matching the C dumper's "W <at> <len> <count>".
    pub fn dump_words(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("words {}\n", self.words.len()));
        for w in &self.words {
            out.push_str(&format!("W {} {} {}\n", w.at, w.len, w.count));
        }
        out
    }

    /// Words after phase4, matching "W <at> <len> <count> <chr> <chr2>".
    pub fn dump_coded_words(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("words {}\n", self.words.len()));
        for w in &self.words {
            out.push_str(&format!("W {} {} {} {} {}\n", w.at, w.len, w.count, w.chr, w.chr2));
        }
        out
    }

    pub fn dump_char_counts(&self) -> String {
        let mut out = String::new();
        for (c, n) in self.char_counts.iter().enumerate() {
            if *n != 0 { out.push_str(&format!("C {} {}\n", c, n)); }
        }
        out
    }

    /// Dump phase1's result in the same format as rust/difftest/dict_phase1_ref.cpp,
    /// so the two can be diffed directly.
    pub fn dump_phase1(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("words {}\n", self.words.len()));
        for w in &self.words {
            out.push_str(&format!("W {} {} {} {}\n", w.at, w.len, w.hash, w.hash0));
        }
        for (c, n) in self.char_counts.iter().enumerate() {
            if *n != 0 { out.push_str(&format!("C {} {}\n", c, n)); }
        }
        out
    }
}

impl Default for Encoder {
    fn default() -> Self { Self::new() }
}

pub struct Encoder {
    /// Copy of the block being encoded; words are (offset, len) into it.
    text: Vec<u8>,
    hashmask: u32,
    hashbits: Vec<u16>,
    codewords: Vec<CodeWord>,
    words: Vec<Word>,
    scan: Vec<Stats>,
    mask: u32,
    max_words: usize,
    char_counts: [i32; 256],
    prefix_for_weak_chars: u8,
}

impl Encoder {
    fn add_word(&mut self, at: usize, len: usize, hash: u32, phash: u32) {
        // ADDWORD: dictionary full, word too long, or a hash whose low 16 bits
        // are zero (which would collide with "empty slot") are all skipped.
        if self.words.len() < self.max_words && len <= MAX_WORD_LEN && (hash as u16) != 0 {
            let h = (hash & self.mask) as usize;
            let ph = (phash & self.mask) as usize;
            self.scan[ph].count = self.scan[ph].count.wrapping_add(1);
            self.scan[h].count = 1;
            self.scan[h].hash0 = phash as u16;
            self.words.push(Word { at, len: len as u32, hash: h as u32, hash0: ph as u32, ..Default::default() });
        }
    }

    /// phase1: build the word list and count byte frequencies.
    pub fn phase1(&mut self, buf: &[u8]) {
        self.text = buf.to_vec();
        let bufsize = buf.len();
        let max_words = roundup_to_power_of_2(core::cmp::max(bufsize as u32 / 32, 32768)) as usize;
        self.max_words = max_words;
        let scanhash_size = max_words * 2;
        self.mask = (scanhash_size - 1) as u32;
        self.scan = vec![Stats::default(); scanhash_size];
        // Slot zero, and every slot at a multiple of 2^16, must never be usable
        // as a word: hash0==0 is the "empty" marker.
        let mut i = 0usize;
        while i < scanhash_size {
            self.scan[i].hash0 = 1;
            i += 1 << 16;
        }

        let mut p = 0usize;
        // bufsize is unsigned in C and this subtraction runs off the front for
        // a very short block; clamped, as the C was after the fix.
        let endbuf = if bufsize > WORD_STEP + 1 { bufsize - WORD_STEP - 1 } else { 0 };

        // Trace hook for the per-phase comparison; the lookup is hoisted so the
        // scan loop pays nothing when it is unset.
        let trace_at: Option<usize> = std::env::var("DARC_TRACE_AT").ok()
            .and_then(|v| v.parse::<usize>().ok());

        while p < endbuf {
            let p0 = p;
            let tracing = trace_at == Some(p0);
            let mut c1 = buf[p];
            p += 1;
            let mut c = buf[p];
            'word: {
                if !allow_extend(c1, c) {
                    break 'word;
                }
                p += 1;
                let mut hash0 = ((c1 as u32) << 8) + c as u32 + 16;

                // SEARCH_IN_HASH takes `hash` by reference in effect: the macro
                // assigns `hash = rehash(hash, c)` on the caller's variable, so
                // hash0 holds the REHASHED value afterwards. Both the ADDWORD
                // below and the found2 loop's `hash = hash0` then use it.
                // Discarding it and reusing the original hash0 stores the word
                // at the unrehashed slot -- C writes "W 21739 2 57816 57816"
                // where this port wrote 17819, one rehash behind.
                match search_in_hash_aliased(&self.scan, self.mask, hash0) {
                    Search::Found(h) => hash0 = h,            // -> found2
                    Search::LongChain(_) => break 'word,      // -> end, no word added
                    Search::Empty(h) => {                     // fall through -> ADDWORD, end
                        hash0 = h;
                        self.add_word(p0, p - p0, hash0, hash0);
                        break 'word;
                    }
                }

                // found2: grow the word until it leaves the dictionary.
                loop {
                    let mut hash = hash0;
                    let mut hash1 = hash0;
                    let mut i = 0usize;
                    let mut hit_bad_char = false;
                    while i < WORD_STEP {
                        c1 = c;
                        c = buf[p + i];
                        if !allow_extend(c1, c) {
                            hit_bad_char = true;
                            break;
                        }
                        hash1 = hash;
                        hash = update_hash(hash1, c);
                        i += 1;
                    }

                    // Which label the C would jump to.
                    enum Next { NextCycle(u32), FoundMax(u32), SearchLess(u32) }
                    let next = if hit_bad_char {
                        // search_max
                        if i > 0 {
                            match search_in_hash(&self.scan, self.mask, hash, hash1, c1) {
                                Search::Found(h) => Next::FoundMax(h),
                                Search::LongChain(h) => Next::FoundMax(h),
                                Search::Empty(h) => Next::SearchLess(h),
                            }
                        } else {
                            Next::FoundMax(hash)
                        }
                    } else {
                        match search_in_hash(&self.scan, self.mask, hash, hash1, c1) {
                            Search::Found(h) => Next::NextCycle(h),
                            Search::LongChain(h) => Next::FoundMax(h),
                            Search::Empty(h) => Next::SearchLess(h),
                        }
                    };

                    if tracing {
                        eprintln!("TRACE p0={} i={} hash={} hash1={} branch={}", p0, i, hash, hash1,
                                  match &next { Next::NextCycle(_) => "next_cycle",
                                                Next::FoundMax(_) => "found_max",
                                                Next::SearchLess(_) => "search_less" });
                    }
                    match next {
                        Next::NextCycle(h) => {
                            p += i;
                            let idx = (h & self.mask) as usize;
                            let counter = self.scan[idx].count as i32;
                            if counter >= min_visits(p - p0) {
                                hash0 = h;
                                if p < endbuf { continue; } else { break; }
                            }
                            if counter > 0 {
                                self.scan[idx].count = (counter + 1) as i16;
                                break;
                            }
                            self.add_word(p0, p - p0, h, hash1);
                            break;
                        }
                        Next::FoundMax(h) => {
                            p += i;
                            let idx = (h & self.mask) as usize;
                            let counter = self.scan[idx].count as i32;
                            if counter <= 0 {
                                self.add_word(p0, p - p0, h, hash1);
                                break;
                            }
                            if counter < SCNT_MAX - 1 {
                                self.scan[idx].count = (counter + 1) as i16;
                            }
                            break;
                        }
                        Next::SearchLess(h) => {
                            // The maximum-length word is absent, so walk up from
                            // the start looking for the longest one present.
                            //
                            // UNRESOLVED: the C jumps to the outer `found_max:`
                            // label on a long hash chain, and because C reuses
                            // `i` and the labels share scope it is ambiguous
                            // from reading alone whether that means the outer
                            // `hash`/`hash1` or the inner ones. Mirroring the
                            // outer reading made agreement *worse* (5/11 rather
                            // than 7/11 inputs), so this keeps the inner
                            // reading, which is closer but still not exact.
                            // See the phase1 status note in the commit message.
                            let maxi = i;
                            let mut h0 = hash0;
                            let mut h3 = hash0;
                            let mut h2 = h;
                            let mut settled = false;
                            let mut j = 1usize;
                            while j < maxi {
                                let cc = buf[p];
                                p += 1;
                                let h1 = update_hash(h0, cc);
                                match search_in_hash(&self.scan, self.mask, h1, h0, cc) {
                                    // SEARCH_IN_HASH mutates h2 as it rehashes;
                                    // `next:` then does h0=h1, h3=h2 with that value.
                                    Search::Found(hh) => { h2 = hh; h0 = h1; h3 = hh; }
                                    Search::LongChain(hh) => {
                                        let idx = (hh & self.mask) as usize;
                                        let counter = self.scan[idx].count as i32;
                                        if counter <= 0 {
                                            self.add_word(p0, p - p0, hh, h0);
                                        } else if counter < SCNT_MAX - 1 {
                                            self.scan[idx].count = (counter + 1) as i16;
                                        }
                                        settled = true;
                                        h2 = hh;
                                        break;
                                    }
                                    Search::Empty(hh) => { h2 = hh; p -= 1; settled = true; break; }
                                }
                                j += 1;
                            }
                            if !settled {
                                // C's `h2 = hash` after the loop uses the OUTER
                                // hash -- which SEARCH_IN_HASH already rehashed
                                // on its way to search_less. `hash` here is the
                                // pre-search value; the rehashed one arrives as
                                // the SearchLess payload.
                                h2 = h;
                            }
                            let idx = (h3 & self.mask) as usize;
                            let counter = self.scan[idx].count as i32;
                            if tracing {
                                eprintln!("TRACE search_less p0={} maxi={} h3={} idx={} counter={} min={} len={} h2={} h0={} settled={}",
                                          p0, maxi, h3, idx, counter, min_visits(p - p0), p - p0, h2, h0, settled);
                            }
                            if counter >= min_visits(p - p0) {
                                p += 1;
                                self.add_word(p0, p - p0, h2, h0);
                            } else {
                                self.scan[idx].count = (counter + 1) as i16;
                            }
                            break;
                        }
                    }
                }
            }
            // end: count the bytes consumed by this position
            let mut q = p0;
            while q < p {
                self.char_counts[buf[q] as usize] += 1;
                q += 1;
            }
        }
        while p < bufsize {
            self.char_counts[buf[p] as usize] += 1;
            p += 1;
        }
    }
}

impl Encoder {
    /// phase2: promote single-child counters, then prune words that do not earn
    /// their place. Returns Err when nothing survives, exactly as the C returns
    /// -1 and lets DictEncode store the block instead.
    pub fn phase2(&mut self, min_large: i32, min_medium: i32, min_small: i32, min_ratio: i32) -> Result<(), c_int> {
        // Hand a single-child parent's counter down to the child. A parent with
        // exactly the minimum-plus-one, or a negative count, has only one child.
        for i in 0..self.words.len() {
            let w = self.words[i];
            let len = w.len as usize;
            let cnt = self.scan[w.hash as usize].count as i32;
            let cnt0 = self.scan[w.hash0 as usize].count as i32;
            if cnt0 == min_visits(len - 1) + 1 || cnt0 < 0 {
                self.scan[w.hash0 as usize].count = 0;
                let sumcnt = core::cmp::min(cnt.abs() + cnt0.abs(), SCNT_MAX);
                // A negative count marks "this one also has a single child", so
                // its own child can claim the total later.
                self.scan[w.hash as usize].count =
                    if cnt == min_visits(len) + 1 { (-sumcnt) as i16 } else { sumcnt as i16 };
            }
        }

        // Walk backwards handing bad children's counters to their parents, and
        // compact the survivors to the end of the array.
        let mut kept: Vec<Word> = Vec::new();
        for i in (0..self.words.len()).rev() {
            let w = self.words[i];
            let cnt = (self.scan[w.hash as usize].count as i32).abs();
            let cnt0 = (self.scan[w.hash0 as usize].count as i32).abs();
            // GOOD_WORD(cnt,cnt0,len)
            let good = cnt > min_large
                || if cnt0 != 0 { cnt > min_medium && cnt > cnt0 * min_ratio } else { cnt > min_small };
            if good {
                let mut k = w;
                k.count = cnt;
                kept.push(k);
            } else {
                self.scan[w.hash0 as usize].count = core::cmp::min(cnt + cnt0, SCNT_MAX) as i16;
            }
        }
        kept.reverse(); // built back-to-front, as the C fills downward from LastWord
        self.scan = Vec::new(); // FreeAndNil(scan_hash)
        self.words = kept;
        if self.words.is_empty() { Err(FREEARC_ERRCODE_GENERAL) } else { Ok(()) }
    }

    /// phase3: choose which characters can be spent on word codes, and how many
    /// words get a one-byte code. Returns `nodes`.
    pub fn phase3(&mut self, min_weak_chars: i32) -> Result<usize, c_int> {
        // Words by descending frequency. See the note at the top of this file on
        // why a stable sort is safe here.
        self.words.sort_by(|a, b| b.count.cmp(&a.count));

        // Characters by ascending frequency.
        let mut chars: Vec<(u8, i32)> = (0..=UCHAR_MAX).map(|c| (c as u8, self.char_counts[c])).collect();
        chars.sort_by(|a, b| a.1.cmp(&b.1));

        let mut n = 0usize;
        while n < self.words.len() && n <= UCHAR_MAX {
            if chars[n].1 >= self.words[n].count {
                break;
            }
            self.char_counts[chars[n].0 as usize] = 0; // conditionally free
            n += 1;
        }
        if n as i32 <= min_weak_chars {
            return Err(FREEARC_ERRCODE_GENERAL); // most likely a binary file
        }

        // The last freed character becomes the prefix for characters whose codes
        // were given away.
        n -= 1;
        let c = chars[n].0;
        self.char_counts[c as usize] = 1;
        self.prefix_for_weak_chars = c;

        let avail = n;
        let word_count = self.words.len();
        let nodes = if word_count <= avail {
            core::cmp::min(word_count, avail)
        } else {
            core::cmp::max(avail as i64 - ((word_count + 259) / 256) as i64, 0) as usize
        };
        Ok(nodes)
    }
}

impl Encoder {
    /// phase4: hand out one- and two-byte codes.
    pub fn phase4(&mut self, nodes: usize) -> Result<(), c_int> {
        // One-byte and two-byte groups are sorted lexicographically separately,
        // which compresses the dictionary itself better.
        let n = self.words.len();
        let split = core::cmp::min(nodes, n);
        let key = |w: &Word, buf: &[u8]| (buf[w.at..w.at + w.len as usize].to_vec(), w.len, w.at);
        let buf = self.text.clone();
        self.words[..split].sort_by_key(|w| key(w, &buf));
        self.words[split..].sort_by_key(|w| key(w, &buf));

        let mut p = 0usize;              // next word awaiting a code
        let two_byte_start = split;

        // One-byte codes for the most useful words.
        let mut c = 0usize;
        while c <= UCHAR_MAX && p < two_byte_start {
            if self.char_counts[c] == 0 && c as u8 != RESERVED_CHAR {
                self.words[p].chr = c as u8;
                self.words[p].chr2 = RESERVED_CHAR;
                p += 1;
            }
            c += 1;
        }

        // Two-byte codes for the rest.
        while c <= UCHAR_MAX && p < self.words.len() {
            if self.char_counts[c] != 0 || c as u8 == RESERVED_CHAR {
                c += 1;
                continue;
            }
            let mut c2 = 0usize;
            while c2 <= UCHAR_MAX && p < self.words.len() {
                if c2 as u8 == RESERVED_CHAR {
                    c2 += 1;
                    continue;
                }
                // GOOD_2BYTE_WORD(len,cnt) is (len>=4): shorter words are not
                // worth two bytes, so they are dropped by zeroing their count.
                while p < self.words.len() && self.words[p].len < 4 {
                    self.words[p].count = 0;
                    p += 1;
                }
                if p < self.words.len() {
                    self.words[p].chr = c as u8;
                    self.words[p].chr2 = c2 as u8;
                    p += 1;
                }
                c2 += 1;
            }
            c += 1;
        }
        self.words.truncate(p);

        // Any remaining free characters go to words that turned down a two-byte
        // code above.
        let mut q = two_byte_start;
        while c <= UCHAR_MAX && q < self.words.len() {
            if self.char_counts[c] == 0 && c as u8 != RESERVED_CHAR {
                self.words[q].chr = c as u8;
                self.words[q].chr2 = RESERVED_CHAR;
                q += 1;
            }
            c += 1;
        }

        // Drop the words that never got a code: sort by descending count and
        // cut at the first zero.
        self.words.sort_by(|a, b| b.count.cmp(&a.count).then(a.at.cmp(&b.at)));
        let keep = self.words.iter().position(|w| w.count == 0).unwrap_or(self.words.len());
        self.words.truncate(keep);
        Ok(())
    }
}

impl Encoder {
    /// phase5: serialise the dictionary. This is the exact format `dict::decode`
    /// reads, which is a useful cross-check: the decoder was ported first and
    /// independently.
    pub fn phase5(&mut self) -> Result<Vec<u8>, c_int> {
        const N: usize = UCHAR_MAX + 1;
        // dict[i]: Some(word index) for a one-byte code, None for unused.
        let mut dict: Vec<Option<usize>> = vec![None; N];
        let mut dict_is_two: Vec<bool> = vec![false; N];
        let mut dict2: Vec<Option<usize>> = vec![None; N * N];
        let mut char_in_use = [false; N];

        for (i, w) in self.words.iter().enumerate() {
            if w.chr2 == RESERVED_CHAR {
                dict[w.chr as usize] = Some(i);
            } else {
                dict2[w.chr as usize * N + w.chr2 as usize] = Some(i);
                dict_is_two[w.chr as usize] = true;
                for k in 0..w.len as usize {
                    char_in_use[self.text[w.at + k] as usize] = true;
                }
            }
        }

        // Separator: the highest character that appears in no two-byte word.
        let mut word_sep = None;
        for c in (0..N).rev() {
            if !char_in_use[c] {
                word_sep = Some(c as u8);
                break;
            }
        }
        // Every character consumed by words leaves no separator available, and
        // the dictionary cannot be expressed.
        let word_sep = word_sep.ok_or(FREEARC_ERRCODE_GENERAL)?;

        let mut out: Vec<u8> = Vec::new();
        let len_of = |i: usize, dict: &Vec<Option<usize>>, two: &Vec<bool>, me: &Encoder| -> u8 {
            if two[i] { USE_DICT2 as u8 } else { dict[i].map(|k| me.words[k].len as u8).unwrap_or(0) }
        };

        // 1. lengths of the one-byte-coded words
        for i in 0..N {
            out.push(len_of(i, &dict, &dict_is_two, self));
        }
        // 2. common-prefix lengths of the two-byte-coded words
        let mut prev: Option<usize> = None;
        for i in 0..N {
            if dict_is_two[i] {
                for j in 0..N {
                    let cur = dict2[i * N + j];
                    let n = match (cur, prev) {
                        (Some(a), Some(b)) => self.common_prefix_length(a, b),
                        _ => 0,
                    };
                    out.push(n as u8);
                    prev = cur;
                }
            }
        }
        // 3. text of the one-byte words
        for i in 0..N {
            if dict_is_two[i] {
                continue;
            }
            match dict[i] {
                Some(k) => {
                    let w = self.words[k];
                    out.extend_from_slice(&self.text[w.at..w.at + w.len as usize]);
                }
                None => {}
            }
        }
        // 4. separator, then the two-byte words minus their shared prefix
        prev = None;
        out.push(word_sep);
        for i in 0..N {
            if dict_is_two[i] {
                for j in 0..N {
                    let cur = dict2[i * N + j];
                    let n = match (cur, prev) {
                        (Some(a), Some(b)) => self.common_prefix_length(a, b),
                        _ => 0,
                    };
                    match cur {
                        Some(k) => {
                            let w = self.words[k];
                            out.extend_from_slice(&self.text[w.at + n..w.at + w.len as usize]);
                        }
                        None => {}
                    }
                    out.push(word_sep);
                    prev = cur;
                }
            }
        }
        // 5. the prefix used for characters that gave their code to a word
        out.push(self.prefix_for_weak_chars);
        Ok(out)
    }

    fn common_prefix_length(&self, a: usize, b: usize) -> usize {
        let wa = self.words[a];
        let wb = self.words[b];
        let n = core::cmp::min(wa.len, wb.len) as usize;
        let mut i = 0;
        while i < n && self.text[wa.at + i] == self.text[wb.at + i] {
            i += 1;
        }
        i
    }
}

/// One entry of the encoding hash built by phase6.
#[derive(Clone, Default)]
struct CodeWord {
    text: Vec<u8>,
    len: u8,
    chr: u8,
    chr2: u8,
}

impl Encoder {
    /// phase6: build the hash used to find the longest dictionary word at a
    /// position. FindWord in phase7 must reproduce this hashing exactly or words
    /// simply stop being found -- the C says as much in a comment.
    pub fn phase6(&mut self) -> Result<(), c_int> {
        let buf = self.text.clone();
        let key = |w: &Word| (buf[w.at..w.at + w.len as usize].to_vec(), w.len, w.at);
        self.words.sort_by_key(key);

        // Hash size is driven by the number of distinct bytes across the words.
        let mut unique_bytes: u32 = 0;
        for i in 0..self.words.len() {
            let cp = if i == 0 { 0 } else { self.common_prefix_length(i, i - 1) };
            unique_bytes += self.words[i].len - cp as u32;
        }
        let hashsize = roundup_to_power_of_2(unique_bytes.saturating_mul(4)).max(1) as usize;
        self.hashmask = (hashsize - 1) as u32;
        self.hashbits = vec![0u16; hashsize];
        self.codewords = vec![CodeWord::default(); hashsize];

        for wi in 0..self.words.len() {
            let w = self.words[wi];
            let mut hash = hashsize as u32 + self.text[w.at] as u32;
            let mut longest: Option<CodeWord> = None;
            let mut abandoned = false;
            for i in 1..w.len as usize {
                let c = self.text[w.at + i];
                let hash0 = hash;
                hash = update_hash(hash, c);
                let mut n = 13;
                while self.hashbits[(hash & self.hashmask) as usize] != 0
                    && self.hashbits[(hash & self.hashmask) as usize] != hash0 as u16
                {
                    n -= 1;
                    if n == 0 {
                        abandoned = true;
                        break;
                    }
                    hash = rehash(hash, c);
                }
                if abandoned {
                    break;
                }
                self.hashbits[(hash & self.hashmask) as usize] = hash0 as u16;

                // Carry the longest prefix word along the chain, so a lookup that
                // stops here still yields the best encodable word.
                let idx = (hash & self.hashmask) as usize;
                if self.codewords[idx].len != 0 {
                    longest = Some(self.codewords[idx].clone());
                } else {
                    match &longest {
                        Some(l) => {
                            self.codewords[idx] = l.clone();
                        }
                        None => {}
                    }
                }
            }
            if abandoned {
                continue;
            }
            let idx = (hash & self.hashmask) as usize;
            self.codewords[idx] = CodeWord {
                text: self.text[w.at..w.at + w.len as usize].to_vec(),
                len: w.len as u8,
                chr: w.chr,
                chr2: w.chr2,
            };
        }
        Ok(())
    }

    /// Port of `FindWord`: longest dictionary word starting at `p0`.
    fn find_word(&self, p0: usize, endbuf: usize) -> Option<&CodeWord> {
        if p0 >= endbuf {
            return None;
        }
        let mut p = p0;
        let mut hash0 = self.hashbits.len() as u32 + self.text[p] as u32;
        p += 1;
        while p < endbuf {
            let c = self.text[p];
            p += 1;
            let mut hash = update_hash(hash0, c);
            let mut n = 13;
            let mut found = false;
            loop {
                let h = self.hashbits[(hash & self.hashmask) as usize];
                if h == 0 {
                    break;
                }
                if h == hash0 as u16 {
                    found = true;
                    break;
                }
                hash = rehash(hash, c);
                n -= 1;
                if n == 0 {
                    break;
                }
            }
            if !found {
                break;
            }
            hash0 = hash;
        }
        p -= 1;
        let word = &self.codewords[(hash0 & self.hashmask) as usize];
        let len = word.len as usize;
        if len == 0 || len > endbuf - p {
            return None;
        }
        if self.text[p0..p0 + len] != word.text[..len] {
            return None;
        }
        Some(word)
    }

    /// phase7: encode the text with the dictionary.
    pub fn phase7(&self) -> Vec<u8> {
        let endbuf = self.text.len();
        let mut out: Vec<u8> = Vec::new();
        let mut p = 0usize;
        while p < endbuf {
            match self.find_word(p, endbuf) {
                Some(w) => {
                    out.push(w.chr);
                    if w.chr2 != RESERVED_CHAR {
                        out.push(w.chr2);
                    }
                    p += w.len as usize;
                }
                None => {
                    let c = self.text[p];
                    p += 1;
                    // A character whose code was given away must be escaped.
                    if self.char_counts[c as usize] == 0 || c == self.prefix_for_weak_chars {
                        out.push(self.prefix_for_weak_chars);
                    }
                    out.push(c);
                }
            }
        }
        out
    }
}

/// Run all seven phases. `Err` means "this block is not worth dict-encoding",
/// which the caller turns into a stored block, exactly as the C does when
/// DictEncode returns non-zero.
#[allow(clippy::too_many_arguments)]
pub fn encode_block(buf: &[u8], min_weak_chars: i32, min_large: i32, min_medium: i32,
                    min_small: i32, min_ratio: i32) -> Result<Vec<u8>, c_int> {
    let mut e = Encoder::new();
    e.phase1(buf);
    e.phase2(min_large, min_medium, min_small, min_ratio)?;
    let nodes = e.phase3(min_weak_chars)?;
    e.phase4(nodes)?;
    let mut out = e.phase5()?;
    e.phase6()?;
    out.extend_from_slice(&e.phase7());
    Ok(out)
}

/// Port of `dict_compress`: block framing around `encode_block`.
#[allow(clippy::too_many_arguments)]
pub fn compress(io: &crate::ffi::Io, block_size: u32, min_compression: c_int, min_weak_chars: c_int,
                min_large: c_int, min_medium: c_int, min_small: c_int, min_ratio: c_int) -> c_int {
    use crate::ffi::{FREEARC_ERRCODE_IO, OK};
    let block_size = block_size.max(1) as usize;
    let mut inbuf = vec![0u8; block_size];
    loop {
        let got = io.read(&mut inbuf);
        if got < 0 { return got; }
        if got == 0 { return OK; }
        let in_size = got as usize;

        let encoded = encode_block(&inbuf[..in_size], min_weak_chars, min_large, min_medium, min_small, min_ratio);
        // The store test is integer arithmetic in the C and is reproduced as
        // written: OutSize/MinCompression >= InSize/100, truncation included.
        let store = match &encoded {
            Err(_) => true,
            Ok(out) => min_compression > 0 && out.len() / min_compression as usize >= in_size / 100,
        };
        if store {
            if io.write(&(-(in_size as i32)).to_le_bytes()) < 0 || io.write(&inbuf[..in_size]) < 0 {
                return FREEARC_ERRCODE_IO;
            }
        } else {
            // `store` is true for `Err`, so this branch only runs on `Ok` -- but
            // matched rather than unwrapped, so that relationship is stated
            // instead of assumed.
            match &encoded {
                Ok(out) => {
                    if io.write(&(out.len() as u32).to_le_bytes()) < 0 || io.write(out) < 0 {
                        return FREEARC_ERRCODE_IO;
                    }
                }
                Err(_) => return FREEARC_ERRCODE_GENERAL,
            }
        }
    }
}
