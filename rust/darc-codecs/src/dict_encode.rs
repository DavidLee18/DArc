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
        Encoder { words: Vec::new(), scan: Vec::new(), mask: 0, max_words: 0,
                  char_counts: [0; 256], prefix_for_weak_chars: 0 }
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

        while p < endbuf {
            let p0 = p;
            let mut c1 = buf[p];
            p += 1;
            let mut c = buf[p];
            'word: {
                if !allow_extend(c1, c) {
                    break 'word;
                }
                p += 1;
                let mut hash0 = ((c1 as u32) << 8) + c as u32 + 16;

                match search_in_hash(&self.scan, self.mask, hash0, hash0, hash0 as u8) {
                    Search::Found(_) => {}                    // -> found2
                    Search::LongChain(_) => break 'word,      // -> end, no word added
                    Search::Empty(_) => {                     // fall through -> ADDWORD, end
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
                                h2 = hash;
                            }
                            let idx = (h3 & self.mask) as usize;
                            let counter = self.scan[idx].count as i32;
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

/// Entry point placeholder: the remaining phases (2-7) are not ported yet, so
/// this deliberately reports failure rather than producing a wrong archive.
pub fn encode(_buf: &[u8]) -> Result<Vec<u8>, c_int> {
    Err(FREEARC_ERRCODE_GENERAL)
}
