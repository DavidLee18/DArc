//! `-m0`, the in-memory REP matcher — `Compression/SREP/compress_inmem.cpp`.
//!
//! Unlike `-m3`/`-m4` this does not index every chunk. It splits the input into
//! `L`-byte blocks and, per block, keeps only the position whose rolling hash is
//! the **local maximum**. That single position per block is what goes in the
//! table, so the index is `L` times smaller and matches are found by anchoring
//! on those maxima and then extending in both directions.
//!
//! # It runs against a dictionary, not a block
//!
//! `srep.cpp:373` forces `dictsize = DEFAULT_DICTSIZE` (512 MB) when `-m0` is
//! selected without `-d`, and `MAX_DIST` is that size. In the C the dictionary
//! is a *ring* that the background reader overwrites, which is what all the
//! modular arithmetic around `DataStart` is for.
//!
//! Here the whole input is already in memory, so the ring never wraps and
//! `match < i` always, which makes `match_distance` just `i - match`. The
//! `MAX_DIST` cap is still applied, so a file larger than the dictionary rejects
//! the same far matches the C's ring would have forgotten.
//!
//! `DataStart` does **not** collapse to `bufstart`, though — see
//! [`InMem::set_bufsize`]. The ring the reader allocates is bigger than
//! `MAX_DIST`, and that difference decides how far a match may be extended
//! backwards.
//!
//! # Its parameters are the `-d*` ones, not the main ones
//!
//! `srep.cpp:544` constructs it with `dict_min_match` as `MIN_MATCH` and
//! `dict_chunk` as `L` — 512 and 64 by default, **not** the 512/512 that
//! `compress()` uses. Passing the main `L` here produces a table 8x too coarse
//! and silently worse matches.

use super::matches::{self, MatchTooShort};
use super::rolling::{RollingHash, PRIME1};

/// `DEFAULT_DICTSIZE` (`srep.cpp:227`) — 512 MB.
pub const DEFAULT_DICTSIZE: u64 = 512 << 20;

/// `DictionaryCompressor` (`compress_inmem.cpp:8`).
pub struct InMem {
    /// `L` — the block size over which a local maximum is chosen. `dict_chunk`.
    l: usize,
    /// `MIN_MATCH` — `dict_min_match`.
    min_match: usize,
    /// `BASE_LEN` for `ENCODE_LZ_MATCH`.
    base_len: u32,
    /// `MAX_DIST` — the dictionary size the compressor was built with.
    max_dist: u64,
    /// The RING size the background reader actually allocates, which is larger:
    /// `roundUp(inmem_dictsize, bufsize) + BUFFERS*bufsize` (`io.cpp:81`).
    ring: u64,
    /// `hasharr[]`, one position per slot.
    hasharr: Vec<u64>,
    hashmask: u64,
}

impl InMem {
    /// `DictionaryCompressor::DictionaryCompressor()` (`:23`).
    ///
    /// `hashsize` follows the C: `min_hash_size` over the *byte* size of one
    /// entry times the number of blocks the dictionary holds, rounded up to a
    /// power of two. With the defaults that is a 128 MB table, which is what the
    /// C allocates too.
    pub fn new(dictsize: u64, l: usize, min_match: usize, base_len: u32) -> Self {
        let entry = std::mem::size_of::<u64>() as u64;
        let want = super::hash_table::min_hash_size(entry * (dictsize / l as u64));
        let hashsize = super::hash_table::roundup_to_power_of_2(want);
        let slots = (hashsize / entry) as usize;
        InMem {
            l,
            min_match,
            base_len,
            max_dist: dictsize,
            // Filled in by `set_bufsize`, which the driver must call: the ring
            // depends on the block size, and DataStart depends on the ring.
            ring: dictsize,
            hasharr: vec![0u64; slots],
            hashmask: slots as u64 - 1,
        }
    }

    /// `io.cpp:81` — the background reader allocates
    /// `roundUp(inmem_dictsize, bufsize) + BUFFERS*bufsize`, and it is THAT
    /// size, not `MAX_DIST`, that `compress()` is handed as `dictsize`.
    ///
    /// The difference is not cosmetic. `DataStart` is
    /// `(bufstart + dictsize - MAX_DIST) % dictsize`, so the extra
    /// `BUFFERS*bufsize` moves it from `bufstart` to `bufstart + 2*bufsize` --
    /// which puts the whole current block *below* it, and so sends every
    /// in-block match down the `LowBound = i - match` branch that can walk back
    /// to the start of the dictionary. Treating DataStart as `bufstart` instead
    /// caps the walk at the block start and finds measurably shorter matches:
    /// 6 of 55 blocks, 32-64 bytes each, only at small block sizes -- at the
    /// default 8 MB the two formulas happen to agree.
    pub fn set_bufsize(&mut self, bufsize: usize) {
        const BUFFERS: u64 = 2; // io.cpp:49
        let b = bufsize as u64;
        let rounded = self.max_dist.div_ceil(b) * b;
        self.ring = rounded + BUFFERS * b;
    }

    /// `prepare_buffer()` (`:56`) — per `L`-byte block, record the position of
    /// the block's maximum rolling hash.
    ///
    /// Returns `(masked_hash, offset_within_block)` pairs, one per block after
    /// the first. The C also appends `INMEM_PREFETCH*2` zeros so its prefetch
    /// can read ahead; that is a memory hint with no effect on the result and is
    /// not reproduced.
    pub fn prepare_buffer(&self, buf: &[u8]) -> Vec<(u64, usize)> {
        let l = self.l;
        let num_blocks = buf.len() / l;
        let mut out = Vec::new();
        if num_blocks <= 1 {
            return out;
        }
        let mut hash = RollingHash::new(l, PRIME1);
        hash.moveto(buf);
        let mut ptr = 0usize;

        for _block in 1..num_blocks {
            let mut maxhash = hash.value;
            let mut maxi = 0usize;
            for i in 0..l {
                // The comparison precedes the update, and maxhash starts at the
                // hash already at `ptr`, so i == 0 can never win the tie.
                if hash.value > maxhash {
                    maxhash = hash.value;
                    maxi = i;
                }
                hash.update(buf[ptr], buf[ptr + l]);
                ptr += 1;
            }
            out.push((maxhash & self.hashmask, maxi));
        }
        out
    }

    /// `DictionaryCompressor::compress()` (`:83`).
    ///
    /// `whole` is the dictionary — here, the entire input. `block_start` is where
    /// this block begins in it. Appends records to `stat` and returns
    /// `literal_bytes`.
    pub fn compress(
        &mut self,
        whole: &[u8],
        block_start: u64,
        block_len: usize,
        marks: &[(u64, usize)],
        stat: &mut Vec<u32>,
    ) -> Result<usize, MatchTooShort> {
        let l = self.l as u64;
        let mut literal_bytes = block_len;
        let bufstart = block_start;
        let bufend = bufstart + block_len as u64;
        let mut last_match_end = bufstart;
        // `DataStart` -- the first byte that may still be included in a match.
        // See `set_bufsize` for why the ring size matters here.
        let data_start = (bufstart + self.ring - self.max_dist) % self.ring;

        let mut mark = marks.iter();
        let mut last_i = bufstart;
        while last_i + 2 * l <= bufend {
            let (hash, maxi) = match mark.next() {
                Some(m) => *m,
                None => break,
            };
            let i = last_i + maxi as u64;

            // "Only check for a match if the previously found match has ended"
            if i >= last_match_end {
                let m = self.hasharr[hash as usize];
                if m != 0 {
                    let match_distance = i - m; // match < i, the ring never wraps
                    if match_distance <= self.max_dist {
                        // The C's LowBound: how far back the comparison may walk
                        // without the match-side index leaving valid data.
                        let low_bound = match m >= data_start {
                            true => match m - data_start > i {
                                true => 0,
                                false => i - (m - data_start),
                            },
                            false => i - m,
                        };
                        let lo = last_match_end.max(low_bound);
                        let start = find_match_start(whole, m, i, lo);
                        let end = find_match_end(whole, m, i, bufend);
                        let match_len = end - start;
                        let lit_len = start - last_match_end;
                        if match_len >= self.min_match as u64 {
                            matches::encode(
                                stat,
                                // -m0 never rounds: ROUND_MATCHES is a -m3 flag.
                                false,
                                self.base_len,
                                lit_len as u32,
                                match_distance,
                                match_len as u32,
                            )?;
                            literal_bytes -= match_len as usize;
                            last_match_end = end;
                        }
                    }
                }
            }
            // Stored whether or not a match was taken -- the C's `no_match:`
            // label falls through to exactly this.
            self.hasharr[hash as usize] = i;
            last_i += l;
        }
        Ok(literal_bytes)
    }
}

/// `find_match_start()` (`:41`) — walk backwards while the bytes agree.
///
/// `q` is bounded below by `start`; `p` follows it. Returns the first position
/// at which they still matched.
fn find_match_start(d: &[u8], p0: u64, q0: u64, start: u64) -> u64 {
    let (mut p, mut q) = (p0, q0);
    while q > start {
        p -= 1;
        q -= 1;
        if d[p as usize] != d[q as usize] {
            return q + 1;
        }
    }
    q
}

/// `find_match_end()` (`:48`) — walk forwards while the bytes agree.
fn find_match_end(d: &[u8], p0: u64, q0: u64, end: u64) -> u64 {
    let (mut p, mut q) = (p0, q0);
    while q < end && (p as usize) < d.len() && d[p as usize] == d[q as usize] {
        p += 1;
        q += 1;
    }
    q
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n)
            .map(|_| {
                s = s.wrapping_mul(1_103_515_245).wrapping_add(12_345);
                (s >> 16) as u8
            })
            .collect()
    }

    /// A small dictionary keeps the table to a few MB in tests; the shape of the
    /// algorithm does not depend on its size.
    fn small(l: usize, min_match: usize) -> InMem {
        InMem::new(4 << 20, l, min_match, 512)
    }

    #[test]
    fn a_local_maximum_is_recorded_for_every_block_after_the_first() {
        let l = 64usize;
        let im = small(l, 512);
        let buf = prng(1, l * 10);
        let marks = im.prepare_buffer(&buf);
        assert_eq!(marks.len(), buf.len() / l - 1, "one mark per block but the first");
        for (_, maxi) in &marks {
            assert!(*maxi < l, "the maximum must lie inside its block");
        }
    }

    #[test]
    fn a_buffer_shorter_than_two_blocks_yields_no_marks() {
        let l = 64usize;
        let im = small(l, 512);
        assert!(im.prepare_buffer(&prng(2, l)).is_empty());
        assert!(im.prepare_buffer(&prng(2, l * 2 - 1)).is_empty());
    }

    #[test]
    fn a_duplicated_region_is_matched_and_extends_both_ways() {
        let l = 64usize;
        let half = prng(3, 40_000);
        let data: Vec<u8> = half.iter().chain(half.iter()).copied().collect();
        let mut im = small(l, 512);
        let marks = im.prepare_buffer(&data);
        let mut stat = Vec::new();
        let lit = im.compress(&data, 0, data.len(), &marks, &mut stat).expect("ok");
        assert!(!stat.is_empty(), "found no match in an exactly-duplicated buffer");
        assert!(lit < data.len(), "literal count did not shrink");
        // Anchors are local maxima, so the match starts wherever the extension
        // reaches -- it should cover most of the second copy.
        assert!(data.len() - lit > 30_000, "match far shorter than the duplicate");
    }

    #[test]
    fn incompressible_input_produces_no_records() {
        let l = 64usize;
        let data = prng(9, 60_000);
        let mut im = small(l, 512);
        let marks = im.prepare_buffer(&data);
        let mut stat = Vec::new();
        let lit = im.compress(&data, 0, data.len(), &marks, &mut stat).expect("ok");
        assert!(stat.is_empty(), "invented a match in noise");
        assert_eq!(lit, data.len());
    }

    #[test]
    fn a_match_shorter_than_min_match_is_not_recorded() {
        // Two 128-byte duplicates with MIN_MATCH 512: found by the hash, then
        // rejected on length.
        let l = 64usize;
        let piece = prng(5, 128);
        let mut data = Vec::new();
        data.extend_from_slice(&piece);
        data.extend_from_slice(&prng(6, 20_000));
        data.extend_from_slice(&piece);
        data.extend_from_slice(&prng(7, 20_000));
        let mut im = small(l, 512);
        let marks = im.prepare_buffer(&data);
        let mut stat = Vec::new();
        let lit = im.compress(&data, 0, data.len(), &marks, &mut stat).expect("ok");
        assert!(stat.is_empty(), "a 128-byte match passed a 512-byte MIN_MATCH");
        assert_eq!(lit, data.len());
    }
}
