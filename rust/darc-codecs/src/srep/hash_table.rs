//! The compressor's chunk index, ported from `Compression/SREP/hash_table.cpp`.
//!
//! Scoped to the `-m3`/`-m4` path, which is what `arc.ini` runs. Two branches of
//! the C are therefore absent, deliberately:
//!
//! * **`SliceHash`** is only consulted when `COMPARE_DIGESTS` is false. `-m3`
//!   sets it true, so `slicehash.check()` is unreachable there and porting it
//!   would be dead code with no oracle. `-m5` needs it.
//! * **`CONTENT_DEFINED_CHUNKING`** (`-m1`/`-m2`) uses `startarr` and
//!   `find_match_CDC` instead of `hasharr`, and is multithreaded.
//!
//! # The chunk digest is NOT part of the format
//!
//! This is the load-bearing observation for the whole port, so it is written
//! down rather than left implicit.
//!
//! The C's per-chunk digest is a `VDigest` — two VMAC tags packed into 20 bytes
//! (`hashes.cpp:386-395`) — and `VHash::init()` with a null seed derives its key
//! from **`cryptographic_prng`** (`:350`). The key is therefore *random on every
//! run*. Despite that, the C's compressed output is byte-identical across runs;
//! measured, five runs each across `-m3f`, `-m3`, `-m3o`, `-m4f`.
//!
//! It is stable because the digest never reaches the output. It is used only to
//! answer "do these two chunks have identical contents?" via `memcmp` between
//! two digests computed under the *same* key. Any hash strong enough to make a
//! collision negligible answers that question identically, so the choice of
//! function is not observable in the compressed stream.
//!
//! This port therefore uses **SHA-256 truncated to 20 bytes**: deterministic,
//! keyless, already a dependency, and the same memory footprint per chunk as the
//! C's `Digest`. The substitution is falsifiable — if the digest did affect the
//! output, `srep-encode-check.sh` would diverge on the first compressible input.
//!
//! ## Why truncated SHA-256 and not SHA-1
//!
//! "Collisions are negligible" is true for random data and **false for crafted
//! data**, and the difference matters here. A digest collision makes the encoder
//! record a match between two chunks that are not identical, which produces a
//! silently corrupt archive — the decoder reproduces the wrong bytes and the
//! block hash catches it only at extract time.
//!
//! The C is not exposed to that: its VMAC key is random per run, so a collision
//! cannot be precomputed against it. An unkeyed SHA-1 *is* exposed, because
//! chosen-prefix SHA-1 collisions are practical. Truncated SHA-256 restores the
//! property — no collision attack is known against it — without needing a key.
//!
//! (Reusing VMAC would additionally have imported the ARM64 `ulong32`
//! miscompilation that already degrades hash verification in the decoder.)

use sha2::{Digest as _, Sha256};

/// `Chunk` — an index into the file's `L`-byte chunks.
pub type Chunk = u32;

/// `NOT_FOUND` (`hash_table.cpp:12`). Zero is a sentinel, so chunk 0 can never
/// be stored; the C asserts this at construction and so does [`HashTable::new`].
pub const NOT_FOUND: Chunk = 0;

/// `MAX_HASH_CHAIN` (`:13`) — how many slots a probe walks before giving up.
pub const MAX_HASH_CHAIN: i32 = 12;

/// `Digest` is `unsigned char[SHA1_SIZE]` (`hashes.cpp:11`).
pub const DIGEST_LEN: usize = 20;

/// `lb()` (`Common.h:691`) — floor(log2(n)), i.e. the index of the highest set
/// bit. The C reaches this through `__builtin_clzll`; `n == 0` is undefined
/// there and never occurs, since every caller has already forced `n >= 2`.
pub fn lb(n: u64) -> u32 {
    debug_assert!(n > 0, "lb(0) is undefined in the C too");
    63 - n.leading_zeros()
}

/// `roundup_to_power_of(n, 2)` (`Common.h:727`).
///
/// Transcribed including its two early exits: `n == 0` yields 0, and `n == 1`
/// yields 1 rather than the base. Both matter — `bitarrsize` is forced to at
/// least 2 precisely because `lb()` of a smaller value would misbehave.
pub fn roundup_to_power_of_2(n: u64) -> u64 {
    if n == 0 {
        return 0;
    }
    let m = n - 1;
    if m == 0 {
        return 1;
    }
    2u64 << lb(m)
}

/// `min_hash_size(n)` (`:15`) — `((n)/4+1)*5`.
pub fn min_hash_size(n: u64) -> u64 {
    (n / 4 + 1) * 5
}

/// Configuration the `-m3`/`-m4` path needs from the driver.
#[derive(Clone, Copy, Debug)]
pub struct Config {
    /// `L` — chunk size in bytes.
    pub l: usize,
    /// `COMPARE_DIGESTS` — true for `-m0..-m3`: confirm a match by comparing
    /// 160-bit chunk digests rather than by re-reading the old data.
    pub compare_digests: bool,
    /// `PRECOMPUTE_DIGESTS` — true for `-m3` only: digest every chunk of a
    /// block up front, in `prepare_buffer`.
    pub precompute_digests: bool,
    /// `ROUND_MATCHES` — `(method == 3) && (dictsize == 0)`.
    pub round_matches: bool,
    /// `BITARR_ACCELERATOR` = `accel * 8` (`srep.cpp:505`). Zero disables the
    /// bit-array pre-filter entirely.
    pub bitarr_accelerator: u64,
}

/// `HashTable` (`:110`), `-m3`/`-m4` subset.
pub struct HashTable {
    cfg: Config,
    total_chunks: u64,
    chunknum_mask: Chunk,
    hash_mask: Chunk,
    hashsize1: u64,
    bitshift: u32,
    /// `chunkarr[]` — open-addressed slots holding a packed (hash bits, chunk).
    chunkarr: Vec<Chunk>,
    /// `hasharr[]` — the top 32 bits of each chunk's rolling hash.
    hasharr: Vec<u32>,
    /// `digestarr[]` — one 20-byte digest per chunk.
    digestarr: Vec<[u8; DIGEST_LEN]>,
    /// `bitarr[]` — a Bloom-ish pre-filter; empty when the accelerator is off.
    bitarr: Vec<u8>,
}

impl HashTable {
    /// `HashTable::HashTable()` (`:136`), for the non-CDC, non-in-memory path.
    ///
    /// `filesize` is `max(filesize, L)` as in the C, so a file shorter than one
    /// chunk still sizes its arrays for one.
    pub fn new(cfg: Config, filesize: u64) -> Self {
        let filesize = filesize.max(cfg.l as u64);
        let total_chunks = filesize / cfg.l as u64;

        let chunknum_mask = (roundup_to_power_of_2(total_chunks + 2) - 1) as Chunk;
        let hash_mask = !chunknum_mask;

        let hs = roundup_to_power_of_2(min_hash_size(total_chunks));
        let hashsize1 = hs - 1;

        // bitarr[] is sized so it stays sparse: the C's comment notes the probe
        // "works fine until 1/8 of bitarr[] gets filled".
        let bitarrsize = match cfg.bitarr_accelerator {
            0 => 0,
            a => roundup_to_power_of_2((total_chunks / 8 * a).max(2)),
        };
        // The C computes bitshift unconditionally; with bitarrsize 0 the shift
        // would be 64 and the array is never indexed, so it is only meaningful
        // when the accelerator is on.
        let bitshift = match bitarrsize {
            0 => 0,
            n => 64 - lb(n),
        };

        HashTable {
            cfg,
            total_chunks,
            chunknum_mask,
            hash_mask,
            hashsize1,
            bitshift,
            chunkarr: vec![0; hs as usize],
            hasharr: vec![0; total_chunks as usize],
            digestarr: match cfg.compare_digests {
                true => vec![[0u8; DIGEST_LEN]; total_chunks as usize],
                false => Vec::new(),
            },
            bitarr: vec![0u8; bitarrsize as usize],
        }
    }

    /// How many chunks the table can hold — the C silently stops indexing past
    /// this, and so must the port.
    pub fn total_chunks(&self) -> u64 {
        self.total_chunks
    }

    // -- the hash-slot macros (`:211-217`) ----------------------------------

    /// `stored_hash(hash2)` — `hash2 >> (8 * (sizeof(BigHash) - sizeof(u32)))`.
    fn stored_hash(hash2: u64) -> u32 {
        (hash2 >> 32) as u32
    }

    /// `hash_index(h)` — the low bits, because the high ones are shared with
    /// the stored hash value.
    fn hash_index(&self, h: u64) -> usize {
        (h & self.hashsize1) as usize
    }

    /// `next_hash_slot(index, h)` (`:212`).
    fn next_hash_slot(h: u64) -> u64 {
        h.wrapping_mul(123_456_791)
            .wrapping_add(h >> 16)
            .wrapping_add(462_782_923)
    }

    /// `chunkarr_value(hash, chunk)` (`:215`).
    fn chunkarr_value(&self, hash: u64, chunk: Chunk) -> Chunk {
        ((hash as Chunk) & self.hash_mask).wrapping_add(chunk)
    }

    // -- the bitarr pre-filter (`:188-190`) ---------------------------------

    /// `check_match_possibility<ACCELERATOR>` — true when the accelerator is
    /// off, so the caller always proceeds to the real probe.
    pub fn check_match_possibility(&self, hash: u64) -> bool {
        match self.bitarr.is_empty() {
            true => true,
            false => {
                let byte = self.bitarr[(hash >> self.bitshift) as usize];
                byte & (1u8 << (hash as usize & 7)) != 0
            }
        }
    }

    /// `mark_match_possibility<ACCELERATOR>`.
    pub fn mark_match_possibility(&mut self, hash: u64) {
        match self.bitarr.is_empty() {
            true => {}
            false => {
                let i = (hash >> self.bitshift) as usize;
                self.bitarr[i] |= 1u8 << (hash as usize & 7);
            }
        }
    }

    // -- digests ------------------------------------------------------------

    /// The chunk digest: SHA-256 truncated to `DIGEST_LEN`. See the module docs
    /// for why this replaces the C's keyed `VDigest`, and why it is not SHA-1.
    fn digest(buf: &[u8]) -> [u8; DIGEST_LEN] {
        let mut h = Sha256::new();
        h.update(buf);
        let out = h.finalize();
        let mut d = [0u8; DIGEST_LEN];
        d.copy_from_slice(&out[..DIGEST_LEN]);
        d
    }

    /// `prepare_buffer()` (`:176`) — for `-m3`, digest every whole `L`-byte
    /// chunk of the block up front.
    ///
    /// The loop condition is `(buf+size)-p >= L`, so a trailing partial chunk is
    /// skipped rather than digested short.
    pub fn prepare_buffer(&mut self, offset: u64, buf: &[u8]) {
        if !self.cfg.precompute_digests {
            return;
        }
        let l = self.cfg.l;
        let mut curchunk = (offset / l as u64) as usize;
        let mut p = 0usize;
        while buf.len() - p >= l {
            if curchunk >= self.digestarr.len() {
                break;
            }
            self.digestarr[curchunk] = Self::digest(&buf[p..p + l]);
            curchunk += 1;
            p += l;
        }
    }

    // -- the table itself ---------------------------------------------------

    /// `add_hash()` -> `add_hash0<false>()` (`:197`, `:222`).
    ///
    /// Records `curchunk` under `hash2` and returns an earlier chunk with the
    /// same contents, or [`NOT_FOUND`].
    pub fn add_hash(&mut self, curchunk: Chunk, hash2: u64) -> Chunk {
        let stored_value = Self::stored_hash(hash2);
        match (curchunk as usize) < self.hasharr.len() {
            true => self.hasharr[curchunk as usize] = stored_value,
            false => return NOT_FOUND,
        }
        // "it's impossible to hash this chunk number since it's used as a
        // signal value" (:227)
        if curchunk == NOT_FOUND {
            return NOT_FOUND;
        }

        let index = hash2;
        let mut h = index;
        let mut limit = MAX_HASH_CHAIN;
        let mut found = NOT_FOUND;
        let saved_hash = self.chunkarr_value(index, 0);

        loop {
            let value = self.chunkarr[self.hash_index(h)];
            limit -= 1;
            if value == NOT_FOUND || limit == 0 {
                break;
            }
            if value & self.hash_mask == saved_hash {
                let chunk = value & self.chunknum_mask;
                let same = match self.cfg.compare_digests {
                    // -m3: the whole chunk's 160-bit digest must match.
                    true => {
                        let (a, b) = (chunk as usize, curchunk as usize);
                        a < self.digestarr.len()
                            && b < self.digestarr.len()
                            && self.digestarr[a] == self.digestarr[b]
                    }
                    // -m4: `speed_opt` short-circuits the slicehash check to
                    // true, so the stored hash match is taken as sufficient.
                    false => self.hasharr[chunk as usize] == stored_value,
                };
                if same {
                    found = chunk;
                    break;
                }
            }
            h += 1;
            if limit & 3 == 0 {
                h = Self::next_hash_slot(h);
            }
        }

        let slot = self.hash_index(h);
        self.chunkarr[slot] = self.chunkarr_value(index, curchunk);
        found
    }

    /// `find_match()` -> `find_match0()` (`:205`, `:263`).
    ///
    /// Unlike [`Self::add_hash`] this does not insert; it only probes. `i` is
    /// the candidate position in `buf`, whose fresh digest is compared against
    /// the stored one.
    pub fn find_match(&self, buf: &[u8], i: usize, hash2: u64) -> Chunk {
        let stored_value = Self::stored_hash(hash2);
        let index = hash2;
        let mut h = index;
        let mut limit = MAX_HASH_CHAIN;
        let saved_hash = self.chunkarr_value(index, 0);

        loop {
            let value = self.chunkarr[self.hash_index(h)];
            limit -= 1;
            if value == NOT_FOUND || limit == 0 {
                return NOT_FOUND;
            }
            if value & self.hash_mask == saved_hash {
                let chunk = value & self.chunknum_mask;
                let ci = chunk as usize;
                if ci < self.hasharr.len() && self.hasharr[ci] == stored_value {
                    match self.cfg.compare_digests {
                        // -m3: confirm with the full chunk digest.
                        true => {
                            if i + self.cfg.l <= buf.len() && ci < self.digestarr.len() {
                                let dig = Self::digest(&buf[i..i + self.cfg.l]);
                                if dig == self.digestarr[ci] {
                                    return chunk;
                                }
                            }
                        }
                        // -m4: `slicehash.check()` is vacuously true with no
                        // SliceHash allocated, so the stored hash is accepted.
                        false => return chunk,
                    }
                }
            }
            h += 1;
            if limit & 3 == 0 {
                h = Self::next_hash_slot(h);
            }
        }
    }

    /// `match_len()` (`:303`) — how far a candidate match actually extends.
    ///
    /// `-m3` only (`COMPARE_DIGESTS`). `start_i` is where the match begins in
    /// `buf`, `last_i` is the exclusive limit it may not run past, and `offset`
    /// is the block's start position in the file.
    ///
    /// Returns `(match_len, add_len)`. `add_len` is always 0 here: the C only
    /// ever sets it in the `-m4`/`-m5` branches, which extend a match
    /// *backwards* by re-reading the input file. Digest comparison works on
    /// whole `L`-byte chunks, so there is nothing to extend backwards into, and
    /// the value is returned anyway to keep the caller's shape identical to the
    /// C's for when those branches land.
    ///
    /// The C walks with `while (p += L, (old_offset += L) < offset)` — a comma
    /// expression, so both advance *before* the test and the first old chunk is
    /// skipped deliberately: `find_match()` already compared it.
    pub fn match_len(
        &self,
        start_chunk: Chunk,
        start_i: usize,
        last_i: usize,
        offset: u64,
        buf: &[u8],
    ) -> (usize, usize) {
        debug_assert!(self.cfg.compare_digests, "match_len: -m3 path only");
        let l = self.cfg.l;
        let add_len = 0usize;
        let mut p = start_i;
        let mut old_offset = u64::from(start_chunk) * l as u64;

        // Phase 1: the old data lies before this block, so compare digests.
        loop {
            p += l;
            old_offset += l as u64;
            if old_offset >= offset {
                break;
            }
            // "We have no L-byte chunk to digest" -- stop, do not digest short.
            if last_i.saturating_sub(p) < l {
                return (p - start_i + add_len, add_len);
            }
            let want = (old_offset / l as u64) as usize;
            if want >= self.digestarr.len() {
                return (p - start_i + add_len, add_len);
            }
            if Self::digest(&buf[p..p + l]) != self.digestarr[want] {
                return (p - start_i + add_len, add_len);
            }
        }

        // Phase 2: the old data is inside this block, so compare bytes.
        //
        // `old_offset - offset` is an index into buf, and is >= 0 here because
        // the loop above only exits once old_offset has reached offset.
        let mut q = (old_offset - offset) as usize;
        while p < last_i && q < buf.len() && p < buf.len() && buf[p] == buf[q] {
            p += 1;
            q += 1;
        }

        (p - start_i + add_len, add_len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lb_is_the_high_bit_index() {
        assert_eq!(lb(1), 0);
        assert_eq!(lb(2), 1);
        assert_eq!(lb(3), 1);
        assert_eq!(lb(4), 2);
        assert_eq!(lb(255), 7);
        assert_eq!(lb(256), 8);
        assert_eq!(lb(u64::MAX), 63);
    }

    #[test]
    fn roundup_matches_the_c_including_its_early_exits() {
        // The C's own example: f(13,2) == 16.
        assert_eq!(roundup_to_power_of_2(13), 16);
        // n==0 returns 0, NOT the base -- a distinct branch in the C.
        assert_eq!(roundup_to_power_of_2(0), 0);
        // n==1 returns 1, also its own branch.
        assert_eq!(roundup_to_power_of_2(1), 1);
        assert_eq!(roundup_to_power_of_2(2), 2);
        assert_eq!(roundup_to_power_of_2(5), 8);
        for k in 2..40u32 {
            let p = 1u64 << k;
            assert_eq!(roundup_to_power_of_2(p), p, "exact power {p}");
            assert_eq!(roundup_to_power_of_2(p - 1), p, "just below {p}");
        }
    }

    #[test]
    fn min_hash_size_matches_the_macro() {
        for n in [0u64, 1, 3, 4, 5, 100, 4096] {
            assert_eq!(min_hash_size(n), (n / 4 + 1) * 5);
        }
    }

    fn cfg(l: usize) -> Config {
        Config {
            l,
            compare_digests: true,
            precompute_digests: true,
            round_matches: true,
            bitarr_accelerator: 0,
        }
    }

    #[test]
    fn identical_chunks_are_matched_and_distinct_ones_are_not() {
        // The table's whole contract: adding a chunk whose contents repeat an
        // earlier one returns that earlier chunk.
        let l = 64usize;
        let mut h = HashTable::new(cfg(l), 64 * 100);
        let block: Vec<u8> = (0..64u16 * 100).map(|i| (i % 251) as u8).collect();
        h.prepare_buffer(0, &block);

        // Chunks 1 and 3 are given the same rolling-hash value and the same
        // digest slot contents, so they must resolve to each other.
        h.digestarr[3] = h.digestarr[1];
        assert_eq!(h.add_hash(1, 0xdead_beef_0000_0001), NOT_FOUND);
        assert_eq!(h.add_hash(3, 0xdead_beef_0000_0001), 1);

        // A different hash must not collide into it.
        assert_eq!(h.add_hash(5, 0x0123_4567_89ab_cdef), NOT_FOUND);
    }

    #[test]
    fn chunk_zero_is_never_stored_because_it_is_the_sentinel() {
        let mut h = HashTable::new(cfg(64), 64 * 100);
        // add_hash(0, ..) records the hash but must report NOT_FOUND and must
        // not make chunk 0 findable -- NOT_FOUND == 0 is why.
        assert_eq!(h.add_hash(NOT_FOUND, 0x1111_2222_3333_4444), NOT_FOUND);
        h.digestarr[2] = h.digestarr[0];
        assert_eq!(h.add_hash(2, 0x1111_2222_3333_4444), NOT_FOUND);
    }

    #[test]
    fn the_bit_prefilter_only_engages_when_accelerated() {
        let mut off = HashTable::new(cfg(64), 64 * 1000);
        // With the accelerator off every probe must pass, or the caller would
        // skip real matches.
        assert!(off.check_match_possibility(0));
        assert!(off.check_match_possibility(u64::MAX));
        off.mark_match_possibility(12345); // must not panic on an empty array

        let mut on = HashTable::new(
            Config {
                bitarr_accelerator: 8,
                ..cfg(64)
            },
            64 * 1000,
        );
        let hash = 0x89ab_cdef_0123_4567u64;
        assert!(!on.check_match_possibility(hash), "unmarked must be absent");
        on.mark_match_possibility(hash);
        assert!(on.check_match_possibility(hash), "marked must be present");
    }

    #[test]
    fn match_len_extends_within_the_block_byte_by_byte() {
        // Old data inside the current block: phase 1 exits immediately (the
        // first `old_offset += L` already reaches offset) and the byte compare
        // decides the length.
        let l = 16usize;
        let h = HashTable::new(cfg(l), 4096);
        let mut buf = vec![0u8; 256];
        for (i, b) in buf.iter_mut().enumerate() {
            *b = (i % 7) as u8;
        }
        // chunk 0 is at buf[0..], the candidate at buf[16..] repeats it for 48
        // bytes because the pattern has period 7 and 48 is not a multiple of 7.
        let (len, add) = h.match_len(0, l, buf.len(), 0, &buf);
        assert_eq!(add, 0, "add_len is never set on the -m3 path");
        // buf[16+k] == buf[k] iff (16+k)%7 == k%7, which never holds, so the
        // very first byte differs and the match is exactly L.
        assert_eq!(len, l);
    }

    #[test]
    fn match_len_stops_where_the_bytes_stop_agreeing() {
        let l = 8usize;
        let h = HashTable::new(cfg(l), 4096);
        // Two identical runs, then a divergence 5 bytes in.
        let mut buf = vec![0xAAu8; 64];
        buf[8 + 5] = 0xBB;
        let (len, _) = h.match_len(0, 8, buf.len(), 0, &buf);
        // Phase 1 advances p by L and reaches offset, then bytes agree until
        // the planted difference.
        assert_eq!(len, l + 5);
    }

    #[test]
    fn match_len_refuses_to_digest_a_short_tail() {
        // The C's `if (last_p-p < L) goto stop` -- with the old data before the
        // block, a candidate that runs out of room must stop rather than digest
        // a partial chunk and compare it against a full one.
        let l = 64usize;
        let mut h = HashTable::new(cfg(l), 64 * 100);
        let buf = vec![3u8; l * 2];
        h.prepare_buffer(0, &buf);
        // offset far ahead of the chunk, so phase 1 runs; last_i leaves less
        // than L bytes after the first advance.
        let (len, _) = h.match_len(0, 0, l + 4, 1 << 20, &buf);
        assert_eq!(len, l, "stopped at the chunk boundary, not past it");
    }

    #[test]
    fn prepare_buffer_skips_a_trailing_partial_chunk() {
        // The C loops while `(buf+size)-p >= L`, so a short tail is not
        // digested. Digesting it would give the final chunk a value no other
        // chunk can equal, which is harmless -- but it would also write one
        // entry further than the C does.
        let l = 64usize;
        let mut h = HashTable::new(cfg(l), 64 * 10);
        let buf = vec![7u8; l * 3 + 5];
        h.prepare_buffer(0, &buf);
        assert_ne!(h.digestarr[0], [0u8; DIGEST_LEN]);
        assert_ne!(h.digestarr[2], [0u8; DIGEST_LEN]);
        assert_eq!(h.digestarr[3], [0u8; DIGEST_LEN], "partial chunk digested");
    }
}
