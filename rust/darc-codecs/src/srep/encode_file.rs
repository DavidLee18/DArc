//! The whole `-m3f` compressor: option defaults, block loop, Future-LZ second
//! pass, and file framing. `srep.cpp:571-830`.
//!
//! # Layout of what this writes
//!
//! ```text
//! archive header   4 words + hash_seed_size bytes
//! per block:       3 words + hash_size bytes    (literal_bytes, len, stat_size)
//!                  stat_size bytes              source-anchored match records
//!                  literal bytes                everything no match covers
//! ```
//!
//! `header[3]` of the archive header is **`FUTURELZ_BASE_LEN`, not `BASE_LEN`**
//! (`:578`) — 0 under Future-LZ. The decoder reads that word as the base for
//! every record it decodes, which is what makes the zero-base records the second
//! pass writes readable. Writing `BASE_LEN` there instead produces a file whose
//! every match length is 512 too long, and which still decodes.
//!
//! # Two passes, because a match's source precedes its finder
//!
//! Pass one compresses each block and keeps its matches in memory. Pass two
//! sorts every match by source and hands it to the block containing that source.
//! See [`super::emit`].
//!
//! Only `-m3`/`-m4` with Future-LZ is implemented. `-m0`/`-m1`/`-m2` are
//! different algorithms and `-mNo`/`-mN` are different framings; each is
//! refused explicitly rather than silently mis-encoded.

use super::compress::{self, Params};
use super::emit::{self, Block};
use super::hash_table::{Config, HashTable};
use super::hashes::Hash;
use super::matches::MatchTooShort;
use super::params::{self, Layout, Method, Options, DEFAULT_BUFSIZE};
use super::{BULAT_ZIGANSHIN_SIGNATURE, SREP_SIGNATURE};

/// Why a compression request could not be served.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EncodeError {
    /// A method or layout this port does not implement yet.
    Unsupported,
    /// `ENCODE_LZ_MATCH`'s "match len too small".
    MatchTooShort(MatchTooShort),
}

impl From<MatchTooShort> for EncodeError {
    fn from(e: MatchTooShort) -> Self {
        EncodeError::MatchTooShort(e)
    }
}

/// The hash written after each block header. `-hash=md5` is `(0, 0, 16)`.
#[derive(Clone, Copy, Debug)]
pub struct HashChoice {
    pub hash_num: u8,
    pub hash_seed_size: u8,
    pub hash_size: u8,
}

impl HashChoice {
    pub const MD5: HashChoice = HashChoice { hash_num: 0, hash_seed_size: 0, hash_size: 16 };
}

fn put_u32(out: &mut Vec<u8>, v: u32) {
    out.extend_from_slice(&v.to_le_bytes());
}

/// Compress `data` into a complete `.srep` file.
pub fn compress_file(
    data: &[u8],
    method: Method,
    layout: Layout,
    opt: Options,
    hash: HashChoice,
    bufsize: usize,
) -> Result<Vec<u8>, EncodeError> {
    // The only shapes this port can produce byte-exactly so far.
    if layout != Layout::FutureLz || method.cdc() || method == Method::InMemory {
        return Err(EncodeError::Unsupported);
    }
    let d = params::derive(method, layout, opt);
    let hasher = Hash::from_num(hash.hash_num, hash.hash_size);

    let mut out = Vec::new();
    // -- archive header (`:571-580`) ---------------------------------------
    put_u32(&mut out, BULAT_ZIGANSHIN_SIGNATURE);
    put_u32(&mut out, SREP_SIGNATURE);
    put_u32(
        &mut out,
        d.format_version
            + (u32::from(hash.hash_num) << 8)
            + (u32::from(hash.hash_seed_size) << 16)
            + ((u32::from(hash.hash_size).wrapping_sub(16)) << 24),
    );
    // NOT base_len -- see the module docs.
    put_u32(&mut out, d.futurelz_base_len);
    // A seeded hash would write its seed here; md5 has none.

    // -- pass one: compress each block (`:590-690`) -------------------------
    let bufsize = match bufsize {
        0 => DEFAULT_BUFSIZE,
        n => n,
    };
    let mut table = HashTable::new(
        Config {
            l: d.l as usize,
            compare_digests: method.compare_digests(),
            precompute_digests: method.precompute_digests(),
            round_matches: d.round_matches,
            bitarr_accelerator: u64::from(d.bitarr_accelerator),
        },
        data.len() as u64,
    );
    let cp = Params {
        round_matches: d.round_matches,
        l: d.l as usize,
        min_match: d.min_match as usize,
        base_len: d.base_len,
        accelerator: d.accelerator as usize,
    };

    let mut blocks: Vec<Block> = Vec::new();
    let mut origsize = 0u64;
    while (origsize as usize) < data.len() {
        let len = bufsize.min(data.len() - origsize as usize);
        let buf = &data[origsize as usize..origsize as usize + len];

        table.prepare_buffer(origsize, buf);
        // The fence (`:605`) -- lit_len past the block end, so the input-match
        // branch stays inert without a dictionary.
        let mut fence = Vec::new();
        super::matches::encode(
            &mut fence,
            d.round_matches,
            d.base_len,
            len as u32 + 1,
            u64::from(d.base_len),
            d.base_len,
        )?;

        let mut stat = Vec::new();
        let res = compress::compress(&cp, origsize, &mut table, buf, &fence, &mut stat)?;

        blocks.push(Block {
            start: origsize,
            size: len,
            literal_bytes: res.literal_bytes,
            stat,
        });
        origsize += len as u64;
    }

    // -- pass two: redistribute by source (`:727-830`) ----------------------
    let sorted = emit::collect_and_sort(&blocks, d.round_matches, d.base_len);
    let per_block = emit::redistribute(&blocks, &sorted, d.futurelz_base_len)?;

    for (block, stat) in blocks.iter().zip(per_block.iter()) {
        let body = &data[block.start as usize..block.start as usize + block.size];
        let stat_bytes: Vec<u8> = stat.iter().flat_map(|w| w.to_le_bytes()).collect();

        // block header: literal_bytes, len, stat_size -- then the block hash.
        put_u32(&mut out, block.literal_bytes as u32);
        put_u32(&mut out, block.size as u32);
        put_u32(&mut out, stat_bytes.len() as u32);
        out.extend_from_slice(&hasher.digest(body));

        out.extend_from_slice(&stat_bytes);
        out.extend_from_slice(&emit::literals(block, body, d.round_matches, d.base_len));
    }

    Ok(out)
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

    fn m3f(data: &[u8], bufsize: usize) -> Vec<u8> {
        compress_file(
            data,
            Method::Digests,
            Layout::FutureLz,
            Options::default(),
            HashChoice::MD5,
            bufsize,
        )
        .expect("compresses")
    }

    #[test]
    fn the_archive_header_is_what_the_decoder_parses() {
        let out = m3f(&prng(1, 4096), 0);
        let w: Vec<u32> = out[..16]
            .chunks(4)
            .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect();
        let h = crate::srep::parse_header(&w).expect("our own header must parse");
        assert_eq!(h.strategy, crate::srep::Strategy::FutureLz);
        assert_eq!(h.hash_num, 0, "md5");
        assert_eq!(h.hash_seed_size, 0);
        assert_eq!(h.hash_size, 16);
        // The one that is easy to get wrong: base_len is FUTURELZ_BASE_LEN.
        assert_eq!(h.base_len, 0, "header[3] is FUTURELZ_BASE_LEN, not BASE_LEN");
    }

    #[test]
    fn a_duplicated_input_actually_shrinks() {
        // If the match finder or the emission were inert this would grow by the
        // framing instead.
        let half = prng(7, 200_000);
        let data: Vec<u8> = half.iter().chain(half.iter()).copied().collect();
        let out = m3f(&data, 0);
        assert!(
            out.len() < data.len() / 2 + 65_536,
            "no dedup happened: {} -> {}",
            data.len(),
            out.len()
        );
    }

    #[test]
    fn incompressible_input_is_stored_with_only_framing_added() {
        let data = prng(9, 100_000);
        let out = m3f(&data, 0);
        // Header 16 + block header 12 + md5 16 = 44 bytes of framing.
        assert_eq!(out.len(), data.len() + 44, "unexpected overhead");
    }

    #[test]
    fn multiple_blocks_each_get_a_header() {
        // A small -b forces several blocks, which is where the second pass
        // actually has to move matches between them.
        let half = prng(3, 100_000);
        let data: Vec<u8> = half.iter().chain(half.iter()).copied().collect();
        let out = m3f(&data, 65_536);
        assert!(!out.is_empty());
        // 4 blocks at 64 KB, so at least 4 block headers beyond the archive one.
        let blocks = data.len().div_ceil(65_536);
        assert!(
            out.len() >= 16 + blocks * (12 + 16),
            "output too small to hold {blocks} block headers"
        );
    }

    #[test]
    fn unsupported_shapes_are_refused_rather_than_mis_encoded() {
        for (m, l) in [
            (Method::Digests, Layout::IoLz),
            (Method::Digests, Layout::IndexLz),
            (Method::Cdc, Layout::FutureLz),
            (Method::InMemory, Layout::FutureLz),
        ] {
            let r = compress_file(b"x", m, l, Options::default(), HashChoice::MD5, 0);
            assert_eq!(r, Err(EncodeError::Unsupported), "{m:?}/{l:?}");
        }
    }

    #[test]
    fn the_gated_decoder_reads_what_this_writes() {
        // The strongest check available in-process: srep::decode is verified
        // byte-exact against the C across all four format versions, so if it
        // reproduces the original from our output, the framing, the record
        // encoding, the source-anchoring and the block hashes are all right.
        use std::io::Cursor;

        let half = prng(11, 60_000);
        let cases: Vec<(&str, Vec<u8>, usize)> = vec![
            ("duplicated", half.iter().chain(half.iter()).copied().collect(), 0),
            ("duplicated-multiblock", half.iter().chain(half.iter()).copied().collect(), 16_384),
            ("noise", prng(13, 40_000), 0),
            ("runs", vec![0xABu8; 50_000], 0),
            ("tiny", b"hello".to_vec(), 0),
            ("empty", Vec::new(), 0),
        ];

        for (name, data, bufsize) in cases {
            let packed = m3f(&data, bufsize);
            let mut fin = Cursor::new(packed);
            let mut fout = Cursor::new(Vec::new());
            crate::srep::decode::decompress(&mut fin, &mut fout)
                .unwrap_or_else(|e| panic!("{name}: decoder rejected our output: {e:?}"));
            assert_eq!(fout.into_inner(), data, "{name}: round-trip differs");
        }
    }

    #[test]
    fn an_empty_input_produces_just_the_archive_header() {
        let out = m3f(&[], 0);
        assert_eq!(out.len(), 16, "no blocks, so nothing after the header");
    }
}
