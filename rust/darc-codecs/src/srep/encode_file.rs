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
use super::{BULAT_ZIGANSHIN_SIGNATURE, FOOTER_VERSION1, SREP_SIGNATURE};

/// `INDEX_LZ_FOOTER_SIZE` (`srep.cpp:583`) -- six `STAT` words.
const INDEX_LZ_FOOTER_BYTES: usize = 6 * 4;

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
    // -m0 is a different algorithm entirely and -m1/-m2 are the multithreaded
    // CDC pair, whose output may depend on thread count.
    if method.cdc() || method == Method::InMemory {
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
        let res = compress::compress(&cp, origsize, &mut table, buf, data, &fence, &mut stat)?;

        blocks.push(Block {
            start: origsize,
            size: len,
            literal_bytes: res.literal_bytes,
            stat,
        });
        origsize += len as u64;
    }

    // -- what each block's written record list is ---------------------------
    //
    // Future-LZ needs the second pass (`:727-830`): matches are re-anchored to
    // the block containing their SOURCE, so they must all be collected and
    // sorted first.
    //
    // I/O-LZ does not. `single_pass_compression` (`:477`) is true for it, and
    // `save_data` (io.cpp:172) writes each block's own statbuf verbatim --
    // destination-anchored, still in the ROUND_MATCHES encoding compress()
    // produced, which is exactly what the v1/v2 decoder expects because
    // header[3] carries BASE_LEN rather than 0 for this layout.
    let per_block: Vec<Vec<u32>> = match layout {
        Layout::FutureLz => {
            let sorted = emit::collect_and_sort(&blocks, d.round_matches, d.base_len);
            emit::redistribute(&blocks, &sorted, d.futurelz_base_len)?
        }
        Layout::IoLz => blocks.iter().map(|b| b.stat.clone()).collect(),
        // Index-LZ re-anchors exactly like Future-LZ; only where the records
        // LAND differs -- they go to a footer instead of beside their block.
        Layout::IndexLz => {
            let sorted = emit::collect_and_sort(&blocks, d.round_matches, d.base_len);
            emit::redistribute(&blocks, &sorted, d.futurelz_base_len)?
        }
    };

    let index_lz = layout == Layout::IndexLz;
    let mut statsizes: Vec<u32> = Vec::with_capacity(blocks.len());
    let mut all_stats: Vec<u8> = Vec::new();

    for (block, stat) in blocks.iter().zip(per_block.iter()) {
        let body = &data[block.start as usize..block.start as usize + block.size];
        let stat_bytes: Vec<u8> = stat.iter().flat_map(|w| w.to_le_bytes()).collect();

        // Block header: literal_bytes, len, stat_size -- then the block hash,
        // which io.cpp:153 writes at `header[i]+3`, immediately after the three
        // words.
        //
        // Under Index-LZ the stat_size field is ZERO here (`:626`): pass one
        // writes the header before the records exist, and they never join it.
        put_u32(&mut out, block.literal_bytes as u32);
        put_u32(&mut out, block.size as u32);
        put_u32(&mut out, match index_lz {
            true => 0,
            false => stat_bytes.len() as u32,
        });
        out.extend_from_slice(&hasher.digest(body));

        match index_lz {
            // Future-LZ and I/O-LZ interleave: records then literals, per block.
            false => {
                out.extend_from_slice(&stat_bytes);
                out.extend_from_slice(&emit::literals(block, body, d.round_matches, d.base_len));
            }
            // Index-LZ writes only literals here; every block's records are held
            // back and concatenated after the last block.
            true => {
                out.extend_from_slice(&emit::literals(block, body, d.round_matches, d.base_len));
                statsizes.push(stat_bytes.len() as u32);
                all_stats.extend_from_slice(&stat_bytes);
            }
        }
    }

    // -- Index-LZ footer (`:862-874`) ---------------------------------------
    if index_lz {
        let total_stat_size = all_stats.len() as u64;
        out.extend_from_slice(&all_stats);
        for n in &statsizes {
            put_u32(&mut out, *n);
        }
        // Six words, and note the two signatures are BITWISE COMPLEMENTS of the
        // archive header's -- that is what lets a reader find the footer by
        // scanning back from the end.
        let statsize_bytes = statsizes.len() * 4;
        put_u32(&mut out, total_stat_size as u32);
        put_u32(&mut out, (total_stat_size >> 32) as u32);
        put_u32(&mut out, (INDEX_LZ_FOOTER_BYTES + statsize_bytes) as u32);
        put_u32(&mut out, FOOTER_VERSION1);
        put_u32(&mut out, !SREP_SIGNATURE);
        put_u32(&mut out, !BULAT_ZIGANSHIN_SIGNATURE);
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
    fn unported_methods_are_refused_rather_than_mis_encoded() {
        // -m0 is a different algorithm; -m1/-m2 are the multithreaded CDC pair.
        // All three LAYOUTS are supported now, for both -m3 and -m4.
        for (m, l) in [
            (Method::Cdc, Layout::FutureLz),
            (Method::ZpaqCdc, Layout::IndexLz),
            (Method::InMemory, Layout::FutureLz),
            (Method::InMemory, Layout::IoLz),
        ] {
            let r = compress_file(b"x", m, l, Options::default(), HashChoice::MD5, 0);
            assert_eq!(r, Err(EncodeError::Unsupported), "{m:?}/{l:?}");
        }
    }

    #[test]
    fn every_layout_round_trips_through_the_gated_decoder() {
        // Index-LZ in particular has a completely different file shape -- the
        // records live in a footer -- so its framing needs its own check.
        use std::io::Cursor;
        let half = prng(21, 40_000);
        let data: Vec<u8> = half.iter().chain(half.iter()).copied().collect();
        for layout in [Layout::FutureLz, Layout::IoLz, Layout::IndexLz] {
            for method in [Method::Digests, Method::Reread] {
                let packed = compress_file(
                    &data, method, layout, Options::default(), HashChoice::MD5, 16_384,
                )
                .unwrap_or_else(|e| panic!("{method:?}/{layout:?}: {e:?}"));
                let mut fin = Cursor::new(packed);
                let mut fout = Cursor::new(Vec::new());
                crate::srep::decode::decompress(&mut fin, &mut fout)
                    .unwrap_or_else(|e| panic!("{method:?}/{layout:?}: decode failed: {e:?}"));
                assert_eq!(fout.into_inner(), data, "{method:?}/{layout:?}");
            }
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
