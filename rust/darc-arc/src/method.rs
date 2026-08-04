//! Compression-method strings, and the parameters they carry.
//!
//! A compression method in DArc is **a string that C parses**, not an ADT — the
//! archive stores `"lzma:1mb:mf=BT4"` and the reader has to turn that back into
//! the parameters the decoder needs. For LZMA that is not optional: DArc writes
//! no `.lzma` header, so the property bytes are rebuilt from the method string
//! every time (`C_LZMA.cpp:64`, and `lzma.rs:227` on the Rust side). Parse the
//! dictionary size wrong and the stream decodes to garbage rather than failing.
//!
//! Methods chain with `'+'`, in *compression* order, so decompression walks the
//! chain backwards (`decompressInMemory`, `ArhiveStructure.hs:387`).

/// `parseInt` (`Common.cpp:193`).
///
/// A leading `'='` is skipped, digits are consumed, and **anything left over is
/// an error** — which is what stops `pb2x` being read as `pb2`.
pub fn parse_int(s: &str) -> Option<u32> {
    let s = s.strip_prefix('=').unwrap_or(s);
    if s.is_empty() {
        return None;
    }
    let mut n: u32 = 0;
    for c in s.chars() {
        match c.to_digit(10) {
            Some(d) => n = n.wrapping_mul(10).wrapping_add(d),
            None => return None,
        }
    }
    Some(n)
}

/// `parseMem` (`Common.cpp:204`).
///
/// Two traps, both faithfully reproduced:
///
/// * the suffix is **one character**. `"16kb"` stops at `'k'` and returns
///   `16*1024`; the `'b'` is never examined. A parser that strips a trailing
///   `'b'` first sees `"16k"`, and this exact mistake once made every
///   multi-block row of a difftest silently not run.
/// * **no suffix means a power of two**, not bytes. `"22"` is `1<<22`, four
///   megabytes — reading it as 22 bytes would size a dictionary at nothing.
pub fn parse_mem(s: &str) -> Option<u32> {
    let s = s.strip_prefix('=').unwrap_or(s);
    if s.is_empty() {
        return None;
    }
    let digits: String = s.chars().take_while(char::is_ascii_digit).collect();
    let mut n: u32 = 0;
    for c in digits.chars() {
        match c.to_digit(10) {
            Some(d) => n = n.wrapping_mul(10).wrapping_add(d),
            None => return None,
        }
    }
    match s[digits.len()..].chars().next() {
        Some('b') => Some(n),
        Some('k') => Some(n.wrapping_mul(1024)),
        Some('m') => Some(n.wrapping_mul(1024 * 1024)),
        Some('g') => Some(n.wrapping_mul(1024 * 1024 * 1024)),
        Some('^') | None => {
            if n < 32 {
                Some(1u32 << n)
            } else {
                None
            }
        }
        Some(_) => None,
    }
}

/// The LZMA parameters that reach the decoder.
///
/// Only four of the nine matter for decoding — the rest steer the *encoder*'s
/// search and leave no trace in the stream. They are kept anyway so that a
/// method string can be round-tripped without loss.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LzmaParams {
    pub dictionary_size: u32,
    pub hash_size: u32,
    pub algorithm: u32,
    pub num_fast_bytes: u32,
    pub match_finder: u32,
    pub match_finder_cycles: u32,
    pub pos_state_bits: u32,
    pub lit_context_bits: u32,
    pub lit_pos_bits: u32,
}

impl Default for LzmaParams {
    /// `LZMA_METHOD::LZMA_METHOD()` (`C_LZMA.cpp:44`). These are the values a
    /// bare `"lzma"` means, so they are part of the format.
    fn default() -> Self {
        LzmaParams {
            dictionary_size: 64 * 1024 * 1024,
            hash_size: 0,
            algorithm: 1,
            num_fast_bytes: 32,
            match_finder: MF_HT4,
            match_finder_cycles: 0,
            pos_state_bits: 2,
            lit_context_bits: 3,
            lit_pos_bits: 0,
        }
    }
}

// Match-finder ids, in the order FindMatchFinder returns them. Decoding does not
// use the value, but parsing must accept the names or a method string is
// rejected outright.
const MF_HC4: u32 = 3;
const MF_HT4: u32 = 4;

/// `FindMatchFinder` — case-insensitive, as `strequ` is not but the table is
/// searched with the name exactly as the C spells it.
fn find_match_finder(name: &str) -> Option<u32> {
    match name.to_ascii_lowercase().as_str() {
        "bt2" => Some(0),
        "bt3" => Some(1),
        "bt4" => Some(2),
        "hc4" => Some(MF_HC4),
        "ht4" => Some(MF_HT4),
        _ => None,
    }
}

impl LzmaParams {
    /// The property bytes the decoder wants, rebuilt exactly as
    /// `decompress_inner` rebuilds them — including the `(Byte)` truncation, so
    /// an out-of-range `pb`/`lc`/`lp` wraps here and is then rejected by the
    /// decoder rather than indexing a table with it.
    pub fn props(&self) -> [u8; 5] {
        let byte0 = self
            .pos_state_bits
            .wrapping_mul(5)
            .wrapping_add(self.lit_pos_bits)
            .wrapping_mul(9)
            .wrapping_add(self.lit_context_bits) as u8;
        let d = self.dictionary_size;
        [byte0, d as u8, (d >> 8) as u8, (d >> 16) as u8, (d >> 24) as u8]
    }
}

/// `ppmd:ORDER:MEM` (`C_PPMD.cpp:130`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PpmdParams {
    pub order: i32,
    pub mem: u32,
    pub mr_method: i32,
}

impl Default for PpmdParams {
    /// `PPMD_METHOD::PPMD_METHOD()` (`C_PPMD.cpp:73`).
    fn default() -> Self {
        PpmdParams { order: 10, mem: 48 * 1024 * 1024, mr_method: 0 }
    }
}

/// `dict:BLOCK:...` (`C_Dict.cpp:86`). Only `block_size` reaches the decoder,
/// but every field is carried because `ShowCompressionMethod` prints them, and
/// the printed form is what an archive stores.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DictParams {
    pub block_size: u32,
    pub min_compression: u32,
    pub min_weak_chars: u32,
    pub min_large_cnt: u32,
    pub min_medium_cnt: u32,
    pub min_small_cnt: u32,
    pub min_ratio: u32,
}

impl Default for DictParams {
    /// `DICT_METHOD::DICT_METHOD()` (`C_Dict.cpp:31`).
    fn default() -> Self {
        DictParams {
            block_size: 64 * 1024 * 1024,
            min_compression: 100,
            min_weak_chars: 20,
            min_large_cnt: 2048,
            min_medium_cnt: 100,
            min_small_cnt: 50,
            min_ratio: 4,
        }
    }
}

/// `lzp:BLOCK:...` (`C_LZP.cpp`). Five of the six parameters reach the decoder,
/// because LZP's output depends on the same hash geometry that produced it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LzpParams {
    pub block_size: u32,
    pub min_compression: u32,
    pub min_match_len: i32,
    pub hash_size_log: i32,
    pub barrier: i32,
    pub smallest_len: i32,
}

impl Default for LzpParams {
    /// `LZP_METHOD::LZP_METHOD()` (`C_LZP.cpp:32`). `Barrier` defaults to
    /// `INT_MAX`, not to a size — it is "no barrier", and a zero here would
    /// change what the decoder reconstructs.
    fn default() -> Self {
        LzpParams {
            block_size: 8 * 1024 * 1024,
            min_compression: 100,
            min_match_len: 64,
            hash_size_log: 18,
            barrier: i32::MAX,
            smallest_len: 32,
        }
    }
}

/// `delta` / `delta:BLOCK:x` (`C_Delta.cpp`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DeltaParams {
    pub block_size: u32,
    pub extended_tables: i32,
}

/// `tta:...` (`C_TTA.cpp:72`) and `mm:...` (`C_MM.cpp:79`).
///
/// One struct for both: the field lists are identical apart from the first
/// (TTA's `level`, MM's `mode`) and the last (TTA's `raw_data`, MM's
/// `reorder`), and their parsers differ only in which letters name those two.
/// Keeping them together keeps the `c*w` fallback -- the one genuinely fiddly
/// part -- in one place.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MmParams {
    /// TTA's `level` (default 3) or MM's `mode` (default 9).
    pub level: i32,
    pub skip_header: i32,
    pub is_float: i32,
    pub num_chan: i32,
    pub word_size: i32,
    pub offset: i32,
    /// TTA's `raw_data` or MM's `reorder`.
    pub extra: i32,
}

/// `lzma2:...` (`C_LZMA2.cpp:118`).
///
/// The same knobs as [`LzmaParams`] minus `hash_size`: `LZMA2_METHOD` has no
/// `h` parameter, so `lzma2:h4mb` is a rejected string where `lzma:h4mb` is not.
/// Kept as its own struct rather than reusing `LzmaParams` for exactly that
/// reason — a shared struct would carry a field the format cannot express, and
/// the printer would have to remember never to print it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lzma2Params {
    pub dictionary_size: u32,
    pub algorithm: u32,
    pub num_fast_bytes: u32,
    pub match_finder: u32,
    pub match_finder_cycles: u32,
    pub pos_state_bits: u32,
    pub lit_context_bits: u32,
    pub lit_pos_bits: u32,
}

impl Default for Lzma2Params {
    /// `LZMA2_METHOD::LZMA2_METHOD()` (`C_LZMA2.cpp:40`) — field for field the
    /// same values as LZMA's constructor, which is why a bare `lzma2` and a bare
    /// `lzma` differ only in the container.
    fn default() -> Self {
        Lzma2Params {
            dictionary_size: 64 * 1024 * 1024,
            algorithm: 1,
            num_fast_bytes: 32,
            match_finder: MF_HT4,
            match_finder_cycles: 0,
            pos_state_bits: 2,
            lit_context_bits: 3,
            lit_pos_bits: 0,
        }
    }
}

/// `bsc:...` (`C_BSC.cpp:218`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BscParams {
    pub block_size: u32,
    pub lzp_hash_size: i32,
    pub lzp_min_len: i32,
    pub block_sorter: i32,
    pub coder: i32,
}

impl Default for BscParams {
    /// `BSC_METHOD::BSC_METHOD()` (`C_BSC.cpp:184`) and libbsc's own defaults:
    /// a 25 MB block, a 15-bit LZP hash, a 72-byte minimum match, BWT, and the
    /// static QLFC coder.
    fn default() -> Self {
        BscParams {
            block_size: 25 * 1024 * 1024,
            lzp_hash_size: 15,
            lzp_min_len: 72,
            block_sorter: 1,
            coder: 1,
        }
    }
}

/// `lz4:...` (`C_LZ4.cpp:143`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lz4Params {
    /// 0 is the fast codec; 1..12 select an HC level.
    pub compressor: i32,
    pub block_size: u32,
    /// Parsed and printed but unused by modern LZ4, exactly as in the C.
    pub hash_size: u32,
    pub min_compression: i32,
}

impl Default for Lz4Params {
    /// `LZ4_METHOD::LZ4_METHOD()` (`C_LZ4.cpp:135`).
    fn default() -> Self {
        Lz4Params { compressor: 0, block_size: 1024 * 1024, hash_size: 0, min_compression: 100 }
    }
}

/// `zstd:LEVEL[:longN][:wN]` (`C_Zstd.cpp:105`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ZstdParams {
    pub level: i32,
    /// 0 leaves the library default; >0 turns on long-range matching.
    pub window_log: u32,
    /// 0 is single-threaded.
    pub workers: u32,
}

impl Default for ZstdParams {
    /// `ZSTD_METHOD::ZSTD_METHOD()` (`C_Zstd.cpp:42`).
    fn default() -> Self {
        ZstdParams { level: 3, window_log: 0, workers: 0 }
    }
}

/// One link of a compression chain.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Method {
    /// `aSTORING` — the block's bytes are its data.
    Storing,
    /// `aFAKE_COMPRESSION` — `--nodata`. The files are not read at all
    /// (`ArcvProcessRead.hs:139`): their sizes are recorded, their CRCs stay
    /// zero, and no data block is written.
    Fake,
    /// `aCRC_ONLY_COMPRESSION` — `--crconly`. The files ARE read and their
    /// CRC32s recorded, but the data is still not stored.
    Crc,
    Lzma(LzmaParams),
    /// `lzma2` — the same LZMA coder inside the chunked LZMA2 container, which
    /// is a different wire format and so a different method, not a parameter.
    Lzma2(Lzma2Params),
    Ppmd(PpmdParams),
    /// Tornado. The decoder reads everything it needs from the stream header;
    /// the parameters here exist so the method string can be printed back.
    Tornado(TornadoParams),
    /// REP. The decoder ignores every parameter (`rep.rs:164`), but the printer
    /// does not, so they are all carried.
    Rep(RepParams),
    Grzip(GrzipParams),
    /// `exe` — the x86 BCJ filter. Takes no parameters at all: `parse_BCJ_X86`
    /// requires `parameters[1] == NULL`.
    Exe,
    Dict(DictParams),
    Lzp(LzpParams),
    Delta(DeltaParams),
    Dispack(DeltaParams),
    /// `tta` — lossless audio. Its `level` defaults to 3.
    Tta(MmParams),
    /// `mm` — the multimedia transform. Its `mode` defaults to 9.
    Mm(MmParams),
    /// `zstd`.
    Zstd(ZstdParams),
    /// `lz4`, fast or HC.
    Lz4(Lz4Params),
    /// `bsc` -- BWT/ST plus QLFC.
    Bsc(BscParams),
    /// `4x4` — the chunking meta-codec every level from -m1 up wraps its real
    /// compressor in. See [`crate::fourx4`].
    FourX4(crate::fourx4::FourX4Params),
    /// An encryption step — `aes-256/ctr:n1000:r0:s…:c…:i…`. Boxed because it
    /// is much the largest variant (four hex strings) and every chain that is
    /// not encrypted would otherwise pay for it.
    ///
    /// A chain read from an archive carries the salt and check code but no key,
    /// so this variant parses fine and cannot decrypt until
    /// [`crate::encryption::generate_decryption`] has put a key into it.
    Encryption(Box<crate::encryption::Encryption>),
    /// A method this port does not decode yet. Carried rather than dropped so
    /// the caller can say *which* method it could not handle, which is the
    /// difference between a useful message and "archive is corrupt".
    Unsupported(String),
}

impl Method {
    /// Parse one `':'`-separated method string.
    ///
    /// Unknown *methods* become [`Method::Unsupported`]; a known method with a
    /// bad *parameter* is `None`, matching `parse_LZMA`'s `if (error) return
    /// NULL` — the C treats that as "not this method", and every other parser
    /// then also refuses, so the whole string is rejected.
    pub fn parse(s: &str) -> Option<Method> {
        let mut parts = s.split(':');
        let name = parts.next().unwrap_or("");
        // The rest of the string, re-joined, is what each parser walks.
        let params: Vec<&str> = parts.collect();
        match name {
            "storing" => Some(Method::Storing),
            "fake" => Some(Method::Fake),
            "crc" => Some(Method::Crc),
            "lzma" => parse_lzma(params.into_iter()).map(Method::Lzma),
            "lzma2" => parse_lzma2(&params).map(Method::Lzma2),
            "ppmd" => parse_ppmd(&params).map(Method::Ppmd),
            "tor" => parse_tornado(&params).map(Method::Tornado),
            "rep" => parse_rep(&params).map(Method::Rep),
            "grzip" => parse_grzip(&params).map(Method::Grzip),
            // parse_BCJ_X86 accepts "exe" ONLY with no parameters.
            "exe" => {
                if params.is_empty() {
                    Some(Method::Exe)
                } else {
                    Some(Method::Unsupported(s.to_string()))
                }
            }
            "dict" => parse_dict(&params).map(Method::Dict),
            "lzp" => parse_lzp(&params).map(Method::Lzp),
            "delta" => parse_delta(&params).map(Method::Delta),
            "dispack" | "dispack070" => parse_delta(&params).map(Method::Dispack),
            // TTA's first field is `m`, MM's is `d`, and MM alone rejects a
            // reorder that is not 0 or 1.
            "tta" => parse_mm_like(&params, 3, b'm', false).map(Method::Tta),
            "mm" => parse_mm_like(&params, 9, b'd', true).map(Method::Mm),
            "zstd" => parse_zstd(&params).map(Method::Zstd),
            "lz4" => parse_lz4(&params).map(Method::Lz4),
            "bsc" => parse_bsc(&params).map(Method::Bsc),
            "4x4" => crate::fourx4::parse(&params).map(Method::FourX4),
            // The C tries every registered parser in turn, and parse_ENCRYPTION
            // is one of them. Splitting on whether the NAME is a cipher keeps
            // the two failure modes apart: an unknown method is carried as
            // Unsupported so the caller can name it, while a cipher with a bad
            // parameter is a rejected string -- no other parser would claim it
            // either.
            _ => match crate::block::is_encryption(name) {
                true => crate::encryption::Encryption::parse(name, &params)
                    .map(|e| Method::Encryption(Box::new(e))),
                false => Some(Method::Unsupported(s.to_string())),
            },
        }
    }

    /// Parse a whole `'+'`-joined chain, in compression order.
    pub fn parse_chain(methods: &[String]) -> Option<Vec<Method>> {
        methods.iter().map(|m| Method::parse(m)).collect()
    }
}

/// `grzip:...` (`C_GRZip.cpp:143`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GrzipParams {
    pub method: u32,
    pub block_size: u32,
    pub enable_lzp: bool,
    pub min_match_len: u32,
    pub hash_size_log: u32,
    pub alternative_bwt_sort: bool,
    pub adaptive_block_size: bool,
    pub delta_filter: bool,
}

impl Default for GrzipParams {
    /// `GRZIP_METHOD::GRZIP_METHOD()` (`C_GRZip.cpp:70`).
    fn default() -> Self {
        GrzipParams {
            method: 1,
            block_size: 8 * 1024 * 1024,
            enable_lzp: true,
            min_match_len: 32,
            hash_size_log: 15,
            alternative_bwt_sort: false,
            adaptive_block_size: false,
            delta_filter: false,
        }
    }
}

/// `rep:...` (`C_REP.cpp:100`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RepParams {
    pub block_size: u32,
    pub min_compression: u32,
    pub min_match_len: u32,
    pub hash_size_log: u32,
    pub barrier: u32,
    pub smallest_len: u32,
    pub amplifier: u32,
}

impl Default for RepParams {
    /// `REP_METHOD::REP_METHOD()` (`C_REP.cpp:13`). `Barrier` is `INT_MAX`.
    fn default() -> Self {
        RepParams {
            block_size: 64 * 1024 * 1024,
            min_compression: 100,
            min_match_len: 512,
            hash_size_log: 0,
            barrier: i32::MAX as u32,
            smallest_len: 512,
            amplifier: 1,
        }
    }
}

/// `tor:...` (`C_Tornado.cpp:85`) — a preset number plus overrides.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TornadoParams {
    pub number: u32,
    pub buffer: u32,
    pub hashsize: u32,
    pub hash_row_width: u32,
    pub encoding_method: u32,
    pub match_parser: u32,
    pub update_step: u32,
    pub find_tables: u32,
    pub auxhash_size: u32,
    pub auxhash_row_width: u32,
    /// The three fields `ShowCompressionMethod` never prints, and that no
    /// method string can set. They come from the preset row alone, and the
    /// ENCODER needs them: they pick which match-finder instantiation runs.
    pub caching_finder: u32,
    pub hash3: u32,
    pub shift: u32,
}

/// `std_Tornado_method[]` (`Tornado.cpp:66`), the fields the printer compares
/// against. The preset number selects a whole row, and any parameter after it
/// overrides one field of that row -- so the defaults a printed method is
/// measured against depend on the preset, not on a single global default.
const TORNADO_PRESETS: [TornadoParams; 12] = [
    // number, buffer, hashsize, row, encoding, parser, update, tables, auxhash, auxrow
    t(0, 1 << 20, 0, 0, 0, 0, 999, 0, 0, 0, 0, 0, 0),
    t(1, 1 << 20, 16 << 10, 1, 1, 1, 999, 0, 0, 0, 0, 0, 0),
    t(2, 2 << 20, 64 << 10, 1, 2, 1, 999, 0, 0, 0, 0, 0, 0),
    t(3, 4 << 20, 128 << 10, 2, 3, 1, 999, 1, 0, 0, 0, 0, 0),
    t(4, 8 << 20, 2 << 20, 2, 3, 1, 999, 1, 0, 0, 1, 0, 0),
    t(5, 16 << 20, 2 << 20, 4, 4, 2, 999, 1, 0, 0, 1, 1, 0),
    t(6, 64 << 20, 32 << 20, 8, 4, 2, 4, 1, 0, 0, 1, 1, 0),
    t(7, 256 << 20, 128 << 20, 32, 4, 2, 1, 1, 128 << 10, 4, 5, 2, 0),
    t(8, 1024 << 20, 512 << 20, 128, 4, 2, 1, 1, 128 << 10, 4, 5, 2, 0),
    t(9, 1024 << 20, 2048 << 20, 256, 4, 2, 1, 1, 512 << 10, 4, 5, 2, 0),
    t(10, 1024 << 20, 2048 << 20, 256, 4, 2, 1, 1, 2 << 20, 32, 6, 2, 0),
    t(11, 1024 << 20, 1600 << 20, 200, 4, 2, 1, 1, 512 << 20, 256, 7, 2, 0),
];

#[allow(clippy::too_many_arguments)]
const fn t(
    number: u32,
    buffer: u32,
    hashsize: u32,
    hash_row_width: u32,
    encoding_method: u32,
    match_parser: u32,
    update_step: u32,
    find_tables: u32,
    auxhash_size: u32,
    auxhash_row_width: u32,
    caching_finder: u32,
    hash3: u32,
    shift: u32,
) -> TornadoParams {
    TornadoParams {
        number,
        buffer,
        hashsize,
        hash_row_width,
        encoding_method,
        match_parser,
        update_step,
        find_tables,
        auxhash_size,
        auxhash_row_width,
        caching_finder,
        hash3,
        shift,
    }
}

/// `default_Tornado_method` — "equivalent to option -5" (`Tornado.cpp:82`).
pub const TORNADO_DEFAULT: usize = 5;

impl TornadoParams {
    /// The preset row this method's `number` came from, which is what
    /// `ShowCompressionMethod` compares each field against.
    pub fn preset(&self) -> TornadoParams {
        TORNADO_PRESETS[(self.number as usize).min(TORNADO_PRESETS.len() - 1)]
    }
}

impl Default for TornadoParams {
    fn default() -> Self {
        TORNADO_PRESETS[TORNADO_DEFAULT]
    }
}

/// `parse_TORNADO` (`C_Tornado.cpp:85`).
fn parse_tornado(params: &[&str]) -> Option<TornadoParams> {
    let mut p = TornadoParams::default();
    for param in params {
        let param: &str = param;
        if param.len() > 1 {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "b" => {
                    p.buffer = parse_mem(rest)?;
                    true
                }
                "h" => {
                    p.hashsize = parse_mem(rest)?;
                    true
                }
                "l" => {
                    p.hash_row_width = parse_int(rest)?;
                    true
                }
                "c" => {
                    p.encoding_method = parse_int(rest)?;
                    true
                }
                "p" => {
                    p.match_parser = parse_int(rest)?;
                    true
                }
                "u" => {
                    p.update_step = parse_int(rest)?;
                    true
                }
                "t" => {
                    p.find_tables = parse_int(rest)?;
                    true
                }
                "a" => match param.get(1..2) {
                    Some("h") => {
                        p.auxhash_size = parse_mem(&param[2..])?;
                        true
                    }
                    Some("l") => {
                        p.auxhash_row_width = parse_int(&param[2..])?;
                        true
                    }
                    Some(_) | None => false,
                },
                _ => false,
            };
            if handled {
                continue;
            }
        }
        // A bare integer selects a whole preset ROW, replacing everything set
        // so far; anything else is the buffer size.
        match parse_int(param) {
            Some(n) => p = TORNADO_PRESETS[(n as usize).min(TORNADO_PRESETS.len() - 1)],
            None => p.buffer = parse_mem(param)?,
        }
    }
    Some(p)
}

/// `parse_GRZIP` (`C_GRZip.cpp:143`).
fn parse_grzip(params: &[&str]) -> Option<GrzipParams> {
    let mut p = GrzipParams::default();
    for param in params {
        let param: &str = param;
        if param.len() == 1 {
            match param {
                "s" => {
                    p.alternative_bwt_sort = true;
                    continue;
                }
                "a" => {
                    p.adaptive_block_size = true;
                    continue;
                }
                "l" => {
                    p.enable_lzp = false;
                    continue;
                }
                "d" => {
                    p.delta_filter = true;
                    continue;
                }
                "p" => {
                    p.adaptive_block_size = false;
                    p.enable_lzp = false;
                    p.delta_filter = true;
                    continue;
                }
                _ => {}
            }
        } else {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "m" => {
                    p.method = parse_int(rest)?;
                    true
                }
                "b" => {
                    p.block_size = parse_mem(rest)?;
                    true
                }
                "l" => {
                    p.min_match_len = parse_int(rest)?;
                    true
                }
                "h" => {
                    p.hash_size_log = parse_int(rest)?;
                    true
                }
                _ => false,
            };
            if handled {
                continue;
            }
        }
        match parse_int(param) {
            Some(n) => p.min_match_len = n,
            None => p.block_size = parse_mem(param)?,
        }
    }
    Some(p)
}

/// `parse_REP` (`C_REP.cpp:100`).
fn parse_rep(params: &[&str]) -> Option<RepParams> {
    let mut p = RepParams::default();
    for param in params {
        let param: &str = param;
        if param.len() > 1 {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "b" => {
                    p.block_size = parse_mem(rest)?;
                    true
                }
                "l" => {
                    p.min_match_len = parse_int(rest)?;
                    true
                }
                "d" => {
                    p.barrier = parse_mem(rest)?;
                    true
                }
                "s" => {
                    p.smallest_len = parse_int(rest)?;
                    true
                }
                "h" => {
                    p.hash_size_log = parse_int(rest)?;
                    true
                }
                "a" => {
                    p.amplifier = parse_int(rest)?;
                    true
                }
                _ => false,
            };
            if handled {
                continue;
            }
        }
        match param.strip_suffix('%') {
            Some(pct) => match parse_int(pct) {
                Some(n) => {
                    p.min_compression = n;
                    continue;
                }
                None => {}
            },
            None => {}
        }
        match parse_int(param) {
            Some(n) => p.min_match_len = n,
            None => p.block_size = parse_mem(param)?,
        }
    }
    Some(p)
}

/// `parse_PPMD` (`C_PPMD.cpp:130`).
///
/// A bare parameter is the ORDER if it parses as an integer, and the memory
/// size otherwise — so `ppmd:10:48mb` is order 10 with 48 MB. An out-of-range
/// order is rejected here rather than reaching the model: "-mppmd:o0" and
/// "-mppmd:o1" once crashed StartModelRare's solid-mode branch.
fn parse_ppmd(params: &[&str]) -> Option<PpmdParams> {
    let mut p = PpmdParams::default();
    for param in params {
        // "mem..." is handled as "m...": the C advances by 2, leaving "m...".
        let param: &str = match param.strip_prefix("me") {
            Some(rest) => rest,
            None => param,
        };
        if param.len() == 1 && param == "r" {
            p.mr_method = 1;
            continue;
        }
        if param.len() > 1 {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "m" => {
                    p.mem = parse_mem(rest)?;
                    true
                }
                "o" => {
                    p.order = parse_int(rest)? as i32;
                    true
                }
                "r" => {
                    p.mr_method = parse_int(rest)? as i32;
                    true
                }
                _ => false,
            };
            if handled {
                continue;
            }
        }
        match parse_int(param) {
            Some(n) => p.order = n as i32,
            None => p.mem = parse_mem(param)?,
        }
    }
    // PPMD_MIN_ORDER / PPMD_MAX_ORDER (C_PPMD.cpp:39).
    if p.order < 2 || p.order > 128 {
        return None;
    }
    Some(p)
}

/// `parse_DICT` (`C_Dict.cpp:86`). Only BlockSize is kept: `dict::decompress`
/// takes nothing else.
fn parse_dict(params: &[&str]) -> Option<DictParams> {
    let mut p = DictParams::default();
    for param in params {
        let param: &str = param;
        if param.len() == 1 {
            // The two presets, which set four fields at once.
            match param {
                "p" => {
                    p.min_large_cnt = 8192;
                    p.min_medium_cnt = 400;
                    p.min_small_cnt = 100;
                    p.min_ratio = 4;
                    continue;
                }
                "f" => {
                    p.min_large_cnt = 2048;
                    p.min_medium_cnt = 100;
                    p.min_small_cnt = 50;
                    p.min_ratio = 0;
                    continue;
                }
                _ => {}
            }
        } else {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "b" => {
                    p.block_size = parse_mem(rest)?;
                    true
                }
                "c" => {
                    p.min_weak_chars = parse_int(rest)?;
                    true
                }
                "l" => {
                    p.min_large_cnt = parse_int(rest)?;
                    true
                }
                "m" => {
                    p.min_medium_cnt = parse_int(rest)?;
                    true
                }
                "s" => {
                    p.min_small_cnt = parse_int(rest)?;
                    true
                }
                "r" => {
                    p.min_ratio = parse_int(rest)?;
                    true
                }
                _ => false,
            };
            if handled {
                continue;
            }
        }
        match param.strip_suffix('%') {
            Some(pct) => match parse_int(pct) {
                Some(n) => {
                    p.min_compression = n;
                    continue;
                }
                None => {}
            },
            None => {}
        }
        // A bare integer is MinWeakChars; anything else is the block size.
        match parse_int(param) {
            Some(n) => p.min_weak_chars = n,
            None => p.block_size = parse_mem(param)?,
        }
    }
    Some(p)
}

/// `parse_TTA` (`C_TTA.cpp:72`) and `parse_MM` (`C_MM.cpp:79`).
///
/// `first` is the letter naming the leading field — `m` for TTA's level, `d`
/// for MM's mode — and `strict_extra` is MM's extra rule that `:r` must be 0
/// or 1, since `:r2` named a transform whose C implementation was `return buf;`
/// and which no decoder accepts.
///
/// The unnamed parameter is `c*w`, optionally suffixed `f`. It is matched with
/// `sscanf("%d*%d%s")`, so a THIRD component that is not exactly `f` is an
/// error rather than being ignored — `2*16x` must not silently become `2*16`.
fn parse_mm_like(
    params: &[&str],
    default_level: i32,
    first: u8,
    strict_extra: bool,
) -> Option<MmParams> {
    let mut p = MmParams {
        level: default_level,
        skip_header: 0,
        is_float: 0,
        num_chan: 0,
        word_size: 0,
        offset: 0,
        extra: 0,
    };
    for param in params {
        let param: &str = param;
        let head = *param.as_bytes().first()?;
        let rest = &param[1..];
        // The named parameters. `s` and `f` are flags and take no value.
        if head == first {
            p.level = parse_int(rest)? as i32;
            continue;
        }
        let handled = match head {
            b's' => {
                p.skip_header = 1;
                true
            }
            b'f' => {
                p.is_float = 1;
                true
            }
            b'c' => {
                p.num_chan = parse_int(rest)? as i32;
                true
            }
            b'w' => {
                p.word_size = parse_int(rest)? as i32;
                true
            }
            b'o' => {
                p.offset = parse_int(rest)? as i32;
                true
            }
            b'r' => {
                p.extra = parse_int(rest)? as i32;
                if strict_extra && p.extra != 0 && p.extra != 1 {
                    return None;
                }
                true
            }
            _ => false,
        };
        if handled {
            continue;
        }
        // `%d*%d` with an optional `f`.
        let (chan, tail) = param.split_once('*')?;
        let a = parse_int(chan)? as i32;
        let digits = tail.chars().take_while(char::is_ascii_digit).count();
        let b = parse_int(&tail[..digits])? as i32;
        // `sscanf("%d*%d%s")==3 && s=="f"` sets the float flag; anything else
        // falls to `sscanf("%d*%d")==2`, which succeeds and DISCARDS the rest.
        // So `2*16x` is `2*16` with the `x` ignored, not an error -- measured,
        // the reference writes an archive for `-mmm:2*16x`. Rejecting it here
        // was the one thing this parser got wrong.
        p.is_float = i32::from(&tail[digits..] == "f");
        p.num_chan = a;
        p.word_size = b;
    }
    Some(p)
}

/// `parse_BSC` (`C_BSC.cpp:218`).
///
/// The bare parameter is the block size, and unlike every other parser here a
/// FAILED `parseMem` still assigns -- the C writes `p->BlockSize = parseMem(...)`
/// and only checks `error` after the loop, so a bad value rejects the method
/// rather than being ignored.
fn parse_bsc(params: &[&str]) -> Option<BscParams> {
    let mut p = BscParams::default();
    for param in params {
        let param: &str = param;
        let (head, rest) = param.split_at(1.min(param.len()));
        let handled = match head {
            "b" => { p.block_sorter = parse_int(rest)? as i32; true }
            "l" => { p.lzp_min_len = parse_int(rest)? as i32; true }
            "h" => { p.lzp_hash_size = parse_int(rest)? as i32; true }
            "c" => { p.coder = parse_int(rest)? as i32; true }
            _ => false,
        };
        if handled {
            continue;
        }
        p.block_size = parse_mem(param)?;
    }
    Some(p)
}

/// `parse_LZ4` (`C_LZ4.cpp:143`).
///
/// Note the trailing `error=1`: unlike `parse_DICT`, an unrecognised parameter
/// here is a hard error rather than a fallback, so `lz4:junk` is not the lz4
/// method at all.
fn parse_lz4(params: &[&str]) -> Option<Lz4Params> {
    let mut p = Lz4Params::default();
    for param in params {
        let param: &str = param;
        if param == "hc" {
            p.compressor = 9;
            continue;
        }
        let (head, rest) = param.split_at(1.min(param.len()));
        let handled = match head {
            "c" => {
                p.compressor = parse_int(rest)? as i32;
                true
            }
            "b" => {
                p.block_size = parse_mem(rest)?;
                true
            }
            "h" => {
                p.hash_size = parse_mem(rest)?;
                true
            }
            _ => false,
        };
        if handled {
            continue;
        }
        match param.strip_suffix('%') {
            Some(pct) => p.min_compression = parse_int(pct)? as i32,
            None => return None,
        }
    }
    Some(p)
}

/// `parse_ZSTD` (`C_Zstd.cpp:105`).
fn parse_zstd(params: &[&str]) -> Option<ZstdParams> {
    let mut p = ZstdParams::default();
    for param in params {
        let param: &str = param;
        match param.strip_prefix("long") {
            Some(rest) => {
                // `zstd:3:long` on its own is REFUSED, not a 27-bit window.
                // C_Zstd.cpp:116 says "long alone enables LDM with w=27", but
                // `parseInt("")` sets error (Common.cpp:197) and the parser
                // then returns NULL, so that line is unreachable -- measured,
                // the reference writes no archive for `-mzstd:3:long`. The 27
                // below is reached only by an explicit `long0`.
                p.window_log = match parse_int(rest)? {
                    0 => 27,
                    n => n,
                };
                continue;
            }
            None => {}
        }
        match param.strip_prefix('w') {
            Some(rest) => {
                p.workers = parse_int(rest)?;
                continue;
            }
            None => {}
        }
        // A bare number is the level, and zero is ignored rather than stored.
        let lvl = parse_int(param)? as i32;
        if lvl != 0 {
            p.level = lvl;
        }
    }
    // "Clamp to zstd's advertised range" -- done at PARSE time, so the clamped
    // value is what gets written into the archive's method string.
    p.level = p.level.clamp(darc_codecs::zstd::min_c_level(), darc_codecs::zstd::max_c_level());
    Some(p)
}

/// `parse_LZP` (`C_LZP.cpp:110`).
fn parse_lzp(params: &[&str]) -> Option<LzpParams> {
    let mut p = LzpParams::default();
    for param in params {
        let param: &str = param;
        if param.len() > 1 {
            let (head, rest) = param.split_at(1);
            let handled = match head {
                "b" => {
                    p.block_size = parse_mem(rest)?;
                    true
                }
                "l" => {
                    p.min_match_len = parse_int(rest)? as i32;
                    true
                }
                "h" => {
                    p.hash_size_log = parse_int(rest)? as i32;
                    true
                }
                "d" => {
                    p.barrier = parse_mem(rest)? as i32;
                    true
                }
                "s" => {
                    p.smallest_len = parse_int(rest)? as i32;
                    true
                }
                _ => false,
            };
            if handled {
                continue;
            }
        }
        match param.strip_suffix('%') {
            Some(pct) => match parse_int(pct) {
                Some(n) => {
                    p.min_compression = n;
                    continue;
                }
                None => {}
            },
            None => {}
        }
        match parse_int(param) {
            Some(n) => p.min_match_len = n as i32,
            None => p.block_size = parse_mem(param)?,
        }
    }
    Some(p)
}

/// `parse_DELTA` (`C_Delta.cpp:72`), which `dispack` shares the shape of.
fn parse_delta(params: &[&str]) -> Option<DeltaParams> {
    let mut p = DeltaParams { block_size: 8 * 1024 * 1024, extended_tables: 0 };
    for param in params {
        let param: &str = param;
        if param == "x" {
            p.extended_tables = 1;
            continue;
        }
        match param.strip_prefix('b') {
            Some(rest) => {
                p.block_size = parse_mem(rest)?;
                continue;
            }
            None => {}
        }
        p.block_size = parse_mem(param)?;
    }
    Some(p)
}

/// `parse_LZMA` (`C_LZMA.cpp:164`), parameter for parameter.
fn parse_lzma<'a, I: Iterator<Item = &'a str>>(params: I) -> Option<LzmaParams> {
    let mut p = LzmaParams::default();
    for param in params {
        match param {
            "max" | "normal" => {
                p.algorithm = 1;
                continue;
            }
            "fast" | "fastest" => {
                p.algorithm = 0;
                continue;
            }
            // "ignored: always write EOS"
            "eos" => continue,
            _ => {}
        }
        // A bare match-finder name, then "mf=NAME", then "mfNAME" -- the C tries
        // all three, in that order.
        match find_match_finder(param) {
            Some(mf) => {
                p.match_finder = mf;
                continue;
            }
            None => {}
        }
        match param.strip_prefix("mf=").or_else(|| param.strip_prefix("mf")) {
            Some(rest) => {
                p.match_finder = find_match_finder(rest)?;
                continue;
            }
            None => {}
        }
        // Two-letter parameters before one-letter ones: 'p' alone is not a
        // parameter, only "pb" is, and 'l' alone is not one either.
        let two = param.get(..2).unwrap_or("");
        let handled = match two {
            "pb" => {
                p.pos_state_bits = parse_int(&param[2..])?;
                true
            }
            "lc" => {
                p.lit_context_bits = parse_int(&param[2..])?;
                true
            }
            "lp" => {
                p.lit_pos_bits = parse_int(&param[2..])?;
                true
            }
            "fb" => {
                p.num_fast_bytes = parse_int(&param[2..])?;
                true
            }
            "mc" => {
                p.match_finder_cycles = parse_int(&param[2..])?;
                true
            }
            _ => false,
        };
        if handled {
            continue;
        }
        match param.chars().next() {
            Some('d') => {
                p.dictionary_size = parse_mem(&param[1..])?;
                continue;
            }
            Some('h') => {
                p.hash_size = parse_mem(&param[1..])?;
                continue;
            }
            Some('a') => {
                p.algorithm = parse_int(&param[1..])?;
                continue;
            }
            // "Arg starts with digit: treat as dictionary size if it has a mem
            // suffix, else fb." The suffix test is on the character AFTER the
            // digits, and '^'/absent is deliberately NOT accepted here -- the C
            // checks only b/k/m/g, so a bare "32" is fast bytes, not 4 GB.
            Some(c) if c.is_ascii_digit() => {
                let digits = param.chars().take_while(char::is_ascii_digit).count();
                let suffix = param[digits..].chars().next();
                match suffix {
                    Some('b') | Some('k') | Some('m') | Some('g') => {
                        match parse_mem(param) {
                            Some(m) => {
                                p.dictionary_size = m;
                                continue;
                            }
                            // "error = 0" -- the C clears the error and falls
                            // through to trying it as a fast-byte count.
                            None => {}
                        }
                    }
                    Some(_) | None => {}
                }
                p.num_fast_bytes = parse_int(param)?;
                continue;
            }
            Some(_) | None => return None,
        }
    }
    Some(p)
}

/// `parse_LZMA2` (`C_LZMA2.cpp:118`).
///
/// Deliberately a separate walk from [`parse_lzma`] rather than a shared one,
/// because the two grammars differ in three places and every difference is a
/// string one side accepts and the other rejects:
///
/// * no `h` — LZMA2 has no hash-size parameter;
/// * `mf=NAME` only. `start_from2(param, "mf=")` is the whole test, so the bare
///   `mfBT4` spelling `parse_LZMA` also takes is an error here;
/// * a bare match-finder name (`BT4`) is still accepted, before either.
fn parse_lzma2(params: &[&str]) -> Option<Lzma2Params> {
    let mut p = Lzma2Params::default();
    for param in params {
        match *param {
            "max" | "normal" => {
                p.algorithm = 1;
                continue;
            }
            "fast" | "fastest" => {
                p.algorithm = 0;
                continue;
            }
            // The end marker is always written, so asking for it is a no-op.
            "eos" => continue,
            _ => {}
        }
        match find_match_finder(param) {
            Some(mf) => {
                p.match_finder = mf;
                continue;
            }
            None => {}
        }
        match param.strip_prefix("mf=") {
            // `if (mf < 0) { error=1; break; }` -- an unknown name after `mf=`
            // rejects the whole string rather than falling through to the
            // switch, which would read the 'm' as the start of `mc`.
            Some(rest) => {
                p.match_finder = find_match_finder(rest)?;
                continue;
            }
            None => {}
        }
        let two = param.get(..2).unwrap_or("");
        let handled = match two {
            "pb" => {
                p.pos_state_bits = parse_int(&param[2..])?;
                true
            }
            "lc" => {
                p.lit_context_bits = parse_int(&param[2..])?;
                true
            }
            "lp" => {
                p.lit_pos_bits = parse_int(&param[2..])?;
                true
            }
            "fb" => {
                p.num_fast_bytes = parse_int(&param[2..])?;
                true
            }
            "mc" => {
                p.match_finder_cycles = parse_int(&param[2..])?;
                true
            }
            _ => false,
        };
        if handled {
            continue;
        }
        match param.chars().next() {
            Some('d') => {
                p.dictionary_size = parse_mem(&param[1..])?;
                continue;
            }
            Some('a') => {
                p.algorithm = parse_int(&param[1..])?;
                continue;
            }
            // As in `parse_lzma`: a leading digit is a dictionary size only
            // when a b/k/m/g suffix follows the digits, and a fast-byte count
            // otherwise. A `d`-less `64m` therefore sets the dictionary, while
            // a bare `64` sets fb.
            Some(c) if c.is_ascii_digit() => {
                let digits = param.chars().take_while(char::is_ascii_digit).count();
                match param[digits..].chars().next() {
                    Some('b') | Some('k') | Some('m') | Some('g') => match parse_mem(param) {
                        Some(m) => {
                            p.dictionary_size = m;
                            continue;
                        }
                        // `error = 0` -- the C clears it and tries fb.
                        None => {}
                    },
                    Some(_) | None => {}
                }
                p.num_fast_bytes = parse_int(param)?;
                continue;
            }
            // Anything else, including a `p`/`l`/`f`/`m` whose second letter did
            // not match above: the C's switch `break`s, the digit test fails on
            // a non-digit, and `error = 1`.
            Some(_) | None => return None,
        }
    }
    Some(p)
}

/// `GetBlockSize` (`Compression.h:341`) — how much data the method processes at
/// one time, and `0` for the ones that stream.
///
/// Only six methods have one, and the default in the base class is what makes
/// the rest zero. This is not a resource knob: `addBlockSizeCrit` turns it into
/// a solid-block boundary, so it decides where blocks END and is therefore
/// archive-visible.
pub fn block_size(m: &Method) -> u64 {
    match m {
        // No block-size constraint: these write no blocks at all.
        Method::Fake | Method::Crc => 0,
        Method::Dict(p) => u64::from(p.block_size),
        Method::Lzp(p) => u64::from(p.block_size),
        Method::Grzip(p) => u64::from(p.block_size),
        Method::Dispack(p) => u64::from(p.block_size),
        Method::FourX4(p) => u64::from(p.block_size),
        // `{return 0;}`, either explicitly or from COMPRESSION_METHOD.
        // `GetBlockSize` is BlockSize for LZ4 (C_LZ4.h:25) and for BSC
        // (C_BSC.h), so both DO cap their solid blocks.
        Method::Lz4(p) => u64::from(p.block_size),
        Method::Bsc(p) => u64::from(p.block_size),
        // TTA and MM are block algorithms internally but report 0 here
        // (C_TTA.h:36, C_MM.h:50), so neither ever caps a solid block; Zstd
        // likewise (C_Zstd.h:44).
        Method::Tta(_)
        | Method::Mm(_)
        | Method::Zstd(_)
        | Method::Storing
        | Method::Lzma(_)
        // `{return 0;}` (C_LZMA2.h:28). LZMA2 does chunk its input internally,
        // but it never asks the archiver for a boundary.
        | Method::Lzma2(_)
        | Method::Ppmd(_)
        | Method::Tornado(_)
        | Method::Rep(_)
        | Method::Exe
        | Method::Delta(_)
        | Method::Encryption(_) => 0,
        // A method this port cannot parse has no parameters to read a block
        // size out of. Zero means "impose no boundary", which leaves the user's
        // -s criteria alone -- the safe direction.
        Method::Unsupported(_) => 0,
    }
}

/// `makeNonSolid` (`ArhiveFileList.hs:334`) — a multimedia method whose files
/// must each get their own block.
///
/// The exception is a chain that already treats its input as one continuous
/// byte sequence, which is what `*8` without `:o` means; then there is nothing
/// to gain from splitting. Both tests are on the raw method STRING, not on
/// parsed parameters, because that is how `substr` reads it.
pub fn make_non_solid(method: &str) -> bool {
    let name = method.split(':').next().unwrap_or("");
    matches!(name, "tta" | "mm" | "jpg")
        && (!method.contains("*8") || method.contains(":o"))
}

/// `isFakeCompressor` (`Compression.hs:84`) — a lone `fake` or `crc` method.
///
/// Together with `aNO_COMPRESSION` this is what short-circuits `splitOneType`:
/// "for fake compressors or -m0 there is no point in splitting the block into
/// parts".
pub fn is_fake_compressor(chain: &[String]) -> bool {
    match chain {
        [one] => matches!(one.split(':').next().unwrap_or(""), "fake" | "crc"),
        _ => false,
    }
}

/// Whether this compressor makes the solid sort order pointless
/// (`Cmdline.hs:635`).
///
/// `getMainCompressor dataCompressor .$ anyf [(==aNO_COMPRESSION),
/// isFakeCompressor, isVeryFastCompressor]` — when it holds, `sort_order` is
/// `""` and files are packed in scan order instead of `"gerpn"`. The reasoning
/// is that grouping similar files only pays if the compressor is slow enough to
/// exploit the similarity; below that the reordering costs more than it saves.
///
/// Only `aNO_COMPRESSION` was implemented, so `-mtor:3` and `-mlzp:8m:64`
/// silently sorted. Nothing caught it: the difftest harnesses drive `-m0`, and
/// `-m0` takes the first clause. It is a FORMAT difference, not a ratio one —
/// the files land in a different order inside the block.
///
/// Both `isFakeCompressor` and `isVeryFastCompressor` are
/// `(method:xs) = … && null xs`: a chain of two methods is neither, however
/// fast its parts are. That is why `-mdict:32k+lzma:1m` still sorts.
pub fn disables_solid_sorting(chain: &[String]) -> bool {
    let only = match chain {
        [one] => one.as_str(),
        _ => return false,
    };
    match only.split(':').next().unwrap_or("") {
        // `aNO_COMPRESSION`, and the fake methods `--nodata`/`--crconly` write.
        "storing" | "fake" | "crc" => true,
        // `m.hash_row_width <= 2` (`C_Tornado.h:20`) — presets 0..4.
        "tor" => match Method::parse(only) {
            Some(Method::Tornado(p)) => p.hash_row_width <= 2,
            Some(_) | None => false,
        },
        // `HashSizeLog <= 15` (`C_LZP.h:26`) — a hash of 128Kb or less.
        "lzp" => match Method::parse(only) {
            Some(Method::Lzp(p)) => p.hash_size_log <= 15,
            Some(_) | None => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The boundaries, both of which are off-by-one-able: Tornado turns over
    /// between preset 4 (row width 2) and 5 (row width 4), and LZP between a
    /// 15-bit hash and a 16-bit one.
    #[test]
    fn only_a_single_fast_method_disables_the_solid_sort() {
        let c = |s: &str| vec![s.to_string()];
        for fast in ["storing", "fake", "crc", "tor:0", "tor:1", "tor:4", "lzp:8m:64:h15"] {
            assert!(disables_solid_sorting(&c(fast)), "{fast} should disable sorting");
        }
        for slow in ["tor:5", "tor:9", "lzp:8m:64:h16", "lzma:1m", "ppmd:8m:o6"] {
            assert!(!disables_solid_sorting(&c(slow)), "{slow} should keep sorting");
        }
        // `null xs`: two methods are never "a very fast compressor", so a chain
        // built out of fast parts still sorts.
        assert!(!disables_solid_sorting(&["tor:1".to_string(), "tor:1".to_string()]));
        assert!(!disables_solid_sorting(&["dict:32k".to_string(), "lzma:1m".to_string()]));
        assert!(!disables_solid_sorting(&[]));
    }

    /// The suffix is ONE character. "16kb" is 16 KiB, and the 'b' is never
    /// looked at -- stripping it first yields "16k" and, in a previous life,
    /// made a whole difftest silently not run.
    #[test]
    fn parse_mem_reads_one_suffix_character_and_ignores_the_rest() {
        assert_eq!(parse_mem("16k"), Some(16 * 1024));
        assert_eq!(parse_mem("16kb"), Some(16 * 1024));
        assert_eq!(parse_mem("1m"), Some(1024 * 1024));
        assert_eq!(parse_mem("1mb"), Some(1024 * 1024));
        assert_eq!(parse_mem("2g"), Some(2 * 1024 * 1024 * 1024));
        assert_eq!(parse_mem("500b"), Some(500));
    }

    /// No suffix means a POWER OF TWO. This is the one that silently sizes a
    /// dictionary at nothing if read as bytes.
    #[test]
    fn parse_mem_with_no_suffix_is_a_power_of_two() {
        assert_eq!(parse_mem("22"), Some(4 * 1024 * 1024));
        assert_eq!(parse_mem("22^"), Some(4 * 1024 * 1024));
        assert_eq!(parse_mem("0"), Some(1));
    }

    #[test]
    fn parse_int_refuses_trailing_junk() {
        assert_eq!(parse_int("32"), Some(32));
        assert_eq!(parse_int("=32"), Some(32));
        assert_eq!(parse_int("32x"), None);
        assert_eq!(parse_int(""), None);
    }

    /// A bare "lzma" is the constructor's defaults, and those are format: they
    /// decide the property bytes for every block written with plain -mlzma.
    #[test]
    fn bare_lzma_is_the_constructor_defaults() {
        assert_eq!(Method::parse("lzma"), Some(Method::Lzma(LzmaParams::default())));
        let d = LzmaParams::default();
        assert_eq!(d.dictionary_size, 64 * 1024 * 1024);
        assert_eq!(d.pos_state_bits, 2);
        assert_eq!(d.lit_context_bits, 3);
        assert_eq!(d.lit_pos_bits, 0);
    }

    /// The exact string a real archive's footer descriptor contains.
    #[test]
    fn the_method_a_real_archive_writes_parses() {
        match Method::parse("lzma:1mb:mf=BT4") {
            Some(Method::Lzma(p)) => {
                assert_eq!(p.dictionary_size, 1024 * 1024);
                assert_eq!(p.match_finder, 2, "BT4");
                // Everything not named keeps its default -- getting this wrong
                // changes the property bytes and decodes to garbage.
                assert_eq!(p.pos_state_bits, 2);
                assert_eq!(p.lit_context_bits, 3);
                assert_eq!(p.lit_pos_bits, 0);
            }
            other => panic!("did not parse: {other:?}"),
        }
    }

    #[test]
    fn the_three_match_finder_spellings_all_work() {
        let by_name = Method::parse("lzma:bt4");
        let by_eq = Method::parse("lzma:mf=bt4");
        let by_prefix = Method::parse("lzma:mfbt4");
        assert_eq!(by_name, by_eq);
        assert_eq!(by_eq, by_prefix);
    }

    /// A digit argument is a dictionary size only with a b/k/m/g suffix;
    /// otherwise it is a fast-byte count. "lzma:32" must not become a 4 GB
    /// dictionary.
    #[test]
    fn a_bare_number_is_fast_bytes_not_a_dictionary_size() {
        match Method::parse("lzma:32") {
            Some(Method::Lzma(p)) => {
                assert_eq!(p.num_fast_bytes, 32);
                assert_eq!(p.dictionary_size, 64 * 1024 * 1024, "default, untouched");
            }
            other => panic!("{other:?}"),
        }
        match Method::parse("lzma:8m") {
            Some(Method::Lzma(p)) => {
                assert_eq!(p.dictionary_size, 8 * 1024 * 1024);
                assert_eq!(p.num_fast_bytes, 32, "default, untouched");
            }
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn the_bit_parameters_parse_and_reach_the_props() {
        match Method::parse("lzma:pb0:lc0:lp2") {
            Some(Method::Lzma(p)) => {
                assert_eq!(p.pos_state_bits, 0);
                assert_eq!(p.lit_context_bits, 0);
                assert_eq!(p.lit_pos_bits, 2);
                // (0*5 + 2)*9 + 0
                assert_eq!(p.props()[0], 18);
            }
            other => panic!("{other:?}"),
        }
    }

    /// props() is the whole reason this module exists: DArc writes no .lzma
    /// header, so these five bytes come only from the method string.
    #[test]
    fn props_encode_the_defaults_and_the_dictionary_size() {
        let p = LzmaParams::default();
        // (2*5 + 0)*9 + 3 = 93
        assert_eq!(p.props()[0], 93);
        assert_eq!(&p.props()[1..], &(64u32 * 1024 * 1024).to_le_bytes());
    }

    #[test]
    fn an_unparseable_parameter_rejects_the_whole_method() {
        assert_eq!(Method::parse("lzma:nonsense"), None);
        assert_eq!(Method::parse("lzma:mf=nosuch"), None);
    }

    /// An unknown method is named, not swallowed: "cannot decode ecm" is a
    /// usable message and "archive is corrupt" is not.
    ///
    /// `tta:1` used to stand here, and stopped being unsupported when TTA got a
    /// variant. `ecm` is one of the external precompressors -- listed in
    /// builtinMethodSubsts under -max, and not something this port can run.
    #[test]
    fn an_unknown_method_is_carried_by_name() {
        assert_eq!(
            Method::parse("ecm"),
            Some(Method::Unsupported("ecm".to_string()))
        );
    }

    #[test]
    fn a_chain_parses_in_compression_order() {
        let chain = vec!["delta".to_string(), "lzma:1mb".to_string()];
        let got = Method::parse_chain(&chain).expect("parses");
        assert_eq!(got.len(), 2);
        assert_eq!(
            got[0],
            Method::Delta(DeltaParams { block_size: 8 * 1024 * 1024, extended_tables: 0 })
        );
        assert_eq!(got[1], Method::Lzma(LzmaParams { dictionary_size: 1 << 20, ..Default::default() }));
    }

    /// Every method that appears in an archive DArc writes at its default
    /// levels. Taken from `arc lt` output for -m0..-m9, -mtor and -mppmd, so
    /// this is what the archives actually contain rather than what the manual
    /// says. A method landing in Unsupported here means `arc t` cannot read a
    /// default archive.
    #[test]
    fn the_methods_real_archives_use_all_parse() {
        let real = [
            "storing",
            "tor:434kb",
            "tor:3:434kb",
            "ppmd:10:48mb",
            "ppmd:8:96mb",
            "ppmd:22:1gb",
            "rep:379kb",
            "exe",
            "delta",
            "lzma:379kb",
            "lzma:379kb:a0:mc8",
            "lzma:379kb:mc16",
            "lzma:1mb:mf=BT4",
            "dict:56kb:85%:l8192:m400:s100",
            "dict:56kb:80%:l8192:m400:s100",
            "lzp:56kb:92%:24:h16",
            "lzp:56kb:90%:65:h16:d1mb:s16",
            "lzp:56kb:92%:105:h16:d1mb",
            "lzp:56kb:92%:235:h16:d1mb",
            "grzip:56kb:m4:l32:h15",
            "grzip:56kb:m3:l",
        ];
        for m in real {
            match Method::parse(m) {
                Some(Method::Unsupported(name)) => panic!("{name} is not supported"),
                Some(_) => {}
                None => panic!("{m} did not parse at all"),
            }
        }
    }

    /// "exe" takes no parameters -- parse_BCJ_X86 requires parameters[1] to be
    /// NULL. "exe:1" is a different, unknown method, not a BCJ filter with an
    /// argument.
    #[test]
    fn exe_with_parameters_is_not_the_bcj_filter() {
        assert_eq!(Method::parse("exe"), Some(Method::Exe));
        assert_eq!(
            Method::parse("exe:1"),
            Some(Method::Unsupported("exe:1".to_string()))
        );
    }

    /// A bare number is the ORDER for ppmd, and a suffixed one is the memory --
    /// so "ppmd:10:48mb" is order 10, not 10 bytes of memory.
    #[test]
    fn ppmd_takes_a_bare_number_as_the_order() {
        assert_eq!(
            Method::parse("ppmd:10:48mb"),
            Some(Method::Ppmd(PpmdParams { order: 10, mem: 48 * 1024 * 1024, mr_method: 0 }))
        );
        // Out of range, and refused here rather than in the model.
        assert_eq!(Method::parse("ppmd:o1"), None);
        assert_eq!(Method::parse("ppmd:o129"), None);
        assert!(Method::parse("ppmd:o2").is_some());
        assert!(Method::parse("ppmd:o128").is_some());
    }

    /// LZP's barrier defaults to INT_MAX -- "no barrier". A zero default would
    /// change what the decoder reconstructs, silently.
    #[test]
    fn lzp_defaults_carry_an_infinite_barrier() {
        match Method::parse("lzp:56kb:92%:24:h16") {
            Some(Method::Lzp(p)) => {
                assert_eq!(p.block_size, 56 * 1024);
                assert_eq!(p.min_match_len, 24, "the bare number is MinMatchLen");
                assert_eq!(p.hash_size_log, 16);
                assert_eq!(p.barrier, i32::MAX, "no barrier, not zero");
                assert_eq!(p.smallest_len, 32);
            }
            other => panic!("{other:?}"),
        }
    }
}

#[cfg(test)]
mod lzma2_tests {
    use super::{Lzma2Params, Method};

    fn p(s: &str) -> Lzma2Params {
        match Method::parse(s) {
            Some(Method::Lzma2(p)) => p,
            other => panic!("{s} parsed as {other:?}"),
        }
    }

    /// A bare `lzma2` is the constructor's values, and those are format: they
    /// are what every `-mlzma2` block is written with.
    #[test]
    fn bare_lzma2_is_the_constructor_defaults() {
        let d = Lzma2Params::default();
        assert_eq!(Method::parse("lzma2"), Some(Method::Lzma2(d)));
        assert_eq!(d.dictionary_size, 64 * 1024 * 1024);
        assert_eq!(d.num_fast_bytes, 32);
        assert_eq!(d.match_finder, 4);
        assert_eq!(d.pos_state_bits, 2);
        assert_eq!(d.lit_context_bits, 3);
    }

    /// The digit rule: a memory suffix makes it a dictionary, its absence makes
    /// it a fast-byte count. Getting this backwards silently rewrites the
    /// dictionary of every `-mlzma2:64` archive.
    #[test]
    fn a_leading_digit_is_a_dictionary_only_with_a_unit() {
        assert_eq!(p("lzma2:1m").dictionary_size, 1024 * 1024);
        assert_eq!(p("lzma2:1m").num_fast_bytes, 32);
        assert_eq!(p("lzma2:64").num_fast_bytes, 64);
        assert_eq!(p("lzma2:64").dictionary_size, 64 * 1024 * 1024);
        assert_eq!(p("lzma2:d64k").dictionary_size, 64 * 1024);
    }

    /// Where the grammar differs from LZMA's, and each difference is a string
    /// one accepts and the other rejects.
    #[test]
    fn lzma2_grammar_is_not_lzmas() {
        // No `h`: LZMA takes a hash size, LZMA2 has no such field.
        assert!(Method::parse("lzma:h4m").is_some());
        assert_eq!(Method::parse("lzma2:h4m"), None);
        // `mf=NAME` only -- `start_from2(param,"mf=")` is the whole test.
        assert_eq!(p("lzma2:mf=BT4").match_finder, 2);
        assert_eq!(Method::parse("lzma2:mfBT4"), None);
        // A bare match-finder name is still taken, before either.
        assert_eq!(p("lzma2:BT2").match_finder, 0);
        assert_eq!(p("lzma2:hc4").match_finder, 3);
        // An unknown name after `mf=` rejects the whole string rather than
        // falling through to reading the 'm' as `mc`.
        assert_eq!(Method::parse("lzma2:mf=nope"), None);
    }

    #[test]
    fn lzma2_named_parameters() {
        assert_eq!(p("lzma2:a0").algorithm, 0);
        assert_eq!(p("lzma2:fast").algorithm, 0);
        assert_eq!(p("lzma2:fastest").algorithm, 0);
        assert_eq!(p("lzma2:max").algorithm, 1);
        assert_eq!(p("lzma2:normal").algorithm, 1);
        assert_eq!(p("lzma2:fb128").num_fast_bytes, 128);
        assert_eq!(p("lzma2:mc48").match_finder_cycles, 48);
        assert_eq!(p("lzma2:pb1").pos_state_bits, 1);
        assert_eq!(p("lzma2:lc0").lit_context_bits, 0);
        assert_eq!(p("lzma2:lp1").lit_pos_bits, 1);
        // "eos" is accepted and ignored: the end marker is always written.
        assert_eq!(Method::parse("lzma2:eos"), Method::parse("lzma2"));
        // A `p`, `l`, `f` or `m` whose second letter is not one of the known
        // pairs is an error, not a silently ignored parameter.
        assert_eq!(Method::parse("lzma2:px1"), None);
        assert_eq!(Method::parse("lzma2:zz"), None);
    }

    /// Parse then print must round-trip, because the printed string is what
    /// goes into the archive and what a later read parses back.
    #[test]
    fn lzma2_round_trips_through_the_printer() {
        for s in [
            "lzma2",
            "lzma2:1mb",
            "lzma2:64kb:a0",
            "lzma2:1mb:fb128",
            "lzma2:1mb:mf=BT4",
            "lzma2:1mb:mc48",
            "lzma2:1mb:pb1:lc1:lp1",
        ] {
            let first = crate::canonize::show(&Method::parse(s).expect(s));
            let again = crate::canonize::show(&Method::parse(&first).expect(&first));
            assert_eq!(first, again, "{s} did not round-trip");
        }
        // The canonical spelling of the defaults, which is what a bare -mlzma2
        // stores.
        assert_eq!(crate::canonize::show(&Method::parse("lzma2").unwrap()), "lzma2:64mb");
        assert_eq!(
            crate::canonize::show(&Method::parse("lzma2:1m:BT4:fb64").unwrap()),
            "lzma2:1mb:fb64:mf=BT4"
        );
    }
}
