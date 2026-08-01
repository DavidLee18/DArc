//! Canonical method strings — `ShowCompressionMethod`, the inverse of
//! [`crate::method`]'s parsers.
//!
//! `CanonizeCompressionMethod` (`CompressionLibrary.cpp:151`) is exactly
//! *parse, then print*. The printed form is what an archive stores, so this is
//! not cosmetic: `-mlzma` on the command line becomes `lzma:64mb` in the
//! descriptor, and a writer that stores the user's spelling instead writes an
//! archive no other build reproduces.
//!
//! # Every printer omits parameters equal to their default
//!
//! `LZMA_METHOD::ShowCompressionMethod` (`C_LZMA.cpp:146`) constructs a
//! `defaults` object and emits a field only when it differs. So the canonical
//! form of a method is the *shortest* string that parses back to it — which is
//! why `lzma:1mb:mf=BT4` has no `:fb32`, even though 32 is what it means.
//!
//! Round-tripping is therefore the property worth testing: `parse(print(m))`
//! must equal `m` for every method, and it is checked below over a spread of
//! parameter combinations.

use crate::method::{DeltaParams, DictParams, LzmaParams, LzpParams, Method, PpmdParams};

/// `showMem` (`Common.cpp:223`) — the largest unit that divides exactly.
///
/// Note the order: gb, then mb, then kb, then bytes. `1048576` prints as `1mb`,
/// never `1024kb`. And zero divides by everything, so it prints as `0gb` —
/// reproduced deliberately, because a writer that prints `0b` produces a string
/// the C would not have written.
pub fn show_mem(mem: u32) -> String {
    const KB: u32 = 1024;
    const MB: u32 = 1024 * 1024;
    const GB: u32 = 1024 * 1024 * 1024;
    if mem % GB == 0 {
        format!("{}gb", mem / GB)
    } else if mem % MB == 0 {
        format!("{}mb", mem / MB)
    } else if mem % KB == 0 {
        format!("{}kb", mem / KB)
    } else {
        format!("{mem}b")
    }
}

/// `kMatchFinderIDs` (`C_LZMA.cpp:16`) — printed in upper case, though
/// `FindMatchFinder` accepts any case.
fn match_finder_name(id: u32) -> &'static str {
    match id {
        0 => "BT2",
        1 => "BT3",
        2 => "BT4",
        3 => "HC4",
        _ => "HT4",
    }
}

/// The canonical string for one method.
pub fn show(method: &Method) -> String {
    match method {
        Method::Storing => "storing".to_string(),
        Method::Lzma(p) => show_lzma(p),
        Method::Ppmd(p) => show_ppmd(p),
        Method::Delta(p) => show_delta("delta", p),
        // `dispack070` is the name parse_DISPACK prints (C_DisPack.cpp:131).
        Method::Dispack(p) => show_delta("dispack070", p),
        Method::Lzp(p) => show_lzp(p),
        Method::Dict(p) => show_dict(p),
        Method::Exe => "exe".to_string(),
        Method::Tornado(p) => show_tornado(p),
        Method::Rep(p) => show_rep(p),
        Method::Grzip(p) => show_grzip(p),
        Method::FourX4(p) => show_fourx4(p),
        Method::Unsupported(s) => s.clone(),
    }
}

/// `LZMA_METHOD::ShowCompressionMethod` (`C_LZMA.cpp:146`).
fn show_lzma(p: &LzmaParams) -> String {
    let d = LzmaParams::default();
    let mut s = format!("lzma:{}", show_mem(p.dictionary_size));
    if p.algorithm != d.algorithm {
        s += &format!(":a{}", p.algorithm);
    }
    if p.num_fast_bytes != d.num_fast_bytes {
        s += &format!(":fb{}", p.num_fast_bytes);
    }
    if p.match_finder != d.match_finder {
        s += &format!(":mf={}", match_finder_name(p.match_finder));
    }
    if p.match_finder_cycles != d.match_finder_cycles {
        s += &format!(":mc{}", p.match_finder_cycles);
    }
    if p.pos_state_bits != d.pos_state_bits {
        s += &format!(":pb{}", p.pos_state_bits);
    }
    if p.lit_context_bits != d.lit_context_bits {
        s += &format!(":lc{}", p.lit_context_bits);
    }
    if p.lit_pos_bits != d.lit_pos_bits {
        s += &format!(":lp{}", p.lit_pos_bits);
    }
    s
}

/// `PPMD_METHOD::ShowCompressionMethod` (`C_PPMD.cpp:103`).
///
/// Unlike the others this prints the order and memory *unconditionally*, so
/// `ppmd` canonicalises to `ppmd:10:48mb` rather than to itself.
fn show_ppmd(p: &PpmdParams) -> String {
    let suffix = match p.mr_method {
        2 => ":r2",
        1 => ":r",
        _ => "",
    };
    format!("ppmd:{}:{}{}", p.order, show_mem(p.mem), suffix)
}

/// `DELTA_METHOD` / `DISPACK_METHOD::ShowCompressionMethod`.
fn show_delta(name: &str, p: &DeltaParams) -> String {
    let mut s = name.to_string();
    if p.block_size != 8 * 1024 * 1024 {
        s += &format!(":{}", show_mem(p.block_size));
    }
    if p.extended_tables != 0 {
        s += ":x";
    }
    s
}

/// `LZP_METHOD::ShowCompressionMethod` (`C_LZP.cpp:76`).
///
/// MinMatchLen and HashSizeLog are printed unconditionally, and in that order,
/// before the optional barrier and smallest-length fields.
fn show_lzp(p: &LzpParams) -> String {
    let d = LzpParams::default();
    let mut s = format!("lzp:{}", show_mem(p.block_size));
    // MinCompression comes immediately after the block size and BEFORE the
    // unconditional match length -- `"lzp:%s%s:%d:h%d%s%s"`. It never reaches
    // the decoder, which is why it was initially left unmodelled; that made
    // four of sixteen real method strings fail to round-trip.
    if p.min_compression != d.min_compression {
        s += &format!(":{}%", p.min_compression);
    }
    s += &format!(":{}:h{}", p.min_match_len, p.hash_size_log);
    if p.barrier != d.barrier {
        s += &format!(":d{}", show_mem(p.barrier as u32));
    }
    if p.smallest_len != d.smallest_len {
        s += &format!(":s{}", p.smallest_len);
    }
    s
}

/// `DICT_METHOD::ShowCompressionMethod` (`C_Dict.cpp:67`).
fn show_dict(p: &DictParams) -> String {
    let d = DictParams::default();
    let mut s = format!("dict:{}", show_mem(p.block_size));
    if p.min_compression != d.min_compression {
        s += &format!(":{}%", p.min_compression);
    }
    if p.min_weak_chars != d.min_weak_chars {
        s += &format!(":c{}", p.min_weak_chars);
    }
    if p.min_large_cnt != d.min_large_cnt {
        s += &format!(":l{}", p.min_large_cnt);
    }
    if p.min_medium_cnt != d.min_medium_cnt {
        s += &format!(":m{}", p.min_medium_cnt);
    }
    if p.min_small_cnt != d.min_small_cnt {
        s += &format!(":s{}", p.min_small_cnt);
    }
    if p.min_ratio != d.min_ratio {
        s += &format!(":r{}", p.min_ratio);
    }
    s
}

/// `GRZIP_METHOD::ShowCompressionMethod` (`C_GRZip.cpp:124`).
///
/// Nothing here is conditional on a default: the block size, the method number
/// and the LZP clause are always printed. The LZP clause is `l<len>:h<log>` when
/// LZP is on and a bare `l` when it is off -- the same letter meaning two
/// different things.
fn show_grzip(p: &crate::method::GrzipParams) -> String {
    let lzp = if p.enable_lzp {
        format!("l{}:h{}", p.min_match_len, p.hash_size_log)
    } else {
        "l".to_string()
    };
    format!(
        "grzip:{}:m{}:{}{}{}{}",
        show_mem(p.block_size),
        p.method,
        lzp,
        if p.alternative_bwt_sort { ":s" } else { "" },
        if p.adaptive_block_size { ":a" } else { "" },
        if p.delta_filter { ":d" } else { "" }
    )
}

/// `REP_METHOD::ShowCompressionMethod` (`C_REP.cpp:49`).
///
/// Note the field ORDER: block size, compression percentage, match length,
/// barrier, smallest length, hash size, amplifier -- which is not the order the
/// parser accepts them in.
fn show_rep(p: &crate::method::RepParams) -> String {
    let d = crate::method::RepParams::default();
    let mut s = format!("rep:{}", show_mem(p.block_size));
    if p.min_compression != d.min_compression {
        s += &format!(":{}%", p.min_compression);
    }
    if p.min_match_len != d.min_match_len {
        s += &format!(":{}", p.min_match_len);
    }
    if p.barrier != d.barrier {
        s += &format!(":d{}", show_mem(p.barrier));
    }
    if p.smallest_len != d.smallest_len {
        s += &format!(":s{}", p.smallest_len);
    }
    if p.hash_size_log != d.hash_size_log {
        s += &format!(":h{}", p.hash_size_log);
    }
    if p.amplifier != d.amplifier {
        s += &format!(":a{}", p.amplifier);
    }
    s
}

/// `TORNADO_METHOD::ShowCompressionMethod` (`C_Tornado.cpp:60`).
///
/// Every field is compared against **that preset's row**, not against a single
/// global default -- `struct PackMethod defaults = std_Tornado_method[m.number]`.
/// So `tor:6:379kb:h2mb` prints the buffer and hash size because they differ
/// from preset 6's, and prints nothing else.
///
/// The preset number itself is printed only when it is not
/// `default_Tornado_method`, which is 5.
fn show_tornado(p: &crate::method::TornadoParams) -> String {
    let d = p.preset();
    let mut s = "tor".to_string();
    if p.number as usize != crate::method::TORNADO_DEFAULT {
        s += &format!(":{}", p.number);
    }
    // The buffer is printed unconditionally, unlike every other field.
    s += &format!(":{}", show_mem(p.buffer));
    if p.hashsize != d.hashsize {
        s += &format!(":h{}", show_mem(p.hashsize));
    }
    if p.hash_row_width != d.hash_row_width {
        s += &format!(":l{}", p.hash_row_width);
    }
    if p.encoding_method != d.encoding_method {
        s += &format!(":c{}", p.encoding_method);
    }
    if p.match_parser != d.match_parser {
        s += &format!(":p{}", p.match_parser);
    }
    if p.update_step != d.update_step {
        s += &format!(":u{}", p.update_step);
    }
    if p.find_tables != d.find_tables {
        s += &format!(":t{}", p.find_tables);
    }
    if p.auxhash_size != d.auxhash_size {
        s += &format!(":ah{}", show_mem(p.auxhash_size));
    }
    if p.auxhash_row_width != d.auxhash_row_width {
        s += &format!(":al{}", p.auxhash_row_width);
    }
    s
}

/// `_4x4_METHOD::ShowCompressionMethod` (`C_4x4.cpp:578`).
///
/// Note the order: the thread count is attached to the NAME (`4x4:t8`), the
/// block size follows as `:b…`, and the inner method comes last — and the inner
/// method is printed as it was given, not canonicalised, because the C stores it
/// as a string.
fn show_fourx4(p: &crate::fourx4::FourX4Params) -> String {
    let t = if p.num_threads != 0 { format!(":t{}", p.num_threads) } else { String::new() };
    let b =
        if p.block_size != 0 { format!(":b{}", show_mem(p.block_size)) } else { String::new() };
    format!("4x4{t}{b}:{}", p.inner_name)
}

/// `CanonizeCompressionMethod` — parse, then print.
///
/// `None` when the string does not parse, matching the C's
/// `FREEARC_ERRCODE_INVALID_COMPRESSOR`.
pub fn canonize(method: &str) -> Option<String> {
    Method::parse(method).map(|m| show(&m))
}

/// Canonicalise a whole `'+'`-joined chain.
pub fn canonize_chain(chain: &str) -> Option<String> {
    let parts: Result<Vec<String>, ()> =
        chain.split('+').map(|m| canonize(m).ok_or(())).collect();
    parts.ok().map(|v| v.join("+"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// showMem picks the LARGEST exact unit, so 1 MiB is "1mb" and never
    /// "1024kb". Getting this wrong changes the descriptor of every block.
    #[test]
    fn show_mem_picks_the_largest_exact_unit() {
        assert_eq!(show_mem(1024), "1kb");
        assert_eq!(show_mem(1024 * 1024), "1mb");
        assert_eq!(show_mem(64 * 1024 * 1024), "64mb");
        assert_eq!(show_mem(1024 * 1024 * 1024), "1gb");
        assert_eq!(show_mem(379 * 1024), "379kb");
        assert_eq!(show_mem(1000), "1000b");
        assert_eq!(show_mem(1536 * 1024), "1536kb", "1.5mb is not an exact mb");
    }

    /// Zero divides by every unit, so the first test wins and it prints "0gb".
    /// Reproduced rather than corrected: `0b` is a string the C never writes.
    #[test]
    fn zero_prints_as_the_first_unit_that_divides_it() {
        assert_eq!(show_mem(0), "0gb");
    }

    /// The property that matters: canonicalising is idempotent, and the result
    /// parses back to the same method. A printer that drops a field the parser
    /// needs fails here.
    #[test]
    fn canonical_forms_round_trip_and_are_idempotent() {
        let cases = [
            "storing",
            "lzma",
            "lzma:1mb",
            "lzma:1mb:mf=BT4",
            "lzma:379kb:mc16",
            "lzma:8mb:a0:fb64:mf=HC4:pb1:lc1:lp1",
            "ppmd",
            "ppmd:10:48mb",
            "ppmd:22:1gb",
            "ppmd:12:192mb:r",
            "delta",
            "delta:16mb",
            "delta:x",
            "exe",
            "dispack070",
            "lzp:56kb:24:h16",
            "lzp:1mb:65:h16:d1mb:s16",
            "dict:56kb",
            "4x4:tor:3:434kb",
            "4x4:b8mb:lzma:379kb:a0:mc8",
            "4x4:t4:b1mb:tor",
        ];
        for c in cases {
            let once = canonize(c).unwrap_or_else(|| panic!("{c} did not parse"));
            let twice = canonize(&once).unwrap_or_else(|| panic!("{once} did not re-parse"));
            assert_eq!(once, twice, "canonicalising {c} is not idempotent");
            assert_eq!(
                crate::method::Method::parse(&once),
                crate::method::Method::parse(c),
                "the canonical form of {c} is a different method"
            );
        }
    }

    /// Defaults are omitted. `lzma:1mb:mf=BT4` carries no `:fb32` even though 32
    /// is what it means -- the canonical form is the SHORTEST string that parses
    /// back to the same method.
    #[test]
    fn default_valued_parameters_are_not_printed() {
        assert_eq!(canonize("lzma:1mb:fb32"), Some("lzma:1mb".to_string()));
        assert_eq!(canonize("lzma:1mb:pb2:lc3:lp0"), Some("lzma:1mb".to_string()));
        assert_eq!(canonize("lzma:1mb:mf=HT4"), Some("lzma:1mb".to_string()), "HT4 is the default");
        assert_eq!(canonize("delta:8mb"), Some("delta".to_string()));
    }

    /// ...but PPMd prints its order and memory unconditionally, so a bare
    /// "ppmd" does NOT canonicalise to itself.
    #[test]
    fn ppmd_always_prints_its_order_and_memory() {
        assert_eq!(canonize("ppmd"), Some("ppmd:10:48mb".to_string()));
        assert_eq!(canonize("ppmd:o12:m192m:r2"), Some("ppmd:12:192mb:r2".to_string()));
    }

    /// A bare "lzma" is a 64 MB dictionary, and the canonical form says so --
    /// which is what an archive descriptor would contain.
    #[test]
    fn a_bare_method_canonicalises_to_its_defaults() {
        assert_eq!(canonize("lzma"), Some("lzma:64mb".to_string()));
    }

    /// The strings real archives contain must be fixed points: the reference
    /// produced them BY canonicalising, so canonicalising them again must
    /// return them unchanged. That makes the reference's own archives an oracle
    /// with no C harness to build.
    ///
    /// rust/difftest/arc-canonize-check.sh runs this over every string
    /// harvested from `arc lt` across all ten -m levels, so it cannot go stale.
    /// The sample below is the same check in-tree, and it is what caught
    /// `lzp`'s compression percentage: four of sixteen real strings failed to
    /// round-trip because the field never reaches the decoder and had been left
    /// unmodelled.
    #[test]
    fn the_strings_real_archives_contain_are_fixed_points() {
        let from_archives = [
            "storing",
            "lzma:1mb:mf=BT4",
            "lzma:379kb",
            "lzma:379kb:mc16",
            "lzma:379kb:a0:mc8",
            "ppmd:10:48mb",
            "ppmd:8:96mb",
            "ppmd:22:1gb",
            "delta",
            "exe",
            "dispack070",
            "tor:434kb",
            "grzip:56kb:m4:l32:h15",
            "grzip:56kb:m3:l",
            "rep:379kb",
            "dict:56kb:80%:l8192:m400:s100",
            "lzp:56kb:90%:65:h16:d1mb:s16",
            "lzp:56kb:92%:24:h16",
            "4x4:tor:3:434kb",
            "4x4:b16mb:lzma:379kb:mc16",
        ];
        for m in from_archives {
            assert_eq!(
                canonize(m),
                Some(m.to_string()),
                "{m} is not a fixed point, so the reference would not have written it"
            );
        }
    }

    #[test]
    fn a_chain_canonicalises_link_by_link() {
        assert_eq!(
            canonize_chain("delta+lzma:1mb:fb32"),
            Some("delta+lzma:1mb".to_string())
        );
    }

    #[test]
    fn an_unparseable_method_has_no_canonical_form() {
        assert_eq!(canonize("lzma:nonsense"), None);
        assert_eq!(canonize_chain("delta+lzma:nonsense"), None);
    }
}
