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

/// One link of a compression chain.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Method {
    /// `aSTORING` — the block's bytes are its data.
    Storing,
    Lzma(LzmaParams),
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
        match name {
            "storing" => Some(Method::Storing),
            "lzma" => parse_lzma(parts).map(Method::Lzma),
            _ => Some(Method::Unsupported(s.to_string())),
        }
    }

    /// Parse a whole `'+'`-joined chain, in compression order.
    pub fn parse_chain(methods: &[String]) -> Option<Vec<Method>> {
        methods.iter().map(|m| Method::parse(m)).collect()
    }
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

#[cfg(test)]
mod tests {
    use super::*;

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

    /// An unknown method is named, not swallowed: "cannot decode tta" is a
    /// usable message and "archive is corrupt" is not.
    #[test]
    fn an_unknown_method_is_carried_by_name() {
        assert_eq!(
            Method::parse("tta:1"),
            Some(Method::Unsupported("tta:1".to_string()))
        );
    }

    #[test]
    fn a_chain_parses_in_compression_order() {
        let chain = vec!["delta".to_string(), "lzma:1mb".to_string()];
        let got = Method::parse_chain(&chain).expect("parses");
        assert_eq!(got.len(), 2);
        assert_eq!(got[0], Method::Unsupported("delta".to_string()));
        match got[1] {
            Method::Lzma(p) => assert_eq!(p.dictionary_size, 1024 * 1024),
            ref other @ (Method::Storing | Method::Unsupported(_)) => panic!("{other:?}"),
        }
    }
}
