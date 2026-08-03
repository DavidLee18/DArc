//! `-sc`/`--charset`, and the list files it exists to decode —
//! `parseCharsetOption` (`Charsets.hs:108`).
//!
//! # Two spellings, one option
//!
//! The option accepts RAR's form and 7-Zip's, distinguished by their first
//! character:
//!
//! ```text
//!   -sc8l        RAR:    charset '8' for domain 'l'
//!   -sclutf8     7-Zip:  domain 'l', charset named "utf8"
//! ```
//!
//! They cannot be confused because the two alphabets are disjoint: charsets are
//! `0 8 u o a` and domains are `s l c f d t p i`. In the RAR form the domain
//! list defaults to `"cl"`, so a bare `-sc8` sets comment files and list files
//! together.
//!
//! # The domains, and which of them this port applies
//!
//! | | | applied here |
//! |---|---|---|
//! | `l`, `s` | list files (`@listfile`, `-n@f`, `-x@f`) | yes |
//! | `c` | comment files (`-z FILE`) | yes |
//! | `f` | file names in the filesystem | no |
//! | `d` | file names in the archive directory | no |
//! | `t` | the terminal | no |
//! | `p` | command-line arguments | no |
//! | `i` | ini and group files | no |
//!
//! The five unapplied ones all default to UTF-8 on Unix, which is what Rust's
//! `String` already is, so leaving them alone is correct *at the default*. A
//! `-sc` that changes one is REFUSED rather than ignored — the same stance the
//! rest of this port takes, and for the same reason.
//!
//! # On Unix there are only three distinct charsets
//!
//! `aCHARSETS` lists five, but `oem2unicode` and `ansi2unicode` are `id` off
//! Windows (`Charsets.hs:302`), so `o` and `a` behave exactly as `0` does.
//! Reproduced rather than collapsed, so a method that names them round-trips.

/// A `Domain` — which kind of text a charset applies to.
pub type Domain = char;

/// A `Charset`, as the single letter the option uses.
pub type Charset = char;

/// `aCharsetDefaults` for Unix (`Charsets.hs:226`): everything UTF-8.
pub const DEFAULTS: [(Domain, Charset); 7] = [
    ('f', '8'),
    ('d', '8'),
    ('l', '8'),
    ('c', '8'),
    ('t', '8'),
    ('p', '8'),
    ('i', '8'),
];

/// The domain letters, in the order `parseCharsetOption` tests them. `s` is an
/// alias for `l`.
const DOMAINS: &[char] = &['s', 'l', 'c', 'f', 'd', 't', 'p', 'i'];

/// The charsets this port implements. On Unix that is all of them.
const CHARSETS: &[char] = &['0', '8', 'u', 'o', 'a'];

/// The resolved domain-to-charset table.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Charsets {
    table: Vec<(Domain, Charset)>,
}

impl Default for Charsets {
    fn default() -> Self {
        Charsets { table: DEFAULTS.to_vec() }
    }
}

/// Why a `-sc` value was refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// Not a charset letter or a known 7-Zip charset name.
    UnknownCharset(String),
    /// A domain this port parses but does not apply.
    Unapplied(char),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnknownCharset(s) => write!(f, "unknown charset {s:?}"),

            Error::Unapplied(c) => write!(
                f,
                "this port does not apply the charset for domain {c:?}; it \
                 handles list files (l) and comment files (c) only"
            ),
        }
    }
}

impl std::error::Error for Error {}

/// `g` (`Charsets.hs:129`) — the 7-Zip charset names.
fn named_charset(name: &str) -> Option<Charset> {
    match name.to_ascii_lowercase().as_str() {
        "utf-8" | "utf8" => Some('8'),
        "utf-16" | "utf16" => Some('u'),
        "win" | "ansi" => Some('a'),
        "dos" | "oem" => Some('o'),
        _ => None,
    }
}

impl Charsets {
    /// The charset in force for a domain.
    pub fn of(&self, domain: Domain) -> Charset {
        // `s` is spelled as a domain on the command line but stored as `l`.
        let d = match domain {
            's' => 'l',
            other => other,
        };
        self.table
            .iter()
            .find(|(k, _)| *k == d)
            .map(|(_, v)| *v)
            .unwrap_or('8')
    }

    fn set(&mut self, domain: Domain, charset: Charset) {
        let d = match domain {
            's' => 'l',
            other => other,
        };
        match self.table.iter_mut().find(|(k, _)| *k == d) {
            Some(slot) => slot.1 = charset,
            None => self.table.push((d, charset)),
        }
    }

    /// Apply one `-sc` value, folding over them as `parseCharsetOption` does.
    pub fn apply(&mut self, opt: &str) -> Result<(), Error> {
        // `-sc--` restores the defaults.
        if opt == "--" {
            *self = Charsets::default();
            return Ok(());
        }
        let first = match opt.chars().next() {
            Some(c) => c,
            // A bare `-sc` names no charset and no domain; nothing to do.
            None => return Ok(()),
        };
        let rest: String = opt.chars().skip(1).collect();

        if DOMAINS.contains(&first) {
            // 7-Zip form: the rest is a charset NAME.
            let cs = named_charset(&rest).ok_or(Error::UnknownCharset(rest))?;
            self.set(first, cs);
            return Ok(());
        }
        if !CHARSETS.contains(&first) {
            return Err(Error::UnknownCharset(first.to_string()));
        }
        // RAR form: the rest is a list of domain letters, defaulting to "cl".
        let domains: Vec<char> = match rest.is_empty() {
            true => vec!['c', 'l'],
            false => rest.chars().collect(),
        };
        for d in domains {
            // An unknown domain letter is ACCEPTED, not refused: the Haskell is
            // `foldl update value [(c,x) | c <- cs ||| "cl"]`, which adds the
            // pair to the table without checking it against anything. Nothing
            // ever looks it up, so `-sc0q` is a no-op rather than an error --
            // measured, after this refused it and the reference did not.
            self.set(d, first);
        }
        Ok(())
    }

    /// Refuse a table that differs from the default in a domain this port does
    /// not apply.
    ///
    /// Parsing `-scf0` and then ignoring it would leave file names decoded the
    /// default way while the user believes otherwise — the failure mode the
    /// `HONOURED` list exists to prevent.
    pub fn check_applied(&self) -> Result<(), Error> {
        for (d, default) in DEFAULTS {
            if matches!(d, 'l' | 'c') {
                continue;
            }
            if self.of(d) != default {
                return Err(Error::Unapplied(d));
            }
        }
        Ok(())
    }
}

/// Decode bytes in `charset` into text.
///
/// `'0'`, `'o'` and `'a'` are the identity on Unix, and "identity" in the
/// Haskell means each BYTE becomes the character with that code — Latin-1, not
/// UTF-8. Decoding them as UTF-8 would reject perfectly good bytes.
pub fn decode(charset: Charset, bytes: &[u8]) -> Result<String, Error> {
    match charset {
        '0' | 'o' | 'a' => Ok(bytes.iter().map(|b| char::from(*b)).collect()),
        '8' => Ok(String::from_utf8_lossy(bytes).into_owned()),
        'u' => {
            // UTF-16, little-endian: a trailing odd byte is dropped rather than
            // being an error, matching a decoder that reads pairs.
            let units: Vec<u16> = bytes
                .chunks_exact(2)
                .map(|p| u16::from_le_bytes([p[0], p[1]]))
                .collect();
            Ok(String::from_utf16_lossy(&units))
        }
        other => Err(Error::UnknownCharset(other.to_string())),
    }
}

/// `linesCRLF` (`Charsets.hs:148`) — split on CR, LF or CRLF, and drop byte
/// order marks anywhere they appear.
///
/// Not `str::lines`: that leaves a lone CR inside the line, and an archive
/// built from a CR-terminated list file would then look for a file whose name
/// ends in a carriage return.
pub fn lines_crlf(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\u{FEFF}' => {}
            '\r' => {
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }
                out.push(std::mem::take(&mut cur));
            }
            '\n' => out.push(std::mem::take(&mut cur)),
            other => cur.push(other),
        }
    }
    out.push(cur);
    out
}

/// `replace_list_files` (`Cmdline.hs:778`) — expand `@listfile` entries.
///
/// A spec that does not start with `@` is passed through. One that does is
/// replaced by the file's lines with the EMPTY ones dropped, which is what
/// makes a trailing newline harmless.
pub fn expand_list_files<F>(specs: &[String], charset: Charset, read: F) -> Result<Vec<String>, String>
where
    F: Fn(&str) -> std::io::Result<Vec<u8>>,
{
    let mut out = Vec::new();
    for spec in specs {
        match spec.strip_prefix('@') {
            None => out.push(spec.clone()),
            Some(path) => {
                let bytes = read(path).map_err(|e| format!("{path}: {e}"))?;
                let text = decode(charset, &bytes).map_err(|e| format!("{path}: {e}"))?;
                out.extend(lines_crlf(&text).into_iter().filter(|l| !l.is_empty()));
            }
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_unix_defaults_are_utf8_everywhere() {
        let c = Charsets::default();
        for d in ['f', 'd', 'l', 'c', 't', 'p', 'i'] {
            assert_eq!(c.of(d), '8', "domain {d}");
        }
        // `s` is an alias for `l`, not a domain of its own.
        assert_eq!(c.of('s'), '8');
    }

    /// The two spellings, and the fact that they cannot collide: the charset
    /// letters and the domain letters are disjoint alphabets.
    #[test]
    fn both_the_rar_and_the_7zip_forms_parse() {
        let mut c = Charsets::default();
        c.apply("0l").expect("RAR form");
        assert_eq!(c.of('l'), '0');

        let mut c = Charsets::default();
        c.apply("lutf-16").expect("7-Zip form");
        assert_eq!(c.of('l'), 'u');

        // `s` and `l` are the same domain.
        let mut c = Charsets::default();
        c.apply("sutf8").expect("7-Zip form via s");
        assert_eq!(c.of('l'), '8');

        for cs in ['0', '8', 'u', 'o', 'a'] {
            assert!(!DOMAINS.contains(&cs), "{cs} is both a charset and a domain");
        }
    }

    /// A bare `-scX` sets comment files AND list files -- `cs ||| "cl"`.
    #[test]
    fn a_bare_charset_sets_comments_and_lists() {
        let mut c = Charsets::default();
        c.apply("0").expect("bare");
        assert_eq!(c.of('c'), '0');
        assert_eq!(c.of('l'), '0');
        assert_eq!(c.of('t'), '8', "and nothing else");
    }

    #[test]
    fn double_dash_restores_the_defaults() {
        let mut c = Charsets::default();
        c.apply("0l").expect("set");
        c.apply("--").expect("reset");
        assert_eq!(c, Charsets::default());
    }

    #[test]
    fn an_unknown_charset_or_domain_is_refused() {
        let mut c = Charsets::default();
        assert!(matches!(c.apply("z"), Err(Error::UnknownCharset(_))));
        // An unknown DOMAIN is accepted and simply never looked up.
        assert!(c.apply("0q").is_ok());
        assert!(matches!(c.apply("lnosuch"), Err(Error::UnknownCharset(_))));
    }

    /// A domain this port does not apply must be refused, not silently
    /// accepted -- otherwise `-scf0` reads as "understood" and changes nothing.
    #[test]
    fn a_domain_this_port_ignores_is_refused() {
        let mut c = Charsets::default();
        c.apply("0l").expect("list files are applied");
        c.apply("0c").expect("comment files are applied");
        c.check_applied().expect("both are fine");

        let mut c = Charsets::default();
        c.apply("0f").expect("parses");
        assert_eq!(c.check_applied(), Err(Error::Unapplied('f')));
    }

    /// On Unix `0`, `o` and `a` are all the identity, and identity means
    /// BYTE-to-char, not UTF-8.
    #[test]
    fn the_identity_charsets_are_latin1_not_utf8() {
        let bytes = &[0x41, 0xC3, 0xA9, 0xFF];
        for cs in ['0', 'o', 'a'] {
            assert_eq!(decode(cs, bytes).expect("decodes"), "A\u{c3}\u{a9}\u{ff}");
        }
        // …whereas UTF-8 reads the middle two bytes as one character.
        assert_eq!(decode('8', bytes).expect("decodes").chars().count(), 3);
    }

    #[test]
    fn utf16_decodes_little_endian_pairs() {
        let bytes = &[0x41, 0x00, 0x42, 0x00];
        assert_eq!(decode('u', bytes).expect("decodes"), "AB");
        // A trailing odd byte is dropped rather than refused.
        assert_eq!(decode('u', &[0x41, 0x00, 0x42]).expect("decodes"), "A");
    }

    /// Every end-of-line spelling, and the BOM.
    #[test]
    fn lines_split_on_cr_lf_and_crlf() {
        assert_eq!(lines_crlf("a\nb"), vec!["a", "b"]);
        assert_eq!(lines_crlf("a\r\nb"), vec!["a", "b"]);
        assert_eq!(lines_crlf("a\rb"), vec!["a", "b"]);
        assert_eq!(lines_crlf("\u{FEFF}a\nb"), vec!["a", "b"]);
        // A trailing newline leaves an empty final entry, which the caller
        // drops -- that is why `deleteIf null` is in replace_list_files.
        assert_eq!(lines_crlf("a\n"), vec!["a", ""]);
    }

    #[test]
    fn a_listfile_is_replaced_by_its_non_empty_lines() {
        let read = |p: &str| -> std::io::Result<Vec<u8>> {
            match p {
                "list" => Ok(b"a.txt\r\nb.txt\n\nc.txt\n".to_vec()),
                _ => Err(std::io::Error::new(std::io::ErrorKind::NotFound, "no")),
            }
        };
        let specs = vec!["x.txt".to_string(), "@list".to_string()];
        assert_eq!(
            expand_list_files(&specs, '8', read).expect("expands"),
            vec!["x.txt", "a.txt", "b.txt", "c.txt"]
        );
    }

    #[test]
    fn a_missing_listfile_is_an_error_not_an_empty_list() {
        let read = |_: &str| -> std::io::Result<Vec<u8>> {
            Err(std::io::Error::new(std::io::ErrorKind::NotFound, "no such file"))
        };
        assert!(expand_list_files(&["@gone".to_string()], '8', read).is_err());
    }
}
