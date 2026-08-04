//! Command-line options — a port of `Options.hs:440-540`.
//!
//! DArc does not match option names exactly. Each option has a short and a long
//! spelling, and a command-line option is matched by **prefix** against both,
//! with two tiers of preference to break ties. `-m9` matches `method` by prefix
//! and takes `9` as its parameter; `-di` matches `display` and several others,
//! and is resolved by the preference list.
//!
//! # Whether an option takes a parameter is derived from its DESCRIPTION
//!
//! `paramName` (`Options.hs:477`) looks for a single all-uppercase word in the
//! option's help text: `"base DIR in archive"` means `-ap` takes a parameter,
//! and `"recursively collect files"` means `-r` does not. Two uppercase words
//! is a hard error in the Haskell. That is unusual enough to be worth writing
//! down rather than re-deriving, so the table below carries the extracted
//! parameter name and the extraction was done mechanically from `Options.hs`.

/// One row of `optionsList`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OptionDef {
    /// The short spelling, without its leading '-'. May be empty.
    pub short: &'static str,
    /// The long spelling, without its leading "--". May be empty.
    pub long: &'static str,
    /// The parameter's name, or `None` for a flag.
    pub param: Option<&'static str>,
}

impl OptionDef {
    /// The name an accepted option is recorded under: the long spelling if it
    /// has one, else the short. `long ||| short` in the Haskell.
    pub fn name(&self) -> &'static str {
        if self.long.is_empty() {
            self.short
        } else {
            self.long
        }
    }
}

/// `optionsList` (`Options.hs:155`), in source order.
pub const OPTIONS: &[OptionDef] = &[
    OptionDef { short: "--", long: "", param: None },
    OptionDef { short: "cfg", long: "config", param: Some("FILES") },
    OptionDef { short: "env", long: "", param: Some("VAR") },
    OptionDef { short: "r", long: "recursive", param: None },
    OptionDef { short: "f", long: "freshen", param: None },
    OptionDef { short: "u", long: "update", param: None },
    OptionDef { short: "", long: "sync", param: None },
    OptionDef { short: "o", long: "overwrite", param: Some("MODE") },
    OptionDef { short: "y", long: "yes", param: None },
    OptionDef { short: "x", long: "exclude", param: Some("FILESPECS") },
    OptionDef { short: "n", long: "include", param: Some("FILESPECS") },
    OptionDef { short: "ep", long: "ExcludePath", param: Some("MODE") },
    OptionDef { short: "ap", long: "arcpath", param: Some("DIR") },
    OptionDef { short: "dp", long: "diskpath", param: Some("DIR") },
    OptionDef { short: "m", long: "method", param: Some("METHOD") },
    OptionDef { short: "dm", long: "dirmethod", param: Some("METHOD") },
    OptionDef { short: "ma", long: "", param: Some("LEVEL") },
    OptionDef { short: "md", long: "dictionary", param: Some("N") },
    OptionDef { short: "mm", long: "multimedia", param: Some("MODE") },
    OptionDef { short: "ms", long: "StoreCompressed", param: None },
    OptionDef { short: "mt", long: "MultiThreaded", param: Some("THREADS") },
    OptionDef { short: "mc", long: "", param: None },
    OptionDef { short: "mx", long: "", param: None },
    OptionDef { short: "max", long: "", param: None },
    OptionDef { short: "ds", long: "sort", param: Some("ORDER") },
    OptionDef { short: "", long: "groups", param: Some("FILE") },
    OptionDef { short: "s", long: "solid", param: Some("GROUPING") },
    OptionDef { short: "p", long: "password", param: Some("PASSWORD") },
    OptionDef { short: "hp", long: "HeadersPassword", param: Some("PASSWORD") },
    OptionDef { short: "ae", long: "encryption", param: Some("ALGORITHM") },
    OptionDef { short: "kf", long: "keyfile", param: Some("KEYFILE") },
    OptionDef { short: "op", long: "OldPassword", param: Some("PASSWORD") },
    OptionDef { short: "okf", long: "OldKeyfile", param: Some("KEYFILE") },
    OptionDef { short: "w", long: "workdir", param: Some("DIRECTORY") },
    OptionDef { short: "", long: "create-in-workdir", param: None },
    OptionDef { short: "sc", long: "charset", param: Some("CHARSETS") },
    OptionDef { short: "", long: "language", param: Some("FILE") },
    OptionDef { short: "tp", long: "pretest", param: Some("MODE") },
    OptionDef { short: "t", long: "test", param: None },
    OptionDef { short: "t", long: "type", param: Some("TYPE") },
    OptionDef { short: "d", long: "delete", param: None },
    OptionDef { short: "df", long: "delfiles", param: None },
    OptionDef { short: "kb", long: "keepbroken", param: None },
    OptionDef { short: "ba", long: "BrokenArchive", param: Some("MODE") },
    OptionDef { short: "ac", long: "ClearArchiveBit", param: None },
    OptionDef { short: "ao", long: "SelectArchiveBit", param: None },
    OptionDef { short: "sm", long: "SizeMore", param: Some("SIZE") },
    OptionDef { short: "sl", long: "SizeLess", param: Some("SIZE") },
    OptionDef { short: "tb", long: "TimeBefore", param: Some("TIME") },
    OptionDef { short: "ta", long: "TimeAfter", param: Some("TIME") },
    OptionDef { short: "tn", long: "TimeNewer", param: Some("PERIOD") },
    OptionDef { short: "to", long: "TimeOlder", param: Some("PERIOD") },
    OptionDef { short: "k", long: "lock", param: None },
    OptionDef { short: "rr", long: "recovery", param: Some("SIZE") },
    OptionDef { short: "sfx", long: "", param: Some("MODULE") },
    OptionDef { short: "z", long: "arccmt", param: Some("FILE") },
    OptionDef { short: "", long: "archive-comment", param: Some("COMMENT") },
    OptionDef { short: "i", long: "indicator", param: Some("TYPE") },
    OptionDef { short: "ad", long: "adddir", param: None },
    OptionDef { short: "ag", long: "autogenerate", param: Some("FMT") },
    OptionDef { short: "", long: "noarcext", param: None },
    OptionDef { short: "tk", long: "keeptime", param: None },
    OptionDef { short: "tl", long: "timetolast", param: None },
    OptionDef { short: "fn", long: "fullnames", param: None },
    OptionDef { short: "", long: "append", param: None },
    OptionDef { short: "", long: "recompress", param: None },
    OptionDef { short: "", long: "dirs", param: None },
    OptionDef { short: "ed", long: "nodirs", param: None },
    OptionDef { short: "", long: "nodates", param: None },
    OptionDef { short: "ioff", long: "shutdown", param: None },
    OptionDef { short: "", long: "pause-before-exit", param: Some("PAUSE") },
    OptionDef { short: "v", long: "volume", param: Some("SIZE") },
    OptionDef { short: "", long: "queue", param: None },
    OptionDef { short: "", long: "arc-32bit-legacy", param: None },
    OptionDef { short: "", long: "cache", param: Some("N") },
    OptionDef { short: "lc", long: "LimitCompMem", param: Some("N") },
    OptionDef { short: "ld", long: "LimitDecompMem", param: Some("N") },
    OptionDef { short: "", long: "nodir", param: None },
    OptionDef { short: "", long: "nodata", param: None },
    OptionDef { short: "", long: "crconly", param: None },
    OptionDef { short: "di", long: "display", param: Some("AMOUNT") },
    OptionDef { short: "", long: "logfile", param: Some("FILE") },
    OptionDef { short: "", long: "print-config", param: None },
    OptionDef { short: "", long: "proxy", param: Some("URL") },
    OptionDef { short: "", long: "bypass", param: Some("URL") },
    OptionDef { short: "", long: "original", param: Some("URL") },
    OptionDef { short: "", long: "save-bad-ranges", param: Some("FILE") },
];

/// `aPREFFERED_OPTIONS` — consulted when a prefix matches more than one option.
pub const PREFERRED: &[&str] = &[
    "method", "sfx", "charset", "SizeMore", "SizeLess", "overwrite", "shutdown", "type",
];

/// `aSUPER_PREFFERED_OPTIONS` — consulted when the list above still ties.
pub const SUPER_PREFERRED: &[&str] = &["OldKeyfile"];

/// A compiled dictionary entry: one spelling of one option.
///
/// The Haskell compiles the long spelling as `'-':long`, so a long option's
/// dictionary key literally begins with a dash. Since the parser has already
/// stripped one dash from the argument, `--recursive` arrives as `-recursive`
/// and matches that key directly. That is reproduced with a flag rather than by
/// building the string, because these are `&'static str` from a const table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Entry {
    /// The spelling WITHOUT any leading dash.
    prefix: &'static str,
    is_long: bool,
    name: &'static str,
    takes_param: bool,
}

impl Entry {
    /// Does this entry match `option` — the argument with ONE leading dash
    /// already removed?
    ///
    /// A flag must match exactly; an option with a parameter matches by prefix,
    /// which is what lets `-m9` carry `9` while `-r9` is simply unknown.
    fn matches(&self, option: &str) -> bool {
        let body = if self.is_long {
            match option.strip_prefix('-') {
                Some(rest) => rest,
                None => return false,
            }
        } else {
            // A short spelling never matches an argument that began with two
            // dashes.
            if option.starts_with('-') {
                return false;
            }
            option
        };
        if self.takes_param {
            body.starts_with(self.prefix)
        } else {
            body == self.prefix
        }
    }

    /// The value carried after the matched spelling.
    ///
    /// `tryToSkip "=" (tryToSkip prefix option)`: drop the spelling, then an
    /// optional '='. Both are skip-if-present, so `-ap=dir` and `-apdir` are the
    /// same option with the same value.
    fn value(&self, option: &str) -> String {
        let body = if self.is_long { option.strip_prefix('-').unwrap_or(option) } else { option };
        let rest = body.strip_prefix(self.prefix).unwrap_or(body);
        rest.strip_prefix('=').unwrap_or(rest).to_string()
    }

    /// How this spelling is written on a command line, for diagnostics.
    fn spelled(&self) -> String {
        if self.is_long {
            format!("--{}", self.prefix)
        } else {
            format!("-{}", self.prefix)
        }
    }
}

/// `optionsDict` — every option contributes up to two entries. `compile` drops
/// an empty spelling, and drops `"-"`, which is what an empty long name becomes
/// once the dash is prepended.
fn dictionary() -> Vec<Entry> {
    let mut out = Vec::with_capacity(OPTIONS.len() * 2);
    for o in OPTIONS {
        let takes_param = o.param.is_some();
        if !o.short.is_empty() && o.short != "-" {
            out.push(Entry { prefix: o.short, is_long: false, name: o.name(), takes_param });
        }
        if !o.long.is_empty() {
            out.push(Entry { prefix: o.long, is_long: true, name: o.name(), takes_param });
        }
    }
    out
}

/// What `parseOptions` rejected.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// `CMDLINE_UNKNOWN_OPTION`.
    Unknown(String),
    /// `CMDLINE_AMBIGUOUS_OPTION`, with the candidates it could not choose
    /// between — the Haskell prints them, so they are carried, not counted.
    Ambiguous { option: String, variants: Vec<String> },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Unknown(o) => write!(f, "unknown option {o}"),
            Error::Ambiguous { option, variants } => {
                write!(f, "ambiguous option {option}, may be {}", variants.join(" or "))
            }
        }
    }
}

/// The result of parsing: options in order, then the free arguments.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Parsed {
    /// `(name, parameter)` pairs. A flag's parameter is the empty string.
    pub options: Vec<(String, String)>,
    pub free: Vec<String>,
}

impl Parsed {
    /// `findReqList` — every value given for `flag`, in order.
    pub fn all(&self, flag: &str) -> Vec<&str> {
        self.options.iter().filter(|(n, _)| n == flag).map(|(_, v)| v.as_str()).collect()
    }

    /// `findReqArg` / `findOptArg` — the LAST value, or `deflt`.
    ///
    /// Last, not first: a later `-m4` overrides an earlier `-m9`, including one
    /// that arrived from the config file or the environment variable, which are
    /// prepended to the command line.
    pub fn arg<'a>(&'a self, flag: &str, deflt: &'a str) -> &'a str {
        self.all(flag).last().copied().unwrap_or(deflt)
    }

    /// `findNoArg`.
    pub fn flag(&self, name: &str) -> bool {
        self.options.iter().any(|(n, _)| n == name)
    }
}

/// `parseOptions` (`Options.hs:483`).
pub fn parse(args: &[String]) -> Result<Parsed, Error> {
    let dict = dictionary();
    let mut out = Parsed::default();
    let mut it = args.iter();
    while let Some(arg) = it.next() {
        // "--" alone stops option processing; everything after is a free
        // argument, verbatim.
        if arg == "--" {
            out.free.extend(it.cloned());
            return Ok(out);
        }
        let option = match arg.strip_prefix('-') {
            Some(rest) => rest,
            None => {
                out.free.push(arg.clone());
                continue;
            }
        };

        let matches: Vec<&Entry> = dict.iter().filter(|e| e.matches(option)).collect();
        let chosen: &Entry = match matches.len() {
            1 => matches[0],
            0 => return Err(Error::Unknown(arg.clone())),
            _ => {
                // Two tiers of preference, tried in order. Note both tiers are
                // consulted by OPTION NAME, so the two spellings of one option
                // never tie with each other.
                let pref: Vec<&Entry> = matches
                    .iter()
                    .copied()
                    .filter(|e| PREFERRED.contains(&e.name) || SUPER_PREFERRED.contains(&e.name))
                    .collect();
                let sup: Vec<&Entry> = matches
                    .iter()
                    .copied()
                    .filter(|e| SUPER_PREFERRED.contains(&e.name))
                    .collect();
                match (pref.len(), sup.len()) {
                    (1, _) => pref[0],
                    (_, 1) => sup[0],
                    (_, _) => {
                        return Err(Error::Ambiguous {
                            option: arg.clone(),
                            variants: matches.iter().map(|e| e.spelled()).collect(),
                        })
                    }
                }
            }
        };
        out.options.push((chosen.name.to_string(), chosen.value(option)));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_args(args: &[&str]) -> Result<Parsed, Error> {
        parse(&args.iter().map(|s| s.to_string()).collect::<Vec<_>>())
    }

    /// Whether an option takes a parameter is read out of its DESCRIPTION -- a
    /// single all-uppercase word. Pin a few, because the table was extracted
    /// mechanically and a mis-extraction changes how the whole line parses.
    #[test]
    fn the_parameter_flags_match_the_help_text() {
        let by_long = |l: &str| OPTIONS.iter().find(|o| o.long == l).copied();
        // A SINGLE uppercase letter is a parameter name too: `paramName` is
        // `filter (all isUpper) (words descr)`, and `all isUpper "N"` is true.
        // The mechanical extraction that built this table wanted two or more,
        // so every `N` row came out wrong -- `-lc64m` and `--dictionary=16m`
        // were "unknown option" while the reference accepts both. The rows
        // pinned below this comment are exactly the ones that were wrong, and
        // they are all single-letter: a check that only pinned multi-letter
        // names is what let the class through in the first place.
        assert_eq!(by_long("dictionary").and_then(|o| o.param), Some("N"));
        assert_eq!(by_long("cache").and_then(|o| o.param), Some("N"));
        assert_eq!(by_long("LimitCompMem").and_then(|o| o.param), Some("N"));
        assert_eq!(by_long("LimitDecompMem").and_then(|o| o.param), Some("N"));
        // "save/check CRC, but don't store data" -- NO parameter, and the
        // reason is the COMMA: the word is "CRC," and `all isUpper` is false
        // for punctuation. Checked against the reference, which answers
        // `--crconly=x` with "unknown option". A sweep that strips punctuation
        // before testing gets this wrong, and mine did.
        assert_eq!(by_long("crconly").and_then(|o| o.param), None);

        assert_eq!(by_long("recursive").and_then(|o| o.param), None);
        assert_eq!(by_long("arcpath").and_then(|o| o.param), Some("DIR"));
        assert_eq!(by_long("diskpath").and_then(|o| o.param), Some("DIR"));
        assert_eq!(by_long("method").and_then(|o| o.param), Some("METHOD"));
        assert_eq!(by_long("yes").and_then(|o| o.param), None);
        assert_eq!(by_long("ExcludePath").and_then(|o| o.param), Some("MODE"));
    }

    /// An option with a parameter matches by PREFIX and swallows the rest.
    #[test]
    fn a_parameterised_option_takes_the_rest_of_the_word() {
        let p = parse_args(&["-m9"]).expect("parses");
        assert_eq!(p.options, vec![("method".to_string(), "9".to_string())]);
        let p = parse_args(&["-mx"]).expect("parses");
        assert_eq!(p.arg("method", ""), "x");
    }

    /// A flag matches EXACTLY. "-r" is recursive; "-r9" is not a longer form of
    /// it, and must be reported rather than silently accepted.
    #[test]
    fn a_flag_does_not_match_by_prefix() {
        assert!(parse_args(&["-r"]).expect("parses").flag("recursive"));
        assert_eq!(parse_args(&["-r9"]), Err(Error::Unknown("-r9".to_string())));
    }

    /// `-ap=dir` and `-apdir` are the same thing: the '=' is skip-if-present.
    #[test]
    fn an_equals_sign_is_optional() {
        for spelling in ["-apsub/dir", "-ap=sub/dir"] {
            let p = parse_args(&[spelling]).expect("parses");
            assert_eq!(p.arg("arcpath", ""), "sub/dir", "{spelling}");
        }
    }

    #[test]
    fn long_options_take_two_dashes() {
        let p = parse_args(&["--recursive", "--diskpath=/tmp"]).expect("parses");
        assert!(p.flag("recursive"));
        assert_eq!(p.arg("diskpath", ""), "/tmp");
    }

    /// The LAST value wins, which is what lets a command line override the
    /// config file and the environment variable prepended before it.
    #[test]
    fn the_last_value_of_a_repeated_option_wins() {
        let p = parse_args(&["-m9", "-m4"]).expect("parses");
        assert_eq!(p.arg("method", ""), "4");
        assert_eq!(p.all("method"), vec!["9", "4"]);
    }

    /// "--" stops option processing; everything after it is a free argument
    /// even if it looks like an option.
    #[test]
    fn a_bare_double_dash_stops_option_processing() {
        let p = parse_args(&["-r", "--", "-m9", "file.txt"]).expect("parses");
        assert!(p.flag("recursive"));
        assert!(!p.flag("method"), "-m9 after -- is a filename");
        assert_eq!(p.free, vec!["-m9".to_string(), "file.txt".to_string()]);
    }

    #[test]
    fn free_arguments_keep_their_order() {
        let p = parse_args(&["a.arc", "-r", "one", "two"]).expect("parses");
        assert_eq!(p.free, vec!["a.arc".to_string(), "one".to_string(), "two".to_string()]);
    }

    /// The preference list exists because prefixes collide. `-m` alone matches
    /// `method`, `ma`, `md`, `mm`, `ms`, `mt`, `mc`, `mx` -- and `method` is
    /// preferred, which is why `-m9` works at all.
    #[test]
    fn the_preference_list_resolves_a_colliding_prefix() {
        let p = parse_args(&["-m"]).expect("parses");
        assert_eq!(p.options, vec![("method".to_string(), "".to_string())]);
    }

    /// Checked against the real binary, not derived. `Tests/arc-ghc l -ti` says
    /// `--type=i: only arc format is supported`, so `-t` resolves to `type`
    /// rather than `test` -- `type` is in aPREFFERED_OPTIONS and `test` is not,
    /// even though `test` comes first in the table.
    #[test]
    fn a_collision_resolves_the_way_the_reference_resolves_it() {
        let p = parse_args(&["-ti"]).expect("parses");
        assert_eq!(p.options, vec![("type".to_string(), "i".to_string())]);
    }

    /// These eight were run through Tests/arc-ghc and this port agrees with it
    /// on every one. Kept as a table because a transcription of a 87-row option
    /// list is exactly the kind of thing that passes its own tests and disagrees
    /// with the program it is copying.
    #[test]
    fn the_reference_agrees_on_these() {
        // (argument, Some(expected option name) or None for "unknown option")
        let cases: [(&str, Option<&str>); 8] = [
            ("-Q", None),
            ("--no-such-option", None),
            ("-zzzz", Some("arccmt")),
            ("-m", Some("method")),
            ("-ti", Some("type")),
            ("-r9", None),
            ("-ap=x", Some("arcpath")),
            ("-apx", Some("arcpath")),
        ];
        for (arg, want) in cases {
            match (parse_args(&[arg]), want) {
                (Ok(p), Some(name)) => {
                    assert_eq!(p.options.first().map(|(n, _)| n.as_str()), Some(name), "{arg}")
                }
                (Err(Error::Unknown(_)), None) => {}
                (got, _) => panic!("{arg}: got {got:?}, wanted {want:?}"),
            }
        }
    }

    #[test]
    fn an_unknown_option_is_reported() {
        // Not "-zzzz": `-z` is `arccmt` and TAKES a parameter, so that parses as
        // `-z` with the value "zzz". Prefix matching means very few strings are
        // actually unknown, which is itself worth knowing.
        assert_eq!(parse_args(&["-Q"]), Err(Error::Unknown("-Q".to_string())));
        assert_eq!(
            parse_args(&["--no-such-option"]),
            Err(Error::Unknown("--no-such-option".to_string()))
        );
    }

    /// Prefix matching makes a typo parse as a DIFFERENT option rather than
    /// fail. `-zzzz` is `-z` (read the archive comment from a file) with the
    /// filename "zzz". Worth a test so the behaviour is a decision rather than
    /// a surprise.
    #[test]
    fn a_typo_can_parse_as_another_option_with_a_parameter() {
        let p = parse_args(&["-zzzz"]).expect("parses");
        assert_eq!(p.options, vec![("arccmt".to_string(), "zzz".to_string())]);
    }

    /// Every option in the table must be reachable by its own spelling. A row
    /// whose short form is shadowed by another row's prefix would be dead.
    #[test]
    fn every_option_is_reachable_by_its_long_spelling() {
        for o in OPTIONS {
            if o.long.is_empty() {
                continue;
            }
            let arg = format!("--{}", o.long);
            match parse(&[arg.clone()]) {
                Ok(p) => assert_eq!(
                    p.options.first().map(|(n, _)| n.as_str()),
                    Some(o.name()),
                    "{arg} resolved elsewhere"
                ),
                Err(e) => panic!("{arg} is unreachable: {e}"),
            }
        }
    }
}
