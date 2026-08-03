//! Which files a command touches — `opt_file_filter` (`Cmdline.hs:493`) and the
//! three combinators built on it (`Arc.hs:243-272`).
//!
//! # One rule, applied in three places
//!
//! DArc has a single file-selection predicate and reuses it everywhere. What
//! changes between commands is only whether the *filespecs* are ANDed in and
//! whether the result is negated:
//!
//! ```text
//!   addFileFilter      a u f m    the disk scan          filter alone
//!   fullFileFilter     ch c k t   archive selection      filespecs AND filter
//!                      l v lt lb
//!   not . fullFileFilter   d      archive selection      NOT (filespecs AND filter)
//!   cmd_archive_filter     a u f j  = const True         archive files unfiltered
//! ```
//!
//! The last row is the one that surprises: for `a`, `u`, `f` and `j` the
//! filespecs select DISK files, so the archive's own entries are kept whatever
//! they are named. `runAdd` sets `cmd_archive_filter = const True` explicitly.
//!
//! # Directories never go through it, on either side
//!
//! Both sides short-circuit directories, and they agree:
//!
//! * reading, `test_dirs` (`Arc.hs:270`): a directory is included exactly when
//!   `opt_x_include_dirs` says so;
//! * writing, `accept_f` (`FileInfo.hs:462`): `include_dirs` if `--dirs`/
//!   `--nodirs` was given, else it depends on whether any n/s/t filter exists.
//!
//! In neither case is the directory's NAME matched against `-n` or `-x`. So
//! `arc a -n*.txt` drops the `sub` entry not because `sub` fails to match
//! `*.txt`, but because giving `-n` at all turns directory inclusion off —
//! while `arc a -x*.dat` keeps it, because `-x` is not an n/s/t filter.
//!
//! This port got that backwards at first, by reasoning from the two observed
//! outcomes instead of from `accept_f`: filtering directories by name produces
//! the same answer for those two cases and a different one for
//! `--dirs -n*.txt`, which is what caught it.

use crate::sort::match_filespecs;

/// `opt_file_filter` — the whole of it: `-n`, `-x`, and the size and time
/// filters.
///
/// The comparisons are the Haskell's, and the asymmetry is real rather than a
/// simplification (`Cmdline.hs:498-505`):
///
/// ```text
///   -sm SIZE     size >  SIZE        strictly greater
///   -sl SIZE     size <  SIZE        strictly less
///   -ta TIME     time >= TIME        inclusive
///   -tb TIME     time <  TIME        exclusive
///   -tn PERIOD   time >= now-PERIOD  inclusive
///   -to PERIOD   time <  now-PERIOD  exclusive
/// ```
///
/// `-tn`/`-to` are resolved to absolute instants by the caller, so nothing here
/// reads the clock and every rule below is testable.
#[derive(Clone, Debug, Default)]
pub struct FileFilter {
    /// `-n` — when [`include_given`](Self::include_given), a name must match
    /// one of these.
    pub include: Vec<String>,
    /// Whether `-n` was given AT ALL, before `@listfile` expansion.
    ///
    /// `match_included = orig_include_list &&& [...]` (`Cmdline.hs:439`) tests
    /// the ORIGINAL list, "since with an empty list file no file should pass
    /// the filter". So `-n@empty.txt` selects NOTHING, where reading the
    /// expanded list as absent would select everything — the difference
    /// between an empty archive and a full one.
    pub include_given: bool,
    /// `-x` — a name matching one of these is rejected.
    pub exclude: Vec<String>,
    /// `--fullnames`: match against the whole stored path rather than the base
    /// name. `match_with = findNoArg o "fullnames" .$ bool fpBasename fpFullname`.
    pub full_names: bool,
    /// `-sm` — keep files strictly LARGER than this.
    pub size_more: Option<u64>,
    /// `-sl` — keep files strictly SMALLER than this.
    pub size_less: Option<u64>,
    /// `-ta`, and `-tn` after resolution — keep files modified at or after this.
    pub time_at_or_after: Vec<i64>,
    /// `-tb`, and `-to` after resolution — keep files modified before this.
    pub time_before: Vec<i64>,
}

impl FileFilter {
    /// Does this name survive the filter?
    ///
    /// `-n` is checked against the ORIGINAL option list, not the expanded one:
    /// "with an empty list file no file should pass the filter"
    /// (`Cmdline.hs:437`). An `-n` that was given but matches nothing therefore
    /// rejects everything, which is why `include.is_empty()` and "include
    /// matched nothing" are different answers.
    pub fn accepts(&self, stored_name: &str, size: u64, time: i64) -> bool {
        let included = !self.include_given
            || match_filespecs(&self.include, stored_name, self.full_names);
        let excluded = !self.exclude.is_empty()
            && match_filespecs(&self.exclude, stored_name, self.full_names);
        included
            && !excluded
            && self.size_more.map(|n| size > n).unwrap_or(true)
            && self.size_less.map(|n| size < n).unwrap_or(true)
            && self.time_at_or_after.iter().all(|t| time >= *t)
            && self.time_before.iter().all(|t| time < *t)
    }

    /// `null nst_filters` — whether any *n/s/t* filter was given.
    ///
    /// `-x` is deliberately absent: `nst_filters` is `match_included` plus the
    /// size and time filters (`Cmdline.hs:498`), and `match_excluded` is applied
    /// separately at `:495`. That distinction is load-bearing exactly once, in
    /// [`include_dirs`], and getting it wrong makes `arc l -x…` stop listing
    /// directories.
    pub fn has_nst(&self) -> bool {
        self.include_given
            || self.size_more.is_some()
            || self.size_less.is_some()
            || !self.time_at_or_after.is_empty()
            || !self.time_before.is_empty()
    }
}

/// `parseSize` (`Utils.hs:66`), which is `parseNumber num 'b'`.
///
/// The trick in `parseNumber` is that it appends the DEFAULT SPECIFIER to the
/// string and then spans digits, so `"512"` is parsed as `"512b"`. Every suffix
/// is one character and the rest is never looked at — `"8mb"` stops at `'m'`.
/// `'^'` means a power of two.
pub fn parse_size(s: &str) -> Option<u64> {
    let lowered = s.to_ascii_lowercase();
    let digits: String = lowered.chars().take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        return None;
    }
    let n: u64 = digits.parse().ok()?;
    // `num ++ [default_specifier]`: with nothing after the digits the appended
    // 'b' is what the span stops on.
    match lowered[digits.len()..].chars().next().unwrap_or('b') {
        'b' => Some(n),
        'k' => n.checked_mul(1024),
        'm' => n.checked_mul(1024 * 1024),
        'g' => n.checked_mul(1024 * 1024 * 1024),
        't' => n.checked_mul(1024u64.pow(4)),
        '^' => match n < 64 {
            true => Some(1u64 << n),
            false => None,
        },
        _ => None,
    }
}

/// `calcDiff` (`Cmdline.hs:481`) — a `-tn`/`-to` PERIOD, in seconds.
///
/// The string is split into digit/letter runs and folded, so `"1d12h"` is a day
/// and a half. A run with NO suffix means DAYS (`_ -> td {tdDay = …}`), which is
/// why `-tn7` is a week and not seven seconds.
///
/// Each run ASSIGNS rather than adds — `td {tdDay = …}` replaces the field — so
/// `"1d2d"` is two days, not three. Faithful, and unlikely to be relied on.
pub fn parse_period(s: &str) -> Option<i64> {
    let (mut days, mut hours, mut mins, mut secs) = (0i64, 0i64, 0i64, 0i64);
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut any = false;
    while i < chars.len() {
        let start = i;
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        if i == start {
            return None; // a suffix with no number in front of it
        }
        let n: i64 = chars[start..i].iter().collect::<String>().parse().ok()?;
        any = true;
        // `last x` is the run's final character; with no suffix the whole run is
        // digits and the fallthrough gives days.
        match chars.get(i) {
            Some('d') => { days = n; i += 1; }
            Some('h') => { hours = n; i += 1; }
            Some('m') => { mins = n; i += 1; }
            Some('s') => { secs = n; i += 1; }
            Some(_) => return None,
            None => days = n,
        }
    }
    match any {
        true => Some(days * 86400 + hours * 3600 + mins * 60 + secs),
        false => None,
    }
}

/// `x_include_dirs` (`Cmdline.hs:521`) — whether a READ command lists or
/// extracts directory entries.
///
/// `--dirs`/`--nodirs` decide it outright. Otherwise directories come along
/// only when nothing has been narrowed: no filespecs, no n/s/t filter, and not
/// the `e` command, which flattens and has nowhere to put them.
pub fn include_dirs(
    dirs_option: Option<bool>,
    default_filespecs: bool,
    filter: &FileFilter,
    command: &str,
) -> bool {
    match dirs_option {
        Some(x) => x,
        None => default_filespecs && !filter.has_nst() && command != "e",
    }
}

/// The same question for a command that WRITES — `accept_f`'s directory arm
/// (`FileInfo.hs:462`).
///
/// `include_dirs `defaultVal` (addDir && … || no_nst_filters && recursive &&
/// include_all)`. The `addDir`/`recursive`/`include_all` half is about which
/// filespec shapes pull a directory in at all, which the scan already decides
/// by producing the entry or not; what is left for here is the option and the
/// n/s/t test.
///
/// Same shape as [`include_dirs`], on purpose: a directory's fate is settled by
/// `--dirs`/`--nodirs` and by whether a selection filter exists, never by its
/// name, and that holds whichever direction the command runs in.
///
/// This governs the entries the WALK produces. The directory a filespec names
/// outright comes from the `addDir` pass, whose test is `include_dirs
/// `defaultVal` True` — only the option, never the n/s/t filters — and which
/// `darc.rs` applies separately.
///
/// `--dirs` used to duplicate the top-level directory of each filespec in the
/// reference: `accept_f` serves both passes, and forcing it true made the
/// addDir pass accept every SIBLING of the named directory, so the entry the
/// walk already emitted appeared twice — and directories the user never named
/// appeared at all. Fixed in `FileInfo.hs:462` by giving the addDir pass its
/// own arm, so the two builds are byte-identical under `--dirs` again.
pub fn write_dirs(dirs_option: Option<bool>, filter: &FileFilter) -> bool {
    match dirs_option {
        Some(x) => x,
        None => !filter.has_nst(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn f(include: &[&str], exclude: &[&str]) -> FileFilter {
        FileFilter {
            include_given: !include.is_empty(),
            include: include.iter().map(|s| s.to_string()).collect(),
            exclude: exclude.iter().map(|s| s.to_string()).collect(),
            full_names: false,
            ..Default::default()
        }
    }

    /// The values are measured from the reference build over a tree of
    /// a.txt, b.dat, sub/n.txt, sub/deep.dat.
    #[test]
    fn exclude_rejects_and_leaves_everything_else() {
        let x = f(&[], &["*.dat"]);
        assert!(x.accepts("a.txt", 0, 0));
        assert!(x.accepts("sub/n.txt", 0, 0));
        assert!(!x.accepts("b.dat", 0, 0));
        assert!(!x.accepts("sub/deep.dat", 0, 0));
        // `sub` matches no *.dat pattern, so the directory survives -- which is
        // what the reference does.
        assert!(x.accepts("sub", 0, 0));
    }

    #[test]
    fn include_admits_only_what_matches_directories_included() {
        let n = f(&["*.txt"], &[]);
        assert!(n.accepts("a.txt", 0, 0));
        assert!(n.accepts("sub/n.txt", 0, 0));
        assert!(!n.accepts("b.dat", 0, 0));
        // …and `sub` does not match *.txt either, so it is dropped. The
        // asymmetry with the -x case above is the two patterns, not two rules.
        assert!(!n.accepts("sub", 0, 0));
    }

    /// Both together, in both orders on the command line: the result is an AND
    /// and does not depend on order. Measured -- an earlier reading of this as
    /// order-dependent came from an unquoted shell test, not from the binary.
    #[test]
    fn include_and_exclude_are_an_and() {
        let both = f(&["*"], &["*.dat"]);
        assert!(both.accepts("a.txt", 0, 0));
        assert!(!both.accepts("b.dat", 0, 0));
        let narrow = f(&["*.txt"], &["a.txt"]);
        assert!(!narrow.accepts("a.txt", 0, 0), "excluded wins over included");
        assert!(narrow.accepts("sub/n.txt", 0, 0));
    }

    /// Matching is on the BASE NAME unless --fullnames, which is why `-xsub`
    /// excludes nothing in a tree whose only `sub` is a directory component.
    #[test]
    fn matching_is_on_the_base_name_by_default() {
        let x = f(&[], &["n.txt"]);
        assert!(!x.accepts("sub/n.txt", 0, 0), "the base name is what is matched");
        let full = FileFilter {
            include: Vec::new(),
            exclude: vec!["n.txt".to_string()],
            full_names: true,
            ..Default::default()
        };
        assert!(full.accepts("sub/n.txt", 0, 0), "--fullnames needs the whole path to match");
    }

    #[test]
    fn an_empty_filter_accepts_everything() {
        let none = f(&[], &[]);
        assert!(none.accepts("anything", 0, 0));
        assert!(none.accepts("", 0, 0));
        assert!(!none.has_nst());
    }

    /// The comparisons are strict for size and asymmetric for time, and both
    /// halves matter: a file exactly ON the boundary is excluded by `-sm`/`-sl`
    /// and by `-tb`, and included by `-ta`.
    #[test]
    fn the_size_and_time_comparisons_are_the_haskells() {
        let sm = FileFilter { size_more: Some(100), ..Default::default() };
        assert!(sm.accepts("f", 101, 0));
        assert!(!sm.accepts("f", 100, 0), "-sm is strictly greater");
        assert!(!sm.accepts("f", 99, 0));

        let sl = FileFilter { size_less: Some(100), ..Default::default() };
        assert!(sl.accepts("f", 99, 0));
        assert!(!sl.accepts("f", 100, 0), "-sl is strictly less");

        let ta = FileFilter { time_at_or_after: vec![1000], ..Default::default() };
        assert!(ta.accepts("f", 0, 1000), "-ta includes the boundary");
        assert!(ta.accepts("f", 0, 1001));
        assert!(!ta.accepts("f", 0, 999));

        let tb = FileFilter { time_before: vec![1000], ..Default::default() };
        assert!(tb.accepts("f", 0, 999));
        assert!(!tb.accepts("f", 0, 1000), "-tb excludes the boundary");

        // Given together they AND, and a band can be empty.
        let band = FileFilter {
            size_more: Some(10),
            size_less: Some(20),
            ..Default::default()
        };
        assert!(band.accepts("f", 15, 0));
        assert!(!band.accepts("f", 25, 0));
    }

    /// `parseSize` appends its default specifier before spanning digits, so a
    /// bare number is BYTES -- unlike `parseMem`, where it is megabytes.
    #[test]
    fn parse_size_defaults_to_bytes_and_takes_a_one_character_suffix() {
        assert_eq!(parse_size("512"), Some(512));
        assert_eq!(parse_size("8k"), Some(8 * 1024));
        assert_eq!(parse_size("8kb"), Some(8 * 1024), "the 'b' is never examined");
        assert_eq!(parse_size("2m"), Some(2 * 1024 * 1024));
        assert_eq!(parse_size("1g"), Some(1024 * 1024 * 1024));
        assert_eq!(parse_size("10^"), Some(1024));
        assert_eq!(parse_size("8M"), Some(8 * 1024 * 1024), "lowercased first");
        assert_eq!(parse_size(""), None);
        assert_eq!(parse_size("k"), None);
        assert_eq!(parse_size("5z"), None);
    }

    /// A PERIOD with no suffix is DAYS, which is the one that would silently
    /// turn `-tn7` into seven seconds.
    #[test]
    fn a_bare_period_is_days() {
        assert_eq!(parse_period("7"), Some(7 * 86400));
        assert_eq!(parse_period("1d"), Some(86400));
        assert_eq!(parse_period("12h"), Some(12 * 3600));
        assert_eq!(parse_period("30m"), Some(1800));
        assert_eq!(parse_period("45s"), Some(45));
        assert_eq!(parse_period("1d12h"), Some(86400 + 12 * 3600));
        // Each run assigns rather than adds, as `td {tdDay = …}` does.
        assert_eq!(parse_period("1d2d"), Some(2 * 86400));
        assert_eq!(parse_period(""), None);
        assert_eq!(parse_period("d"), None);
        assert_eq!(parse_period("3x"), None);
    }

    /// `-x` is not an n/s/t filter and must not suppress directories on the
    /// read side; `-n` is and does, and so does every size and time filter.
    #[test]
    fn only_the_include_filter_counts_as_nst() {
        assert!(!f(&[], &["*.dat"]).has_nst());
        assert!(f(&["*.txt"], &[]).has_nst());
        assert!(FileFilter { size_more: Some(1), ..Default::default() }.has_nst());
        assert!(FileFilter { size_less: Some(1), ..Default::default() }.has_nst());
        assert!(FileFilter { time_at_or_after: vec![1], ..Default::default() }.has_nst());
        assert!(FileFilter { time_before: vec![1], ..Default::default() }.has_nst());

        assert!(include_dirs(None, true, &f(&[], &["*.dat"]), "l"));
        assert!(!include_dirs(None, true, &f(&["*.txt"], &[]), "l"));
        assert!(!include_dirs(None, false, &f(&[], &[]), "l"), "filespecs narrow it");
        assert!(!include_dirs(None, true, &f(&[], &[]), "e"), "e never takes directories");
        // The options win outright, including over `e`.
        assert!(include_dirs(Some(true), false, &f(&["*.txt"], &[]), "e"));
        assert!(!include_dirs(Some(false), true, &f(&[], &[]), "l"));
    }
}
