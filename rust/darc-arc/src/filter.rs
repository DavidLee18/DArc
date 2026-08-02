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

/// `opt_file_filter`, restricted to the parts this port implements: `-n` and
/// `-x`.
///
/// The size and time filters (`-sm`, `-sl`, `-tb`, `-ta`, `-tn`, `-to`) belong
/// here too and are NOT implemented; `darc` refuses them rather than ignoring
/// them, so this type never has to pretend they passed.
#[derive(Clone, Debug, Default)]
pub struct FileFilter {
    /// `-n` — when non-empty, a name must match one of these.
    pub include: Vec<String>,
    /// `-x` — a name matching one of these is rejected.
    pub exclude: Vec<String>,
    /// `--fullnames`: match against the whole stored path rather than the base
    /// name. `match_with = findNoArg o "fullnames" .$ bool fpBasename fpFullname`.
    pub full_names: bool,
}

impl FileFilter {
    /// Does this name survive the filter?
    ///
    /// `-n` is checked against the ORIGINAL option list, not the expanded one:
    /// "with an empty list file no file should pass the filter"
    /// (`Cmdline.hs:437`). An `-n` that was given but matches nothing therefore
    /// rejects everything, which is why `include.is_empty()` and "include
    /// matched nothing" are different answers.
    pub fn accepts(&self, stored_name: &str) -> bool {
        let included = self.include.is_empty()
            || match_filespecs(&self.include, stored_name, self.full_names);
        let excluded = !self.exclude.is_empty()
            && match_filespecs(&self.exclude, stored_name, self.full_names);
        included && !excluded
    }

    /// `null nst_filters` — whether any *n/s/t* filter was given.
    ///
    /// `-x` is deliberately absent: `nst_filters` is `match_included` plus the
    /// size and time filters (`Cmdline.hs:498`), and `match_excluded` is applied
    /// separately at `:495`. That distinction is load-bearing exactly once, in
    /// [`include_dirs`], and getting it wrong makes `arc l -x…` stop listing
    /// directories.
    pub fn has_nst(&self) -> bool {
        !self.include.is_empty()
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
/// # A deliberate divergence: `--dirs` duplicates a directory in the reference
///
/// `arc a --dirs x.arc .` over a tree containing `a/b/c` writes the entry `a`
/// **twice** — measured, on both `-r` and no `-r`. Only the top-level directory
/// of each filespec is affected; `a/b` and `a/b/c` appear once. It happens
/// because forcing `include_dirs` true makes `accept_f` accept the directory
/// both as a filespec match and again from the walk, and the default expression
/// it replaces happens to reject one of them.
///
/// This port writes the entry once. The archives are otherwise identical and
/// the two list the same set of names, so nothing downstream can tell them
/// apart except by byte count — a duplicate directory entry carries no data and
/// `removeDuplicates` collapses it on the next update anyway.
///
/// The alternative was to reproduce it and keep byte-identity under `--dirs`.
/// It is one line here if that is ever wanted; the harness compares
/// deduplicated name lists for `--dirs` instead of bytes, and byte-identity
/// still holds for every other filter combination including `--nodirs`.
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
            include: include.iter().map(|s| s.to_string()).collect(),
            exclude: exclude.iter().map(|s| s.to_string()).collect(),
            full_names: false,
        }
    }

    /// The values are measured from the reference build over a tree of
    /// a.txt, b.dat, sub/n.txt, sub/deep.dat.
    #[test]
    fn exclude_rejects_and_leaves_everything_else() {
        let x = f(&[], &["*.dat"]);
        assert!(x.accepts("a.txt"));
        assert!(x.accepts("sub/n.txt"));
        assert!(!x.accepts("b.dat"));
        assert!(!x.accepts("sub/deep.dat"));
        // `sub` matches no *.dat pattern, so the directory survives -- which is
        // what the reference does.
        assert!(x.accepts("sub"));
    }

    #[test]
    fn include_admits_only_what_matches_directories_included() {
        let n = f(&["*.txt"], &[]);
        assert!(n.accepts("a.txt"));
        assert!(n.accepts("sub/n.txt"));
        assert!(!n.accepts("b.dat"));
        // …and `sub` does not match *.txt either, so it is dropped. The
        // asymmetry with the -x case above is the two patterns, not two rules.
        assert!(!n.accepts("sub"));
    }

    /// Both together, in both orders on the command line: the result is an AND
    /// and does not depend on order. Measured -- an earlier reading of this as
    /// order-dependent came from an unquoted shell test, not from the binary.
    #[test]
    fn include_and_exclude_are_an_and() {
        let both = f(&["*"], &["*.dat"]);
        assert!(both.accepts("a.txt"));
        assert!(!both.accepts("b.dat"));
        let narrow = f(&["*.txt"], &["a.txt"]);
        assert!(!narrow.accepts("a.txt"), "excluded wins over included");
        assert!(narrow.accepts("sub/n.txt"));
    }

    /// Matching is on the BASE NAME unless --fullnames, which is why `-xsub`
    /// excludes nothing in a tree whose only `sub` is a directory component.
    #[test]
    fn matching_is_on_the_base_name_by_default() {
        let x = f(&[], &["n.txt"]);
        assert!(!x.accepts("sub/n.txt"), "the base name is what is matched");
        let full = FileFilter {
            include: Vec::new(),
            exclude: vec!["n.txt".to_string()],
            full_names: true,
        };
        assert!(full.accepts("sub/n.txt"), "--fullnames needs the whole path to match");
    }

    #[test]
    fn an_empty_filter_accepts_everything() {
        let none = f(&[], &[]);
        assert!(none.accepts("anything"));
        assert!(none.accepts(""));
        assert!(!none.has_nst());
    }

    /// `-x` is not an n/s/t filter and must not suppress directories on the
    /// read side; `-n` is and does.
    #[test]
    fn only_the_include_filter_counts_as_nst() {
        assert!(!f(&[], &["*.dat"]).has_nst());
        assert!(f(&["*.txt"], &[]).has_nst());

        assert!(include_dirs(None, true, &f(&[], &["*.dat"]), "l"));
        assert!(!include_dirs(None, true, &f(&["*.txt"], &[]), "l"));
        assert!(!include_dirs(None, false, &f(&[], &[]), "l"), "filespecs narrow it");
        assert!(!include_dirs(None, true, &f(&[], &[]), "e"), "e never takes directories");
        // The options win outright, including over `e`.
        assert!(include_dirs(Some(true), false, &f(&["*.txt"], &[]), "e"));
        assert!(!include_dirs(Some(false), true, &f(&[], &[]), "l"));
    }
}
