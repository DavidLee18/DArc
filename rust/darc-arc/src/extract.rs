//! Extraction — `arcExtract` / `extractFile` (`ArcExtract.hs:57`).
//!
//! # Where a file's disk name comes from
//!
//! Not from the stored name directly. `archiveReadDir` builds three names per
//! entry and the disk one is chosen by the `-ep` option (`ArhiveDirectory.hs:291`):
//!
//! | `ep` | `make_disk_name` | meaning |
//! |---|---|---|
//! | 0 | `const ""` | the `e` command — drop every directory, extract flat |
//! | 3 | `id` | `-ep3`, keep the full stored path |
//! | else | `stripRoot` | the default — drop a leading `d:\` or `/` |
//!
//! and the result is prefixed with `disk_basedir`. `arc_basedir` is dropped from
//! the front of the stored name first.
//!
//! # Concurrency
//!
//! A solid block is decompressed **once** and its files written from the one
//! buffer. Blocks are independent, so they run in parallel; within a block the
//! files are written in order, because that is the order they occupy the stream
//! and writing them in any other order would mean holding more of it.

use crate::directory::Entry;

/// How stored names become disk names — the `-ep` and basedir options.
#[derive(Clone, Debug)]
pub struct Layout {
    /// `opt_arc_basedir`: dropped from the front of the stored name.
    pub arc_basedir: String,
    /// `opt_disk_basedir`: prefixed to the result.
    pub disk_basedir: String,
    /// `opt_dir_exclude_path`. 0 for the `e` command, 3 for `-ep3`.
    pub ep: u32,
}

impl Default for Layout {
    /// What `arc x` with no options means.
    fn default() -> Self {
        Layout { arc_basedir: String::new(), disk_basedir: String::new(), ep: 1 }
    }
}

impl Layout {
    /// `arc e` — every file into the destination directory, flat.
    pub fn flat() -> Self {
        Layout { ep: 0, ..Default::default() }
    }

    /// Whether a directory ENTRY is materialised on disk.
    ///
    /// Measured, not derived: `arc e` on a corpus with eight directories creates
    /// none of them, while `arc x` creates all eight, and the 218 files are
    /// identical either way. Extracting flat makes directories meaningless, so
    /// this is consistent -- but it does not fall out of `make_disk_name` on its
    /// own, and `arc x -ep0` (also ep 0) behaves like neither, so the rule is
    /// pinned to what the reference does rather than to a derivation.
    pub fn creates_directories(&self) -> bool {
        self.ep != 0
    }

    /// `fpFullname . fiDiskName`, for one entry.
    pub fn disk_name(&self, entry: &Entry) -> String {
        // drop_arc_basedir: `drop (length arc_basedir + 1)`, i.e. the basedir
        // AND the separator after it.
        let filtered: &str = if self.arc_basedir.is_empty() {
            &entry.stored_name
        } else {
            entry
                .stored_name
                .get(self.arc_basedir.len() + 1..)
                .unwrap_or(&entry.stored_name)
        };
        let name = match self.ep {
            // `const ""` — the parent directory becomes empty, so only the base
            // name survives.
            0 => filtered.rsplit('/').next().unwrap_or(filtered).to_string(),
            3 => filtered.to_string(),
            // stripRoot = dropDrive. On a Unix path that is the leading '/'.
            _ => strip_root(filtered),
        };
        if self.disk_basedir.is_empty() {
            name
        } else {
            format!("{}/{}", self.disk_basedir.trim_end_matches('/'), name)
        }
    }
}

/// `stripRoot = dropDrive` — remove `d:\` or a leading separator, so an absolute
/// path in the archive cannot become an absolute path on disk.
fn strip_root(path: &str) -> String {
    let bytes = path.as_bytes();
    // "d:/rest" or "d:rest"
    if bytes.len() >= 2 && bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
        return path[2..].trim_start_matches('/').to_string();
    }
    path.trim_start_matches('/').to_string()
}

/// Reject a name that would write outside the destination.
///
/// `remove_unsafe_dirs` already strips `..` from the *directory* part when the
/// directory is read, but the per-file base name is never passed through it, and
/// neither is a name from an archive written by another tool. Extraction is the
/// one place where a bad name becomes a file somewhere it should not be, so it
/// is checked here rather than trusted from upstream.
pub fn is_safe(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    if name.starts_with('/') {
        return false;
    }
    if name.as_bytes().len() >= 2
        && name.as_bytes()[1] == b':'
        && name.as_bytes()[0].is_ascii_alphabetic()
    {
        return false;
    }
    !name.split('/').any(|c| c == "..")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(stored: &str) -> Entry {
        Entry {
            stored_name: stored.to_string(),
            size: 0,
            time: 0,
            is_dir: false,
            crc: 0,
            block: 0,
            pos_in_block: 0,
        }
    }

    #[test]
    fn the_default_keeps_the_stored_path() {
        let l = Layout::default();
        assert_eq!(l.disk_name(&entry("sub/dir/a.txt")), "sub/dir/a.txt");
    }

    /// The `e` command extracts flat: `make_disk_name = const ""`, so the
    /// directory part disappears entirely and only the base name is left.
    #[test]
    fn the_e_command_drops_every_directory() {
        let l = Layout::flat();
        assert_eq!(l.disk_name(&entry("sub/dir/a.txt")), "a.txt");
        assert_eq!(l.disk_name(&entry("a.txt")), "a.txt");
    }

    /// -ep3 keeps the full name, including a leading root that the default
    /// strips. That is the option's whole purpose.
    #[test]
    fn ep3_keeps_a_root_that_the_default_strips() {
        let full = Layout { ep: 3, ..Default::default() };
        let default = Layout::default();
        assert_eq!(full.disk_name(&entry("/etc/passwd")), "/etc/passwd");
        assert_eq!(default.disk_name(&entry("/etc/passwd")), "etc/passwd");
    }

    /// `arc e` creates no directories; `arc x` creates them all.
    #[test]
    fn only_the_x_layout_materialises_directories() {
        assert!(Layout::default().creates_directories());
        assert!(Layout { ep: 3, ..Default::default() }.creates_directories());
        assert!(!Layout::flat().creates_directories());
    }

    #[test]
    fn a_windows_drive_letter_is_stripped_by_default() {
        let l = Layout::default();
        assert_eq!(l.disk_name(&entry("d:/data/x.bin")), "data/x.bin");
        assert_eq!(l.disk_name(&entry("d:data/x.bin")), "data/x.bin");
    }

    /// arc_basedir is dropped WITH its separator -- `drop (length + 1)`.
    #[test]
    fn the_archive_basedir_is_dropped_with_its_separator() {
        let l = Layout { arc_basedir: "sub".to_string(), ..Default::default() };
        assert_eq!(l.disk_name(&entry("sub/dir/a.txt")), "dir/a.txt");
    }

    #[test]
    fn the_disk_basedir_is_prefixed() {
        let l = Layout { disk_basedir: "/tmp/out".to_string(), ..Default::default() };
        assert_eq!(l.disk_name(&entry("sub/a.txt")), "/tmp/out/sub/a.txt");
        let l = Layout { disk_basedir: "/tmp/out/".to_string(), ..Default::default() };
        assert_eq!(l.disk_name(&entry("sub/a.txt")), "/tmp/out/sub/a.txt", "no doubled separator");
    }

    /// Extraction is where a hostile name becomes a file in the wrong place.
    /// The directory reader strips ".." from the DIRECTORY part; the base name
    /// and archives written by other tools are not covered by that.
    #[test]
    fn names_that_would_escape_the_destination_are_refused() {
        assert!(!is_safe("../outside"));
        assert!(!is_safe("a/../../outside"));
        assert!(!is_safe("/etc/passwd"));
        assert!(!is_safe("d:/windows/system32"));
        assert!(!is_safe(""));
        // ...and ordinary names are not.
        assert!(is_safe("a.txt"));
        assert!(is_safe("sub/dir/a.txt"));
        assert!(is_safe("..hidden"), "a leading .. in a NAME is not a traversal");
        assert!(is_safe("a..b/c"));
    }
}
