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
use rayon::prelude::*;
use std::io::Write;

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
            // Both separators, or `arc e` is only flat on Unix: a stored
            // `sub\a.txt` would keep its `sub\` and create a directory on
            // Windows, which is the one thing this layout exists to prevent.
            0 => filtered.rsplit(SEPARATORS).next().unwrap_or(filtered).to_string(),
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

/// Both path separators, on every platform.
///
/// Windows is a first-class target here (`windows-cross`, `windows-arm64-test`
/// and `interop-windows-amd64` all ship binaries), and on Windows `\` separates
/// path components. Treating only `/` as a separator meant `..\..\evil` had no
/// component equal to `..` and passed every check below.
///
/// Applied on ALL platforms rather than under `cfg!(windows)`, deliberately. A
/// name is a property of the archive, not of the machine unpacking it: an
/// archive that is safe to extract on Linux and dangerous on Windows is a worse
/// object than one that means the same thing everywhere. The cost is refusing a
/// Unix filename that genuinely contains a backslash, which is legal but
/// pathological, and refusing it is the trade we want.
const SEPARATORS: [char; 2] = ['/', '\\'];

/// `stripRoot = dropDrive` — remove `d:\` or a leading separator, so an absolute
/// path in the archive cannot become an absolute path on disk.
fn strip_root(path: &str) -> String {
    let bytes = path.as_bytes();
    // "d:/rest", "d:\rest" or "d:rest"
    if bytes.len() >= 2 && bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
        return path[2..].trim_start_matches(SEPARATORS).to_string();
    }
    // Also strips a UNC prefix's leading separators (`\\server\share`), which
    // on Windows names a remote host rather than anything under the
    // destination.
    path.trim_start_matches(SEPARATORS).to_string()
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
    // A leading separator of either kind is an absolute path, and two of them
    // is a Windows UNC path naming another host.
    if name.starts_with(SEPARATORS) {
        return false;
    }
    if name.as_bytes().len() >= 2
        && name.as_bytes()[1] == b':'
        && name.as_bytes()[0].is_ascii_alphabetic()
    {
        return false;
    }
    !name.split(SEPARATORS).any(|c| c == "..")
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

    /// Windows separators. Before these, `is_safe` split only on `/`, so
    /// `..\..\evil` had no component equal to `..`, returned true, and
    /// `Path::join` on Windows -- a shipped target -- wrote it outside the
    /// destination. Nothing about this is a conformance question; the reference
    /// is equally wrong.
    #[test]
    fn a_backslash_cannot_escape_the_destination() {
        for bad in [
            r"..\..\evil",
            r"sub\..\..\evil",
            r"a/b\..\..\..\evil",
            r"\evil",           // absolute from the drive root
            r"\\server\share",  // UNC: another host entirely
            r"..\\..\\evil",
        ] {
            assert!(!is_safe(bad), "{bad:?} must be refused");
        }
        // Still safe, and must stay accepted.
        for ok in [r"sub\dir\a.txt", "sub/dir/a.txt", "a..b", "..a", "a.."] {
            assert!(is_safe(ok), "{ok:?} must remain accepted");
        }
    }

    /// `strip_root`'s doc always claimed it removed `d:\`; it trimmed only `/`,
    /// so `d:\evil` became `\evil` -- an absolute path from the drive root.
    #[test]
    fn strip_root_removes_both_separators_and_a_drive() {
        assert_eq!(strip_root(r"d:\evil"), "evil");
        assert_eq!(strip_root("d:/evil"), "evil");
        assert_eq!(strip_root(r"\evil"), "evil");
        assert_eq!(strip_root("/evil"), "evil");
        assert_eq!(strip_root(r"\\server\share"), r"server\share");
        assert_eq!(strip_root("plain/x"), "plain/x");
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

// ── Shared with the `unarc` front-end ───────────────────────────────────────
//
// `run_blocks` and the two formatters below used to live in `bin/darc.rs`.
// They are here because `unarc` and every SFX module need exactly this work --
// walk the data blocks, decrypt, decompress, verify each file's CRC, and either
// report or write it -- and a second copy is how `Unarc/ArcStructure.h` came to
// read the per-file time field as 4 bytes while the writer wrote 8, so that
// directories extracted as empty files and every CRC failed.
//
// One implementation, two callers. That is the whole reason the C++ reader is
// being retired rather than ported alongside this one.

/// `showMem3`-style digit grouping: `438744` -> `438.744`.
pub fn show3(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::with_capacity(s.len() + s.len() / 3);
    let b = s.as_bytes();
    for (i, c) in b.iter().enumerate() {
        if i > 0 && (b.len() - i) % 3 == 0 {
            out.push('.');
        }
        out.push(*c as char);
    }
    out
}

/// `ratio3` (`UIBase.hs:159`) — integer arithmetic, which truncates where a
/// rounding formatter would not.
pub fn ratio3(count: u64, total: u64) -> String {
    if total == 0 {
        return "0.0".to_string();
    }
    let scaled = (count.saturating_mul(1000) / total).to_string();
    match scaled.len() {
        1 => format!("0.{scaled}"),
        n => format!("{}.{}", &scaled[..n - 1], &scaled[n - 1..]),
    }
}
pub fn run_blocks(
    info: &crate::archive::ArchiveInfo,
    data: &[u8],
    layout: &Layout,
    extracting: bool,
    pw: &crate::passwords::Passwords,
    entries: &[Entry],
    skip: &std::collections::HashSet<String>,
    keep_broken: bool,
) -> i32 {
    // The safety check runs on the archive's contribution alone: the
    // destination is the user's own and may be absolute.
    let relative = Layout { disk_basedir: String::new(), ..layout.clone() };

    if extracting && layout.creates_directories() {
        for e in entries.iter().filter(|e| e.is_dir) {
            if !crate::extract::is_safe(&relative.disk_name(e)) {
                eprintln!("ERROR: refusing unsafe path {:?}", e.stored_name);
                return 2;
            }
            match std::fs::create_dir_all(layout.disk_name(e)) {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("ERROR: {}: {err}", e.stored_name);
                    return 2;
                }
            }
        }
    }

    let total_bytes: u64 = entries.iter().map(|e| e.size).sum();
    if !extracting {
        println!(
            "Testing {} files, {} bytes.",
            show3(entries.len() as u64),
            show3(total_bytes)
        );
    }

    let mut per_block: Vec<Vec<&Entry>> = vec![Vec::new(); info.data_blocks.len()];
    for e in entries {
        match per_block.get_mut(e.block) {
            Some(v) => v.push(e),
            None => {}
        }
    }

    // Counted where the write happens, not from `entries`, and atomics rather
    // than a per-block return so the early exits above stay as they are.
    use std::sync::atomic;
    let written_files = atomic::AtomicU64::new(0);
    let written_bytes = atomic::AtomicU64::new(0);

    let results: Vec<Vec<String>> = per_block
        .par_iter()
        .enumerate()
        .map(|(bi, entries)| {
            let mut bad = Vec::new();
            let b = match info.data_blocks.get(bi) {
                Some(b) => b,
                None => return bad,
            };
            if entries.iter().all(|e| e.is_dir) {
                return bad;
            }
            let start = b.pos as usize;
            let end = start.saturating_add(b.comp_size as usize);
            let packed = match data.get(start..end) {
                Some(s) => s,
                None => {
                    bad.push(format!("{} is truncated", b.name()));
                    return bad;
                }
            };
            // The block's chain may name an encryption method, which carries a
            // salt but no key until a password has been verified against it.
            let compressor = match crate::archive::keyed(&b.compressor, b, pw) {
                Ok(c) => c,
                Err(e) => {
                    bad.push(format!("{e}"));
                    return bad;
                }
            };
            let unpacked =
                match crate::decompress::decompress_chain(&compressor, packed, b.orig_size as usize) {
                    Ok(u) => u,
                    Err(e) => {
                        bad.push(format!("{}: {e}", b.name()));
                        return bad;
                    }
                };
            for e in entries {
                if e.is_dir {
                    continue;
                }
                let from = e.pos_in_block as usize;
                let to = from.saturating_add(e.size as usize);
                let bytes = match unpacked.get(from..to) {
                    Some(x) => x,
                    None => {
                        bad.push(format!("{}: runs past the end of its block", e.stored_name));
                        continue;
                    }
                };
                // `-kb`/`--keepbroken` (ArcExtract.hs:99): a file that failed
                // its CRC is normally not left on disk. With -kb it is kept,
                // and the failure is still reported -- keeping the bytes is not
                // the same as calling them good.
                let crc_ok = crate::crc::calc(bytes) == e.crc;
                if !crc_ok {
                    bad.push(format!("{}: CRC failed", e.stored_name));
                    if !keep_broken {
                        continue;
                    }
                }
                if !extracting {
                    continue;
                }
                if !crate::extract::is_safe(&relative.disk_name(e)) {
                    bad.push(format!("refusing unsafe path {:?}", e.stored_name));
                    continue;
                }
                // `-o-`, or an existing file the user declined to overwrite.
                // Decided serially before this loop; see resolve_overwrites.
                if skip.contains(&e.stored_name) {
                    continue;
                }
                let name = layout.disk_name(e);
                match std::path::Path::new(&name).parent() {
                    Some(dir) => match std::fs::create_dir_all(dir) {
                        Ok(()) => {}
                        Err(err) => {
                            bad.push(format!("{}: {err}", dir.display()));
                            continue;
                        }
                    },
                    None => {}
                }
                match std::fs::File::create(&name).and_then(|mut f| f.write_all(bytes)) {
                    // Counted HERE, at the only place a file is actually
                    // written, rather than from the selected-entry list. See
                    // the summary below for why that distinction matters.
                    Ok(()) => {
                        written_files.fetch_add(1, atomic::Ordering::Relaxed);
                        written_bytes.fetch_add(bytes.len() as u64, atomic::Ordering::Relaxed);
                    }
                    Err(err) => bad.push(format!("{name}: {err}")),
                }
            }
            bad
        })
        .collect();

    let mut failures = 0usize;
    for block in &results {
        for msg in block {
            eprintln!("ERROR: {msg}");
            failures += 1;
        }
    }

    if !extracting {
        // Blocks whose unpacked size is zero are never decompressed, so they
        // contribute nothing to the packed total.
        let packed: u64 =
            info.data_blocks.iter().filter(|b| b.orig_size > 0).map(|b| b.comp_size).sum();
        println!(
            "Tested {} files, {} => {} bytes. Ratio {}%",
            show3(entries.len() as u64),
            show3(packed),
            show3(total_bytes),
            ratio3(packed, total_bytes)
        );
    } else {
        // What was WRITTEN, not what was selected. `entries.len()` counted every
        // entry the filters picked -- including directories, and including
        // files that were then refused for an unsafe path, failed their CRC, or
        // were skipped at an overwrite prompt. So a run that refused its only
        // entry still printed "Extracted 1 files", directly under the ERROR
        // saying it had not been.
        //
        // **A DELIBERATE DIVERGENCE, and #140 recorded the wrong reason for it.**
        // That commit claimed the reference prints no summary here, so there was
        // nothing to match; it does print one, and I had missed it by tailing
        // output whose progress is drawn with carriage returns. It reads
        //
        //     Extracted 226 files, 33.111 => 438.744 bytes. Ratio 7.5%
        //
        // on a tree of 218 files and 8 directories -- the same entries-not-files
        // count this used to have. So the port is right and the reference is
        // wrong, which is the trade CLAUDE.md asks for, but it IS a divergence
        // and is marked as one here rather than left looking like a free fix.
        // `arc-cli-check.sh` holds both builds against the disk and reports the
        // reference's miscount without failing on it.
        println!(
            "Extracted {} files, {} bytes.",
            show3(written_files.load(atomic::Ordering::Relaxed)),
            show3(written_bytes.load(atomic::Ordering::Relaxed))
        );
    }
    if failures == 0 {
        println!("All OK");
        0
    } else {
        2
    }
}
