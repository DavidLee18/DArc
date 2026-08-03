//! The native `.7z` reader, replacing `Compression/7z` and its vendored copy of
//! the 7-Zip SDK.
//!
//! DArc reads `.7z` natively and writes it by shelling out to `7zz`/`7z` (see
//! `Arc7z.hs`), so this side is read-only: list, extract, test. Those three are
//! the entire C ABI -- `Compression.h:374-376` -- and this crate reimplements
//! exactly them, over [`sevenz_rust2`] instead of over 19 vendored `.c` files.
//!
//! # What "equivalent" means here
//!
//! Not byte-identity. Every codec ported before this one had to reproduce the C
//! byte for byte, because DArc emits those bytes and an archive that decodes
//! correctly but *encodes* differently is unreadable by every other build. No
//! `.7z` byte in existence was written by DArc, so the bar is behavioural: the
//! same entries listed, the same files written, the same CRCs checked, and the
//! same `SRes` code handed back to `Arc7z.hs`.
//!
//! The printed forms are reproduced deliberately, down to the 20-column date
//! placeholder the C never filled in, so that `arc l x.7z` output does not move
//! under anyone parsing it. `last_modified_date` is available now and the
//! placeholder could become a real date -- that is a user-visible change to a
//! listing format and belongs in its own commit, not smuggled into a port.
//!
//! # Two behaviours that are deliberately *not* reproduced
//!
//! * **Path traversal is refused.** `C_7z.c:197` built the output path with a
//!   bare `snprintf("%s/%s", out_dir, name)` and no validation, so an archive
//!   naming `../../etc/x` -- or an absolute path -- wrote outside the extraction
//!   directory. See [`safe_join`].
//! * **Entry data is streamed, not buffered.** `SzArEx_Extract` decoded a whole
//!   solid block into one heap buffer sized from the archive, so a crafted
//!   header could ask for an arbitrary allocation before a single byte was
//!   verified. Here each entry is copied through a fixed buffer.
//!
//! Extraction *progress lines* follow block order rather than file order, since
//! decoding in file order costs O(n^2) on a solid archive. The set of files
//! written is identical; only the order of the `- name` chatter differs.
//!
//! # This reads strictly more than the C did
//!
//! Not a design goal -- a consequence of the vendored SDK having been compiled
//! with most of its own decoders switched off. `rust/difftest/sevenz-check.sh`
//! runs both readers over the same archives and records where they part:
//!
//! | archive uses | vendored C | here |
//! |---|---|---|
//! | Copy, LZMA, LZMA2, BCJ x86, Delta, BCJ2 | reads | reads |
//! | PPMd | `SZ_ERROR_UNSUPPORTED` -- `Z7_PPMD_SUPPORT` is commented out in `7zDec.c` | reads |
//! | ARM64 / ARMT branch filters | `SZ_ERROR_UNSUPPORTED` -- `Z7_USE_FILTER_ARM64`/`_ARMT` never defined | reads |
//! | Delta chained ahead of BCJ2 | `SZ_ERROR_UNSUPPORTED` -- outside `CheckSupportedFolder`'s 4-coder template | reads |
//! | AES-256 | `SZ_ERROR_UNSUPPORTED` | `SZ_ERROR_UNSUPPORTED` (no `aes256` feature) |
//!
//! The harness therefore does not demand equal return codes in both directions.
//! It demands that this reader agree wherever the C succeeded, and never fail
//! where the C succeeded; the C refusing something this reads is allowed and
//! reported.

// The reader logic is unsafe-free and denied crate-wide; `exports` needs unsafe
// to read `*const c_char` from the caller and opts back in for itself, so the
// guarantee holds everywhere it can.
#![deny(unsafe_code)]
// Totality, matching darc-codecs and darc-crypto. `sevenz_rust2::Error` is not
// `#[non_exhaustive]`, so `sres` below matches every variant by name and a crate
// upgrade that adds one fails the build rather than silently folding it into a
// catch-all -- which is exactly what this lint is for.
#![deny(clippy::wildcard_enum_match_arm)]
#![deny(clippy::todo, clippy::unimplemented, clippy::mem_forget)]
#![deny(unused_must_use)]
// These run behind a C ABI where an unwind is undefined behaviour. `exports`
// installs a catch_unwind firewall regardless, but a panic that reaches it has
// already lost the error code it should have returned.
#![deny(clippy::unwrap_used, clippy::expect_used)]
#![allow(clippy::single_match)]

pub mod exports;

use std::fs::File;
use std::io::{Read, Write};
use std::os::raw::c_int;
use std::path::{Component, Path, PathBuf};

use sevenz_rust2::{ArchiveEntry, ArchiveReader, Error, Password};

/// `SRes` codes, from the `Compression/7z/sdk/7zTypes.h` this crate replaces.
///
/// `Arc7z.hs:64` reports the raw number back to the user (`"7z decoder returned
/// SRes=..."`), so these values are part of the observable behaviour and are
/// pinned rather than reinvented.
mod sres {
    use std::os::raw::c_int;

    pub const OK: c_int = 0;
    pub const DATA: c_int = 1;
    pub const MEM: c_int = 2;
    pub const CRC: c_int = 3;
    pub const UNSUPPORTED: c_int = 4;
    pub const PARAM: c_int = 5;
    pub const READ: c_int = 8;
    pub const WRITE: c_int = 9;
    pub const FAIL: c_int = 11;
    pub const ARCHIVE: c_int = 16;
    pub const NO_ARCHIVE: c_int = 17;
}

/// The `Date` column the C never populated: 19 spaces then `-`, exactly 20 wide.
///
/// Measured out of `C_7z.c`, not counted by eye.
const DATE_PLACEHOLDER: &str = "                   -";

/// Map a crate error onto the `SRes` code the C would have returned.
///
/// Every variant is named on purpose -- see the `wildcard_enum_match_arm` note
/// at the top of the file.
fn sres(err: &Error) -> c_int {
    match err {
        // Not a .7z at all.
        Error::BadSignature(_) => sres::NO_ARCHIVE,

        // Structurally a .7z, but one this reader cannot parse.
        Error::UnsupportedVersion { .. } => sres::UNSUPPORTED,
        Error::ExternalUnsupported => sres::UNSUPPORTED,
        Error::UnsupportedCompressionMethod(_) => sres::UNSUPPORTED,
        Error::Unsupported(_) => sres::UNSUPPORTED,

        // Encrypted. The vendored C had no AES either and also failed here; see
        // the `aes256` note in Cargo.toml for what it would take to support.
        Error::PasswordRequired => sres::UNSUPPORTED,
        Error::MaybeBadPassword(_) => sres::UNSUPPORTED,

        // Integrity.
        Error::ChecksumVerificationFailed => sres::CRC,
        Error::NextHeaderCrcMismatch => sres::CRC,

        // Malformed header structure.
        Error::BadTerminatedStreamsInfo(_) => sres::ARCHIVE,
        Error::BadTerminatedUnpackInfo => sres::ARCHIVE,
        Error::BadTerminatedPackInfo(_) => sres::ARCHIVE,
        Error::BadTerminatedSubStreamsInfo => sres::ARCHIVE,
        Error::BadTerminatedHeader(_) => sres::ARCHIVE,
        Error::Other(_) => sres::DATA,

        // Resources and I/O. `FileOpen` is the case `InFile_Open` failed on, and
        // the C returned SZ_ERROR_FAIL for it (`C_7z.c:99`).
        Error::MaxMemLimited { .. } => sres::MEM,
        Error::FileOpen(_, _) => sres::FAIL,
        Error::FileNotFound => sres::FAIL,
        Error::Io(_, _) => sres::READ,
    }
}

/// Join `name` onto `dest`, refusing anything that escapes it.
///
/// `C_7z.c` did not do this. It formatted `out_dir + "/" + name` straight from
/// the archive, so `../../x` climbed out of the extraction directory and a name
/// like `/etc/x`... did not, as it happens, because the C always prefixed
/// `out_dir/` -- but `..` alone was enough, and this is reached from `arc x` on
/// an archive the user did not write.
///
/// Backslash is treated as a separator too, so a Windows-authored `..\..\x` is
/// caught when extracting on Unix, where `Path` would otherwise see one
/// filename component with backslashes in it.
fn safe_join(dest: &Path, name: &str) -> Result<PathBuf, c_int> {
    let normalized = name.replace('\\', "/");
    let mut out = dest.to_path_buf();
    for component in Path::new(&normalized).components() {
        match component {
            Component::Normal(part) => out.push(part),
            Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                eprintln!("7z: refusing entry whose path escapes the output directory: {name}");
                return Err(sres::DATA);
            }
        }
    }
    Ok(out)
}

/// Write one line, turning a broken pipe or full disk into `SZ_ERROR_WRITE`
/// rather than a panic. `unused_must_use` is denied, so this cannot be skipped.
fn put(out: &mut dyn Write, line: &str) -> Result<(), c_int> {
    match writeln!(out, "{line}") {
        Ok(()) => Ok(()),
        Err(_) => Err(sres::WRITE),
    }
}

/// Open the archive, reporting the failure the way `Arc7z.hs` expects.
///
/// The file is opened here rather than through `ArchiveReader::open` so that a
/// missing or unreadable archive keeps returning `SZ_ERROR_FAIL`, which is what
/// `C_7z.c:99` returned when `InFile_Open` failed. The crate folds that case
/// into `Error::Io`, which is indistinguishable from a read error part-way
/// through a valid archive -- a distinction `Arc7z.hs` surfaces to the user as a
/// number, so it is worth keeping.
fn open(archive: &Path) -> Result<ArchiveReader<File>, c_int> {
    let file = match File::open(archive) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("7z: cannot open {}: {err}", archive.display());
            return Err(sres::FAIL);
        }
    };
    // Password::empty(): without the `aes256` feature an encrypted archive
    // surfaces as Error::PasswordRequired, which maps to SZ_ERROR_UNSUPPORTED --
    // the same refusal the vendored C gave.
    match ArchiveReader::new(file, Password::empty()) {
        Ok(reader) => Ok(reader),
        Err(err) => {
            eprintln!("7z: cannot read {}: {err}", archive.display());
            Err(sres(&err))
        }
    }
}

/// The body of [`list`], split out so the `stdout` borrow ends before the flush.
fn list_lines(reader: &ArchiveReader<File>, out: &mut dyn Write) -> Result<(), c_int> {
    put(out, &format!("{:<20} {:>10}  {}", "Date", "Size", "Name"))?;
    for entry in &reader.archive().files {
        let suffix = match entry.is_directory() {
            true => "/",
            false => "",
        };
        put(
            out,
            &format!(
                "{:<20} {:>10}  {}{}",
                DATE_PLACEHOLDER,
                entry.size(),
                entry.name(),
                suffix
            ),
        )?;
    }
    Ok(())
}

/// `darc_7z_list` -- print one line per entry.
pub fn list(archive: &Path) -> c_int {
    let reader = match open(archive) {
        Ok(reader) => reader,
        Err(code) => return code,
    };

    let stdout = std::io::stdout();
    let mut out = stdout.lock();
    let result = list_lines(&reader, &mut out);
    drop(out.flush());

    match result {
        Ok(()) => sres::OK,
        Err(code) => code,
    }
}

/// One entry, for either direction. `out_dir == None` is test-only: nothing is
/// written, but the data is still read to the end so the CRC is checked --
/// draining is what makes `arc t` mean anything, since the verifying reader only
/// compares once the expected byte count has been consumed.
fn handle_entry(
    entry: &ArchiveEntry,
    data: &mut dyn Read,
    out_dir: Option<&Path>,
    out: &mut dyn Write,
) -> Result<(), c_int> {
    let name = entry.name();

    if entry.is_directory() {
        // The C created the directory and printed nothing (`C_7z.c:181-188`).
        match out_dir {
            Some(dir) => {
                let target = safe_join(dir, name)?;
                match std::fs::create_dir_all(&target) {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("7z: cannot create {}: {err}", target.display());
                        return Err(sres::FAIL);
                    }
                }
            }
            None => {}
        }
        return Ok(());
    }

    match out_dir {
        None => {
            match std::io::copy(data, &mut std::io::sink()) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("7z: {name}: {err}");
                    return Err(sres::DATA);
                }
            }
            put(out, &format!("T {name}"))
        }
        Some(dir) => {
            let target = safe_join(dir, name)?;
            // Directory entries arrive after every block (see for_each_entries),
            // so a file's parent may not exist yet. Create it per file rather
            // than relying on ordering.
            match target.parent() {
                Some(parent) => match std::fs::create_dir_all(parent) {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("7z: cannot create {}: {err}", parent.display());
                        return Err(sres::FAIL);
                    }
                },
                None => {}
            }
            let mut file = match File::create(&target) {
                Ok(file) => file,
                Err(err) => {
                    eprintln!(
                        "7z: cannot open {} for writing: {err}",
                        target.display()
                    );
                    return Err(sres::FAIL);
                }
            };
            match std::io::copy(data, &mut file) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("7z: {}: {err}", target.display());
                    // A CRC mismatch surfaces through the copy as an io error
                    // from the verifying reader, so this is not purely a disk
                    // failure; DATA is the honest generic answer.
                    return Err(sres::DATA);
                }
            }
            put(out, &format!("- {name}"))
        }
    }
}

/// `darc_7z_extract` (`out_dir == Some`) and `darc_7z_test` (`None`).
pub fn extract_or_test(archive: &Path, out_dir: Option<&Path>) -> c_int {
    let mut reader = match open(archive) {
        Ok(reader) => reader,
        Err(code) => return code,
    };

    let stdout = std::io::stdout();
    let mut out = stdout.lock();

    // for_each_entries stops on Ok(false); the code that caused it is carried
    // out here rather than squeezed into the crate's error type, so the SRes the
    // caller sees is the one this file chose.
    let mut failure = sres::OK;
    let walked = reader.for_each_entries(|entry, data| {
        match handle_entry(entry, data, out_dir, &mut out) {
            Ok(()) => Ok(true),
            Err(code) => {
                failure = code;
                Ok(false)
            }
        }
    });

    drop(out.flush());

    match failure {
        sres::OK => {}
        code => return code,
    }
    match walked {
        Ok(()) => sres::OK,
        Err(err) => {
            eprintln!("7z: {}: {err}", archive.display());
            sres(&err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn date_placeholder_is_twenty_wide() {
        // The C wrote it through %-20s; anything else shifts every column.
        assert_eq!(DATE_PLACEHOLDER.len(), 20);
        assert!(DATE_PLACEHOLDER.ends_with('-'));
        assert_eq!(DATE_PLACEHOLDER.matches(' ').count(), 19);
    }

    #[test]
    fn header_matches_the_c_format() {
        // printf("%-20s %10s  %s\n", "Date", "Size", "Name")
        assert_eq!(
            format!("{:<20} {:>10}  {}", "Date", "Size", "Name"),
            "Date                       Size  Name"
        );
    }

    #[test]
    fn safe_join_accepts_ordinary_names() {
        let root = Path::new("/tmp/out");
        assert_eq!(
            safe_join(root, "dir/file.txt"),
            Ok(PathBuf::from("/tmp/out/dir/file.txt"))
        );
        assert_eq!(
            safe_join(root, "./a/./b"),
            Ok(PathBuf::from("/tmp/out/a/b"))
        );
    }

    #[test]
    fn safe_join_refuses_traversal() {
        let root = Path::new("/tmp/out");
        // What C_7z.c would have written straight through.
        assert_eq!(safe_join(root, "../../etc/passwd"), Err(sres::DATA));
        assert_eq!(safe_join(root, "/etc/passwd"), Err(sres::DATA));
        // Backslash form, which on Unix is one component to `Path` and would
        // otherwise slip past a components() check.
        assert_eq!(safe_join(root, "..\\..\\etc\\passwd"), Err(sres::DATA));
        // Buried in the middle rather than leading.
        assert_eq!(safe_join(root, "a/../../b"), Err(sres::DATA));
    }
}
