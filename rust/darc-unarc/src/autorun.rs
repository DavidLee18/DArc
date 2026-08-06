//! The installer SFX: extract to a scratch directory, run one program from it,
//! then delete the directory.
//!
//! This restores the one thing FreeArc's SFX could do that DArc's deliberately
//! could not. `Unarc/unarc.cpp` built it behind `-DFREEARC_INSTALLER` (lines 84,
//! 454, 515): on a bare double-click it made `%TEMP%/installer<N>`, extracted
//! into it, ran a **hardcoded `setup.exe`**, and recursively deleted the
//! directory. That code was deleted with the rest of `Unarc/` in #151.
//!
//! # Four deliberate divergences, and why
//!
//! 1. **The command is in the ARCHIVE, not the binary.** The C hardcoded
//!    `setup.exe`; here it is `--autorun'CMD'` recorded in the footer. That is
//!    what keeps ONE `unarc` deciding at runtime. A second build behind a
//!    feature flag is what caused #149: two cargo outputs collide on
//!    `target/release/unarc` and silently overwrite each other.
//!
//! 2. **It asks first.** The C ran the payload silently on the most likely user
//!    action, which is the shape of a piece of malware, not of an installer.
//!    This prints the command and requires a `y`. `-y` says yes in advance, so
//!    an unattended install is still one flag away — but it has to be asked
//!    for. Anything that is not a `y` — a `n`, a stray word, EOF because stdin
//!    is closed or redirected from `/dev/null` — declines. Defaulting to "no"
//!    on EOF matters more than it looks: a non-interactive context is exactly
//!    where a prompt is not seen.
//!
//! 3. **The command must resolve inside the extracted directory.** Checked when
//!    the archive is written (`darc` refuses `--autorun'../../bin/sh'`) and
//!    again here, because the archive may not have been written by this build.
//!
//! 4. **The child's exit code is propagated.** The C discarded it, so a failed
//!    install reported success.
//!
//! Everything else is kept: `create_dir` in a unique-name loop rather than
//! `create_dir_all`, so that failing when the directory already exists is what
//! stops it adopting one an attacker planted; and the recursive wipe.
//!
//! # What still never runs anything
//!
//! `darc l`, `darc t` and `darc x` on an SFX file, and `unarc` given any
//! explicit command. Opening an archive to see what is in it must never be able
//! to execute it.

use darc_arc::{archive::ArchiveInfo, extract::Layout, passwords::Passwords};
use std::io::Write;
use std::path::{Path, PathBuf};

/// Split the stored command into a program and its arguments.
///
/// Whitespace-separated, with no quoting: the program is a path inside the
/// archive, and a path that needs quoting to survive this is one this refuses
/// to run rather than one it guesses about.
fn split(command: &str) -> Option<(String, Vec<String>)> {
    let mut words = command.split_ascii_whitespace().map(str::to_string);
    let program = words.next()?;
    Some((program, words.collect()))
}

/// Reject a command that names anything outside the extracted tree.
///
/// `is_safe` is the archiver's own check — the one that stops an entry called
/// `../../etc/passwd` being written — reused rather than reimplemented, because
/// a second copy of a path rule is how the two drift apart.
fn program_is_safe(program: &str) -> bool {
    darc_arc::extract::is_safe(program)
}

/// A scratch directory nobody else owns.
///
/// `create_dir`, never `create_dir_all`: the error when the directory already
/// exists is the whole point. `create_dir_all` succeeds on an existing
/// directory, which would let anything that can write to the temp directory
/// pre-create the name and have the payload extracted into a tree it controls.
///
/// On Unix the mode is set **at creation** via `DirBuilder`, not with a
/// `set_permissions` afterwards: between the two there is a window in which the
/// directory is world-readable, and the files land in it during exactly that
/// window.
fn scratch_dir() -> std::io::Result<PathBuf> {
    let base = std::env::temp_dir();
    let pid = std::process::id();
    let mut last = None;
    for n in 0..1000u32 {
        // Not random, and it does not need to be: uniqueness comes from
        // `create_dir` refusing an existing name, not from being unguessable.
        // Guessing the name buys nothing when creating it first makes the run
        // abort rather than proceed.
        let dir = base.join(format!("darc-sfx-{pid}-{n}"));
        let mut b = std::fs::DirBuilder::new();
        #[cfg(unix)]
        {
            use std::os::unix::fs::DirBuilderExt;
            b.mode(0o700);
        }
        match b.create(&dir) {
            Ok(()) => return Ok(dir),
            Err(e) => last = Some(e),
        }
    }
    Err(last.unwrap_or_else(|| std::io::Error::other("no free scratch directory name")))
}

/// Ask, and read the answer. Anything but `y` is no, and so is EOF.
fn confirmed(command: &str, dir: &Path) -> bool {
    println!("This archive wants to run a program after extracting itself.");
    println!("  command:   {command}");
    println!("  directory: {}", dir.display());
    print!("Run it? [y/N] ");
    let _ = std::io::stdout().flush();
    let mut answer = String::new();
    match std::io::stdin().read_line(&mut answer) {
        Ok(0) | Err(_) => false,
        Ok(_) => matches!(answer.trim(), "y" | "Y" | "yes" | "Yes"),
    }
}

/// Make the program executable. Extraction does not store or restore a Unix
/// mode, so a freshly written file is 0644 and `execve` would fail with EACCES
/// on something the archive plainly intended to run.
#[cfg(unix)]
fn make_executable(path: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o700))
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) -> std::io::Result<()> {
    Ok(())
}

/// Extract into a scratch directory, run the command, wipe, and return the
/// child's exit code.
///
/// The wipe happens on **every** path out of here once the directory exists,
/// including the ones that refuse to run anything: a declined installer must
/// not leave its payload lying in the temp directory.
pub fn run(
    command: &str,
    info: &ArchiveInfo,
    data: &[u8],
    pw: &Passwords,
    entries: &[darc_arc::directory::Entry],
    assume_yes: bool,
) -> i32 {
    let (program, args) = match split(command) {
        Some(p) => p,
        None => {
            eprintln!("ERROR: this archive records an empty autorun command");
            return 2;
        }
    };
    // Checked here as well as at creation time, because the archive in hand was
    // not necessarily written by this build.
    if !program_is_safe(&program) {
        eprintln!("ERROR: refusing autorun {program:?}: it names a path outside the archive");
        return 2;
    }

    let dir = match scratch_dir() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("ERROR: can't make a scratch directory: {e}");
            return 2;
        }
    };

    // Asked BEFORE extracting rather than after: declining should not have cost
    // the user the time and the disk space of a full extraction first.
    if !assume_yes && !confirmed(command, &dir) {
        println!("Not running it. Nothing was extracted.");
        wipe(&dir);
        return 2;
    }

    let layout =
        Layout { disk_basedir: dir.to_string_lossy().into_owned(), ..Layout::default() };
    let skip = std::collections::HashSet::new();
    println!("Extracting to {}", dir.display());
    let code = darc_arc::extract::run_blocks(info, data, &layout, true, pw, entries, &skip, false);
    if code != 0 {
        // A CRC failure or a bad password. Running a payload out of a tree that
        // did not extract cleanly is running something other than what was
        // signed off, so this stops.
        eprintln!("ERROR: extraction failed; not running {program:?}");
        wipe(&dir);
        return code;
    }

    let exe = dir.join(&program);
    // Last check, after extraction: the resolved program must still be under
    // the scratch directory. `is_safe` reasons about the stored string; this
    // reasons about what is on disk, which is what `execve` will follow.
    // Both sides are canonicalized, since the temp directory itself is very
    // often a symlink (`/tmp` -> `/private/tmp` on macOS).
    match (exe.canonicalize(), dir.canonicalize()) {
        (Ok(real), Ok(root)) if real.starts_with(&root) => {}
        (Ok(real), Ok(_)) => {
            eprintln!("ERROR: refusing autorun: {} resolves outside the scratch directory", real.display());
            wipe(&dir);
            return 2;
        }
        (Err(e), _) | (_, Err(e)) => {
            eprintln!("ERROR: autorun {program:?}: {e}");
            wipe(&dir);
            return 2;
        }
    }
    if let Err(e) = make_executable(&exe) {
        eprintln!("ERROR: can't make {program:?} executable: {e}");
        wipe(&dir);
        return 2;
    }

    println!("Running {command}");
    // `current_dir` is the scratch directory, so a payload that refers to its
    // own files by relative name finds them.
    let status = std::process::Command::new(&exe).args(&args).current_dir(&dir).status();
    wipe(&dir);
    match status {
        Ok(s) => match s.code() {
            Some(c) => c,
            // Killed by a signal. There is no code to propagate, and reporting
            // 0 would call a crashed installer a success.
            None => {
                eprintln!("ERROR: {program:?} was killed: {s}");
                2
            }
        },
        Err(e) => {
            eprintln!("ERROR: can't run {program:?}: {e}");
            2
        }
    }
}

/// Delete the scratch tree, reporting but not failing on a leftover: the
/// command's exit code is what the caller is owed, and losing it to report a
/// temp-directory problem would be the wrong trade.
fn wipe(dir: &Path) {
    match std::fs::remove_dir_all(dir) {
        Ok(()) => {}
        Err(e) => eprintln!("WARNING: couldn't remove {}: {e}", dir.display()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_command_splits_into_a_program_and_its_arguments() {
        assert_eq!(split("setup.sh"), Some(("setup.sh".into(), vec![])));
        assert_eq!(
            split("bin/setup --quiet -f"),
            Some(("bin/setup".into(), vec!["--quiet".into(), "-f".into()]))
        );
        assert_eq!(split("   "), None);
        assert_eq!(split(""), None);
    }

    /// The escape this exists to stop. Each of these ran, in FreeArc's design,
    /// only because the command was hardcoded and so could not say them.
    #[test]
    fn a_program_outside_the_archive_is_refused() {
        assert!(!program_is_safe("../../../bin/sh"));
        assert!(!program_is_safe("/bin/sh"));
        assert!(!program_is_safe(r"\bin\sh"));
        assert!(!program_is_safe("c:/windows/system32/cmd.exe"));
        assert!(!program_is_safe("a/../../b"));
        assert!(!program_is_safe(""));
        assert!(program_is_safe("setup.sh"));
        assert!(program_is_safe("bin/setup"));
    }

    /// Two scratch directories are never the same one, and each is fresh.
    #[test]
    fn scratch_directories_do_not_collide() {
        let a = scratch_dir().expect("a");
        let b = scratch_dir().expect("b");
        assert_ne!(a, b);
        assert!(a.is_dir() && b.is_dir());
        assert_eq!(std::fs::read_dir(&a).expect("read").count(), 0);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&a).expect("stat").permissions().mode();
            assert_eq!(mode & 0o777, 0o700, "the scratch directory is the owner's alone");
        }
        wipe(&a);
        wipe(&b);
        assert!(!a.exists(), "wipe removes it");
    }

    /// `create_dir`, not `create_dir_all`: an existing name must be stepped
    /// over, never adopted. Without this, anything able to write to the temp
    /// directory could pre-create the name and own the tree the payload runs
    /// from.
    #[test]
    fn an_existing_directory_is_not_adopted() {
        let squatted = std::env::temp_dir().join(format!("darc-sfx-{}-0", std::process::id()));
        std::fs::create_dir_all(&squatted).expect("squat");
        let got = scratch_dir().expect("scratch");
        assert_ne!(got, squatted, "the pre-created name was stepped over");
        wipe(&got);
        wipe(&squatted);
    }
}
