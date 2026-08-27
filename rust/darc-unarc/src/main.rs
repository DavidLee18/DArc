//! `unarc` — the standalone extractor, and the payload of every SFX module.
//!
//! Replaces `Unarc/unarc.cpp`, `ArcStructure.h` and `CUI.h`.
//!
//! # It owns no format knowledge, on purpose
//!
//! The C++ it replaces was a second, independent implementation of the archive
//! reader. That is exactly how `ArcStructure.h` came to read the per-file time
//! field as **4 bytes** while `ByteStream` writes `CTime` as a fixed 64-bit
//! value: everything stored after it — the directory flags and the CRCs — came
//! out of the wrong offset, so directories were recreated as zero-byte *files*
//! and every extracted file failed its CRC. Sizes and names, stored *before*
//! the time field, were perfect, so a listing looked correct throughout.
//!
//! Nothing here parses a block, a descriptor or a directory. It reads argv,
//! decides which file to open, and calls `darc_arc`. When the format changes,
//! this cannot fail to notice, because there is nothing here to update.
//!
//! # SFX is detected at RUNTIME, not compiled in
//!
//! An SFX module is the *prefix* of an archive: `darc a -sfx<module>` writes
//! `[module][archive]` as one file, so `argv[0]` names something that is both a
//! program and an archive. `darc_arc::archive` already copes — the footer
//! descriptor is found by scanning back from EOF, and the stub is whatever sits
//! below the lowest block position, so there is no offset to pass.
//!
//! The C did this with `-DFREEARC_SFX`, compiling a second binary. This asks
//! the file instead: if our own executable ends in an archive, extract it;
//! otherwise behave as `unarc`. That is not a stylistic preference — a compiled
//! flag needs two build outputs, and with cargo both land on
//! `target/release/unarc` and silently overwrite each other. That actually
//! happened here: a `--features sfx` build clobbered the plain one, and the
//! next `unarc l foo.arc` reported "signature not found" while naming the
//! BINARY, because it was reading itself. One binary cannot have that bug.

mod autorun;

use darc_arc::{archive, directory::Entry, extract::Layout, passwords::Passwords};

/// What the C's `COMMAND` struct carried, minus the Windows and GUI fields.
struct Command {
    /// `l` `v` `t` `x` `e`. SFX defaults to `x`.
    cmd: char,
    archive: String,
    /// `-dp<path>` or `-d<path>`: where to extract.
    outpath: String,
    /// `-s`/`-s0`/`-s1`/`-s2`. Only silence is honoured; the C's levels above
    /// 0 differ in how much progress they draw, and this draws none.
    silent: u8,
    /// Whether a command was NAMED, as against defaulted to `x` by SFX mode.
    /// Only the defaulted case can autorun — see [`autorun`].
    explicit_cmd: bool,
    /// `-y`: answer the autorun confirmation with yes.
    assume_yes: bool,
}

fn usage(program: &str) -> ! {
    eprintln!(
        "Usage: {program} <command> [options] <archive> [files...]\n\
         \n\
         Commands:\n\
         \x20 l   list files\n\
         \x20 v   list files verbosely\n\
         \x20 t   test archive integrity\n\
         \x20 x   extract, keeping paths\n\
         \x20 e   extract, ignoring paths\n\
         \n\
         Options:\n\
         \x20 -dp<path> extract into <path>\n\
         \x20 -d<path>  the same, as the SFX modules spell it\n\
         \x20 -s[0-2]   quieter output\n\
         \x20 -y        confirm running a self-extracting archive's command\n\
         \x20 --        stop parsing options\n\
         \n\
         Reads the same archives as `darc`, using the same reader.",
    );
    std::process::exit(2)
}

/// The C's option loop, minus the Windows-only and installer-only flags.
///
/// The C had TWO of these, chosen by `#ifdef FREEARC_SFX` — an SFX module took
/// `-d<path>` (`unarc.cpp:128`) and a plain `unarc` took `-dp<path>`
/// (`unarc.cpp:181`), and neither binary knew the other's spelling. This is one
/// binary in both roles, so it takes BOTH, and `-dp` is matched first.
///
/// DELIBERATE DIVERGENCE (issue #177): the SFX role now also accepts `-dp`,
/// which the C's SFX branch read as `-d` with a path beginning `p`. That is the
/// one thing this costs, and it is reachable as `-d./pFolder`. The alternative —
/// honouring `-d` or `-dp` depending on which role we booted into — makes the
/// same executable answer to different flags depending on how it was started,
/// and breaks every script that passes `-d` to a plain `unarc`.
///
/// `-n` is accepted and ignored rather than refused: this build never prompts
/// about overwriting, so it already describes what happens. Refusing it would
/// break scripts that pass it for the C's benefit. `-y` used to be in the same
/// position and no longer is — it now confirms an autorun. `--noarcext` joins
/// them, and matches the C exactly: `unarc.cpp:178` sets the flag and NOTHING
/// ever reads it, because appending `.arc` was an unimplemented TODO at the top
/// of that file. The only change here is that it stops being a usage error.
fn parse(args: &[String], program: &str, sfx: bool) -> Command {
    let mut cmd = if sfx { 'x' } else { '\0' };
    let mut archive = if sfx { program.to_string() } else { String::new() };
    let mut outpath = String::new();
    let mut silent = 0u8;
    let mut nooptions = false;
    let mut explicit_cmd = false;
    let mut assume_yes = false;
    let mut rest: Vec<&String> = Vec::new();

    for a in args {
        if !nooptions && a.starts_with('-') && a.len() > 1 {
            match a.as_str() {
                "-l" => (cmd, explicit_cmd) = ('l', true),
                "-v" => (cmd, explicit_cmd) = ('v', true),
                "-e" => (cmd, explicit_cmd) = ('e', true),
                "-x" => (cmd, explicit_cmd) = ('x', true),
                "-t" => (cmd, explicit_cmd) = ('t', true),
                // `-y` was accepted and ignored, and still is for EXTRACTION --
                // this build never prompts about overwriting. It now also
                // answers the one question this build does ask, the autorun
                // confirmation, which is the nearest honest reading of it.
                "-y" => assume_yes = true,
                // Still accepted and ignored. `-n` declines a prompt that is
                // not shown, which is what happens anyway, and `-o+`/`-o-`
                // choose an overwrite policy where this always overwrites.
                // Refusing them would break callers that pass them for the C's
                // benefit -- `Tests/sfx-roundtrip.sh` passes `-o+`, and
                // rejecting it made every method in that harness report
                // "unarc failed".
                "-n" | "--noarcext" => {}
                s if s.starts_with("-o") => {}
                "-s" | "-s1" => silent = 1,
                "-s0" => silent = 0,
                "-s2" => silent = 2,
                "--" => nooptions = true,
                // `-dp` BEFORE `-d`: `-dpFolder` is the plain-unarc spelling and
                // means Folder, not pFolder. A match arm order, not a
                // longest-prefix search -- put these the other way round and
                // the bug this fixes comes straight back.
                s if s.starts_with("-dp") => outpath = s[3..].to_string(),
                s if s.starts_with("-d") => outpath = s[2..].to_string(),
                _ => usage(program),
            }
            continue;
        }
        rest.push(a);
    }

    // Without the SFX feature the first non-option word is the command and the
    // second is the archive, which is what `unarc x foo.arc` means.
    let mut it = rest.into_iter();
    if !sfx {
        match it.next() {
            Some(c) if c.len() == 1 => (cmd, explicit_cmd) = (c.chars().next().unwrap_or('\0'), true),
            Some(_) | None => usage(program),
        }
        match it.next() {
            Some(a) => archive = a.clone(),
            None => usage(program),
        }
    }
    if archive.is_empty() || !matches!(cmd, 'l' | 'v' | 't' | 'x' | 'e') {
        usage(program);
    }
    Command { cmd, archive, outpath, silent, explicit_cmd, assume_yes }
}

/// Are we an SFX module — that is, does our own executable end in an archive?
///
/// `current_exe` rather than `argv[0]`: `argv[0]` is whatever the caller chose
/// to say, and a shell can set it to anything. What matters is the file the
/// kernel actually loaded.
///
/// A plain `unarc` has no footer descriptor in its last bytes, so this is false
/// and the CLI takes over. Deliberately quiet on every error — an unreadable
/// `/proc/self/exe`, a stripped binary, a permission problem — because "this is
/// not an SFX" is the right answer to all of them, and a diagnostic here would
/// fire on every ordinary `unarc x foo.arc`.
fn appended_archive() -> Option<String> {
    let exe = std::env::current_exe().ok()?;
    let pw = Passwords::default();
    match archive::read_info(&exe, &pw) {
        Ok(_) => Some(exe.to_string_lossy().into_owned()),
        Err(_) => None,
    }
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let program = argv.first().cloned().unwrap_or_else(|| "unarc".to_string());
    let sfx = appended_archive();
    let c = parse(&argv[1..], sfx.as_deref().unwrap_or(&program), sfx.is_some());

    // No password support yet: the C's SFX modules prompt, and this build has
    // no console prompt. An encrypted archive therefore fails at the block that
    // needs a key, with the same diagnosis `darc` gives, rather than silently
    // extracting nothing.
    let pw = Passwords::default();
    let path = std::path::Path::new(&c.archive);

    let info = match archive::read_info(path, &pw) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("ERROR: {}: {e}", c.archive);
            std::process::exit(2);
        }
    };

    // The installer path. Three conditions, all required:
    //
    //   * we are our own SFX stub -- `unarc x installer.sfx` never autoruns,
    //     only the stub run as itself does;
    //   * the archive records a command;
    //   * no command was NAMED. The C fired on `argv[1] == NULL`, the bare
    //     double-click, and any argument at all disabled it. This asks the
    //     narrower question so that `-y` and `-s` remain usable -- an
    //     unattended install must not have to give up the flag that makes it
    //     unattended -- while `x`, `e`, `l` and `t` still mean exactly what
    //     they say and run nothing.
    if sfx.is_some() && !info.footer.autorun.is_empty() && !c.explicit_cmd {
        let data = match archive::open(path) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("ERROR: {}: {e}", c.archive);
                std::process::exit(2);
            }
        };
        let entries: Vec<Entry> = info.entries.clone();
        std::process::exit(autorun::run(
            &info.footer.autorun,
            &info,
            &data,
            &pw,
            &entries,
            c.assume_yes,
        ));
    }

    let code = match c.cmd {
        // The same `list` the archiver runs, so the columns, the totals and the
        // local-time rendering cannot drift from `darc l`.
        'l' | 'v' => {
            let all: Vec<Entry> = info.entries.clone();
            darc_arc::extract::list(&c.cmd.to_string(), &info, &all)
        }
        cmd => {
            let data = match archive::open(path) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("ERROR: {}: {e}", c.archive);
                    std::process::exit(2);
                }
            };
            let entries: Vec<Entry> = info.entries.clone();
            let skip = std::collections::HashSet::new();
            let layout = match cmd {
                'e' => Layout { disk_basedir: c.outpath.clone(), ..Layout::flat() },
                _ => Layout { disk_basedir: c.outpath.clone(), ..Layout::default() },
            };
            if c.silent == 0 {
                let what = if cmd == 't' { "Testing" } else { "Extracting" };
                println!("{what} {}", c.archive);
            }
            darc_arc::extract::run_blocks(
                &info, &data, &layout, cmd != 't', &pw, &entries, &skip, false,
            )
        }
    };
    std::process::exit(code);
}

#[cfg(test)]
mod tests {
    use super::parse;

    fn args(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| (*s).to_string()).collect()
    }

    /// Issue #177. The C's plain-`unarc` branch spells the destination `-dp`,
    /// and reading it as `-d` put the `p` at the front of the path: `-dpFolder`
    /// extracted into `pFolder`.
    #[test]
    fn dp_is_the_destination_path_not_a_path_starting_with_p() {
        let c = parse(&args(&["x", "-dpFolder", "t.arc"]), "unarc", false);
        assert_eq!(c.outpath, "Folder");
        assert_eq!(c.cmd, 'x');
        assert_eq!(c.archive, "t.arc");
    }

    /// `-d` is the SFX branch's spelling and keeps working in both roles --
    /// dropping it would break every caller written against this build.
    #[test]
    fn plain_d_still_sets_the_destination() {
        let c = parse(&args(&["x", "-d/tmp/out", "t.arc"]), "unarc", false);
        assert_eq!(c.outpath, "/tmp/out");
    }

    /// The cost of taking both spellings in one binary, stated as a test so it
    /// cannot be changed by accident: a destination that really does begin with
    /// `p` is written `-d./pFolder`, because `-dpFolder` is now `-dp`.
    #[test]
    fn a_destination_beginning_with_p_needs_a_qualified_path() {
        assert_eq!(parse(&args(&["x", "-d./pFolder", "t.arc"]), "unarc", false).outpath, "./pFolder");
    }

    /// Both spellings reach the SFX role too, where no command word is given
    /// and the archive is the executable itself.
    #[test]
    fn sfx_takes_both_spellings() {
        let c = parse(&args(&["-dpFolder"]), "self.exe", true);
        assert_eq!((c.outpath.as_str(), c.cmd, c.explicit_cmd), ("Folder", 'x', false));
        let c = parse(&args(&["-dFolder"]), "self.exe", true);
        assert_eq!((c.outpath.as_str(), c.cmd, c.explicit_cmd), ("Folder", 'x', false));
    }

    /// Accepted and ignored, like `-n` and `-o` -- and like the C, where the
    /// flag is set at `unarc.cpp:178` and never read. Before #177 it was
    /// refused, which printed usage and exited 2.
    #[test]
    fn noarcext_is_accepted_and_ignored() {
        let c = parse(&args(&["l", "--noarcext", "t.arc"]), "unarc", false);
        assert_eq!((c.cmd, c.archive.as_str()), ('l', "t.arc"));
    }

    /// The last one wins, as in the C -- each match arm assigns rather than
    /// accumulating.
    #[test]
    fn the_last_destination_wins() {
        let c = parse(&args(&["x", "-dfirst", "-dpsecond", "t.arc"]), "unarc", false);
        assert_eq!(c.outpath, "second");
    }
}
