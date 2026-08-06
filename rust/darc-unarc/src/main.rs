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
//! # SFX
//!
//! Under `--features sfx` the binary is the *prefix* of an archive: the module
//! is prepended to the archive bytes, so the file on disk is `[stub][archive]`
//! and `argv[0]` names it. `darc_arc::archive` already handles that — the
//! footer descriptor is found by scanning back from EOF, and the stub is
//! whatever sits below the lowest block position. There is no offset to pass.

use darc_arc::{archive, directory::Entry, extract::Layout, passwords::Passwords};

/// What the C's `COMMAND` struct carried, minus the Windows and GUI fields.
struct Command {
    /// `l` `v` `t` `x` `e`. SFX defaults to `x`.
    cmd: char,
    archive: String,
    /// `-d<path>`: where to extract.
    outpath: String,
    /// `-s`/`-s0`/`-s1`/`-s2`. Only silence is honoured; the C's levels above
    /// 0 differ in how much progress they draw, and this draws none.
    silent: u8,
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
         \x20 -d<path>  extract into <path>\n\
         \x20 -s[0-2]   quieter output\n\
         \x20 --        stop parsing options\n\
         \n\
         Reads the same archives as `darc`, using the same reader.",
    );
    std::process::exit(2)
}

/// The C's option loop (`unarc.cpp:117-137`), minus the Windows-only and
/// installer-only flags.
///
/// `-y`/`-n` are accepted and ignored rather than refused: this build never
/// prompts, so both already describe what it does. Refusing them would break
/// scripts that pass `-y` for the C's benefit.
fn parse(args: &[String], program: &str) -> Command {
    let sfx = cfg!(feature = "sfx");
    let mut cmd = if sfx { 'x' } else { '\0' };
    let mut archive = if sfx { program.to_string() } else { String::new() };
    let mut outpath = String::new();
    let mut silent = 0u8;
    let mut nooptions = false;
    let mut rest: Vec<&String> = Vec::new();

    for a in args {
        if !nooptions && a.starts_with('-') && a.len() > 1 {
            match a.as_str() {
                "-l" => cmd = 'l',
                "-v" => cmd = 'v',
                "-e" => cmd = 'e',
                "-x" => cmd = 'x',
                "-t" => cmd = 't',
                "-y" | "-n" => {}
                "-s" | "-s1" => silent = 1,
                "-s0" => silent = 0,
                "-s2" => silent = 2,
                "--" => nooptions = true,
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
            Some(c) if c.len() == 1 => cmd = c.chars().next().unwrap_or('\0'),
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
    Command { cmd, archive, outpath, silent }
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let program = argv.first().cloned().unwrap_or_else(|| "unarc".to_string());
    let c = parse(&argv[1..], &program);

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

    let code = match c.cmd {
        // Listing still lives in the `darc` binary; lifting it needs the local
        // time formatter, which is FFI and platform-split, so it is a separate
        // step rather than a rider on this one.
        'l' | 'v' => {
            eprintln!("ERROR: listing is not wired up yet; use `darc l`");
            2
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
