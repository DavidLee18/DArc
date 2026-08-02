//! `darc` — the Rust archiver, read commands.
//!
//!     darc <command> [options] <archive> [files...]
//!
//! Implements `l`, `t`, `x` and `e` with DArc's real option parsing, so the
//! command line is the same one `arc` takes: prefix-matched options, `-ap`/`-dp`
//! base directories, `-ep`, `--`, and the rest.
//!
//! The write commands are not here yet. An unimplemented command says so and
//! exits 2 rather than pretending — a silent no-op on `arc a` would look like
//! a successful archive.

use darc_arc::{archive, crc, decompress, directory::Entry, extract::Layout, options};
use rayon::prelude::*;
use std::io::Write;

fn show3(n: u64) -> String {
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
fn ratio3(count: u64, total: u64) -> String {
    if total == 0 {
        return "0.0".to_string();
    }
    let scaled = (count.saturating_mul(1000) / total).to_string();
    match scaled.len() {
        1 => format!("0.{scaled}"),
        n => format!("{}.{}", &scaled[..n - 1], &scaled[n - 1..]),
    }
}

fn main() {
    let argv: Vec<String> = std::env::args().skip(1).collect();
    if argv.is_empty() {
        eprintln!("usage: darc <command> [options] <archive> [files...]");
        eprintln!("commands: l (list), t (test), x (extract), e (extract flat)");
        std::process::exit(2);
    }
    // The command is the first argument and is NOT an option, matching
    // `parseCmdline`: options may appear anywhere after it.
    let command = argv[0].clone();
    let parsed = match options::parse(&argv[1..]) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(2);
        }
    };
    // `addArcExtension` (Cmdline.hs:770): an archive name with NO extension
    // gets `.arc` appended, unless `--noarcext`. It applies to EVERY command,
    // so `arc l x` opens `x.arc`.
    //
    // This interacts with `-sfx`, which renames `x.arc` to `x` on Unix: the
    // resulting SFX archive cannot then be named without `--noarcext`, because
    // `arc l x` would look for `x.arc` again. That is the reference's
    // behaviour, not an artefact here.
    let add_arc_ext = |name: &str| -> String {
        let has_ext = std::path::Path::new(name).extension().is_some();
        match has_ext || parsed.flag("noarcext") {
            true => name.to_string(),
            false => format!("{name}.arc"),
        }
    };
    let archive_name = match parsed.free.first() {
        Some(a) => add_arc_ext(a),
        None if command == "canonize" || command == "fit" || command == "types" => {
            String::new()
        }
        None => {
            eprintln!("ERROR: no archive name given");
            std::process::exit(2);
        }
    };

    // `dir_exclude_path` (Cmdline.hs:137): 0 for the `e` command, otherwise the
    // -ep value, defaulting to 9.
    let ep: u32 = if command == "e" {
        0
    } else {
        match parsed.arg("ExcludePath", "9") {
            "" => 0,
            s => s.parse().unwrap_or(9),
        }
    };
    let layout = Layout {
        arc_basedir: parsed.arg("arcpath", "").to_string(),
        disk_basedir: parsed.arg("diskpath", "").to_string(),
        ep,
    };

    // An option this port does not implement must be REFUSED, not ignored.
    //
    // This exists because of a measured failure: `arc a -x*.dat` archived the
    // .dat files. The option parsed, was recorded, and was never read -- so the
    // port wrote an archive containing exactly what the user had asked to leave
    // out, and said "All OK". Every option below is one the code actually
    // consults; anything else stops the command.
    //
    // `-y`, `-i…` and `--display` are accepted and ignored deliberately: they
    // steer prompting and progress output, not what is written.
    const HONOURED: &[&str] = &[
        "append", "arccmt", "archive-comment", "arcpath", "delete", "delfiles",
        "diskpath", "encryption", "ExcludePath", "freshen", "fullnames", "groups",
        "HeadersPassword", "keyfile", "lock", "method", "nodates", "OldKeyfile",
        "OldPassword", "password", "recompress", "recursive", "solid", "sync",
        "update", "include", "exclude", "dirs", "nodirs",
        "SizeMore", "SizeLess", "TimeBefore", "TimeAfter", "TimeNewer", "TimeOlder",
        "recovery", "volume", "sfx", "noarcext",
        // Accepted and deliberately ignored: UI only.
        "yes", "indicator", "display",
    ];
    let unimplemented: Vec<&str> = parsed
        .options
        .iter()
        .map(|(n, _)| n.as_str())
        .filter(|n| !HONOURED.contains(n))
        .collect();
    if !unimplemented.is_empty() {
        let mut names: Vec<&str> = unimplemented;
        names.sort_unstable();
        names.dedup();
        eprintln!(
            "ERROR: this port does not implement {} yet, and ignoring it would \
             write an archive that is not what you asked for",
            names.join(", ")
        );
        std::process::exit(2);
    }

    // `opt_file_filter` -- one predicate, shared by the disk scan, the archive
    // selection and the read commands. See `darc_arc::filter`.
    // `-sm`/`-sl` take a size, `-ta`/`-tb` an absolute YYYYMMDDHHMMSS in LOCAL
    // time, `-tn`/`-to` a period back from now. The last two are resolved here
    // so the filter itself never reads the clock.
    let size_of = |flag: &str| -> Option<u64> {
        match parsed.arg(flag, "--") {
            "--" => None,
            s => match darc_arc::filter::parse_size(s) {
                Some(n) => Some(n),
                None => {
                    eprintln!("ERROR: -{flag}: {s:?} is not a size");
                    std::process::exit(2);
                }
            },
        }
    };
    let time_of = |flag: &str| -> Option<i64> {
        match parsed.arg(flag, "--") {
            "--" => None,
            s => match parse_local_time(s) {
                Some(t) => Some(t),
                None => {
                    eprintln!("ERROR: -{flag}: {s:?} is not a YYYYMMDDHHMMSS time");
                    std::process::exit(2);
                }
            },
        }
    };
    let ago_of = |flag: &str| -> Option<i64> {
        match parsed.arg(flag, "--") {
            "--" => None,
            s => match darc_arc::filter::parse_period(s) {
                Some(secs) => Some(now_seconds() - secs),
                None => {
                    eprintln!("ERROR: -{flag}: {s:?} is not a time period");
                    std::process::exit(2);
                }
            },
        }
    };
    let file_filter = darc_arc::filter::FileFilter {
        include: parsed.all("include").iter().map(|s| s.to_string()).collect(),
        exclude: parsed.all("exclude").iter().map(|s| s.to_string()).collect(),
        full_names: parsed.flag("fullnames"),
        size_more: size_of("SizeMore"),
        size_less: size_of("SizeLess"),
        // -ta and -tn are both `>=` and both apply, so they collect rather than
        // overwrite: giving one must not silently drop the other.
        time_at_or_after: [time_of("TimeAfter"), ago_of("TimeNewer")]
            .into_iter()
            .flatten()
            .collect(),
        time_before: [time_of("TimeBefore"), ago_of("TimeOlder")]
            .into_iter()
            .flatten()
            .collect(),
    };
    // `findNoArgs o "dirs" "nodirs"` -- Nothing when neither is given.
    let dirs_option = match (parsed.flag("dirs"), parsed.flag("nodirs")) {
        (true, false) => Some(true),
        (false, true) => Some(false),
        // Both or neither: the Haskell's findNoArgs returns the LAST of the two
        // it saw, and neither means "decide from the context".
        _ => None,
    };

    // The passwords, cooked once: the prompt must not appear twice, and both
    // the reader and the writer need the same answer.
    let pw = cook_passwords(&parsed, &command);

    let path = std::path::Path::new(&archive_name);
    // Only the read commands need an existing archive; `a` creates one.
    let open_existing = || match archive::read_info(path, &pw) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(2);
        }
    };

    // `setArcFilter (test_dirs fullFileFilter)` for the read commands: a file
    // must match the filespecs AND pass the filter; a DIRECTORY is decided by
    // `x_include_dirs` alone and never consults either.
    //
    // The filespecs default to `["*"]` (aDEFAULT_FILESPECS), and whether they
    // were defaulted is itself an input to `x_include_dirs` -- `arc l x.arc`
    // lists directories and `arc l x.arc '*.txt'` does not.
    let read_specs: Vec<String> = parsed.free.iter().skip(1).cloned().collect();
    let default_specs = read_specs.is_empty();
    let read_specs = if default_specs { vec!["*".to_string()] } else { read_specs };
    let show_dirs =
        darc_arc::filter::include_dirs(dirs_option, default_specs, &file_filter, &command);
    let selects = |e: &Entry| -> bool {
        match e.is_dir {
            true => show_dirs,
            false => {
                darc_arc::sort::match_filespecs(
                    &read_specs,
                    &e.stored_name,
                    file_filter.full_names,
                ) && file_filter.accepts(&e.stored_name, e.size, e.time)
            }
        }
    };

    let code = match command.as_str() {
        "l" | "v" | "lb" | "lt" => {
            let info = open_existing();
            let selected: Vec<Entry> =
                info.entries.iter().filter(|e| selects(e)).cloned().collect();
            list(&command, &info, &selected)
        }
        "t" | "x" | "e" => {
            let info = open_existing();
            let selected: Vec<Entry> =
                info.entries.iter().filter(|e| selects(e)).cloned().collect();
            let data = match archive::open(path) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    std::process::exit(2);
                }
            };
            let extracting = command != "t";
            run_blocks(&info, &data, &layout, extracting, &pw, &selected)
        }
        // One function: every one of these is `runArchiveAdd` with a different
        // archive filter and a different source of files (Arc.hs:122-131).
        "a" | "u" | "f" | "d" | "ch" | "c" | "k" | "j" | "m" | "mf" => {
            add(&command, &archive_name, &parsed, &pw, &file_filter, dirs_option)
        }
        // Not an `arc` command: a probe, so the canonicaliser can be checked
        // against the method strings real archives contain. Prints the
        // canonical form of each argument, or "?" if it does not parse.
        // Another probe: `fit <bytes> <chain>...` prints each chain after the
        // data-size limiting ArcvProcessRead.hs:122 applies.
        "fit" => {
            let mut it = parsed.free.iter();
            let total: u64 = match it.next().and_then(|s| s.parse().ok()) {
                Some(n) => n,
                None => {
                    eprintln!("usage: darc fit <total-bytes> <chain>...");
                    std::process::exit(2);
                }
            };
            let mut bad = 0;
            for chain in it {
                match darc_arc::memlimit::fit_to_data(chain, total) {
                    Some(c) => println!("{c}"),
                    None => {
                        println!("?");
                        bad += 1;
                    }
                }
            }
            if bad > 0 { 2 } else { 0 }
        }
        // A probe: classify the files under a directory and print each
        // resulting group's total size and type index, so the split can be
        // compared with `arc lt` on a reference archive.
        "types" => {
            let dir = parsed.free.first().cloned().unwrap_or_else(|| ".".to_string());
            let mut found = Vec::new();
            match scan(std::path::Path::new(&dir), dir.trim_end_matches('/'), true, &mut found) {
                Ok(()) => {}
                Err(e) => {
                    eprintln!("ERROR: {dir}: {e}");
                    std::process::exit(2);
                }
            }
            let mut entries: Vec<Entry> = Vec::new();
            let mut bodies: Vec<Vec<u8>> = Vec::new();
            for (stored, disk, is_dir) in &found {
                if *is_dir {
                    continue;
                }
                let body = std::fs::read(disk).unwrap_or_default();
                entries.push(Entry {
                    stored_name: stored.clone(),
                    size: body.len() as u64,
                    time: 0,
                    is_dir: false,
                    crc: 0,
                    block: 0,
                    pos_in_block: 0,
                });
                bodies.push(body);
            }
            let groups = load_groups(&parsed);
            entries = darc_arc::sort::sort_files("gerpn", &groups, &entries);
            // Re-associate the bodies after sorting.
            let by_name: std::collections::HashMap<&str, &Vec<u8>> = found
                .iter()
                .filter(|(_, _, d)| !*d)
                .map(|(n, _, _)| n.as_str())
                .zip(bodies.iter())
                .collect();
            let cands: Vec<darc_arc::filetype::Candidate<'_>> = entries
                .iter()
                .map(|e| darc_arc::filetype::Candidate {
                    stored_name: &e.stored_name,
                    size: e.size,
                    data: by_name.get(e.stored_name.as_str()).map(|v| v.as_slice()).unwrap_or(&[]),
                    default_type: "$binary",
                })
                .collect();
            let names: Vec<String> =
                ["", "$obj", "$text"].iter().map(|s| s.to_string()).collect();
            let split = darc_arc::filetype::split_file_types(&cands, &names);
            // The -m4 chains, so the blocks come out in the order the reference
            // writes them (sorted by chain, not by type index).
            let chains = [
                "rep+exe+delta+4x4:lzma".to_string(),
                String::new(),
                "dict+lzp+ppmd".to_string(),
            ];
            let merged =
                darc_arc::filetype::merge_by_type(&split, |t| chains[t].clone());
            for (ty, files) in &merged {
                let size: u64 = files.iter().map(|&i| cands[i].size).sum();
                println!("type {ty} ({}) {size} bytes", names[*ty]);
            }
            0
        }
        // A probe, not an `arc` command: encrypt a fixed plaintext with a fixed
        // key and IV under each of the two hex decodings, so a harness can
        // prove they DIFFER. Both formats round-trip, so every cross-decryption
        // row passes whether or not `:h1` is honoured; this is what catches the
        // parameter being parsed and then ignored.
        //
        // The key and IV are constants, not secrets -- nothing is protected by
        // them and no archive uses them.
        "crypt-probe" => {
            let which = parsed.free.first().map(String::as_str).unwrap_or("h1");
            let h = match which {
                "h1" => ":h1",
                "h0" => "",
                other => {
                    eprintln!("usage: darc crypt-probe h0|h1 (got {other:?})");
                    std::process::exit(2);
                }
            };
            let method = format!(
                "aes-256/ctr:n1000:r0{h}\
                 :kf012ca272b5efb2bbe496b21da1ee037004ff64d3a2ee911c842316cf886e145\
                 :i6090b4cacecf5fb120ba94b9125db455"
            );
            let mut parts = method.split(':');
            let name = parts.next().unwrap_or("");
            let params: Vec<&str> = parts.collect();
            let e = match darc_arc::encryption::Encryption::parse(name, &params) {
                Some(e) => e,
                None => {
                    eprintln!("ERROR: the probe's own method string does not parse");
                    std::process::exit(2);
                }
            };
            let mut buf = vec![b'A'; 64];
            match e.apply(&mut buf, true) {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("ERROR: {err}");
                    std::process::exit(2);
                }
            }
            println!("{}", darc_arc::encryption::encode16(&buf));
            0
        }
        "canonize" => {
            let mut bad = 0;
            for m in &parsed.free {
                match darc_arc::canonize::canonize_chain(m) {
                    Some(c) => println!("{c}"),
                    None => {
                        println!("?");
                        bad += 1;
                    }
                }
            }
            if bad > 0 { 2 } else { 0 }
        }
        // `runArchiveRecovery` (ArcRecover.hs:301) -- repair an archive using
        // its own recovery records, writing `fixed.<name>` beside it.
        "r" => recover(path, &archive_name),
        // `rr…` is `ch -rr…` and `s…` is `ch -sfx…` (Cmdline.hs:124, :166) --
        // the same copy path, with the setting read off the command's own
        // suffix.
        //
        // The `"r"` arm above must come FIRST: `r` is the recovery command and
        // is not a prefix of anything, but matching `starts_with("rr")` before
        // it would be harmless while matching a bare `starts_with('r')` would
        // swallow it.
        c if c.starts_with("rr") || c.starts_with('s') => {
            add(&command, &archive_name, &parsed, &pw, &file_filter, dirs_option)
        }
        other => {
            eprintln!("ERROR: unknown command {other:?}");
            2
        }
    };
    std::process::exit(code);
}

/// `arc a` -- create an archive.
///
/// Only `-m0` (storing) so far. Every other method needs the method-string
/// canonicalisation the C does in SetCompressionMem / LimitCompressionMem,
/// which scales a chain like `dict:p:64m:85%` down to the `dict:56kb:85%:...`
/// an archive actually contains. Getting that wrong writes an archive that is
/// valid, decodes correctly, and is not the bytes the reference would have
/// written -- the failure mode this repo cares most about. Refusing is the
/// honest behaviour until it is ported.
/// `changeSfxExt` (`ArcCreate.hs:336`) — the archive is RENAMED when an SFX
/// module is added or removed.
///
/// On Unix `aDEFAULT_SFX_EXTENSION` is the empty string, so `-sfxMODULE` turns
/// `x.arc` into `x`, and `-sfx-` turns an extensionless `x` back into `x.arc`.
/// A name whose extension is neither is left alone.
///
/// Applied only when the archive does not already exist (`ArcCreate.hs:68`):
/// updating an existing archive never renames it.
fn change_sfx_ext(sfx: &str, arcname: &str) -> String {
    const SFX_EXT: &str = ""; // ".exe" on Windows
    const ARC_EXT: &str = ".arc";
    let ext = match std::path::Path::new(arcname).extension() {
        Some(e) => format!(".{}", e.to_string_lossy()),
        None => String::new(),
    };
    let replace = |new: &str| -> String {
        let stem = match arcname.rfind('.') {
            // `replaceExtension` only looks after the last separator; a dot in
            // a directory name is not an extension.
            Some(i) if !arcname[i..].contains('/') => &arcname[..i],
            _ => arcname,
        };
        format!("{stem}{new}")
    };
    match sfx {
        "--" => arcname.to_string(),
        "-" => match ext == SFX_EXT {
            true => replace(ARC_EXT),
            false => arcname.to_string(),
        },
        _ => match ext == ARC_EXT {
            true => replace(SFX_EXT),
            false => arcname.to_string(),
        },
    }
}

fn add(
    command: &str,
    archive_name: &str,
    parsed: &options::Parsed,
    pw: &darc_arc::passwords::Passwords,
    file_filter: &darc_arc::filter::FileFilter,
    dirs_option: Option<bool>,
) -> i32 {
    // opt_update_type (Cmdline.hs). The COMMAND wins over the options, and the
    // options are tried in the order `freshen`, `update`, `sync` -- so `-u
    // --sync` is an update, not a sync, and `f --sync` is a freshen.
    let update_type = match command {
        "u" => darc_arc::joinlist::UpdateType::Update,
        "f" => darc_arc::joinlist::UpdateType::Freshen,
        _ => match (parsed.flag("freshen"), parsed.flag("update"), parsed.flag("sync")) {
            (true, _, _) => darc_arc::joinlist::UpdateType::Freshen,
            (false, true, _) => darc_arc::joinlist::UpdateType::Update,
            // `--sync` brings the archive in line with the disk: a file the
            // filespecs did not reach is DELETED from it, which no other mode
            // does. An empty result removes the archive, as `d` does.
            (false, false, true) => darc_arc::joinlist::UpdateType::Sync,
            (false, false, false) => darc_arc::joinlist::UpdateType::Add,
        },
    };
    // The subset of builtinMethodSubsts (Compression.hs:428) this port can
    // write. Each maps a -m level to its unfitted chain; the data-size fitting
    // happens once the block's contents are known.
    // decode_method expands a -m level into one chain per file type.
    // `mainMethod ||| aDEFAULT_COMPRESSOR` (Cmdline.hs:334), and
    // `aDEFAULT_COMPRESSOR = "4"` (Options.hs:370). `|||` treats an EMPTY value
    // as absent, so a bare `-m` also means -m4.
    //
    // This was missing, and nothing caught it: every harness row passes `-m`
    // explicitly, so the port had never once been asked to pick a default. It
    // surfaced as "-m expanded to nothing" the moment a copy command was run
    // the way a user would run it.
    let method = match parsed.arg("method", "") {
        "" => "4",
        m => m,
    };
    let decoded = darc_arc::methodtable::decode_method(method);
    if decoded.is_empty() {
        eprintln!("ERROR: -m{method} expanded to nothing");
        return 2;
    }
    // NOT validated here. A level defines chains for types no file can be given
    // -- $wav and $bmp need an arc.groups entry, and getDefaultType maps every
    // autodetectable type to $binary -- so refusing a level because its dead
    // $wav arm uses `tta` would reject -m1 and -m4 outright. The chains that
    // are actually reached are checked below, once the split is known.
    let type_names: Vec<String> = decoded.iter().map(|(t, _)| t.clone()).collect();
    let chains: Vec<String> = decoded.iter().map(|(_, c)| c.join("+")).collect();
    // The main chain, used for the single-block paths below.
    let chain: &str = &chains[0];
    // `parseSolidOption` (Cmdline.hs:757).
    //
    // `-s` has NO command form. The `s…` command is `ch -sfx…`, not `ch -s…`
    // (Cmdline.hs:166) -- its suffix names an SFX module. An earlier version
    // here derived the solid grouping from it, which was wrong; it was dead
    // code because `s…` is refused, but it said something untrue.
    let solid = match darc_arc::grouping::parse_solid(parsed.arg("solid", "")) {
        Some(s) => s,
        None => {
            eprintln!(
                "ERROR: invalid value {:?} for option --solid (-s)",
                parsed.arg("solid", "")
            );
            return 2;
        }
    };
    // sort_order (Cmdline.hs:617): "" when the main compressor is
    // aNO_COMPRESSION, and ALSO "" when group_data is [GroupNone] -- there is
    // nothing to gain from ordering files that do not share a block. -m0 is the
    // one level where the first clause applies, which is why it packs in scan
    // order.
    let per_file = solid.data == vec![darc_arc::grouping::Grouping::None];
    let sort_order = if chain == "storing" || per_file { "" } else { "gerpn" };
    let recursive = parsed.flag("recursive");
    let nodates = parsed.flag("nodates");
    // `runDelete = runArchiveAdd . setArcFilter ((not.) . fullFileFilter)`
    // (Arc.hs:213): `d` is `a` with NO disk files and an archive filter that
    // keeps everything the filespecs do NOT match.
    let deleting = command == "d";

    // `delete_files` (Cmdline.hs:610). `m`/`-d` remove the archived files AND
    // the directories that held them; `mf`/`-df` remove only the files. Giving
    // both is an error rather than a union.
    //
    // This is the only thing `m` adds to `a`, and it touches the DISK, not the
    // archive -- an `m` archive is byte-identical to the `a` archive, so a
    // byte-comparison harness cannot see whether the deletion happened at all.
    let del_dirs = parsed.flag("delete") || command == "m";
    let del_files = parsed.flag("delfiles") || command == "mf";
    if del_dirs && del_files {
        eprintln!("ERROR: incompatible options: m/-d and mf/-df");
        return 2;
    }
    // `runCopy = runArchiveAdd . setArcFilter fullFileFilter` (Arc.hs:211) --
    // the same shape as `d` with the filter NOT negated: no disk files, and the
    // archive's own files kept where the filespecs DO match. `ch` is the
    // general form; `c` is `ch -z`, `k` is `ch` plus the lock, and `s…`/`rr…`
    // differ only in options this port does not implement yet.
    //
    // The kept files are RECOMPRESSED, not copied: splitToSolidBlocks preserves
    // existing solid blocks only under --keep-original (ArhiveFileList.hs:297).
    // That is what makes `ch -m1` on a -m9 archive do anything at all.
    // `is_COPYING_COMMAND` (Options.hs:300), ALL of it -- `rr…` and `s…` are
    // prefixes, not literals. Spelling this as `matches!(command, "ch"|"c"|"k")`
    // left `rr1%` outside it, so the command scanned the DISK with the default
    // "." filespec instead of copying the archive: on this repo that was a walk
    // of the whole tree, which showed up as a hang rather than a wrong archive.
    let copying = matches!(command, "c" | "ch" | "k" | "d" | "j")
        || command.starts_with("rr")
        || command.starts_with('s');
    // The copy commands that keep files by filespec rather than dropping them:
    // `d` negates the filter and `j` does not filter at all, so neither belongs
    // here even though both are copying commands.
    let keeps_matching = copying && !deleting && command != "j";
    // Nothing here reads the disk. Everything below that asks "is there a disk
    // side" asks this.
    let archive_only = copying && command != "j";

    // Everything after the archive name is a filespec.
    let specs: Vec<String> = parsed.free.iter().skip(1).cloned().collect();

    // `is_CMD_WITHOUT_ARGS` (Options.hs:305): a copying command takes no
    // filespecs -- except `d` and `j`, whose arguments are archive members and
    // archive names respectively. `arc ch x.arc a.txt` is
    // "command \"ch\" shouldn't have additional arguments", not a request to
    // keep only a.txt.
    //
    // Accepting it silently is what a harness row of mine did: `ch … a.txt
    // archive` made a.txt the ARCHIVE NAME, both binaries failed identically,
    // and the row passed while testing nothing.
    let takes_no_args = copying && !matches!(command, "d" | "j");
    if takes_no_args && !specs.is_empty() {
        eprintln!("ERROR: command {command:?} shouldn't have additional arguments");
        return 2;
    }
    // `aDEFAULT_FILESPECS` is `["*"]` for the archive-only commands: `ch` with
    // no filespec re-packs everything. For `a` it is `.`, the disk tree.
    let specs = match (specs.is_empty(), archive_only) {
        (false, _) => specs,
        (true, true) => vec!["*".to_string()],
        (true, false) => vec![".".to_string()],
    };

    // Names are stored WITH the filespec as the user wrote it: `arc a x.arc .`
    // stores "./a.txt" and the directory name ".", not "a.txt" and "".
    // remove_unsafe_dirs (Files.hs:143) strips the "." again on READ, so both
    // list identically -- but the stored bytes differ, and this is a
    // format-compatibility port. Measured: the reference's directory block is
    // exactly 3 bytes longer than one built without the prefix, which is
    // ".\0" plus "./" on the one subdirectory name.
    let mut found: Vec<(String, std::path::PathBuf, bool)> = Vec::new();
    // `j`'s filespecs are ARCHIVE NAMES, not files to add: runJoin passes them
    // as `cmd_added_arcnames` and gives runArchiveAdd no disk filespecs at all
    // (Arc.hs:200). Scanning them as a tree makes the port try to walk an
    // archive as a directory.
    let reads_disk = !(archive_only || command == "j");
    for spec in if reads_disk { &specs[..] } else { &[][..] } {
        let root = spec.trim_end_matches('/');
        // A filespec that NAMES A DIRECTORY is scanned recursively even without
        // -r. find_filter_and_process_files (FileInfo.hs:403) rewrites it as the
        // two wildcards "dir" and "dir/", and the trailing separator sets
        // `dir_slash`, which is OR-ed into `recursive` (FileInfo.hs:461).
        //
        // Without this, `arc u -y archive .` silently misses every
        // subdirectory -- 32 bytes on the update harness's tree, and an
        // archive that lists as if the files had never existed.
        let names_a_dir = spec.ends_with('/')
            || std::path::Path::new(spec).is_dir();
        match scan(std::path::Path::new(spec), root, recursive || names_a_dir, &mut found) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("ERROR: {spec}: {e}");
                return 2;
            }
        }
    }

    // Read every file first, then sort, then lay the block out: the sort keys
    // include the size, so it cannot run before the scan is complete.
    let mut dir_entries: Vec<Entry> = Vec::new();
    let mut file_entries: Vec<Entry> = Vec::new();
    let mut contents: std::collections::HashMap<String, Vec<u8>> =
        std::collections::HashMap::new();
    // Size and mtime AS SCANNED, for `checkThatFileWasNotChanged` under -d/-df.
    // Re-statting at deletion time would compare the file against itself and
    // always agree, which is the same as not checking.
    let mut scanned: std::collections::HashMap<std::path::PathBuf, (u64, i64)> =
        std::collections::HashMap::new();
    // `accept_f` (FileInfo.hs:462). The scan applies the filter itself, so an
    // excluded file is never read, never CRC'd and never considered for
    // deletion by -d.
    //
    // A DIRECTORY's name is not matched against -n or -x on either side: it is
    // decided by --dirs/--nodirs, or failing that by whether any n/s/t filter
    // exists at all.
    let keep_dirs = darc_arc::filter::write_dirs(dirs_option, file_filter);
    found.retain(|(stored, disk, is_dir)| match is_dir {
        true => keep_dirs,
        // The size and time filters need the file's own numbers, so this stats
        // here rather than filtering on the name alone. Removing the entry from
        // `found` -- not merely skipping it later -- is what keeps `-d` from
        // deleting a file the filters excluded from the archive.
        //
        // The filter sees the REAL mtime. `--nodates` zeroes what is STORED,
        // not what `fiTime` holds, so `-tn1d --nodates` still selects by age.
        false => match std::fs::symlink_metadata(disk) {
            Ok(md) => file_filter.accepts(stored, md.len(), mtime(&md)),
            // Unreadable: keep it, so the loop below reports the error rather
            // than silently dropping the file.
            Err(_) => true,
        },
    });

    for (stored, disk, is_dir) in &found {
        let meta = match std::fs::symlink_metadata(disk) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("ERROR: {}: {e}", disk.display());
                return 2;
            }
        };
        let time = if nodates { 0 } else { mtime(&meta) };
        if *is_dir {
            dir_entries.push(Entry {
                stored_name: stored.clone(),
                size: 0,
                time,
                is_dir: true,
                crc: 0,
                block: 0,
                pos_in_block: 0,
            });
            continue;
        }
        let body = match std::fs::read(disk) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("ERROR: {}: {e}", disk.display());
                return 2;
            }
        };
        // The REAL mtime, not the `--nodates` zero: the check compares against
        // what the filesystem will report later, not against what was stored.
        scanned.insert(disk.clone(), (body.len() as u64, mtime(&meta)));
        file_entries.push(Entry {
            stored_name: stored.clone(),
            size: body.len() as u64,
            time,
            is_dir: false,
            crc: crc::calc(&body),
            block: 0,
            pos_in_block: 0,
        });
        contents.insert(stored.clone(), body);
    }

    // `sortFiles command diskfiles` (ArcCreate.hs:114) -- the DISK files, and
    // only those. The archive's own files keep the order the archive wrote them
    // in, and the two lists are interleaved by `mergeFilelists` below.
    let groups = load_groups(parsed);
    file_entries = darc_arc::sort::sort_files(sort_order, &groups, &file_entries);

    // For u/f: read what is already in the archive and merge. By DEFAULT the
    // kept files are recompressed rather than copied -- splitToSolidBlocks only
    // preserves existing solid blocks under --keep-original
    // (ArhiveFileList.hs:297) -- so the merged list is packed exactly as a
    // fresh one would be.
    // What the OUTPUT archive inherits from the input: the comment and the lock
    // are copied by default, so `arc u` on a commented archive keeps the
    // comment. Missing this was a real bug -- an update quietly shortened the
    // archive by exactly the comment's length.
    let mut old_comment = String::new();
    let mut old_locked = false;
    let mut old_recovery = String::new();
    let mut old_sfx: Vec<u8> = Vec::new();
    // Where each archive-origin file came from: (which input archive, which of
    // its blocks, position within that block). Captured before `pos_in_block` is
    // overwritten for the OUTPUT archive, which is the only chance to know it.
    //
    // The archive index exists for `j`: block numbers are local to the archive
    // that carried them, so two inputs both have a block 0.
    let mut source_of: std::collections::HashMap<String, (usize, usize, u64)> =
        std::collections::HashMap::new();
    let mut sources: Vec<(archive::ArchiveInfo, Vec<u8>)> = Vec::new();

    // `opt_recompress` and `opt_keep_original` (Cmdline.hs:372-378).
    //
    // A copying command RECOMPRESSES only when it was told to — by `-m`,
    // `--nodata`, `--crconly` or `--recompress`. Otherwise it keeps the input's
    // own compression, which is why `arc ch x.arc` with no options is nearly
    // free and `arc ch -m0 x.arc` repacks everything.
    //
    // Missing this made `arc d` without `-m` write a different archive from the
    // reference's: 279 bytes against 249 on a three-block test, because the port
    // repacked with the -m4 default what the reference had copied.
    let is_copying = copying;
    // `mainMethod > ""` -- a bare `-m` with no value does NOT count as given,
    // so it does not force a recompress.
    let method_given = !parsed.arg("method", "").is_empty();
    let recompress = parsed.flag("recompress") || (is_copying && method_given);
    let keep_original = parsed.flag("append") || (is_copying && !recompress);

    // `j` joins archives: every extra name on the command line is an INPUT
    // archive whose files are added, with `cmd_archive_filter = const True`
    // (Arc.hs:200). The filespecs are archive names, not file patterns.
    let joining = command == "j";

    if (archive_only || joining || update_type != darc_arc::joinlist::UpdateType::Add)
        && std::path::Path::new(archive_name).exists()
    {
        let info = match archive::read_info(std::path::Path::new(archive_name), pw) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("ERROR: {e}");
                return 2;
            }
        };
        // `abort_on_locked_archive` (ArcCreate.hs:84). A locked archive refuses
        // every modifying command, including the one that would unlock it --
        // that is the point of the lock.
        if info.footer.locked {
            eprintln!("ERROR: can't modify archive locked with -k");
            return 2;
        }
        old_comment = info.footer.comment.clone();
        old_locked = info.footer.locked;
        old_recovery = info.footer.recovery.clone();
        let data = match archive::open(std::path::Path::new(archive_name)) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("ERROR: {e}");
                return 2;
            }
        };
        // `archiveCopyData oldArchive 0 oldSFXSize archive` -- the default is
        // to carry the input archive's SFX stub across.
        old_sfx = data.get(..info.footer.sfx_size as usize).unwrap_or(&[]).to_vec();
        let full_names = parsed.flag("fullnames");
        let main: Vec<darc_arc::joinlist::Candidate> = info
            .entries
            .iter()
            // The archive filter, `setArcFilter` (Arc.hs:211-213). Three shapes:
            // `d` keeps what the filespecs do NOT match, the copy commands keep
            // what they DO, and for a/u/f/j the filespecs select disk files or
            // archive names rather than archive members, so everything is kept.
            .filter(|e| {
                // `fullFileFilter` = the filespec match AND opt_file_filter
                // (Arc.hs:255). `d` negates the whole conjunction, so a file
                // the filters exclude is NOT deleted even when its name matches
                // the filespec -- the command was never allowed to see it.
                let full = darc_arc::sort::match_filespecs(
                    &specs,
                    &e.stored_name,
                    full_names,
                ) && file_filter.accepts(&e.stored_name, e.size, e.time);
                match (deleting, keeps_matching) {
                    (true, _) => !full,
                    (false, true) => full,
                    // a/u/f/j: `cmd_archive_filter = const True`. The filespecs
                    // select DISK files for these, so the archive's own entries
                    // come along whatever they are called.
                    (false, false) => true,
                }
            })
            .map(|e| darc_arc::joinlist::Candidate {
                entry: e.clone(),
                origin: darc_arc::joinlist::Origin::Archive,
                archive: 0,
            })
            .collect();
        sources.push((info, data));

        // `added_list = concatMap arcDirectory added_archives ++ map DiskFile
        // added_diskfiles` (ArhiveFileList.hs:157). The extra archives named by
        // `j` contribute to the ADDED side, ahead of any disk files, and their
        // entries are archive-origin like the main one's.
        let mut added: Vec<darc_arc::joinlist::Candidate> = Vec::new();
        if joining {
            for name in &specs {
                let path = std::path::Path::new(name);
                let extra = match archive::read_info(path, pw) {
                    Ok(i) => i,
                    Err(e) => {
                        eprintln!("ERROR: {name}: {e}");
                        return 2;
                    }
                };
                let extra_data = match archive::open(path) {
                    Ok(d) => d,
                    Err(e) => {
                        eprintln!("ERROR: {name}: {e}");
                        return 2;
                    }
                };
                let idx = sources.len();
                for e in &extra.entries {
                    added.push(darc_arc::joinlist::Candidate {
                        entry: e.clone(),
                        origin: darc_arc::joinlist::Origin::Archive,
                        archive: idx,
                    });
                }
                sources.push((extra, extra_data));
            }
        }
        for e in dir_entries.iter().chain(file_entries.iter()) {
            added.push(darc_arc::joinlist::Candidate {
                entry: e.clone(),
                origin: darc_arc::joinlist::Origin::Disk,
                archive: 0,
            });
        }
        // `mergeFilelists sort_order`, not a concatenation. Only the DISK files
        // were sorted (ArcCreate.hs:114); the archive's own keep their ARCHIVE
        // order, and the two sorted lists are interleaved.
        //
        // Sorting the merged list instead is wrong whenever the archive was not
        // written in sorted order -- which is exactly what `-s-` produces, since
        // per-file blocks set sort_order to "". `ch -m1` on such an archive then
        // packed the block's files in a different order and compressed to 96
        // bytes where the reference got 100.
        let merged = darc_arc::joinlist::join_lists(
            &main,
            &added,
            update_type,
            parsed.flag("append"),
            sort_order,
            |a, b| {
                darc_arc::joinlist::merge_filelists(
                    sort_order,
                    |order, e| darc_arc::sort::sort_key(order, &groups, e),
                    a,
                    b,
                )
            },
        );
        // Pull the bytes of everything that came from the archive.
        dir_entries.clear();
        file_entries.clear();
        for c in merged {
            if c.origin == darc_arc::joinlist::Origin::Archive && !c.entry.is_dir {
                source_of.insert(
                    c.entry.stored_name.clone(),
                    (c.archive, c.entry.block, c.entry.pos_in_block),
                );
                let (src_info, src_data) = match sources.get(c.archive) {
                    Some(s) => s,
                    None => {
                        eprintln!("ERROR: internal: no input archive {}", c.archive);
                        return 2;
                    }
                };
                match archive::read_entry(src_data, src_info, &c.entry, pw) {
                    Ok(body) => {
                        contents.insert(c.entry.stored_name.clone(), body);
                    }
                    Err(e) => {
                        eprintln!("ERROR: {}: {e}", c.entry.stored_name);
                        return 2;
                    }
                }
            }
            if c.entry.is_dir {
                dir_entries.push(c.entry);
            } else {
                file_entries.push(c.entry);
            }
        }
    }


    // `partition isCompressedFile files` then `groupOn cfArcBlock`
    // (ArhiveFileList.hs:292-298). Under keep_original the files that came from
    // an archive are pulled OUT of the type split and grouped by the block they
    // came from -- consecutively, which is well defined because only the DISK
    // files are sorted (ArcCreate.hs:114) and the merge leaves archive files in
    // archive order, so a block's members stay adjacent.
    //
    // `groupOn`, not `sort_and_groupOn`: two runs of the same block separated by
    // a file from another block would be two groups, and neither would be a
    // whole block. That is the behaviour, not an accident of this port.
    let mut kept_groups: Vec<Vec<Entry>> = Vec::new();
    if keep_original {
        let mut rest: Vec<Entry> = Vec::new();
        let mut current: Vec<Entry> = Vec::new();
        // Keyed by (archive, block): under `j` two inputs both have a block 0,
        // and merging their files into one group would produce a "whole block"
        // that is nothing of the sort.
        let mut current_block: Option<(usize, usize)> = None;
        for e in file_entries.drain(..) {
            match source_of.get(&e.stored_name).copied() {
                Some((ai, bi, _)) => {
                    if current_block != Some((ai, bi)) && !current.is_empty() {
                        kept_groups.push(std::mem::take(&mut current));
                    }
                    current_block = Some((ai, bi));
                    current.push(e);
                }
                None => rest.push(e),
            }
        }
        if !current.is_empty() {
            kept_groups.push(current);
        }
        file_entries = rest;
    }

    let mut data = Vec::new();
    // `block` is assigned when the blocks are actually written, below: the
    // directories block may or may not exist, so the first data block's index
    // is not known here.
    for e in &mut file_entries {
        e.pos_in_block = data.len() as u64;
        match contents.get(&e.stored_name) {
            Some(body) => data.extend_from_slice(body),
            None => {}
        }
    }

    // splitFileTypes: which files share a block, decided by CONTENT. Only
    // reached when the level defines more than one chain -- with a single chain
    // every file is type 0 anyway, and probing would be wasted work.
    let type_groups: Vec<(usize, Vec<usize>)> = if chains.len() > 1 {
        let cands: Vec<darc_arc::filetype::Candidate<'_>> = file_entries
            .iter()
            .map(|e| darc_arc::filetype::Candidate {
                stored_name: &e.stored_name,
                size: e.size,
                data: contents.get(&e.stored_name).map(Vec::as_slice).unwrap_or(&[]),
                // getDefaultType: every autodetectable type becomes $binary,
                // and only arc.groups can produce anything else.
                default_type: "$binary",
            })
            .collect();
        let split = darc_arc::filetype::split_file_types(&cands, &type_names);
        darc_arc::filetype::merge_by_type(&split, |t| chains[t].clone())
    } else if file_entries.is_empty() {
        // No files means no data block. `concatMap splitOneType (splitByType …)`
        // over an empty list produces nothing, the same way the directories
        // block is omitted when there are no directories. `arc d *.txt` on an
        // -m0 archive leaves only directories, and an empty block made it one
        // byte longer than the reference's.
        Vec::new()
    } else {
        vec![(0, (0..file_entries.len()).collect())]
    };

    // Now that the split is known, check only the chains it reaches.
    for (ty, _) in &type_groups {
        for m in chains[*ty].split('+') {
            match darc_arc::method::Method::parse(m) {
                Some(darc_arc::method::Method::Unsupported(name)) => {
                    eprintln!(
                        "ERROR: -m{method} needs {name} for {}, which this port cannot write yet",
                        if type_names[*ty].is_empty() {
                            "the default file type"
                        } else {
                            &type_names[*ty]
                        }
                    );
                    return 2;
                }
                Some(_) => {}
                None => {
                    eprintln!("ERROR: -m{method}: {m} does not parse");
                    return 2;
                }
            }
        }
    }

    // splitToSolidBlocks (ArhiveFileList.hs:291): directories go into their own
    // block, always stored. The files are split by opt_group_data -- except for
    // aNO_COMPRESSION, where splitOneType returns a single block whatever the
    // grouping says, so -m0 -s- is still one block.


    // The encryption algorithm, canonicalised the way Cmdline.hs:529 does it
    // before it can reach a block: `-ae aes` becomes `aes-256/ctr:n1000:r0`.
    let mut algorithm = Vec::new();
    if !pw.data.is_empty() || !pw.headers.is_empty() {
        for part in parsed.arg("encryption", "aes").split('+') {
            match darc_arc::encryption::canonize_for_writing(part) {
                Some(c) => algorithm.push(c),
                None => {
                    eprintln!("ERROR: bad name or parameters in encryption algorithm {part}");
                    return 2;
                }
            }
        }
    }
    let mut w = darc_arc::writer::Writer::with_encryption(
        algorithm,
        pw.data.clone(),
        pw.headers.clone(),
    );
    // `defaultDirCompressor = thd3 grouping ||| aDEFAULT_DIR_COMPRESSION`
    // (Cmdline.hs:117): three of the -s presets force an UNCOMPRESSED
    // directory, which is archive-visible.
    if !solid.dir_method.is_empty() {
        let decoded = darc_arc::methodtable::decode_method(&solid.dir_method);
        match decoded.first() {
            Some((_, chain)) => w.set_dir_compressor(chain.clone()),
            None => {
                eprintln!("ERROR: -dm{}: expanded to nothing", solid.dir_method);
                return 2;
            }
        }
    }
    // `writeSFX` (ArcCreate.hs:323), and the `s…` command is `ch -sfx…`
    // (Cmdline.hs:166) -- its suffix is the module name.
    //
    //   -sfx-      drop the stub the input archive had
    //   -sfx--     copy it across (the default)
    //   -sfxNAME   prepend the named module file
    //
    // A bare `-sfx` means aDEFAULT_SFX, "freearc.sfx", which the reference
    // looks up in its library directory. That lookup is `findFile
    // libraryFilePlaces`, the same executable-relative search that already does
    // not resolve for this port, so a bare `-sfx` is refused rather than
    // silently producing an archive with no stub.
    let sfx_default = match command.strip_prefix('s') {
        Some(rest) if command.starts_with('s') => rest,
        _ => "--",
    };
    let sfx = parsed.arg("sfx", sfx_default);
    // The name is decided TWICE. `ArcCreate.hs:68` picks the SFX name up front
    // for an archive that does not exist yet, and `renameArchiveAsSFX`
    // (`:172`) renames afterwards in every case -- so `ch -sfx-` on an existing
    // `x` still produces `x.arc`. Doing only the first gives the right bytes
    // under the wrong name.
    let written_name = change_sfx_ext(sfx, archive_name);
    let archive_name: &str = &match std::path::Path::new(archive_name).exists() {
        true => archive_name.to_string(),
        false => written_name.clone(),
    };
    match sfx {
        "-" => {}
        "--" => w.write_sfx(&old_sfx),
        "" => {
            eprintln!(
                "ERROR: a bare -sfx needs the default module freearc.sfx from the \
                 library directory, which this port does not locate; name one \
                 explicitly"
            );
            return 2;
        }
        name => match std::fs::read(name) {
            Ok(module) => w.write_sfx(&module),
            Err(e) => {
                eprintln!("ERROR: can't open SFX module {name}: {e}");
                return 2;
            }
        },
    }
    w.write_header();

    // `dirs &&& [(aNO_COMPRESSION, dirs)]` (ArhiveFileList.hs:291): the
    // directories block exists only when there ARE directories. Writing an
    // empty one produces an archive that lists identically and is five bytes
    // longer -- which is how `arc f` on a tree whose only subdirectory is not
    // freshened first showed it.
    let mut data_blocks: Vec<darc_arc::block::ArchiveBlock> = Vec::new();
    if !dir_entries.is_empty() {
        match w.write_data(&[], darc_arc::writer::no_compression(), dir_entries.len()) {
            Ok(b) => data_blocks.push(b),
            Err(e) => {
                eprintln!("ERROR: {e}");
                return 2;
            }
        }
    }

    // The kept blocks come BEFORE the freshly split ones: splitToSolidBlocks is
    // `dirs ++ map … solidBlocksToKeep ++ concatMap splitOneType …`.
    let mut kept_entries: Vec<Entry> = Vec::new();
    for group in &kept_groups {
        let (ai, bi) = match source_of.get(&group[0].stored_name) {
            Some((ai, bi, _)) => (*ai, *bi),
            None => continue,
        };
        let (src_info, src_bytes) = match sources.get(ai) {
            Some(s) => s,
            None => {
                eprintln!("ERROR: internal: no input archive {ai}");
                return 2;
            }
        };
        let src = match src_info.data_blocks.get(bi) {
            Some(b) => b,
            None => {
                eprintln!("ERROR: internal: block {bi} is not in input archive {ai}");
                return 2;
            }
        };
        // `isWholeSolidBlock` (ArhiveFileList.hs:387), all four conditions: the
        // group starts at offset 0 of the block, has as many files as the block
        // has, and is in increasing position order. A group failing any of them
        // cannot be copied -- the block's bytes also contain the files that did
        // not survive.
        let positions: Vec<u64> = group
            .iter()
            .filter_map(|e| match source_of.get(&e.stored_name) {
                // Same archive AND same block: a group can only be whole if
                // every member agrees on both.
                Some((a, b, p)) if *a == ai && *b == bi => Some(*p),
                _ => None,
            })
            .collect();
        let whole = positions.len() == group.len()
            && positions.first() == Some(&0)
            && src.files == Some(group.len())
            && positions.windows(2).all(|w| w[0] <= w[1]);

        let block = match whole {
            true => {
                let start = src.pos as usize;
                let end = start.saturating_add(src.comp_size as usize);
                let packed = match src_bytes.get(start..end) {
                    Some(p) => p,
                    None => {
                        eprintln!("ERROR: the input archive is truncated at block {bi}");
                        return 2;
                    }
                };
                for e in group {
                    let mut e = e.clone();
                    e.block = data_blocks.len();
                    // The bytes are unchanged, so every offset into them is too.
                    kept_entries.push(e.clone());
                    drop(e);
                }
                w.write_copied_data(
                    packed,
                    src.orig_size,
                    src.compressor.clone(),
                    group.len(),
                )
            }
            // A partial block is repacked, but with the block's OWN chain, not
            // the -m default: the group is keyed by `cfCompressor . head`.
            false => {
                let mut body = Vec::new();
                for e in group {
                    let mut e = e.clone();
                    e.block = data_blocks.len();
                    e.pos_in_block = body.len() as u64;
                    match contents.get(&e.stored_name) {
                        Some(b) => body.extend_from_slice(b),
                        None => {}
                    }
                    kept_entries.push(e);
                }
                let original = src.compressor.join("+");
                let fitted = match darc_arc::memlimit::fit_for_add(&original, body.len() as u64) {
                    Some(f) => f,
                    None => {
                        eprintln!("ERROR: cannot fit {original} to {} bytes", body.len());
                        return 2;
                    }
                };
                let compressor: Vec<String> = fitted.split('+').map(str::to_string).collect();
                match w.write_compressed_data(&body, compressor, group.len()) {
                    Ok(b) => b,
                    Err(e) => {
                        eprintln!("ERROR: {e}");
                        return 2;
                    }
                }
            }
        };
        data_blocks.push(block);
    }

    // splitOneType (ArhiveFileList.hs:312) applied per file TYPE: each type's
    // files are split into solid blocks by the -s criteria, and each block is
    // packed with ITS OWN type's chain. Using the main chain for all of them
    // was wrong by 1.500 to 3.100 bytes on the four multi-type levels.
    //
    // `aNO_COMPRESSION` short-circuits: "for fake compressors or -m0 there is
    // no point in splitting the block into parts", so -m0 -s- is still ONE
    // block however the grouping reads.
    let mut reordered: Vec<Entry> = Vec::new();
    for (ty, group) in &type_groups {
        let chain = &chains[*ty];
        let crits: Vec<darc_arc::grouping::Grouping> = match chain.as_str() {
            "storing" => Vec::new(),
            _ => solid.data.clone(),
        };
        let items: Vec<darc_arc::grouping::Item> = group
            .iter()
            .map(|&i| darc_arc::grouping::Item {
                size: file_entries[i].size,
                ext: darc_arc::sort::lc_extension(&file_entries[i].stored_name),
            })
            .collect();
        let mut at = 0usize;
        for len in darc_arc::grouping::split_blocks(&crits, &items) {
            let mut body = Vec::new();
            for &i in &group[at..at + len] {
                let mut e = file_entries[i].clone();
                e.block = data_blocks.len();
                e.pos_in_block = body.len() as u64;
                match contents.get(&e.stored_name) {
                    Some(b) => body.extend_from_slice(b),
                    None => {}
                }
                reordered.push(e);
            }
            at += len;
            let fitted = match darc_arc::memlimit::fit_for_add(chain, body.len() as u64) {
                Some(f) => f,
                None => {
                    eprintln!("ERROR: cannot fit {chain} to {} bytes", body.len());
                    return 2;
                }
            };
            let compressor: Vec<String> = fitted.split('+').map(str::to_string).collect();
            match w.write_compressed_data(&body, compressor, len) {
                Ok(b) => data_blocks.push(b),
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    return 2;
                }
            }
        }
    }
    file_entries = reordered;

    // Same order as the blocks: directories, then the copied/kept ones, then
    // the freshly split ones. The directory stores block membership as a run
    // length over this list, so the two orders have to agree.
    let mut entries = dir_entries;
    entries.extend(kept_entries);
    entries.extend(file_entries);
    w.write_directory(&data_blocks, &entries);
    // An archive with nothing left in it is REMOVED, not written empty.
    // Measured: `arc d a.arc "*"` leaves no file behind, because the basename
    // match takes the directories too. Writing a 161-byte archive of nothing
    // would be valid, listable, and not what the reference does.
    if entries.is_empty() {
        match std::fs::remove_file(archive_name) {
            Ok(()) => {}
            // Already absent is the same outcome.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                eprintln!("ERROR: {archive_name}: {e}");
                return 2;
            }
        }
        println!("All OK");
        return 0;
    }

    // `getArcComment` (ArcCreate.hs:299). --archive-comment wins outright;
    // otherwise the -z value decides, and its DEFAULT is "--" meaning "keep the
    // one the input archive had". The `c` command is documented as "ch -z", so
    // its default is "" -- read the new comment from stdin.
    let arccmt_default = if command == "c" { "" } else { "--" };
    let comment = match parsed.arg("archive-comment", "") {
        s if !s.is_empty() => s.to_string(),
        _ => match parsed.arg("arccmt", arccmt_default) {
            // Read it from stdin, which is what `arc c` does.
            //
            // `uiInputArcComment` (CUI.hs:210) reads LINES UNTIL A LONE "." --
            // not until end of file. Hitting EOF first is an error there
            // (`hGetLine: end of file`) and the archive is left alone, so a
            // piped comment that forgets its terminator must fail rather than
            // silently replacing the comment with whatever arrived.
            "" => {
                println!("Enter archive comment, ending with \".\" on separate line:");
                drop(std::io::stdout().flush());
                let mut lines: Vec<String> = Vec::new();
                let mut terminated = false;
                for line in std::io::BufRead::lines(std::io::stdin().lock()) {
                    match line {
                        Ok(l) => {
                            if l == "." {
                                terminated = true;
                                break;
                            }
                            lines.push(l);
                        }
                        Err(e) => {
                            eprintln!("ERROR: reading the comment: {e}");
                            return 2;
                        }
                    }
                }
                if !terminated {
                    eprintln!("ERROR: <stdin>: end of file before the closing \".\"");
                    return 2;
                }
                lines.join("\n")
            }
            // Delete the old comment.
            "-" => String::new(),
            // Copy it -- the default.
            "--" => old_comment.clone(),
            // Read it from the named file.
            file => match std::fs::read_to_string(file) {
                Ok(s) => s.trim_end_matches(['\r', '\n']).to_string(),
                Err(e) => {
                    eprintln!("ERROR: {file}: {e}");
                    return 2;
                }
            },
        },
    };
    // `opt_lock_archive = findNoArg o "lock" || cmd=="k"` (Cmdline.hs). The lock
    // is one-way: there is no option that clears it, so an archive that arrived
    // locked stays locked -- and a locked one is refused above before it gets
    // here, so `old_locked` can only be false in practice. It is carried anyway
    // rather than hard-coded, so that the day a `-k-` appears this still says
    // what it means.
    let locked = old_locked || parsed.flag("lock") || command == "k";

    // `-rr`, defaulting to the archive's own setting -- and the `rr…` command
    // is `ch -rr…` (Cmdline.hs:124), so the suffix after "rr" is the value.
    //
    // `0.1%`/`0.01%` are rewritten to their `N*SS` spellings before the option
    // is looked at, as `changeTo` does.
    let rr_default = match command.strip_prefix("rr") {
        Some(rest) => rest,
        None => "--",
    };
    let rr_option = match parsed.arg("recovery", rr_default) {
        "0.1%" => "0*4kb",
        "0.01%" => "0*64kb",
        s => s,
    };
    // `rr_ok` runs before anything interprets the value, and it rejects "+" --
    // so `arc a -rr+` is an error even though writeRecoveryBlocks has a case
    // for it.
    if !darc_arc::recovery::option_is_valid(rr_option) {
        eprintln!(
            "ERROR: invalid value {rr_option:?} for option --recovery (-rr); \
             allowed: MEM, N, N%, MEM;SS, N%;SS, N*SS, -, \"\""
        );
        return 2;
    }
    // The recommendation depends on the archive's size, which is not known
    // until the blocks are written; `finish` is where it lands.
    let bytes = match darc_arc::recovery::resolve(rr_option, &old_recovery, 0).is_empty()
        && rr_option != ""
        && rr_option != "+"
    {
        // Nothing to add: the plain footer.
        true => w.finish(&comment, "", locked),
        false => {
            // The size-dependent default needs the finished length, so ask the
            // writer for it and resolve there.
            let plain_len = w.projected_len(&comment, locked);
            let recovery =
                darc_arc::recovery::resolve(rr_option, &old_recovery, plain_len);
            match recovery.is_empty() {
                true => w.finish(&comment, "", locked),
                false => {
                    // Half the physical memory, as the C caps it. Not measured
                    // here: a cap that varied with the machine would make the
                    // archive's geometry vary with it too.
                    const MEM_LIMIT: u64 = 2 * 1024 * 1024 * 1024;
                    match w.finish_with_recovery(&comment, &recovery, locked, MEM_LIMIT) {
                        Some(b) => b,
                        None => {
                            eprintln!(
                                "ERROR: -rr{recovery}: not a recovery-info size \
                                 (MEM, N, N%, MEM;SS, N%;SS, N*SS, -)"
                            );
                            return 2;
                        }
                    }
                }
            }
        }
    };

    match std::fs::write(archive_name, &bytes) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("ERROR: {archive_name}: {e}");
            return 2;
        }
    }

    // `renameArchiveAsSFX` (ArcCreate.hs:172) -- after writing, in every case,
    // not only when the archive was new.
    let archive_name: &str = match written_name != archive_name {
        false => archive_name,
        true => {
            println!("Renaming {archive_name} to {written_name}");
            match std::fs::rename(archive_name, &written_name) {
                Ok(()) => &written_name,
                Err(e) => {
                    eprintln!("ERROR: {archive_name}: {e}");
                    return 2;
                }
            }
        }
    };

    // `-v`/`--volume` (ArcCreate.hs:218) -- split the FINISHED archive into
    // `.001`, `.002`, … and remove the original.
    //
    // There is no per-volume header and no cross-volume structure: it is a
    // plain byte split, which is why the message tells the user to reassemble
    // with `cat`. The read side exists in C (`darc_join_volumes`) and has NO
    // caller, so nothing reads volumes back automatically -- checked, not
    // assumed.
    //
    // The archive is written whole first and split after, so a `-v` smaller
    // than the archive still needs room for both at once. That is the
    // reference's behaviour too.
    match parsed.arg("volume", "") {
        "" => {}
        spec => {
            let volsize = match darc_arc::filter::parse_size(spec) {
                Some(n) if n > 0 => n,
                // `parseSize` errors out in the Haskell; a zero or unparseable
                // size would otherwise loop for ever writing empty volumes.
                _ => {
                    eprintln!("ERROR: -v{spec}: not a volume size");
                    return 2;
                }
            };
            let mut vol = 0usize;
            for chunk in bytes.chunks(volsize as usize) {
                vol += 1;
                let name = format!("{archive_name}.{vol:03}");
                match std::fs::write(&name, chunk) {
                    Ok(()) => {}
                    Err(e) => {
                        eprintln!("ERROR: {name}: {e}");
                        return 2;
                    }
                }
            }
            match std::fs::remove_file(archive_name) {
                Ok(()) => {}
                Err(e) => {
                    eprintln!("ERROR: {archive_name}: {e}");
                    return 2;
                }
            }
            println!(
                "Split into {vol} volume(s): {archive_name}.001 .. .{vol:03}"
            );
            println!(
                "To extract, reassemble with: cat {archive_name}.* > {archive_name}"
            );
        }
    }

    // `postProcessWrapper` (ArcCreate.hs:248) -- only AFTER the archive is
    // safely written, and only for files that came from disk.
    if del_dirs || del_files {
        println!("Deleting successfully archived files");
        for (_, disk, is_dir) in &found {
            if *is_dir {
                continue;
            }
            // `checkThatFileWasNotChanged` (ArcCreate.hs:287): size and mtime
            // must still match what was archived. A file rewritten while the
            // archive was being built is NOT deleted -- the copy in the archive
            // is of the old contents, so removing it would lose the new ones.
            let same = match (std::fs::metadata(disk), scanned.get(disk)) {
                (Ok(md), Some((size, time))) => md.len() == *size && mtime(&md) == *time,
                // Never scanned, or gone: leave it alone. Deleting a file this
                // run did not archive is the one outcome with no way back.
                _ => false,
            };
            if !same {
                continue;
            }
            // `ignoreErrors . fileRemove`: a file that cannot be removed is not
            // a failure of the archiving, which has already succeeded.
            drop(std::fs::remove_file(disk));
        }
        if del_dirs {
            // `reverse dirs` -- deepest first, so a directory is empty by the
            // time its own removal is attempted. `dirRemove` is likewise
            // best-effort: a directory holding something that was not archived
            // simply stays.
            for (_, disk, is_dir) in found.iter().rev() {
                if *is_dir {
                    drop(std::fs::remove_dir(disk));
                }
            }
        }
    }

    println!("All OK");
    0
}


/// The groups file, resolved the way `Cmdline.hs:382` resolves it.
///
/// `--groups=FILE` names one, `--groups-` disables grouping, and the default is
/// `arc.groups` beside the executable — `configFilePlaces` is
/// `takeDirectory(getExeName) </> filename` and nothing else (`Files.hs:208`).
/// No groups file means `[reANY_FILE]`: one group holding everything.
///
/// ## The reference never finds one on macOS
///
/// Measured, and it changes what this port must do to match. `Tests/arc-ghc`
/// produces the same file order with no option, with `--groups-`, and with
/// `arc.groups` copied to sit beside the binary — while `--groups=<path>`
/// produces a different one. So `getExeName` does not resolve here, the default
/// lookup finds nothing, and the reference's default IS the one-group path.
///
/// That is a pre-existing bug in the reference on this platform, not something
/// this port should imitate: the lookup below is the faithful one. It simply
/// finds nothing either, because `darc` does not live beside an arc.groups.
fn load_groups(parsed: &options::Parsed) -> darc_arc::sort::Groups {
    match parsed.arg("groups", "--") {
        // `--groups-` -- the option's value is a bare "-".
        "-" => return darc_arc::sort::Groups::single(),
        "--" => {}
        explicit => match std::fs::read_to_string(explicit) {
            Ok(text) => return darc_arc::sort::Groups::parse(&text),
            Err(e) => {
                eprintln!("ERROR: {explicit}: {e}");
                std::process::exit(2);
            }
        },
    }
    let beside_exe = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(|d| d.join("arc.groups")));
    match beside_exe {
        Some(path) => match std::fs::read_to_string(&path) {
            Ok(text) => darc_arc::sort::Groups::parse(&text),
            Err(_) => darc_arc::sort::Groups::single(),
        },
        None => darc_arc::sort::Groups::single(),
    }
}

fn mtime(meta: &std::fs::Metadata) -> i64 {
    use std::time::UNIX_EPOCH;
    match meta.modified().ok().and_then(|t| t.duration_since(UNIX_EPOCH).ok()) {
        Some(d) => d.as_secs() as i64,
        None => 0,
    }
}

/// Collect files under `path`, with names stored relative to the spec.
///
/// The order is neither plain depth-first nor breadth-first, and getting it
/// wrong writes an archive that reads back perfectly and is not the reference's
/// bytes. `findFiles_FileInfo` (`FileInfo.hs:320`) drives `processDir` with
/// `recursiveM`, and `processDir` hands the WHOLE of one directory's contents
/// to `process_f` in a single chunk, then returns its subdirectories for the
/// recursion. So:
///
/// > list a directory completely, THEN descend into its subdirectories in order.
///
/// On the test corpus that yields directories
/// `binary edge many nested text` then `nested/a nested/a/b nested/a/b/c` --
/// which looks breadth-first, because every level-1 directory is named in the
/// first chunk -- while the files come out `binary/* edge/* many/*
/// nested/a/shallow.txt nested/a/b/c/deep.txt text/*`, which looks
/// depth-first. Both fall out of the one rule above. Emitting a directory and
/// immediately descending into it (the obvious depth-first walk) misorders
/// `text`; listing strictly level by level misorders the `nested` files.
///
/// Each directory's entries are sorted before use. Not tidiness: readdir order
/// is filesystem order, and an archive built from it is not reproducible.
fn scan(
    dir: &std::path::Path,
    prefix: &str,
    recursive: bool,
    out: &mut Vec<(String, std::path::PathBuf, bool)>,
) -> std::io::Result<()> {
    let mut names: Vec<std::ffi::OsString> =
        std::fs::read_dir(dir)?.filter_map(|e| e.ok()).map(|e| e.file_name()).collect();
    names.sort();

    // One chunk: every entry of this directory, dirs and files together.
    let mut subdirs = Vec::new();
    for name in names {
        let child = dir.join(&name);
        let stored = if prefix.is_empty() {
            name.to_string_lossy().into_owned()
        } else {
            format!("{prefix}/{}", name.to_string_lossy())
        };
        let meta = std::fs::symlink_metadata(&child)?;
        if meta.is_dir() {
            out.push((stored.clone(), child.clone(), true));
            subdirs.push((child, stored));
        } else if meta.is_file() {
            out.push((stored, child, false));
        }
    }

    // ...and only then descend.
    if recursive {
        for (child, stored) in subdirs {
            scan(&child, &stored, recursive, out)?;
        }
    }
    Ok(())
}

/// `arc l`, `v`, `lb` and `lt` (`ArcExtract.hs:189`).
///
/// The four share one summary line and differ only in the body. `lb` prints
/// bare names with `myPutStr` -- no trailing newline and no summary at all.
fn list(command: &str, info: &archive::ArchiveInfo, entries: &[Entry]) -> i32 {
    if command == "lb" {
        // `myPutStr$ joinWith "\n"$ map filename directory` -- names, joined.
        // Two things measured rather than read off: `lb` prints NO banner, the
        // only listing command that does not, and the output DOES end with a
        // newline even though myPutStr writes none.
        let names: Vec<&str> =
            entries.iter().map(|e| e.stored_name.as_str()).collect();
        println!("{}", names.join("\n"));
        return 0;
    }
    if command == "lt" {
        println!("              Pos            Size      Compressed   Files Method");
        println!(
            "-----------------------------------------------------------------------------"
        );
        for b in &info.data_blocks {
            // The leading column is the encryption marker.
            println!(
                "{} {:>15} {:>15} {:>15} {:>7} {}",
                if b.is_encrypted() { "*" } else { " " },
                show3(b.pos),
                show3(b.orig_size),
                show3(b.comp_size),
                show3(b.files.unwrap_or(0) as u64),
                b.compressor.join("+")
            );
        }
        println!(
            "-----------------------------------------------------------------------------"
        );
        let total: u64 = entries.iter().map(|e| e.size).sum();
        // `lt` sums the block table directly, unlike `l` and `v`.
        let packed: u64 = info.data_blocks.iter().map(|b| b.comp_size).sum();
        println!(
            "{} files, {} bytes, {} compressed",
            show3(entries.len() as u64),
            show3(total),
            show3(packed)
        );
        println!("All OK\n");
        return 0;
    }
    let verbose = command == "v";
    if verbose {
        println!(
            "Date/time              Attr            Size          Packed      CRC Filename"
        );
        println!(
            "-----------------------------------------------------------------------------"
        );
    } else {
        println!("Date/time                  Size Filename");
        println!("----------------------------------------");
    }
    let mut total = 0u64;
    // myMapM (ArcExtract.hs:231): a block's packed size is charged to the first
    // file of each contiguous run sharing it.
    let mut compressed = 0u64;
    let mut prev: Option<u64> = None;
    for e in entries {
        let (pos, csize) = match info.data_blocks.get(e.block) {
            Some(b) => (b.pos, b.comp_size),
            None => (0, 0),
        };
        // myMapM charges the block to the FIRST file of each contiguous run,
        // and `v` prints that per-file figure in its Packed column.
        let charged = if prev != Some(pos) { csize } else { 0 };
        compressed += charged;
        prev = Some(pos);

        if verbose {
            println!(
                "{} {} {:>15} {:>15} {:0>8x} {}",
                format_time(e.time),
                if e.is_dir { ".D....." } else { "......." },
                e.size,
                charged,
                e.crc,
                e.stored_name
            );
        } else {
            let size = if e.is_dir { "-dir-".to_string() } else { show3(e.size) };
            println!("{} {:>11} {}", format_time(e.time), size, e.stored_name);
        }
        total += e.size;
    }
    if verbose {
        println!(
            "-----------------------------------------------------------------------------"
        );
    } else {
        println!("----------------------------------------");
    }
    println!(
        "{} files, {} bytes, {} compressed",
        show3(entries.len() as u64),
        show3(total),
        show3(compressed)
    );
    println!("All OK\n");
    0
}

/// `arc t`, `arc x` and `arc e` — one pass over the solid blocks, in parallel.
///
/// Testing and extracting differ only in what happens after the CRC check, so
/// they share the loop rather than duplicating the block handling.
fn run_blocks(
    info: &archive::ArchiveInfo,
    data: &[u8],
    layout: &Layout,
    extracting: bool,
    pw: &darc_arc::passwords::Passwords,
    entries: &[Entry],
) -> i32 {
    // The safety check runs on the archive's contribution alone: the
    // destination is the user's own and may be absolute.
    let relative = Layout { disk_basedir: String::new(), ..layout.clone() };

    if extracting && layout.creates_directories() {
        for e in entries.iter().filter(|e| e.is_dir) {
            if !darc_arc::extract::is_safe(&relative.disk_name(e)) {
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
            let compressor = match archive::keyed(&b.compressor, b, pw) {
                Ok(c) => c,
                Err(e) => {
                    bad.push(format!("{e}"));
                    return bad;
                }
            };
            let unpacked =
                match decompress::decompress_chain(&compressor, packed, b.orig_size as usize) {
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
                if crc::calc(bytes) != e.crc {
                    bad.push(format!("{}: CRC failed", e.stored_name));
                    continue;
                }
                if !extracting {
                    continue;
                }
                if !darc_arc::extract::is_safe(&relative.disk_name(e)) {
                    bad.push(format!("refusing unsafe path {:?}", e.stored_name));
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
                    Ok(()) => {}
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
        println!(
            "Extracted {} files, {} bytes.",
            show3(entries.len() as u64),
            show3(total_bytes)
        );
    }
    if failures == 0 {
        println!("All OK");
        0
    } else {
        2
    }
}

/// mtimes are formatted in LOCAL time, as `System.Time`'s `toCalendarTime`
/// does. Reproduced rather than corrected: matching the reference is the bar.
fn format_time(t: i64) -> String {
    let secs = t + local_offset_seconds();
    let days = secs.div_euclid(86_400);
    let tod = secs.rem_euclid(86_400);
    let (y, m, d) = civil_from_days(days);
    format!("{y:04}-{m:02}-{d:02} {:02}:{:02}:{:02}", tod / 3600, (tod % 3600) / 60, tod % 60)
}

fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

fn local_offset_seconds() -> i64 {
    // SAFETY: localtime_r writes into a tm we own; time 0 is always valid.
    unsafe {
        let t: i64 = 0;
        let mut tm: Tm = std::mem::zeroed();
        localtime_r(&t, &mut tm);
        tm.tm_gmtoff
    }
}

#[repr(C)]
struct Tm {
    tm_sec: i32,
    tm_min: i32,
    tm_hour: i32,
    tm_mday: i32,
    tm_mon: i32,
    tm_year: i32,
    tm_wday: i32,
    tm_yday: i32,
    tm_isdst: i32,
    tm_gmtoff: i64,
    tm_zone: *const i8,
}

extern "C" {
    fn localtime_r(t: *const i64, tm: *mut Tm) -> *mut Tm;
}

// ── Passwords ───────────────────────────────────────────────────────────────

/// `cookPasswords`'s inputs, gathered from the parsed command line
/// (`Cmdline.hs:534-582`).
///
/// `-op` has two spellings. Besides the option itself, `-o` values that begin
/// with `p` are peeled and treated as old passwords (`is_op_option`,
/// `Cmdline.hs:156`) — which is why `-op-` disables prompting even though `-op`
/// looks like an overwrite mode.
fn cook_passwords(parsed: &options::Parsed, command: &str) -> darc_arc::passwords::Passwords {
    use darc_arc::passwords::{cook, Prompt, Raw};

    // `partition is_op_option (findReqList o "overwrite")`, then `tryToSkip "p"`.
    let op_opt: Vec<&str> = parsed
        .all("overwrite")
        .into_iter()
        .filter(|v| v.len() >= 2 && v.starts_with('p'))
        .map(|v| &v[1..])
        .collect();

    let read_file = |name: &str| -> Vec<u8> {
        match std::fs::read(name) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("ERROR: {name}: {e}");
                std::process::exit(2);
            }
        }
    };
    let mut old_keyfiles: Vec<Vec<u8>> = Vec::new();
    for name in parsed.all("OldKeyfile").into_iter().chain(parsed.all("keyfile")) {
        old_keyfiles.push(read_file(name));
    }
    // `unlessNull fileGetBinary` — the LAST -kf, and nothing when absent.
    let keyfile = match parsed.arg("keyfile", "") {
        "" => Vec::new(),
        name => read_file(name),
    };

    let mut old_password_list = op_opt.clone();
    old_password_list.extend(parsed.all("OldPassword"));

    let raw = Raw {
        password: parsed.arg("password", "--"),
        headers_password: parsed.arg("HeadersPassword", "--"),
        password_list: parsed.all("password"),
        headers_list: parsed.all("HeadersPassword"),
        old_password_list,
        old_keyfiles,
        keyfile,
        dont_ask: op_opt.last().copied() == Some("-")
            || parsed.arg("OldPassword", "") == "-"
            || parsed.arg("password", "") == "-"
            || parsed.arg("HeadersPassword", "") == "-",
    };
    // `cmdType cmd == ADD_CMD` picks the double-entry encryption prompt.
    let prompt = match command {
        "a" | "u" | "f" | "m" | "mf" | "c" | "ch" | "k" | "d" | "j" => Prompt::Encryption,
        _ => Prompt::Decryption,
    };
    cook(&raw, prompt, ask_password)
}

/// `ask_encryption_password` / `ask_decryption_password` (`CUI.hs:141`).
///
/// The encryption prompt asks twice and repeats until the two agree; the
/// decryption prompt asks once. Both hide the input. The strings and their
/// leading blank line are reproduced exactly, because they are what a user
/// driving both binaries side by side sees.
fn ask_password(prompt: darc_arc::passwords::Prompt) -> String {
    match prompt {
        darc_arc::passwords::Prompt::Decryption => {
            print!("\n  Enter decryption password:");
            drop(std::io::stdout().flush());
            hidden_line()
        }
        darc_arc::passwords::Prompt::Encryption => loop {
            print!("\n  Enter encryption password:");
            drop(std::io::stdout().flush());
            let first = hidden_line();
            print!("  Reenter encryption password:");
            drop(std::io::stdout().flush());
            let second = hidden_line();
            if first == second {
                return first;
            }
            println!("  Passwords are different. You need to repeat input");
        },
    }
}

/// `getHiddenLine` — read one line with the terminal's echo turned off, and
/// print the newline the terminal no longer echoes.
///
/// Echo is restored even when the read fails, so a `^C` at the prompt does not
/// leave the user's shell silent.
fn hidden_line() -> String {
    let restore = echo_off();
    let mut line = String::new();
    let read = std::io::stdin().read_line(&mut line);
    restore();
    println!();
    match read {
        Ok(_) => line.trim_end_matches(['\r', '\n']).to_string(),
        Err(_) => String::new(),
    }
}

/// Turn terminal echo off, returning the action that turns it back on.
///
/// Driven through `stty` rather than `tcsetattr`. The reason is not taste: a
/// hand-declared `struct termios` has a different field width and `c_cc` length
/// on macOS than on Linux, and getting it wrong corrupts the terminal settings
/// on whichever platform was not the one it was written on — while compiling
/// and appearing to work on the other. `stty` knows the layout on both.
///
/// It reads `/dev/tty`, not stdin, so a harness that pipes a password in gets
/// a harmless failure here and an un-hidden read after it, rather than having
/// its pipe consumed.
fn echo_off() -> impl FnOnce() {
    let worked = stty("-echo");
    move || {
        if worked {
            let restored = stty("echo");
            if !restored {
                eprintln!("WARNING: could not restore terminal echo");
            }
        }
    }
}

fn stty(arg: &str) -> bool {
    let tty = match std::fs::File::open("/dev/tty") {
        Ok(f) => f,
        Err(_) => return false,
    };
    match std::process::Command::new("stty").arg(arg).stdin(tty).status() {
        Ok(status) => status.success(),
        Err(_) => false,
    }
}

// ── Times on the command line ───────────────────────────────────────────────

/// `makeCalendarTime` (`Cmdline.hs:459`) — a `YYYYMMDDHHMMSS` argument to
/// `-ta`/`-tb`, as seconds since the epoch.
///
/// The fields are taken by POSITION, not by parsing separators: four digits of
/// year, then two each of month, day, hour, minute, second, and a short string
/// simply runs out — `readInt ""` is 0, so `-ta2025` means midnight on the 1st
/// of month 0, which `max (x-1) 0` clamps to January.
///
/// The value is LOCAL time. The Haskell gets there by round-tripping through
/// `toCalendarTime . toClockTime` twice to settle `ctTZ`; this uses the offset
/// `localtime_r` reports, which is the same answer for every zone without a
/// mid-interval DST change.
fn parse_local_time(s: &str) -> Option<i64> {
    let digits: Vec<char> = s.chars().collect();
    if digits.iter().any(|c| !c.is_ascii_digit()) || digits.is_empty() {
        return None;
    }
    // `readInt (take n (drop k s))`, with a short string yielding "" -> 0.
    let field = |from: usize, len: usize| -> i64 {
        let end = (from + len).min(digits.len());
        if from >= digits.len() {
            return 0;
        }
        digits[from..end].iter().collect::<String>().parse().unwrap_or(0)
    };
    let year = field(0, 4);
    // `ctMonth = readInt … .$ (\x -> max (x-1) 0) .$ toEnum` -- a 0 or absent
    // month is January, and the value is a MONTH INDEX from 0.
    let month = (field(4, 2) - 1).max(0) as u32 + 1;
    let day = field(6, 2).max(1);
    let (hour, min, sec) = (field(8, 2), field(10, 2), field(12, 2));
    if !(1..=12).contains(&month) {
        return None;
    }
    let days = days_from_civil(year, month, day);
    Some(days * 86_400 + hour * 3_600 + min * 60 + sec - local_offset_seconds())
}

/// The inverse of [`civil_from_days`] — Howard Hinnant's `days_from_civil`.
fn days_from_civil(y: i64, m: u32, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = if m > 2 { m - 3 } else { m + 9 } as i64;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// Seconds since the epoch, for `-tn`/`-to`.
fn now_seconds() -> i64 {
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => d.as_secs() as i64,
        // Before 1970 on this machine's clock; the filters then select nothing,
        // which is better than wrapping into the future.
        Err(_) => 0,
    }
}

// ── arc r ───────────────────────────────────────────────────────────────────

/// `runArchiveRecovery` (`ArcRecover.hs:301`).
///
/// Repairs an archive from its own recovery records and writes the result to
/// `fixed.<name>` in the same directory, leaving the damaged file alone. The
/// damaged archive is the only copy of anything unrecoverable, so overwriting
/// it in place would be the one mistake with no way back.
fn recover(path: &std::path::Path, archive_name: &str) -> i32 {
    // `arcname `replaceBaseName` ("fixed."++takeBaseName arcname)` -- the
    // extension is kept and the base name prefixed, so `a.arc` becomes
    // `fixed.a.arc`.
    let fixed = {
        let dir = path.parent().unwrap_or_else(|| std::path::Path::new(""));
        match path.file_name().map(|s| s.to_string_lossy().into_owned()) {
            Some(b) => dir.join(format!("fixed.{b}")),
            None => {
                eprintln!("ERROR: {archive_name} has no file name");
                return 2;
            }
        }
    };
    if fixed.exists() {
        eprintln!("ERROR: file {} already exists", fixed.display());
        return 2;
    }

    let data = match archive::open(path) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("ERROR: {e}");
            return 2;
        }
    };
    // Only the FOOTER is read: a damaged archive may have no readable
    // directory, and repairing it is the point.
    let pw = darc_arc::passwords::Passwords::default();
    let (_, footer) = match archive::read_footer(&data, &pw) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("ERROR: {e}");
            return 2;
        }
    };

    let scan = match darc_arc::recovery::scan(&footer.blocks, &data) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ERROR: archive can't be recovered - {e}");
            return 2;
        }
    };
    let ss = scan.control.sector_size;
    println!(
        "{} recovery sectors ({}) present",
        show3(scan.control.rec_sectors),
        show_memory(scan.control.rec_sectors * ss)
    );
    if scan.bad.is_empty() {
        println!("Archive ok, no need to restore it!");
        return 0;
    }

    let (recoverable, lost) =
        darc_arc::recovery::partition_bad(&scan.bad, scan.control.rec_sectors);
    if recoverable.is_empty() {
        eprintln!(
            "ERROR: {} unrecoverable errors ({}) found, can't restore anything!",
            show3(lost.len() as u64),
            show_memory(lost.len() as u64 * ss)
        );
        return 2;
    }
    print!(
        "{} recoverable errors ({}) ",
        show3(recoverable.len() as u64),
        show_memory(recoverable.len() as u64 * ss)
    );
    if !lost.is_empty() {
        print!(
            "and {} unrecoverable errors ({}) ",
            show3(lost.len() as u64),
            show_memory(lost.len() as u64 * ss)
        );
    }
    println!("found");

    let (out, still_bad) = match darc_arc::recovery::recover(&scan, &data) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("ERROR: {e}");
            return 2;
        }
    };
    match std::fs::write(&fixed, &out) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("ERROR: {}: {e}", fixed.display());
            return 2;
        }
    }
    println!("Recovered archive saved to {}", fixed.display());
    if !still_bad.is_empty() {
        eprintln!(
            "WARNING: {} errors ({}) remain unrecovered",
            show3(still_bad.len() as u64),
            show_memory(still_bad.len() as u64 * ss)
        );
    }
    println!("All OK");
    0
}

/// `showMemory` (`Compression.hs:590`), which is `showM` over
/// `[(gb," gbytes"),(mb," mbytes"),(kb," kbytes"),(b," bytes")]`.
///
/// Not the same spelling as `showMem`: this one has a SPACE and the long unit
/// names, so 1024 is `"1 kbytes"` and not `"1kb"`. Both exist and both are
/// used; the recovery messages use this one.
///
/// A unit is taken when it divides exactly OR when the next unit down would
/// need four thousand of itself, and the displayed number is rounded rather
/// than truncated: `show ((mem + val/2) div val)`.
fn show_memory(bytes: u64) -> String {
    const KB: u64 = 1024;
    if bytes == 0 {
        return "0 bytes".to_string();
    }
    let units: [(u64, &str); 4] = [
        (KB * KB * KB, " gbytes"),
        (KB * KB, " mbytes"),
        (KB, " kbytes"),
        (1, " bytes"),
    ];
    for (i, (val, name)) in units.iter().enumerate() {
        // The final unit is 1, which always divides, so the loop always stops.
        let next = units.get(i + 1).map(|(v, _)| *v).unwrap_or(1);
        if bytes % val == 0 || bytes / next >= 4096 {
            return format!("{}{}", (bytes + val / 2) / val, name);
        }
    }
    format!("{bytes} bytes")
}
