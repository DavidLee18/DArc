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
    let archive_name = match parsed.free.first() {
        Some(a) => a.clone(),
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

    let code = match command.as_str() {
        "l" | "v" | "lb" | "lt" => list(&command, &open_existing()),
        "t" | "x" | "e" => {
            let info = open_existing();
            let data = match archive::open(path) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    std::process::exit(2);
                }
            };
            let extracting = command != "t";
            run_blocks(&info, &data, &layout, extracting, &pw)
        }
        "a" | "u" | "f" | "d" => add(&command, &archive_name, &parsed, &pw),
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
        "c" | "m" | "j" | "ch" | "rr" | "k" => {
            eprintln!("ERROR: command {command:?} is not implemented in this port yet");
            2
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
fn add(
    command: &str,
    archive_name: &str,
    parsed: &options::Parsed,
    pw: &darc_arc::passwords::Passwords,
) -> i32 {
    // opt_update_type (Cmdline.hs). --sync is not offered: it deletes, and the
    // deletion path is not exercised by any harness here yet.
    let update_type = match command {
        "u" => darc_arc::joinlist::UpdateType::Update,
        "f" => darc_arc::joinlist::UpdateType::Freshen,
        _ => darc_arc::joinlist::UpdateType::Add,
    };
    // The subset of builtinMethodSubsts (Compression.hs:428) this port can
    // write. Each maps a -m level to its unfitted chain; the data-size fitting
    // happens once the block's contents are known.
    // decode_method expands a -m level into one chain per file type.
    let method = parsed.arg("method", "");
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
    // parseSolidOption (Cmdline.hs:757). Only the two forms this port needs:
    // "-s-" is [GroupNone], one solid block per file; the default and a bare
    // "-s" are the empty criteria list, which splits nothing.
    let per_file = parsed.arg("solid", "") == "-";

    // sort_order (Cmdline.hs:617): "" when the main compressor is
    // aNO_COMPRESSION, and ALSO "" when group_data is [GroupNone] -- there is
    // nothing to gain from ordering files that do not share a block. -m0 is the
    // one level where the first clause applies, which is why it packs in scan
    // order.
    let sort_order = if chain == "storing" || per_file { "" } else { "gerpn" };
    let recursive = parsed.flag("recursive");
    let nodates = parsed.flag("nodates");
    // `runDelete = runArchiveAdd . setArcFilter ((not.) . fullFileFilter)`
    // (Arc.hs:213): `d` is `a` with NO disk files and an archive filter that
    // keeps everything the filespecs do NOT match.
    let deleting = command == "d";

    // Everything after the archive name is a filespec.
    let specs: Vec<String> = parsed.free.iter().skip(1).cloned().collect();
    let specs = if specs.is_empty() { vec![".".to_string()] } else { specs };

    // Names are stored WITH the filespec as the user wrote it: `arc a x.arc .`
    // stores "./a.txt" and the directory name ".", not "a.txt" and "".
    // remove_unsafe_dirs (Files.hs:143) strips the "." again on READ, so both
    // list identically -- but the stored bytes differ, and this is a
    // format-compatibility port. Measured: the reference's directory block is
    // exactly 3 bytes longer than one built without the prefix, which is
    // ".\0" plus "./" on the one subdirectory name.
    let mut found: Vec<(String, std::path::PathBuf, bool)> = Vec::new();
    for spec in if deleting { &[][..] } else { &specs[..] } {
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

    // For u/f: read what is already in the archive and merge. By DEFAULT the
    // kept files are recompressed rather than copied -- splitToSolidBlocks only
    // preserves existing solid blocks under --keep-original
    // (ArhiveFileList.hs:297) -- so the merged list is packed exactly as a
    // fresh one would be.
    if (deleting || update_type != darc_arc::joinlist::UpdateType::Add)
        && std::path::Path::new(archive_name).exists()
    {
        let info = match archive::read_info(std::path::Path::new(archive_name), pw) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("ERROR: {e}");
                return 2;
            }
        };
        let data = match archive::open(std::path::Path::new(archive_name)) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("ERROR: {e}");
                return 2;
            }
        };
        let full_names = parsed.flag("fullnames");
        let main: Vec<darc_arc::joinlist::Candidate> = info
            .entries
            .iter()
            // The archive filter. For `d` it is the NEGATION of the filespec
            // match; for u/f the filespecs select disk files, not archive ones,
            // so everything is kept.
            .filter(|e| {
                !deleting
                    || !darc_arc::sort::match_filespecs(&specs, &e.stored_name, full_names)
            })
            .map(|e| darc_arc::joinlist::Candidate {
                entry: e.clone(),
                origin: darc_arc::joinlist::Origin::Archive,
            })
            .collect();
        let mut added: Vec<darc_arc::joinlist::Candidate> = Vec::new();
        for e in dir_entries.iter().chain(file_entries.iter()) {
            added.push(darc_arc::joinlist::Candidate {
                entry: e.clone(),
                origin: darc_arc::joinlist::Origin::Disk,
            });
        }
        let merged = darc_arc::joinlist::join_lists(
            &main,
            &added,
            update_type,
            parsed.flag("append"),
            // The merge order only matters when a sort order is in effect, and
            // the files are re-sorted below in any case.
            "",
            |a, b| {
                let mut v = a.to_vec();
                v.extend_from_slice(b);
                v
            },
        );
        // Pull the bytes of everything that came from the archive.
        dir_entries.clear();
        file_entries.clear();
        for c in merged {
            if c.origin == darc_arc::joinlist::Origin::Archive && !c.entry.is_dir {
                match archive::read_entry(&data, &info, &c.entry, pw) {
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

    // sortFiles: the solid sort order decides the layout of the block, and so
    // the archive's bytes.
    let groups = load_groups(parsed);
    file_entries = darc_arc::sort::sort_files(sort_order, &groups, &file_entries);
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
    let one_block_per_file = per_file && chain != "storing";

    // The encryption algorithm, canonicalised the way Cmdline.hs:529 does it
    // before it can reach a block: `-ae aes` becomes `aes-256/ctr:n1000:r0`.
    let mut algorithm = Vec::new();
    if !pw.data.is_empty() || !pw.headers.is_empty() {
        for part in parsed.arg("encryption", "aes").split('+') {
            match darc_arc::encryption::canonize(part) {
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

    if one_block_per_file {
        // splitToSolidBlocks runs splitFileTypes FIRST and only then splits each
        // type's files by the grouping, so a file keeps ITS OWN type's chain
        // even when every file is its own block. Using the main chain for all of
        // them was wrong by 1.500 to 3.100 bytes on the four multi-type levels.
        let mut reordered: Vec<Entry> = Vec::new();
        for (ty, group) in &type_groups {
            let chain = &chains[*ty];
            for &i in group {
                let mut e = file_entries[i].clone();
                let body = contents.get(&e.stored_name).cloned().unwrap_or_default();
                e.block = data_blocks.len();
                e.pos_in_block = 0;
                let fitted = match darc_arc::memlimit::fit_for_add(chain, body.len() as u64) {
                    Some(f) => f,
                    None => {
                        eprintln!("ERROR: cannot fit {chain} to {} bytes", body.len());
                        return 2;
                    }
                };
                let compressor: Vec<String> = fitted.split('+').map(str::to_string).collect();
                match w.write_compressed_data(&body, compressor, 1) {
                    Ok(b) => data_blocks.push(b),
                    Err(err) => {
                        eprintln!("ERROR: {err}");
                        return 2;
                    }
                }
                reordered.push(e);
            }
        }
        file_entries = reordered;
    } else {
        // One block per file TYPE, in the order merge_by_type produced. The
        // entries are reordered to match, because the directory stores block
        // membership as a run length over the file list.
        let mut reordered: Vec<Entry> = Vec::new();
        for (ty, group) in &type_groups {
            let mut body = Vec::new();
            for &i in group {
                let e = &file_entries[i];
                let mut e = e.clone();
                e.block = data_blocks.len();
                e.pos_in_block = body.len() as u64;
                match contents.get(&e.stored_name) {
                    Some(b) => body.extend_from_slice(b),
                    None => {}
                }
                reordered.push(e);
            }
            let chain = &chains[*ty];
            let fitted = match darc_arc::memlimit::fit_for_add(chain, body.len() as u64) {
                Some(f) => f,
                None => {
                    eprintln!("ERROR: cannot fit {chain} to {} bytes", body.len());
                    return 2;
                }
            };
            let compressor: Vec<String> = fitted.split('+').map(str::to_string).collect();
            match w.write_compressed_data(&body, compressor, group.len()) {
                Ok(b) => data_blocks.push(b),
                Err(e) => {
                    eprintln!("ERROR: {e}");
                    return 2;
                }
            }
        }
        file_entries = reordered;
    }

    let mut entries = dir_entries;
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

    let bytes = w.finish("", "", false);

    match std::fs::write(archive_name, &bytes) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("ERROR: {archive_name}: {e}");
            return 2;
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
fn list(command: &str, info: &archive::ArchiveInfo) -> i32 {
    if command == "lb" {
        // `myPutStr$ joinWith "\n"$ map filename directory` -- names, joined.
        // Two things measured rather than read off: `lb` prints NO banner, the
        // only listing command that does not, and the output DOES end with a
        // newline even though myPutStr writes none.
        let names: Vec<&str> =
            info.entries.iter().map(|e| e.stored_name.as_str()).collect();
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
        let total: u64 = info.entries.iter().map(|e| e.size).sum();
        // `lt` sums the block table directly, unlike `l` and `v`.
        let packed: u64 = info.data_blocks.iter().map(|b| b.comp_size).sum();
        println!(
            "{} files, {} bytes, {} compressed",
            show3(info.entries.len() as u64),
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
    for e in &info.entries {
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
        show3(info.entries.len() as u64),
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
) -> i32 {
    // The safety check runs on the archive's contribution alone: the
    // destination is the user's own and may be absolute.
    let relative = Layout { disk_basedir: String::new(), ..layout.clone() };

    if extracting && layout.creates_directories() {
        for e in info.entries.iter().filter(|e| e.is_dir) {
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

    let total_bytes: u64 = info.entries.iter().map(|e| e.size).sum();
    if !extracting {
        println!(
            "Testing {} files, {} bytes.",
            show3(info.entries.len() as u64),
            show3(total_bytes)
        );
    }

    let mut per_block: Vec<Vec<&Entry>> = vec![Vec::new(); info.data_blocks.len()];
    for e in &info.entries {
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
            show3(info.entries.len() as u64),
            show3(packed),
            show3(total_bytes),
            ratio3(packed, total_bytes)
        );
    } else {
        println!(
            "Extracted {} files, {} bytes.",
            show3(info.entries.len() as u64),
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
