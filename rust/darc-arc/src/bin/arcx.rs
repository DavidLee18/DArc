//! `arc x` and `arc e`, in Rust.
//!
//!     arcx [-e] <archive> [destination]
//!
//! `-e` selects the flat `e` layout (`-ep0`); the default is `x`.
//!
//! Each solid block is decompressed once and its files written from that one
//! buffer, with blocks running in parallel. Files within a block are written in
//! stream order, which is also the order they were laid down.

use darc_arc::{archive, decompress, extract::Layout};
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

fn main() {
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    let flat = args.first().map(String::as_str) == Some("-e");
    if flat {
        args.remove(0);
    }
    let path = match args.first() {
        Some(p) => p.clone(),
        None => {
            eprintln!("usage: arcx [-e] <archive> [destination]");
            std::process::exit(2);
        }
    };
    let dest = args.get(1).cloned().unwrap_or_else(|| ".".to_string());

    let base = if flat { Layout::flat() } else { Layout::default() };
    // Two layouts over the same options: one that produces the path to write,
    // and one WITHOUT the destination, which is what the safety check runs on.
    // The destination is the user's own choice and may legitimately be
    // absolute; anything the archive contributes may not escape it.
    let layout = Layout { disk_basedir: dest.clone(), ..base.clone() };
    let relative = base;

    let p = std::path::Path::new(&path);
    let info = match archive::read_info(p) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(2);
        }
    };
    let data = match archive::open(p) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(2);
        }
    };

    // Directories first, and serially. They are cheap, and creating them up
    // front means the parallel file writes below never race to create the same
    // parent -- which is a real race, not a theoretical one: two files in the
    // same new directory are usually in the same block, but with -s- they are
    // not.
    for e in &info.entries {
        if !e.is_dir || !layout.creates_directories() {
            continue;
        }
        if !darc_arc::extract::is_safe(&relative.disk_name(e)) {
            eprintln!("ERROR: refusing unsafe path {:?}", e.stored_name);
            std::process::exit(2);
        }
        let name = layout.disk_name(e);
        match std::fs::create_dir_all(&name) {
            Ok(()) => {}
            Err(err) => {
                eprintln!("ERROR: cannot create directory {name}: {err}");
                std::process::exit(2);
            }
        }
    }

    let mut per_block: Vec<Vec<&darc_arc::directory::Entry>> =
        vec![Vec::new(); info.data_blocks.len()];
    for e in &info.entries {
        match per_block.get_mut(e.block) {
            Some(v) => v.push(e),
            None => {}
        }
    }

    let results: Vec<Result<u64, String>> = per_block
        .par_iter()
        .enumerate()
        .map(|(bi, entries)| {
            let b = match info.data_blocks.get(bi) {
                Some(b) => b,
                None => return Ok(0),
            };
            if entries.iter().all(|e| e.is_dir) {
                return Ok(0);
            }
            let start = b.pos as usize;
            let end = start.saturating_add(b.comp_size as usize);
            let packed = match data.get(start..end) {
                Some(s) => s,
                None => return Err(format!("{} is truncated", b.name())),
            };
            let unpacked =
                decompress::decompress_chain(&b.compressor, packed, b.orig_size as usize)
                    .map_err(|e| format!("{}: {e}", b.name()))?;

            let mut written = 0u64;
            for e in entries {
                if e.is_dir {
                    continue;
                }
                let from = e.pos_in_block as usize;
                let to = from.saturating_add(e.size as usize);
                let bytes = match unpacked.get(from..to) {
                    Some(x) => x,
                    None => {
                        return Err(format!("{}: runs past the end of its block", e.stored_name))
                    }
                };
                // The CRC is checked BEFORE the bytes reach the disk. The
                // Haskell writes first and removes the file afterwards unless
                // -kb; refusing up front is strictly safer and, on a good
                // archive, indistinguishable.
                let got = darc_arc::crc::calc(bytes);
                if got != e.crc {
                    return Err(format!("{}: CRC failed", e.stored_name));
                }
                if !darc_arc::extract::is_safe(&relative.disk_name(e)) {
                    return Err(format!("refusing unsafe path {:?}", e.stored_name));
                }
                let name = layout.disk_name(e);
                match std::path::Path::new(&name).parent() {
                    Some(dir) => {
                        std::fs::create_dir_all(dir)
                            .map_err(|err| format!("{}: {err}", dir.display()))?;
                    }
                    None => {}
                }
                let mut f = std::fs::File::create(&name)
                    .map_err(|err| format!("{name}: {err}"))?;
                f.write_all(bytes).map_err(|err| format!("{name}: {err}"))?;
                written += e.size;
            }
            Ok(written)
        })
        .collect();

    let mut total = 0u64;
    let mut failures = 0usize;
    for r in results {
        match r {
            Ok(n) => total += n,
            Err(msg) => {
                eprintln!("ERROR: {msg}");
                failures += 1;
            }
        }
    }

    println!(
        "Extracted {} files, {} bytes.",
        show3(info.entries.len() as u64),
        show3(total)
    );
    if failures > 0 {
        std::process::exit(2);
    }
    println!("All OK");
}

