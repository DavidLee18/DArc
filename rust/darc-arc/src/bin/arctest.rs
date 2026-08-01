//! `arc t`, in Rust.
//!
//! Decompresses every solid block and checks every file's CRC. The two things
//! that matter beyond correctness:
//!
//! * **each solid block is decompressed exactly once**, not once per file. That
//!   is what "solid" means — the files share one stream — and decompressing per
//!   file turns a linear test into a quadratic one.
//! * **blocks are tested in parallel.** They are independent by construction
//!   (`splitToSolidBlocks` is pure and runs before any concurrency), and the
//!   verdicts are reduced in block order so the report cannot depend on which
//!   finished first.
//!
//!     cargo run -p darc-arc --bin arctest -- some.arc

use darc_arc::{archive, decompress};
use rayon::prelude::*;

/// `show3` — group digits in threes with '.'.
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

/// `ratio3` (`UIBase.hs:159`) — INTEGER arithmetic, `count*1000 \`div\` total`,
/// with the decimal point inserted before the last digit.
///
/// Not `format!("{:.1}")`. That rounds; `div` truncates. 33.241 of 438.744 is
/// 7.576%, which the Haskell prints as **7.5** and a rounding formatter prints
/// as 7.6 — one digit, in a line that is compared byte for byte.
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
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("usage: arctest <archive>");
            std::process::exit(2);
        }
    };
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

    let total_bytes: u64 = info.entries.iter().map(|e| e.size).sum();
    println!("Testing {} files, {} bytes.", show3(info.entries.len() as u64), show3(total_bytes));

    // Group the entries by block first, so a block is unpacked once.
    let mut per_block: Vec<Vec<&darc_arc::directory::Entry>> =
        vec![Vec::new(); info.data_blocks.len()];
    for e in &info.entries {
        match per_block.get_mut(e.block) {
            Some(v) => v.push(e),
            None => {}
        }
    }

    // One task per block. `collect` into a Vec keeps block order, so the first
    // failure reported is the first failure in the archive, not the first one a
    // worker happened to reach.
    let results: Vec<Vec<String>> = per_block
        .par_iter()
        .enumerate()
        .map(|(bi, entries)| {
            let mut bad = Vec::new();
            let b = match info.data_blocks.get(bi) {
                Some(b) => b,
                None => return bad,
            };
            let start = b.pos as usize;
            let end = start.saturating_add(b.comp_size as usize);
            let packed = match data.get(start..end) {
                Some(s) => s,
                None => {
                    bad.push(format!("{} is truncated", b.name()));
                    return bad;
                }
            };
            let unpacked = match decompress::decompress_chain(
                &b.compressor,
                packed,
                b.orig_size as usize,
            ) {
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
                match unpacked.get(from..to) {
                    Some(bytes) => {
                        let got = darc_arc::crc::calc(bytes);
                        if got != e.crc {
                            bad.push(format!("{}: CRC failed", e.stored_name));
                        }
                    }
                    None => bad.push(format!("{}: runs past the end of its block", e.stored_name)),
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

    // Blocks whose UNPACKED size is zero are not counted.
    //
    // The Haskell's cbytes comes from actual decompression -- ArcExtract.hs:70
    // hands `uiCompressedBytes` to the decompress process -- and a block with
    // nothing to produce never runs it. `arc lt`, which sums the block table
    // directly, therefore reports MORE than `arc t` on the same archive:
    // 75.984 against 75.965 on a -mtor -s- corpus. The 19-byte gap is exactly
    // the two zero-size blocks in that archive's table (the directories' stored
    // block at 0, and a 19-byte tor block holding one empty file).
    //
    // Two wrong theories died here first: "sum the block table" (off by 19) and
    // "charge each block once per contiguous run of files", the rule the LISTING
    // uses -- which collapses a single-block archive to 0. `arc l` and `arc t`
    // count differently, and neither rule can be guessed from the other.
    let packed: u64 =
        info.data_blocks.iter().filter(|b| b.orig_size > 0).map(|b| b.comp_size).sum();
    println!(
        "Tested {} files, {} => {} bytes. Ratio {}",
        show3(info.entries.len() as u64),
        show3(packed),
        show3(total_bytes),
        format!("{}%", ratio3(packed, total_bytes))
    );
    if failures == 0 {
        println!("All OK");
    } else {
        // The reference exits 2 on a failed command; the MicroHs build's exit 1
        // is one of the divergences docs/testing.md records.
        eprintln!("{failures} error(s)");
        std::process::exit(2);
    }
}
