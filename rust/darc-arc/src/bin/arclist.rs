//! `arc l`, in Rust — the first end-to-end command of the port.
//!
//! Output is byte-for-byte what the Haskell prints, so it can be diffed against
//! `Tests/arc-ghc l` directly. The formatting is not incidental: `show3` groups
//! digits with '.', "-dir-" stands in for a directory's size, and the column
//! widths are fixed. A listing is a documented interface.
//!
//!     cargo run -p darc-arc --bin arclist -- some.arc

use darc_arc::archive;

/// `show3` (`Utils.hs`) — group digits in threes with '.', e.g. 9006 -> "9.006".
fn show3(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::with_capacity(s.len() + s.len() / 3);
    let bytes = s.as_bytes();
    for (i, c) in bytes.iter().enumerate() {
        if i > 0 && (bytes.len() - i) % 3 == 0 {
            out.push('.');
        }
        out.push(*c as char);
    }
    out
}

/// The Haskell formats mtimes in LOCAL time (`System.Time`'s
/// `toCalendarTime`), so a --nodates archive shows 1970-01-01 plus the local
/// UTC offset rather than midnight. Reproduced rather than corrected: matching
/// the reference is the acceptance bar, and "fixing" it would be a difference.
fn format_time(t: i64) -> String {
    let secs = t + local_offset_seconds();
    let days = secs.div_euclid(86_400);
    let tod = secs.rem_euclid(86_400);
    let (y, m, d) = civil_from_days(days);
    format!(
        "{y:04}-{m:02}-{d:02} {:02}:{:02}:{:02}",
        tod / 3600,
        (tod % 3600) / 60,
        tod % 60
    )
}

/// Howard Hinnant's civil_from_days.
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

/// The local UTC offset, read once from the C library via `localtime_r` — no
/// dependency, and no assumption that the machine runs UTC.
fn local_offset_seconds() -> i64 {
    // SAFETY: localtime_r writes into a tm we own; time 0 is always valid.
    unsafe {
        let t: libc_time_t = 0;
        let mut tm: Tm = std::mem::zeroed();
        localtime_r(&t, &mut tm);
        tm.tm_gmtoff
    }
}

#[allow(non_camel_case_types)]
type libc_time_t = i64;

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
    fn localtime_r(t: *const libc_time_t, tm: *mut Tm) -> *mut Tm;
}

fn compressed_str(n: u64) -> String {
    show3(n)
}

fn main() {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("usage: arclist <archive>");
            std::process::exit(2);
        }
    };
    let info = match archive::read_info(std::path::Path::new(&path)) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(2);
        }
    };

    println!("Date/time                  Size Filename");
    println!("----------------------------------------");
    let mut total = 0u64;
    // `myMapM` (ArcExtract.hs:231): a solid block's packed size is charged to
    // the FIRST file of each contiguous run of files sharing it, and zero to the
    // rest. It is a run-length dedup keyed on blPos, not a set -- a block whose
    // files were not contiguous would be counted twice, and reproducing that is
    // the point.
    let mut compressed = 0u64;
    let mut prev_block_pos: Option<u64> = None;
    for e in &info.entries {
        let size = if e.is_dir { "-dir-".to_string() } else { show3(e.size) };
        println!("{} {:>11} {}", format_time(e.time), size, e.stored_name);
        total += e.size;
        let block = info.data_blocks.get(e.block);
        let (pos, csize) = match block {
            Some(b) => (b.pos, b.comp_size),
            None => (0, 0),
        };
        if prev_block_pos != Some(pos) {
            compressed += csize;
        }
        prev_block_pos = Some(pos);
    }
    println!("----------------------------------------");
    println!(
        "{} files, {} bytes, {} compressed",
        show3(info.entries.len() as u64),
        show3(total),
        compressed_str(compressed)
    );
    // Two newlines, not one. `myPutStr` writes the summary without a newline and
    // the command epilogue supplies both -- so the reference listing ends with a
    // blank line, and a port that "tidies" it away differs from the reference.
    println!("All OK\n");
}
