//! The small pure functions the difftest harnesses used `python3` one-liners
//! for.
//!
//! Separate from `corpusgen` on purpose: that one produces test INPUTS, these
//! compute answers ABOUT bytes. Both exist because the harnesses had an
//! undeclared Python dependency, and both keep the orchestration in shell,
//! where the clang builds and the external binaries live.
//!
//! ```text
//!   difftest-util lzma2-blocks <file>     count LZMA2 dictionary resets
//!   difftest-util genhex key|iv N EXTRA   the crypto harness's key/IV material
//!   difftest-util all-zeros <file> [skip] 1 if the rest of the file is zero
//! ```

use std::io::Read;

/// Count LZMA2 dictionary resets, which is exactly one per block.
///
/// `Lzma2Enc.c:106-111` sets `needInitProp` and `needInitState` together at
/// every block start, so a block's first chunk is either mode 3 (control
/// >= 0xE0) or a `COPY_RESET_DIC` (control == 1).
///
/// Chunk layout (`Lzma2Enc.c:197-225` and `:168-191`):
///
/// ```text
///   control 0x00      end of stream
///   control >= 0x80   LZMA: 5 header bytes, +1 prop byte when control >= 0xC0,
///                           then packSize payload bytes
///   control 1 or 2    copy: 3 header bytes then unpackSize raw bytes
/// ```
///
/// Both sizes are stored minus one. Byte 0 of the stream is the LZMA2 property
/// byte, so the walk starts at 1.
fn lzma2_blocks(b: &[u8]) -> usize {
    let (mut i, mut n) = (1usize, 0usize);
    while i < b.len() {
        let c = b[i];
        if c == 0 {
            break;
        }
        if c >= 0x80 {
            if i + 5 > b.len() {
                break;
            }
            let pack = (((b[i + 3] as usize) << 8) | b[i + 4] as usize) + 1;
            i += 5 + usize::from(c >= 0xC0) + pack;
            if c >= 0xE0 {
                n += 1;
            }
        } else if c == 1 || c == 2 {
            if i + 3 > b.len() {
                break;
            }
            let unpack = (((b[i + 1] as usize) << 8) | b[i + 2] as usize) + 1;
            i += 3 + unpack;
            if c == 1 {
                n += 1;
            }
        } else {
            break;
        }
    }
    n
}

/// The crypto harness's deterministic key and IV material, as hex.
///
/// The IV's two low bytes are `0xff` deliberately: the little-endian counter
/// then carries out of byte 0 on the very first increment and out of byte 1
/// soon after, which is the case a CTR implementation gets wrong.
fn genhex(kind: &str, n: usize, extra: i64) -> String {
    let bytes: Vec<u8> = match kind {
        "key" => (0..n).map(|i| ((0x5a + 7 * i as i64 + extra) & 0xff) as u8).collect(),
        _ => (0..n)
            .map(|i| match i {
                0 | 1 => 0xff,
                _ => ((0xa3 + 11 * i as i64) & 0xff) as u8,
            })
            .collect(),
    };
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn read_file(path: &str) -> Vec<u8> {
    let mut v = Vec::new();
    std::fs::File::open(path)
        .unwrap_or_else(|e| panic!("open {path}: {e}"))
        .read_to_end(&mut v)
        .unwrap_or_else(|e| panic!("read {path}: {e}"));
    v
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cmd = args.first().map(String::as_str).unwrap_or("");
    match cmd {
        "lzma2-blocks" => {
            let b = read_file(args.get(1).map(String::as_str).unwrap_or(""));
            println!("{}", lzma2_blocks(&b));
        }
        "genhex" => {
            let kind = args.get(1).map(String::as_str).unwrap_or("key");
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(0);
            let extra: i64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(0);
            println!("{}", genhex(kind, n, extra));
        }
        // `d = read()[skip:]; print(int(d == bytes(len(d))))` -- 1 when every
        // remaining byte is zero.
        "all-zeros" => {
            let b = read_file(args.get(1).map(String::as_str).unwrap_or(""));
            let skip: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(0);
            let rest = b.get(skip..).unwrap_or(&[]);
            println!("{}", i32::from(rest.iter().all(|x| *x == 0)));
        }
        other => {
            eprintln!("difftest-util: unknown command {other:?}");
            eprintln!("  lzma2-blocks <file> | genhex key|iv N EXTRA | all-zeros <file> [skip]");
            std::process::exit(2);
        }
    }
}
