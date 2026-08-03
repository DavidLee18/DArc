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
//!   difftest-util elf-text <obj>        the .text section of an i386 ELF
//! ```

use std::io::{Read, Write};

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

/// The `.text` section of a 32-bit little-endian ELF object, to stdout.
///
/// The dispack harnesses compile a real i386 object and use its machine code as
/// their corpus — synthetic bytes do not have the call/jump density `detect()`
/// keys on. Only the shape the Python read is handled: `e_shoff` at 0x20,
/// `e_shentsize` at 0x2e, `e_shnum` at 0x30, `e_shstrndx` at 0x32, and 24-byte
/// section headers whose 5th and 6th words are offset and size.
///
/// Returns nothing when the file is missing or has no `.text`, which is how the
/// harness detects "no i386 compiler here" and skips the code corpus.
fn elf_text(path: &str) -> Vec<u8> {
    let d = match std::fs::read(path) {
        Ok(d) => d,
        Err(_) => return Vec::new(),
    };
    let u32_at = |o: usize| -> Option<u32> {
        d.get(o..o + 4).map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    };
    let u16_at = |o: usize| -> Option<u16> {
        d.get(o..o + 2).map(|b| u16::from_le_bytes([b[0], b[1]]))
    };
    let (shoff, ent, num, stx) = match (u32_at(0x20), u16_at(0x2e), u16_at(0x30), u16_at(0x32)) {
        (Some(a), Some(b), Some(c), Some(e)) => (a as usize, b as usize, c as usize, e as usize),
        _ => return Vec::new(),
    };
    let sh = |i: usize| -> Option<(u32, u32, u32)> {
        let o = shoff + i * ent;
        Some((u32_at(o)?, u32_at(o + 16)?, u32_at(o + 20)?))
    };
    let strtab = match sh(stx) {
        Some((_, off, _)) => off as usize,
        None => return Vec::new(),
    };
    for i in 0..num {
        let (name, off, size) = match sh(i) {
            Some(t) => t,
            None => continue,
        };
        let start = strtab + name as usize;
        let end = match d[start..].iter().position(|b| *b == 0) {
            Some(n) => start + n,
            None => continue,
        };
        if &d[start..end] == b".text" {
            return d
                .get(off as usize..off as usize + size as usize)
                .map(<[u8]>::to_vec)
                .unwrap_or_default();
        }
    }
    Vec::new()
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
        "elf-text" => {
            let out = std::io::stdout();
            out.lock()
                .write_all(&elf_text(args.get(1).map(String::as_str).unwrap_or("")))
                .expect("write");
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
