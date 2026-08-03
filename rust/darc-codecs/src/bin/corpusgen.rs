//! Test corpora for `rust/difftest`, in place of the Python the harnesses used
//! to embed.
//!
//! Every harness under `rust/difftest` built its inputs with a `python3`
//! heredoc — about 3,000 lines of it across 40 scripts. Python is not a
//! dependency this project wants, and it was never declared as one: the
//! harnesses simply assumed it.
//!
//! # These bytes are load-bearing
//!
//! The corpora are what the codec difftests compare the Rust against the C
//! over. A corpus that changes does not fail — it silently tests something
//! else, which is the failure mode this repo is most careful about. So each
//! function here is a literal transcription of the Python it replaces, and each
//! conversion was accepted only on `cmp` against the files the Python wrote.
//!
//! That is also why the odd-looking expressions are kept as they are:
//! `(i * 2654435761 >> 28) & 7` is not a formula worth improving, it is the one
//! that produced the bytes already in use.
//!
//! ```text
//!   corpusgen <name> <outdir>
//! ```

use std::io::Write;

/// The LCG every harness's corpus is built from:
///
/// ```python
/// def prng(seed,n):
///     s=seed; o=bytearray()
///     for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
///     return bytes(o)
/// ```
///
/// Note the order: the state is advanced BEFORE the first byte is taken, so
/// `prng(seed, 1)` is not `seed >> 16`. Getting that backwards shifts every
/// corpus by one byte and still looks like plausible noise.
fn prng(seed: u32, n: usize) -> Vec<u8> {
    let mut s = seed;
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        s = s.wrapping_mul(1103515245).wrapping_add(12345);
        out.push((s >> 16) as u8);
    }
    out
}

/// `b"..." * n`.
fn repeat(pattern: &[u8], n: usize) -> Vec<u8> {
    pattern.repeat(n)
}

fn write(dir: &std::path::Path, name: &str, bytes: &[u8]) {
    let path = dir.join(name);
    let mut f = std::fs::File::create(&path)
        .unwrap_or_else(|e| panic!("create {}: {e}", path.display()));
    f.write_all(bytes).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

const FOX: &[u8] = b"the quick brown fox jumps over the lazy dog. ";
const ENGLISH: &[u8] = b"compression algorithms rearrange data so that \
                         statistical redundancy can be removed by an entropy coder. ";

/// `b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(n))`
fn runs(n: usize) -> Vec<u8> {
    let mut out = Vec::new();
    for i in 0..n {
        out.extend(std::iter::repeat_n((i % 97) as u8, 1 + (i * 7) % 200));
    }
    out
}

/// `bytes((0 if (i*2654435761>>28)&7 else (i%251)) for i in range(n))`
///
/// Python's ints are unbounded, so `i*2654435761` does NOT wrap at 32 bits
/// before the shift. `u64` reproduces it; doing this in `u32` gives a different
/// file that still looks like plausible skewed data.
fn skew(n: u64) -> Vec<u8> {
    (0..n)
        .map(|i| match (i.wrapping_mul(2654435761) >> 28) & 7 {
            0 => (i % 251) as u8,
            _ => 0,
        })
        .collect()
}

/// `bytes(i%256 for i in range(n))`
fn alphabet(n: usize) -> Vec<u8> {
    (0..n).map(|i| (i % 256) as u8).collect()
}

/// `b"".join((b"\x00"*300 + bytes([i%251])) for i in range(n))`
fn sparse(n: usize) -> Vec<u8> {
    let mut out = Vec::new();
    for i in 0..n {
        out.extend(std::iter::repeat_n(0u8, 300));
        out.push((i % 251) as u8);
    }
    out
}

/// `bsc-check.sh`.
fn bsc(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 3000));
    write(dir, "english", &repeat(ENGLISH, 900));
    write(dir, "runs", &runs(2000));
    write(dir, "onebyte", &vec![0x5a; 80000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 40000));
    write(dir, "noise", &prng(9, 200000));
    write(dir, "skew", &skew(150000));
    write(dir, "alphabet", &alphabet(200000));
    write(dir, "sparse", &sparse(500));
    for n in [1usize, 2, 3, 16, 17, 255, 256, 257, 4096, 65537] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `bsc-full-check.sh` — a different corpus, not a repeat-count variant of the
/// one above: it adds the small and multi-sub-block cases the coder only
/// segments for past 262144 and 1048576 bytes.
fn bsc_full(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 4000));
    write(dir, "text_sm", &repeat(FOX, 200));
    write(dir, "english", &repeat(ENGLISH, 900));
    write(dir, "runs", &runs(3000));
    write(dir, "onebyte", &vec![0x5a; 80000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 45000));
    write(dir, "noise", &prng(9, 200000));
    write(dir, "alphabet", &alphabet(200000));
    write(dir, "sparse", &sparse(500));
    write(dir, "skew", &skew(150000));
    write(dir, "big_text", &repeat(FOX, 12000));
    write(dir, "big_eng", &repeat(ENGLISH, 5000));
    write(dir, "big_noise", &prng(11, 400000));
    write(dir, "big_skew", &skew(400000));
    for n in [1usize, 2, 3, 16, 255, 256, 257, 4096, 65535, 65536, 65537] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `int(30000*math.sin(i/50.0)) >> (8*(i%2)) & 0xff` — a 16-bit sine, emitted
/// little-endian one byte at a time, which is what `mm-reorder-check.sh` feeds
/// MM's `:r1` transpose.
///
/// Python's `int()` truncates toward zero and its `>>` on a NEGATIVE value is
/// an arithmetic shift of an unbounded two's-complement integer. `as i64` plus
/// `>>` matches both; casting to u8 last is what takes the `& 0xff`.
fn sine(n: usize) -> Vec<u8> {
    (0..n)
        .map(|i| {
            let v = (30000.0 * (i as f64 / 50.0).sin()) as i64;
            ((v >> (8 * (i % 2))) & 0xff) as u8
        })
        .collect()
}

fn main() {
    let mut args = std::env::args().skip(1);
    let name = args.next().unwrap_or_default();

    // The primitives write to stdout, so a harness can redirect one file
    // without naming a whole corpus.
    let num = |a: Option<String>| -> usize {
        a.unwrap_or_default().parse().unwrap_or_else(|_| {
            eprintln!("corpusgen: expected a number");
            std::process::exit(2);
        })
    };
    match name.as_str() {
        "sine" => {
            let n = num(args.next());
            let out = std::io::stdout();
            out.lock().write_all(&sine(n)).expect("write");
            return;
        }
        "prng" => {
            let seed = num(args.next()) as u32;
            let n = num(args.next());
            let out = std::io::stdout();
            out.lock().write_all(&prng(seed, n)).expect("write");
            return;
        }
        "repeat" => {
            let text = args.next().unwrap_or_default();
            let n = num(args.next());
            let out = std::io::stdout();
            out.lock().write_all(&repeat(text.as_bytes(), n)).expect("write");
            return;
        }
        _ => {}
    }

    let dir = args.next().unwrap_or_default();
    if name.is_empty() || dir.is_empty() {
        eprintln!(
            "usage: corpusgen <name> <outdir>\n       \
             corpusgen sine N | prng SEED N | repeat TEXT N   (to stdout)"
        );
        std::process::exit(2);
    }
    let dir = std::path::PathBuf::from(dir);
    std::fs::create_dir_all(&dir).unwrap_or_else(|e| panic!("mkdir {}: {e}", dir.display()));
    match name.as_str() {
        "bsc" => bsc(&dir),
        "bsc-full" => bsc_full(&dir),
        other => {
            eprintln!("corpusgen: unknown corpus {other:?}");
            std::process::exit(2);
        }
    }
}
