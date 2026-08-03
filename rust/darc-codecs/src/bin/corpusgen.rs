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

/// `b"".join(bytes([i%m])*len for i in range(n))` — runs of a FIXED length,
/// unlike [`runs`] whose length varies with the index.
fn runs_fixed(n: usize, m: usize, len: usize) -> Vec<u8> {
    let mut out = Vec::new();
    for i in 0..n {
        out.extend(std::iter::repeat_n((i % m) as u8, len));
    }
    out
}

/// `bytes(sorted(prng(seed, n)))` — the same bytes, ascending. A sorted input
/// is the worst case for a suffix sorter, which is why it is in the corpus.
fn sorted_prng(seed: u32, n: usize) -> Vec<u8> {
    let mut v = prng(seed, n);
    v.sort_unstable();
    v
}

/// `bytes(range(256))*reps`.
fn alphabet_full(reps: usize) -> Vec<u8> {
    let one: Vec<u8> = (0..=255u8).collect();
    one.repeat(reps)
}

/// `(pattern*reps)[:n]` — a repeat cut to an exact length.
fn truncated_repeat(pattern: &[u8], reps: usize, n: usize) -> Vec<u8> {
    let mut v = pattern.repeat(reps);
    v.truncate(n);
    v
}

/// The Fibonacci word: `f(0)="a"`, `f(1)="ab"`, `f(k)=f(k-1)+f(k-2)`, at k=24.
///
/// Its LMS substrings collide maximally, which is what forces SA-IS to recurse
/// instead of resolving names in a single pass.
fn fibonacci_word(k: usize) -> Vec<u8> {
    let (mut a, mut b): (Vec<u8>, Vec<u8>) = (b"a".to_vec(), b"ab".to_vec());
    if k == 0 {
        return a;
    }
    for _ in 2..=k {
        let next = [b.clone(), a].concat();
        a = b;
        b = next;
    }
    b
}

/// `bsc-bwt-check.sh` — the inverse BWT.
fn bsc_bwt(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 4000));
    write(dir, "text_sm", &repeat(FOX, 200));
    write(dir, "runs", &runs(3000));
    write(dir, "onebyte", &vec![0x5a; 80000]);
    write(dir, "onebyte_s", &vec![0x5a; 5000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 45000));
    write(dir, "noise", &prng(9, 200000));
    write(dir, "noise_sm", &prng(9, 4000));
    write(dir, "alphabet", &alphabet(200000));
    write(dir, "sparse", &sparse(500));
    write(dir, "skew", &skew(150000));
    for n in [2usize, 3, 16, 255, 256, 257, 4095, 4096, 65535, 65536, 65537, 131072] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `bsc-bwt-encode-check.sh` — the forward BWT, against libsais.
fn bsc_bwt_encode(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 2000));
    write(dir, "runs", &runs_fixed(400, 251, 200));
    write(dir, "longruns", &runs_fixed(60, 7, 5000));
    write(dir, "noise", &prng(7, 150000));
    write(dir, "zeros", &vec![0u8; 80000]);
    write(dir, "one_byte", &vec![b'Q'; 40000]);
    write(dir, "sorted", &sorted_prng(11, 120000));
    write(dir, "full_alpha", &alphabet_full(300));
    let mut ends_zero = prng(3, 50000);
    ends_zero.pop();
    ends_zero.push(0);
    write(dir, "ends_zero", &ends_zero);
    write(dir, "periodic3", &repeat(b"abc", 30000));
    write(dir, "periodic2", &repeat(b"ab", 45000));
    write(dir, "fibonacci", &fibonacci_word(24));
    write(dir, "almost_per", &repeat(b"abcabcabcabd", 8000));
    // `bytes((i*i)%2 for i in range(100000))` -- i*i is even iff i is.
    write(dir, "two_symbols", &(0..100000u64).map(|i| ((i * i) % 2) as u8).collect::<Vec<u8>>());
    write(dir, "bwt_like", &sorted_prng(5, 90000));
    for n in [2usize, 3, 4, 5, 17, 255, 256, 257, 1000, 65535, 65536] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abracadabra", 10000, n));
    }
}

/// `bsc-st-check.sh` — the inverse ST.
fn bsc_st(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 3000));
    write(dir, "english", &repeat(ENGLISH, 900));
    write(dir, "runs", &runs(2000));
    write(dir, "onebyte", &vec![0x5a; 80000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 40000));
    write(dir, "noise", &prng(9, 200000));
    write(dir, "alphabet", &alphabet(200000));
    write(dir, "sparse", &sparse(500));
    write(dir, "skew", &skew(150000));
    for n in [2usize, 3, 16, 255, 256, 257, 4096, 65537] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `bsc-st-encode-check.sh` — the forward ST.
fn bsc_st_encode(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 2000));
    write(dir, "noise", &prng(7, 120000));
    write(dir, "zeros", &vec![0u8; 60000]);
    write(dir, "runs", &runs_fixed(400, 251, 150));
    write(dir, "sorted", &sorted_prng(11, 90000));
    write(dir, "alpha", &alphabet_full(300));
    for n in [2usize, 3, 4, 17, 255, 256, 65537] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abracadabra", 10000, n));
    }
}

/// `bsc-lzp-encode-check.sh` — the LZP match finder.
fn bsc_lzp_encode(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 4000));
    write(dir, "runs", &runs_fixed(600, 251, 300));
    write(dir, "noise", &prng(7, 300000));
    write(dir, "zeros", &vec![0u8; 200000]);
    // Near-repeats: a long block repeated with one byte changed each time, so a
    // match starts, runs and dies. A corpus of exact repeats never reaches the
    // `heuristic` short-circuit this is for.
    let base = prng(11, 4096);
    let mut near = Vec::new();
    for i in 0..80usize {
        let cut = i % 4000;
        near.extend_from_slice(&base[..cut]);
        near.push(((i * 7) & 0xff) as u8);
        near.extend_from_slice(&base[cut..]);
    }
    write(dir, "near", &near);
    // Matches far longer than 254, so the length continuation bytes are emitted.
    let mut longmatch_unit = vec![b'A'; 100000];
    longmatch_unit.extend(prng(3, 64));
    write(dir, "longmatch", &longmatch_unit.repeat(3));
    // Literal flag bytes (0xF2) in otherwise compressible data: each is escaped.
    write(
        dir,
        "flagbyte",
        &(0..150000usize)
            .map(|i| match i % 13 {
                0 => 0xF2,
                _ => ((i * 5) & 0xff) as u8,
            })
            .collect::<Vec<u8>>(),
    );
    write(dir, "flagtext", &repeat(b"lorem ipsum dolor \xf2 sit amet ", 6000));
    for n in [0usize, 1, 32, 33, 64, 4096, 65536, 65537] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abcdefgh", 20000, n));
    }
}

/// `bsc-qlfc-encode-check.sh`, main corpus.
fn bsc_qlfc_encode(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 2000));
    write(dir, "runs", &runs_fixed(400, 251, 200));
    write(dir, "longruns", &runs_fixed(60, 7, 5000));
    write(dir, "noise", &prng(7, 150000));
    write(dir, "zeros", &vec![0u8; 80000]);
    write(dir, "one_byte", &vec![b'Q'; 40000]);
    write(dir, "sorted", &sorted_prng(11, 120000));
    write(dir, "full_alpha", &alphabet_full(300));
    let mut ends_zero = prng(3, 50000);
    ends_zero.pop();
    ends_zero.push(0);
    write(dir, "ends_zero", &ends_zero);
    for n in [256usize, 1024, 65536] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abracadabra", 10000, n));
    }
}

/// The second, LARGE corpus of `bsc-qlfc-encode-check.sh`.
///
/// Sized to force 2 and 4 blocks: `bsc_coder_compress`'s splitter only runs
/// above `2*2*65536` bytes, and without these every case takes the
/// single-block shortcut and the splitter is never executed at all.
fn bsc_qlfc_encode_big(dir: &std::path::Path) {
    let mut two = repeat(b"the quick brown fox ", 20000);
    two.extend(prng(5, 60000));
    write(dir, "two", &two.repeat(2));
    write(dir, "four", &sorted_prng(3, 1200000));
}

/// `bsc-qlfc-transform-check.sh`.
fn bsc_qlfc_transform(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 3000));
    write(dir, "runs", &runs_fixed(500, 251, 200));
    write(dir, "noise", &prng(7, 200000));
    write(dir, "zeros", &vec![0u8; 100000]);
    write(dir, "one_byte", &vec![b'Q'; 50000]);
    let mut ends_zero = prng(3, 60000);
    ends_zero.pop();
    ends_zero.push(0);
    write(dir, "ends_zero", &ends_zero);
    // `b"".join(b"\x00"*50 + bytes([i%255+1])*7 for i in range(2000))`
    let mut azr = Vec::new();
    for i in 0..2000usize {
        azr.extend(std::iter::repeat_n(0u8, 50));
        azr.extend(std::iter::repeat_n(((i % 255) + 1) as u8, 7));
    }
    write(dir, "all_zero_runs", &azr);
    write(dir, "full_alphabet", &alphabet_full(400));
    // 255 distinct values, one short of the alphabet: the preamble's
    // terminating repeat depends on an unused entry existing.
    write(
        dir,
        "alphabet_255",
        &(0..120000usize).map(|i| ((i % 255) + 1) as u8).collect::<Vec<u8>>(),
    );
    write(dir, "bwt_like", &sorted_prng(11, 150000));
    for n in [1usize, 2, 3, 4, 17, 255, 256, 257, 65536] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abracadabra", 10000, n));
    }
}

/// `4x4-check.sh`.
fn fourx4(dir: &std::path::Path) {
    // Big enough to span SEVERAL blocks at the sizes the harness sweeps:
    // single-block input never exercises the framing this exists to test.
    write(dir, "text", &repeat(FOX, 20000));
    write(
        dir,
        "english",
        &repeat(
            b"compression algorithms rearrange data so that statistical \
              redundancy can be removed by an entropy coder. ",
            8000,
        ),
    );
    // `b"".join((b"chunk-%d-" % i) + prng(i, 300) for i in range(2000))`
    let mut mixed = Vec::new();
    for i in 0..2000u32 {
        mixed.extend_from_slice(format!("chunk-{i}-").as_bytes());
        mixed.extend(prng(i, 300));
    }
    write(dir, "mixed", &mixed);
    // `runs` with a 400 modulus rather than 200.
    let mut r = Vec::new();
    for i in 0..4000usize {
        r.extend(std::iter::repeat_n((i % 97) as u8, 1 + (i * 7) % 400));
    }
    write(dir, "runs", &r);
    write(dir, "noise", &prng(9, 900000));
    write(dir, "zeros", &vec![0u8; 400000]);
    let mut exe_unit = b"\x7fELF\x02\x01\x01".to_vec();
    exe_unit.extend(prng(3, 120));
    write(dir, "exe", &exe_unit.repeat(4000));
    for n in [0usize, 1, 255, 256, 65537] {
        write(dir, &format!("n_{n}"), &prng(5, n));
    }
}

/// `rep-check.sh`.
fn rep(dir: &std::path::Path) {
    write(dir, "empty", b"");
    write(dir, "tiny", b"hello");
    write(dir, "nomatch", &prng(1, 20000));
    let blk = prng(2, 2000);
    let mut one_match = blk.clone();
    one_match.extend(prng(3, 5000));
    one_match.extend_from_slice(&blk);
    one_match.extend_from_slice(&blk);
    write(dir, "one_match", &one_match);
    let mut many_unit = blk.clone();
    many_unit.extend(prng(4, 600));
    write(dir, "many", &many_unit.repeat(40));
    write(dir, "zeros", &vec![0u8; 100000]);
    write(dir, "text", &repeat(FOX, 2000));
    for n in [511usize, 512, 513, 1023, 1024, 1025] {
        write(dir, &format!("rep_{n}"), &prng(6, n).repeat(3));
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
        "bsc-bwt" => bsc_bwt(&dir),
        "bsc-bwt-encode" => bsc_bwt_encode(&dir),
        "bsc-st" => bsc_st(&dir),
        "bsc-st-encode" => bsc_st_encode(&dir),
        "bsc-lzp-encode" => bsc_lzp_encode(&dir),
        "bsc-qlfc-encode" => bsc_qlfc_encode(&dir),
        "bsc-qlfc-encode-big" => bsc_qlfc_encode_big(&dir),
        "bsc-qlfc-transform" => bsc_qlfc_transform(&dir),
        "4x4" => fourx4(&dir),
        "rep" => rep(&dir),
        other => {
            eprintln!("corpusgen: unknown corpus {other:?}");
            std::process::exit(2);
        }
    }
}
