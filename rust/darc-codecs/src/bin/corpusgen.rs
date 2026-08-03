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
    // Some corpora name files inside subdirectories (`nested/deep/b.bin`), which
    // the Python got for free from the shell having made them.
    match path.parent() {
        Some(p) => std::fs::create_dir_all(p)
            .unwrap_or_else(|e| panic!("mkdir {}: {e}", p.display())),
        None => {}
    }
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

/// CPython's `random.Random` — MT19937, seeded and consumed exactly as CPython
/// does it.
///
/// Two corpora (`dict` and `ppmd`) were built with `random.Random(seed)`, and
/// their bytes cannot be reproduced by any other generator. This is not a
/// "good enough" random source: it is a transcription, and the only thing that
/// makes it correct is that it produces the same files.
struct MersenneTwister {
    mt: [u32; 624],
    index: usize,
}

impl MersenneTwister {
    /// `init_by_array` with the key CPython derives from an integer seed: the
    /// absolute value, little-endian, in 32-bit words.
    fn new(seed: u64) -> Self {
        let mut mt = [0u32; 624];
        // init_genrand(19650218)
        mt[0] = 19650218;
        for i in 1..624 {
            mt[i] = 1812433253u32
                .wrapping_mul(mt[i - 1] ^ (mt[i - 1] >> 30))
                .wrapping_add(i as u32);
        }
        let key: Vec<u32> = match seed {
            0 => vec![0],
            _ => {
                let mut k = Vec::new();
                let mut s = seed;
                while s > 0 {
                    k.push((s & 0xffff_ffff) as u32);
                    s >>= 32;
                }
                k
            }
        };
        let (mut i, mut j) = (1usize, 0usize);
        for _ in 0..624.max(key.len()) {
            mt[i] = (mt[i] ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1664525))
                .wrapping_add(key[j])
                .wrapping_add(j as u32);
            i += 1;
            j += 1;
            if i >= 624 {
                mt[0] = mt[623];
                i = 1;
            }
            if j >= key.len() {
                j = 0;
            }
        }
        for _ in 0..623 {
            mt[i] = (mt[i] ^ (mt[i - 1] ^ (mt[i - 1] >> 30)).wrapping_mul(1566083941))
                .wrapping_sub(i as u32);
            i += 1;
            if i >= 624 {
                mt[0] = mt[623];
                i = 1;
            }
        }
        mt[0] = 0x8000_0000;
        MersenneTwister { mt, index: 624 }
    }

    fn genrand_u32(&mut self) -> u32 {
        if self.index >= 624 {
            for i in 0..624 {
                let y = (self.mt[i] & 0x8000_0000) | (self.mt[(i + 1) % 624] & 0x7fff_ffff);
                let mut next = self.mt[(i + 397) % 624] ^ (y >> 1);
                if y & 1 != 0 {
                    next ^= 0x9908_b0df;
                }
                self.mt[i] = next;
            }
            self.index = 0;
        }
        let mut y = self.mt[self.index];
        self.index += 1;
        y ^= y >> 11;
        y ^= (y << 7) & 0x9d2c_5680;
        y ^= (y << 15) & 0xefc6_0000;
        y ^ (y >> 18)
    }

    /// `random()` — `genrand_res53`, 53 bits from two draws.
    fn random(&mut self) -> f64 {
        let a = self.genrand_u32() >> 5;
        let b = self.genrand_u32() >> 6;
        (a as f64 * 67108864.0 + b as f64) / 9007199254740992.0
    }

    /// `getrandbits(k)` for k <= 32.
    fn getrandbits(&mut self, k: u32) -> u32 {
        match k {
            0 => 0,
            _ => self.genrand_u32() >> (32 - k),
        }
    }

    /// `_randbelow(n)` — rejection sampling on `n.bit_length()` bits, which is
    /// what `randrange(n)` calls.
    fn randbelow(&mut self, n: u32) -> u32 {
        if n == 0 {
            return 0;
        }
        let k = 32 - (n - 1).leading_zeros();
        loop {
            let r = self.getrandbits(k);
            if r < n {
                return r;
            }
        }
    }

    /// `choices(population, weights, k)` — cumulative weights and a bisect on
    /// `random() * total`, with `hi = len-1` as CPython passes it.
    fn choices(&mut self, cum_weights: &[f64], k: usize) -> Vec<usize> {
        let total = *cum_weights.last().unwrap_or(&1.0);
        let hi = cum_weights.len() - 1;
        (0..k)
            .map(|_| {
                let x = self.random() * total;
                // bisect_right over [0, hi)
                let (mut lo, mut high) = (0usize, hi);
                while lo < high {
                    let mid = (lo + high) / 2;
                    if x < cum_weights[mid] {
                        high = mid;
                    } else {
                        lo = mid + 1;
                    }
                }
                lo
            })
            .collect()
    }
}

/// `b"".join(struct.pack("<I", v) for v in vals)`
fn le32<I: IntoIterator<Item = u32>>(vals: I) -> Vec<u8> {
    let mut out = Vec::new();
    for v in vals {
        out.extend_from_slice(&v.to_le_bytes());
    }
    out
}

/// `b"".join(struct.pack("<H", v) for v in vals)`
fn le16<I: IntoIterator<Item = u16>>(vals: I) -> Vec<u8> {
    let mut out = Vec::new();
    for v in vals {
        out.extend_from_slice(&v.to_le_bytes());
    }
    out
}

/// `struct.pack("<h", max(-32768, min(32767, int(v))))` over a sample list.
///
/// `int()` truncates TOWARD ZERO, which is what Rust's `as i32` does too, and
/// the clamp is applied after truncation exactly as the Python writes it.
fn s16<I: IntoIterator<Item = f64>>(vals: I) -> Vec<u8> {
    let mut out = Vec::new();
    for v in vals {
        let n = (v as i32).clamp(-32768, 32767) as i16;
        out.extend_from_slice(&n.to_le_bytes());
    }
    out
}

/// `tornado-check.sh` — the decoder sweep.
fn tornado(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 20000));
    let mut repeats_unit = repeat(b"ABCDEFGHIJKLMNOP", 64);
    repeats_unit.extend(prng(1, 256));
    write(dir, "repeats", &repeats_unit.repeat(400));
    write(dir, "noise", &prng(7, 900000));
    write(dir, "zeros", &vec![0u8; 700000]);
    let mut mixed = Vec::new();
    for i in 0..300u32 {
        mixed.extend(prng(i, 1000));
        mixed.extend(repeat(b"pattern", 200));
    }
    write(dir, "mixed", &mixed);
    // Tables of 4- and 2-byte little-endian counters: what the data-table
    // detector is built to find.
    write(dir, "table4", &le32((0..200000u32).map(|i| i.wrapping_mul(7).wrapping_add(3))));
    write(dir, "table2", &le16((0..400000u32).map(|i| (i.wrapping_mul(11) & 0xffff) as u16)));
    let mut table_mixed = le32(0..50000u32);
    table_mixed.extend(prng(3, 200000));
    table_mixed.extend(le16((0..100000u32).map(|i| (i & 0xffff) as u16)));
    write(dir, "table_mixed", &table_mixed);
    // Larger than HUGE_BUFFER_SIZE (8 MB), so the output window wraps and
    // flushes mid-stream. A corpus without this passed while the port still had
    // a panic in it.
    write(dir, "big_table", &le32((0..2600000u32).map(|i| i.wrapping_mul(7).wrapping_add(3))));
    for n in [0usize, 1, 15, 16, 17, 63, 64, 65, 4095, 4096, 65535, 65536, 65537] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"the quick brown fox ", 10000, n));
    }
}

/// `tornado-encode-check.sh` — a superset of the decoder corpus.
fn tornado_encode(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 20000));
    let mut repeats_unit = repeat(b"ABCDEFGHIJKLMNOP", 64);
    repeats_unit.extend(prng(1, 256));
    write(dir, "repeats", &repeats_unit.repeat(400));
    write(dir, "noise", &prng(7, 900000));
    write(dir, "zeros", &vec![0u8; 700000]);
    let mut mixed = Vec::new();
    for i in 0..300u32 {
        mixed.extend(prng(i, 1000));
        mixed.extend(repeat(b"pattern", 200));
    }
    write(dir, "mixed", &mixed);
    // Short unique prefix + noise, repeated. Diverges at preset 9 and nowhere
    // else; the sweep was green on preset 9 until this was added.
    let mut chunky = Vec::new();
    for i in 0..2000u32 {
        chunky.extend_from_slice(format!("chunk-{i}-").as_bytes());
        chunky.extend(prng(i, 300));
    }
    write(dir, "chunky", &chunky);
    write(dir, "table4", &le32((0..200000u32).map(|i| i.wrapping_mul(7).wrapping_add(3))));
    // Distances either side of accept_match()'s 48 KB / 192 KB / 1 MB limits.
    let seg = prng(5, 300000);
    let mut far = seg.clone();
    far.extend(prng(11, 40 * 1024));
    far.extend_from_slice(&seg[..50000]);
    far.extend(prng(13, 200 * 1024));
    far.extend_from_slice(&seg[..50000]);
    far.extend(prng(17, 1100 * 1024));
    far.extend_from_slice(&seg[..50000]);
    write(dir, "far_matches", &far);
    // Past LARGE_BUFFER_SIZE (256 KB) many times over, so the input buffer is
    // refilled and slid repeatedly rather than read once.
    write(dir, "big_text", &repeat(FOX, 400000));
    write(dir, "big_noise", &prng(23, 5 * 1024 * 1024));
    for n in [
        0usize, 1, 3, 4, 5, 15, 16, 17, 63, 64, 65, 255, 256, 257, 4095, 4096, 65535, 65536,
        65537,
    ] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"the quick brown fox ", 10000, n));
    }
}

/// `tta-check.sh` — audio-shaped input, which is what TTA models.
fn tta(dir: &std::path::Path) {
    const N: usize = 100000;
    write(
        dir,
        "sine16_stereo",
        &s16((0..N).flat_map(|i| {
            let i = i as f64;
            [20000.0 * (i * 0.03).sin(), 15000.0 * (i * 0.041).sin()]
        })),
    );
    write(
        dir,
        "chord16_stereo",
        &s16((0..N).flat_map(|i| {
            let i = i as f64;
            [
                8000.0 * (i * 0.02).sin() + 6000.0 * (i * 0.05).sin(),
                7000.0 * (i * 0.03).sin() + 5000.0 * (i * 0.07).sin(),
            ]
        })),
    );
    write(
        dir,
        "quiet16_stereo",
        &s16((0..N).flat_map(|i| {
            let i = i as f64;
            [30.0 * (i * 0.03).sin(), 25.0 * (i * 0.05).sin()]
        })),
    );
    write(dir, "ramp16_mono", &s16((0..2 * N).map(|i| ((i % 2000) as f64) - 1000.0)));
    write(dir, "silence16", &vec![0u8; 2 * (2 * N)]);
    write(
        dir,
        "sine8_mono",
        &(0..2 * N)
            .map(|i| (128i32 + (100.0 * (i as f64 * 0.05).sin()) as i32) as u8)
            .collect::<Vec<u8>>(),
    );
    write(dir, "noise8", &prng(9, 2 * N));
    for n in [0usize, 1, 3, 4, 8, 1 << 18, (1 << 18) + 1, (1 << 18) - 1] {
        write(
            dir,
            &format!("n16_{n}"),
            &s16((0..n).map(|i| 9000.0 * (i as f64 * 0.03).sin())),
        );
    }
    // A table of ascending 32-bit LE integers -- NOT audio, and the shape that
    // separates TTA's candidate model set from MM's. TTA's own channels/bits
    // arrays are narrower, and on this input the wide set picks 1x32, which TTA
    // refuses and stores.
    write(dir, "table32", &le32((0..8000u32).map(|i| i.wrapping_mul(3))));
    write(
        dir,
        "table32_wide",
        &le32((0..8000u32).map(|i| i.wrapping_mul(2654435761) & 0xffff)),
    );
}

/// `lzp-check.sh`.
fn lzp(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 4000));
    write(dir, "english", &repeat(ENGLISH, 1200));
    let mut longrep_unit = vec![b'A'; 300];
    longrep_unit.extend(prng(2, 120));
    write(dir, "longrep", &longrep_unit.repeat(400));
    write(dir, "onebyte", &vec![0x5a; 200000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 100000));
    let mut r = Vec::new();
    for i in 0..3000usize {
        r.extend(std::iter::repeat_n((i % 97) as u8, 1 + (i * 7) % 400));
    }
    write(dir, "runs", &r);
    write(dir, "noise", &prng(9, 300000));
    write(dir, "alphabet", &alphabet(300000));
    let mut sp = Vec::new();
    for i in 0..600usize {
        sp.extend(std::iter::repeat_n(0u8, 500));
        sp.push((i % 251) as u8);
    }
    write(dir, "sparse", &sp);
    let mut big_unit = b"repeatable-chunk-".to_vec();
    big_unit.extend(prng(11, 900));
    write(dir, "big", &big_unit.repeat(1200));
    for n in [0usize, 1, 2, 63, 64, 65, 255, 256, 257, 4096, 65537] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `lz4hc-check.sh`.
fn lz4hc(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 3000));
    write(dir, "english", &repeat(ENGLISH, 900));
    write(dir, "runs", &runs(2000));
    write(dir, "onebyte", &vec![0x5a; 80000]);
    write(dir, "twobyte", &repeat(b"\x00\xff", 40000));
    write(dir, "skew", &skew(150000));
    write(dir, "sparse", &sparse(500));
    write(dir, "noise", &prng(9, 200000));
    write(dir, "alphabet", &alphabet(200000));
    // For the OPTIMAL parser (levels 10-12): SHORT matches under
    // `sufficient_len` interleaved with literal runs past 269 bytes, which is
    // where `literalsPrice` picks up its extra length byte.
    let pool: Vec<Vec<u8>> = (0..24usize)
        .map(|i| (0..20 + (i % 21)).map(|j| ((i * 7 + j * 13) % 251) as u8).collect())
        .collect();
    let mut seg = Vec::new();
    for i in 0..400usize {
        seg.extend(prng(1000 + i as u32, 280 + (i * 37) % 400));
        seg.extend_from_slice(&pool[i % pool.len()]);
    }
    write(dir, "priced", &seg);
    // COMPETING matches: a small vocabulary in varying order, so phrases recur
    // partially at many distances and the parser must weigh "short match now"
    // against "literals now, longer match later".
    let vocab: Vec<Vec<u8>> = (0..120usize)
        .map(|i| (0..3 + (i % 10)).map(|j| (((i * 29 + j * 7) % 26) + 97) as u8).collect())
        .collect();
    let mut txt = Vec::new();
    let mut st: u32 = 12345;
    for i in 0..6000usize {
        st = st.wrapping_mul(1103515245).wrapping_add(12345);
        txt.extend_from_slice(&vocab[((st >> 16) as usize) % vocab.len()]);
        txt.push(b' ');
        if i % 200 == 0 {
            txt.extend(prng(7000 + i as u32, 300 + (i % 300)));
        }
    }
    write(dir, "competing", &txt);
    // Offsets are 16 bits, so these straddle the 65535 window where an
    // off-by-one in lowest_match_index shows.
    let w1 = prng(3, 70000);
    let mut window = w1.clone();
    window.extend_from_slice(&w1[..2000]);
    write(dir, "window", &window);
    let mut farback = prng(5, 65000);
    farback.extend(repeat(b"MARKER", 8));
    farback.extend(prng(7, 60000));
    farback.extend(repeat(b"MARKER", 8));
    write(dir, "farback", &farback);
    for n in [1usize, 4, 12, 13, 14, 17, 255, 256, 257, 4096, 65535, 65536, 65537] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `ppmd-check.sh`.
fn ppmd(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 6000));
    write(
        dir,
        "english",
        &repeat(
            b"compression algorithms rearrange data so that statistical \
              redundancy can be removed by an entropy coder. ",
            2500,
        ),
    );
    write(dir, "noise", &prng(7, 200000));
    write(dir, "zeros", &vec![0u8; 120000]);
    write(dir, "runs", &runs(2000));
    write(dir, "full_alpha", &alphabet_full(500));
    let mut sp = Vec::new();
    for i in 0..400usize {
        sp.extend(std::iter::repeat_n(0u8, 300));
        sp.push((i % 251) as u8);
    }
    write(dir, "sparse", &sp);
    write(
        dir,
        "binaryish",
        &(0..150000u64).map(|i| ((i.wrapping_mul(2654435761) >> 16) & 0xff) as u8).collect::<Vec<u8>>(),
    );
    // `random.Random(3).choices(range(256), weights=[4000]+[1]*255, k=1500)`.
    //
    // A dominant symbol plus a long tail of rare ones, at LOW order: the only
    // shape found that reaches rescale's shrink path, where the port's one real
    // PPMd bug lived (EscFreq is UINT in rescale and int in refresh). Every
    // other input passed with that bug present, so these exact 1500 bytes
    // matter -- which is why this needs CPython's Mersenne Twister and not any
    // other generator.
    let mut cum = Vec::with_capacity(256);
    let mut acc = 0.0f64;
    for i in 0..256usize {
        acc += match i {
            0 => 4000.0,
            _ => 1.0,
        };
        cum.push(acc);
    }
    let mut rng = MersenneTwister::new(3);
    write(dir, "dominant", &rng.choices(&cum, 1500).into_iter().map(|i| i as u8).collect::<Vec<u8>>());
    write(dir, "bignoise", &prng(3, 600000));
    let mut mixed = prng(5, 300000);
    mixed.extend(repeat(b"the quick brown fox ", 5000));
    write(dir, "mixed", &mixed);
    for n in [1usize, 2, 3, 17, 255, 256, 4096, 65536] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abracadabra", 10000, n));
    }
}

/// `srep-check.sh` and `srep-encode-check.sh`, which share a corpus.
fn srep(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 20000));
    let mut dup_unit = prng(1, 100000);
    dup_unit.extend(prng(2, 50000));
    write(dir, "dup", &dup_unit.repeat(6));
    write(dir, "noise", &prng(9, 900000));
    write(dir, "runs", &runs_fixed(900, 251, 997));
    let mut mixed = Vec::new();
    for i in 0..120u32 {
        mixed.extend(prng(i, 2000));
        mixed.extend(repeat(b"COMMON-SECTION", 400));
    }
    write(dir, "mixed", &mixed);
    let mut far = prng(4, 300000);
    far.extend(prng(5, 300000));
    far.extend(prng(4, 300000));
    write(dir, "farapart", &far);
    for n in [0usize, 1, 100, 4096, 65536] {
        write(dir, &format!("n_{n}"), &prng(3, n));
    }
}

/// `debug-assert-check.sh` — deliberately small, since a debug build is slow
/// and this looks for a fired assertion rather than for byte coverage.
fn debug_assert(dir: &std::path::Path) {
    // `chunky` is the shape that exposed the Hash3 dispatch bug, so it is first.
    let mut chunky = Vec::new();
    for i in 0..400u32 {
        chunky.extend_from_slice(format!("chunk-{i}-").as_bytes());
        chunky.extend(prng(i, 300));
    }
    write(dir, "chunky", &chunky);
    write(dir, "text", &repeat(FOX, 1500));
    let mut r = Vec::new();
    for i in 0..600usize {
        r.extend(std::iter::repeat_n((i % 97) as u8, 1 + (i * 7) % 400));
    }
    write(dir, "runs", &r);
    write(dir, "noise", &prng(9, 80000));
    write(dir, "tables", &le32((0..20000u32).map(|i| i.wrapping_mul(7).wrapping_add(3))));
    for n in [0usize, 1, 255, 256, 65537] {
        write(dir, &format!("n_{n}"), &prng(5, n));
    }
}

/// `sevenz-check.sh` — the tree `7z` is asked to archive.
fn sevenz(dir: &std::path::Path) {
    write(dir, "empty.bin", b"");
    write(dir, "tiny.txt", b"hello");
    write(dir, "text.txt", &repeat(b"the quick brown fox jumps over the lazy dog.\n", 5000));
    write(dir, "random.bin", &prng(7, 300000));
    write(dir, "zeros.bin", &vec![0u8; 200000]);
    write(dir, "nested/a.txt", &repeat(b"nested file\n", 100));
    write(dir, "nested/deep/b.bin", &prng(11, 65536));
    // An x86-ish body, so BCJ/BCJ2 has something to transform rather than
    // passing incompressible noise straight through.
    let mut body = Vec::new();
    for i in 0..20000u64 {
        body.push(0xe8);
        body.extend_from_slice(&(((i * 7) % 4294967296) as u32).to_le_bytes());
        body.extend_from_slice(b"\x90\x8b\xc0");
    }
    write(dir, "codeish.bin", &body);
    // Non-ASCII name: names are UTF-16 in the container and UTF-8 outside it.
    write(dir, "ünicode-日本語.txt", b"unicode name\n");
    // An EMPTY directory, which the harness used to create in the shell before
    // calling the Python. It is a corpus entry in its own right: 7z stores a
    // directory record with no stream, and a reader that skips those loses it.
    std::fs::create_dir_all(dir.join("emptydir")).expect("emptydir");
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
        "tornado" => tornado(&dir),
        "tornado-encode" => tornado_encode(&dir),
        "tta" => tta(&dir),
        "lzp" => lzp(&dir),
        "lz4hc" => lz4hc(&dir),
        "ppmd" => ppmd(&dir),
        "srep" => srep(&dir),
        "debug-assert" => debug_assert(&dir),
        "sevenz" => sevenz(&dir),
        other => {
            eprintln!("corpusgen: unknown corpus {other:?}");
            std::process::exit(2);
        }
    }
}
