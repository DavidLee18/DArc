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

    /// `_randbelow(n)` — rejection sampling on `n.bit_length()` bits.
    ///
    /// `n.bit_length()`, NOT `(n-1).bit_length()`. CPython's comment on that
    /// line is literally "don't use (n-1) here", and the two agree except when
    /// `n` is an exact power of two — where the wrong one draws a bit fewer,
    /// never rejects, and desynchronises every draw after it. `random.choice`
    /// over a 2-element sequence is the first place that bites, and it is the
    /// only corpus entry that caught it.
    fn randbelow(&mut self, n: u32) -> u32 {
        if n == 0 {
            return 0;
        }
        let k = 32 - n.leading_zeros();
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

/// `grzip-check.sh`, main corpus.
fn grzip(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 9000));
    let mut repeats_unit = repeat(b"ABCDEFGHIJKLMNOP", 64);
    repeats_unit.extend(prng(1, 128));
    write(dir, "repeats", &repeats_unit.repeat(300));
    // `bytes([i%251])*(1+(i%97))` -- the run length follows i%97, not (i*7)%200.
    let mut r = Vec::new();
    for i in 0..6000usize {
        r.extend(std::iter::repeat_n((i % 251) as u8, 1 + (i % 97)));
    }
    write(dir, "runs", &r);
    write(dir, "noise", &prng(7, 400000));
    write(dir, "zeros", &vec![0u8; 300000]);
    write(dir, "rec4", &le32((0..120000u32).map(|i| i.wrapping_mul(7).wrapping_add(3))));
    write(dir, "rec2", &le16((0..200000u32).map(|i| (i.wrapping_mul(11) & 0xffff) as u16)));
    let mut mixed = Vec::new();
    for i in 0..200u32 {
        mixed.extend(prng(i, 600));
        mixed.extend(repeat(b"pattern", 120));
    }
    write(dir, "mixed", &mixed);
    // ~7 MB, just under one GRZip block.
    write(dir, "big", &le32((0..1800000u32).map(|i| i.wrapping_mul(2654435761))));
    for n in [1usize, 2, 3, 4, 27, 28, 29, 255, 256, 257, 4096, 65537] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"the quick brown fox ", 4000, n));
    }
}

/// The second file `grzip-check.sh` builds -- larger than `GRZ_MaxBlockSize`,
/// so the STREAM layer has to split it. The block harness cannot take one.
///
/// Emitted to STDOUT: the Python wrote a single named file, not a directory.
fn grzip_big() -> Vec<u8> {
    let mut out = Vec::new();
    for i in 0..3000000u32 {
        match i % 3 {
            0 => out.extend_from_slice(&b"the quick brown fox "[..4]),
            _ => out.extend_from_slice(&i.wrapping_mul(2654435761).to_le_bytes()),
        }
    }
    out
}

/// `lzma2-mt-check.sh` — sized around the 4 MiB block a 1 MB dictionary
/// implies, so the split's boundaries are straddled from both sides.
fn lzma2_mt(dir: &std::path::Path) {
    const MB: usize = 1 << 20;
    // `o += b'lorem ipsum dolor sit amet %d consectetur\n' % (s >> 16)` until
    // the buffer is long enough, then truncated. The line length VARIES with
    // the number, so this cannot be a fixed-size repeat.
    let gen = |seed: u32, n: usize| -> Vec<u8> {
        let mut s = seed;
        let mut o: Vec<u8> = Vec::with_capacity(n + 64);
        while o.len() < n {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            o.extend_from_slice(
                format!("lorem ipsum dolor sit amet {} consectetur\n", s >> 16).as_bytes(),
            );
        }
        o.truncate(n);
        o
    };
    for (name, n) in [
        ("half_block", 2 * MB),
        ("one_block", 4 * MB),
        ("one_plus", 4 * MB + 1),
        ("two_blocks", 8 * MB),
        ("five_blocks", 20 * MB + 12345),
    ] {
        write(dir, name, &gen(7, n));
    }
    // Incompressible, which drives the copy-chunk path inside each block.
    let mut s: u32 = 11;
    let want = 9 * MB;
    let mut noise: Vec<u8> = Vec::with_capacity(want + 4);
    while noise.len() < want {
        s = s.wrapping_mul(1103515245).wrapping_add(12345);
        noise.extend_from_slice(&s.to_le_bytes());
    }
    noise.truncate(want);
    write(dir, "noise_two_blocks", &noise);
}

/// `dict-check.sh`.
///
/// What Dict accepts is narrower than "repeated words": text made only of
/// lowercase letters and spaces is REFUSED outright, because `MinWeakChars`
/// demands a spread of non-word characters first. Only the `natural_*` inputs
/// engage the encoder, and the harness asserts that at least eight blocks do --
/// so these words, in this order, are what makes the comparison mean anything.
fn dict(dir: &std::path::Path) {
    const COMMON: &str = "the of and to in a is that for it as was with be by on not he this but have \
from they which one you were all her she there would their we him been has \
when who will more no if out so said what up its about into than them can";
    const TOPIC: &str = "compression algorithm dictionary preprocessor archive redundancy entropy encoder \
decoder statistical frequency threshold occurrence substitution replacement \
transformation implementation";

    let common: Vec<&str> = COMMON.split_whitespace().collect();
    let topic: Vec<&str> = TOPIC.split_whitespace().collect();

    let natural = |seed: u64, n: usize, topic_every: usize| -> Vec<u8> {
        let mut r = MersenneTwister::new(seed);
        let mut words: Vec<String> = Vec::new();
        for i in 0..n {
            let w = match i % topic_every {
                0 => topic[r.randbelow(topic.len() as u32) as usize],
                _ => common[r.randbelow(common.len() as u32) as usize],
            };
            // `str.capitalize()`: first character upper, the rest lower. Every
            // word here is already lowercase ASCII.
            let w = match i % 17 {
                0 => {
                    let mut c = w.chars();
                    match c.next() {
                        Some(f) => f.to_ascii_uppercase().to_string() + c.as_str(),
                        None => String::new(),
                    }
                }
                _ => w.to_string(),
            };
            words.push(w);
            if i % 11 == 0 {
                words.push((i % 1000).to_string());
            }
            if i % 13 == 0 {
                words.push(",".to_string());
            }
            if i % 29 == 0 {
                words.push(".".to_string());
            }
            if i % 97 == 0 {
                words.push("\n".to_string());
            }
        }
        words.join(" ").into_bytes()
    };

    write(dir, "natural_a", &natural(7, 200000, 6));
    write(dir, "natural_b", &natural(11, 120000, 3));
    write(dir, "natural_c", &natural(23, 60000, 12));

    let sent = "The quick brown fox jumps over the lazy dog, and the dog barks. \
Compression of text depends on repeated words appearing often. ";
    write(dir, "english", &repeat(sent.as_bytes(), 3000));

    // These DECLINE, and that is worth testing: the stored path has its own
    // four-byte framing and the two implementations must agree on it.
    let src = "static int compute_value(struct context *ctx, int index) {\n\
    \x20   if (ctx == NULL || index < 0) return -1;\n\
    \x20   return ctx->table[index] + ctx->offset;\n\
}\n";
    write(dir, "source", &repeat(src.as_bytes(), 6000));
    write(
        dir,
        "markup",
        &repeat(b"<div class=\"row\"><span id=\"x\">value</span></div>\n", 20000),
    );
    write(dir, "noise", &prng(9, 300000));
    write(dir, "zeros", &vec![0u8; 200000]);
    write(dir, "binary", &alphabet(300000));
    for n in [0usize, 1, 2, 3, 63, 64, 65, 255, 256, 257, 4095, 4096, 65537] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"word ", (n / 5) + 1, n));
    }
}

/// `geometric_bytes` from `mm-check.sh` — a geometric distribution over 256
/// symbols whose rate is bisected until its entropy hits `target_bits`.
///
/// This exists to sit ON `autodetect_by_entropy`'s absolute gate
/// (`model0_result < bufsize*0.80`, i.e. 6.4 bits/byte). Uniform data cannot
/// reach it: `calc_results` scores a slot as `count * log2(total/count)` with
/// that division done in INTEGER arithmetic, and for a uniform alphabet of k
/// symbols the quotient is exactly k, so the truncation is a no-op. The
/// distribution has to be skewed before `floor()` bites.
///
/// The bisection is 60 rounds of f64, so this is only correct if it matches
/// CPython's arithmetic bit for bit — which is what `verify-corpus.sh` checks.
fn geometric_bytes(target_bits: f64, n: usize, seed: u32) -> Vec<u8> {
    let ent = |r: f64| -> (f64, Vec<f64>) {
        let p: Vec<f64> = (0..256).map(|i| r.powi(i)).collect();
        let s: f64 = p.iter().sum();
        let p: Vec<f64> = p.iter().map(|x| x / s).collect();
        let e: f64 = -p.iter().filter(|x| **x > 0.0).map(|x| x * x.log2()).sum::<f64>();
        (e, p)
    };
    let (mut lo, mut hi) = (0.5f64, 0.99999f64);
    for _ in 0..60 {
        let mid = (lo + hi) / 2.0;
        if ent(mid).0 < target_bits {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    let (_, p) = ent((lo + hi) / 2.0);
    let mut cum = Vec::with_capacity(256);
    let mut acc = 0.0f64;
    for x in &p {
        acc += x;
        cum.push(acc);
    }
    let mut s = seed;
    (0..n)
        .map(|_| {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            let x = (((s >> 8) & 0xffffff) as f64) / 0x1000000 as f64;
            // `bisect.bisect_left(cum, x)`
            let (mut a, mut b) = (0usize, cum.len());
            while a < b {
                let mid = (a + b) / 2;
                if cum[mid] < x {
                    a = mid + 1;
                } else {
                    b = mid;
                }
            }
            a.min(255) as u8
        })
        .collect()
}

/// `mm-check.sh` — multimedia data, plus the inputs that sit on the detector's
/// decision boundaries.
fn mm(dir: &std::path::Path) {
    const N: usize = 300000; // 300k stereo 16-bit samples = 1.2 MB
    let pcm = s16((0..N).flat_map(|i| {
        let i = i as f64;
        [20000.0 * (i * 0.03).sin(), 15000.0 * (i * 0.041).sin()]
    }));
    write(dir, "sine16_stereo", &pcm);
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
    // A real 44-byte canonical WAV header, so autodetect_wav_header takes the
    // offset path rather than the entropy analyzer.
    let mut wav = Vec::new();
    wav.extend_from_slice(b"RIFF");
    wav.extend_from_slice(&((36 + pcm.len()) as u32).to_le_bytes());
    wav.extend_from_slice(b"WAVEfmt ");
    // `struct.pack("<IHHIIHH", 16,1,2,44100,44100*4,4,16)`
    wav.extend_from_slice(&16u32.to_le_bytes());
    wav.extend_from_slice(&1u16.to_le_bytes());
    wav.extend_from_slice(&2u16.to_le_bytes());
    wav.extend_from_slice(&44100u32.to_le_bytes());
    wav.extend_from_slice(&(44100u32 * 4).to_le_bytes());
    wav.extend_from_slice(&4u16.to_le_bytes());
    wav.extend_from_slice(&16u16.to_le_bytes());
    wav.extend_from_slice(b"data");
    wav.extend_from_slice(&(pcm.len() as u32).to_le_bytes());
    wav.extend_from_slice(&pcm);
    write(dir, "wav16_stereo", &wav);
    write(
        dir,
        "rgb24",
        &(0..400000usize)
            .flat_map(|i| {
                [
                    ((i * 7) % 256) as u8,
                    ((128 + (100.0 * (i as f64 * 0.01).sin()) as i64) & 0xff) as u8,
                    ((i / 97) % 256) as u8,
                ]
            })
            .collect::<Vec<u8>>(),
    );
    // `struct.pack("<i", int(...) & 0xffffff)[:3]` -- Python's `&` on a negative
    // int is two's complement with infinite sign extension, so the mask yields a
    // positive value below 2^24 and the low three bytes are taken.
    let mut pcm24 = Vec::new();
    for i in 0..2 * N {
        let v = (4000000.0 * (i as f64 * 0.02).sin()) as i64 & 0xffffff;
        pcm24.extend_from_slice(&(v as u32).to_le_bytes()[..3]);
    }
    write(dir, "pcm24_stereo", &pcm24);
    let mut f32s = Vec::new();
    for i in 0..350000usize {
        f32s.extend_from_slice(&((i as f64 * 0.01).sin() as f32).to_le_bytes());
    }
    write(dir, "float32_mono", &f32s);
    write(dir, "ramp32", &le32((0..350000u32).map(|i| i.wrapping_mul(2654435761))));
    write(dir, "noise8", &prng(9, 1300000));
    write(dir, "silence", &vec![0u8; 1300000]);
    write(dir, "text", &repeat(FOX, 30000));
    // `(pcm*8)[:n]`
    for n in [1usize, 3, 7, 8, 63, (1 << 20) - 1, 1 << 20, (1 << 20) + 1, (1 << 20) + 7] {
        let mut v = pcm.repeat(8);
        v.truncate(n);
        write(dir, &format!("n_{n}"), &v);
    }
    // Blends of clean signal into noise, so some land inside the 5% bands the
    // selection rules use and a small change in the estimate flips the model.
    const M: usize = 60000;
    for k in 0..12usize {
        let amp = 1.0 - k as f64 / 12.0;
        let noise = prng(1000 + k as u32, 4 * M);
        write(
            dir,
            &format!("blend16_{k:02}"),
            &s16((0..2 * M).map(|i| {
                let n = noise[(2 * i) % noise.len()] as f64 - 128.0;
                amp * 12000.0 * (i as f64 * 0.031).sin() + (1.0 - amp) * (n * 90.0)
            })),
        );
    }
    // Ambiguous between 8- and 16-bit: the low byte carries almost as much
    // structure as the high one, so the two models score close together.
    write(
        dir,
        "amb8_16",
        &(0..500000usize)
            .flat_map(|i| {
                [
                    ((i * 13) % 251) as u8,
                    ((128 + (60.0 * (i as f64 * 0.02).sin()) as i64) & 0xff) as u8,
                ]
            })
            .collect::<Vec<u8>>(),
    );
    // 24-bit with the top bit SET, so signed and unsigned readings differ --
    // _24bit_run measures signed values, _24bit_diff_run unsigned differences.
    let mut hi24 = Vec::new();
    for i in 0..200000usize {
        let v = (0xC00000i64 + (200000.0 * (i as f64 * 0.02).sin()) as i64) & 0xffffff;
        hi24.extend_from_slice(&(v as u32).to_le_bytes()[..3]);
    }
    write(dir, "hi24", &hi24);
    for (j, tb) in [6.20, 6.30, 6.35, 6.38, 6.40, 6.42, 6.45, 6.50, 6.60].iter().enumerate() {
        write(dir, &format!("gate8_{j}"), &geometric_bytes(*tb, 200000, 7000 + j as u32));
    }
    // Quiet multimedia: low order-0 entropy AND a large diff advantage, so it
    // fails one gate and passes the other. ~1000 sits inside the band while
    // compressing 54% better than order-0 -- the detector stores it anyway,
    // because the gate asks "is order-0 already good" rather than "would MM
    // help". Pinning that behaviour is the point.
    for amp in [700.0f64, 850.0, 950.0, 1000.0, 1050.0, 1150.0, 1400.0, 2200.0] {
        let vals: Vec<f64> = (0..2 * 60000)
            .flat_map(|i| {
                let i = i as f64;
                [amp * (i * 0.03).sin(), amp * 0.75 * (i * 0.041).sin()]
            })
            .take(2 * 60000)
            .collect();
        write(dir, &format!("quiet16_a{}", amp as i64), &s16(vals));
    }
}

/// `grzip-stage-check.sh` — the LZP and record stages.
///
/// Several of these are placed rather than chosen: the record-shaped inputs
/// exist because `GRZip_Rec_Test` returns 0 for everything else, so the record
/// stage would be untested; the `exact_L` blocks exist because nothing else
/// produces a match of exactly `MinMatchLen`, so flipping that `<` to `<=`
/// changed no output at all.
fn grzip_stage(dir: &std::path::Path) {
    write(dir, "repeat8", &repeat(b"abcdefgh", 40000));
    write(dir, "text", &repeat(FOX, 8000));
    write(dir, "noise", &prng(7, 300000));
    let mut mixed = Vec::new();
    for i in 0..300u32 {
        match i % 3 {
            0 => mixed.extend(prng(i, 400)),
            _ => mixed.extend(repeat(b"abcdefgh", 50)),
        }
    }
    write(dir, "mixed", &mixed);
    write(
        dir,
        "f2heavy",
        &(0..200000usize)
            .map(|i| match i % 5 {
                0 => 0xF2,
                _ => ((i * 7) & 0xff) as u8,
            })
            .collect::<Vec<u8>>(),
    );
    write(dir, "runs", &runs_fixed(300, 256, 1000));
    write(dir, "zeros", &vec![0u8; 400000]);
    // Inputs that make the BWT comparison's TIE loop run. `simple_cmp` only
    // walks when two positions have identical ranks, and that loop executed 0%
    // of the time on ordinary text -- so changing its stride from 2 to 1, a
    // real semantic change, left the whole run green.
    write(
        dir,
        "bwt_ties_runs",
        &(0..200000usize)
            .map(|i| match i % 100 < 90 {
                true => 0x78,
                false => (i % 251) as u8,
            })
            .collect::<Vec<u8>>(),
    );
    write(
        dir,
        "bwt_ties_blocks",
        &(0..200000usize).map(|i| (((i / 64) % 4) + 0x70) as u8).collect::<Vec<u8>>(),
    );
    for n in [32usize, 33, 40, 63, 64, 65, 1000, 4096, 4097] {
        write(dir, &format!("n_{n}"), &truncated_repeat(b"abcdefgh", 2000, n));
    }
    for n in [4096usize, 4097, 5000] {
        write(dir, &format!("rnd_{n}"), &prng(3, n));
    }
    // Record-shaped inputs. Modes 1 and 2 are plain 2- and 4-byte
    // de-interleaves, 3 and 4 the delta-coded versions; the harness asserts all
    // four appear.
    write(dir, "rec16_counter", &le16((0..60000u32).map(|i| ((i * 3) & 0xffff) as u16)));
    write(
        dir,
        "rec16_noisy",
        &le16((0..60000u32).map(|i| (((i * 3) & 0xffff) ^ u32::from(prng(i, 1)[0] & 0x7)) as u16)),
    );
    write(dir, "rec32_counter", &le32((0..30000u32).map(|i| i * 7)));
    write(dir, "rec32_table", &le32((0..30000u32).map(|i| 0x40000000 + (i % 997) * 13)));
    write(dir, "rec16_flat", &le16((0..60000u32).map(|i| ((i % 251) * 17) as u16)));
    // `struct.pack("<HBB", i&0xffff, (i*5)&0xff, 0x20)`
    let mut rec32_struct = Vec::new();
    for i in 0..30000u32 {
        rec32_struct.extend_from_slice(&((i & 0xffff) as u16).to_le_bytes());
        rec32_struct.push(((i * 5) & 0xff) as u8);
        rec32_struct.push(0x20);
    }
    write(dir, "rec32_struct", &rec32_struct);
    write(
        dir,
        "rec16_desc",
        &le16((0..60000u32).map(|i| (65535u32.wrapping_sub(i * 3) & 0xffff) as u16)),
    );
    // Modes 1 and 2 need de-interleaving to pay off while DELTA coding does
    // not: one noisy field, the rest near constant.
    let n16 = prng(41, 60000);
    write(dir, "rec16_noise_lo", &le16((0..60000).map(|i| 0x2500u16 | u16::from(n16[i]))));
    let n32 = prng(43, 30000);
    write(dir, "rec32_noise_lo", &le32((0..30000).map(|i| 0x40302000u32 | u32::from(n32[i]))));
    // Mode 2 additionally needs the record VALUES to stay small: the delta test
    // compares Sum against `MinCode*(Size>>2)`, a `uint32 * int` product, and
    // this is the non-overflowing side of it.
    let n32b = prng(61, 30000);
    write(dir, "rec32_small", &le32((0..30000).map(|i| 0x2000u32 | u32::from(n32b[i]))));
    // `MinCode*(Size>>1)` is `int * int`, so with MinCode near 0xF000 and 60k
    // records the product is 3.7e9 and wraps NEGATIVE. Without an input in this
    // range, widening it to 64 bits changes nothing and the sabotage passes.
    let n16b = prng(71, 60000);
    write(dir, "rec16_high", &le16((0..60000).map(|i| 0xF000u16 | u16::from(n16b[i]))));
    // Either side of the delta test's 6.25% slack band. Step 33 sits just above
    // it (mode 4 either way), step 34 inside it (mode 2 only because of the
    // slack). Derived by modelling the comparison and confirmed against the C.
    write(dir, "rec32_band_out", &le32((0..4096u32).map(|i| 0x2000 | ((i * 33) & 0xFF))));
    write(dir, "rec32_band_in", &le32((0..4096u32).map(|i| 0x2000 | ((i * 34) & 0xFF))));
    // Matches of EXACTLY MinMatchLen: a repeated 4-byte context, then agreement
    // for exactly L more bytes before one divergent byte.
    for l in [7usize, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65] {
        let mut blk = Vec::new();
        for r in 0..400usize {
            blk.extend_from_slice(&[0xA1, 0xB2, 0xC3, 0xD4]);
            blk.extend((0..l).map(|i| ((r * 13 + i) & 0xff) as u8));
            blk.push(((r * 77) & 0xff) as u8);
        }
        write(dir, &format!("exact_{l}"), &blk);
    }
}

/// `mmdet-check.sh` — the file-type detector.
///
/// Every file re-seeds at 12345, because the shell called `python3 -c` once per
/// file and each got a fresh interpreter.
///
/// The `gate_*` sweep is dense rather than sampled on purpose: the band that
/// discriminates depends on the noise, and the harness records that with five
/// points only one caught a sabotage of the order-0 sum, and that one was luck.
fn mmdet(dir: &std::path::Path) {
    write(dir, "text_english", &repeat(FOX, 3000));
    write(dir, "text_source", &repeat(b"int main(void) { return compute(x, y); }\n", 2500));

    // `random.choice(seq)` is `seq[_randbelow(len(seq))]`.
    let choice = |rng: &mut MersenneTwister, seq: &[u8], n: usize| -> Vec<u8> {
        (0..n).map(|_| seq[rng.randbelow(seq.len() as u32) as usize]).collect()
    };
    let mut rng = MersenneTwister::new(12345);
    write(dir, "text_narrow", &choice(&mut rng, b"abcdefghijklmnopqrst ", 100000));
    let mut rng = MersenneTwister::new(12345);
    let a17: Vec<u8> = (97u8..114).collect();
    write(dir, "text_17chars", &choice(&mut rng, &a17, 80000));
    let mut rng = MersenneTwister::new(12345);
    let a80: Vec<u8> = (33u8..113).collect();
    write(dir, "text_80chars", &choice(&mut rng, &a80, 80000));

    // `random.getrandbits(8)` is the top 8 bits of one draw.
    let mut rng = MersenneTwister::new(12345);
    write(
        dir,
        "noise",
        &(0..200000).map(|_| rng.getrandbits(8) as u8).collect::<Vec<u8>>(),
    );

    // `getrandbits(8) if random() < p else 65` -- the CONDITION is evaluated
    // first, and `getrandbits` is only drawn when it is true. Getting that
    // order wrong desynchronises the whole stream.
    for (name, p) in [("near_gate1", 0.93f64), ("near_gate2", 0.85), ("near_gate3", 0.97)] {
        let mut rng = MersenneTwister::new(12345);
        write(
            dir,
            name,
            &(0..200000)
                .map(|_| match rng.random() < p {
                    true => rng.getrandbits(8) as u8,
                    false => 65,
                })
                .collect::<Vec<u8>>(),
        );
    }
    // `65 if random() < bias/10000.0 else getrandbits(8)` -- the other way
    // round, so the draw happens on the FALSE branch here.
    for bias in [
        1040u32, 1050, 1060, 1070, 1075, 1080, 1085, 1088, 1090, 1092, 1095, 1100, 1105, 1110,
        1120, 1130,
    ] {
        let mut rng = MersenneTwister::new(12345);
        let p = f64::from(bias) / 10000.0;
        write(
            dir,
            &format!("gate_{bias}"),
            &(0..400000)
                .map(|_| match rng.random() < p {
                    true => 65,
                    false => rng.getrandbits(8) as u8,
                })
                .collect::<Vec<u8>>(),
        );
    }
    // One byte repeated: every count but one is zero, so the integer division
    // in the order-0 sum matters most here.
    write(dir, "all_zeros", &vec![0u8; 200000]);
    let mut rng = MersenneTwister::new(12345);
    write(dir, "two_symbols", &choice(&mut rng, b"ab", 200000));
    // Structured binary, which should be neither text nor compressed.
    write(dir, "struct32", &le32((0..50000u32).map(|i| i % 7919)));
    let mut wav = Vec::new();
    wav.extend_from_slice(b"RIFF");
    wav.extend_from_slice(&(36u32 + 8000).to_le_bytes());
    wav.extend_from_slice(b"WAVEfmt ");
    wav.extend_from_slice(&16u32.to_le_bytes());
    wav.extend_from_slice(&1u16.to_le_bytes());
    wav.extend_from_slice(&2u16.to_le_bytes());
    wav.extend_from_slice(&44100u32.to_le_bytes());
    wav.extend_from_slice(&176400u32.to_le_bytes());
    wav.extend_from_slice(&4u16.to_le_bytes());
    wav.extend_from_slice(&16u16.to_le_bytes());
    wav.extend_from_slice(b"data");
    wav.extend_from_slice(&8000u32.to_le_bytes());
    for i in 0..4000usize {
        let v = ((8000.0 + 7000.0 * (i as f64 / 40.0).sin()) as i64).rem_euclid(65536) as u16;
        wav.extend_from_slice(&v.to_le_bytes());
    }
    write(dir, "wavlike", &wav);
    // Sizes around the scan's own boundaries: the match loop needs bufsize > 10
    // and stops 10 bytes early, and mm_bytes changes shape at 64 KB and 1 MB.
    for n in [0usize, 1, 9, 10, 11, 12, 100, 65535, 65536, 65537, 1048576, 2097152] {
        let mut rng = MersenneTwister::new(12345);
        let name = match n {
            1048576 => "size_1m".to_string(),
            2097152 => "size_2m".to_string(),
            _ => format!("size_{n}"),
        };
        write(dir, &name, &(0..n).map(|_| rng.getrandbits(8) as u8).collect::<Vec<u8>>());
    }
}

/// `lzma-gap-check.sh` — the main corpus plus its `stream/` subdirectory.
///
/// Everything in the top level is at most ~40 KB, smaller than the smallest
/// dictionary swept, so the window never slides for any of it and it cannot
/// tell a correct window from a broken one. The `stream/` files are many times
/// the dictionary size and exist only for that.
fn lzma_gap(dir: &std::path::Path) {
    write(dir, "text", &repeat(FOX, 700));
    write(dir, "zeros", &vec![0u8; 40000]);
    let mut r = Vec::new();
    for i in 0..300usize {
        r.extend(std::iter::repeat_n((i % 251) as u8, 1 + (i * 7) % 300));
    }
    write(dir, "runs", &r);
    write(dir, "noise", &prng(9, 40000));
    let mut mixed = Vec::new();
    for i in 0..150u32 {
        mixed.extend_from_slice(format!("chunk-{i}-").as_bytes());
        mixed.extend(prng(i, 200));
    }
    write(dir, "mixed", &mixed);
    let mut nearby = Vec::new();
    for i in 0..60u32 {
        nearby.extend(prng(i % 5, 500));
    }
    write(dir, "nearby", &nearby);
    let mut distant = prng(1, 30000);
    distant.extend(prng(2, 30000));
    distant.extend(prng(1, 30000));
    write(dir, "distant", &distant);
    for n in [1usize, 2, 17, 4096] {
        write(dir, &format!("n_{n}"), &prng(5, n));
    }

    // Four bytes per step, so multi-megabyte corpora stay cheap to generate.
    let prng4 = |seed: u32, n: usize| -> Vec<u8> {
        let mut s = seed;
        let mut o: Vec<u8> = Vec::with_capacity(n + 4);
        while o.len() < n {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            o.extend_from_slice(&s.to_le_bytes());
        }
        o.truncate(n);
        o
    };
    write(dir, "stream/big_noise", &prng4(11, 3_000_000));
    // Long-range repeats BEYOND the dictionary, so matches fall out of the
    // window as it slides -- where an off-by-one in MoveBlock changes the parse.
    let blk = prng4(12, 250_000);
    let mut far = blk.clone();
    far.extend(prng4(13, 600_000));
    far.extend_from_slice(&blk);
    far.extend(prng4(14, 600_000));
    far.extend_from_slice(&blk);
    write(dir, "stream/big_far_repeat", &far);
    write(dir, "stream/big_text", &repeat(FOX, 60_000));
    let mut br = Vec::new();
    for i in 0..6000usize {
        br.extend(std::iter::repeat_n((i % 251) as u8, 1 + (i * 13) % 900));
    }
    write(dir, "stream/big_runs", &br);
}

/// `make_code` from the dispack harnesses — real i386 machine code with its
/// `E8` relocation placeholders rewritten into backward calls, so `detect()`
/// sees an executable rather than an object file.
///
/// The mask is `0xffffff`, not `0xffffffff`: this LCG is deliberately narrower
/// than [`prng`]'s, so the three address bytes stay inside 24 bits.
fn make_code(text: &[u8], reps: usize, seed: u32) -> Vec<u8> {
    let mut blob = text.repeat(reps);
    let mut s = seed;
    let mut i = 0usize;
    while blob.len() >= 5 && i < blob.len() - 5 {
        if blob[i] == 0xE8
            && blob[i + 1] == 0
            && blob[i + 2] == 0
            && blob[i + 3] == 0
            && blob[i + 4] == 0
        {
            s = s.wrapping_mul(1103515245).wrapping_add(12345) & 0xffffff;
            blob[i + 1] = (s & 0xff) as u8;
            blob[i + 2] = ((s >> 8) & 0xff) as u8;
            blob[i + 3] = ((s >> 16) & 0xff) as u8;
            blob[i + 4] = 0xFF;
        }
        i += 1;
    }
    blob
}

/// `dispack-check.sh`. `text` is the `.text` of an i386 object, or empty when
/// no i386 compiler was available — in which case the code inputs are skipped,
/// exactly as the Python skipped them.
fn dispack(dir: &std::path::Path, text: &[u8]) {
    if !text.is_empty() {
        write(dir, "code_small", &make_code(text, 30, 1));
        // Spans multiple chunks.
        write(dir, "code_big", &make_code(text, 200, 7));
        // Code then data.
        let mut cn = make_code(text, 60, 3);
        cn.extend(prng(9, 40000));
        write(dir, "code_noise", &cn);
    }
    write(dir, "noise", &prng(2, 300000));
    write(dir, "zeros", &vec![0u8; 200000]);
    write(dir, "text", &repeat(FOX, 4000));
    for n in [0usize, 1, 4, 5, 64, 4096, 65536] {
        write(dir, &format!("n_{n}"), &prng(4, n));
    }
}

/// The filler `dispack-filter-check.sh` builds for `detect()`, whose thresholds
/// are otherwise unfalsifiable.
///
/// `detect()` scans for `p[0]==0xE8` with `p[4]`/`p[5]` deciding the form —
/// exe is `p[4]==0xFF && p[5]!=0xFF`, obj is `p[4]==0x00 && p[5]!=0x00` — and
/// requires `e8/len >= 0.002`, `(exe+obj)/e8 >= 0.20` and `exe/e8 >= 0.01`.
/// The zero and ffff forms count as NEITHER and exist only so the `p[5]` halves
/// of those two tests are falsifiable.
#[allow(clippy::too_many_arguments)]
fn dispack_calls(
    n_e8: usize,
    total: usize,
    exe_frac: f64,
    obj_frac: f64,
    seed: u32,
    zero_frac: f64,
    tail_e8: bool,
    ffff_frac: f64,
) -> Vec<u8> {
    let mut b = prng(seed, total);
    // Scrub stray E8 so the count is exact.
    for x in b.iter_mut() {
        if *x == 0xE8 {
            *x = 0xE7;
        }
    }
    let n_exe = (n_e8 as f64 * exe_frac) as usize;
    let n_obj = (n_e8 as f64 * obj_frac) as usize;
    let n_zero = (n_e8 as f64 * zero_frac) as usize;
    let n_ffff = (n_e8 as f64 * ffff_frac) as usize;
    let step = (total / n_e8.max(1)).max(6);
    for k in 0..n_e8 {
        let i = k * step;
        if i + 6 > total {
            break;
        }
        b[i] = 0xE8;
        let (p4, p5) = if k < n_exe {
            (0xFF, 0x11)
        } else if k < n_exe + n_obj {
            (0x00, 0x22)
        } else if k < n_exe + n_obj + n_zero {
            (0x00, 0x00)
        } else if k < n_exe + n_obj + n_zero + n_ffff {
            (0xFF, 0xFF)
        } else {
            (0x7F, 0x33)
        };
        b[i + 4] = p4;
        b[i + 5] = p5;
    }
    // An E8 at len-5, whose p[5] would be buf[total] -- out of range. The 0xFF
    // is needed too: a scan running one position too far only READS p[5] when
    // p[4] is 0xFF or 0x00, otherwise `&&` short-circuits and the bug is
    // invisible. The first attempt at this input caught nothing for that reason.
    if tail_e8 && total >= 6 {
        b[total - 5] = 0xE8;
        b[total - 1] = 0xFF;
    }
    b
}

/// `dispack-filter-check.sh`.
fn dispack_filter(dir: &std::path::Path, text: &[u8]) {
    if !text.is_empty() {
        write(dir, "code_small", &make_code(text, 30, 1));
        write(dir, "code_big", &make_code(text, 200, 7));
        let mut cn = make_code(text, 60, 3);
        cn.extend(prng(9, 40000));
        write(dir, "code_noise", &cn);
    }
    // A plausible jump table: dwords that all land inside the block's address
    // range, which is what DetectJumpTable keys on (>= 3 consecutive entries).
    const BASE: u32 = 0x401000;
    let mut tbl = le32((0..64u32).map(|i| BASE + ((i * 17) % 0x8000)));
    match text.is_empty() {
        true => tbl.extend(prng(5, 4000)),
        false => tbl.extend_from_slice(text),
    }
    write(dir, "jumptable", &tbl);
    // Runs of EXACTLY TWO in-range dwords separated by an out-of-range one. The
    // threshold is "fewer than 3 is coincidence", so this is the only shape
    // that tells 3 from 2 -- with 64 consecutive entries both behave alike.
    let mut pairs = Vec::new();
    for i in 0..300u32 {
        pairs.extend_from_slice(&(BASE + ((i * 29) % 0x8000)).to_le_bytes());
        pairs.extend_from_slice(&(BASE + ((i * 31) % 0x8000)).to_le_bytes());
        pairs.extend_from_slice(&(0xF0000000u32 + i).to_le_bytes());
    }
    write(dir, "pairs", &pairs);
    // Makes the MTF SEARCH BOUND falsifiable. add_mtf pushes to the front, so
    // after inserting t0..t299 the table holds mtf[k] == t(299-k), putting t45
    // at index 254. Referencing t45 next is found with the real bound and
    // missed with one smaller -- and a miss emits a full 32-bit address instead
    // of a one-byte index, which changes the output.
    let mut many = le32((0..300u32).map(|i| BASE + i * 4));
    many.extend_from_slice(&(BASE + 45 * 4).to_le_bytes());
    write(dir, "mtf_boundary", &many);
    const TOT: usize = 100000;
    // e8 density either side of 0.002 (200 sites in 100000 bytes).
    write(dir, "det_dense_just_over", &dispack_calls(260, TOT, 0.50, 0.30, 11, 0.0, false, 0.0));
    write(dir, "det_dense_just_under", &dispack_calls(150, TOT, 0.50, 0.30, 12, 0.0, false, 0.0));
    // (exe+obj)/e8 straddling 0.20 CLOSELY -- 0.25 vs 0.15 leaves room to move
    // the threshold to 0.22 without reclassifying anything, which it did.
    write(dir, "det_callish_over", &dispack_calls(400, TOT, 0.080, 0.125, 13, 0.0, false, 0.0));
    write(dir, "det_callish_under", &dispack_calls(400, TOT, 0.080, 0.115, 14, 0.0, false, 0.0));
    write(dir, "det_exe_share_over", &dispack_calls(400, TOT, 0.0125, 0.60, 15, 0.0, false, 0.0));
    // exe_frac 0 but plenty of 0xFF/0xFF sites: correct code counts NO exe (so
    // DATA), while dropping the `p[5] != 0xFF` test counts them all (so EXE).
    write(dir, "det_exe_share_under", &dispack_calls(400, TOT, 0.0, 0.60, 16, 0.0, false, 0.30));
    write(dir, "det_obj_zero_tail", &dispack_calls(400, TOT, 0.05, 0.10, 17, 0.40, false, 0.0));
    write(dir, "det_e8_at_end", &dispack_calls(300, TOT, 0.50, 0.30, 18, 0.0, true, 0.0));
    write(dir, "noise", &prng(2, 200000));
    write(dir, "zeros", &vec![0u8; 100000]);
    write(dir, "text", &repeat(FOX, 3000));
    for n in [0usize, 1, 4, 5, 14, 15, 16, 64, 4096, 65536] {
        write(dir, &format!("n_{n}"), &prng(4, n));
    }
}

/// The SYNTHETIC half of `bcj-check.sh`'s corpus.
///
/// The real x86-64 binaries are found and copied in by the shell, which is
/// where host scanning belongs; everything here is generated.
fn bcj(dir: &std::path::Path) {
    // Plausible instruction soup with CALL/JMP rel32 whose displacements are
    // small and signed -- i.e. whose most significant byte is 0x00 or 0xFF,
    // which is exactly the Test86MSByte test that decides whether a branch is
    // converted. Without this an input can be full of E8 bytes and never reach
    // the conversion at all.
    let synth = |seed: u32, n: usize| -> Vec<u8> {
        let filler: [&[u8]; 11] = [
            b"\x55",
            b"\x48\x89\xe5",
            b"\x8b\x45\xfc",
            b"\x83\xc0\x01",
            b"\x89\x45\xfc",
            b"\x0f\xb6\x00",
            b"\x48\x83\xec\x20",
            b"\x31\xc0",
            b"\xc3",
            b"\x66\x90",
            b"\x0f\x1f\x40\x00",
        ];
        let mut s = seed;
        let mut out: Vec<u8> = Vec::with_capacity(n + 8);
        while out.len() < n {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            let r = ((s >> 16) & 0xffff) as usize;
            if r % 7 == 0 {
                out.push(match r & 1 {
                    0 => 0xE9,
                    _ => 0xE8,
                });
                // len(out) here INCLUDES the opcode just pushed.
                let disp = (r % 30000) as i64 - 15000 - out.len() as i64;
                out.extend_from_slice(&(disp as i32).to_le_bytes());
            } else {
                out.extend_from_slice(filler[r % filler.len()]);
            }
        }
        out.truncate(n);
        out
    };
    write(dir, "synth_small", &synth(1, 60_000));
    // Crosses the 256 KiB buffer twice.
    write(dir, "synth_big", &synth(2, 700_000));

    // Every alignment, and every spacing from 1 to 8 bytes. Spacings of 1-3 are
    // the only thing that makes prevMask nonzero, which is what selects the
    // kMaskToBitNumber / kMaskToAllowedStatus paths and the inner re-encoding
    // loop.
    let mut buf = vec![0x90u8; 40_000];
    let mut p = 16usize;
    for _ in 0..60 {
        for gap in 1..9usize {
            for op in [0xE8u8, 0xE9] {
                if p + 8 >= buf.len() {
                    break;
                }
                buf[p] = op;
                let v = ((p * 13) % 4096) as i32 - 2048;
                buf[p + 1..p + 5].copy_from_slice(&v.to_le_bytes());
                p += gap;
            }
        }
    }
    write(dir, "adjacent", &buf);
    // A solid run of branch bytes: prevMask saturates and every allowed-status
    // entry is visited.
    write(dir, "e8_run", &vec![0xe8u8; 30_000]);
    write(dir, "e9_run", &vec![0xe9u8; 30_000]);
    write(dir, "e8e9", &repeat(b"\xe8\xe9", 15_000));

    // Branches whose 5 bytes straddle the 256 KiB read boundary, where the
    // wrapper's remainder memmove and the carried prevMask have to agree with a
    // single-buffer run. LARGE_BUFFER_SIZE is 262144 (Compression.h:41).
    const LB: usize = 262144;
    for off in [LB - 5, LB - 4, LB - 3, LB - 2, LB - 1, LB, LB + 1] {
        let mut b = prng((off & 0xff) as u32, LB + 64);
        for x in b.iter_mut() {
            if *x & 0xFE == 0xE8 {
                *x = 0x90;
            }
        }
        b[off] = 0xE8;
        b[off + 1..off + 5].copy_from_slice(&(-64i32).to_le_bytes());
        b[off - 9] = 0xE9;
        b[off - 8..off - 4].copy_from_slice(&64i32.to_le_bytes());
        write(dir, &format!("boundary_{off}"), &b);
    }
    // Branch bytes in the last few positions, never convertible, must pass
    // through untouched.
    for tail in 1..9usize {
        let mut b = vec![0x90u8; 512];
        let n = b.len();
        b[n - tail] = 0xE8;
        write(dir, &format!("tail_{tail}"), &b);
    }
    write(dir, "noise", &prng(9, 200_000));
    write(dir, "zeros", &vec![0u8; 50_000]);
    write(dir, "ff", &vec![0xffu8; 50_000]);
    write(dir, "text", &repeat(FOX, 2000));
    for n in [0usize, 1, 2, 4, 5, 6, 7, 9, 10, 255, 4096] {
        write(dir, &format!("n_{n}"), &prng(3, n));
        let mut b = prng(4, n);
        if n >= 1 {
            b[0] = 0xE8;
        }
        if n >= 6 {
            b[5] = 0x00;
        }
        write(dir, &format!("e8_{n}"), &b);
    }
}

/// `crypto-check.sh` -- sizes either side of the 256 KB pipeline buffer, each
/// seeded by its own length so no two files share a prefix.
fn crypto(dir: &std::path::Path) {
    for n in [0usize, 1, 8, 15, 16, 17, 255, 4096, 262143, 262144, 262145, 300000] {
        write(dir, &format!("n_{n}"), &prng(n as u32 + 1, n));
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
        "grzip-big" => {
            let out = std::io::stdout();
            out.lock().write_all(&grzip_big()).expect("write");
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
    // An optional THIRD argument is auxiliary input a corpus needs but cannot
    // produce -- the dispack ones need the .text of a real i386 object, which
    // the shell compiles and difftest-util extracts.
    let aux = match args.next() {
        Some(p) => std::fs::read(p).unwrap_or_default(),
        None => Vec::new(),
    };
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
        "grzip" => grzip(&dir),
        "lzma2-mt" => lzma2_mt(&dir),
        "dict" => dict(&dir),
        "crypto" => crypto(&dir),
        "mm" => mm(&dir),
        "grzip-stage" => grzip_stage(&dir),
        "mmdet" => mmdet(&dir),
        "lzma-gap" => lzma_gap(&dir),
        "dispack" => dispack(&dir, &aux),
        "dispack-filter" => dispack_filter(&dir, &aux),
        "bcj" => bcj(&dir),
        other => {
            eprintln!("corpusgen: unknown corpus {other:?}");
            std::process::exit(2);
        }
    }
}
