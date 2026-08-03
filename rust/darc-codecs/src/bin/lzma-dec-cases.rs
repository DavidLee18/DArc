//! The LZMA decoder harness's corpus and case manifest.
//!
//! ```text
//!   lzma-dec-cases <workdir> <encoder> <quick>
//! ```
//!
//! Same shape as `lzma2-cases`: content files, streams built by running the
//! encoder and then mutating what it produced, and one manifest that is the
//! harness's whole case list. `lzma-decode-check.sh` keeps the running, the
//! comparing and the sabotage rehearsal.
//!
//! `<workdir>/content` and `<workdir>/stream` must exist, and `<workdir>/cenc`
//! must be the C encoder driver — the corpus uses that binary's own bytes as its
//! "a real executable" content, which is the only shape here with the byte
//! statistics an archiver actually meets.
//!
//! Manifest fields, space separated:
//!
//! ```text
//!   group name dict lc lp pb fb mc mf algo chunk stream expect expfile
//!   consumed produced maxrss tags
//! ```

use std::io::Write;
use std::path::{Path, PathBuf};

/// The encoder parameter tuple, in the order the driver takes it on argv.
type Params = [u64; 8];

/// DArc's OWN default (`C_LZMA.cpp:249-257`): matchFinder kHT4, a five-byte hash
/// CHAIN, not the BT4 a hand-written case reaches for.
const DFLT: Params = [67108864, 3, 0, 2, 32, 0, 4, 1];
const BT4: Params = [1048576, 3, 0, 2, 32, 0, 2, 1];
const SMALL: Params = [65536, 3, 0, 2, 32, 0, 2, 1];
const DMIN: Params = [4096, 3, 0, 2, 273, 0, 2, 1];
const LITS: Params = [1048576, 0, 2, 0, 32, 0, 2, 1];
const FAST: Params = [1048576, 4, 0, 2, 32, 0, 3, 0];

fn prng(seed: u32, n: usize) -> Vec<u8> {
    let mut s = seed;
    let mut o = Vec::with_capacity(n + 4);
    while o.len() < n {
        s = s.wrapping_mul(1103515245).wrapping_add(12345);
        o.extend_from_slice(&s.to_le_bytes());
    }
    o.truncate(n);
    o
}

fn write(path: &Path, data: &[u8]) {
    std::fs::File::create(path)
        .unwrap_or_else(|e| panic!("create {}: {e}", path.display()))
        .write_all(data)
        .unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

// ── a minimal transcription of the SDK's range coder ────────────────────────
// A real encoder cannot emit an out-of-range distance, so the three sides of
// `LzmaDec.c:537` need streams written by hand. This is `RangeEnc_ShiftLow`,
// `RC_BIT`, `LenEnc_Encode`, the pos-slot tree and `WriteEndMarker`
// (`LzmaEnc.c:685-2157`), and no more. It is SELF-CHECKED by the harness: every
// case whose distance is legal must decode to the plaintext computed here, and
// if the range coder were wrong none of them would decode at all.

const K_PROB_INIT: u16 = 1024;
const K_TOP: u64 = 1 << 24;
const K_LITERAL_NEXT_STATES: [usize; 12] = [0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5];
const K_MATCH_NEXT_STATES: [usize; 12] = [7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10];

struct Rc {
    low: u64,
    range: u64,
    cache: u8,
    cache_size: u64,
    out: Vec<u8>,
}

impl Rc {
    fn new() -> Self {
        Rc { low: 0, range: 0xFFFF_FFFF, cache: 0, cache_size: 0, out: Vec::new() }
    }

    fn shift_low(&mut self) {
        let low = self.low & 0xFFFF_FFFF;
        let high = ((self.low >> 32) & 0xFF) as u8;
        self.low = (low << 8) & 0xFFFF_FFFF;
        if low < 0xFF00_0000 || high != 0 {
            self.out.push(self.cache.wrapping_add(high));
            self.cache = ((low >> 24) & 0xFF) as u8;
            if self.cache_size == 0 {
                return;
            }
            let h = high.wrapping_add(0xFF);
            loop {
                self.out.push(h);
                self.cache_size -= 1;
                if self.cache_size == 0 {
                    return;
                }
            }
        }
        self.cache_size += 1;
    }

    fn norm(&mut self) {
        if self.range < K_TOP {
            self.range = (self.range << 8) & 0xFFFF_FFFF;
            self.shift_low();
        }
    }

    fn bit(&mut self, probs: &mut [u16], i: usize, b: u32) {
        let ttt = u64::from(probs[i]);
        let nb = (self.range >> 11) * ttt;
        match b == 0 {
            true => {
                self.range = nb;
                probs[i] = (ttt + ((2048 - ttt) >> 5)) as u16;
            }
            false => {
                self.low += nb;
                self.range -= nb;
                probs[i] = (ttt - (ttt >> 5)) as u16;
            }
        }
        self.norm();
    }

    fn direct(&mut self, b: u32) {
        self.range >>= 1;
        if b != 0 {
            self.low += self.range;
        }
        self.norm();
    }
}

fn pos_slot(dist: u32) -> u32 {
    if dist < 2 {
        return dist;
    }
    let n = 31 - dist.leading_zeros();
    (n << 1) | ((dist >> (n - 1)) & 1)
}

/// Emits: N literals, one simple match with a CHOSEN `reps[0]`, then EOPM.
struct Hand {
    lc: u32,
    lp: u32,
    pb_mask: usize,
    rc: Rc,
    is_match: Vec<Vec<u16>>,
    is_rep: Vec<u16>,
    pos_slot_enc: Vec<Vec<u16>>,
    pos_encoders: Vec<u16>,
    pos_align: Vec<u16>,
    len_low: Vec<u16>,
    len_high: Vec<u16>,
    lit: Vec<u16>,
    state: usize,
    pos: usize,
    prev: u8,
}

impl Hand {
    fn new(lc: u32, lp: u32, pb: u32) -> Self {
        let p = |n: usize| vec![K_PROB_INIT; n];
        Hand {
            lc,
            lp,
            pb_mask: (1usize << pb) - 1,
            rc: Rc::new(),
            is_match: (0..12).map(|_| p(16)).collect(),
            is_rep: p(12),
            pos_slot_enc: (0..4).map(|_| p(64)).collect(),
            pos_encoders: p(128),
            pos_align: p(16),
            len_low: p(256),
            len_high: p(256),
            lit: p(0x300 << (lc + lp)),
            state: 0,
            pos: 0,
            prev: 0,
        }
    }

    fn literal(&mut self, byte: u8) {
        let ps = self.pos & self.pb_mask;
        let s = self.state;
        self.rc.bit(&mut self.is_match[s], ps, 0);
        let base = match self.pos == 0 {
            true => 0usize,
            false => {
                let ls = ((self.pos & ((1usize << self.lp) - 1)) << self.lc)
                    + (u32::from(self.prev) >> (8 - self.lc)) as usize;
                0x300 * ls
            }
        };
        let mut sym = u32::from(byte) | 0x100;
        loop {
            let i = base + (sym >> 8) as usize;
            let b = (sym >> 7) & 1;
            sym <<= 1;
            self.rc.bit(&mut self.lit, i, b);
            if sym >= 0x10000 {
                break;
            }
        }
        self.state = K_LITERAL_NEXT_STATES[self.state];
        self.pos += 1;
        self.prev = byte;
    }

    fn len(&mut self, sym: u32, ps: usize) {
        let mut sym = sym;
        let mut base = 0usize;
        if sym >= 8 {
            self.rc.bit(&mut self.len_low, 0, 1);
            base = 8;
            if sym >= 16 {
                self.rc.bit(&mut self.len_low, 8, 1);
                let mut s = (sym - 16) | 0x100;
                loop {
                    let i = (s >> 8) as usize;
                    let b = (s >> 7) & 1;
                    s <<= 1;
                    self.rc.bit(&mut self.len_high, i, b);
                    if s >= 0x10000 {
                        return;
                    }
                }
            }
            sym -= 8;
        }
        self.rc.bit(&mut self.len_low, base, 0);
        let o = base + (ps << 4);
        let b = (sym >> 2) & 1;
        self.rc.bit(&mut self.len_low, o + 1, b);
        let mut m = 2 + b as usize;
        let b = (sym >> 1) & 1;
        self.rc.bit(&mut self.len_low, o + m, b);
        m = (m << 1) + b as usize;
        let b = sym & 1;
        self.rc.bit(&mut self.len_low, o + m, b);
    }

    fn emit_match(&mut self, rep0: u32, length: u32) {
        let ps = self.pos & self.pb_mask;
        let s = self.state;
        self.rc.bit(&mut self.is_match[s], ps, 1);
        self.rc.bit(&mut self.is_rep, s, 0);
        self.state = K_MATCH_NEXT_STATES[self.state];
        self.len(length - 2, ps);
        let dist = rep0 - 1;
        let slot = pos_slot(dist);
        let pi = match length < 5 {
            true => (length - 2) as usize,
            false => 3,
        };
        let mut sym = slot + 64;
        while sym < 4096 {
            let i = (sym >> 6) as usize;
            let b = (sym >> 5) & 1;
            sym <<= 1;
            self.rc.bit(&mut self.pos_slot_enc[pi], i, b);
        }
        if dist >= 4 {
            let footer = (slot >> 1) - 1;
            match dist < 128 {
                true => {
                    let base = ((2 | (slot & 1)) << footer) as usize;
                    let (mut m, mut d) = (1usize, dist);
                    for _ in 0..footer {
                        let b = d & 1;
                        d >>= 1;
                        self.rc.bit(&mut self.pos_encoders, base + m, b);
                        m = (m << 1) | b as usize;
                    }
                }
                false => {
                    let mut p2 = ((dist | 0xF) << (32 - footer)) as u64 & 0xFFFF_FFFF;
                    loop {
                        self.rc.direct((p2 >> 31) as u32);
                        p2 = (p2 << 1) & 0xFFFF_FFFF;
                        if p2 == 0xF000_0000 {
                            break;
                        }
                    }
                    let (mut m, mut d) = (1usize, dist);
                    for _ in 0..4 {
                        let b = d & 1;
                        d >>= 1;
                        self.rc.bit(&mut self.pos_align, m, b);
                        m = (m << 1) + b as usize;
                    }
                }
            }
        }
        self.pos += length as usize;
    }

    fn finish(mut self) -> Vec<u8> {
        let ps = self.pos & self.pb_mask;
        let s = self.state;
        self.rc.bit(&mut self.is_match[s], ps, 1);
        self.rc.bit(&mut self.is_rep, s, 0);
        self.state = K_MATCH_NEXT_STATES[self.state];
        self.len(0, ps);
        let mut m = 1usize;
        while m < 64 {
            self.rc.bit(&mut self.pos_slot_enc[0], m, 1);
            m = (m << 1) + 1;
        }
        for _ in 0..26 {
            self.rc.direct(1);
        }
        let mut m = 1usize;
        while m < 16 {
            self.rc.bit(&mut self.pos_align, m, 1);
            m = (m << 1) + 1;
        }
        for _ in 0..5 {
            self.rc.shift_low();
        }
        self.rc.out
    }
}

/// A hand-built stream and the plaintext it decodes to.
fn hand(nlits: usize, rep0: u32, mlen: u32) -> (Vec<u8>, Vec<u8>) {
    let mut e = Hand::new(3, 0, 2);
    let mut data: Vec<u8> = Vec::new();
    for i in 0..nlits {
        let b = ((i * 37 + 11) & 0xFF) as u8;
        e.literal(b);
        data.push(b);
    }
    e.emit_match(rep0, mlen);
    // The match may overlap what it is producing, so this reads `data` as it
    // grows -- exactly as the decoder's ring buffer does.
    let base = data.len() as i64 - i64::from(rep0);
    for k in 0..i64::from(mlen) {
        let src = base + k;
        let b = match src >= 0 && (src as usize) < data.len() {
            true => data[src as usize],
            false => 0,
        };
        data.push(b);
    }
    (e.finish(), data)
}

// ── the builder ─────────────────────────────────────────────────────────────

struct Cases {
    content: PathBuf,
    stream: PathBuf,
    encoder: String,
    man: Vec<String>,
}

/// Everything the manifest says about one case beyond its parameters.
#[derive(Clone)]
struct Expect {
    expect: String,
    expfile: String,
    cons: String,
    prod: String,
    rss: String,
    tags: String,
}

impl Expect {
    fn any(tags: &str) -> Self {
        Expect {
            expect: "any".to_owned(),
            expfile: "-".to_owned(),
            cons: "-".to_owned(),
            prod: "-".to_owned(),
            rss: "-".to_owned(),
            tags: tags.to_owned(),
        }
    }
    fn verdict(v: &str, tags: &str) -> Self {
        Expect { expect: v.to_owned(), ..Expect::any(tags) }
    }
    fn accept(expfile: &str, tags: &str) -> Self {
        Expect { expect: "accept".to_owned(), expfile: expfile.to_owned(), ..Expect::any(tags) }
    }
    fn prod(mut self, p: &str) -> Self {
        self.prod = p.to_owned();
        self
    }
    fn cons(mut self, c: &str) -> Self {
        self.cons = c.to_owned();
        self
    }
    fn rss(mut self, r: u64) -> Self {
        self.rss = r.to_string();
        self
    }
}

impl Cases {
    fn content(&self, name: &str, data: &[u8]) -> String {
        let p = self.content.join(name);
        write(&p, data);
        p.to_string_lossy().into_owned()
    }

    fn stream_file(&self, name: &str, data: &[u8]) -> String {
        let p = self.stream.join(name);
        write(&p, data);
        p.to_string_lossy().into_owned()
    }

    /// Run the encoder over a content file. Aborts the whole run on failure: a
    /// corpus that silently lost cases is how a sweep reports a clean pass over
    /// configurations it never touched.
    fn encode(&self, name: &str, cpath: &str, params: Params) -> String {
        let p = self.stream.join(name);
        let input = std::fs::File::open(cpath).unwrap_or_else(|e| panic!("open {cpath}: {e}"));
        let out = std::fs::File::create(&p).unwrap_or_else(|e| panic!("create {p:?}: {e}"));
        let args: Vec<String> = params.iter().map(u64::to_string).collect();
        let st = std::process::Command::new(&self.encoder)
            .args(&args)
            .stdin(input)
            .stdout(out)
            .stderr(std::process::Stdio::null())
            .status()
            .unwrap_or_else(|e| panic!("spawning {}: {e}", self.encoder));
        if !st.success() {
            eprintln!("encoder failed for {name} with {args:?}");
            std::process::exit(1);
        }
        p.to_string_lossy().into_owned()
    }

    fn case(&mut self, group: &str, name: &str, params: Params, stream: &str, e: &Expect) {
        let ps: Vec<String> = params.iter().map(u64::to_string).collect();
        self.man.push(format!(
            "{group} {name} {} 1 {stream} {} {} {} {} {} {}",
            ps.join(" "),
            e.expect,
            e.expfile,
            e.cons,
            e.prod,
            e.rss,
            e.tags
        ));
    }
}

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    let usage = "usage: lzma-dec-cases <workdir> <encoder> <quick>";
    let w = PathBuf::from(a.first().expect(usage));
    let encoder = a.get(1).expect(usage).clone();
    let quick = a.get(2).map(String::as_str) == Some("1");

    let mut c = Cases {
        content: w.join("content"),
        stream: w.join("stream"),
        encoder,
        man: Vec::new(),
    };

    // ── (d) the content corpus ──────────────────────────────────────────────
    // Empty and 1-byte are the degenerate ends; all-zeros and highly-repetitive
    // make the parse almost entirely matches; incompressible makes it almost
    // entirely literals; and a real binary is the only one of these with the
    // byte statistics an archiver actually meets.
    let mut real_binary = std::fs::read(w.join("cenc")).expect("the C encoder driver");
    real_binary.truncate(300000);

    let repeat: Vec<u8> = (0..900u32)
        .flat_map(|i| std::iter::repeat_n((i % 251) as u8, 1 + (i as usize * 13) % 400))
        .collect();
    let runs: Vec<u8> = (0..700u32)
        .flat_map(|i| std::iter::repeat_n((i % 7) as u8, 1 + (i as usize * 29) % 900))
        .collect();
    let text: Vec<u8> = b"the quick brown fox jumps over the lazy dog. ".repeat(2700);

    let contents: Vec<(&str, Vec<u8>)> = vec![
        ("empty", Vec::new()),
        ("one", vec![0x5a]),
        ("zeros", vec![0u8; 50000]),
        ("noise", prng(7, 50000)),
        ("repeat", repeat),
        ("text", text),
        ("binary", real_binary),
        ("runs", runs),
    ];
    for (n, d) in &contents {
        c.content(n, d);
    }

    // ── (a) round-trip over the parameter space ─────────────────────────────
    let params: [(&str, Params); 6] = [
        ("dflt", DFLT),
        ("bt4", BT4),
        ("small", SMALL),
        ("dmin", DMIN),
        ("lits", LITS),
        ("fast", FAST),
    ];
    for (pn, pp) in &params {
        for (cn, cd) in &contents {
            let cp = c.content.join(cn).to_string_lossy().into_owned();
            let nm = format!("rt_{pn}_{cn}");
            let st = c.encode(&nm, &cp, *pp);
            let e = Expect::accept(&cp, "roundtrip").prod(&cd.len().to_string());
            c.case("A", &nm, *pp, &st, &e);
        }
    }

    // ── (d) dictionary window wrap ──────────────────────────────────────────
    // The decoder's ring buffer is exactly dicSize bytes (LzmaDec_Allocate), so
    // nothing under that size can tell a working wrap from a missing one -- and
    // the wrap is where `dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)]`
    // earns its keep. Sizes straddle the boundary by one byte in each direction,
    // then run many multiples past it.
    for ds in [4096usize, 65536] {
        let pp: Params = [ds as u64, 3, 0, 2, 32, 0, 2, 1];
        let mut sizes = vec![ds - 1, ds, ds + 1, 2 * ds, 2 * ds + 1];
        if !quick {
            sizes.push(ds * 31);
        }
        for sz in sizes {
            // A period that does not divide the dictionary, so matches land at a
            // different phase of the ring on every lap.
            let cycle: Vec<u8> =
                prng(3, 1021).iter().copied().cycle().take(sz).collect();
            for (shape, data) in [("noise", prng((ds + sz) as u32, sz)), ("cycle", cycle)] {
                let nm = format!("wrap_{ds}_{sz}_{shape}");
                let cp = c.content(&nm, &data);
                let st = c.encode(&nm, &cp, pp);
                let tag = match sz > ds {
                    true => "wrap",
                    false => "roundtrip",
                };
                let e = Expect::accept(&cp, tag).prod(&sz.to_string());
                c.case("A", &nm, pp, &st, &e);
            }
        }
    }

    // ── (b) EOPM position ───────────────────────────────────────────────────
    // The sharpest cheap test in the whole harness. Junk after the marker must
    // be invisible: same output, same produced count, and the SAME consumed
    // count -- the decoder has to stop at the marker rather than at end of
    // input.
    let junk = [1usize, 2, 20, 65536];
    let eopm_bases: [(&str, &str); 6] = [
        ("small", "text"),
        ("small", "noise"),
        ("dmin", "repeat"),
        ("dflt", "binary"),
        ("bt4", "one"),
        ("bt4", "empty"),
    ];
    for (pn, cn) in &eopm_bases {
        let pp = params.iter().find(|(n, _)| n == pn).map(|(_, p)| *p).expect("named params");
        let cp = c.content.join(cn).to_string_lossy().into_owned();
        let base = format!("eopm_{pn}_{cn}");
        let st = c.stream.join(format!("rt_{pn}_{cn}"));
        let raw = std::fs::read(&st).expect("the round-trip stream");
        let csize = std::fs::metadata(&cp).expect("content").len();
        let e = Expect::accept(&cp, "eopm_junk").prod(&csize.to_string());
        c.case("B", &base, pp, &st.to_string_lossy(), &e);
        for j in junk {
            // 0xA5, not zeros: a zero byte is a plausible continuation of a
            // range coder, so junk made of zeros can be decoded rather than
            // ignored and would make this test pass for the wrong reason.
            let nm = format!("{base}_junk{j}");
            let mut d = raw.clone();
            d.extend(std::iter::repeat_n(0xa5u8, j));
            let p = c.stream_file(&nm, &d);
            let e = Expect::accept(&cp, "eopm_junk")
                .cons(&format!("same:{base}"))
                .prod(&format!("same:{base}"));
            c.case("B", &nm, pp, &p, &e);
        }
    }

    // The marker landing exactly on the 64 KiB output-buffer boundary
    // (C_LZMA.cpp:170), and one byte either side of it -- the point at which the
    // decode loop returns to the caller with a full buffer and must then
    // discover that the next thing in the stream is the end.
    for sz in [65535usize, 65536, 65537, 131071, 131072, 131073] {
        let cycle: Vec<u8> = prng(5, 977).iter().copied().cycle().take(sz).collect();
        for (shape, data) in [("noise", prng(sz as u32, sz)), ("cycle", cycle)] {
            let nm = format!("bound_{sz}_{shape}");
            let cp = c.content(&nm, &data);
            let st = c.encode(&nm, &cp, BT4);
            let e = Expect::accept(&cp, "eopm_boundary").prod(&sz.to_string());
            c.case("B", &nm, BT4, &st, &e);
        }
    }

    // A final match that overruns the 64 KiB output limit by 1..273 bytes,
    // forcing LzmaDec_WriteRem (LzmaDec.c:616-650) to carry the remainder into
    // the next call. fb=273 caps matches at the maximum length, and a 61-byte
    // period guarantees the parse takes maximum-length matches all the way to
    // the end, so for every k the last match starts at 65536+k-273 -- before the
    // boundary for k<273, exactly on it for k=273.
    let wr_pp: Params = [65536, 3, 0, 2, 273, 0, 2, 1];
    let period = prng(17, 61);
    let ks: Vec<usize> = match quick {
        true => vec![1, 2, 3, 137, 271, 272, 273],
        false => (1..274).collect(),
    };
    for k in ks {
        let sz = 65536 + k;
        let nm = format!("writerem_{k}");
        let data: Vec<u8> = period.iter().copied().cycle().take(sz).collect();
        let cp = c.content(&nm, &data);
        let st = c.encode(&nm, &cp, wr_pp);
        let e = Expect::accept(&cp, "writerem").prod(&sz.to_string());
        c.case("B", &nm, wr_pp, &st, &e);
    }

    // ── (c) malformed input ─────────────────────────────────────────────────
    // The base victim: small enough that thousands of mutations are cheap, big
    // enough that its stream contains literals, matches, rep-matches and an
    // EOPM.
    let vic_pp = SMALL;
    let vic_data: Vec<u8> = [
        b"the quick brown fox jumps over the lazy dog. ".repeat(60),
        prng(21, 900),
        b"abcabcabcabc".repeat(60),
    ]
    .concat();
    let vic_c = c.content("victim", &vic_data);
    let vic_s = c.encode("victim", &vic_c, vic_pp);
    let victim = std::fs::read(&vic_s).expect("the victim stream");
    let vlen = vic_data.len().to_string();
    let e = Expect::accept(&vic_c, "roundtrip,dual").prod(&vlen);
    c.case("C", "victim_ok", vic_pp, &vic_s, &e);

    // props[0] across 0..255. LzmaProps_Decode (LzmaDec.c:1273) rejects
    // d >= 9*5*5 = 225 outright with SZ_ERROR_UNSUPPORTED, which C_LZMA.cpp:165
    // turns into FREEARC_ERRCODE_INVALID_COMPRESSOR (-2). Below 225 the byte
    // always decomposes into some (lc, lp, pb), so the props are ACCEPTED even
    // when they are not the ones the stream was written with -- the failure then
    // comes later and looks completely different (-7). Distinguishing those two
    // rejections is the point.
    for b in 0..256u64 {
        let (lc, d) = (b % 9, b / 9);
        let (lp, pb) = (d % 5, d / 5);
        let pp: Params = [65536, lc, lp, pb, 32, 0, 2, 1];
        let nm = format!("props_{b}");
        let e = match b {
            225.. => Expect::verdict("reject:-2", "props"),
            // the stream's own props: lc3 lp0 pb2
            93 => Expect::accept(&vic_c, "props").prod(&vlen),
            // Accepted as properties; the stream then decodes to nonsense or
            // fails. What must NOT happen is -2, which would mean the props were
            // refused.
            _ => Expect::any("props"),
        };
        c.case("C", &nm, pp, &vic_s, &e);
    }

    // The same axis as a user can actually reach it: `-mlzma:8m:pbN` sets pb
    // directly. MEASURED, not assumed -- the arithmetic truncates, so pb=5 is
    // the value that trips the >= 225 check (25*9+3 = 228) while pb=9 wraps to
    // (45*9+3) & 255 = 152 and is accepted as a different lc/lp/pb triple
    // entirely.
    for pbarg in 0..16u64 {
        let v = ((pbarg * 5) * 9 + 3) & 0xFF;
        let pp: Params = [65536, 3, 0, pbarg, 32, 0, 2, 1];
        let e = match (v >= 225, pbarg == 2) {
            (true, _) => Expect::verdict("reject:-2", "props"),
            (false, true) => Expect::accept(&vic_c, "props"),
            (false, false) => Expect::any("props"),
        };
        c.case("C", &format!("pbarg_{pbarg}"), pp, &vic_s, &e);
    }

    // dictSize edges. LzmaProps_Decode clamps anything below LZMA_DIC_MIN up to
    // 4096 (LzmaDec.c:1268), so 0/1/4095 are not rejections -- they silently
    // become a 4 KiB window, which decodes correctly for any content that fits
    // in it and fails for anything that does not. 0xFFFFFFFF is the one that
    // must be watched rather than merely classified: it must not turn into a
    // 4 GiB allocation.
    for ds in [0u64, 1, 4095, 4096] {
        let pp: Params = [ds, 3, 0, 2, 32, 0, 2, 1];
        c.case("C", &format!("dict_{ds}"), pp, &vic_s, &Expect::any("dictsize,dual"));
    }
    c.case(
        "C",
        "dict_4294967295",
        [4294967295, 3, 0, 2, 32, 0, 2, 1],
        &vic_s,
        &Expect::any("dictsize").rss(1 << 30),
    );

    // A match distance beyond the decoder's window, reached the way an archive
    // reaches it: encode with a dictionary large enough to find a far repeat,
    // then decode with a smaller one. The third copy of block A is 400 KB behind
    // the first, so at dictSize 65536 the distance exceeds checkDicSize and
    // LzmaDec.c:537 must reject.
    let (far_a, far_b) = (prng(31, 200000), prng(32, 200000));
    let far_data: Vec<u8> = [far_a.clone(), far_b.clone(), far_a.clone()].concat();
    let far_c = c.content("far", &far_data);
    let far_s = c.encode("far", &far_c, BT4);
    let e = Expect::accept(&far_c, "roundtrip,wrap").prod(&far_data.len().to_string());
    c.case("C", "far_ok", BT4, &far_s, &e);
    c.case(
        "C",
        "far_dict_too_small",
        SMALL,
        &far_s,
        &Expect::verdict("reject:-7", "distover,dual"),
    );

    // The first byte of an LZMA stream is the range coder's cache and is always
    // 0 (RangeEnc_Init leaves cache=0). LzmaDec.c:966 rejects anything else.
    for v in [1u8, 0x7f, 0x80, 0xff] {
        let mut m = victim.clone();
        m[0] = v;
        let nm = format!("firstbyte_{v}");
        let p = c.stream_file(&nm, &m);
        c.case("C", &nm, vic_pp, &p, &Expect::verdict("reject:-7", "firstbyte,dual"));
    }

    // A rep-match as the very first symbol is unrepresentable -- there is
    // nothing to repeat -- and rather than test for it in the hot loop the SDK
    // checks the range coder's initial code against kBadRepCode
    // (LzmaDec.c:979-982), which is 0xC0000000 - 0x400 by the compile-time
    // assertion at :666.
    const KBADREP: u32 = 0xC000_0000 - 0x400;
    for (label, code) in
        [("eq", KBADREP), ("hi", 0xC000_0000), ("max", 0xFFFF_FFFF), ("below", KBADREP - 1)]
    {
        let mut d = victim[..1].to_vec();
        d.extend_from_slice(&code.to_be_bytes());
        d.extend_from_slice(&victim[5..]);
        let nm = format!("badrep_{label}");
        let p = c.stream_file(&nm, &d);
        // `below` is one short of the threshold: it must NOT be rejected by this
        // check. Without it the test would pass for a decoder that rejects
        // everything.
        let e = match label == "below" {
            true => Expect::any("badrep,dual"),
            false => Expect::verdict("reject:-7", "badrep,dual"),
        };
        c.case("C", &nm, vic_pp, &p, &e);
    }

    // ---- hand-built streams, for the three sides of LzmaDec.c:537 ----------
    // `distance >= (checkDicSize == 0 ? processedPos : checkDicSize)` with
    // distance = reps[0] - 1, so the boundary is reps[0] <= the limit.
    // checkDicSize is 0 until processedPos reaches dicSize, which is why the same
    // comparison has two completely different right-hand sides.
    const DS: usize = 4096;
    let hand_cases: [(&str, usize, u32, u32, &str); 7] = [
        ("hand_pos1_ok", 1, 1, 4, "accept"),        // distance 0 < 1
        ("hand_pos1_over", 1, 2, 4, "reject:-7"),   // distance 1 >= 1
        ("hand_mid_ok", 100, 100, 4, "accept"),
        ("hand_mid_over", 100, 101, 4, "reject:-7"),
        ("hand_dic_minus1", DS + 104, DS as u32 - 1, 4, "accept"),
        ("hand_dic_exact", DS + 104, DS as u32, 4, "accept"),
        ("hand_dic_plus1", DS + 104, DS as u32 + 1, 4, "reject:-7"),
    ];
    let hpp: Params = [DS as u64, 3, 0, 2, 32, 0, 2, 1];
    for (nm, nl, r0, ml, exp) in hand_cases {
        let (st, plain) = hand(nl, r0, ml);
        let p = c.stream_file(nm, &st);
        let e = match exp == "accept" {
            true => {
                let cp = c.content(nm, &plain);
                Expect::accept(&cp, "distance,dual").prod(&plain.len().to_string())
            }
            false => Expect::verdict(exp, "distance,dual"),
        };
        c.case("C", nm, hpp, &p, &e);
    }

    // ── truncation ──────────────────────────────────────────────────────────
    // EVERY prefix length of a short valid stream. A decoder that reports
    // success on a prefix has invented the tail; one that produces more
    // plaintext than the whole stream held has invented output. Both are gated:
    // `le:` below.
    let short_data = b"the quick brown fox jumps over the lazy dog. ".repeat(9);
    let short_c = c.content("short", &short_data);
    let short_s = c.encode("short", &short_c, vic_pp);
    let short = std::fs::read(&short_s).expect("the short stream");
    let slen = short_data.len().to_string();
    let e = Expect::accept(&short_c, "roundtrip").prod(&slen);
    c.case("C", "short_ok", vic_pp, &short_s, &e);
    for n in 0..short.len() {
        let nm = format!("trunc_short_{n}");
        let p = c.stream_file(&nm, &short[..n]);
        c.case("C", &nm, vic_pp, &p, &Expect::any("trunc,dual").prod(&format!("le:{slen}")));
    }
    // And on the larger victim: the first few bytes, where the range-coder init
    // itself is incomplete, and the last few, where the end marker is.
    let tv: Vec<usize> = (0..6).chain((1..21).map(|k| victim.len() - k)).collect();
    for n in tv {
        let nm = format!("trunc_victim_{n}");
        let p = c.stream_file(&nm, &victim[..n]);
        c.case("C", &nm, vic_pp, &p, &Expect::any("trunc,dual").prod(&format!("le:{vlen}")));
    }

    // ── mutation ────────────────────────────────────────────────────────────
    // Single-bit and single-byte damage. No expectation beyond "does not crash"
    // and "does not exceed the cap": what these are FOR is the
    // cross-implementation accept/reject comparison, where a disagreement is the
    // finding. In C-only mode they still earn their place through the `dual`
    // sampling below, which requires the answer not to depend on how the input
    // was buffered.
    let (nbit, nbyte) = match quick {
        true => (60usize, 20usize),
        false => (2000, 500),
    };
    for i in 0..nbit {
        let off = (i * 7919) % victim.len();
        let mut m = victim.clone();
        m[off] ^= 1 << (i % 8);
        let nm = format!("bit_{i}");
        let p = c.stream_file(&nm, &m);
        let tags = match i % 5 == 0 {
            true => "mutate,dual",
            false => "mutate",
        };
        c.case("C", &nm, vic_pp, &p, &Expect::any(tags));
    }
    for i in 0..nbyte {
        let off = (i * 4093) % victim.len();
        let mut m = victim.clone();
        m[off] = m[off].wrapping_add((1 + i) as u8);
        let nm = format!("byte_{i}");
        let p = c.stream_file(&nm, &m);
        let tags = match i % 5 == 0 {
            true => "mutate,dual",
            false => "mutate",
        };
        c.case("C", &nm, vic_pp, &p, &Expect::any(tags));
    }

    write(&w.join("manifest"), format!("{}\n", c.man.join("\n")).as_bytes());
    eprintln!("corpus: {} cases", c.man.len());
}
