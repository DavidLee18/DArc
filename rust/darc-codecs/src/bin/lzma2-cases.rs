//! The LZMA2 harness's corpus and case manifests.
//!
//! ```text
//!   lzma2-cases <workdir> <driver> <quick>
//! ```
//!
//! Not a `corpusgen` corpus: this writes content files AND synthesises LZMA2
//! streams byte by byte, some of them by running the encoder driver and then
//! mutating what it produced. The manifests it emits are the harness's whole
//! case list, so the shell keeps the running and the comparing and this keeps
//! the construction — the split the rest of the difftest tree already uses.
//!
//! `<workdir>/content` and `<workdir>/stream` must exist; `lzma2-check.sh`
//! creates them alongside its own scratch directories.
//!
//! Manifest fields, space separated, exactly as `lzma2-check.sh` reads them:
//!
//! ```text
//!   enc: name dict lc lp pb fb mc mf algo expect content tags
//!   dec: name stream chunk expect expfile cons tags
//! ```

use sha2::{Digest, Sha256};
use std::io::Write;
use std::path::{Path, PathBuf};

/// The encoder parameter tuple, in the order the driver takes it on argv.
type Params = [u64; 8];

/// DArc's OWN defaults (`C_LZMA2.cpp:201-211`). matchFinder is kHT4 — a
/// five-byte hash CHAIN, not BT4 — because no preset in `Compression.hs` names
/// one, so every `-mlzma2` archive DArc has written used the configuration a
/// hand-written case would never pick.
const DFLT: Params = [64 << 20, 3, 0, 2, 32, 0, 4, 1];

/// `LZMA2_UNPACK_SIZE_MAX` (`Lzma2Enc.c:29`), the cap that ends a subblock.
const M2: usize = 2 << 20;
/// The EFFECTIVE boundary, which is lower and worth straddling too:
/// `LzmaEnc.c:2666` stops at `processed + kNumOpts + 300 >= maxUnpackSize`.
const EFF: usize = M2 - 2048 - 300;

/// Genuinely incompressible, and DETERMINISTIC — a SHA-256 counter stream
/// rather than random bytes, so the corpus is the same on every run and a
/// divergence can be reproduced.
fn rnd(seed: u32, n: usize) -> Vec<u8> {
    let mut o = Vec::with_capacity(n + 32);
    let mut i = 0u32;
    while o.len() < n {
        o.extend_from_slice(&Sha256::digest(format!("{seed}:{i}").as_bytes()));
        i += 1;
    }
    o.truncate(n);
    o
}

/// An LCG's low bits are strongly structured, so this is "incompressible
/// enough to be copied" without being random — a different shape from [`rnd`].
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

fn text(n: usize) -> Vec<u8> {
    const B: &[u8] = b"the quick brown fox jumps over the lazy dog. ";
    B.iter().copied().cycle().take(n).collect()
}

/// Alternating compressible and incompressible megabytes. This is the shape
/// that makes `useCopyBlock` (`Lzma2Enc.c:154`) flip repeatedly inside ONE
/// stream, which no single-shape input does.
fn mixed(n: usize) -> Vec<u8> {
    let mut o = Vec::with_capacity(n + (1 << 20));
    let mut i = 0u32;
    while o.len() < n {
        match i.is_multiple_of(2) {
            true => o.extend(std::iter::repeat_n(0u8, 1 << 20)),
            false => o.extend_from_slice(&rnd(500 + i, 1 << 20)),
        }
        i += 1;
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

/// The builder's whole mutable state: where things go, and what has been said
/// about them.
struct Cases {
    content: PathBuf,
    stream: PathBuf,
    driver: String,
    enc: Vec<String>,
    dec: Vec<String>,
}

impl Cases {
    /// A plaintext file, returned as the path the manifests name it by.
    fn content(&self, name: &str, data: &[u8]) -> String {
        let p = self.content.join(name);
        write(&p, data);
        p.to_string_lossy().into_owned()
    }

    /// A stream file, returned as the path the manifests name it by.
    fn stream(&self, name: &str, data: &[u8]) -> String {
        let p = self.stream.join(name);
        write(&p, data);
        p.to_string_lossy().into_owned()
    }

    fn enc_case(&mut self, name: &str, params: Params, expect: &str, cpath: &str, tags: &str) {
        let ps: Vec<String> = params.iter().map(u64::to_string).collect();
        self.enc.push(format!("{name} {} {expect} {cpath} {tags}", ps.join(" ")));
    }

    // The arguments ARE the manifest's seven fields, in the order the shell
    // reads them. Bundling them into a struct would only move the list.
    #[expect(clippy::too_many_arguments, reason = "one parameter per manifest field")]
    fn dec_case(
        &mut self,
        name: &str,
        stream: &str,
        chunk: u32,
        expect: &str,
        expfile: &str,
        cons: &str,
        tags: &str,
    ) {
        self.dec.push(format!("{name} {stream} {chunk} {expect} {expfile} {cons} {tags}"));
    }

    /// A rejection or don't-care case, whose plaintext and consumed count are
    /// both "-".
    fn dec_bare(&mut self, name: &str, stream: &str, expect: &str, tags: &str) {
        self.dec_case(name, stream, 1, expect, "-", "-", tags);
    }

    /// Run the encoder driver over `data` and keep what it wrote.
    fn encode(&self, name: &str, data: &[u8], params: Params) -> (String, Vec<u8>) {
        let mut args: Vec<String> = params.iter().map(u64::to_string).collect();
        args.push("enc".to_owned());
        let mut child = std::process::Command::new(&self.driver)
            .args(&args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .unwrap_or_else(|e| panic!("corpus: spawning {} failed: {e}", self.driver));
        child
            .stdin
            .take()
            .expect("stdin")
            .write_all(data)
            .unwrap_or_else(|e| panic!("corpus: feeding {name} failed: {e}"));
        let out = child.wait_with_output().expect("wait");
        match out.status.success() {
            true => (self.stream(name, &out.stdout), out.stdout),
            false => {
                eprintln!("corpus: encoding {name} failed");
                std::process::exit(1);
            }
        }
    }
}

/// `[props] chunk... [0x00] tail`.
fn stream_bytes(props: u8, chunks: &[Vec<u8>]) -> Vec<u8> {
    let mut o = vec![props];
    for c in chunks {
        o.extend_from_slice(c);
    }
    o.push(0);
    o
}

/// An LZMA2 uncompressed chunk: control, then the size biased by one, big
/// endian, then the payload.
fn copy_chunk(data: &[u8], reset: bool) -> Vec<u8> {
    let n = data.len() - 1;
    let mut o = vec![u8::from(!reset) + 1, ((n >> 8) & 0xFF) as u8, (n & 0xFF) as u8];
    o.extend_from_slice(data);
    o
}

fn size_tags(n: usize) -> String {
    let mut t: Vec<&str> = Vec::new();
    if n <= 2 {
        t.push("size_tiny");
    }
    if [EFF - 1, EFF, EFF + 1, M2 - 1, M2, M2 + 1].contains(&n) {
        t.push("size_edge2m");
    }
    if [2 * M2, 3 * M2 + 12345].contains(&n) {
        t.push("size_mult2m");
    }
    t.join(",")
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let w = PathBuf::from(args.first().expect("usage: lzma2-cases <workdir> <driver> <quick>"));
    let driver = args.get(1).expect("usage: lzma2-cases <workdir> <driver> <quick>").clone();
    let quick = args.get(2).map(String::as_str) == Some("1");

    let mut c = Cases {
        content: w.join("content"),
        stream: w.join("stream"),
        driver,
        enc: Vec::new(),
        dec: Vec::new(),
    };

    // ── sizes ───────────────────────────────────────────────────────────────
    // 2*M2 stays even in quick mode: it is the only size that carries the
    // size_mult2m coverage category, and dropping it makes the run fail the
    // coverage gate rather than merely run faster.
    let sizes: Vec<usize> = match quick {
        true => vec![0, 1, 65536, M2 - 1, M2, M2 + 1, 2 * M2],
        false => vec![
            0,
            1,
            2,
            65535,
            65536,
            65537,
            EFF - 1,
            EFF,
            EFF + 1,
            M2 - 1,
            M2,
            M2 + 1,
            2 * M2,
            3 * M2 + 12345,
        ],
    };

    // ── content shapes at every size ────────────────────────────────────────
    // `rnd` and `prng` are the ones that trigger the uncompressed-chunk path;
    // `mixed` is the one that makes the decision flip repeatedly within a
    // stream.
    for n in &sizes {
        let n = *n;
        for (shape, tag) in [
            ("text", "compressible"),
            ("rnd", "incompressible"),
            ("prng", "incompressible"),
            ("mixed", "mixed"),
        ] {
            // mixed needs at least two megabyte halves.
            if shape == "mixed" && n < (1 << 21) {
                continue;
            }
            let data = match shape {
                "text" => text(n),
                "rnd" => rnd(7, n),
                "prng" => prng(3, n),
                _ => mixed(n),
            };
            let nm = format!("sz_{shape}_{n}");
            let cp = c.content(&nm, &data);
            let st = size_tags(n);
            let tags = match st.is_empty() {
                true => tag.to_owned(),
                false => format!("{tag},{st}"),
            };
            c.enc_case(&nm, DFLT, "accept", &cp, &tags);
        }
    }

    // One case per shape through a bintree finder as well, so the size sweep is
    // not entirely one match finder.
    for n in [65536, M2, M2 + 1] {
        let cp = c.content.join(format!("sz_rnd_{n}")).to_string_lossy().into_owned();
        c.enc_case(
            &format!("szbt_rnd_{n}"),
            [1 << 20, 3, 0, 2, 32, 0, 2, 1],
            "accept",
            &cp,
            "incompressible,size_edge2m",
        );
    }

    // ── a compact content set for the parameter sweep ───────────────────────
    let p_mix: Vec<u8> = [text(120000), rnd(11, 120000), text(60000)].concat();
    let pset: [(&str, Vec<u8>); 3] =
        [("p_text", text(300000)), ("p_rnd", rnd(9, 300000)), ("p_mix", p_mix)];
    let ppaths: Vec<(String, String)> =
        pset.iter().map(|(nm, d)| ((*nm).to_owned(), c.content(nm, d))).collect();

    // match finder x parser. All five ids and both parsers, at DArc's own
    // dictionary.
    for mf in 0..5u64 {
        for algo in [0u64, 1] {
            sweep(
                &mut c,
                &ppaths,
                &format!("mf{mf}a{algo}"),
                [64 << 20, 3, 0, 2, 32, 0, mf, algo],
                &format!("mf{mf},algo{algo},mfsweep"),
            );
        }
    }

    // dictSize. Anything below 2 MiB is the ONLY thing that exercises the
    // keepWindowSize widening at LzmaEnc.c:2729 -- `beforeSize = keepWindowSize
    // - dictSize` with keepWindowSize = LZMA2_KEEP_WINDOW_SIZE = 2 MiB -- so
    // half of these are deliberately small.
    for ds in [4096u64, 65536, 262144, 1 << 20, (1 << 21) - 1, 1 << 21, 8 << 20, 64 << 20] {
        let tag = match ds < (1 << 21) {
            true => "dict_small",
            false => "dict_big",
        };
        sweep(&mut c, &ppaths, &format!("d{ds}"), [ds, 3, 0, 2, 32, 0, 4, 1], tag);
    }

    for fb in [5u64, 32, 64, 273] {
        sweep(&mut c, &ppaths, &format!("fb{fb}"), [1 << 20, 3, 0, 2, fb, 0, 2, 1], "fb");
    }
    for mc in [0u64, 8, 64] {
        sweep(&mut c, &ppaths, &format!("mc{mc}"), [1 << 20, 3, 0, 2, 32, mc, 2, 1], "mc");
    }

    // lc/lp/pb across the lc + lp <= 4 region Lzma2Enc_SetProps enforces
    // (Lzma2Enc.c:470-471), and then the region it must refuse.
    for (lc, lp, pb) in [
        (0u64, 0u64, 0u64),
        (0, 4, 0),
        (4, 0, 0),
        (2, 2, 0),
        (3, 0, 2),
        (1, 3, 4),
        (0, 2, 1),
        (3, 1, 2),
        (2, 1, 3),
    ] {
        sweep(
            &mut c,
            &ppaths,
            &format!("lclp{lc}{lp}{pb}"),
            [1 << 20, lc, lp, pb, 32, 0, 2, 1],
            "lclp",
        );
    }
    let first = ppaths[0].1.clone();
    for (lc, lp) in [(4u64, 1u64), (3, 2), (0, 5), (5, 0), (9, 0), (4, 4)] {
        c.enc_case(
            &format!("reject_lclp_{lc}_{lp}"),
            [1 << 20, lc, lp, 2, 32, 0, 2, 1],
            "reject",
            &first,
            "lclp_reject",
        );
    }

    // ── decode corpus ───────────────────────────────────────────────────────
    // A victim with both chunk kinds in it: text first (so the block opens with
    // an LZMA chunk, mode 3), then incompressible (so copy chunks follow).
    let vic_plain: Vec<u8> = [text(150000), rnd(21, 150000), text(50000)].concat();
    let vic_c = c.content("victim", &vic_plain);
    let (vic_s, victim) = c.encode("victim", &vic_plain, DFLT);
    let vlen = victim.len().to_string();
    c.dec_case("victim_ok", &vic_s, 1, "accept", &vic_c, &vlen, "roundtrip");

    // An all-copy victim, so the copy-chunk decode path has a base case of its
    // own.
    let cp_plain = rnd(22, 120000);
    let cp_c = c.content("copyvictim", &cp_plain);
    let (cp_s, copyvic) = c.encode("copyvictim", &cp_plain, DFLT);
    let cplen = copyvic.len().to_string();
    c.dec_case("copyvictim_ok", &cp_s, 1, "accept", &cp_c, &cplen, "roundtrip");

    // ---- hand-built copy streams -------------------------------------------
    // The decoder's 64 KiB copy chunk, which lzma2_compress provably never emits
    // (see the header). Anything else that writes LZMA2 does, so a Rust decoder
    // has to handle it, and nothing in a round-trip corpus would ever show it.
    let many: Vec<Vec<u8>> = (0..20u32).map(|i| rnd(38 + i, 4096)).collect();
    let hand: [(&str, Vec<Vec<u8>>); 5] = [
        ("hand_copy1", vec![rnd(31, 1)]),
        ("hand_copy64k", vec![rnd(32, 65536)]),
        ("hand_copy64k_x3", vec![rnd(33, 65536), rnd(34, 65536), rnd(35, 7)]),
        ("hand_copy_max_min", vec![rnd(36, 65536), rnd(37, 1)]),
        ("hand_copy_many", many),
    ];
    for (nm, payloads) in &hand {
        let plain: Vec<u8> = payloads.concat();
        let cpath = c.content(nm, &plain);
        let chunks: Vec<Vec<u8>> =
            payloads.iter().enumerate().map(|(i, p)| copy_chunk(p, i == 0)).collect();
        let st = stream_bytes(0x18, &chunks);
        let p = c.stream(nm, &st);
        c.dec_case(nm, &p, 1, "accept", &cpath, &st.len().to_string(), "dec_copy64k");
    }

    // Junk after the 0x00 terminator must be invisible: same plaintext, and the
    // SAME consumed count. chunk=1 throughout, because lzma2_decompress reads
    // through a 64 KiB buffer (C_LZMA2.cpp:127) -- feeding one byte at a time is
    // what makes the terminator's position observable from outside.
    let junk_plain = rnd(41, 1000);
    let junk_c = c.content("junkbase", &junk_plain);
    let junk_st = stream_bytes(0x18, &[copy_chunk(&junk_plain, true)]);
    let jp = c.stream("junk_base", &junk_st);
    c.dec_case("junk_base", &jp, 1, "accept", &junk_c, &junk_st.len().to_string(), "dec_junk");
    for j in [1usize, 2, 20, 65536] {
        // 0xA5, not zeros: a zero byte is a valid LZMA2 terminator, so junk made
        // of zeros would be consumed as structure and the test would pass for
        // the wrong reason.
        let mut d = junk_st.clone();
        d.extend(std::iter::repeat_n(0xa5u8, j));
        let p = c.stream(&format!("junk_{j}"), &d);
        c.dec_case(&format!("junk_{j}"), &p, 1, "accept", &junk_c, "same:junk_base", "dec_junk");
    }
    // The same for a real encoder stream, which ends inside an LZMA chunk rather
    // than inside a copy chunk.
    c.dec_case("junk_vic_base", &vic_s, 1, "accept", &vic_c, &vlen, "dec_junk");
    for j in [1usize, 20, 65536] {
        let mut d = victim.clone();
        d.extend(std::iter::repeat_n(0xa5u8, j));
        let p = c.stream(&format!("junk_vic_{j}"), &d);
        c.dec_case(
            &format!("junk_vic_{j}"),
            &p,
            1,
            "accept",
            &vic_c,
            "same:junk_vic_base",
            "dec_junk",
        );
    }

    // ---- rejection, one case per documented error edge ---------------------

    // (1) a stream opening with control 2 (Lzma2Dec.c:110-114). needInitLevel
    // starts at 0xE0, and an uncompressed control that is not 1 with
    // needInitLevel == 0xE0 is an immediate error -- the first chunk MUST reset
    // the dictionary.
    let p = c.stream("rej_ctl2", &stream_bytes(0x18, &[copy_chunk(&rnd(51, 100), false)]));
    c.dec_bare("rej_ctl2", &p, "reject", "reject_ctl2");
    // ...and control 3, which trips the `b > 2` half of the same test.
    let mut ch = copy_chunk(&rnd(52, 100), false);
    ch[0] = 3;
    let p = c.stream("rej_ctl3", &stream_bytes(0x18, &[ch]));
    c.dec_bare("rej_ctl3", &p, "reject", "reject_ctl2");

    // (2) an LZMA chunk whose control is below needInitLevel
    // (Lzma2Dec.c:117-118). The first chunk of a stream needs control >= 0xE0;
    // 0x80/0xA0/0xC0 are the three LZMA modes that are not "reset dic", and all
    // three must be refused HERE while being perfectly legal later in a stream.
    for ctl in [0x80u8, 0xA0, 0xC0] {
        let mut m = victim.clone();
        m[1] = (m[1] & 0x1F) | ctl;
        let nm = format!("rej_needinit_{ctl:02x}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_needinit");
    }

    // (3) prop >= 225 in a chunk's prop byte (Lzma2Dec.c:148-149). The victim's
    // first chunk has control 0xE? so it carries a prop byte, at offset 6
    // (props, control, u1, u0, p1, p0, prop).
    assert!(victim[1] & 0x40 != 0, "victim's first chunk must carry a prop byte");
    for v in [225u8, 226, 240, 255] {
        let mut m = victim.clone();
        m[6] = v;
        let nm = format!("rej_prop{v}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_prop225");
    }
    // The neighbour that must NOT be refused by that check: 224 decomposes to
    // lc=8, which the lc+lp test below then refuses -- so it is still a
    // rejection, but for the other reason. 93 is the victim's own lc3 lp0 pb2
    // and must be accepted.
    c.dec_case("prop93_ok", &vic_s, 1, "accept", &vic_c, &vlen, "reject_prop225");

    // (4) a prop byte with lc + lp > 4 (Lzma2Dec.c:154-155). encode is
    // (pb*5 + lp)*9 + lc, so these are chosen by inverse decomposition and each
    // is below 225 -- they must fail the SECOND test, not the first.
    for v in [8u8, 13, 31, 224, 134] {
        let (lc, lp) = (v % 9, (v / 9) % 5);
        assert!(v < 225 && lc + lp > 4, "{v} {lc} {lp}");
        let mut m = victim.clone();
        m[6] = v;
        let nm = format!("rej_lclp{v}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_lclp");
    }
    // And the boundary that must be ACCEPTED as properties: lc + lp == 4
    // exactly. Accepted as PROPERTIES; the stream then decodes to nonsense or
    // fails on a later symbol. What must not happen is a refusal of the property
    // byte, and the cross-implementation comparison is what makes `any` worth
    // running.
    for (lc, lp, pb) in [(4u8, 0u8, 0u8), (0, 4, 0), (2, 2, 1)] {
        let mut m = victim.clone();
        m[6] = (pb * 5 + lp) * 9 + lc;
        let nm = format!("prop_lclp4_{lc}{lp}{pb}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "any", "reject_lclp");
    }

    // (5) the LEADING prop byte > 40 at allocate (Lzma2Dec.c:59-60). This is
    // DArc's own framing byte, not the SDK's, so a port that never wrote it
    // would fail here rather than silently.
    //
    // 40 itself is deliberately absent: it means dicSize = 0xFFFFFFFF and makes
    // LzmaDec_Allocate ask for 4 GiB. Testing the rejection does not require
    // rehearsing an out-of-memory condition on the developer's machine.
    for v in [41u8, 42, 100, 200, 255] {
        let mut m = victim.clone();
        m[0] = v;
        let nm = format!("rej_dicprop{v}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_dicprop");
    }
    // Small leading props that ARE legal: 0 is a 4 KiB window, which is smaller
    // than the stream was written with, so this is accepted as a property and
    // then fails on a distance -- a different failure entirely, and one worth
    // comparing across implementations.
    for v in [0u8, 1, 18, 24, 39] {
        let mut m = victim.clone();
        m[0] = v;
        let nm = format!("dicprop{v}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "any", "reject_dicprop");
    }

    // (6) truncation at each header-byte boundary. Six of them for an LZMA-first
    // stream (control, unpack hi, unpack lo, pack hi, pack lo, prop) plus the
    // leading LZMA2 prop byte before them, and three more for a copy-first
    // stream, whose header is shorter. A decoder that reports success on any of
    // these has invented the tail.
    for n in 0..9usize {
        let nm = format!("rej_trunc_lzma_{n}");
        let p = c.stream(&nm, &victim[..n]);
        c.dec_bare(&nm, &p, "reject", "reject_trunc");
    }
    for n in 0..6usize {
        let nm = format!("rej_trunc_copy_{n}");
        let p = c.stream(&nm, &copyvic[..n]);
        c.dec_bare(&nm, &p, "reject", "reject_trunc");
    }
    // Truncation deeper in, where the header is complete and the PAYLOAD is
    // short -- the same failure as (7) but reached by removing bytes rather than
    // by lying about the size.
    for n in [10, 50, victim.len() / 2, victim.len() - 1] {
        let nm = format!("rej_trunc_body_{n}");
        let p = c.stream(&nm, &victim[..n]);
        c.dec_bare(&nm, &p, "reject", "reject_trunc");
    }

    // (7) a packSize that overruns the input. The pack field is stored biased by
    // one at offsets 4..5 of an LZMA chunk; raising it makes the decoder ask for
    // bytes the stream does not contain, and C_LZMA2.cpp:168 turns "needs more
    // input with nothing left" into BAD_COMPRESSED_DATA.
    for (hi, lo, nm) in [(0xFFu8, 0xFFu8, "max"), (0x80, 0x00, "half"), (0x00, 0xFF, "small")] {
        let mut m = victim.clone();
        m[4] = hi;
        m[5] = lo;
        let nm = format!("rej_packover_{nm}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_packover");
    }
    // The copy chunk's equivalent: an unpack size larger than the bytes present.
    let mut m = copyvic.clone();
    m[2] = 0xFF;
    m[3] = 0xFF;
    let p = c.stream("rej_packover_copy", &m);
    c.dec_bare("rej_packover_copy", &p, "any", "reject_packover");

    // (8) an LZMA chunk payload whose first byte is not 0x00 (Lzma2Dec.c:414-419
    // in the parse path; the decode path rejects it through the range coder).
    // The victim's first chunk payload begins right after its prop byte, at
    // offset 7.
    for v in [0x01u8, 0x80, 0xFF] {
        let mut m = victim.clone();
        m[7] = v;
        let nm = format!("rej_firstbyte_{v:02x}");
        let p = c.stream(&nm, &m);
        c.dec_bare(&nm, &p, "reject", "reject_firstbyte");
    }

    // (9) an empty input, and a stream that is nothing but the terminator.
    let p = c.stream("rej_empty", b"");
    c.dec_bare("rej_empty", &p, "reject", "reject_trunc");
    let empty_c = c.content("empty", b"");
    let p = c.stream("only_term", &[0x18, 0x00]);
    c.dec_case("only_term", &p, 1, "accept", &empty_c, "2", "roundtrip");

    write(&w.join("enc-manifest"), format!("{}\n", c.enc.join("\n")).as_bytes());
    write(&w.join("dec-manifest"), format!("{}\n", c.dec.join("\n")).as_bytes());
    eprintln!("corpus: {} encode cases, {} decode cases", c.enc.len(), c.dec.len());
}

/// One case per parameter set over the compact content set.
fn sweep(c: &mut Cases, ppaths: &[(String, String)], prefix: &str, params: Params, tags: &str) {
    for (nm, cp) in ppaths {
        let cp = cp.clone();
        c.enc_case(&format!("{prefix}_{nm}"), params, "accept", &cp, tags);
    }
}
