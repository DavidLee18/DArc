//! End-to-end round-trip coverage: `decode_raw(encode(x)) == x` across a broad
//! corpus. Pure Rust — no C oracle required. This proves the encoder emits valid
//! LZMA that the ported decoder reads back exactly; **byte-exactness against the C
//! `LzmaEnc_MemEncode`** is a separate guarantee verified out-of-tree (see
//! `docs/comparing-against-the-c-oracle.md`). Runs on a plain `cargo test`.

use crate::{LzmaProps, decode_raw, encode};

/// Deterministic xorshift PRNG bytes (stable across platforms/runs).
fn prng(n: usize, seed: u32) -> Vec<u8> {
    let mut x = seed | 1;
    (0..n)
        .map(|_| {
            x ^= x << 13;
            x ^= x >> 17;
            x ^= x << 5;
            (x >> 24) as u8
        })
        .collect()
}

/// A broad corpus mirroring the encoder's hazards and the sizes CHD uses,
/// including inputs large enough to trigger the periodic price rebuilds and the
/// `kNumOpts` overflow path in the optimal parser.
fn corpus() -> Vec<(&'static str, Vec<u8>)> {
    let text = b"the quick brown fox jumps over the lazy dog. ";
    let mut cases: Vec<(&'static str, Vec<u8>)> = vec![
        ("empty", Vec::new()),
        ("one", vec![0x42]),
        ("two", vec![0x42, 0x43]),
        ("zeros_100", vec![0u8; 100]),
        ("zeros_4k", vec![0u8; 4096]),
        ("zeros_65k", vec![0u8; 65536]),
        ("ff_1000", vec![0xFFu8; 1000]),
        ("text", text.repeat(64)),
        ("text_big", text.repeat(2000)),
        ("abc", b"abcabcabcabc".repeat(200)),
        ("counter", (0..6000u32).map(|i| i as u8).collect()),
        ("counter_big", (0..70000u32).map(|i| i as u8).collect()),
        ("random_4k", prng(4096, 1)),
        ("random_19584", prng(19584, 7)),
        ("random_65k", prng(65536, 99)),
    ];
    // Random prefix then its repeat (a long match spanning the middle).
    let r = prng(8000, 1234);
    let mut mixed = r.clone();
    mixed.extend_from_slice(&r);
    cases.push(("mixed", mixed));
    // Text with periodic random noise, ~CHD hunk size.
    let mut noisy = text.repeat(500);
    let noise = prng(noisy.len(), 55);
    for (i, b) in noisy.iter_mut().enumerate() {
        if i % 37 == 0 {
            *b = noise[i];
        }
    }
    cases.push(("noisy_text", noisy));
    cases
}

#[test]
fn round_trip_corpus() {
    for (name, input) in corpus() {
        let reduce = (input.len() as u32).max(1);
        let props = LzmaProps::for_level(8, reduce);
        let stream = encode(&input, &props);
        // Every LZMA range-coder stream begins with the initial 0x00 cache byte.
        assert_eq!(
            stream.first(),
            Some(&0u8),
            "[{name}] stream must start with 0x00"
        );
        let decoded = decode_raw(&stream, &props.decoder_props(), input.len());
        assert_eq!(
            decoded,
            input,
            "[{name}] round-trip mismatch ({} bytes)",
            input.len()
        );
    }
}

#[test]
fn round_trip_chd_hunk_sizes() {
    // The exact props CHD derives per hunk, on incompressible data.
    for &hunk in &[4096u32, 19584, 65536] {
        let input = prng(hunk as usize, hunk);
        let props = LzmaProps::chd_for_hunk(hunk);
        let stream = encode(&input, &props);
        let decoded = decode_raw(&stream, &props.decoder_props(), input.len());
        assert_eq!(decoded, input, "hunk {hunk} round-trip mismatch");
    }
}
