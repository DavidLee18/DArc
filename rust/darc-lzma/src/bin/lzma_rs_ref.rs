//! Drive darc-lzma's stock-SDK encoder over stdin -> stdout, for comparison
//! against the pinned C in `rust/difftest/lzma_ref.cpp`.
//!
//! Deliberately minimal: it takes the same parameters the C driver takes, so the
//! measurement compares encoders and not two different parameter interpretations.
//! `matchFinder` and `algorithm` are accepted and *checked* rather than used --
//! this crate implements BT4 with the optimal parser only, and silently ignoring a
//! request for BT2 or the fast parser would make the comparison meaningless.

use std::io::{Read, Write};

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    if a.len() < 8 {
        eprintln!("usage: lzma_rs_ref dictSize lc lp pb fb mc matchFinder algorithm");
        std::process::exit(2);
    }
    let num = |i: usize| -> u32 {
        a[i].parse().unwrap_or_else(|_| {
            eprintln!("argument {i} is not a number: {}", a[i]);
            std::process::exit(2)
        })
    };
    let (dict_size, lc, lp, pb, fb, mc) = (num(0), num(1) as u8, num(2) as u8, num(3) as u8, num(4), num(5));
    let mf = num(6);
    let algorithm = num(7);

    // kBT4 == 2 in C_LZMA.cpp's `enum { kBT2, kBT3, kBT4, kHC4, kHT4 }`.
    if mf != 2 {
        eprintln!("darc-lzma implements BT4 only (matchFinder=2); got {mf}");
        std::process::exit(3);
    }
    if algorithm != 1 {
        eprintln!("darc-lzma implements the optimal parser only (algorithm=1); got {algorithm}");
        std::process::exit(3);
    }

    let mut input = Vec::new();
    match std::io::stdin().read_to_end(&mut input) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("reading stdin: {e}");
            std::process::exit(1);
        }
    }

    // DArc passes mc = 0 meaning "auto" and lets the SDK derive it; this crate
    // takes mc literally, so 0 makes cut_value 0 and the BT4 tree walk underflows.
    // Apply the SDK's own formula rather than inventing one --
    // LzmaEnc.c:99 (7z24):
    //
    //     if (p->mc == 0) p->mc = (16 + (fb >> 1)) >> (btMode ? 0 : 1);
    //
    // btMode is 1 here (BT4 is the only finder this crate implements), so the
    // shift is 0.
    let mc = if mc == 0 { 16 + (fb >> 1) } else { mc };

    // DArc always sets writeEndMark: C_LZMA.cpp says "FreeArc streams with EOPM
    // (unknown size)". Matching it is the point of this driver.
    let props = darc_lzma::LzmaProps { lc, lp, pb, dict_size, fb, mc, write_end_mark: true };
    let out = darc_lzma::encode(&input, &props);
    match std::io::stdout().write_all(&out) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("writing stdout: {e}");
            std::process::exit(1);
        }
    }
}
