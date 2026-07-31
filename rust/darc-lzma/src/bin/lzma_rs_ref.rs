//! Drive darc-lzma's encoder over stdin -> stdout, for comparison against the
//! pinned C in `rust/difftest/lzma_ref.cpp`.
//!
//! Deliberately minimal: it takes the same parameters the C driver takes, so the
//! measurement compares encoders and not two different parameter interpretations.
//! `matchFinder` and `algorithm` are accepted and *checked* rather than used --
//! this crate implements BT4 with the optimal parser only, and silently ignoring a
//! request for BT2 or the fast parser would make the comparison meaningless.
//!
//! It goes through `encode_stream` rather than `encode`, so the differential test
//! exercises the same streaming path DArc's codec entry point uses -- including the
//! short reads a pipe produces, which the in-memory wrapper never generates.

use std::io::{Read, Write};

use darc_lzma::{InStream, LzmaProps, MatchFinderKind, OutStream, StreamError};

/// stdin as an `InStream`. `Ok(0)` means end of stream, so a `read` returning 0
/// from a non-empty pipe would truncate -- `Read::read` only does that at EOF.
struct StdinIn(std::io::Stdin);

impl InStream for StdinIn {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
        match self.0.read(buf) {
            Ok(n) => Ok(n),
            Err(e) => {
                eprintln!("reading stdin: {e}");
                Err(StreamError(1))
            }
        }
    }
}

/// stdout as an `OutStream`.
struct StdoutOut(std::io::Stdout);

impl OutStream for StdoutOut {
    fn write(&mut self, data: &[u8]) -> Result<(), StreamError> {
        match self.0.write_all(data) {
            Ok(()) => Ok(()),
            Err(e) => {
                eprintln!("writing stdout: {e}");
                Err(StreamError(2))
            }
        }
    }
}

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
    let (dict_size, lc, lp, pb, fb, mc) = (
        num(0),
        num(1) as u8,
        num(2) as u8,
        num(3) as u8,
        num(4),
        num(5),
    );
    let mf = num(6);
    let algorithm = num(7);

    // The DArc matchFinder ID, resolved through the same table C_LZMA.cpp uses.
    // Refused rather than defaulted: the C's `default:` arm silently picks BT4, and
    // a driver that quietly measured a different finder than the one asked for would
    // report agreement it never tested.
    let mf = match MatchFinderKind::from_stream(mf as i32) {
        Some(k) => k,
        None => {
            eprintln!("no such matchFinder id: {mf} (valid: 0=BT2 1=BT3 2=BT4 3=HC4 4=HT4/Hc5)");
            std::process::exit(3);
        }
    };
    // algorithm 0 is the fast parser, 1 the optimal one (LzmaEnc.c:568 --
    // `fastMode = (algo == 0)`). Both are implemented; anything else is not a value
    // C_LZMA.cpp can produce, so refuse rather than guess.
    let fast_mode = match algorithm {
        0 => true,
        1 => false,
        other => {
            eprintln!("algorithm must be 0 (fast) or 1 (optimal); got {other}");
            std::process::exit(3);
        }
    };

    // DArc passes mc = 0 meaning "auto" and lets the SDK derive it; this crate takes
    // mc literally, so 0 makes cut_value 0 and the search's cut counter underflows.
    // The formula is the SDK's own (LzmaEnc.c:99) and it is finder-dependent -- the
    // hash chains get half what the trees do, via the `>> (btMode ? 0 : 1)`.
    let mc = if mc == 0 { mf.auto_mc(fb) } else { mc };

    // DArc always sets writeEndMark: C_LZMA.cpp says "FreeArc streams with EOPM
    // (unknown size)". Matching it is the point of this driver.
    let props = LzmaProps {
        lc,
        lp,
        pb,
        dict_size,
        fb,
        mc,
        mf,
        fast_mode,
        num_threads: 1,
        write_end_mark: true,
    };

    let mut source = StdinIn(std::io::stdin());
    let mut sink = StdoutOut(std::io::stdout());
    match darc_lzma::encode_stream(&mut source, &mut sink, &props) {
        Ok(()) => {}
        // The stream implementations above already reported the cause.
        Err(StreamError(_)) => std::process::exit(1),
    }
    match std::io::stdout().flush() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("flushing stdout: {e}");
            std::process::exit(1);
        }
    }
}
