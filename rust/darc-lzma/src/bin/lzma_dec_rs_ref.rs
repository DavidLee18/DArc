//! Drive darc-lzma's decoder over stdin -> stdout, for comparison against DArc's
//! real `lzma_decompress` in `rust/difftest/lzma_dec_ref.cpp`.
//!
//! The contract is that driver's, exactly, so `lzma-decode-check.sh` can hand the
//! same case to both and compare four things: the verdict, the FreeArc error code,
//! the plaintext, and the consumed/produced byte counts.
//!
//!     lzma_dec_rs_ref  dictSize lc lp pb fb mc matchFinder algorithm [readChunk]
//!
//! The first eight are the *encoder's* argv unchanged, which is what lets one case
//! description feed both the encode and decode drivers. Only `dictSize`, `pb`, `lc`
//! and `lp` are read here — `C_LZMA.cpp:158` builds the five decoder property bytes
//! from those four and nothing else, so a decoder cannot depend on the match finder
//! or the parser. The rest are accepted and ignored on purpose rather than rejected.
//!
//! `readChunk` caps each read, making the end-of-payload marker's position
//! observable: DArc's C reads through a 64 KiB buffer (`C_LZMA.cpp:169`), so bytes
//! handed to the callback are not bytes the decoder consumed.

use std::io::{Read, Write};

use darc_lzma::{InStream, LzmaDecodeError, OutStream, StreamError};

/// The FreeArc codes this reports, from `Compression/Compression.h:20-27`.
const FREEARC_OK: i32 = 0;
const FREEARC_ERRCODE_GENERAL: i32 = -1;
const FREEARC_ERRCODE_INVALID_COMPRESSOR: i32 = -2;
const FREEARC_ERRCODE_NOT_ENOUGH_MEMORY: i32 = -5;
const FREEARC_ERRCODE_BAD_COMPRESSED_DATA: i32 = -7;

/// stdin, served in bounded chunks so the harness can vary the read granularity.
struct ChunkedIn {
    data: Vec<u8>,
    pos: usize,
    chunk: usize,
}

impl InStream for ChunkedIn {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
        let mut want = buf.len().min(self.data.len() - self.pos);
        if self.chunk != 0 {
            want = want.min(self.chunk);
        }
        buf[..want].copy_from_slice(&self.data[self.pos..self.pos + want]);
        self.pos += want;
        Ok(want)
    }
}

/// stdout, counting everything decoded even past the write cap.
///
/// The cap exists so a case that legitimately decodes gigabytes does not have to
/// materialize them; `produced` still counts the whole stream, because that is the
/// number being compared against the C.
struct CountingOut {
    sink: std::io::Stdout,
    produced: u64,
    cap: u64,
    capped: bool,
}

impl OutStream for CountingOut {
    fn write(&mut self, data: &[u8]) -> Result<(), StreamError> {
        let room = self.cap.saturating_sub(self.produced);
        let take = (data.len() as u64).min(room) as usize;
        if take < data.len() {
            self.capped = true;
        }
        self.produced += data.len() as u64;
        match self.sink.write_all(&data[..take]) {
            Ok(()) => Ok(()),
            Err(e) => {
                eprintln!("writing stdout: {e}");
                Err(StreamError(FREEARC_ERRCODE_GENERAL))
            }
        }
    }
}

/// The same mapping `darc_lzma_decompress` applies, so the two agree on `rc` and not
/// merely on accept-vs-reject.
fn errcode(e: LzmaDecodeError) -> i32 {
    match e {
        LzmaDecodeError::UnsupportedProps => FREEARC_ERRCODE_INVALID_COMPRESSOR,
        LzmaDecodeError::DataError | LzmaDecodeError::TruncatedInput => {
            FREEARC_ERRCODE_BAD_COMPRESSED_DATA
        }
        LzmaDecodeError::NotEnoughMemory => FREEARC_ERRCODE_NOT_ENOUGH_MEMORY,
        LzmaDecodeError::Stream(StreamError(code)) => code,
        LzmaDecodeError::Internal => FREEARC_ERRCODE_GENERAL,
    }
}

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    if a.len() < 8 {
        eprintln!(
            "usage: lzma_dec_rs_ref dictSize lc lp pb fb mc matchFinder algorithm [readChunk]"
        );
        std::process::exit(2);
    }
    // strtoll-equivalent, not atoi: dictSize=4294967295 must stay 0xFFFFFFFF rather
    // than clamping, because that value is one of the cases under test.
    let num = |i: usize| -> i64 {
        match a.get(i).map(|s| s.parse::<i64>()) {
            Some(Ok(v)) => v,
            _ => {
                eprintln!("argument {i} is not a number");
                std::process::exit(2)
            }
        }
    };
    let dict_size = num(0) as u32;
    let (lc, lp, pb) = (num(1) as u32, num(2) as u32, num(3) as u32);
    let chunk = match a.len() > 8 {
        true => num(8).max(0) as usize,
        false => 0,
    };
    let cap: u64 = match std::env::var("LZMA_DEC_OUT_CAP").ok().map(|v| v.parse::<u64>()) {
        Some(Ok(v)) => v,
        _ => u64::MAX,
    };

    let mut data = Vec::new();
    match std::io::stdin().read_to_end(&mut data) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("reading stdin: {e}");
            std::process::exit(2);
        }
    }

    // `encode_props` (C_LZMA.cpp:135-143), truncation included. That truncation is
    // not incidental: pb=9/lc=3/lp=0 gives 408, which becomes 152 and is a VALID
    // props byte, while pb=5 gives 228 and is the value that actually trips
    // LzmaDec.c:1273. Computing this in wider arithmetic would test a different
    // program.
    let byte0 = ((pb.wrapping_mul(5).wrapping_add(lp)).wrapping_mul(9).wrapping_add(lc)) as u8;
    let props = [
        byte0,
        dict_size as u8,
        (dict_size >> 8) as u8,
        (dict_size >> 16) as u8,
        (dict_size >> 24) as u8,
    ];

    let mut source = ChunkedIn {
        data,
        pos: 0,
        chunk,
    };
    let mut sink = CountingOut {
        sink: std::io::stdout(),
        produced: 0,
        cap,
        capped: false,
    };

    let (rc, consumed) = match darc_lzma::decode_stream::decode_stream(&mut source, &mut sink, &props)
    {
        Ok(summary) => (FREEARC_OK, summary.input_consumed),
        Err(e) => (errcode(e), 0),
    };

    let _ = sink.sink.flush();
    // maxrss is reported by the C driver and gated C-side only; -1 says "not
    // measured here" rather than inventing a number.
    eprintln!(
        "DARC_DEC rc={} consumed={} produced={} capped={} maxrss=-1",
        rc,
        consumed,
        sink.produced,
        u8::from(sink.capped)
    );
    std::process::exit(match rc {
        FREEARC_OK => 0,
        _ => 1,
    });
}
