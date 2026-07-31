//! Drive darc-lzma's LZMA2 over stdin -> stdout, for comparison against DArc's real
//! `lzma2_compress` / `lzma2_decompress` in `rust/difftest/lzma2_ref.cpp`.
//!
//!     lzma2_rs_ref  dictSize lc lp pb fb mc matchFinder algorithm <enc|dec> [readChunk]
//!
//! Argv is `lzma2_ref.cpp`'s verbatim so one case description feeds both. The first
//! eight fields are the *encoder's*; on the decode path they are ignored, exactly as
//! in the C, where `lzma2_decompress` takes no parameters at all — everything the
//! decoder needs travels in the stream's own leading property byte.
//!
//! `enc` writes the whole of `lzma2_compress` (`C_LZMA2.cpp:47-107`): the single
//! property byte, then the LZMA2 stream, then the `0x00` terminator. That property
//! byte is written by the wrapper rather than by the SDK, so a driver that emitted
//! only the stream would compare the wrong thing.

use std::io::{Read, Write};

use darc_lzma::{InStream, Lzma2EncProps, Lzma2Error, OutStream, StreamError};

/// `Compression/Compression.h:20-27`.
const FREEARC_OK: i32 = 0;
const FREEARC_ERRCODE_GENERAL: i32 = -1;
const FREEARC_ERRCODE_INVALID_COMPRESSOR: i32 = -2;
const FREEARC_ERRCODE_BAD_COMPRESSED_DATA: i32 = -7;

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

/// The mapping `C_LZMA2.cpp` applies: a props/parameter fault is
/// `INVALID_COMPRESSOR` (`:92-93`), a data fault is `BAD_COMPRESSED_DATA`
/// (`:157-158`), and a callback error is returned verbatim.
fn enc_errcode(e: Lzma2Error) -> i32 {
    match e {
        Lzma2Error::LcLpTooLarge
        | Lzma2Error::LzmaParam
        | Lzma2Error::MultiThreadedBlocks
        | Lzma2Error::MultiThreadedMatchFinder
        | Lzma2Error::NonSolidBlock
        | Lzma2Error::Bt5MatchFinder => FREEARC_ERRCODE_INVALID_COMPRESSOR,
        Lzma2Error::OutputEof | Lzma2Error::Fail | Lzma2Error::WindowTooNarrow => {
            FREEARC_ERRCODE_GENERAL
        }
        Lzma2Error::Stream(StreamError(code)) => code,
    }
}

fn main() {
    let a: Vec<String> = std::env::args().skip(1).collect();
    if a.len() < 9 {
        eprintln!(
            "usage: lzma2_rs_ref dictSize lc lp pb fb mc matchFinder algorithm <enc|dec> [readChunk]"
        );
        std::process::exit(2);
    }
    let num = |i: usize| -> i64 {
        match a.get(i).map(|s| s.parse::<i64>()) {
            Some(Ok(v)) => v,
            _ => {
                eprintln!("argument {i} is not a number");
                std::process::exit(2)
            }
        }
    };
    let dir = a[8].clone();
    let chunk = match a.len() > 9 {
        true => num(9).max(0) as usize,
        false => 0,
    };
    let cap: u64 = match std::env::var("LZMA_DEC_OUT_CAP")
        .ok()
        .map(|v| v.parse::<u64>())
    {
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
    let input_len = data.len() as u64;

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

    // Bytes the DECODER consumed, which is not the same as bytes stdin held: an
    // LZMA2 stream self-terminates on its 0x00 control byte and anything after it is
    // untouched. The 4x4 layer that wraps LZMA2 depends on that boundary, so the
    // harness compares this against the C rather than taking it on trust.
    let mut dec_consumed: u64 = 0;
    let rc = match dir.as_str() {
        "enc" => {
            // C_LZMA2.cpp:63-89 -- the wrapper sets these explicitly rather than
            // relying on the level defaults, so build them the same way.
            let mut props = Lzma2EncProps::init();
            props.lzma.dict_size = num(0) as u32;
            props.lzma.lc = num(1) as i32;
            props.lzma.lp = num(2) as i32;
            props.lzma.pb = num(3) as i32;
            props.lzma.fb = num(4) as i32;
            props.lzma.mc = num(5) as u32;
            // The DArc matchFinder id -> (btMode, numHashBytes), C_LZMA2.cpp:75-82.
            let (bt, nhb) = match num(6) {
                0 => (1, 2),
                1 => (1, 3),
                2 => (1, 4),
                3 => (0, 4),
                4 => (0, 5),
                other => {
                    eprintln!("no such matchFinder id: {other}");
                    std::process::exit(2)
                }
            };
            props.lzma.bt_mode = bt;
            props.lzma.num_hash_bytes = nhb;
            props.lzma.algo = num(7) as i32;
            // The harness sets DARC_LZMA2_THREADS for both drivers; the C driver
            // reads the same variable in its GetCompressionThreads() stub. Above 1
            // this selects the multi-block stream, so it must be swept, not fixed.
            let threads: i32 = match std::env::var("DARC_LZMA2_THREADS")
                .ok()
                .map(|v| v.parse::<i32>())
            {
                Some(Ok(v)) if v >= 1 => v,
                _ => 1,
            };
            props.num_total_threads = threads;
            props.num_block_threads_max = threads;
            props.normalize();
            match darc_lzma::lzma2_enc::compress_stream(&mut source, &mut sink, &props) {
                Ok(()) => FREEARC_OK,
                Err(e) => enc_errcode(e),
            }
        }
        "dec" => match decode(&mut source, &mut sink) {
            Ok(n) => {
                dec_consumed = n;
                FREEARC_OK
            }
            Err(code) => code,
        },
        other => {
            eprintln!("direction must be enc or dec; got {other}");
            std::process::exit(2);
        }
    };

    let _ = sink.sink.flush();
    // `consumed` is only meaningful for an accepted stream; on a rejection the C
    // reports whatever its 64 KiB buffer had taken, which is an artefact of the
    // buffer rather than of the stream.
    let consumed = match (dir.as_str(), rc) {
        (_, FREEARC_OK) if dir == "dec" => dec_consumed,
        (_, FREEARC_OK) => input_len,
        _ => 0,
    };
    eprintln!(
        "DARC_LZMA2 dir={} rc={} consumed={} produced={} capped={} maxrss=-1",
        dir,
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

/// `lzma2_decompress` (`C_LZMA2.cpp:112-182`) — the property byte plus the stream.
///
/// The C collapses every data fault to `FREEARC_ERRCODE_BAD_COMPRESSED_DATA`
/// (`C_LZMA2.cpp:157-158`), so the harness records error codes rather than gating on
/// them; the distinctions darc-lzma draws are finer than the wrapper can express.
fn decode(source: &mut ChunkedIn, sink: &mut CountingOut) -> Result<u64, i32> {
    match darc_lzma::lzma2_dec::decode_lzma2_stream(source, sink) {
        Ok(summary) => Ok(summary.input_consumed),
        Err(darc_lzma::LzmaDecodeError::UnsupportedProps) => Err(FREEARC_ERRCODE_INVALID_COMPRESSOR),
        Err(darc_lzma::LzmaDecodeError::Stream(StreamError(code))) => Err(code),
        Err(_) => Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA),
    }
}
