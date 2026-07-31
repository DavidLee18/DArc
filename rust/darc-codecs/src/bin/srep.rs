//! `srep` — a decompress-only drop-in for SREP 3.93a beta.
//!
//! The archiver reaches SREP as an external compressor: `arc.ini`'s
//! `[External compressor:srep]` runs `srep -d <in> <out>` to decompress and
//! `srep -<option> <in> <out>` to compress. This binary implements the
//! decompress command against the Rust port, byte-for-byte compatible with the
//! reference `Tests/srep` on every file the port has been verified against.
//!
//! It is deliberately decode-only. SREP is being ported decode-first, so
//! compression still belongs to the C binary; a compress invocation here fails
//! loudly rather than silently producing nothing, so a misconfiguration is
//! visible instead of corrupting an archive.
//!
//! The C accepts many options; the archiver only ever passes `-d`, so that is
//! what is handled. Any unrecognised leading option is reported rather than
//! ignored.

use std::fs::OpenOptions;
use std::process::exit;

const NO_ERRORS: i32 = 0;
const ERROR_CMDLINE: i32 = 2;
const ERROR_IO: i32 = 3;
const ERROR_COMPRESSION: i32 = 4;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    exit(run(&args));
}

fn run(args: &[String]) -> i32 {
    // Collect option flags and the two filenames, matching the C's positional
    // parse: options begin with '-', everything else is a filename in order.
    let mut decompress = false;
    let mut files: Vec<&str> = Vec::new();
    let mut copt = CompressOpts::default();
    for a in &args[1..] {
        if a == "-d" {
            decompress = true;
        } else if a.starts_with('-') && a.len() > 1 {
            match decompress {
                // A stray option after -d: harmless to ignore, since the
                // compressed format is self-describing.
                true => eprintln!(
                    "srep (Rust port): ignoring option {a} (decompression is self-describing)"
                ),
                false => match parse_compress_option(a, &mut copt) {
                    Ok(()) => {}
                    Err(msg) => {
                        eprintln!("srep (Rust port): {msg}");
                        return ERROR_CMDLINE;
                    }
                },
            }
        } else {
            files.push(a);
        }
    }

    if !decompress {
        if files.len() != 2 {
            eprintln!("srep needs exactly two filenames, got {}", files.len());
            return ERROR_CMDLINE;
        }
        return compress_files(&copt, files[0], files[1]);
    }
    if files.len() != 2 {
        eprintln!("srep -d needs exactly two filenames, got {}", files.len());
        return ERROR_CMDLINE;
    }

    let mut fin = match std::fs::File::open(files[0]) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("srep: can't open {} for read: {e}", files[0]);
            return ERROR_IO;
        }
    };
    // The output file doubles as the LZ dictionary, so it must be readable as
    // well as writable -- I/O-LZ seeks back into what it has already written.
    let mut fout = match OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(files[1])
    {
        Ok(f) => f,
        Err(e) => {
            eprintln!("srep: can't open {} for write: {e}", files[1]);
            return ERROR_IO;
        }
    };

    match darc_codecs::srep::decode::decompress(&mut fin, &mut fout) {
        Ok(_) => NO_ERRORS,
        Err(darc_codecs::srep::decode::Error::NotSrep) => {
            eprintln!("srep: {} is not an SREP compressed file", files[0]);
            ERROR_COMPRESSION
        }
        Err(e) => {
            eprintln!("srep: decompression failed: {e:?}");
            ERROR_COMPRESSION
        }
    }
}

// ---------------------------------------------------------------------------
// Compression
// ---------------------------------------------------------------------------

use darc_codecs::srep::encode_file::{compress_file, EncodeError, HashChoice};
use darc_codecs::srep::params::{Layout, Method, Options};

/// The compress-side options this port understands.
struct CompressOpts {
    method: Method,
    layout: Layout,
    opt: Options,
    hash: HashChoice,
    bufsize: usize,
}

impl Default for CompressOpts {
    fn default() -> Self {
        // srep.cpp:224 -- SREP_METHOD3, Index-LZ, no explicit sizes.
        CompressOpts {
            method: Method::Digests,
            layout: Layout::IndexLz,
            opt: Options::default(),
            hash: HashChoice::MD5,
            bufsize: 0,
        }
    }
}

/// Parse `parseMem`-style sizes: a number with an optional k/m/g unit and an
/// optional trailing `b`, so `16k`, `16kb` and `16384` are all the same.
///
/// The trailing `b` must be stripped BEFORE the unit is read. Taking the last
/// character first turns "16kb" into "16k", which then fails to parse -- and the
/// only symptom was `-b16kb` producing no output at all, which the harness
/// reported as eleven differing streams rather than as a refusal.
fn parse_mem(s: &str) -> Option<u64> {
    let s = s.strip_suffix('b').or(s.strip_suffix('B')).unwrap_or(s);
    let (digits, mult) = match s.chars().last()?.to_ascii_lowercase() {
        'k' => (&s[..s.len() - 1], 1u64 << 10),
        'm' => (&s[..s.len() - 1], 1 << 20),
        'g' => (&s[..s.len() - 1], 1 << 30),
        _ => (s, 1),
    };
    digits.parse::<u64>().ok().map(|n| n * mult)
}

fn parse_compress_option(a: &str, o: &mut CompressOpts) -> Result<(), String> {
    let body = &a[1..];
    // `-hash=` is the only multi-letter option, so it is checked before the
    // single-letter dispatch below.
    match body.strip_prefix("hash=") {
        Some("md5") => {
            o.hash = HashChoice::MD5;
            return Ok(());
        }
        Some(other) => return Err(format!("this port only writes -hash=md5, not {other}")),
        None => {}
    }

    let (letter, rest) = match body.split_at_checked(1) {
        Some(pair) => pair,
        None => return Err(format!("empty option {a}")),
    };
    let size = |v: &str| parse_mem(v).ok_or_else(|| format!("bad size in {a}"));

    match letter {
        "m" => {
            // -mN[o|f]
            let (digit, suffix) = match rest.split_at_checked(1) {
                Some(pair) => pair,
                None => return Err(format!("no method digit in {a}")),
            };
            o.method = match digit {
                "0" => Method::InMemory,
                "1" => Method::Cdc,
                "2" => Method::ZpaqCdc,
                "3" => Method::Digests,
                "4" => Method::Reread,
                "5" => Method::Exhaustive,
                _ => return Err(format!("unknown method in {a}")),
            };
            o.layout = match suffix {
                "" => Layout::IndexLz,
                "f" => Layout::FutureLz,
                "o" => Layout::IoLz,
                _ => return Err(format!("unknown layout suffix in {a}")),
            };
            Ok(())
        }
        "b" => {
            o.bufsize = size(rest)? as usize;
            Ok(())
        }
        "a" => {
            // -a{accel}[/{ACCELERATOR}]
            let (accel, acc2) = match rest.split_once('/') {
                Some((x, y)) => (x, Some(y)),
                None => (rest, None),
            };
            o.opt.accel = accel.parse::<u32>().ok();
            o.opt.accelerator = acc2.and_then(|v| v.parse::<u32>().ok());
            Ok(())
        }
        "l" => {
            o.opt.min_match = size(rest)? as u32;
            Ok(())
        }
        "c" => {
            o.opt.l = size(rest)? as u32;
            Ok(())
        }
        _ => Err(format!("unsupported option {a}")),
    }
}

fn compress_files(o: &CompressOpts, inname: &str, outname: &str) -> i32 {
    let data = match std::fs::read(inname) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("srep: can't read {inname}: {e}");
            return ERROR_IO;
        }
    };
    let packed = match compress_file(&data, o.method, o.layout, o.opt, o.hash, o.bufsize) {
        Ok(p) => p,
        Err(EncodeError::Unsupported) => {
            eprintln!(
                "srep (Rust port): this method/layout is not ported yet -- \
                 only -m3f/-m4f are implemented"
            );
            return ERROR_CMDLINE;
        }
        Err(e) => {
            eprintln!("srep: compression failed: {e:?}");
            return ERROR_COMPRESSION;
        }
    };
    match std::fs::write(outname, &packed) {
        Ok(()) => NO_ERRORS,
        Err(e) => {
            eprintln!("srep: can't write {outname}: {e}");
            ERROR_IO
        }
    }
}
