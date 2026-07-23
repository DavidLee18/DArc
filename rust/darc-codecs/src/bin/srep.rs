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
    for a in &args[1..] {
        if a == "-d" {
            decompress = true;
        } else if a.starts_with('-') && a.len() > 1 {
            // A compress method (-m..), or any other option. This port does not
            // compress, and passing an unknown option through silently would be
            // worse than saying so.
            if !decompress {
                eprintln!(
                    "srep (Rust port): only decompression is implemented; \
                     compress with the reference srep binary"
                );
                return ERROR_CMDLINE;
            }
            // A stray option after -d: harmless to ignore for decompression,
            // since the format is self-describing, but note it.
            eprintln!("srep (Rust port): ignoring option {a} (decompression is self-describing)");
        } else {
            files.push(a);
        }
    }

    if !decompress {
        eprintln!("usage: srep -d <compressed> <output>   (this port decompresses only)");
        return ERROR_CMDLINE;
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
