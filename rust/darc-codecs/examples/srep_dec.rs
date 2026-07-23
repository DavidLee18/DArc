//! Minimal SREP decompressor driver, for the differential harness.
//!
//!     srep_dec <compressed> <output>
//!
//! The real deliverable is a full `srep` binary; this exists so the port can be
//! compared against the C reference before that wrapper is written.

use std::fs::OpenOptions;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        eprintln!("usage: {} <compressed> <output>", args[0]);
        std::process::exit(2);
    }
    let mut fin = match std::fs::File::open(&args[1]) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("cannot open {}: {e}", args[1]);
            std::process::exit(3);
        }
    };
    let mut fout = match OpenOptions::new().read(true).write(true).create(true).truncate(true).open(&args[2]) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("cannot create {}: {e}", args[2]);
            std::process::exit(3);
        }
    };
    match darc_codecs::srep::decode::decompress(&mut fin, &mut fout) {
        Ok(n) => eprintln!("decompressed {n} bytes"),
        Err(e) => {
            eprintln!("srep: {e:?}");
            std::process::exit(4);
        }
    }
}
