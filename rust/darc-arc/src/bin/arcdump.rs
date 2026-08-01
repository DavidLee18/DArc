//! Dump an archive's block structure, as the Rust reader sees it.
//!
//! This exists to be checked against the archiver: the unit tests in this crate
//! only prove the reader agrees with its own writer, which is exactly the shape
//! of test that passes while the format is wrong. A real archive, written by
//! `Tests/arc-ghc`, is the first evidence that means anything.
//!
//!     cargo run -p darc-arc --bin arcdump -- some.arc
//!
//! A dev helper, not a shipped tool -- it may panic on a bad path, and `src/bin`
//! is deliberately outside the crate's no-unwrap deny.

use darc_arc::block::{self, BlockType};
use std::io::{Read, Seek, SeekFrom};

fn main() {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("usage: arcdump <archive>");
            std::process::exit(2);
        }
    };
    let mut f = std::fs::File::open(&path).expect("open");
    let size = f.metadata().expect("stat").len();

    // archiveReadFooter: the last aSCAN_MAX bytes should hold the footer's
    // descriptor.
    let scan = block::SCAN_MAX.min(size);
    let mut buf = vec![0u8; scan as usize];
    f.seek(SeekFrom::Start(size - scan)).expect("seek");
    f.read_exact(&mut buf).expect("read");

    let (at, footer_descr) = match block::find_descriptor(size - scan, &buf, buf.len()) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("no footer descriptor: {e:?}");
            std::process::exit(1);
        }
    };
    println!("archive:    {path} ({size} bytes)");
    println!("descriptor: at {at}, {}", footer_descr.name());
    println!("  compressor {:?}", footer_descr.compressor);
    println!("  orig {}  comp {}", footer_descr.orig_size, footer_descr.comp_size);

    if footer_descr.block_type != BlockType::Footer {
        eprintln!("last block is not a footer block");
        std::process::exit(1);
    }

    // Only a stored footer can be decoded until the method-string dispatcher is
    // ported; say so plainly rather than decoding garbage.
    if !footer_descr.is_stored() {
        println!("footer is compressed with {:?} -- not yet decodable", footer_descr.compressor);
        return;
    }

    let mut body = vec![0u8; footer_descr.orig_size as usize];
    f.seek(SeekFrom::Start(footer_descr.pos)).expect("seek block");
    f.read_exact(&mut body).expect("read block");
    let got = darc_arc::crc::calc(&body);
    println!("body crc:   {got:08x} (descriptor says {:08x})", footer_descr.crc);
    if got != footer_descr.crc {
        eprintln!("footer block failed its CRC");
        std::process::exit(1);
    }

    match block::read_footer(at, &body, footer_descr) {
        Ok(footer) => {
            println!("locked {}  sfx {}  recovery {:?}", footer.locked, footer.sfx_size, footer.recovery);
            println!("comment {:?}", footer.comment);
            println!("{} blocks:", footer.blocks.len());
            for b in &footer.blocks {
                println!(
                    "  {:16} pos {:>10}  orig {:>10}  comp {:>10}  crc {:08x}  {:?}",
                    b.block_type.name(),
                    b.pos,
                    b.orig_size,
                    b.comp_size,
                    b.crc,
                    b.compressor
                );
            }
        }
        Err(e) => {
            eprintln!("footer did not decode: {e}");
            std::process::exit(1);
        }
    }
}
