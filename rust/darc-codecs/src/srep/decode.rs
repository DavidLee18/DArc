//! The file-level decompression driver, ported from
//! `Compression/SREP/srep.cpp` (the block loop at :1020-1100).
//!
//! A compressed file is:
//!
//! ```text
//!   [archive header: 4 x u32]  [hash seed: hash_seed_size bytes]
//!   [block][block]...          [EOF marker or end of file]
//! ```
//!
//! and each block is:
//!
//! ```text
//!   [header: 3 x u32 + hash_size bytes]
//!   [stat array: statsize bytes]      -- the match list
//!   [literals:   datasize bytes]
//! ```
//!
//! The three header words are the literal byte count, the block's decompressed
//! size, and the stat byte count, in that order. The end of the stream is
//! either running out of file or a header whose first two words are zero.
//!
//! Only the I/O-LZ strategies (v1, v2) are implemented here. v3 (Future-LZ) and
//! v4 (Index-LZ) reorganise where matches live and need machinery this does not
//! have yet -- a pending-match heap for v3, a footer index for v4 -- so they
//! are refused explicitly rather than silently mis-decoded.

use super::io_lz::{self, Dictionary};
use super::{parse_header, Strategy, ARCHIVE_HEADER_WORDS, BLOCK_HEADER_WORDS};
use std::io::{Read, Seek, SeekFrom, Write};

#[derive(Debug)]
pub enum Error {
    /// Not an SREP file, or a version outside 1..=4.
    NotSrep,
    /// A version this port does not implement yet.
    Unsupported(u32),
    BadData,
    Io(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<io_lz::Error> for Error {
    fn from(e: io_lz::Error) -> Self {
        match e {
            io_lz::Error::BadData => Error::BadData,
            io_lz::Error::Io => Error::BadData,
        }
    }
}

/// A hard ceiling on any size read from a block header. The C compares against
/// its own allocated buffers; this port has no preallocated buffers, so it
/// needs an explicit bound or a corrupt header drives the allocation.
const MAX_BLOCK: u64 = 1 << 31;

fn read_words<R: Read>(r: &mut R, n: usize) -> Result<Option<Vec<u32>>, Error> {
    let mut bytes = vec![0u8; n * 4];
    let mut got = 0;
    while got < bytes.len() {
        match r.read(&mut bytes[got..])? {
            0 => break,
            k => got += k,
        }
    }
    if got == 0 {
        return Ok(None); // clean end of stream
    }
    if got != bytes.len() {
        return Err(Error::BadData);
    }
    Ok(Some(
        bytes.chunks_exact(4).map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]])).collect(),
    ))
}

/// The output file, doubling as the LZ dictionary. This is the whole point of
/// I/O-LZ: matches reach back into output already written, so the decoder seeks
/// into what it has produced rather than holding a window in memory.
struct FileDict<'a, W: Read + Write + Seek> {
    out: &'a mut W,
    /// Where the writer is, so the seek can be undone.
    write_pos: u64,
}

impl<W: Read + Write + Seek> Dictionary for FileDict<'_, W> {
    fn read_at(&mut self, offset: u64, buf: &mut [u8]) -> Result<(), io_lz::Error> {
        self.out.seek(SeekFrom::Start(offset)).map_err(|_| io_lz::Error::Io)?;
        self.out.read_exact(buf).map_err(|_| io_lz::Error::Io)?;
        self.out.seek(SeekFrom::Start(self.write_pos)).map_err(|_| io_lz::Error::Io)?;
        Ok(())
    }
}

/// Decompress a whole SREP file. Returns the number of bytes written.
pub fn decompress<R: Read + Seek, W: Read + Write + Seek>(
    fin: &mut R,
    fout: &mut W,
) -> Result<u64, Error> {
    let head = read_words(fin, ARCHIVE_HEADER_WORDS)?.ok_or(Error::NotSrep)?;
    let ah = parse_header(&head).ok_or(Error::NotSrep)?;

    // Keyed hashes (VMAC) store their seed right after the header.
    if ah.hash_seed_size > 0 {
        let mut seed = vec![0u8; ah.hash_seed_size as usize];
        fin.read_exact(&mut seed)?;
    }

    match ah.strategy {
        Strategy::IoLz | Strategy::IoLzRounded => {}
        Strategy::FutureLz => return Err(Error::Unsupported(3)),
        Strategy::IndexLz => return Err(Error::Unsupported(4)),
    }
    let round = ah.strategy.round_matches();
    let l = ah.base_len;

    let mut origsize: u64 = 0;
    loop {
        let hdr = match read_words(fin, BLOCK_HEADER_WORDS)? {
            None => break, // end of file
            Some(h) => h,
        };
        // An all-zero leading pair is the explicit end marker.
        if hdr[0] == 0 && hdr[1] == 0 {
            break;
        }
        // The block header carries the hash after its three words.
        if ah.hash_size > 0 {
            let mut h = vec![0u8; ah.hash_size as usize];
            fin.read_exact(&mut h)?;
        }

        let datasize = hdr[0] as u64; // literal bytes
        let origsize1 = hdr[1] as u64; // decompressed size
        let statsize = hdr[2] as u64; // stat bytes
        if datasize > MAX_BLOCK || origsize1 > MAX_BLOCK || statsize > MAX_BLOCK {
            return Err(Error::BadData);
        }
        // The stat array is whole 32-bit words by construction.
        if statsize % 4 != 0 {
            return Err(Error::BadData);
        }

        let block_start = origsize;

        let mut statbuf = vec![0u8; statsize as usize];
        fin.read_exact(&mut statbuf)?;
        let stat: Vec<u32> = statbuf
            .chunks_exact(4)
            .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect();

        let mut lits = vec![0u8; datasize as usize];
        fin.read_exact(&mut lits)?;

        let mut out = vec![0u8; origsize1 as usize];
        {
            let write_pos = fout.stream_position()?;
            let mut dict = FileDict { out: fout, write_pos };
            io_lz::decompress_block(&mut dict, round, l, block_start, &stat, &lits, &mut out)?;
        }
        fout.write_all(&out)?;
        origsize += origsize1;
    }

    Ok(origsize)
}
