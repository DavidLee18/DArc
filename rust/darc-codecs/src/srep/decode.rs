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
//! All four strategies decode. v1/v2 use the I/O-LZ block decompressor, v3/v4
//! the Future-LZ one; the difference between v3 and v4 is only *where the match
//! list comes from*, not how it is applied:
//!
//! * v3 stores each block's records inline, between the block header and the
//!   literals, exactly as v1/v2 do.
//! * v4 moves every record into a footer at the end of the file, with a second
//!   footer array giving each block's share. Its blocks therefore carry no
//!   inline stats at all, and the file must be read back-to-front before the
//!   first block can be decoded.

use super::future_lz::{self, PendingHeap};
use super::io_lz::{self, Dictionary};
use super::{parse_header, Strategy, ARCHIVE_HEADER_WORDS, BLOCK_HEADER_WORDS,
            BULAT_ZIGANSHIN_SIGNATURE, INDEX_LZ_FOOTER_WORDS, SREP_SIGNATURE, FOOTER_VERSION1};
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

    let round = ah.strategy.round_matches();
    let future = !matches!(ah.strategy, Strategy::IoLz | Strategy::IoLzRounded);
    let l = ah.base_len;
    let mut pending = PendingHeap::new();

    // v4 keeps every record in a footer, so the tail of the file has to be read
    // before the first block can be decoded. Two arrays live there: the records
    // themselves, and one word per block saying how many bytes of records that
    // block owns.
    let mut index_stats: Vec<u32> = Vec::new();
    let mut index_sizes: Vec<u32> = Vec::new();
    let mut index_at = 0usize; // cursor into index_stats
    let mut index_block = 0usize;
    if ah.strategy == Strategy::IndexLz {
        let filesize = fin.seek(SeekFrom::End(0))?;
        let footer_bytes = (INDEX_LZ_FOOTER_WORDS * 4) as u64;
        if filesize < footer_bytes {
            return Err(Error::BadData);
        }
        fin.seek(SeekFrom::Start(filesize - footer_bytes))?;
        let f = read_words(fin, INDEX_LZ_FOOTER_WORDS)?.ok_or(Error::BadData)?;
        // The footer signatures are the archive's, bitwise complemented.
        if f[5] != !BULAT_ZIGANSHIN_SIGNATURE || f[4] != !SREP_SIGNATURE {
            return Err(Error::BadData);
        }
        if (f[3] & 255) != FOOTER_VERSION1 {
            return Err(Error::BadData);
        }
        let footer_size = f[2] as u64;
        let stat_size = f[0] as u64 | ((f[1] as u64) << 32);
        if stat_size % 4 != 0 || footer_size < footer_bytes {
            return Err(Error::BadData);
        }
        if stat_size > MAX_BLOCK || footer_size > MAX_BLOCK {
            return Err(Error::BadData);
        }
        if footer_size + stat_size > filesize {
            return Err(Error::BadData);
        }

        fin.seek(SeekFrom::Start(filesize - footer_size - stat_size))?;
        index_stats = read_words(fin, (stat_size / 4) as usize)?.unwrap_or_default();
        // Immediately after the records: one word per block.
        let total_blocks = ((footer_size - footer_bytes) / 4) as usize;
        index_sizes = read_words(fin, total_blocks)?.unwrap_or_default();
        if index_sizes.len() != total_blocks {
            return Err(Error::BadData);
        }

        // Back to the first block.
        let hdr_bytes = (ARCHIVE_HEADER_WORDS * 4) as u64 + ah.hash_seed_size as u64;
        fin.seek(SeekFrom::Start(hdr_bytes))?;
    }

    let mut origsize: u64 = 0;
    loop {
        // v4 ends by block count, not by reaching the end of the file: its
        // footer sits immediately after the last block, and reading three more
        // words would parse the footer as a block header. That failure is
        // sneaky, because a file with no matches has a footer beginning with
        // two zero words -- which reads exactly like the end marker and
        // "works". Only files that actually contain matches expose it.
        if ah.strategy == Strategy::IndexLz && index_block >= index_sizes.len() {
            break;
        }
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

        // v1-v3 carry their records inline; v4 takes this block's slice out of
        // the footer index instead.
        let stat: Vec<u32> = if ah.strategy == Strategy::IndexLz {
            if index_block >= index_sizes.len() {
                return Err(Error::BadData);
            }
            let words = (index_sizes[index_block] / 4) as usize;
            index_block += 1;
            let end = index_at.checked_add(words).ok_or(Error::BadData)?;
            if end > index_stats.len() {
                return Err(Error::BadData);
            }
            let slice = index_stats[index_at..end].to_vec();
            index_at = end;
            slice
        } else {
            let mut statbuf = vec![0u8; statsize as usize];
            fin.read_exact(&mut statbuf)?;
            statbuf
                .chunks_exact(4)
                .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
                .collect()
        };

        let mut lits = vec![0u8; datasize as usize];
        fin.read_exact(&mut lits)?;

        let mut out = vec![0u8; origsize1 as usize];
        {
            let write_pos = fout.stream_position()?;
            let mut dict = FileDict { out: fout, write_pos };
            if future {
                future_lz::decompress_block(
                    &mut dict, round, l, block_start, &stat, &lits, &mut out, &mut pending,
                )?;
            } else {
                io_lz::decompress_block(&mut dict, round, l, block_start, &stat, &lits, &mut out)?;
            }
        }
        fout.write_all(&out)?;
        origsize += origsize1;
    }

    Ok(origsize)
}
