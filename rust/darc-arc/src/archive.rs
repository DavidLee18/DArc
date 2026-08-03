//! Opening an archive and reading its directory — `archiveReadInfo`
//! (`ArhiveDirectory.hs:72`).
//!
//! # Where the concurrency is
//!
//! An archive's directory is spread over one or more `DIR_BLOCK`s, each an
//! independently LZMA-compressed unit whose position comes from the footer. They
//! are decoded **in parallel**, and the results concatenated in footer order —
//! so the file list does not depend on which block finished first.
//!
//! That is safe for a reason, not by luck. The Haskell decides solid-block
//! membership in `splitToSolidBlocks`, a pure function
//! (`ArhiveFileList.hs:291`) called before any concurrency exists
//! (`ArcvProcessRead.hs:104`), and measurement backs it: the threaded GHC build
//! is byte-identical to itself across `-N1/-N2/-N8` × `-mt1/-mt8`. Parallelism
//! belongs strictly *below* the partition, never through it.

use crate::block::{self, ArchiveBlock, BlockType, FooterBlock};
use crate::decompress;
use crate::directory::{self, Entry};
use crate::passwords::Passwords;
use rayon::prelude::*;
use std::io::Read;

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    /// `"0357 archive signature not found at the end of archive"`.
    NoSignature,
    /// `"0358 last block of archive is not footer block"`.
    NotAFooter,
    /// `"0359 %1 failed decompression"`, with the block named as
    /// `block_name` names it.
    Block { name: String, cause: decompress::Error },
    /// A service block decoded, but its contents did not parse.
    Malformed { name: String, cause: crate::bytestream::Error },
    /// The block is encrypted and no supplied password opened it. Kept apart
    /// from `Block` because it is not a corruption report: the archive is
    /// intact and the credential is not. `BAD_PASSWORD` in `Errors.hs`.
    BadPassword { name: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{e}"),
            Error::NoSignature => write!(f, "archive signature not found at the end of archive"),
            Error::NotAFooter => write!(f, "last block of archive is not footer block"),
            Error::Block { name, cause } => write!(f, "{name} failed decompression: {cause}"),
            Error::Malformed { name, cause } => write!(f, "{name} is corrupted: {cause}"),
            Error::BadPassword { name } => write!(f, "{name}: wrong password"),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

/// `ArchiveInfo`.
#[derive(Debug)]
pub struct ArchiveInfo {
    pub footer: FooterBlock,
    /// Every file, in the order the directory blocks list them — which is the
    /// order they occupy the solid blocks.
    pub entries: Vec<Entry>,
    /// The solid blocks, concatenated across directory blocks.
    pub data_blocks: Vec<ArchiveBlock>,
    pub dir_bytes: u64,
    pub dir_cbytes: u64,
    pub data_bytes: u64,
    pub data_cbytes: u64,
}

/// Read the whole file. Archives here are read wholly into memory on the way to
/// listing anyway (the Haskell reads each block into a pool buffer), and one
/// owned buffer is what lets the block decoders run in parallel over borrowed
/// slices without a lock on the file handle.
fn read_all(path: &std::path::Path) -> Result<Vec<u8>, Error> {
    let mut f = std::fs::File::open(path)?;
    let size = f.metadata()?.len();
    let mut buf = Vec::with_capacity(size as usize);
    f.read_to_end(&mut buf)?;
    Ok(buf)
}

/// `archiveReadFooter` — find the last descriptor and decode the footer.
pub fn read_footer(data: &[u8], pw: &Passwords) -> Result<(u64, FooterBlock), Error> {
    let size = data.len() as u64;
    let scan = block::SCAN_MAX.min(size);
    let from = (size - scan) as usize;
    let window = data.get(from..).ok_or(Error::NoSignature)?;

    let (_at, descriptor) = block::find_descriptor(from as u64, window, window.len())
        .map_err(|_| Error::NoSignature)?;
    if descriptor.block_type != BlockType::Footer {
        return Err(Error::NotAFooter);
    }
    let body = read_service_block(data, &descriptor, pw)?;
    // Relative to blPos of the footer BLOCK, not of its descriptor.
    let base = descriptor.pos;
    let name = descriptor.name();
    let footer = block::read_footer(base, &body, descriptor)
        .map_err(|cause| Error::Malformed { name, cause })?;
    Ok((base, footer))
}

/// `generateDecryption` over a chain read from the archive — give every
/// encryption link the key derived from a password that verifies against its
/// stored check code.
///
/// A chain with no encryption link is returned as-is without touching the KDF,
/// which is what keeps an ordinary archive from paying for PBKDF2 iterations it
/// does not need.
pub fn keyed(compressor: &[String], b: &ArchiveBlock, pw: &Passwords) -> Result<Vec<String>, Error> {
    if !compressor.iter().any(|m| crate::block::is_encryption(m)) {
        return Ok(compressor.to_vec());
    }
    crate::encryption::generate_decryption(compressor, &pw.unpack, &pw.keyfiles).map_err(|e| {
        match e {
            crate::encryption::Error::BadPassword => Error::BadPassword { name: b.name() },
            // Not a password problem: the stored method string is unusable, or
            // the cipher refused what it named. Both are properties of the
            // archive, so they report as a bad block rather than sending the
            // user off to re-type a password that was never wrong.
            other @ (crate::encryption::Error::BadMethod(_)
            | crate::encryption::Error::Cipher(_)
            | crate::encryption::Error::NoEntropy) => Error::Block {
                name: b.name(),
                cause: decompress::Error::BadMethod(other.to_string()),
            },
        }
    })
}

/// `archiveBlockReadAll` — the packed bytes of one service block, decompressed
/// and checked.
fn read_service_block(data: &[u8], b: &ArchiveBlock, pw: &Passwords) -> Result<Vec<u8>, Error> {
    let start = b.pos as usize;
    let end = start.saturating_add(b.comp_size as usize);
    let packed = data.get(start..end).ok_or_else(|| Error::Block {
        name: b.name(),
        cause: decompress::Error::WrongSize {
            expected: b.comp_size as usize,
            got: data.len().saturating_sub(start),
        },
    })?;
    let compressor = keyed(&b.compressor, b, pw)?;
    decompress::read_block(&compressor, packed, b.orig_size as usize, b.crc)
        .map_err(|cause| Error::Block { name: b.name(), cause })
}

/// `archiveReadInfo` — the footer, then every directory block.
pub fn read_info(path: &std::path::Path, pw: &Passwords) -> Result<ArchiveInfo, Error> {
    let data = read_all(path)?;
    let (_base, footer) = read_footer(&data, pw)?;

    let dir_blocks: Vec<&ArchiveBlock> =
        footer.blocks.iter().filter(|b| b.block_type == BlockType::Dir).collect();

    // The parallel step. Each directory block is decompressed and decoded on its
    // own; `collect` into a Vec keeps footer order regardless of completion
    // order, so the file list is identical whatever the scheduler does.
    let decoded: Vec<Result<directory::Directory, Error>> = dir_blocks
        .par_iter()
        .map(|b| {
            let body = read_service_block(&data, b, pw)?;
            directory::read_directory(b.pos, &body)
                .map_err(|cause| Error::Malformed { name: b.name(), cause })
        })
        .collect();

    let mut entries = Vec::new();
    let mut data_blocks = Vec::new();
    for d in decoded {
        let d = d?;
        // Entries carry a block index that is local to their own directory
        // block; shift it as the block lists are concatenated.
        let base = data_blocks.len();
        for mut e in d.entries {
            e.block += base;
            entries.push(e);
        }
        data_blocks.extend(d.blocks);
    }

    let dir_bytes = dir_blocks.iter().map(|b| b.orig_size).sum();
    let dir_cbytes = dir_blocks.iter().map(|b| b.comp_size).sum();
    let data_bytes = data_blocks.iter().map(|b| b.orig_size).sum();
    let data_cbytes = data_blocks.iter().map(|b| b.comp_size).sum();

    Ok(ArchiveInfo {
        footer,
        entries,
        data_blocks,
        dir_bytes,
        dir_cbytes,
        data_bytes,
        data_cbytes,
    })
}

/// Read one file's bytes, by decompressing the solid block that holds it.
///
/// Deliberately naive for now: a solid block is decompressed in full even to
/// extract one file from it, which is what "solid" means — there is no way to
/// start in the middle. Extracting many files from one block should decompress
/// it once, and that is the caller's job to arrange.
pub fn read_entry(
    data: &[u8],
    info: &ArchiveInfo,
    entry: &Entry,
    pw: &Passwords,
) -> Result<Vec<u8>, Error> {
    let b = info.data_blocks.get(entry.block).ok_or(Error::NoSignature)?;
    let start = b.pos as usize;
    let end = start.saturating_add(b.comp_size as usize);
    let packed = data.get(start..end).ok_or_else(|| Error::Block {
        name: b.name(),
        cause: decompress::Error::WrongSize { expected: b.comp_size as usize, got: 0 },
    })?;
    let compressor = keyed(&b.compressor, b, pw)?;
    let unpacked = decompress::decompress_chain(&compressor, packed, b.orig_size as usize)
        .map_err(|cause| Error::Block { name: b.name(), cause })?;
    let from = entry.pos_in_block as usize;
    let to = from.saturating_add(entry.size as usize);
    let bytes = unpacked.get(from..to).ok_or_else(|| Error::Block {
        name: b.name(),
        cause: decompress::Error::WrongSize { expected: to, got: unpacked.len() },
    })?;
    Ok(bytes.to_vec())
}

/// Open the file and hand back its bytes, so a caller can list and then extract
/// without reading it twice.
pub fn open(path: &std::path::Path) -> Result<Vec<u8>, Error> {
    read_all(path)
}
