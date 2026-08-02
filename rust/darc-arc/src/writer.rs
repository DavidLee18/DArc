//! Writing an archive — the inverse of [`crate::block`] and
//! [`crate::directory`].
//!
//! The layout, from `ArcCreate.hs` and confirmed against archives the reference
//! writes:
//!
//! ```text
//!   HEADER_BLOCK  + descriptor      the archive signature and version
//!   DATA_BLOCK...                   no descriptors -- the directory locates them
//!   DIR_BLOCK     + descriptor
//!   FOOTER_BLOCK  + descriptor      the list of every block above
//! ```
//!
//! A **descriptor follows every block except a data block**
//! (`ArhiveStructure.hs:7`), and carries the block's own CRC plus a CRC of
//! itself. That is what makes an archive readable backwards from EOF and
//! recoverable when the middle is damaged.

use crate::block::{ArchiveBlock, BlockType, SIGNATURE};
use crate::bytestream::OutStream;
use crate::crc;
use crate::directory::Entry;

/// `aARCHIVE_VERSION = make4byte 0 0 5 1` (`Options.hs:322`) — wire format 0.51.
pub const ARCHIVE_VERSION: u32 = 0 + 256 * (0 + 256 * (5 + 256));

/// `aNO_COMPRESSION = [aSTORING]`.
pub fn no_compression() -> Vec<String> {
    vec!["storing".to_string()]
}

/// `archiveWriteHeaderBlock` — the signature and version, eight bytes.
pub fn header_block() -> Vec<u8> {
    let mut o = OutStream::new();
    o.u32(SIGNATURE);
    o.u32(ARCHIVE_VERSION);
    o.into_bytes()
}

/// `archiveWriteBlockDescriptor` — the descriptor for one service block, plus
/// the CRC of the descriptor itself.
///
/// `arcpos` is where the descriptor will sit, and the block's position is stored
/// as `arcpos - compsize`, i.e. implicitly: the descriptor immediately follows
/// the data it describes.
pub fn descriptor(block: &ArchiveBlock) -> Vec<u8> {
    let mut o = OutStream::new();
    o.u32(SIGNATURE);
    assert!(o.varint(block.block_type as u64), "block type is unrepresentable");
    o.compressor(&block.compressor);
    assert!(o.varint(block.orig_size), "block size is unrepresentable");
    assert!(o.varint(block.comp_size), "block size is unrepresentable");
    o.crc(block.crc);
    let body = o.into_bytes();
    let mut out = body.clone();
    out.extend_from_slice(&crc::calc(&body).to_le_bytes());
    out
}

/// `archiveWriteDir` — one directory block's uncompressed bytes.
///
/// `arcpos` is where the directory block itself will start; the data blocks'
/// positions are stored relative to it.
///
/// The layout is column-wise and two fields are *not* written: a file's block
/// membership comes from the per-block counts, and its offset within the block
/// from the running sum of the sizes before it.
pub fn directory_block(arcpos: u64, blocks: &[ArchiveBlock], entries: &[Entry]) -> Vec<u8> {
    let mut o = OutStream::new();

    // 1. The block descriptions.
    assert!(o.varint(blocks.len() as u64));
    for b in blocks {
        assert!(o.varint(b.files.unwrap_or(0) as u64));
    }
    for b in blocks {
        o.compressor(&b.compressor);
    }
    for b in blocks {
        // blEncodePosRelativeTo arcpos block = arcpos - blPos block
        assert!(o.varint(arcpos.saturating_sub(b.pos)));
    }
    for b in blocks {
        assert!(o.varint(b.comp_size));
    }

    // 2. The directory names, deduplicated, in first-seen order --
    //    `enumDirectories` assigns numbers as it walks the file list, so the
    //    order is the file order and not a sorted one.
    let mut dir_names: Vec<String> = Vec::new();
    let mut dir_of: Vec<u64> = Vec::with_capacity(entries.len());
    for e in entries {
        let parent = match e.stored_name.rfind('/') {
            Some(i) => e.stored_name[..i].to_string(),
            None => String::new(),
        };
        let idx = match dir_names.iter().position(|d| *d == parent) {
            Some(i) => i,
            None => {
                dir_names.push(parent);
                dir_names.len() - 1
            }
        };
        dir_of.push(idx as u64);
    }
    assert!(o.varint(dir_names.len() as u64));
    for d in &dir_names {
        o.string(d);
    }

    // 3. One column per field.
    for e in entries {
        let base = match e.stored_name.rfind('/') {
            Some(i) => &e.stored_name[i + 1..],
            None => e.stored_name.as_str(),
        };
        o.string(base);
    }
    for d in &dir_of {
        assert!(o.varint(*d));
    }
    for e in entries {
        assert!(o.varint(e.size));
    }
    for e in entries {
        o.i64(e.time);
    }
    for e in entries {
        o.bool(e.is_dir);
    }
    for e in entries {
        o.crc(e.crc);
    }

    // 4. aTAG_END: no optional fields are defined yet.
    assert!(o.varint(0));
    o.into_bytes()
}

/// `archiveWriteFooterBlock` — the list of every service block, and the archive
/// comment.
///
/// `arcpos` is where the footer block starts; every listed block's position is
/// stored relative to it. Reading it back with the DESCRIPTOR's position instead
/// shifts every block in the archive by the footer's packed size, silently.
pub fn footer_block(
    arcpos: u64,
    blocks: &[ArchiveBlock],
    locked: bool,
    comment: &str,
    recovery: &str,
) -> Vec<u8> {
    let mut o = OutStream::new();
    assert!(o.varint(blocks.len() as u64));
    for b in blocks {
        assert!(o.varint(b.block_type as u64));
        o.compressor(&b.compressor);
        assert!(o.varint(arcpos.saturating_sub(b.pos)));
        assert!(o.varint(b.orig_size));
        assert!(o.varint(b.comp_size));
        o.crc(b.crc);
    }
    o.bool(locked);
    // The pre-UTF-8 comment, always written as an empty list.
    assert!(o.varint(0));
    o.string(recovery);
    let utf8 = comment.as_bytes();
    assert!(o.varint(utf8.len() as u64));
    for b in utf8 {
        o.u8(*b);
    }
    o.into_bytes()
}

/// Accumulates an archive, tracking each block's position as it goes.
pub struct Writer {
    out: Vec<u8>,
    /// Service blocks, in the order the footer must list them.
    service: Vec<ArchiveBlock>,
    /// The canonical `-ae` chain, e.g. `["aes-256/ctr:n1000:r0"]`. Only
    /// consulted when a password is set.
    encryption_algorithm: Vec<String>,
    /// `opt_data_password` — empty means data blocks go in unencrypted.
    data_password: Vec<u8>,
    /// `opt_headers_password` — likewise for the directory and footer blocks.
    headers_password: Vec<u8>,
}

impl Default for Writer {
    fn default() -> Self {
        Self::new()
    }
}

impl Writer {
    pub fn new() -> Self {
        Writer {
            out: Vec::new(),
            service: Vec::new(),
            encryption_algorithm: Vec::new(),
            data_password: Vec::new(),
            headers_password: Vec::new(),
        }
    }

    /// A writer that encrypts. `algorithm` is the canonical `-ae` chain, and
    /// either password may be empty to leave that half of the archive in the
    /// clear — `-p` without `-hp` is exactly that case.
    pub fn with_encryption(
        algorithm: Vec<String>,
        data_password: Vec<u8>,
        headers_password: Vec<u8>,
    ) -> Self {
        Writer {
            encryption_algorithm: algorithm,
            data_password,
            headers_password,
            ..Writer::new()
        }
    }

    /// Which password a block type is encrypted with
    /// (`ArcvProcessCompress.hs:83`).
    ///
    /// The header block is NOT encrypted even under `-hp`: it is the eight
    /// bytes a reader identifies the file by, and its arm of the case is `""`.
    /// Measured on a reference archive written with `-hp`, whose header block
    /// still reads `["storing"]`.
    /// The three `Unknown` tags have no arm: the Haskell's case ends in
    /// `error$ "Unexpected block type "++…` rather than a catch-all, and
    /// defaulting them to "no password" would write an unencrypted block into
    /// an archive the user asked to encrypt. This writer never produces one, so
    /// the case is unreachable rather than merely unhandled.
    fn password_for(&self, block_type: BlockType) -> Option<&[u8]> {
        match block_type {
            BlockType::Data => Some(&self.data_password),
            BlockType::Dir | BlockType::Footer => Some(&self.headers_password),
            BlockType::Header | BlockType::Descr | BlockType::Recovery => Some(&[]),
            BlockType::Unknown | BlockType::Unknown2 | BlockType::Unknown3 => None,
        }
    }

    /// `generateEncryption` for one block: the chain that drives the cipher and
    /// the chain the archive stores, both freshly salted.
    ///
    /// Returns two empty suffixes when this block type takes no password, so a
    /// caller can append unconditionally.
    fn encryption_for(
        &self,
        block_type: BlockType,
    ) -> Result<(Vec<String>, Vec<String>), crate::encryption::Error> {
        let password = match self.password_for(block_type) {
            Some(p) => p,
            None => {
                return Err(crate::encryption::Error::BadMethod(format!(
                    "unexpected block type {block_type:?}"
                )))
            }
        };
        if password.is_empty() {
            return Ok((Vec::new(), Vec::new()));
        }
        crate::encryption::generate(&self.encryption_algorithm, password)
    }

    pub fn pos(&self) -> u64 {
        self.out.len() as u64
    }

    /// Append a service block and its descriptor, and record it for the footer.
    ///
    /// The descriptor records the CRC of the block's **unpacked** bytes, and its
    /// packed size separately -- which is how a reader knows how much to read
    /// before it can check anything.
    fn service_block(&mut self, block_type: BlockType, body: &[u8], compressor: Vec<String>) {
        // Two chains: the one that runs, carrying the key, and the one that is
        // written down, carrying the salt instead. Storing the wrong one would
        // put the key in the archive.
        let (real_suffix, stored_suffix) = match self.encryption_for(block_type) {
            Ok(pair) => pair,
            Err(e) => panic!("cannot encrypt a {block_type:?} block: {e}"),
        };
        let mut real = compressor.clone();
        real.extend(real_suffix);
        let mut compressor = compressor;
        compressor.extend(stored_suffix);
        let packed = match crate::decompress::compress_chain(&real, body) {
            Ok(p) => p,
            // A service block that cannot be compressed is a bug in the method
            // string, not a condition to recover from silently: storing it
            // instead would write an archive whose descriptor lies.
            Err(e) => panic!("cannot compress a {block_type:?} block with {compressor:?}: {e}"),
        };
        let pos = self.pos();
        self.out.extend_from_slice(&packed);
        let block = ArchiveBlock {
            block_type,
            compressor,
            pos,
            orig_size: body.len() as u64,
            comp_size: packed.len() as u64,
            crc: crc::calc(body),
            files: None,
        };
        self.out.extend_from_slice(&descriptor(&block));
        self.service.push(block);
    }

    /// `aDEFAULT_DIR_COMPRESSION = "lzma:bt4:1m"` (`Options.hs:376`), as the
    /// canonical string an archive carries.
    ///
    /// Directory and footer blocks are LZMA-compressed even in a `-m0` archive:
    /// `-m` and `-dm` are separate options and `-m0` does not touch the latter.
    pub fn dir_compressor() -> Vec<String> {
        vec!["lzma:1mb:mf=BT4".to_string()]
    }

    /// The archive signature. Stored, not compressed -- it is the eight bytes a
    /// reader identifies the file by.
    pub fn write_header(&mut self) {
        let body = header_block();
        self.service_block(BlockType::Header, &body, no_compression());
    }

    /// A data block: bytes only, no descriptor. Returns the block record the
    /// directory will describe it with.
    ///
    /// Fallible only because of encryption. The directories block is written
    /// through here with `no_compression()`, and under `-p` it still gets an
    /// encryption method appended — measured on a reference archive, whose
    /// empty directories block reads `storing+aes-256/ctr:…` with a salt of its
    /// own. Writing its bytes out raw would leave the block unencrypted while
    /// claiming otherwise.
    pub fn write_data(
        &mut self,
        body: &[u8],
        compressor: Vec<String>,
        files: usize,
    ) -> Result<ArchiveBlock, crate::decompress::Error> {
        self.write_compressed_data(body, compressor, files)
    }

    /// A data block whose bytes are COMPRESSED with `compressor`.
    ///
    /// `orig_size` is the unpacked length and `comp_size` the packed one; the
    /// directory stores both, which is how a reader knows what to allocate.
    pub fn write_compressed_data(
        &mut self,
        body: &[u8],
        compressor: Vec<String>,
        files: usize,
    ) -> Result<ArchiveBlock, crate::decompress::Error> {
        let (real_suffix, stored_suffix) = self
            .encryption_for(BlockType::Data)
            .map_err(|e| crate::decompress::Error::BadMethod(e.to_string()))?;
        let mut real = compressor.clone();
        real.extend(real_suffix);
        let mut compressor = compressor;
        compressor.extend(stored_suffix);
        let packed = crate::decompress::compress_chain(&real, body)?;
        let pos = self.pos();
        self.out.extend_from_slice(&packed);
        Ok(ArchiveBlock {
            block_type: BlockType::Data,
            compressor,
            pos,
            orig_size: body.len() as u64,
            comp_size: packed.len() as u64,
            crc: 0,
            files: Some(files),
        })
    }

    /// The directory block, describing `blocks`.
    pub fn write_directory(&mut self, blocks: &[ArchiveBlock], entries: &[Entry]) {
        let body = directory_block(self.pos(), blocks, entries);
        self.service_block(BlockType::Dir, &body, Self::dir_compressor());
    }

    /// The footer, and the finished archive.
    pub fn finish(mut self, comment: &str, recovery: &str, locked: bool) -> Vec<u8> {
        let blocks = self.service.clone();
        let body = footer_block(self.pos(), &blocks, locked, comment, recovery);
        self.service_block(BlockType::Footer, &body, Self::dir_compressor());
        self.out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passwords::Passwords;
    use crate::{archive, block, directory};

    fn entry(name: &str, size: u64, is_dir: bool, crc: u32) -> Entry {
        Entry {
            stored_name: name.to_string(),
            size,
            time: 0,
            is_dir,
            crc,
            block: 0,
            pos_in_block: 0,
        }
    }

    /// The version constant is format. Pin its bytes: make4byte 0 0 5 1 is
    /// little-endian, so the file holds 00 00 05 01.
    #[test]
    fn the_archive_version_is_wire_format_0_51() {
        assert_eq!(ARCHIVE_VERSION.to_le_bytes(), [0, 0, 5, 1]);
        assert_eq!(header_block().len(), 8);
    }

    /// A descriptor this module writes must decode through the reader that
    /// reads the reference's -- the two were written from the same spec, so
    /// this catches a spec misreading in only one direction, but it catches
    /// the writer drifting from the reader outright.
    #[test]
    fn a_written_descriptor_reads_back() {
        let b = ArchiveBlock {
            block_type: BlockType::Dir,
            compressor: vec!["lzma:1mb".to_string()],
            pos: 5000,
            orig_size: 900,
            comp_size: 400,
            crc: 0x1234_5678,
            files: None,
        };
        let d = descriptor(&b);
        // The descriptor sits at pos + comp_size.
        let got = block::read_descriptor(5400, &d).expect("decodes");
        assert_eq!(got, b);
    }

    /// The whole round trip: build an archive in memory and read it back with
    /// the same code path that reads the reference's archives.
    #[test]
    fn a_written_archive_reads_back_through_the_reader() {
        let files: [(&str, &[u8]); 3] = [
            ("a.txt", b"hello"),
            ("sub/b.bin", b"0123456789"),
            ("sub/c.dat", b"xyz"),
        ];
        let mut data = Vec::new();
        let mut entries = Vec::new();
        let mut pos = 0u64;
        for (name, body) in files {
            entries.push(Entry {
                stored_name: name.to_string(),
                size: body.len() as u64,
                time: 1_700_000_000,
                is_dir: false,
                crc: crc::calc(body),
                block: 0,
                pos_in_block: pos,
            });
            data.extend_from_slice(body);
            pos += body.len() as u64;
        }

        let mut w = Writer::new();
        w.write_header();
        let data_block = w.write_data(&data, no_compression(), entries.len()).expect("stores");
        w.write_directory(&[data_block], &entries);
        let archive_bytes = w.finish("", "", false);

        let (base, footer) = archive::read_footer(&archive_bytes, &Passwords::default()).expect("footer");
        assert_eq!(footer.sfx_size, 0, "no SFX stub");
        assert_eq!(base, footer.blocks.last().map(|b| b.pos).unwrap_or(0), "the footer block's own position is what the block list is relative to");
        // header, dir, and the footer's own descriptor.
        assert_eq!(footer.blocks.len(), 3);
        let dir = footer
            .blocks
            .iter()
            .find(|b| b.block_type == BlockType::Dir)
            .expect("a directory block");
        // The directory block is LZMA-compressed, as it is in every archive the
        // reference writes -- -m0 controls the DATA method, not -dm.
        assert_eq!(dir.compressor, Writer::dir_compressor());
        let packed = &archive_bytes[dir.pos as usize..(dir.pos + dir.comp_size) as usize];
        let body = crate::decompress::read_block(
            &dir.compressor,
            packed,
            dir.orig_size as usize,
            dir.crc,
        )
        .expect("directory block decompresses and checks out");
        let d = directory::read_directory(dir.pos, &body).expect("directory decodes");
        assert_eq!(d.entries.len(), 3);
        assert_eq!(d.entries[0].stored_name, "a.txt");
        assert_eq!(d.entries[1].stored_name, "sub/b.bin");
        assert_eq!(d.entries[2].pos_in_block, 15, "5 + 10");
        assert_eq!(d.blocks.len(), 1);
        assert_eq!(d.blocks[0].orig_size, 18);
    }

    /// Directory numbers are assigned in FIRST-SEEN order, not sorted order --
    /// `enumDirectories` walks the file list and appends. Sorting them would
    /// renumber every file and produce a different archive.
    #[test]
    fn directory_names_are_numbered_in_first_seen_order() {
        let entries = vec![
            entry("zeta/a", 1, false, 0),
            entry("alpha/b", 1, false, 0),
            entry("zeta/c", 1, false, 0),
        ];
        let blocks = vec![ArchiveBlock {
            block_type: BlockType::Data,
            compressor: no_compression(),
            pos: 0,
            orig_size: 3,
            comp_size: 3,
            crc: 0,
            files: Some(3),
        }];
        let body = directory_block(1000, &blocks, &entries);
        let d = directory::read_directory(1000, &body).expect("decodes");
        assert_eq!(d.entries[0].stored_name, "zeta/a");
        assert_eq!(d.entries[1].stored_name, "alpha/b");
        assert_eq!(d.entries[2].stored_name, "zeta/c");
    }

    #[test]
    fn an_empty_archive_still_has_a_readable_footer() {
        let mut w = Writer::new();
        w.write_header();
        w.write_directory(&[], &[]);
        let bytes = w.finish("", "", false);
        let (_, footer) = archive::read_footer(&bytes, &Passwords::default()).expect("footer");
        assert_eq!(footer.blocks.len(), 3);
    }

    #[test]
    fn a_comment_round_trips() {
        let mut w = Writer::new();
        w.write_header();
        w.write_directory(&[], &[]);
        let bytes = w.finish("a comment with Ünïcödé", "", false);
        let (_, footer) = archive::read_footer(&bytes, &Passwords::default()).expect("footer");
        assert_eq!(footer.comment, "a comment with Ünïcödé");
    }
}
