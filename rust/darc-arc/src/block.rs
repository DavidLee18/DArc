//! Archive blocks and their descriptors — the port of `ArhiveStructure.hs`.
//!
//! An archive is a sequence of blocks. Every block except a data block is
//! followed by a **descriptor**: a fixed preamble that says what the block was,
//! how big it is packed and unpacked, and what its CRC is. Descriptors are what
//! make an archive readable from the end, and recoverable when the middle is
//! damaged.
//!
//! ```text
//!   [SFX stub?] [header] [data]... [dir][descr] ... [footer][descr] EOF
//! ```
//!
//! Reading starts at EOF: scan the last 4 KB backwards for the signature, decode
//! the footer's descriptor, read the footer, and take the block list from it.

use crate::bytestream::{self, InStream};
use crate::crc;

/// `aSIGNATURE = make4byte 65 114 67 1` — "ArC\x01" in memory on a
/// little-endian machine, which is the only order `Utils.hs:46` supports for
/// this build.
#[allow(clippy::identity_op)] // kept as a transcription of make4byte 65 114 67 1
pub const SIGNATURE: u32 = 65 + 256 * (114 + 256 * (67 + 256 * 1));

/// `aSCAN_MAX` — how far back from EOF the footer descriptor is looked for.
pub const SCAN_MAX: u64 = 4096;

/// `ArhiveStructure.hs:313`. The values are written into the archive, so the
/// discriminants are format, not an implementation detail: new ones may only be
/// added at the end.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BlockType {
    Descr = 0,
    Header = 1,
    Data = 2,
    Dir = 3,
    Footer = 4,
    Recovery = 5,
    /// The three "unknown" tags exist so that a block written by a newer build
    /// decodes as a block rather than as corruption.
    Unknown = 6,
    Unknown2 = 7,
    Unknown3 = 8,
}

impl BlockType {
    /// `toEnum`. Out-of-range is `None` rather than a panic — this is a value
    /// read from an untrusted file.
    pub fn from_tag(tag: u64) -> Option<Self> {
        match tag {
            0 => Some(BlockType::Descr),
            1 => Some(BlockType::Header),
            2 => Some(BlockType::Data),
            3 => Some(BlockType::Dir),
            4 => Some(BlockType::Footer),
            5 => Some(BlockType::Recovery),
            6 => Some(BlockType::Unknown),
            7 => Some(BlockType::Unknown2),
            8 => Some(BlockType::Unknown3),
            _ => None,
        }
    }

    /// `block_name` (`ArhiveStructure.hs:302`), used verbatim in diagnostics —
    /// the wording is part of what `arc t` prints.
    pub fn name(self) -> &'static str {
        match self {
            BlockType::Descr => "block descriptor",
            BlockType::Header => "header block",
            BlockType::Data => "data block",
            BlockType::Dir => "directory block",
            BlockType::Footer => "footer block",
            BlockType::Recovery => "recovery block",
            BlockType::Unknown | BlockType::Unknown2 | BlockType::Unknown3 => {
                "block of unknown type"
            }
        }
    }
}

/// `ArchiveBlock` minus its back-reference to the open archive, which in Rust
/// is the caller's business rather than a field.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArchiveBlock {
    pub block_type: BlockType,
    /// `blCompressor` — the chain of methods, stored in the archive as one
    /// `'+'`-joined string. `aNO_COMPRESSION` is `["storing"]`, never empty.
    pub compressor: Vec<String>,
    /// Absolute position of the block's data in the archive file.
    pub pos: u64,
    pub orig_size: u64,
    pub comp_size: u64,
    /// CRC of the *unpacked* data. Meaningful for service blocks only.
    pub crc: u32,
    /// `blFiles`, data blocks only — reconstructed by the directory reader.
    pub files: Option<usize>,
}

impl ArchiveBlock {
    /// `enc = any isEncryption`. A method is an encryption step when its name
    /// starts with one of the cipher names, which is how `Compression.hs`
    /// decides it too.
    pub fn is_encrypted(&self) -> bool {
        self.compressor.iter().any(|m| is_encryption(m))
    }

    /// `blCompressor == aNO_COMPRESSION` — the block's bytes are its data.
    ///
    /// `aSTORING` is the string "storing" (`Compression.hs:39`), not an empty
    /// chain: a check for an empty compressor is a check for something the
    /// format never writes.
    pub fn is_stored(&self) -> bool {
        self.compressor.len() == 1 && self.compressor[0] == "storing"
    }

    /// `block_name block` — "<kind> at pos <n>".
    pub fn name(&self) -> String {
        format!("{} at pos {}", self.block_type.name(), self.pos)
    }
}

/// `isEncryption` (`Compression.hs:66`) — in the C, whether `parse_ENCRYPTION`
/// claims the method string.
///
/// The name is peeled the way `C_Encryption.cpp:161` peels it: parameters are
/// separated by `':'`, the encryption mode by `'/'` (`aes/cfb`), and the key
/// size by `'-'` (`aes-128`). So `aes-256/cfb:k…:i…` is the cipher `aes`.
/// Matching the whole string against the cipher table instead — which is the
/// obvious-looking version — recognises no real archive's method, because every
/// encrypted block carries at least the key and IV parameters.
pub fn is_encryption(method: &str) -> bool {
    let cipher = method
        .split(':')
        .next()
        .unwrap_or("")
        .split('/')
        .next()
        .unwrap_or("")
        .split('-')
        .next()
        .unwrap_or("");
    // cipher_descriptor[] in C_Encryption.cpp:22, in LibTomCrypt registration
    // order -- the order is format, since parse_ENCRYPTION stores the index.
    matches!(cipher, "aes" | "blowfish" | "serpent" | "twofish")
}

/// Why a candidate descriptor was rejected.
///
/// These are not all errors: the scan tries every position where the signature
/// appears, and most rejections are simply "that four bytes was file data".
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DescriptorError {
    /// Not enough bytes for a descriptor plus its trailing CRC.
    TooShort,
    /// The descriptor's own CRC does not match — `ArhiveStructure.hs:72`.
    BadCrc { at: u64 },
    /// The leading word was not `aSIGNATURE`, or the block position came out
    /// negative (`ArhiveStructure.hs:80`).
    NotADescriptor,
    /// A block type tag this build does not know.
    UnknownBlockType { tag: u64 },
    /// The body did not decode.
    Malformed(bytestream::Error),
}

/// `archiveReadBlockDescriptor` — decode the descriptor occupying `window`,
/// which sits at `arcpos` in the file.
///
/// `window` must end exactly where the descriptor ends: the descriptor's own
/// CRC is read from its **last four bytes**, and the CRC is computed over
/// everything before them. That is the Haskell's shape (`bufsize - sizeOf CRC`)
/// and it is why the scan hands in a window that runs to the end of the region
/// being searched rather than a guessed length.
pub fn read_descriptor(arcpos: u64, window: &[u8]) -> Result<ArchiveBlock, DescriptorError> {
    let n = window.len();
    if n < 4 + 4 {
        return Err(DescriptorError::TooShort);
    }
    let body = &window[..n - 4];
    let stored = u32::from_le_bytes([window[n - 4], window[n - 3], window[n - 2], window[n - 1]]);
    if crc::calc(body) != stored {
        return Err(DescriptorError::BadCrc { at: arcpos });
    }

    let mut s = InStream::new(body);
    let sign = s.u32().map_err(DescriptorError::Malformed)?;
    let tag = s.varint().map_err(DescriptorError::Malformed)?;
    let compressor = s.compressor().map_err(DescriptorError::Malformed)?;
    let orig_size = s.varint().map_err(DescriptorError::Malformed)?;
    let comp_size = s.varint().map_err(DescriptorError::Malformed)?;
    let crc_value = s.crc().map_err(DescriptorError::Malformed)?;

    if sign != SIGNATURE {
        return Err(DescriptorError::NotADescriptor);
    }
    let block_type =
        BlockType::from_tag(tag).ok_or(DescriptorError::UnknownBlockType { tag })?;
    // blDecodePosRelativeTo arcpos compsize = arcpos - compsize: the descriptor
    // sits immediately after the data it describes. A descriptor claiming more
    // data than precedes it in the file is not one.
    let pos = arcpos.checked_sub(comp_size).ok_or(DescriptorError::NotADescriptor)?;

    Ok(ArchiveBlock {
        block_type,
        compressor,
        pos,
        orig_size,
        comp_size,
        crc: crc_value,
        files: None,
    })
}

/// `archiveFindBlockDescriptor` — the last descriptor starting within the first
/// `len` bytes of `buf`, where `buf` begins at `base_pos` in the file.
///
/// Searches **backwards**, and returns the first candidate that decodes: the
/// last descriptor in the archive is the footer's, and scanning forwards would
/// find an earlier one first. Every rejected position costs one CRC over the
/// remaining window, which is why `len` is bounded to `SCAN_MAX` by callers.
pub fn find_descriptor(
    base_pos: u64,
    buf: &[u8],
    len: usize,
) -> Result<(u64, ArchiveBlock), DescriptorError> {
    let size = buf.len();
    let mut last_err = DescriptorError::NotADescriptor;
    // go ((size - sizeOf aSIGNATURE) `max` (len-1)) -- start at whichever is
    // further out, so a descriptor whose signature sits at the very last legal
    // offset is still tried.
    let start = size.saturating_sub(4).max(len.saturating_sub(1));
    let mut pos = start;
    loop {
        let word = match buf.get(pos..pos + 4) {
            Some(b) => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
            None => {
                if pos == 0 {
                    return Err(last_err);
                }
                pos -= 1;
                continue;
            }
        };
        if word == SIGNATURE {
            match read_descriptor(base_pos + pos as u64, &buf[pos..]) {
                Ok(block) => return Ok((base_pos + pos as u64, block)),
                Err(e) => last_err = e,
            }
        }
        if pos == 0 {
            return Err(last_err);
        }
        pos -= 1;
    }
}

/// `FooterBlock` (`ArhiveStructure.hs:214`).
#[derive(Clone, Debug)]
pub struct FooterBlock {
    /// Every service block in the archive, in file order, with the footer's own
    /// descriptor appended — matching `ftBlocks = blocks ++ [footer]`.
    pub blocks: Vec<ArchiveBlock>,
    pub locked: bool,
    pub comment: String,
    pub recovery: String,
    /// Size of any SFX stub before the archive proper, computed as the position
    /// of the earliest block.
    pub sfx_size: u64,
}

/// `archiveReadFooterBlock` — decode the *already decompressed* footer body.
///
/// `arcpos` is **`blPos` of the footer block itself** — where its packed data
/// starts — not the position of its descriptor. `archiveReadFooterBlock`
/// destructures `blPos = pos` from the footer block and passes that to
/// `tupleToBlock`, and `archiveWriteFooterBlock` encodes against the same value.
///
/// Passing the descriptor's position instead shifts every block in the archive
/// by the footer's packed size — here, 59 bytes. Nothing errors: the header
/// block simply appears at 59 instead of 0, and `ftSFXSize`, which is
/// `minimum (map blPos blocks)`, reports a 59-byte SFX stub on an archive that
/// has none. It was caught by noticing that number, not by a check.
///
/// The two `isEOFMemory` probes are format, not defensiveness: builds older than
/// the recovery field wrote neither it nor the UTF-8 comment, and an archive
/// from one of those must still read. Absent fields default rather than fail.
pub fn read_footer(
    arcpos: u64,
    body: &[u8],
    footer_descriptor: ArchiveBlock,
) -> Result<FooterBlock, bytestream::Error> {
    let mut s = InStream::new(body);

    // blockToTuple wrote (type, compressor, arcpos - pos, origsize, compsize, crc).
    let tuples = s.list(|s| {
        let tag = s.varint()?;
        let compressor = s.compressor()?;
        let offset = s.varint()?;
        let orig_size = s.varint()?;
        let comp_size = s.varint()?;
        let crc = s.crc()?;
        Ok((tag, compressor, offset, orig_size, comp_size, crc))
    })?;

    let locked = s.bool()?;
    // The pre-UTF-8 comment: a length, then that many Word32 characters.
    let old_len = s.count()?;
    let old_comment: String = s
        .exactly(old_len, |s| s.u32())?
        .into_iter()
        .filter_map(char::from_u32)
        .collect();
    // Older builds stop here.
    let recovery = if s.is_eof() { String::new() } else { s.string()? };
    let comment = if s.is_eof() {
        String::new()
    } else {
        let n = s.count()?;
        let bytes = s.exactly(n, |s| s.u8())?;
        String::from_utf8_lossy(&bytes).into_owned()
    };

    let mut blocks: Vec<ArchiveBlock> = Vec::with_capacity(tuples.len() + 1);
    for (tag, compressor, offset, orig_size, comp_size, crc) in tuples {
        // tupleToBlock: pos = arcpos - offset. A tuple that points past the
        // start of the file is corruption, not a block.
        let pos = arcpos.checked_sub(offset).ok_or(bytestream::Error::ImplausibleLength {
            at: 0,
            len: offset,
            remaining: arcpos as usize,
        })?;
        blocks.push(ArchiveBlock {
            block_type: BlockType::from_tag(tag).unwrap_or(BlockType::Unknown),
            compressor,
            pos,
            orig_size,
            comp_size,
            crc,
            files: None,
        });
    }
    // ftSFXSize is computed BEFORE the footer's own descriptor is appended --
    // `minimum (map blPos blocks)` runs on the list read from the block, so
    // appending first would be harmless here but wrong in an archive whose only
    // block is the footer.
    let sfx_size = blocks.iter().map(|b| b.pos).min().unwrap_or(0);
    blocks.push(footer_descriptor);

    Ok(FooterBlock {
        blocks,
        locked,
        comment: if comment.is_empty() { old_comment } else { comment },
        recovery,
        sfx_size,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytestream::OutStream;

    /// The signature is four ASCII bytes in the file. Pin the byte order: this
    /// build is little-endian-only (`Utils.hs:45`), and getting it backwards
    /// would make every archive unreadable while every unit test still passed.
    #[test]
    fn the_signature_is_arc1_in_file_order() {
        assert_eq!(SIGNATURE.to_le_bytes(), [65, 114, 67, 1]);
        assert_eq!(&SIGNATURE.to_le_bytes()[..3], b"ArC");
    }

    /// Build a descriptor the way archiveWriteBlockDescriptor does, so the
    /// reader is checked against the writer's layout rather than against itself.
    fn make_descriptor(
        block_type: BlockType,
        compressor: &[&str],
        orig: u64,
        comp: u64,
        data_crc: u32,
    ) -> Vec<u8> {
        let mut o = OutStream::new();
        o.u32(SIGNATURE);
        assert!(o.varint(block_type as u64));
        let methods: Vec<String> = compressor.iter().map(|s| s.to_string()).collect();
        o.compressor(&methods);
        assert!(o.varint(orig));
        assert!(o.varint(comp));
        o.crc(data_crc);
        let body = o.into_bytes();
        let mut out = body.clone();
        out.extend_from_slice(&crc::calc(&body).to_le_bytes());
        out
    }

    #[test]
    fn a_descriptor_round_trips_through_the_reader() {
        let d = make_descriptor(BlockType::Dir, &["lzma:96mb"], 5000, 1234, 0xAABB_CCDD);
        // The descriptor sits at 10_000, so the block it describes starts at
        // 10_000 - 1234.
        let block = read_descriptor(10_000, &d).expect("decodes");
        assert_eq!(block.block_type, BlockType::Dir);
        assert_eq!(block.compressor, vec!["lzma:96mb".to_string()]);
        assert_eq!(block.orig_size, 5000);
        assert_eq!(block.comp_size, 1234);
        assert_eq!(block.crc, 0xAABB_CCDD);
        assert_eq!(block.pos, 10_000 - 1234, "pos is arcpos - compsize");
    }

    /// A flipped bit anywhere in a descriptor must be caught. The descriptor is
    /// the only thing standing between a damaged archive and a reader that
    /// believes a garbage block position.
    #[test]
    fn one_flipped_bit_anywhere_fails_the_crc() {
        let d = make_descriptor(BlockType::Footer, &[], 100, 100, 7);
        for i in 0..d.len() {
            let mut bad = d.clone();
            bad[i] ^= 1;
            match read_descriptor(1000, &bad) {
                Err(DescriptorError::BadCrc { .. }) => {}
                // Flipping a bit inside the trailing CRC itself also fails, by
                // the same check from the other side.
                other => panic!("byte {i} flipped was accepted: {other:?}"),
            }
        }
    }

    #[test]
    fn a_descriptor_claiming_more_data_than_precedes_it_is_rejected() {
        let d = make_descriptor(BlockType::Data, &[], 10, 5000, 0);
        assert_eq!(read_descriptor(100, &d), Err(DescriptorError::NotADescriptor));
    }

    #[test]
    fn an_unknown_block_tag_is_named_not_guessed() {
        let mut o = OutStream::new();
        o.u32(SIGNATURE);
        assert!(o.varint(99));
        o.compressor(&[]);
        assert!(o.varint(1));
        assert!(o.varint(1));
        o.crc(0);
        let body = o.into_bytes();
        let mut d = body.clone();
        d.extend_from_slice(&crc::calc(&body).to_le_bytes());
        assert_eq!(read_descriptor(1000, &d), Err(DescriptorError::UnknownBlockType { tag: 99 }));
    }

    /// The scan must find the LAST descriptor, not the first: the footer's is
    /// the one at the end, and an archive contains many.
    #[test]
    fn the_scan_finds_the_last_descriptor_not_the_first() {
        let first = make_descriptor(BlockType::Dir, &[], 10, 4, 1);
        let second = make_descriptor(BlockType::Footer, &[], 20, 4, 2);
        let mut buf = Vec::new();
        buf.extend_from_slice(&first);
        let second_at = buf.len();
        buf.extend_from_slice(&second);
        let len = buf.len();
        let (at, block) = find_descriptor(0, &buf, len).expect("finds one");
        assert_eq!(at, second_at as u64, "found the earlier descriptor");
        assert_eq!(block.block_type, BlockType::Footer);
    }

    /// Four bytes of file data that happen to equal the signature must not stop
    /// the scan -- the CRC is what distinguishes a descriptor from a coincidence.
    #[test]
    fn a_stray_signature_in_the_data_does_not_derail_the_scan() {
        let real = make_descriptor(BlockType::Footer, &[], 20, 4, 2);
        let mut buf = Vec::new();
        buf.extend_from_slice(&SIGNATURE.to_le_bytes());
        buf.extend_from_slice(b"not a descriptor at all");
        let real_at = buf.len();
        buf.extend_from_slice(&real);
        let len = buf.len();
        let (at, block) = find_descriptor(0, &buf, len).expect("skips the decoy");
        assert_eq!(at, real_at as u64);
        assert_eq!(block.block_type, BlockType::Footer);
    }

    #[test]
    fn a_buffer_with_no_signature_reports_rather_than_looping() {
        let buf = vec![0u8; 512];
        assert!(find_descriptor(0, &buf, buf.len()).is_err());
    }

    #[test]
    fn encryption_is_recognised_by_the_method_name() {
        // The forms that actually appear in an archive: cipher, key size,
        // mode, and the key/IV/salt parameters the writer appends.
        assert!(is_encryption("aes"));
        assert!(is_encryption("aes-256"));
        assert!(is_encryption("aes-256/cfb"));
        assert!(is_encryption("aes-256/cfb:k0123:i4567:s89:c00"));
        assert!(is_encryption("serpent/ctr:kabc"));
        assert!(is_encryption("blowfish-448"));
        assert!(is_encryption("twofish"));
        // And the compression methods, which must not be mistaken for one.
        assert!(!is_encryption("lzma:96mb:normal"));
        assert!(!is_encryption("tor:3"));
        assert!(!is_encryption("delta"));
        assert!(!is_encryption(""));
    }
}
