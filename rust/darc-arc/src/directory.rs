//! The archive directory — `archiveReadDir` (`ArhiveDirectory.hs:231`).
//!
//! A directory block is stored **column-wise**: every file's name, then every
//! file's directory number, then every file's size, and so on. It is not a
//! sequence of records. That is what makes the fields compress — a run of sizes
//! or of CRCs has far more structure than the same values interleaved — and it
//! is why the reader has to know `total_files` before it can read any field.
//!
//! Two things are stored *implicitly* and have to be rebuilt:
//!
//! * **which block a file is in** — files are laid out in block order, so the
//!   `num_of_files` counts partition the file list.
//! * **where a file starts inside its block** — the running sum of the sizes of
//!   the files before it in that block. `cfArcBlock` and `cfPos` are, in the
//!   Haskell's words, "encoded implicitly, by sorting on these two fields".
//!
//! ```text
//!   blocks:  n, then n x {file count, compressor, offset, packed size}
//!   dirs:    n, then n x directory name
//!   files:   name, dir number, size, mtime, is-dir, CRC   -- each a full column
//!   tags:    aTAG_END
//! ```

use crate::block::{ArchiveBlock, BlockType};
use crate::bytestream::{self, InStream};

/// One entry in the archive.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Entry {
    /// The name as stored, with `'/'` separators — `archiveWriteDir` always
    /// writes `unixifyPath` for cross-OS interop.
    pub stored_name: String,
    /// Size in bytes. Zero for a directory.
    pub size: u64,
    /// mtime as a Unix timestamp. `--nodates` writes
    /// `aMINIMAL_POSSIBLE_DATETIME`, which is 0.
    pub time: i64,
    pub is_dir: bool,
    pub crc: u32,
    /// Index into the directory's own block list.
    pub block: usize,
    /// Offset of this file's bytes inside its unpacked solid block.
    pub pos_in_block: u64,
}

/// Everything one directory block describes.
#[derive(Clone, Debug)]
pub struct Directory {
    /// The data blocks this directory covers, in archive order.
    pub blocks: Vec<ArchiveBlock>,
    pub entries: Vec<Entry>,
}

/// `aTAG_END` — the terminator of the optional-field section.
const TAG_END: u64 = 0;

/// `remove_unsafe_dirs` (`Files.hs:143`) — drop "." components and resolve
/// "..", so a stored name cannot escape the extraction directory.
///
/// The "." filter is not only a safety measure: `arc a -r x.arc .` stores every
/// directory name with a leading ".", and the listing shows `sub`, not `./sub`.
/// Skipping this makes every path in every listing wrong by two characters.
fn sanitise(path: &str) -> String {
    let mut out: Vec<&str> = Vec::new();
    // Both separators: on Windows `\` divides components, so splitting only on
    // `/` left `..\..\x` as a single "component" that no `..` test could see.
    // See `extract::SEPARATORS` for why this is unconditional rather than
    // `cfg!(windows)`.
    for part in path.split(['/', '\\']) {
        match part {
            "." | "" => continue,
            ".." => {
                out.pop();
            }
            other => out.push(other),
        }
    }
    out.join("/")
}

/// Decode a directory block's unpacked bytes.
///
/// `arcpos` is `blPos` of the **directory block**, which the data blocks'
/// positions are stored relative to — the same relative-position convention as
/// the footer, and the same failure mode if the wrong base is passed: every data
/// block lands at a plausible but wrong offset and nothing reports an error.
pub fn read_directory(arcpos: u64, body: &[u8]) -> Result<Directory, bytestream::Error> {
    let mut s = InStream::new(body);

    // 1. The block descriptions.
    let num_blocks = s.count()?;
    let files_per_block = s.exactly(num_blocks, |s| s.varint())?;
    let compressors = s.exactly(num_blocks, |s| s.compressor())?;
    let offsets = s.exactly(num_blocks, |s| s.varint())?;
    let comp_sizes = s.exactly(num_blocks, |s| s.varint())?;

    // 2. The directory names, written with '/' separators.
    let total_dirs = s.count()?;
    let dir_names = s.exactly(total_dirs, |s| s.string())?;

    // 3. One column per field. total_files is the sum of the per-block counts,
    //    never a stored value -- a mismatch here is what desynchronises the
    //    columns, so it is derived rather than trusted twice.
    let total_files: u64 = files_per_block.iter().sum();
    let total_files = usize::try_from(total_files).map_err(|_| {
        bytestream::Error::ImplausibleLength {
            at: s.pos(),
            len: total_files,
            remaining: s.remaining(),
        }
    })?;
    if total_files > s.remaining() {
        // Every file costs at least one byte per column, so this cannot be met.
        return Err(bytestream::Error::ImplausibleLength {
            at: s.pos(),
            len: total_files as u64,
            remaining: s.remaining(),
        });
    }
    let names = s.exactly(total_files, |s| s.string())?;
    let dir_numbers = s.exactly(total_files, |s| s.varint())?;
    let sizes = s.exactly(total_files, |s| s.varint())?;
    // FileTime is CTime, eight bytes signed -- NOT a varint. Reading it as one
    // would consume the wrong width and shift every column after it.
    let times = s.exactly(total_files, |s| s.i64())?;
    // Bool as a list is one byte per element: the bit-packing writeListFast in
    // ByteStream.hs:524 is commented out, so the default element-wise path runs.
    let dir_flags = s.exactly(total_files, |s| s.bool())?;
    let crcs = s.exactly(total_files, |s| s.crc())?;

    // 4. Optional fields.
    //
    // `archiveWriteDir` writes aTAG_END and nothing else, and the READER's
    // optional-field loop is commented out entirely (ArhiveDirectory.hs:275):
    // the Haskell closes the stream without even reading the tag. So a
    // directory carrying a real optional field would be silently ignored by
    // both -- but by the same token, an unexpected tag here means the columns
    // above did not end where they should have, which is worth refusing.
    //
    // Reading the tag and discarding the comparison, which this did first, is
    // the worst of the three options: it pays for the check and then throws the
    // answer away.
    if !s.is_eof() {
        let tag = s.varint()?;
        if tag != TAG_END {
            return Err(bytestream::Error::ImplausibleLength {
                at: s.pos(),
                len: tag,
                remaining: s.remaining(),
            });
        }
    }

    // Rebuild the data blocks. blOrigSize is not stored: it is the sum of the
    // sizes of the files in the block (`map sum filesizes`).
    let mut blocks = Vec::with_capacity(num_blocks);
    let mut file_index = 0usize;
    for b in 0..num_blocks {
        let count = usize::try_from(files_per_block[b]).unwrap_or(0);
        let end = file_index.saturating_add(count).min(total_files);
        let orig_size: u64 = sizes[file_index..end].iter().sum();
        let pos = arcpos.checked_sub(offsets[b]).ok_or(bytestream::Error::ImplausibleLength {
            at: 0,
            len: offsets[b],
            remaining: arcpos as usize,
        })?;
        blocks.push(ArchiveBlock {
            block_type: BlockType::Data,
            compressor: compressors[b].clone(),
            pos,
            orig_size,
            comp_size: comp_sizes[b],
            // tupleToDataBlock passes 0: a data block carries no CRC of its own,
            // only the per-file CRCs read above.
            crc: 0,
            files: Some(count),
        });
        file_index = end;
    }

    // Assemble the entries, restoring the two implicit fields as we go.
    let mut entries = Vec::with_capacity(total_files);
    let mut block_of = 0usize;
    let mut left_in_block = usize::try_from(files_per_block.first().copied().unwrap_or(0)).unwrap_or(0);
    let mut pos_in_block = 0u64;
    for i in 0..total_files {
        // Advance past any blocks that hold no files at all, rather than
        // assuming the next block is nonempty.
        while left_in_block == 0 && block_of + 1 < num_blocks {
            block_of += 1;
            left_in_block = usize::try_from(files_per_block[block_of]).unwrap_or(0);
            pos_in_block = 0;
        }
        let dir = usize::try_from(dir_numbers[i]).unwrap_or(usize::MAX);
        let parent = sanitise(dir_names.get(dir).map(String::as_str).unwrap_or(""));
        let stored_name = if parent.is_empty() {
            names[i].clone()
        } else {
            format!("{parent}/{}", names[i])
        };
        entries.push(Entry {
            stored_name,
            size: sizes[i],
            time: times[i],
            is_dir: dir_flags[i],
            crc: crcs[i],
            block: block_of,
            pos_in_block,
        });
        pos_in_block += sizes[i];
        left_in_block = left_in_block.saturating_sub(1);
    }

    Ok(Directory { blocks, entries })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytestream::OutStream;

    /// `sanitise` had no direct test, and it is the one place a directory name
    /// READ FROM AN ARCHIVE is normalised.
    ///
    /// This port's own writer cannot produce a dangerous name -- `arc a` stores
    /// `../outside.txt` as `outside.txt` and an absolute path with its root
    /// stripped -- and the directory block's CRC stops one being patched into
    /// an existing archive. So the only way such a name arrives is from another
    /// tool, which is exactly the case this function exists for and exactly the
    /// case no end-to-end harness here can construct.
    #[test]
    fn sanitise_resolves_traversal_under_either_separator() {
        // `..` is resolved, not merely detected.
        assert_eq!(sanitise("../evil"), "evil");
        assert_eq!(sanitise("a/../../evil"), "evil");
        assert_eq!(sanitise("sub/../other"), "other");
        // Backslash is a separator here too -- see extract::SEPARATORS.
        assert_eq!(sanitise(r"..\..\evil"), "evil");
        assert_eq!(sanitise(r"a\..\..\evil"), "evil");
        // A leading root cannot survive: the empty first component is dropped.
        assert_eq!(sanitise("/etc/passwd"), "etc/passwd");
        assert_eq!(sanitise(r"\windows\system32"), "windows/system32");
        // Popping past the start must not underflow, and must not escape.
        assert_eq!(sanitise("../../.."), "");
        assert_eq!(sanitise("../../../etc"), "etc");
        // "." is dropped, which is also what keeps `arc a -r x.arc .` listings
        // from showing "./sub".
        assert_eq!(sanitise("./sub/./a"), "sub/a");
        // ...and ordinary names with dots are untouched.
        assert_eq!(sanitise("..hidden/a..b"), "..hidden/a..b");
        assert_eq!(sanitise("sub/dir/a.txt"), "sub/dir/a.txt");
    }

    /// Encode a directory the way archiveWriteDir does, column by column.
    struct Dir {
        blocks: Vec<(u64, &'static str, u64, u64)>, // (file count, compressor, offset, comp size)
        dirs: Vec<&'static str>,
        files: Vec<(&'static str, u64, u64, i64, bool, u32)>, // name, dir#, size, time, isdir, crc
    }

    fn encode(d: &Dir) -> Vec<u8> {
        let mut o = OutStream::new();
        assert!(o.varint(d.blocks.len() as u64));
        for b in &d.blocks {
            assert!(o.varint(b.0));
        }
        for b in &d.blocks {
            o.compressor(&[b.1.to_string()]);
        }
        for b in &d.blocks {
            assert!(o.varint(b.2));
        }
        for b in &d.blocks {
            assert!(o.varint(b.3));
        }
        assert!(o.varint(d.dirs.len() as u64));
        for n in &d.dirs {
            o.string(n);
        }
        for f in &d.files {
            o.string(f.0);
        }
        for f in &d.files {
            assert!(o.varint(f.1));
        }
        for f in &d.files {
            assert!(o.varint(f.2));
        }
        for f in &d.files {
            o.i64(f.3);
        }
        for f in &d.files {
            o.bool(f.4);
        }
        for f in &d.files {
            o.crc(f.5);
        }
        assert!(o.varint(TAG_END));
        o.into_bytes()
    }

    fn sample() -> Dir {
        Dir {
            // Two blocks: the first holds two files, the second holds one.
            blocks: vec![(2, "lzma:1mb", 5000, 900), (1, "storing", 2000, 40)],
            dirs: vec!["", "sub"],
            files: vec![
                ("a.txt", 0, 100, 1_700_000_000, false, 0x1111_1111),
                ("b.bin", 1, 250, 1_700_000_001, false, 0x2222_2222),
                ("c.dat", 1, 40, 1_700_000_002, false, 0x3333_3333),
            ],
        }
    }

    #[test]
    fn a_directory_round_trips_field_for_field() {
        let d = sample();
        let body = encode(&d);
        let got = read_directory(10_000, &body).expect("decodes");
        assert_eq!(got.entries.len(), 3);
        assert_eq!(got.entries[0].stored_name, "a.txt");
        assert_eq!(got.entries[1].stored_name, "sub/b.bin");
        assert_eq!(got.entries[2].stored_name, "sub/c.dat");
        assert_eq!(got.entries[1].size, 250);
        assert_eq!(got.entries[1].time, 1_700_000_001);
        assert_eq!(got.entries[2].crc, 0x3333_3333);
    }

    /// The two implicit fields. Getting either wrong extracts the right bytes
    /// from the wrong place, which round-trips as a CRC failure much later.
    #[test]
    fn block_membership_and_in_block_offsets_are_rebuilt() {
        let body = encode(&sample());
        let got = read_directory(10_000, &body).expect("decodes");
        assert_eq!(got.entries[0].block, 0);
        assert_eq!(got.entries[1].block, 0);
        assert_eq!(got.entries[2].block, 1, "the third file starts the second block");
        // Running sum WITHIN a block, reset at the boundary.
        assert_eq!(got.entries[0].pos_in_block, 0);
        assert_eq!(got.entries[1].pos_in_block, 100);
        assert_eq!(got.entries[2].pos_in_block, 0, "reset, not 350");
    }

    /// blOrigSize is not stored: it is the sum of the block's file sizes.
    #[test]
    fn block_orig_size_is_summed_from_the_files() {
        let body = encode(&sample());
        let got = read_directory(10_000, &body).expect("decodes");
        assert_eq!(got.blocks.len(), 2);
        assert_eq!(got.blocks[0].orig_size, 350);
        assert_eq!(got.blocks[1].orig_size, 40);
        assert_eq!(got.blocks[0].comp_size, 900);
        assert_eq!(got.blocks[0].files, Some(2));
    }

    /// Positions are relative to the DIRECTORY block, like the footer's.
    #[test]
    fn data_block_positions_are_relative_to_the_directory() {
        let body = encode(&sample());
        let got = read_directory(10_000, &body).expect("decodes");
        assert_eq!(got.blocks[0].pos, 10_000 - 5000);
        assert_eq!(got.blocks[1].pos, 10_000 - 2000);
    }

    /// mtime is a fixed eight bytes, not a varint. If it were read as one, every
    /// column after it would shift -- and the is-dir flags would decode as
    /// garbage rather than as an error.
    #[test]
    fn mtimes_are_eight_fixed_bytes() {
        let mut d = sample();
        // A time whose varint encoding would be much shorter than 8 bytes.
        d.files[0].3 = 1;
        let body = encode(&d);
        let got = read_directory(10_000, &body).expect("decodes");
        assert_eq!(got.entries[0].time, 1);
        assert!(!got.entries[0].is_dir, "columns after mtime are still aligned");
        assert_eq!(got.entries[0].crc, 0x1111_1111);
    }

    /// --nodates writes aMINIMAL_POSSIBLE_DATETIME, which is 0.
    #[test]
    fn nodates_archives_carry_a_zero_mtime() {
        let mut d = sample();
        for f in &mut d.files {
            f.3 = 0;
        }
        let body = encode(&d);
        let got = read_directory(10_000, &body).expect("decodes");
        assert!(got.entries.iter().all(|e| e.time == 0));
    }

    #[test]
    fn a_directory_entry_is_flagged_and_has_no_size() {
        let mut d = sample();
        d.files[0] = ("sub", 0, 0, 0, true, 0);
        let body = encode(&d);
        let got = read_directory(10_000, &body).expect("decodes");
        assert!(got.entries[0].is_dir);
        assert_eq!(got.entries[0].size, 0);
        assert_eq!(got.entries[1].pos_in_block, 0, "a zero-size entry advances nothing");
    }

    #[test]
    fn an_empty_directory_block_decodes_to_nothing() {
        let d = Dir { blocks: vec![], dirs: vec![], files: vec![] };
        let body = encode(&d);
        let got = read_directory(10_000, &body).expect("decodes");
        assert!(got.blocks.is_empty());
        assert!(got.entries.is_empty());
    }

    /// A corrupt count must be refused before it allocates, not after.
    /// The terminator is checked, not merely consumed. A tag other than
    /// aTAG_END means the columns did not end where the counts said they would.
    #[test]
    fn a_wrong_terminator_tag_is_refused() {
        let mut body = encode(&sample());
        // encode() writes TAG_END last, as a one-byte varint.
        let last = body.len() - 1;
        body[last] = 4; // varint for 2
        assert!(read_directory(10_000, &body).is_err());
    }

    #[test]
    fn an_implausible_file_count_is_refused() {
        let mut o = OutStream::new();
        assert!(o.varint(1)); // one block
        assert!(o.varint(1_000_000_000)); // holding a billion files
        o.compressor(&["storing".to_string()]);
        assert!(o.varint(0));
        assert!(o.varint(0));
        assert!(o.varint(0)); // no directory names
        let body = o.into_bytes();
        assert!(read_directory(10_000, &body).is_err());
    }

    /// Truncation is an error, never a panic -- this is untrusted input.
    #[test]
    fn every_truncation_of_a_valid_directory_is_an_error_not_a_panic() {
        let body = encode(&sample());
        for n in 0..body.len() {
            // Any verdict is acceptable; the test is that it neither panics nor
            // returns a directory built from bytes that were not there.
            match read_directory(10_000, &body[..n]) {
                Err(_) => {}
                Ok(d) => assert!(
                    d.entries.len() <= 3,
                    "a {n}-byte prefix produced {} entries",
                    d.entries.len()
                ),
            }
        }
    }
}
