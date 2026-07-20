//! Delta: binary table preprocessor, ported from Compression/Delta/Delta.cpp
//! (originally Bulat Ziganshin, 2008).
//!
//! Delta does not compress. It finds tables of fixed-width binary records,
//! subtracts each row from the previous one column-wise, and moves near-constant
//! columns to the front, so that a *later* codec in the chain compresses the
//! result better. Output is therefore about the same size as input by design.
//!
//! The port must be bit-exact. These transforms are part of the archive format:
//! a decompressor that produces different bytes than the C original silently
//! corrupts every archive it touches, and one that writes different bytes than
//! the C compressor makes archives older builds cannot read.
//!
//! Decompression is ported first, deliberately. It carries no heuristics -- the
//! table layout is recorded in the stream, so this code only replays decisions
//! the compressor already made -- and it can be validated immediately by
//! decompressing output the C compressor produced.
//!
//! Stream format, one or more blocks until EOF (all integers little-endian,
//! matching FREEARC_INTEL_BYTE_ORDER):
//!
//!   u32   data_size
//!   u32   table_size            bytes in each of the three descriptor arrays
//!   u8    skip[table_size]      per table: bytes to skip since the previous one
//!   u8    type[table_size]      per table: packed column layout, see decode_type
//!   u8    rows[table_size]      per table: number of rows
//!   u8    data[data_size]
//!
//! There are `table_size / 4` tables, the arrays being u32 each.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

/// Maximum size of one table element. `Delta.cpp` sizes its `doDiff` and
/// `immutable` arrays with this and then fills them from a value decoded out of
/// the stream, without bounding it -- so a hostile or corrupt `type` word walks
/// off the end of two stack arrays there. Here it is a hard limit and an error.
const MAX_ELEMENT_SIZE: usize = 30;

/// Decode the packed `type` word into column count and per-column flags.
///
/// The C original (`decode_type`) is:
///     for (i=0; type>1; i++, type>>=1) { immutable[i] = type&1; doDiff[i] = !immutable[i]; }
///     N = i;
/// i.e. the position of the highest set bit is the element width, and the bits
/// below it mark columns that must not be diffed.
///
/// Returns `None` when the width exceeds MAX_ELEMENT_SIZE, which the C version
/// would have written past its arrays for.
fn decode_type(mut ty: u32) -> Option<(usize, [bool; MAX_ELEMENT_SIZE], [bool; MAX_ELEMENT_SIZE])> {
    let mut do_diff = [false; MAX_ELEMENT_SIZE];
    let mut immutable = [false; MAX_ELEMENT_SIZE];
    let mut n = 0usize;
    while ty > 1 {
        if n >= MAX_ELEMENT_SIZE {
            return None;
        }
        immutable[n] = ty & 1 != 0;
        do_diff[n] = !immutable[n];
        n += 1;
        ty >>= 1;
    }
    Some((n, do_diff, immutable))
}

/// Add each element to the previous one, undoing `diff_table`.
///
/// Byte-wise with carry, LSB first, and the carry is kept only across adjacent
/// diffed columns -- a non-diffed column resets it. Mirrors `undiff_table`:
///     sum = r[i] + r[i-N] + carry;  r[i] = sum;  carry = sum/256;
/// `r[i] = sum` truncates to a byte in C; that truncation is load-bearing, so
/// it is spelled out here rather than left to an `as` cast.
fn undiff_table(n: usize, table: &mut [u8], rows: usize, do_diff: &[bool; MAX_ELEMENT_SIZE]) {
    for row in 1..rows {
        let base = row * n;
        let prev = base - n;
        let mut carry = 0u32;
        for i in 0..n {
            if do_diff[i] {
                let sum = table[base + i] as u32 + table[prev + i] as u32 + carry;
                table[base + i] = (sum & 0xff) as u8;
                carry = sum >> 8;
            } else {
                carry = 0;
            }
        }
    }
}

/// Undo `reorder_table`, which had gathered every immutable column ahead of
/// every mutable one.
///
/// Mirrors `unreorder_table`, including its early return: with no immutable
/// columns, or with every column immutable, the reordering was a no-op and must
/// not be undone.
fn unreorder_table(
    n: usize,
    table: &mut [u8],
    rows: usize,
    immutable: &[bool; MAX_ELEMENT_SIZE],
    scratch: &mut Vec<u8>,
) {
    let imm_columns = immutable[..n].iter().filter(|&&b| b).count();
    if imm_columns == 0 || imm_columns == n {
        return;
    }

    let len = n * rows;
    scratch.clear();
    scratch.extend_from_slice(&table[..len]);

    // Immutable columns were stored first, all `rows` of them, then the mutable
    // ones. Walk both runs in step and interleave them back.
    let mut q = 0usize; // cursor into the immutable run
    let mut q1 = imm_columns * rows; // cursor into the mutable run
    let mut p = 0usize;
    for _ in 0..rows {
        for k in 0..n {
            table[p] = if immutable[k] {
                let b = scratch[q];
                q += 1;
                b
            } else {
                let b = scratch[q1];
                q1 += 1;
                b
            };
            p += 1;
        }
    }
}

fn read_u32_le(b: &[u8]) -> u32 {
    u32::from_le_bytes([b[0], b[1], b[2], b[3]])
}

/// Port of `delta_decompress`.
///
/// `block_size` and `extended_tables` are accepted to match the C signature but
/// are unused on this path, exactly as in the original: the block layout is
/// taken entirely from the stream.
pub fn decompress(io: &Io, _block_size: u32, _extended_tables: c_int) -> c_int {
    let mut data: Vec<u8> = Vec::new();
    let mut tskip: Vec<u8> = Vec::new();
    let mut ttype: Vec<u8> = Vec::new();
    let mut trows: Vec<u8> = Vec::new();
    let mut scratch: Vec<u8> = Vec::new();

    loop {
        // READ4_OR_EOF: a zero-length read ends the stream cleanly; anything
        // else short is an IO error.
        let mut hdr = [0u8; 4];
        match io.read(&mut hdr) {
            0 => return OK,
            4 => {}
            n if n < 0 => return n,
            _ => return FREEARC_ERRCODE_IO,
        }
        let data_size = read_u32_le(&hdr) as usize;

        let mut hdr2 = [0u8; 4];
        if io.read(&mut hdr2) != 4 {
            return FREEARC_ERRCODE_IO;
        }
        let table_size = read_u32_le(&hdr2) as usize;

        // The three descriptor arrays are parallel and each holds u32s.
        if table_size % 4 != 0 {
            return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
        }

        for (buf, len) in [
            (&mut tskip, table_size),
            (&mut ttype, table_size),
            (&mut trows, table_size),
            (&mut data, data_size),
        ] {
            buf.clear();
            buf.resize(len, 0);
            if len != 0 {
                let got = io.read(&mut buf[..]);
                if got < 0 {
                    return got;
                }
                if got as usize != len {
                    return FREEARC_ERRCODE_IO;
                }
            }
        }

        // Replay the recorded tables over the block.
        let mut p = 0usize;
        for i in 0..(table_size / 4) {
            let off = i * 4;
            let skip = read_u32_le(&tskip[off..]) as usize;
            let ty = read_u32_le(&ttype[off..]);
            let rows = read_u32_le(&trows[off..]) as usize;

            let Some((n, do_diff, immutable)) = decode_type(ty) else {
                return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
            };

            // Every one of these would be an out-of-bounds walk in the C
            // version, which trusts the descriptors completely.
            p = match p.checked_add(skip) {
                Some(v) => v,
                None => return FREEARC_ERRCODE_BAD_COMPRESSED_DATA,
            };
            let span = match n.checked_mul(rows) {
                Some(v) => v,
                None => return FREEARC_ERRCODE_BAD_COMPRESSED_DATA,
            };
            let end = match p.checked_add(span) {
                Some(v) => v,
                None => return FREEARC_ERRCODE_BAD_COMPRESSED_DATA,
            };
            if end > data.len() {
                return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
            }

            if n > 0 && rows > 0 {
                let table = &mut data[p..end];
                unreorder_table(n, table, rows, &immutable, &mut scratch);
                undiff_table(n, table, rows, &do_diff);
            }
            p = end;
        }

        // WRITE: only a negative return is a failure. The write callback "on
        // success guarantees to write all the data and may return 0"
        // (Compression.h:100), so this must not insist on an exact count.
        if data_size != 0 {
            let got = io.write(&data);
            if got < 0 {
                return got;
            }
        }
    }
}
