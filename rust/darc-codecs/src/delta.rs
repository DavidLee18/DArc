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
pub fn decompress(io: &Io, block_size: u32, _extended_tables: c_int) -> c_int {
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
            // `len` is a raw u32 straight out of the archive; bound it against the
            // method's own block size and allocate fallibly, so a corrupt header is
            // an error rather than a multi-gigabyte request or an abort.
            match crate::ffi::archive_sized_buffer(len, block_size) {
                Ok(b) => *buf = b,
                Err(e) => return e,
            }
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

// ---------------------------------------------------------------------------
// Compression
// ---------------------------------------------------------------------------
//
// Unlike decompression, this half is all heuristics: which distances look like
// a table row width, where a table starts and ends, which columns are worth
// diffing. Every one of those decisions has to reproduce exactly, because the
// answers are recorded in the stream and a different answer means a different
// archive.

/// Size of the window in which repeated distances are counted (`LINE`).
const LINE: usize = 32;
/// Maximum deviation still considered table-like in the fast check (`DELTA`).
const DELTA: i32 = 8;

fn read_i16(buf: &[u8], at: isize) -> i16 {
    // C reads these as unaligned `*(int16*)`. Every call site is kept inside
    // the block by the loop guards; if one were not, the C original would be
    // reading uninitialised memory from its own BigAlloc block and would not be
    // reproducible either.
    //
    // `assert!`, not `debug_assert!`: an out-of-range read here means the loop
    // guards are wrong, and returning 0 for it would let a mis-transcribed guard
    // produce a plausible-looking but different stream in exactly the build that
    // ships. There is deliberately no fallback below.
    assert!(at >= 0 && (at as usize) + 2 <= buf.len(), "i16 read outside the block");
    i16::from_le_bytes([buf[at as usize], buf[at as usize + 1]])
}

fn read_u32(buf: &[u8], at: usize) -> u32 {
    if at + 4 > buf.len() {
        return 0;
    }
    u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]])
}

/// Whole part of a binary logarithm, matching `lb()` in Common.h (n > 0).
fn lb(n: u32) -> u32 {
    31 - n.leading_zeros()
}

/// Port of `search_for_table_boundary`.
///
/// `n` is a signed stride: the backward scan is called with `-N`. Returns the
/// index of the boundary, and the `useless` count through the out-parameter the
/// C version uses.
fn search_for_table_boundary(
    buf: &[u8],
    n: isize,
    start_at: isize,
    bufstart: isize,
    bufend: isize,
    out_useless: &mut i32,
) -> isize {
    let mut t = start_at;
    let mut dir: i32 = if (read_i16(buf, t + n) as i32) - (read_i16(buf, t) as i32) < 0 { -1 } else { 1 };
    let (mut len, mut omit, mut useless, mut bad) = (0i32, 0i32, 0i32, 0i32);
    *out_useless = 0;
    let mut lastpoint = t;
    let mut first_time = true;

    t += n;
    while bufstart <= t + n && t + n + 2 <= bufend {
        let diff = (read_i16(buf, t) as i32) - (read_i16(buf, t - n) as i32);
        let mut itemlb = lb(1 + (read_i16(buf, t) as i32).unsigned_abs());
        let difflb = lb(1 + diff.unsigned_abs());
        itemlb -= (itemlb > 10) as u32; // itemlb /= 1.1, per the original's comment

        if (dir < 0 && diff < 0) || (dir > 0 && diff > 0) {
            // The C source writes this as
            //     difflb < itemlb? len++,omit=0 : useless++,omit++;
            // where `?:` binds tighter than `,`, so it parses as
            //     (cond ? (len++, omit=0) : useless++), omit++;
            // and `omit++` therefore runs on BOTH paths -- the `omit=0` is
            // immediately undone to 1. Confirmed by compiling it, not by
            // reading it. `lastpoint = t - n*omit` depends on this.
            if difflb < itemlb {
                len += 1;
                omit = 0;
            } else {
                useless += 1;
            }
            omit += 1;
        } else if diff == 0 {
            useless += 1;
        } else {
            if len >= 4 || first_time {
                bad = 0;
                lastpoint = t - n * omit as isize;
                *out_useless = useless;
                first_time = false;
            } else {
                bad += 1;
                if bad >= 2 {
                    break; // second short monotonic run in a row
                }
            }
            dir = if (read_i16(buf, t + n) as i32) - (read_i16(buf, t) as i32) < 0 { -1 } else { 1 };
            len = 0;
            omit = 0;
            if dir * diff > 0 {
                t -= n; // V-shaped transition: restart at the current value
            }
        }
        t += n;
    }
    lastpoint
}

/// Port of `analyze_table`: decide per column whether it is near-constant
/// (left alone and gathered to the front) or worth diffing.
fn analyze_table(
    buf: &[u8],
    n: usize,
    table_start: usize,
    rows: usize,
    do_diff: &mut [bool; MAX_ELEMENT_SIZE],
    immutable: &mut [bool; MAX_ELEMENT_SIZE],
) {
    for k in 0..n {
        let mut neq = 0i32;
        let mut p = table_start + k;
        for _ in 1..rows {
            neq += (buf[p + n] != buf[p]) as i32;
            p += n;
        }
        // "Constant" means fewer than a quarter of the rows change. The N
        // exclusions are in the original and are load-bearing.
        immutable[k] = neq * 4 < rows as i32 && n != 2 && n != 4 && n != 8;
        do_diff[k] = !immutable[k];
    }
}

/// Port of `diff_table`: subtract each row from the next, LSB-first with carry
/// kept only across adjacent diffed columns.
fn diff_table(buf: &mut [u8], n: usize, table_start: usize, rows: usize, do_diff: &[bool; MAX_ELEMENT_SIZE]) {
    let mut row = rows;
    while row > 1 {
        row -= 1;
        let base = table_start + row * n;
        let prev = base - n;
        let mut carry = 0u32;
        for i in 0..n {
            if do_diff[i] {
                let cur = buf[base + i] as u32;
                let sub = buf[prev + i] as u32 + carry;
                let newcarry = (cur < sub) as u32;
                buf[base + i] = (cur.wrapping_sub(sub) & 0xff) as u8;
                carry = newcarry;
            } else {
                carry = 0;
            }
        }
    }
}

/// Port of `reorder_table`: move every immutable column ahead of the mutable
/// ones, which helps the LZ77 stage downstream.
fn reorder_table(
    buf: &mut [u8],
    n: usize,
    table_start: usize,
    rows: usize,
    immutable: &[bool; MAX_ELEMENT_SIZE],
    scratch: &mut Vec<u8>,
) {
    let len = n * rows;
    scratch.clear();
    scratch.extend_from_slice(&buf[table_start..table_start + len]);

    let mut p = table_start;
    for pass_immutable in [true, false] {
        let mut q = 0usize;
        for _ in 0..rows {
            for k in 0..n {
                if immutable[k] == pass_immutable {
                    buf[p] = scratch[q];
                    p += 1;
                }
                q += 1;
            }
        }
    }
}

fn encode_type(n: usize, immutable: &[bool; MAX_ELEMENT_SIZE]) -> u32 {
    let mut ty = 1u32 << n;
    for i in 0..n {
        ty += (immutable[i] as u32) << i;
    }
    ty
}

/// Port of `slow_check_for_data_table`. Returns the table extent and its
/// encoded type when the candidate is judged worth encoding.
#[allow(clippy::too_many_arguments)]
fn slow_check_for_data_table(
    buf: &mut [u8],
    n: usize,
    p: usize,
    bufstart: isize,
    bufend: isize,
    scratch: &mut Vec<u8>,
) -> Option<(usize, usize, u32)> {
    let mut useless = 0i32;
    let table_start = search_for_table_boundary(buf, -(n as isize), p as isize, bufstart, bufend, &mut useless);
    let table_end = search_for_table_boundary(buf, n as isize, table_start, bufstart, bufend, &mut useless);

    if table_end <= table_start {
        return None;
    }
    let rows = ((table_end - table_start) / n as isize) as usize;
    let useful = rows as i32 - useless;
    let skip_bits = (core::cmp::max(table_start - bufstart, 1) as f64).log2().floor();

    // The acceptance test, in double arithmetic exactly as the original:
    //   useful*sqrt(N) > 30 + 4*skipBits
    if (useful as f64) * (n as f64).sqrt() > 30.0 + 4.0 * skip_bits {
        let mut do_diff = [false; MAX_ELEMENT_SIZE];
        let mut immutable = [false; MAX_ELEMENT_SIZE];
        let ts = table_start as usize;
        analyze_table(buf, n, ts, rows, &mut do_diff, &mut immutable);
        diff_table(buf, n, ts, rows, &do_diff);
        reorder_table(buf, n, ts, rows, &immutable, scratch);
        return Some((ts, table_end as usize, encode_type(n, &immutable)));
    }
    None
}

/// Port of the `FAST_CHECK_FOR_DATA_TABLE` macro: a cheap filter before the
/// expensive boundary search.
fn fast_check(buf: &[u8], n: usize, p: usize) -> bool {
    let b = |off: usize| buf[p + off] as i32;
    if p + 3 * n + 2 > buf.len() {
        return false;
    }
    (((b(1) - b(n + 1) + DELTA) as u32) <= 2 * DELTA as u32)
        && (((b(n + 1) - b(2 * n + 1) + DELTA) as u32) <= 2 * DELTA as u32)
        && (((b(2 * n + 1) - b(3 * n + 1) + DELTA) as u32) <= 2 * DELTA as u32)
        && (read_i16(buf, p as isize) as i32 + read_i16(buf, (p + n) as isize) as i32
            != read_i16(buf, (p + 2 * n) as isize) as i32 + read_i16(buf, (p + 3 * n) as isize) as i32)
}

/// Port of `delta_compress`.
pub fn compress(io: &Io, block_size: u32, _extended_tables: c_int) -> c_int {
    let block_size = block_size.max(1) as usize;
    let mut buf = vec![0u8; block_size];
    let mut scratch: Vec<u8> = Vec::new();
    let (mut tskip, mut ttype, mut trows): (Vec<u8>, Vec<u8>, Vec<u8>) = (Vec::new(), Vec::new(), Vec::new());

    loop {
        // READ_LEN_OR_EOF: a non-positive result ends the stream.
        let size = io.read(&mut buf[..]);
        if size < 0 {
            return size;
        }
        if size == 0 {
            return OK;
        }
        let size = size as usize;

        tskip.clear();
        ttype.clear();
        trows.clear();

        let bufend = size as isize;
        let mut last_table_end: usize = 0;
        // C seeds these with `buf-1`, one before the block; -1 as a signed
        // offset is the same thing without forming an out-of-range pointer.
        let mut hash = [-1i64; 256];

        let mut ptr = LINE;
        while ptr + MAX_ELEMENT_SIZE * 4 < size {
            // Cheap skip for runs of identical bytes, e.g. blocks of zeroes.
            if read_u32(&buf, ptr) != read_u32(&buf, ptr + 3) {
                let mut count = [0u8; MAX_ELEMENT_SIZE];
                let mut p = ptr;
                for _ in 0..LINE {
                    let slot = (buf[p] / 16) as usize;
                    let n = p as i64 - hash[slot];
                    hash[slot] = p as i64;
                    if n <= MAX_ELEMENT_SIZE as i64 {
                        count[(n - 1) as usize] = count[(n - 1) as usize].wrapping_add(1);
                    }
                    p += 1;
                }

                let mut found_end: Option<usize> = None;
                'candidates: for i in 0..MAX_ELEMENT_SIZE {
                    if count[i] > 5 {
                        let n = i + 1;
                        for j in 0..n {
                            let p = ptr + j;
                            if !fast_check(&buf, n, p) {
                                continue;
                            }
                            match slow_check_for_data_table(
                                &mut buf, n, p, last_table_end as isize, bufend, &mut scratch,
                            ) {
                                Some((ts, te, ty)) => {
                                    tskip.extend_from_slice(
                                        &((ts - last_table_end) as u32).to_le_bytes(),
                                    );
                                    ttype.extend_from_slice(&ty.to_le_bytes());
                                    trows.extend_from_slice(
                                        &(((te - ts) / n) as u32).to_le_bytes(),
                                    );
                                    last_table_end = te;
                                    found_end = Some(te);
                                    break 'candidates;
                                }
                                // No table at this candidate; try the next `p`.
                                None => {}
                            }
                        }
                    }
                }
                match found_end {
                    Some(te) => {
                        ptr = core::cmp::max(ptr + LINE, te);
                        continue;
                    }
                    None => {}
                }
            }
            ptr += LINE;
        }

        // Emit the block: sizes, the three descriptor arrays, then the data.
        if io.write(&(size as u32).to_le_bytes()) < 0
            || io.write(&(ttype.len() as u32).to_le_bytes()) < 0
            || io.write(&tskip) < 0
            || io.write(&ttype) < 0
            || io.write(&trows) < 0
            || io.write(&buf[..size]) < 0
        {
            return FREEARC_ERRCODE_IO;
        }
    }
}
