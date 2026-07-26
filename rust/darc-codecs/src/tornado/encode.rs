//! The Tornado compression loop, ported from `Compression/Tornado/Tornado.cpp`
//! (`tor_compress_chunk` :133, `read_next_chunk` :94, `tor_compress` :307).
//!
//! ## What decides the output bytes
//!
//! Everything. Unlike the decoder, where the stream dictates each step, the
//! encoder is free to emit any valid parse -- so a port that is merely correct
//! produces a different archive than the C for the same input. Byte-identity is
//! the only useful bar, and it makes the window bookkeeping load-bearing in a
//! way it never was on the decode side:
//!
//! * `matchend` is `bufend - min(MAX_HASHED_BYTES, bufend-buf)`, and it is what
//!   `find_matchlen` is given as its limit -- *not* `bufend`. Matches therefore
//!   stop 12 bytes early, and the tail is coded as literals.
//! * `read_point` sits `LOOKAHEAD` bytes short of the data end, so the last
//!   256 bytes of a chunk are never parsed until more data arrives (or the
//!   input ends).
//! * The window slide is driven by `m.shift`: positive slides by that much,
//!   negative keeps that many bytes, and -1 means "do not slide, refill from
//!   scratch" -- which also clears the whole hash table rather than rebasing it.
//!
//! Each of those changes which matches are findable, so a plausible-looking
//! simplification changes the output.
//!
//! ## `compress_all_at_once`
//!
//! The C reads a global (`Common.cpp:6`) that is 0 everywhere except inside the
//! 4x4 codec, which sets it around a nested compressor call. It changes the
//! chunking, the output buffer size and whether the input is refilled at all.
//! It is threaded through here as a field rather than assumed away, but nothing
//! sets it yet: 4x4 is not ported, and wiring the drop-in will have to pass the
//! C global's value in explicitly, since the drop-in cannot see it.

use super::lz77_enc::{DynamicCoder, Lz77Encoder, IMPOSSIBLE_LEN};
use super::tables::{check_for_data_table, looks_like_table, LastChecked};
use super::matchfinder::{
    CachingMatchFinder, CombineMF, CycledCachingMatchFinder, ExactMatchFinder, Hash3,
    LazyMatching, MatchFinder, MatchFinder1, MatchFinder2, MAX_HASHED_BYTES,
};
use super::{ARICODER, BITCODER, BYTECODER, HUFCODER, STORING};
use crate::ffi::{Io, FREEARC_ERRCODE_GENERAL};
use core::ffi::c_int;

/// `LOOKAHEAD` (Tornado.cpp:62): the minimum lookahead the compressor keeps, and
/// the slack allocated past the end of the input buffer so `p[11]` needs no
/// check.
pub const LOOKAHEAD: usize = 256;

const KB: usize = 1024;
const LARGE_BUFFER_SIZE: usize = 256 * KB;
/// If no table turned up in this many bytes, skip the next `TABLE_SHIFT`
/// (Tornado.cpp:58).
const TABLE_DIST: usize = 256 * 1024;
const TABLE_SHIFT: usize = 128;
const HUGE_BUFFER_SIZE: usize = 8 * 1024 * KB;

/// `PackMethod` (Tornado.cpp:11), laid out to match the C so it can cross the
/// ABI by value the way `tor_compress` takes it.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct PackMethod {
    pub number: c_int,
    pub encoding_method: c_int,
    pub find_tables: bool,
    pub hash_row_width: c_int,
    pub hashsize: u32,
    pub caching_finder: c_int,
    pub buffer: u32,
    pub match_parser: c_int,
    pub hash3: c_int,
    pub shift: c_int,
    pub update_step: c_int,
    pub auxhash_size: u32,
    pub auxhash_row_width: c_int,
}

/// `tornado_compressor_outbuf_size` (:65).
fn outbuf_size(buffer: usize, all_at_once: bool) -> usize {
    if all_at_once {
        buffer + buffer / 8 + 512
    } else {
        HUGE_BUFFER_SIZE
    }
}

/// The parts of the loop state the window slide has to fix up together.
struct Window {
    buf: Vec<u8>,
    /// One past the last valid input byte.
    bufend: usize,
    /// Where the parser must stop and refill.
    read_point: usize,
    /// Bytes the callback returned last time; 0 means end of input.
    bytes: usize,
    /// How far `buf[0]` is into the original stream.
    offset: u64,
    /// Set by `read_next_chunk` when the window slid, so the caller can rebase
    /// the table bookkeeping it owns.
    last_shift: Option<usize>,
}

/// `tor_compress_chunk` (:133), specialised to one match finder and the runtime
/// coder. The C gets this by templating; the finder is a trait object here
/// because the nine live instantiations differ only in which finder they build.
fn compress_chunk(
    io: &Io,
    m: &PackMethod,
    mf: &mut dyn MatchFinder,
    coder_kind: u32,
    all_at_once: bool,
) -> Result<(), c_int> {
    let bufsize = m.buffer as usize;
    let minlen = mf.min_length() as i32;

    // Read in these chunks. `m.shift` has already been resolved to a concrete
    // value by the caller, so the `m.shift>0` test here is the C's.
    let chunk = if all_at_once {
        bufsize
    } else {
        (if m.shift > 0 { m.shift as usize } else { bufsize }).min(LARGE_BUFFER_SIZE)
    };

    // calloc(m.buffer+LOOKAHEAD, 1) -- zeroed, so hashing past the data end is
    // deterministic rather than reading whatever was on the heap.
    let mut w = Window {
        buf: vec![0u8; bufsize + LOOKAHEAD + MAX_HASHED_BYTES],
        bufend: 0,
        read_point: 0,
        bytes: 0,
        offset: 0,
        last_shift: None,
    };

    let n = io.read(&mut w.buf[..chunk]);
    if n < 0 {
        return Err(n);
    }
    w.bytes = n as usize;
    w.bufend = w.bytes;
    w.read_point = w.bufend - LOOKAHEAD.min(w.bytes);
    if all_at_once {
        w.read_point = w.bufend;
    }

    // The C constructs the match finder here -- *after* the first read
    // (:138 then :144) -- and every constructor calls clear_hash(buf) on the way
    // in. That ordering is load-bearing for the caching finders: their empty
    // slots store `key(buf+1)`, four bytes lifted out of the buffer, so seeding
    // them before the read would use zeros where the C uses real data. Every
    // subsequent comparison against those slots then takes a different branch.
    // For the non-caching finders this just rewrites the empty marker.
    mf.clear_hash(&w.buf);

    // `coder_kind` comes from the instantiation, not from the header field:
    // the STORING arm of the C's dispatch chain builds an LZ77_ByteCoder while
    // still writing 0 as the encoding method (:331-333). The resulting stream is
    // one no decoder accepts -- the C's own switch rejects STORING with
    // BAD_COMPRESSED_DATA (:522) -- but that is what the C produces.
    let mut coder = DynamicCoder::new(coder_kind, io, outbuf_size(bufsize, all_at_once), chunk * 2)
        .ok_or(FREEARC_ERRCODE_GENERAL)?;

    // Six-byte header (:154).
    coder.put8(m.encoding_method as u32);
    coder.put8(minlen as u32);
    coder.put32(m.buffer);

    // Table-detection state (:149-152). When tables are off, `table_end` is
    // parked past the end of the buffer so the `p > table_end` test never fires.
    let find_tables = coder.support_tables() && m.find_tables;
    let mut table_end: usize = if find_tables { 0 } else { bufsize + LOOKAHEAD };
    let mut last_found: usize = 0;
    let mut last_checked = LastChecked::new();

    // The first four bytes go out as literals, so the match finder's
    // `update()` can look back two bytes without a special case (:157).
    let mut matchend = w.bufend - MAX_HASHED_BYTES.min(w.bufend);
    let mut finished = false;
    for p in 0..4 {
        if p >= w.bufend {
            finished = true;
            break;
        }
        coder.encode(0, &w.buf, p, p as i32, minlen);
    }

    if !finished {
        let mut p = 4usize;
        loop {
            if p >= w.read_point {
                let before = w.bufend;
                match read_next_chunk(io, m, mf, &mut coder, &mut w, &mut p, chunk, all_at_once) {
                    Err(e) => return Err(e),
                    Ok(false) => break, // all input compressed
                    Ok(true) => {}
                }
                // A slide moves every recorded position (:113-116).
                if let Some(sh) = w.last_shift.take() {
                    let _ = before;
                    if find_tables {
                        table_end = if table_end > sh { table_end - sh } else { 0 };
                        last_found = if last_found > sh { last_found - sh } else { 0 };
                    }
                    last_checked.reset();
                }
                matchend = w.bufend - MAX_HASHED_BYTES.min(w.bufend);
            }

            // Check for a data table worth diffing (:177-185). `diff_table`
            // rewrites the buffer in place, so a match cached by the lazy
            // finder has to be dropped.
            if find_tables && p > table_end {
                let mut found = None;
                // The 2-byte check is skipped in the faster modes, where it
                // would not pay; min_length() is 2 exactly when Hash3 is on.
                if mf.min_length() < 4 && looks_like_table(&w.buf, p, 2) {
                    found = check_for_data_table(&mut w.buf, 2, p, w.bufend, &mut last_checked);
                }
                if found.is_none() && looks_like_table(&w.buf, p, 4) {
                    found = check_for_data_table(&mut w.buf, 4, p, w.bufend, &mut last_checked);
                }
                match found {
                    Some(t) => {
                        table_end = p + t.row * t.items;
                        coder.encode_table(t.row as i32, t.items as i32);
                        mf.invalidate_match();
                        last_found = table_end;
                    }
                    None => {
                        // Nothing found for a long while: skip ahead rather
                        // than keep paying for the check (:181).
                        if p - last_found > TABLE_DIST {
                            table_end = p + TABLE_SHIFT;
                        }
                    }
                }
            }

            let len = mf.find_matchlen(&w.buf, p, matchend, 0);
            let q = mf.get_matchptr();
            if coder.encode(len as i32, &w.buf, p, (p - q) as i32, minlen) == 0 {
                p += 1;
            } else {
                mf.update_hash(&w.buf, p, len, m.update_step as u32);
                p += len as usize;
            }
        }
    }

    if let Some(e) = mf.error() {
        return Err(e);
    }
    if let Some(e) = coder.error() {
        return Err(e);
    }
    // End of data (:209).
    coder.encode(IMPOSSIBLE_LEN, &w.buf, 0, i32::MAX / 2, minlen);
    coder.finish();
    match coder.error() {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

/// `read_next_chunk` (:94). Returns `Ok(true)` if there is more to compress,
/// `Ok(false)` when the input is exhausted.
#[allow(clippy::too_many_arguments)]
fn read_next_chunk(
    io: &Io,
    m: &PackMethod,
    mf: &mut dyn MatchFinder,
    coder: &mut DynamicCoder,
    w: &mut Window,
    p: &mut usize,
    chunk: usize,
    all_at_once: bool,
) -> Result<bool, c_int> {
    if w.bytes == 0 || all_at_once {
        return Ok(false);
    }
    let bufsize = m.buffer as usize;

    // Slide only once the free space at the end has shrunk below LOOKAHEAD.
    if w.bufend > bufsize - LOOKAHEAD {
        let sh = if m.shift == -1 {
            // Do not slide: move the parse position back to buf+2 and start the
            // hash over. Everything before p is discarded.
            *p - 2
        } else if m.shift > 0 {
            m.shift as usize
        } else {
            // Negative: keep that many bytes of history.
            (w.bufend as i64 + m.shift as i64) as usize
        };
        w.buf.copy_within(sh..w.bufend, 0);
        if m.shift == -1 {
            mf.clear_hash(&w.buf);
        } else {
            mf.shift(sh);
        }
        *p -= sh;
        w.bufend -= sh;
        w.offset += sh as u64;
        mf.invalidate_match();
        coder.shift_occurs();
        w.last_shift = Some(sh);
    }

    let want = chunk.min(bufsize - w.bufend);
    let end = w.bufend + want;
    // Bytes the callback does not deliver must read as zero, not as whatever the
    // previous chunk left there, or the hash of the tail becomes history-
    // dependent.
    w.buf[w.bufend..end].fill(0);
    let n = io.read(&mut w.buf[w.bufend..end]);
    if n < 0 {
        return Err(n);
    }
    w.bytes = n as usize;
    w.bufend += w.bytes;
    w.read_point = if w.bytes == 0 { w.bufend } else { w.bufend - LOOKAHEAD.min(w.bufend) };
    coder.flush();
    Ok(*p < w.bufend)
}

/// `tor_compress` (:307) -- resolve the method, pick the instantiation, run.
///
/// Only the instantiations whose match finders are ported are dispatched; the
/// rest return `FREEARC_ERRCODE_INVALID_COMPRESSOR` so a caller gets a clean
/// refusal rather than a stream that differs from the C's. The C's own chain
/// ends the same way for a combination it was not compiled for.
pub fn compress(mut m: PackMethod, io: &Io, all_at_once: bool) -> c_int {
    // (:221-230) Round the buffer up, shrink oversized hashes, resolve `shift`.
    m.buffer = ((m.buffer.max(32 * 1024) as u64 + 4095) & !4095) as u32;
    if m.hashsize / 8 > m.buffer {
        m.hashsize = 1 << lb32(m.buffer.saturating_mul(8).max(1));
    }
    if m.auxhash_size / 8 > m.buffer {
        m.auxhash_size = 1 << lb32(m.buffer.saturating_mul(8).max(1));
    }
    if m.shift == 0 {
        m.shift = if m.hash_row_width > 4 {
            (m.buffer / 4) as c_int
        } else if m.hash_row_width > 2 {
            (m.buffer / 2) as c_int
        } else if m.hashsize >= 512 * 1024 {
            (m.buffer / 4 * 3) as c_int
        } else {
            -1
        };
    }

    // The C's `#else` if-chain (:329-360), in its order. `plain` is every
    // condition the first six arms share.
    let e = m.encoding_method as u32;
    let row = m.hash_row_width;
    let plain = m.hash3 == 0 && m.caching_finder == 0 && m.match_parser == LAZY_OFF;

    let r = if (e == BYTECODER && row == 1 && plain) || e == STORING {
        // (:331) LZ77_ByteCoder either way -- STORING included.
        run(io, &m, MatchFinder1::new(m.hashsize, row), BYTECODER, all_at_once)
    } else if e == BITCODER && row == 1 && plain {
        // (:334)
        run(io, &m, MatchFinder1::new(m.hashsize, row), BITCODER, all_at_once)
    } else if e == HUFCODER && row == 2 && plain {
        // (:336)
        run(io, &m, MatchFinder2::new(m.hashsize, row), HUFCODER, all_at_once)
    } else if e == HUFCODER
        && row >= 2
        && m.hash3 == 0
        && m.caching_finder != 0
        && m.match_parser == LAZY_OFF
    {
        // (:338) CachingMatchFinder<4>. The condition tests `m.caching_finder`
        // for truth, not for 1.
        run(io, &m, CachingMatchFinder::new(4, m.hashsize, row), HUFCODER, all_at_once)
    } else if (e == ARICODER || e == HUFCODER)
        && row >= 2
        && m.hash3 == 1
        && m.caching_finder == 1
        && m.match_parser == LAZY_ON
    {
        // (:340) and (:343): the same finder under both coders. The second is
        // what FreeArc's -m4$compressed reaches via "-5 -c3".
        //
        // Hash3 makes min_length() 2 rather than 4, so the header's minlen byte
        // and the coder's MINLEN both change -- it is not merely an extra
        // lookup. Note this arm tests caching_finder == 1 exactly, where the
        // huffman arm above tests it for truth.
        let mf = LazyMatching::new(Hash3::new(
            CachingMatchFinder::new(4, m.hashsize, row),
            12,
            10,
            false,
        ));
        run(io, &m, mf, e, all_at_once)
    } else if row >= 2 && m.hash3 == 2 && m.match_parser == LAZY_ON && (5..=7).contains(&m.caching_finder)
    {
        // (:347), (:351) and (:354): three arms differing only in the cycled
        // finder's N and in what the auxiliary hash wraps. The coder is
        // LZ77_DynamicCoder, which picks a back-end from encoding_method at run
        // time -- which is exactly what DynamicCoder::new does.
        //
        // CombineMF gives its children the main and the *auxiliary* hash
        // geometry respectively, and its min_length() is the smaller of the
        // two -- so with Hash3 on the auxiliary side that is 2, not 5.
        let n = m.caching_finder as usize;
        let aux: Box<dyn MatchFinder> = if m.caching_finder == 5 {
            // -7..-9 pair the cycled finder with an exact one.
            Box::new(Hash3::new(
                ExactMatchFinder::new(4, m.auxhash_size, m.auxhash_row_width),
                16,
                12,
                true,
            ))
        } else {
            // -10 and -11 use a cycled finder on both sides.
            Box::new(Hash3::new(
                CycledCachingMatchFinder::new(4, m.auxhash_size, m.auxhash_row_width),
                16,
                12,
                true,
            ))
        };
        let mf = LazyMatching::new(CombineMF::new(
            CycledCachingMatchFinder::new(n, m.hashsize, row),
            aux,
        ));
        run(io, &m, mf, e, all_at_once)
    } else {
        // The remaining instantiations need the cycled caching finder, the
        // exact finder and CombineMF, or the data-table detector, none of
        // which are ported yet. Refusing is what the C's own chain does for a combination
        // it was not compiled for (:358).
        return FREEARC_ERRCODE_INVALID_COMPRESSOR;
    };
    match r {
        Ok(()) => crate::ffi::OK,
        Err(e) => e,
    }
}

/// `GREEDY` and `LAZY` (Tornado.cpp:35).
const LAZY_OFF: c_int = 1;
const LAZY_ON: c_int = 2;

fn run<M: MatchFinder + 'static>(
    io: &Io,
    m: &PackMethod,
    mut mf: M,
    coder_kind: u32,
    all_at_once: bool,
) -> Result<(), c_int> {
    if let Some(e) = mf.error() {
        return Err(e);
    }
    compress_chunk(io, m, &mut mf, coder_kind, all_at_once)
}

/// `lb` on a u32, as Common.h:507.
fn lb32(n: u32) -> u32 {
    31 - n.max(1).leading_zeros()
}

/// `FREEARC_ERRCODE_INVALID_COMPRESSOR` (Compression.h:21).
const FREEARC_ERRCODE_INVALID_COMPRESSOR: c_int = -2;
