//! BCJ x86: the `exe` branch-conversion filter, ported from
//! `Compression/LZMA/C_BCJ.cpp` and the three 7-Zip files it textually
//! `#include`s:
//!
//! * `Compression/LZMA/7zip/Compress/Branch/BranchX86.c:10-84` — `x86_Convert`,
//!   the filter itself.
//! * `Compression/LZMA/7zip/Compress/Branch/BranchCoder.cpp:6-18` —
//!   `CBranchConverter::Init` / `::Filter`, which own the cross-call
//!   `_bufferPos`.
//! * `Compression/LZMA/7zip/Compress/Branch/x86.cpp:6-14` — the two
//!   instantiations that pick `encoding = 1` / `0`.
//! * `Compression/LZMA/7zip/Compress/Branch/x86.h:12-19` — `CBranch86`, which
//!   owns the other half of the cross-call state, `_prevMask`.
//!
//! (Those four files are the only live part of `Compression/LZMA/7zip/`
//! reachable from `C_BCJ.cpp`; everything else under that directory is dead.)
//!
//! ## What it does
//!
//! `exe` does not compress. It rewrites the 32-bit *relative* displacement of
//! every x86 `CALL`/`JMP rel32` (opcodes `E8`/`E9`, matched as
//! `(b & 0xFE) == 0xE8`) into an *absolute* target, so that repeated calls to
//! the same function become repeated byte sequences a later codec in the `+`
//! chain can match. Output length equals input length; only four bytes per
//! accepted branch change.
//!
//! ## Bit-exactness hazards
//!
//! This filter is part of the archive format: a different byte here silently
//! corrupts, so every quirk below is reproduced deliberately rather than
//! cleaned up.
//!
//! 1. **The cross-call state is two fields, not one.** `_bufferPos`
//!    (`BranchCoder.h:16`) accumulates every processed byte across calls and
//!    feeds `nowPos`, which is *added to* the displacement — so a wrong
//!    `_bufferPos` changes the bytes written, not just a heuristic. `_prevMask`
//!    (`x86.h:14`) carries the last three positions' worth of "was there an
//!    E8/E9 here" bits. A port that resets either between buffers round-trips
//!    perfectly in one call and corrupts in two, which is why
//!    `rust/difftest/bcj-check.sh` feeds every input at ten different chunk
//!    sizes.
//!
//! 2. **`prevPosT` is a `size_t` that deliberately starts at `(SizeT)0 - 1`**
//!    (`BranchX86.c:17`) and is then used in `bufferPos - prevPosT`
//!    (`:29`, `:81`). The first hit therefore computes `bufferPos + 1` by
//!    wrapping around, i.e. "the previous branch was one byte before the
//!    buffer". [`x86_convert`] uses [`usize::wrapping_sub`] for exactly that.
//!    Using a signed type instead would agree on the common path and diverge on
//!    a hit at offset 0, 1 or 2.
//!
//! 3. **`(UInt32)bufferPos` truncates** (`BranchX86.c:58`, `:60`). Kept as an
//!    explicit `as u32`; it cannot bite at the 256 KiB buffer
//!    [`de_compress`] uses, but [`x86_convert`] is callable with any slice.
//!
//! 4. **`InSize <= 5` bypasses the filter entirely** (`C_BCJ.cpp:20`), and
//!    because the bypass does not go through `CBranchConverter::Filter`, those
//!    bytes are *not* added to `_bufferPos` and `_prevMask` is left alone. So a
//!    stream whose final read is 1..=5 bytes leaves the state describing fewer
//!    bytes than were actually emitted. That is what the C does; encoder and
//!    decoder agree only because they hit the same buffer boundaries.
//!
//! 5. **A short write is returned as-is** (`C_BCJ.cpp:21`, `:26`): the C
//!    returns the callback's value, which for a short-but-non-negative write is
//!    a *success* code. Notably `FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED` (-9)
//!    must be propagated unchanged — see [`crate::ffi::Io::write`] for the
//!    archives that were reported corrupt when a codec substituted its own
//!    error here.

use crate::ffi::{Io, CALLBACK_FUNC, FREEARC_ERRCODE_GENERAL, FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, OK};
use core::ffi::{c_int, c_void};

/// `LARGE_BUFFER_SIZE`, `Compression/Compression.h:41` (`256*kb`).
///
/// Read the constant, do not infer it: this value is not a tuning knob. It sets
/// where the read boundaries fall, and the boundaries decide `nowPos` for every
/// following branch, so changing it changes the output bytes.
const LARGE_BUFFER_SIZE: usize = 256 * 1024;

/// Which direction `x86_Convert`'s `encoding` flag selects
/// (`x86.cpp:8` passes 1, `x86.cpp:13` passes 0).
///
/// An enum rather than the C `int`, so the two call sites cannot be transposed
/// silently and `match` has to name both.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Direction {
    /// `CBCJ_x86_Encoder::SubFilter` — relative displacement to absolute.
    Encode,
    /// `CBCJ_x86_Decoder::SubFilter` — absolute back to relative.
    Decode,
}

/// `kMaskToAllowedStatus`, `BranchX86.c:7`.
const K_MASK_TO_ALLOWED_STATUS: [u8; 8] = [1, 1, 1, 0, 1, 0, 0, 0];
/// `kMaskToBitNumber`, `BranchX86.c:8`.
const K_MASK_TO_BIT_NUMBER: [u8; 8] = [0, 1, 2, 2, 3, 3, 3, 3];

/// `Test86MSByte`, `BranchX86.c:5`.
fn test86_ms_byte(b: u8) -> bool {
    b == 0 || b == 0xFF
}

/// `prevMask << (prevPosT - 1)` (`BranchX86.c:34`, `:82`), guarded.
///
/// `prevPosT` is in `1..=3` at both call sites — the `> 3` test above each one
/// establishes the upper bound, and the distance between two branch positions
/// is at least 1 because `bufferPos` always advances after `prevPosT` is set.
/// A zero would make the C shift by `-1`, which is undefined behaviour there
/// and a panic here, so the shift is done with `checked_shl` and asserted
/// instead of trusting the reasoning.
fn shift_mask(prev_mask: u32, prev_pos_t: usize) -> u32 {
    debug_assert!((1..=3).contains(&prev_pos_t), "prevPosT out of range: {prev_pos_t}");
    let shift = prev_pos_t.wrapping_sub(1) as u32;
    prev_mask.checked_shl(shift).unwrap_or(0) & 0x7
}

/// `x86_Convert`, `BranchX86.c:10-84`.
///
/// Converts in place and returns the number of bytes processed — always `<=
/// buffer.len()`, and normally `buffer.len() - 4`, because the last four bytes
/// of a buffer may be the start of a branch whose displacement is not all here
/// yet. The caller must carry that remainder over to the next buffer; see
/// [`de_compress`].
///
/// `now_pos` is the absolute offset of `buffer[0]` in the stream
/// (`CBranchConverter::_bufferPos`), and `prev_mask_mix` is the carried
/// `_prevMask`, updated on return. Note that a buffer shorter than 5 bytes
/// returns 0 *and leaves `prev_mask_mix` untouched* (`:14-15`).
pub fn x86_convert(
    buffer: &mut [u8],
    mut now_pos: u32,
    prev_mask_mix: &mut u32,
    dir: Direction,
) -> usize {
    let end_pos = buffer.len();
    let mut buffer_pos: usize = 0;
    let mut prev_mask: u32 = *prev_mask_mix & 0x7;
    if end_pos < 5 {
        return 0;
    }
    now_pos = now_pos.wrapping_add(5);
    // (SizeT)0 - 1: see hazard 2 in the module comment.
    let mut prev_pos_t: usize = usize::MAX;

    // `limit` is where a branch can still have its whole displacement inside
    // the buffer. `end_pos >= 5` makes the subtraction safe.
    let limit = end_pos - 4;

    loop {
        // Scan for the next E8/E9. `buffer_pos` can already be past `limit`
        // when the previous iteration consumed a branch at the very end, in
        // which case the C loop body does not execute and `bufferPos` keeps its
        // (out-of-range-for-scanning) value.
        let mut p = buffer_pos;
        while p < limit {
            if buffer[p] & 0xFE == 0xE8 {
                break;
            }
            p += 1;
        }
        buffer_pos = p;
        if p >= limit {
            break;
        }

        prev_pos_t = buffer_pos.wrapping_sub(prev_pos_t);
        if prev_pos_t > 3 {
            prev_mask = 0;
        } else {
            prev_mask = shift_mask(prev_mask, prev_pos_t);
            if prev_mask != 0 {
                let idx = K_MASK_TO_BIT_NUMBER[prev_mask as usize] as usize;
                // `idx` is 1..=3 for a nonzero mask, so this stays inside the
                // buffer: buffer_pos + 4 < end_pos.
                let b = buffer[buffer_pos + 4 - idx];
                if K_MASK_TO_ALLOWED_STATUS[prev_mask as usize] == 0 || test86_ms_byte(b) {
                    prev_pos_t = buffer_pos;
                    prev_mask = ((prev_mask << 1) & 0x7) | 1;
                    buffer_pos += 1;
                    continue;
                }
            }
        }
        prev_pos_t = buffer_pos;

        if test86_ms_byte(buffer[buffer_pos + 4]) {
            let mut src = (u32::from(buffer[buffer_pos + 4]) << 24)
                | (u32::from(buffer[buffer_pos + 3]) << 16)
                | (u32::from(buffer[buffer_pos + 2]) << 8)
                | u32::from(buffer[buffer_pos + 1]);
            let mut dest: u32;
            loop {
                // (UInt32)bufferPos truncates in the C; hazard 3.
                let pos = now_pos.wrapping_add(buffer_pos as u32);
                dest = match dir {
                    Direction::Encode => pos.wrapping_add(src),
                    Direction::Decode => src.wrapping_sub(pos),
                };
                if prev_mask == 0 {
                    break;
                }
                // 8, 16 or 24 for a nonzero mask, so neither shift below can
                // reach 32 (which would be undefined in C and a panic here).
                let index = u32::from(K_MASK_TO_BIT_NUMBER[prev_mask as usize]) * 8;
                let b = (dest >> (24 - index)) as u8;
                if !test86_ms_byte(b) {
                    break;
                }
                src = dest ^ ((1u32 << (32 - index)) - 1);
            }
            // `~(((dest >> 24) & 1) - 1)` truncated to a byte: 0xFF when the
            // top bit of the absolute target is set, 0x00 otherwise. Written as
            // the C wrote it, wrapping made explicit.
            buffer[buffer_pos + 4] = !(((dest >> 24) & 1).wrapping_sub(1)) as u8;
            buffer[buffer_pos + 3] = (dest >> 16) as u8;
            buffer[buffer_pos + 2] = (dest >> 8) as u8;
            buffer[buffer_pos + 1] = dest as u8;
            buffer_pos += 5;
        } else {
            prev_mask = ((prev_mask << 1) & 0x7) | 1;
            buffer_pos += 1;
        }
    }

    prev_pos_t = buffer_pos.wrapping_sub(prev_pos_t);
    *prev_mask_mix = if prev_pos_t > 3 { 0 } else { shift_mask(prev_mask, prev_pos_t) };
    buffer_pos
}

/// `CBranchConverter` plus `CBranch86`: the filter's cross-call state.
///
/// `BranchCoder.h:11-24` and `x86.h:12-19`. Kept as one struct because the two
/// fields are only ever used together, and separating them is how a caller
/// forgets one.
#[derive(Clone, Copy, Debug, Default)]
pub struct X86Filter {
    /// `CBranchConverter::_bufferPos` (`BranchCoder.h:16`): total bytes
    /// processed so far, i.e. `nowPos` for the next buffer.
    buffer_pos: u32,
    /// `CBranch86::_prevMask` (`x86.h:14`).
    prev_mask: u32,
}

impl X86Filter {
    /// `CBranchConverter::Init` (`BranchCoder.cpp:6-11`) followed by
    /// `CBranch86::x86Init` (`x86.h:15`, i.e. `x86_Convert_Init`,
    /// `BranchX86.h:8`): both fields zero.
    pub fn new() -> Self {
        X86Filter { buffer_pos: 0, prev_mask: 0 }
    }

    /// `CBranchConverter::Filter` (`BranchCoder.cpp:13-18`): run the converter
    /// over `data` and advance `_bufferPos` by however much it processed.
    pub fn filter(&mut self, data: &mut [u8], dir: Direction) -> usize {
        let processed = x86_convert(data, self.buffer_pos, &mut self.prev_mask, dir);
        // `_bufferPos += processedSize` is a UInt32 add; it wraps in the C on a
        // stream past 4 GiB, and the wrap is part of the output.
        self.buffer_pos = self.buffer_pos.wrapping_add(processed as u32);
        processed
    }
}

/// `bcj_x86_de_compress<T>`, `C_BCJ.cpp:9-28`: the callback-driven streaming
/// loop, shared by both directions.
///
/// The shape matters as much as the filter: each round reads into the buffer
/// *after* the remainder left by the previous round, filters the concatenation,
/// writes what was processed, and `memmove`s the new remainder to the front.
pub fn de_compress(io: &Io, dir: Direction) -> c_int {
    let mut filter = X86Filter::new();
    // `malloc(LARGE_BUFFER_SIZE)`; the C returns
    // FREEARC_ERRCODE_NOT_ENOUGH_MEMORY when it fails (`C_BCJ.cpp:14`).
    let mut buf: Vec<u8> = Vec::new();
    match buf.try_reserve_exact(LARGE_BUFFER_SIZE) {
        Ok(()) => buf.resize(LARGE_BUFFER_SIZE, 0),
        Err(_) => return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY,
    }

    // Data left over from the previous round: at most 4 bytes, since
    // `x86_convert` stops 4 before the end.
    let mut remainder: usize = 0;

    loop {
        let x = io.read(&mut buf[remainder..]);
        if x < 0 {
            // `while (... >= 0)` falls out of the loop to `Error: return x`.
            return x;
        }
        let in_size = x as usize + remainder;
        if in_size == 0 {
            return OK;
        }

        // "this filter doesn't handle less than 5 bytes :)" — and note the
        // bypass skips `Filter`, so the state does not advance. Hazard 4.
        let out_size = match in_size <= 5 {
            true => in_size,
            false => filter.filter(&mut buf[..in_size], dir),
        };

        let written = io.write(&buf[..out_size]);
        if written != out_size as c_int {
            // Returned unchanged, including non-negative short writes and
            // FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED. Hazard 5.
            return written;
        }

        remainder = in_size - out_size;
        if remainder > 0 {
            buf.copy_within(out_size..in_size, 0);
        }
    }
}

// ---------------------------------------------------------------------------
// C entry points.
//
// `Compression/LZMA/C_BCJ.h:4-6` already declares `bcj_x86_compress` and
// `bcj_x86_decompress`, inside the `extern "C"` block C_BCJ.cpp wraps that
// header in — and nothing in the C tree ever defined them. So these two names
// are the natural drop-in: `BCJ_X86_METHOD::compress` / `::decompress` call
// them instead of instantiating the template, and no header changes at all.
//
// The `darc_rs_`-prefixed pair mirrors the convention in `exports.rs` and is
// what `rust/difftest/bcj_ref.cpp` links against, so the harness cannot
// accidentally resolve the C symbol of the same name.
// ---------------------------------------------------------------------------

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bcj_x86_compress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    crate::ffi::guard(move || {
        match Io::new(callback, auxdata) {
            Some(io) => de_compress(&io, Direction::Encode),
            None => FREEARC_ERRCODE_GENERAL,
        }
    })
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bcj_x86_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    crate::ffi::guard(move || {
        match Io::new(callback, auxdata) {
            Some(io) => de_compress(&io, Direction::Decode),
            None => FREEARC_ERRCODE_GENERAL,
        }
    })
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn bcj_x86_compress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    crate::ffi::guard(move || {
        darc_rs_bcj_x86_compress(callback, auxdata)
    })
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn bcj_x86_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    crate::ffi::guard(move || {
        darc_rs_bcj_x86_decompress(callback, auxdata)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `CALL rel32` at `at`, displacement `disp`, inside `len` bytes of
    /// filler chosen not to contain E8/E9 (0x90 = NOP).
    fn call_at(len: usize, at: usize, disp: i32) -> Vec<u8> {
        let mut v = vec![0x90u8; len];
        v[at] = 0xE8;
        v[at + 1..at + 5].copy_from_slice(&disp.to_le_bytes());
        v
    }

    fn enc(buf: &mut [u8], now_pos: u32, mask: &mut u32) -> usize {
        x86_convert(buf, now_pos, mask, Direction::Encode)
    }

    #[test]
    fn under_five_bytes_is_a_no_op_and_keeps_the_mask() {
        // BranchX86.c:14-15 returns 0 *without* touching *prevMaskMix.
        let mut buf = [0xE8u8, 0x00, 0x00, 0x00];
        let before = buf;
        let mut mask = 0x5;
        assert_eq!(enc(&mut buf, 0, &mut mask), 0);
        assert_eq!(buf, before);
        assert_eq!(mask, 0x5);
    }

    #[test]
    fn no_branch_bytes_processes_all_but_four() {
        let mut buf = vec![0x90u8; 64];
        let mut mask = 0;
        assert_eq!(enc(&mut buf, 0, &mut mask), 60);
        assert!(buf.iter().all(|&b| b == 0x90));
    }

    #[test]
    fn one_call_becomes_absolute() {
        // disp is relative to the byte after the instruction, i.e. at+5, and
        // nowPos + 5 + bufferPos is exactly that. With the branch at offset 8
        // and nowPos 0, an absolute target of 8+5+16 = 29 is expected.
        let mut buf = call_at(64, 8, 16);
        let mut mask = 0;
        assert_eq!(enc(&mut buf, 0, &mut mask), 60);
        assert_eq!(&buf[8..13], &[0xE8, 29, 0, 0, 0]);
    }

    #[test]
    fn negative_displacement_round_trips() {
        let mut buf = call_at(200, 100, -40);
        let orig = buf.clone();
        let mut mask = 0;
        enc(&mut buf, 0, &mut mask);
        assert_ne!(buf, orig, "the filter did not touch the branch");
        let mut mask = 0;
        x86_convert(&mut buf, 0, &mut mask, Direction::Decode);
        assert_eq!(buf, orig);
    }

    /// `C_BCJ.cpp:9-28`'s loop over an in-memory stream, with the read callback
    /// capped at `chunk` bytes. `de_compress` itself needs an `Io`, which needs
    /// a C callback, so the wrapper's arithmetic is reproduced here; the
    /// differential harness drives the real entry points.
    fn run(input: &[u8], chunk: usize, dir: Direction) -> Vec<u8> {
        let mut f = X86Filter::new();
        let mut buf = vec![0u8; LARGE_BUFFER_SIZE];
        let mut out: Vec<u8> = Vec::new();
        let mut pos = 0usize;
        let mut remainder = 0usize;
        loop {
            let want = (LARGE_BUFFER_SIZE - remainder).min(chunk);
            let got = (input.len() - pos).min(want);
            buf[remainder..remainder + got].copy_from_slice(&input[pos..pos + got]);
            pos += got;
            let in_size = got + remainder;
            if in_size == 0 {
                return out;
            }
            let out_size = match in_size <= 5 {
                true => in_size,
                false => f.filter(&mut buf[..in_size], dir),
            };
            out.extend_from_slice(&buf[..out_size]);
            remainder = in_size - out_size;
            if remainder > 0 {
                buf.copy_within(out_size..in_size, 0);
            }
        }
    }

    /// Branches at varied alignments and spacings, including pairs 1-3 bytes
    /// apart (which is the only thing that drives `prevMask`).
    fn branchy(len: usize) -> Vec<u8> {
        let mut v = vec![0x90u8; len];
        let mut at = 3usize;
        let mut step = 7usize;
        while at + 5 < len {
            v[at] = match at % 3 {
                0 => 0xE8,
                1 => 0xE9,
                _ => 0xE8,
            };
            let disp: i32 = (at as i32) * 37 - 5000;
            v[at + 1..at + 5].copy_from_slice(&disp.to_le_bytes());
            step = 1 + (step * 5 + 1) % 11;
            at += step;
        }
        v
    }

    /// The whole point of the carried state: `_bufferPos` is an *absolute*
    /// stream offset, and `_prevMask` is pre-shifted on the way out so that a
    /// branch pair straddling a buffer boundary still sees the right distance.
    /// Both make the emitted stream independent of where the read boundaries
    /// fall — so every chunk size at or above 6 must produce identical bytes.
    /// This is the test that fails when either field is reset between buffers.
    #[test]
    fn output_is_independent_of_chunking() {
        let src = branchy(300_000);
        for dir in [Direction::Encode, Direction::Decode] {
            let reference = run(&src, LARGE_BUFFER_SIZE, dir);
            assert_eq!(reference.len(), src.len());
            assert_ne!(reference, src, "nothing was filtered at all ({dir:?})");
            for chunk in [6usize, 7, 9, 64, 4095, 4096, 65536, 262_143, LARGE_BUFFER_SIZE] {
                assert_eq!(run(&src, chunk, dir), reference, "chunk {chunk} diverged ({dir:?})");
            }
        }
    }

    /// Hazard 4: with every read at most 5 bytes, `InSize <= 5` holds forever
    /// and the filter is never invoked, so the stream passes through unchanged.
    #[test]
    fn chunks_of_five_or_less_bypass_the_filter() {
        let src = branchy(4096);
        for chunk in [1usize, 2, 3, 4, 5] {
            assert_eq!(run(&src, chunk, Direction::Encode), src, "chunk {chunk} filtered something");
        }
    }

    #[test]
    fn stream_round_trip_at_many_chunk_sizes() {
        let mut src = vec![0u8; 300_000];
        let mut s = 12345u32;
        for (i, b) in src.iter_mut().enumerate() {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            *b = match i % 23 {
                0 => 0xE8,
                1 => 0xE9,
                _ => (s >> 16) as u8,
            };
        }

        for chunk in [1usize, 2, 5, 6, 7, 4096, 65536, LARGE_BUFFER_SIZE] {
            let encoded = run(&src, chunk, Direction::Encode);
            assert_eq!(encoded.len(), src.len(), "chunk {chunk}: length changed");
            let decoded = run(&encoded, chunk, Direction::Decode);
            assert_eq!(decoded, src, "chunk {chunk}: round trip differs");
        }
    }

    #[test]
    fn adjacent_branches_drive_the_prev_mask() {
        // Two E8s one byte apart: the second is inspected through
        // kMaskToBitNumber/kMaskToAllowedStatus rather than converted blindly.
        let mut buf = vec![0x90u8; 64];
        buf[10] = 0xE8;
        buf[11] = 0xE8;
        buf[12] = 0x00;
        buf[13] = 0x00;
        buf[14] = 0x00;
        buf[15] = 0x00;
        let mut mask = 0;
        let n = enc(&mut buf, 0, &mut mask);
        assert_eq!(n, 60);
        // Whatever it did, it must be reversible.
        let mut mask = 0;
        let mut back = buf.clone();
        x86_convert(&mut back, 0, &mut mask, Direction::Decode);
        let mut want = vec![0x90u8; 64];
        want[10] = 0xE8;
        want[11] = 0xE8;
        want[12] = 0x00;
        want[13] = 0x00;
        want[14] = 0x00;
        want[15] = 0x00;
        assert_eq!(back, want);
    }

    #[test]
    fn mask_is_carried_out_when_a_branch_ends_near_the_buffer_end() {
        // A branch at the last scannable position leaves prevPosT close to
        // bufferPos, so the outgoing mask is nonzero — the state a naive port
        // drops.
        let mut buf = vec![0x90u8; 32];
        buf[27] = 0xE8; // limit is 28, so this is the last position scanned
        let mut mask = 0;
        let n = enc(&mut buf, 0, &mut mask);
        assert_eq!(n, 28);
        assert_ne!(mask, 0, "outgoing prevMask was dropped");
    }
}
