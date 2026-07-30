//! DisPack forward filter -- `DisFilter` and `DisFilterCtx` (`DisPack.cpp:328-654`).
//!
//! The mirror of [`super::filter`]. Where the decoder reassembles one
//! instruction stream from `ST_MAX` parallel byte streams, this splits it: the
//! opcode goes to `ST_OP`, a byte displacement to whichever `ST_DISP8_Rn`
//! matches the register, an immediate to `ST_IMM8/16/32`, and so on. Grouping
//! like with like is the whole trick -- the entropy coder downstream sees runs
//! of similar bytes instead of interleaved instruction fields.
//!
//! Two transforms go beyond splitting, and both are why this must be
//! byte-exact rather than merely reversible:
//!
//! * **Relative call/jump targets become absolute.** The same callee reached
//!   from different call sites has a different relative displacement each time
//!   but one absolute address, which compresses far better.
//! * **Call targets are then MTF-coded.** A recently-called function costs one
//!   index byte in `ST_CALL_IDX` instead of four address bytes in `ST_CALL32`.
//!
//! ## Endianness is asymmetric, and deliberately so
//!
//! The instruction stream is x86, so operands are read **little-endian**
//! (`Fetch16`/`Fetch32`). The values written into the output streams are
//! **big-endian** (`Store16B`/`Store32B`) -- putting the high-order byte first
//! groups the slowly-varying bytes of nearby addresses together, which is again
//! about what the entropy coder sees. But the `ST_MAX` stream *sizes* in the
//! block header are written with `Write32`, which is **little-endian**.
//!
//! So: header LE, payload BE. Confirmed against the ported decoder rather than
//! inferred -- [`super::filter`] reads the header with `from_le_bytes` and the
//! payload with `from_be_bytes`.

use super::tables::*;
use crate::ffi::{Io, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

/// `MAXINSTR` (`DisPack.cpp:121`) -- the longest instruction this encoder will
/// consume. The tail loop pads to this, so `process_instr` may always read it.
pub const MAXINSTR: usize = 15;

/// `OP_CALLF` -- far call, one of the three opcodes carrying a 16-bit operand
/// ahead of the normal operand flow.
const OP_CALLF: u8 = 0x9a;

/// `FindMTF` (`DisPack.cpp:246`): index of `val`, moving it to the front.
///
/// Returns `None` when absent, having inserted it -- the caller then emits the
/// full 32-bit address and a `0` index byte, which is what tells the decoder to
/// read one. Note the search covers 255 entries, not 256: the last slot is
/// write-only, evicted by `add_mtf` before it can ever be found.
fn find_mtf(mtf: &mut [u32; 256], val: u32) -> Option<usize> {
    for i in 0..255 {
        if mtf[i] == val {
            move_to_front(mtf, i, val);
            return Some(i);
        }
    }
    add_mtf(mtf, val);
    None
}

/// `DisFilterCtx` (`DisPack.cpp:371`).
struct Encoder {
    /// One output buffer per stream. `DataBuffer` in the C, which grows by
    /// doubling; a `Vec` does the same thing without the realloc bookkeeping.
    buf: Vec<Vec<u8>>,
    func_table: [u32; 256],
    next_is_func: bool,
    code_start: u32,
    code_end: u32,
}

impl Encoder {
    fn new(code_start: u32, code_end: u32) -> Self {
        Encoder {
            buf: vec![Vec::new(); ST_MAX],
            func_table: [0u32; 256],
            // The first instruction of a block starts a function.
            next_is_func: true,
            code_start,
            code_end,
        }
    }

    #[inline]
    fn put8(&mut self, stream: usize, v: u8) {
        self.buf[stream].push(v);
    }
    /// Big-endian, unlike the little-endian read that produced `v`.
    #[inline]
    fn put16(&mut self, stream: usize, v: u16) {
        self.buf[stream].extend_from_slice(&v.to_be_bytes());
    }
    #[inline]
    fn put32(&mut self, stream: usize, v: u32) {
        self.buf[stream].extend_from_slice(&v.to_be_bytes());
    }

    /// `DetectJumpTable` (`DisPack.cpp:396`) -- count leading dwords that look
    /// like in-range code addresses.
    ///
    /// Fewer than three in a row is treated as coincidence, not a table; that
    /// threshold is format, since the decoder trusts the `JUMPTAB` marker.
    fn detect_jump_table(&self, instr: &[u8], addr: u32) -> usize {
        if addr >= self.code_end {
            return 0;
        }
        let n_max = ((self.code_end - addr) / 4) as usize;
        let mut count = 0usize;
        while count < n_max {
            let off = count * 4;
            let Some(w) = instr.get(off..off + 4) else { break };
            let coded = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
            if coded >= self.code_start && coded < self.code_end {
                count += 1;
            } else {
                break;
            }
        }
        if count < 3 {
            0
        } else {
            count
        }
    }

    /// Emit one call target: an MTF index when the function is known, otherwise
    /// index 0 plus the absolute address.
    fn put_call_target(&mut self, target: u32) {
        match find_mtf(&mut self.func_table, target) {
            Some(ind) => self.put8(ST_CALL_IDX, (ind + 1) as u8),
            None => {
                self.put8(ST_CALL_IDX, 0);
                self.put32(ST_CALL32, target);
            }
        }
    }

    /// `ProcessInstr` (`DisPack.cpp:418`) -- encode one instruction (or one
    /// jump-table run) and return the number of input bytes consumed.
    ///
    /// `instr` is guaranteed to hold at least `MAXINSTR` bytes: the driver's
    /// main loop stops that far from the end, and its tail loop pads.
    fn process_instr(&mut self, instr: &[u8], memory: u32) -> usize {
        let n_jump = self.detect_jump_table(instr, memory);
        if n_jump != 0 {
            // A jump/vtable run, emitted in chunks of at most 256 because the
            // count is stored in one byte as count-1.
            let mut remaining = n_jump;
            let mut p = 0usize;
            while remaining != 0 {
                let count = remaining.min(256);
                self.put8(ST_OP, JUMPTAB);
                self.put8(ST_JUMPTBL_COUNT, (count - 1) as u8);
                for _ in 0..count {
                    let w = &instr[p..p + 4];
                    let target = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                    p += 4;
                    self.put_call_target(target);
                }
                remaining -= count;
            }
            return n_jump * 4;
        }

        let mut p = 0usize;
        let mut code = instr[p];
        p += 1;
        let mut code2 = 0u8;
        let mut o16 = false;

        // A function begins after the previous one returned. int3 is padding
        // between functions, so it does not count as the entry point.
        if self.next_is_func && code != OP_INT3 {
            add_mtf(&mut self.func_table, memory);
            self.next_is_func = false;
        }

        if code == OP_OSIZE {
            o16 = true;
            code = instr[p];
            p += 1;
        }

        let mut flags = if code == OP_2BYTE {
            code2 = instr[p];
            p += 1;
            TABLE2[code2 as usize]
        } else {
            TABLE1[code as usize]
        };

        if code == OP_RETNI || code == OP_RETN || code == OP_INT3 {
            self.next_is_func = true;
        }

        // Opcodes whose operand shape lives in the ModR/M reg field.
        if flags == F_MEXTRA {
            let m = instr[p];
            flags = TABLEX[(((m >> 3) & 7) | ((code & 0x01) << 3) | ((code & 0x08) << 1)) as usize];
        }

        if flags == F_ERR {
            // Not decodable: escape the single byte and resynchronise.
            self.put8(ST_OP, ESCAPE);
            self.put8(ST_OP, instr[0]);
            return 1;
        }

        if o16 {
            self.put8(ST_OP, OP_OSIZE);
        }
        self.put8(ST_OP, code);
        if code == OP_2BYTE {
            self.put8(ST_OP2, code2);
        }

        // Far call/jump carry a 48-bit address: the segment word is copied
        // here and the 32-bit offset falls out of the normal flow below.
        // `enter` likewise has a word operand followed by a byte operand.
        if code == OP_CALLF || code == OP_JMPF || code == OP_ENTER {
            let v = u16::from_le_bytes([instr[p], instr[p + 1]]);
            p += 2;
            self.put16(ST_IMM16, v);
        }

        if flags & F_MODE == F_MR {
            let modrm = instr[p];
            p += 1;
            self.put8(ST_MODRM, modrm);
            let mut sib = 0u8;

            if modrm & 0x07 == 4 && modrm < 0xc0 {
                sib = instr[p];
                p += 1;
                self.put8(ST_SIB, sib);
            }

            if modrm & 0xc0 == 0x40 {
                // register + byte displacement: one stream per base register,
                // which is what makes these bytes compressible.
                let v = instr[p];
                p += 1;
                self.put8(ST_DISP8_R0 + (modrm & 0x07) as usize, v);
            }

            if modrm & 0xc0 == 0x80 || modrm & 0xc7 == 0x05 || (modrm < 0x40 && sib & 0x07 == 5) {
                let w = &instr[p..p + 4];
                let v = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                p += 4;
                let stream = if modrm & 0xc7 == 0x05 { ST_ADDR32 } else { ST_DISP32 };
                self.put32(stream, v);
            }
        }

        if flags & F_MODE == F_AM {
            match flags & F_TYPE {
                F_AD => {
                    let w = &instr[p..p + 4];
                    let v = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                    p += 4;
                    self.put32(ST_ADDR32, v);
                }
                F_DA => {
                    let w = &instr[p..p + 4];
                    let v = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                    p += 4;
                    self.put32(ST_AJUMP32, v);
                }
                F_BR => {
                    let v = instr[p];
                    p += 1;
                    self.put8(ST_JUMP8, v);
                }
                // Named, not `_`: this arm carries F_DR's real logic, and as a
                // catch-all it would have silently applied that logic to any
                // unexpected value. It is correct only because F_DR is the
                // fourth of four; say which case it is.
                F_DR => {
                    // Relative dword target -> absolute.
                    let w = &instr[p..p + 4];
                    let disp = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                    p += 4;
                    // `p` is now the full instruction length, so this is
                    // "displacement + address of the next instruction".
                    let target = disp.wrapping_add(p as u32).wrapping_add(memory);
                    if code != OP_CALLN {
                        self.put32(ST_JUMP32, target);
                    } else {
                        self.put_call_target(target);
                    }
                }
                // F_TYPE is 0xc -- two bits, four values -- and the four arms
                // above are 0x0/0x4/0x8/0xc. The C's switch (DisPack.cpp:516)
                // has no `default` for the same reason.
                _ => unreachable!("flags & F_TYPE outside the four-value mask"),
            }
        } else {
            match flags & F_TYPE {
                F_BI => {
                    let v = instr[p];
                    p += 1;
                    self.put8(ST_IMM8, v);
                }
                F_WI => {
                    let v = u16::from_le_bytes([instr[p], instr[p + 1]]);
                    p += 2;
                    self.put16(ST_IMM16, v);
                }
                F_DI => {
                    if !o16 {
                        let w = &instr[p..p + 4];
                        let v = u32::from_le_bytes([w[0], w[1], w[2], w[3]]);
                        p += 4;
                        self.put32(ST_IMM32, v);
                    } else {
                        let v = u16::from_le_bytes([instr[p], instr[p + 1]]);
                        p += 2;
                        self.put16(ST_IMM16, v);
                    }
                }
                // F_NI, "no immediate". The C's switch (DisPack.cpp:541) lists
                // only fBI/fWI/fDI and lets fNI fall through, so a no-op is the
                // correct port -- named so it is distinguishable from the
                // impossible fourth value.
                F_NI => {}
                _ => unreachable!("flags & F_TYPE outside the four-value mask"),
            }
        }

        p
    }

    /// `Flush` (`DisPack.cpp:565`): `ST_MAX` little-endian sizes, then the
    /// stream bodies back to back.
    fn flush(self) -> Vec<u8> {
        let total: usize = ST_MAX * 4 + self.buf.iter().map(|b| b.len()).sum::<usize>();
        let mut out = Vec::with_capacity(total);
        for b in &self.buf {
            // Little-endian here, unlike the payload -- see the module header.
            out.extend_from_slice(&(b.len() as u32).to_le_bytes());
        }
        for b in &self.buf {
            out.extend_from_slice(b);
        }
        // assert_eq!, not debug_assert_eq!: this runs once per block, so it is
        // affordable in release, and a wrong output length means a malformed
        // stream. The difftests build --release, where a debug_assert would not
        // have been checked at all.
        assert_eq!(out.len(), total, "DisPack encode produced the wrong length");
        out
    }
}

/// `EXETYPE` (`C_DisPack.cpp:149`). Only the EXE/DATA distinction is used.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExeType {
    Data,
    Exe,
}

/// `detect` (`C_DisPack.cpp:151`) -- decide whether a chunk is x86 code.
///
/// This is the gate in front of the whole filter: a chunk classified `Data` is
/// stored verbatim and [`dis_filter`] never sees it. That makes these three
/// ratios format-relevant, not a heuristic detail -- they decide which chunks
/// get a `TAG_EXE` header, so changing them changes the archive.
///
/// The signal is the density of `E8` (near call) bytes and what follows their
/// 4-byte displacement: a high byte of `0xFF` means a backward call, typical of
/// linked executables, while `0x00` means a forward call into a relocation
/// placeholder, typical of object files. Both count toward "this disassembles";
/// only the executable form is separately required to appear at all.
///
/// The thresholds are transcribed, not derived:
/// * at least 0.2% of bytes start an `E8`
/// * at least 20% of those look like a call (exe or obj form)
/// * at least 1% look specifically like the executable form
///
/// Division is `double` in the C. It is kept as `f64` here rather than
/// rearranged into integer comparisons: the multiplied-out forms are equivalent
/// in exact arithmetic but not necessarily at the rounding boundary, and this
/// decides archive bytes.
pub fn detect(buf: &[u8]) -> ExeType {
    let (mut e8, mut exe, mut obj) = (0i64, 0i64, 0i64);
    let len = buf.len();
    // `for (p = buf; p+5 < buf+len; p++)` -- reads p[4] and p[5], so the last
    // start position is len-6.
    let mut i = 0usize;
    while i + 5 < len {
        if buf[i] == 0xE8 {
            e8 += 1;
            if buf[i + 4] == 0xFF && buf[i + 5] != 0xFF {
                exe += 1;
            }
            if buf[i + 4] == 0x00 && buf[i + 5] != 0x00 {
                obj += 1;
            }
        }
        i += 1;
    }
    // The C divides by `len` and by `e8` without guarding either. len==0 gives
    // 0/0 = NaN and every comparison false -> Data, which is what returning
    // early reproduces; e8==0 gives the same via the first test failing.
    if len == 0 || e8 == 0 {
        return ExeType::Data;
    }
    let dense = e8 as f64 / len as f64 >= 0.002;
    let callish = (exe + obj) as f64 / e8 as f64 >= 0.20;
    let executable = exe as f64 / e8 as f64 >= 0.01;
    if dense && callish && executable {
        ExeType::Exe
    } else {
        ExeType::Data
    }
}

/// `DisFilter` (`DisPack.cpp:600`) -- filter one block of x86 code.
///
/// `origin` is the address the block would be loaded at; call/jump targets are
/// made absolute relative to it, so the same bytes at a different origin
/// produce a different (and equally valid) filtered stream.
pub fn dis_filter(src: &[u8], origin: u32) -> Vec<u8> {
    let size = src.len();
    let mut ctx = Encoder::new(origin, origin.wrapping_add(size as u32));
    let mut pos = 0usize;

    // Main loop: stay MAXINSTR bytes clear of the end so every read is in
    // range. Signed comparison, because `size - MAXINSTR` is negative for a
    // short block and must skip the loop rather than wrap.
    while (pos as isize) < size as isize - MAXINSTR as isize {
        let bytes = ctx.process_instr(&src[pos..], origin.wrapping_add(pos as u32));
        if bytes == 0 {
            break; // cannot happen -- every path consumes >= 1 -- but never spin
        }
        pos += bytes;
    }

    // Tail: an instruction here could run past the end of the input, so encode
    // into a zero-padded copy, and if it turns out to have consumed more than
    // is really there, roll every stream back and stop.
    while pos < size {
        let mut instr_buf = [0u8; MAXINSTR];
        let n = size - pos;
        instr_buf[..n].copy_from_slice(&src[pos..]);

        let checkpoint: Vec<usize> = ctx.buf.iter().map(|b| b.len()).collect();
        let bytes = ctx.process_instr(&instr_buf, origin.wrapping_add(pos as u32));

        if bytes != 0 && pos + bytes <= size {
            pos += bytes;
        } else {
            for (b, &mark) in ctx.buf.iter_mut().zip(checkpoint.iter()) {
                b.truncate(mark);
            }
            break;
        }
    }

    // Whatever is left cannot be a whole instruction: escape it byte by byte.
    while pos < size {
        ctx.put8(ST_OP, ESCAPE);
        ctx.put8(ST_OP, src[pos]);
        pos += 1;
    }

    ctx.flush()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The header is `ST_MAX` little-endian sizes covering the rest exactly.
    #[test]
    fn header_describes_the_body() {
        let out = dis_filter(&[0x90u8; 64], 0x401000);
        assert!(out.len() >= ST_MAX * 4);
        let mut sum = 0usize;
        for i in 0..ST_MAX {
            let b = &out[i * 4..i * 4 + 4];
            sum += u32::from_le_bytes([b[0], b[1], b[2], b[3]]) as usize;
        }
        assert_eq!(ST_MAX * 4 + sum, out.len());
    }

    /// A run of nops is all one-byte opcodes: they land in ST_OP and nowhere
    /// else, which is the simplest check that streams are being separated.
    #[test]
    fn nops_go_only_to_the_opcode_stream() {
        let out = dis_filter(&[0x90u8; 64], 0x401000);
        let sizes: Vec<u32> = (0..ST_MAX)
            .map(|i| {
                let b = &out[i * 4..i * 4 + 4];
                u32::from_le_bytes([b[0], b[1], b[2], b[3]])
            })
            .collect();
        assert!(sizes[ST_OP] > 0, "opcode stream is empty");
        for (i, &s) in sizes.iter().enumerate() {
            if i != ST_OP {
                assert_eq!(s, 0, "stream {i} should be empty for a nop run");
            }
        }
    }

    /// Every input length must terminate and be accounted for, including the
    /// ones shorter than MAXINSTR that skip the main loop entirely.
    #[test]
    fn every_length_terminates() {
        for len in 0..64usize {
            let src: Vec<u8> = (0..len).map(|i| (i * 7 % 251) as u8).collect();
            let out = dis_filter(&src, 0x401000);
            assert!(out.len() >= ST_MAX * 4, "len {len} produced no header");
        }
    }

    /// find_mtf searches 255 entries, not 256: the final slot is write-only.
    /// A hit must move to the front so the next lookup is cheaper.
    #[test]
    fn find_mtf_moves_to_front_and_misses_insert() {
        let mut t = [0u32; 256];
        assert_eq!(find_mtf(&mut t, 0xdead), None); // absent -> inserted
        assert_eq!(t[0], 0xdead);
        assert_eq!(find_mtf(&mut t, 0xdead), Some(0)); // now found at the front
        assert_eq!(find_mtf(&mut t, 0xbeef), None);
        assert_eq!(t[0], 0xbeef);
        assert_eq!(find_mtf(&mut t, 0xdead), Some(1)); // pushed back by one
        assert_eq!(t[0], 0xdead); // and moved to the front again
    }
}

// ---------------------------------------------------------------------------
// The chunked compress driver -- DISPACK_METHOD::compress (`C_DisPack.cpp:170`)
// ---------------------------------------------------------------------------

/// `TAG_DATA` / `TAG_EXE` (`C_DisPack.cpp:77`). The stream is a sequence of
/// tagged chunks; TAG_EXE carries a filtered block, TAG_DATA a verbatim one.
const TAG_DATA: u32 = 0xC71B_3AE1;
const TAG_EXE: u32 = TAG_DATA + 1;

/// `is_tag` (`C_DisPack.cpp:78`) -- the tag values occupy one aligned run of
/// 16, so a single masked compare covers both.
#[inline]
fn is_tag(x: u32) -> bool {
    (x ^ TAG_DATA) < 0x10
}

/// `CHUNK_SIZE` (`C_DisPack.cpp:176`) -- the granularity `detect` runs at, and
/// the value written into the stream header for the decoder.
const CHUNK_SIZE: usize = 16 * 1024;

/// Compress driver, mirroring `DISPACK_METHOD::compress`.
///
/// Reads `CHUNK_SIZE` at a time and accumulates consecutive chunks that
/// [`detect`] calls executable into one block, which is then filtered as a
/// unit; the first chunk that is not executable ends the block and is emitted
/// verbatim. That is why `detect` runs per 16 KB chunk rather than per block:
/// it is looking for the boundary between code and data in a mixed file.
///
/// `BaseAddress` is the synthetic load address handed to the filter. It starts
/// at 1 GB and advances by the bytes consumed, wrapping down by 2 GB once it
/// reaches 3 GB so it never approaches the point where a 32-bit target
/// computation would behave differently. It is part of the format: the decoder
/// walks the identical sequence.
pub fn compress(io: &Io, block_size: u32) -> c_int {
    let block_size = block_size.max(CHUNK_SIZE as u32) as usize;
    let mut inbuf = vec![0u8; block_size + 2];
    let mut base_address: u32 = 1 << 30;
    let mut first_time = true;

    loop {
        // Accumulate executable chunks into one block.
        let mut filled = 0usize; // bytes of confirmed code, at inbuf[..filled]
        let mut tail = 0usize; // a trailing non-code chunk at inbuf[filled..]
        loop {
            let want = CHUNK_SIZE.min(inbuf.len() - filled);
            if want == 0 {
                break;
            }
            let got = io.read(&mut inbuf[filled..filled + want]);
            if got < 0 {
                return got;
            }
            if got == 0 {
                break;
            }
            let n = got as usize;
            if detect(&inbuf[filled..filled + n]) != ExeType::Exe {
                tail = n; // not code: ends the block, emitted verbatim below
                break;
            }
            filled += n;
            // `while (p-In <= BlockSize-CHUNK_SIZE)` -- stop before a further
            // full chunk could overrun the buffer.
            if filled > block_size.saturating_sub(CHUNK_SIZE) {
                break;
            }
        }

        if filled + tail == 0 {
            return OK; // input exhausted
        }

        if first_time {
            if io.write(&(CHUNK_SIZE as u32).to_le_bytes()) < 0 {
                return FREEARC_ERRCODE_IO;
            }
            first_time = false;
        }

        if filled != 0 {
            let out = dis_filter(&inbuf[..filled], base_address);
            if io.write(&TAG_EXE.to_le_bytes()) < 0
                || io.write(&(filled as u32).to_le_bytes()) < 0
                || io.write(&(out.len() as u32).to_le_bytes()) < 0
                || io.write(&out) < 0
            {
                return FREEARC_ERRCODE_IO;
            }
        }

        if tail != 0 {
            let data = &inbuf[filled..filled + tail];
            // A full chunk whose first word cannot be mistaken for a tag needs
            // no header at all -- the decoder infers it. Anything else must be
            // tagged explicitly, or the decoder would misread the data as a
            // chunk header.
            let first_word = if data.len() >= 4 {
                u32::from_le_bytes([data[0], data[1], data[2], data[3]])
            } else {
                0
            };
            if tail != CHUNK_SIZE || is_tag(first_word) {
                if io.write(&TAG_DATA.to_le_bytes()) < 0
                    || io.write(&(tail as u32).to_le_bytes()) < 0
                {
                    return FREEARC_ERRCODE_IO;
                }
            }
            if io.write(data) < 0 {
                return FREEARC_ERRCODE_IO;
            }
        }

        base_address = base_address.wrapping_add((filled + tail) as u32);
        if base_address >= 3u32 << 30 {
            base_address -= 2u32 << 30;
        }
    }
}
