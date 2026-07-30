//! The inverse x86 filter, ported from `Compression/DisPack/DisPack.cpp`
//! (`DisUnFilter` :674, plus the `Copy*`/`Check*` helpers at :660-672).
//!
//! The encoder split one instruction stream into `ST_MAX` parallel byte
//! streams and rewrote relative call/jump targets as absolute, big-endian. This
//! walks the opcode stream and, for each instruction, pulls its remaining bytes
//! from the stream each belongs to -- reassembling the original code and
//! turning the absolute targets back into relative ones.
//!
//! ## Byte order is a trap
//!
//! Multi-byte fields were stored big-endian in the streams but belong
//! little-endian in x86 code, so `copy16`/`copy32` **fetch big-endian, write
//! little-endian**; `copy8` is a plain byte. Getting this backwards corrupts
//! every immediate and displacement while still "working" structurally.
//!
//! ## Every read is bounded
//!
//! The C guards each stream read against that stream's end and each write
//! against the output end (`CheckSrc`/`CheckDst`), returning false -- a bad
//! block -- on any overrun. This runs on `arc t` over attacker-supplied data,
//! so those bounds are reproduced exactly; a violation is `None`.

use super::tables::*;

/// The `ST_MAX` input streams and a write cursor over the output. Positions are
/// indices, so an overrun is a checked comparison rather than a raw pointer
/// walking off a buffer.
struct Streams<'a> {
    /// `source` split into `ST_MAX` slices; `pos[i]` is stream i's cursor.
    data: &'a [u8],
    start: [usize; ST_MAX],
    end: [usize; ST_MAX],
    pos: [usize; ST_MAX],
    out: Vec<u8>,
}

impl<'a> Streams<'a> {
    /// Read the `ST_MAX` stream sizes from the header and carve the body.
    /// The header is `ST_MAX` big-endian... no -- little-endian (`Fetch32`,
    /// not `Fetch32B`) u32 sizes, then the streams back to back.
    fn parse(source: &'a [u8], out_cap: usize) -> Option<Streams<'a>> {
        if source.len() < ST_MAX * 4 {
            return None;
        }
        let mut start = [0usize; ST_MAX];
        let mut end = [0usize; ST_MAX];
        let mut cur = ST_MAX * 4;
        for i in 0..ST_MAX {
            let sz = u32::from_le_bytes([
                source[i * 4],
                source[i * 4 + 1],
                source[i * 4 + 2],
                source[i * 4 + 3],
            ]) as usize;
            start[i] = cur;
            cur = cur.checked_add(sz)?;
            end[i] = cur;
            if cur > source.len() {
                return None;
            }
        }
        // The stream sizes must account for exactly the whole body.
        if cur != source.len() {
            return None;
        }
        Some(Streams {
            data: source,
            start,
            end,
            pos: start,
            out: Vec::with_capacity(out_cap),
        })
    }

    #[inline]
    fn src_avail(&self, s: usize, n: usize) -> bool {
        self.pos[s] + n <= self.end[s]
    }

    #[inline]
    fn dst_ok(&self, cap: usize, n: usize) -> bool {
        self.out.len() + n <= cap
    }

    /// `Fetch8` on stream `s`. Caller must have checked availability.
    #[inline]
    fn fetch8(&mut self, s: usize) -> u8 {
        let b = self.data[self.pos[s]];
        self.pos[s] += 1;
        b
    }

    #[inline]
    fn fetch32b(&mut self, s: usize) -> u32 {
        let p = self.pos[s];
        let v = u32::from_be_bytes([self.data[p], self.data[p + 1], self.data[p + 2], self.data[p + 3]]);
        self.pos[s] += 4;
        v
    }

    /// `Copy8`: one byte, stream to output, returned so the opcode flow can key
    /// on it.
    #[inline]
    fn copy8(&mut self, s: usize) -> u8 {
        let v = self.fetch8(s);
        self.out.push(v);
        v
    }

    /// `Copy16`: fetch big-endian, write little-endian.
    #[inline]
    fn copy16(&mut self, s: usize) {
        let p = self.pos[s];
        let v = u16::from_be_bytes([self.data[p], self.data[p + 1]]);
        self.pos[s] += 2;
        self.out.extend_from_slice(&v.to_le_bytes());
    }

    /// `Copy32`: fetch big-endian, write little-endian.
    #[inline]
    fn copy32(&mut self, s: usize) {
        let v = self.fetch32b(s);
        self.out.extend_from_slice(&v.to_le_bytes());
    }
}

/// `DisUnFilter`. Reconstructs the instruction stream from `source` into a
/// `dest` of exactly `dest_size` bytes, with `mem_start` the block's base
/// address. Returns the output on success, `None` on any bounds violation --
/// the C's `sFALSE`.
pub fn dis_unfilter(source: &[u8], dest_size: usize, mem_start: u32) -> Option<Vec<u8>> {
    let mut st = Streams::parse(source, dest_size)?;
    let cap = dest_size;

    let mut func_table = [0u32; 256];
    let mut next_is_func = true;

    // These macro-equivalents return None (the C's `return sFALSE`) on overrun.
    macro_rules! check_src {
        ($s:expr, $n:expr) => {
            if !st.src_avail($s, $n) {
                return None;
            }
        };
    }
    macro_rules! copy8_chk {
        ($s:expr) => {{
            if !st.src_avail($s, 1) || !st.dst_ok(cap, 1) {
                return None;
            }
            st.copy8($s)
        }};
    }
    macro_rules! copy16_chk {
        ($s:expr) => {{
            if !st.src_avail($s, 2) || !st.dst_ok(cap, 2) {
                return None;
            }
            st.copy16($s);
        }};
    }
    macro_rules! copy32_chk {
        ($s:expr) => {{
            if !st.src_avail($s, 4) || !st.dst_ok(cap, 4) {
                return None;
            }
            st.copy32($s);
        }};
    }

    while st.pos[ST_OP] < st.end[ST_OP] {
        let start_len = st.out.len(); // dest - start, in the C
        let memory = mem_start.wrapping_add((st.out.len()) as u32);

        let mut code = st.fetch8(ST_OP) as u32;

        if code as u8 == JUMPTAB {
            // A jump/vtable run: a count, then that many targets, each either a
            // move-to-front index or a fresh big-endian address.
            check_src!(ST_JUMPTBL_COUNT, 1);
            let count = st.fetch8(ST_JUMPTBL_COUNT) as usize + 1;
            for _ in 0..count {
                check_src!(ST_CALL_IDX, 1);
                let ind = st.fetch8(ST_CALL_IDX) as usize;
                let target = if ind != 0 {
                    let v = func_table[ind - 1];
                    move_to_front(&mut func_table, ind - 1, v)
                } else {
                    check_src!(ST_CALL32, 4);
                    let t = st.fetch32b(ST_CALL32);
                    add_mtf(&mut func_table, t);
                    t
                };
                if !st.dst_ok(cap, 4) {
                    return None;
                }
                st.out.extend_from_slice(&target.to_le_bytes());
            }
            continue;
        }

        // A RET/INT3 seen earlier flagged the next opcode as a likely function
        // start; record its address in the MTF table before decoding it.
        if next_is_func && code as u8 != OP_INT3 {
            add_mtf(&mut func_table, memory);
            next_is_func = false;
        }

        if code as u8 == ESCAPE {
            // A byte that would not disassemble, passed through verbatim.
            copy8_chk!(ST_OP);
            continue;
        }

        // Emit the opcode byte itself.
        if !st.dst_ok(cap, 1) {
            return None;
        }
        st.out.push(code as u8);

        let mut o16 = false;
        if code as u8 == OP_OSIZE {
            // Operand-size prefix: the real opcode follows in the same stream.
            o16 = true;
            if !st.src_avail(ST_OP, 1) || !st.dst_ok(cap, 1) {
                return None;
            }
            code = st.copy8(ST_OP) as u32;
        }

        if code as u8 == OP_RETNI || code as u8 == OP_RETN || code as u8 == OP_INT3 {
            next_is_func = true;
        }

        let flags = if code as u8 == OP_2BYTE {
            // Two-byte opcode: the second byte selects the format.
            if !st.src_avail(ST_OP2, 1) || !st.dst_ok(cap, 1) {
                return None;
            }
            let op2 = st.copy8(ST_OP2);
            TABLE2[op2 as usize]
        } else {
            TABLE1[code as usize]
        };

        // fERR should be unreachable in a well-formed stream; the C asserts it.
        // Here a corrupt stream that reaches it is simply rejected.
        if flags == F_ERR {
            return None;
        }

        if code as u8 == OP_CALLF || code as u8 == OP_JMPF || code as u8 == OP_ENTER {
            // Far call/jump carry a 16-bit segment (or enter's word operand);
            // coded here, the 32-bit part handled by the normal flow below.
            copy16_chk!(ST_IMM16);
        }

        // "Has a ModR/M byte" is `flags & fMR` (0x2): true for fMR (0x2) and
        // fMEXTRA (0x3), false for fNM (0x0) and fAM (0x1).
        let mut flags = flags;
        if flags & F_MR != 0 {
            if !st.src_avail(ST_MODRM, 1) || !st.dst_ok(cap, 1) {
                return None;
            }
            let modrm = st.copy8(ST_MODRM);

            if flags == F_MEXTRA {
                let idx = ((modrm as usize >> 3) & 7)
                    | ((code as usize & 0x01) << 3)
                    | ((code as usize & 0x08) << 1);
                flags = TABLEX[idx];
                if flags == F_ERR {
                    return None;
                }
            }

            let mut sib = 0u8;
            if (modrm & 0x07) == 4 && modrm < 0xc0 {
                if !st.src_avail(ST_SIB, 1) || !st.dst_ok(cap, 1) {
                    return None;
                }
                sib = st.copy8(ST_SIB);
            }

            if (modrm & 0xc0) == 0x40 {
                // register + byte displacement, one stream per register.
                let s = (modrm as usize & 0x07) + ST_DISP8_R0;
                copy8_chk!(s);
            }

            if (modrm & 0xc0) == 0x80
                || (modrm & 0xc7) == 0x05
                || (modrm < 0x40 && (sib & 0x07) == 0x05)
            {
                let s = if (modrm & 0xc7) == 5 { ST_ADDR32 } else { ST_DISP32 };
                copy32_chk!(s);
            }
        }

        if (flags & F_MODE) == F_AM {
            match flags & F_TYPE {
                F_AD => copy32_chk!(ST_ADDR32),
                F_DA => copy32_chk!(ST_AJUMP32),
                F_BR => {
                    copy8_chk!(ST_JUMP8);
                }
                F_DR => {
                    // Relative dword target: recover it from the absolute value.
                    let target = if code as u8 == OP_CALLN {
                        check_src!(ST_CALL_IDX, 1);
                        let ind = st.fetch8(ST_CALL_IDX) as usize;
                        if ind != 0 {
                            let v = func_table[ind - 1];
                            move_to_front(&mut func_table, ind - 1, v)
                        } else {
                            check_src!(ST_CALL32, 4);
                            let t = st.fetch32b(ST_CALL32);
                            add_mtf(&mut func_table, t);
                            t
                        }
                    } else {
                        check_src!(ST_JUMP32, 4);
                        st.fetch32b(ST_JUMP32)
                    };
                    // relative = absolute - (end-of-instruction address).
                    let rel = target
                        .wrapping_sub((st.out.len() - start_len) as u32)
                        .wrapping_sub(4)
                        .wrapping_sub(memory);
                    if !st.dst_ok(cap, 4) {
                        return None;
                    }
                    st.out.extend_from_slice(&rel.to_le_bytes());
                }
                // Dead by arithmetic, not by assumption: F_TYPE is 0xc, two
                // bits, so `flags & F_TYPE` has exactly four possible values,
                // and F_AD/F_DA/F_BR/F_DR are 0x0/0x4/0x8/0xc -- all four. The
                // C's switch (DisPack.cpp:812) has no `default` for the same
                // reason. Only a bare-integer mask forces an arm here at all;
                // modelling F_TYPE as an enum would remove it.
                _ => unreachable!("flags & F_TYPE outside the four-value mask"),
            }
        } else {
            match flags & F_TYPE {
                F_BI => {
                    copy8_chk!(ST_IMM8);
                }
                F_WI => copy16_chk!(ST_IMM16),
                F_DI => {
                    if !o16 {
                        copy32_chk!(ST_IMM32);
                    } else {
                        copy16_chk!(ST_IMM16);
                    }
                }
                // F_NI, "no immediate" -- the common case for most opcodes.
                // Named rather than left as `_`: the C's switch
                // (DisPack.cpp:849) lists only fBI/fWI/fDI and lets fNI fall
                // through, so doing nothing is the correct port, and saying so
                // separates it from the impossible fourth value below.
                F_NI => {}
                _ => unreachable!("flags & F_TYPE outside the four-value mask"),
            }
        }
    }

    // The C returns success without requiring dest to be exactly full; the
    // caller (C_DisPack) knows OutSize and trusts the filter to have produced
    // it. Reproduce that: the driver checks the length.
    Some(st.out)
}
