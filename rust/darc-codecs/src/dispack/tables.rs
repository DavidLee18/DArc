//! The x86 opcode-classification tables and constants for DisPack's
//! disassembler, ported verbatim from `Compression/DisPack/DisPack.cpp`
//! (the `Streams`/`Opcodes`/`InstructionFormat` enums and `Table1`/`Table2`/
//! `TableX` at :168-320).
//!
//! DisPack is an x86 branch/call/jump filter: it splits an instruction stream
//! into parallel byte streams (opcodes here, ModR/M bytes there, call targets
//! somewhere else) and rewrites relative call/jump targets as absolute. To know
//! which byte of an instruction belongs to which stream, the decoder runs a
//! table-driven partial disassembler -- it does not need to *understand* each
//! instruction, only its length structure: does it carry a ModR/M byte, and
//! what size immediate.
//!
//! These three tables are that disassembler's entire knowledge, and they are
//! pure transcribed data: 256 + 256 + 32 hand-entered nibbles. A single wrong
//! entry desynchronises the stream split for every instruction of that opcode
//! and silently corrupts the output -- which is why they are pinned by tests
//! against the structural invariants the C relies on, and ultimately by the
//! differential harness against the C itself.

/// The separate byte streams an instruction is split across (`enum Streams`).
/// The order is the format: stream *i*'s size is the *i*-th word of the block
/// header, so these indices are load-bearing, not arbitrary.
pub const ST_OP: usize = 0; // prefixes, first opcode byte
pub const ST_SIB: usize = 1;
pub const ST_CALL_IDX: usize = 2; // call-table (MTF) index
pub const ST_DISP8_R0: usize = 3; // byte displacements, one stream per reg
pub const ST_JUMP8: usize = 11; // short jump
pub const ST_IMM8: usize = 12;
pub const ST_IMM16: usize = 13;
pub const ST_IMM32: usize = 14;
pub const ST_DISP32: usize = 15;
pub const ST_ADDR32: usize = 16; // direct address
pub const ST_CALL32: usize = 17; // 32-bit call target
pub const ST_JUMP32: usize = 18; // 32-bit jump target
pub const ST_MAX: usize = 19;

// Aliases: streams stored together with another because they correlate well.
pub const ST_MODRM: usize = ST_OP;
pub const ST_OP2: usize = ST_OP;
pub const ST_AJUMP32: usize = ST_JUMP32;
pub const ST_JUMPTBL_COUNT: usize = ST_OP;

/// Opcodes the disassembler special-cases (`enum Opcodes`).
pub const OP_2BYTE: u8 = 0x0f;
pub const OP_OSIZE: u8 = 0x66; // operand-size prefix
pub const OP_CALLF: u8 = 0x9a;
pub const OP_RETNI: u8 = 0xc2;
pub const OP_RETN: u8 = 0xc3;
pub const OP_ENTER: u8 = 0xc8;
pub const OP_INT3: u8 = 0xcc;
pub const OP_INTO: u8 = 0xce;
pub const OP_CALLN: u8 = 0xe8;
pub const OP_JMPF: u8 = 0xea;
pub const OP_ICEBP: u8 = 0xf1;

/// Escape codes, reusing two rare one-byte opcodes with no operands: ESCAPE
/// wraps bytes that would not disassemble, JUMPTAB marks a jump/vtable run.
pub const ESCAPE: u8 = OP_ICEBP;
pub const JUMPTAB: u8 = OP_INTO;

// Instruction-format nibble (`enum InstructionFormat`). The low two bits are
// the encoding mode; the high two are re-used for immediate size (no-ModRM) or
// address type (address mode).
pub const F_NM: u8 = 0x0; // no ModR/M
pub const F_AM: u8 = 0x1; // no ModR/M, address operand (jump / direct address)
pub const F_MR: u8 = 0x2; // ModR/M present
pub const F_MEXTRA: u8 = 0x3; // ModR/M present, opcode extension in reg field
pub const F_MODE: u8 = 0x3; // mode mask

pub const F_NI: u8 = 0x0; // no immediate
pub const F_BI: u8 = 0x4; // byte immediate
pub const F_WI: u8 = 0x8; // word immediate
pub const F_DI: u8 = 0xc; // dword immediate
pub const F_TYPE: u8 = 0xc; // type mask

pub const F_AD: u8 = 0x0; // absolute address
pub const F_DA: u8 = 0x4; // dword absolute jump target
pub const F_BR: u8 = 0x8; // byte relative jump target
pub const F_DR: u8 = 0xc; // dword relative jump target

pub const F_ERR: u8 = 0xf; // invalid opcode

// The nibble combinations, so the tables below read like the C source.
const MR_NI: u8 = F_MR | F_NI;
const MR_BI: u8 = F_MR | F_BI;
const MR_DI: u8 = F_MR | F_DI;
const NM_NI: u8 = F_NM | F_NI;
const NM_BI: u8 = F_NM | F_BI;
const NM_WI: u8 = F_NM | F_WI;
const NM_DI: u8 = F_NM | F_DI;
const AM_BR: u8 = F_AM | F_BR;
const AM_DA: u8 = F_AM | F_DA;
const AM_AD: u8 = F_AM | F_AD;
const AM_DR: u8 = F_AM | F_DR;
const ERR: u8 = F_ERR;
const MEXTRA: u8 = F_MEXTRA;

/// One-byte opcode formats (`Table1`, :263).
#[rustfmt::skip]
pub const TABLE1: [u8; 256] = [
    MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI,MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI, // 0
    MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI,MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI, // 1
    MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI,MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI, // 2
    MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI,MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_DI,NM_NI,NM_NI, // 3
    NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI, // 4
    NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI, // 5
    NM_NI,NM_NI,MR_NI,MR_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_DI,MR_DI,NM_BI,MR_BI,NM_NI,NM_NI,NM_NI,NM_NI, // 6
    AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR,AM_BR, // 7
    MR_BI,MR_DI,MR_BI,MR_BI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 8
    NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,AM_DA,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI, // 9
    AM_AD,AM_AD,AM_AD,AM_AD,NM_NI,NM_NI,NM_NI,NM_NI,NM_BI,NM_DI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI, // a
    NM_BI,NM_BI,NM_BI,NM_BI,NM_BI,NM_BI,NM_BI,NM_BI,NM_DI,NM_DI,NM_DI,NM_DI,NM_DI,NM_DI,NM_DI,NM_DI, // b
    MR_BI,MR_BI,NM_WI,NM_NI,MR_NI,MR_NI,MR_BI,MR_DI,NM_BI,NM_NI,NM_WI,NM_NI,NM_NI,NM_BI,ERR,  NM_NI, // c
    MR_NI,MR_NI,MR_NI,MR_NI,NM_BI,NM_BI,NM_NI,NM_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // d
    AM_BR,AM_BR,AM_BR,AM_BR,NM_BI,NM_BI,NM_BI,NM_BI,AM_DR,AM_DR,AM_AD,AM_BR,NM_NI,NM_NI,NM_NI,NM_NI, // e
    NM_NI,ERR,  NM_NI,NM_NI,NM_NI,NM_NI,MEXTRA,MEXTRA,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,MEXTRA,MEXTRA, // f
];

/// Two-byte opcode formats (`Table2`, :299), indexed by the byte after `0x0f`.
#[rustfmt::skip]
pub const TABLE2: [u8; 256] = [
    ERR,  ERR,  ERR,  ERR,  ERR,  ERR,  NM_NI,ERR,  NM_NI,NM_NI,ERR,  ERR,  ERR,  ERR,  ERR,  ERR,   // 0
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,ERR,  ERR,  ERR,  ERR,  ERR,  ERR,  ERR,   // 1
    MR_NI,MR_NI,MR_NI,MR_NI,ERR,  ERR,  ERR,  ERR,  MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 2
    NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,ERR,  NM_NI,ERR,  ERR,  ERR,  ERR,  ERR,  ERR,  ERR,  ERR,   // 3
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 4
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 5
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 6
    MR_BI,MR_BI,MR_BI,MR_BI,MR_NI,MR_NI,MR_NI,NM_NI,ERR,  ERR,  ERR,  ERR,  ERR,  ERR,  MR_NI,MR_NI, // 7
    AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR,AM_DR, // 8
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 9
    NM_NI,NM_NI,NM_NI,MR_NI,MR_BI,MR_NI,MR_NI,MR_NI,ERR,  ERR,  ERR,  MR_NI,MR_BI,MR_NI,ERR,  MR_NI, // a
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,ERR,  ERR,  ERR,  MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // b
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI,NM_NI, // c
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // d
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // e
    MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,ERR,   // f
];

/// Opcode-extension formats for the ModR/M-reg escapes of 0xf6/0xf7/0xfe/0xff
/// (`TableX`, :318). Indexed by `(reg field) | (opcode&1)<<3 | (opcode&8)<<1`.
#[rustfmt::skip]
pub const TABLEX: [u8; 32] = [
    MR_BI,ERR,  MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 0xf6
    MR_DI,ERR,  MR_NI,MR_NI,MR_NI,MR_NI,MR_NI,MR_NI, // 0xf7
    MR_NI,MR_NI,ERR,  ERR,  ERR,  ERR,  ERR,  ERR,   // 0xfe
    MR_NI,MR_NI,MR_NI,ERR,  MR_NI,ERR,  MR_NI,ERR,   // 0xff
];

/// `MoveToFront` (:232): shift `table[0..=pos]` down and set `table[0] = val`.
pub fn move_to_front(table: &mut [u32; 256], pos: usize, val: u32) -> u32 {
    let mut p = pos;
    while p > 0 {
        table[p] = table[p - 1];
        p -= 1;
    }
    table[0] = val;
    val
}

/// `AddMTF` (:241): push `val` to the front, evicting the last entry.
pub fn add_mtf(mtf: &mut [u32; 256], val: u32) {
    move_to_front(mtf, 255, val);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The stream indices are the block-header word order, so their exact
    /// values and count are format, not implementation detail.
    #[test]
    fn stream_layout_matches_the_format() {
        assert_eq!(ST_MAX, 19);
        // The eight per-register byte-displacement streams are contiguous.
        assert_eq!(ST_DISP8_R0, 3);
        assert_eq!(ST_JUMP8, ST_DISP8_R0 + 8);
        // The aliases fold onto real streams, never past the end.
        for s in [ST_MODRM, ST_OP2, ST_JUMPTBL_COUNT, ST_AJUMP32] {
            assert!(s < ST_MAX);
        }
    }

    /// Every table entry is a valid nibble: a mode in the low two bits and a
    /// type in the high two, or the all-ones error marker. A stray high bit
    /// would mean a mistyped constant.
    #[test]
    fn table_entries_are_well_formed_nibbles() {
        for (name, t) in [("t1", &TABLE1[..]), ("t2", &TABLE2[..]), ("tx", &TABLEX[..])] {
            for (i, &e) in t.iter().enumerate() {
                assert!(e <= 0x0f, "{name}[{i}] = {e:#x} is not a nibble");
            }
        }
        // fMEXTRA marks the four extension-escape opcodes, and appears only in
        // Table1 -- never in the resolved tables. (ERR, 0xf, is excluded: it
        // shares the mode bits but is the invalid-opcode marker, not fMEXTRA.)
        for (i, &e) in TABLE2.iter().enumerate() {
            assert!(e == ERR || e & F_MODE != F_MEXTRA, "t2[{i}] unexpectedly fMEXTRA");
        }
    }

    /// A handful of anchor opcodes checked by hand against the x86 encoding, so
    /// a wholesale row shift would show up somewhere.
    #[test]
    fn known_opcodes_classify_correctly() {
        // 0xE8 CALL rel32 -- address mode, dword relative.
        assert_eq!(TABLE1[OP_CALLN as usize], F_AM | F_DR);
        // 0x0F two-byte escape is itself a no-ModRM, no-immediate one-byte op.
        assert_eq!(TABLE1[OP_2BYTE as usize], F_NM | F_NI);
        // 0x66 operand-size prefix, treated as a bare opcode.
        assert_eq!(TABLE1[OP_OSIZE as usize], F_NM | F_NI);
        // 0x70..0x7f Jcc rel8 -- address mode, byte relative.
        for op in 0x70..=0x7f {
            assert_eq!(TABLE1[op], F_AM | F_BR, "Jcc {op:#x}");
        }
        // 0xF6/0xF7 are the extension escapes.
        assert_eq!(TABLE1[0xf6], F_MEXTRA);
        assert_eq!(TABLE1[0xf7], F_MEXTRA);
        // 0x0F 0x80..0x8f Jcc rel32 -- dword relative.
        for i in 0x80..=0x8f {
            assert_eq!(TABLE2[i], F_AM | F_DR, "0f {i:#x}");
        }
    }

    #[test]
    fn mtf_moves_to_front_and_evicts() {
        let mut t = [0u32; 256];
        add_mtf(&mut t, 10);
        add_mtf(&mut t, 20);
        add_mtf(&mut t, 30);
        assert_eq!(&t[..3], &[30, 20, 10]);
        // Referencing index 2 (value 10) promotes it and shifts the rest down.
        let at2 = t[2];
        let v = move_to_front(&mut t, 2, at2);
        assert_eq!(v, 10);
        assert_eq!(&t[..3], &[10, 30, 20]);
    }
}
