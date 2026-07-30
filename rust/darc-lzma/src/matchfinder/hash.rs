//! Rolling hash for the match finder — a port of `LzHash.h` and the hash
//! computation in `LzFind.c`. Constants are copied verbatim from `LzHash.h`.
//!
//! BT4 uses 2-, 3-, and 4-byte hashes built from a CRC table:
//! `crc0`, `crc1 << Shift_1`, `crc2 << Shift_2`.

/// `kHash2Size` — must be `>= 1 << 8`.
pub const HASH2_SIZE: usize = 1 << 10;
/// `kHash3Size` — must be `>= 1 << 16`.
pub const HASH3_SIZE: usize = 1 << 16;

/// `kFix3HashSize` — offset of the 3-byte hash region.
pub const FIX3_HASH_SIZE: usize = HASH2_SIZE;
/// `kFix4HashSize` — offset of the 4-byte hash region (BT4).
pub const FIX4_HASH_SIZE: usize = HASH2_SIZE + HASH3_SIZE;

/// `kLzHash_CrcShift_1`.
pub const CRC_SHIFT_1: u32 = 5;

// Compile-time enforcement of the "Required" invariants documented in LzHash.h.
const _: () = assert!(HASH2_SIZE >= 1 << 8);
const _: () = assert!(HASH3_SIZE >= 1 << 16);
const _: () = assert!(FIX4_HASH_SIZE == HASH2_SIZE + HASH3_SIZE);

// L2: the CRC table (`p->crc`, built in `LzFind.c`) and the 2/3/4-byte hash
// functions go here, feeding `bt4::get_matches`.
