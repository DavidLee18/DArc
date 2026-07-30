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
/// `kFix4HashSize` — offset of the 4-byte hash region (BT4/HC4).
pub const FIX4_HASH_SIZE: usize = HASH2_SIZE + HASH3_SIZE;
/// `kFix5HashSize` — offset of the 5-byte hash region (BT5/HC5).
///
/// **Equal to [`FIX4_HASH_SIZE`], not larger.** `LzHash.h:18` defines it as
/// `kHash2Size + kHash3Size + kHash4Size`, but that line is commented out and
/// `LzFind.c:29` redefines it as `kFix4HashSize` — this SDK vintage has no 4-byte
/// table (`h4` is commented out in `HASH5_CALC` too). Taking the header's version
/// puts every 5-byte index `1 << 20` off.
pub const FIX5_HASH_SIZE: usize = FIX4_HASH_SIZE;

/// `kLzHash_CrcShift_1`.
pub const CRC_SHIFT_1: u32 = 5;
/// `kLzHash_CrcShift_2` — used only by the 5-byte hash.
pub const CRC_SHIFT_2: u32 = 10;

// Compile-time enforcement of the "Required" invariants documented in LzHash.h.
const _: () = assert!(HASH2_SIZE >= 1 << 8);
const _: () = assert!(HASH3_SIZE >= 1 << 16);
const _: () = assert!(FIX4_HASH_SIZE == HASH2_SIZE + HASH3_SIZE);

// L2: the CRC table (`p->crc`, built in `LzFind.c`) and the 2/3/4-byte hash
// functions go here, feeding `bt4::get_matches`.
