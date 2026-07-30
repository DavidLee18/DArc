//! Encoder properties: a port of `CLzmaEncProps` + `LzmaEncProps_Init` /
//! `LzmaEncProps_Normalize` (`LzmaEnc.c:57-115`) and `LzmaEnc_WriteProperties`
//! (`LzmaEnc.c:3037`).
//!
//! This crate targets only the single-threaded, optimal-parser, BT4 path that
//! MAME's CHD codec uses (`chdcodec.cpp:1310`: `level = 8`, `reduceSize = hunk`),
//! so [`LzmaProps`] exposes just the fields that path needs. `algo` is always 1
//! (optimal parser) and the match finder is always BT4 (`numHashBytes = 4`).

/// Smallest dictionary the encoder will use, even after reduction
/// (`kReduceMin`, `LzmaEnc.c:84`).
const K_REDUCE_MIN: u32 = 1 << 12; // 4096

/// Normalized LZMA encoder properties for the optimal-parser / BT4 path.
///
/// Construct via [`LzmaProps::for_level`] or [`LzmaProps::chd_for_hunk`]; both
/// reproduce `LzmaEncProps_Normalize` exactly. The fields mirror the C
/// `CLzmaEncProps` members of the same name after normalization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LzmaProps {
    /// Literal context bits (`lc`). Level-8 default: 3. Range 0..=8.
    pub lc: u8,
    /// Literal position bits (`lp`). Level-8 default: 0. Range 0..=4.
    pub lp: u8,
    /// Position bits (`pb`). Level-8 default: 2. Range 0..=4.
    pub pb: u8,
    /// Dictionary size in bytes, after `reduceSize` clamping.
    pub dict_size: u32,
    /// Fast bytes (`fb` / `numFastBytes`). Level-8 default: 64.
    pub fb: u32,
    /// Match cycles (`mc` / `cutValue`). Level-8 / BT4 default: 48.
    pub mc: u32,
    /// Emit an end-of-payload marker before the final flush, i.e. the SDK's
    /// `writeEndMark`.
    ///
    /// **DArc always sets this** (`C_LZMA.cpp`: "FreeArc streams with EOPM
    /// (unknown size)"), because its streams do not carry an uncompressed length.
    /// Upstream lzma-sdk-rs had no equivalent and always emitted none, which is
    /// the entire remaining difference between this encoder and DArc's C -- see
    /// PROVENANCE.md for the measurement.
    pub write_end_mark: bool,
}

impl LzmaProps {
    /// Reproduce `LzmaEncProps_Init` defaults for `level` followed by
    /// `LzmaEncProps_Normalize` (`LzmaEnc.c:68`), with `reduceSize` set to
    /// `reduce_size`.
    ///
    /// Pass `reduce_size = u32::MAX` for "no reduction" (matches the C default of
    /// `reduceSize = (UInt64)-1`). `level` is clamped into the SDK's documented
    /// range conceptually; callers use 8 for CHD.
    pub fn for_level(level: u32, reduce_size: u32) -> Self {
        // dictSize default by level (LzmaEnc.c:74-79).
        let mut dict_size = if level <= 3 {
            1u32 << (level * 2 + 16)
        } else if level <= 6 {
            1u32 << (level + 19)
        } else if level <= 7 {
            1u32 << 25
        } else {
            1u32 << 26
        };

        // reduceSize clamp with the kReduceMin floor (LzmaEnc.c:81-89).
        if dict_size > reduce_size {
            let mut v = reduce_size;
            if v < K_REDUCE_MIN {
                v = K_REDUCE_MIN;
            }
            if dict_size > v {
                dict_size = v;
            }
        }

        // lc/lp/pb defaults (LzmaEnc.c:91-93).
        let lc = 3u8;
        let lp = 0u8;
        let pb = 2u8;

        // We only support the optimal parser + BT4. With algo defaulting to
        // `level < 5 ? 0 : 1` and btMode to `algo == 0 ? 0 : 1`, every level >= 5
        // lands on algo=1 / btMode=1 / numHashBytes=4 (LzmaEnc.c:95-98).
        let fb = if level < 7 { 32u32 } else { 64u32 };
        // mc = (16 + (fb >> 1)) >> (btMode ? 0 : 1); btMode == 1 here
        // (LzmaEnc.c:99).
        let mc = 16 + (fb >> 1);

        LzmaProps {
            lc,
            lp,
            pb,
            dict_size,
            fb,
            mc,
            // Upstream's default and the SDK's: `p->writeEndMark = 0`
            // (LzmaEnc.c:64). DArc's callers set it explicitly.
            write_end_mark: false,
        }
    }

    /// The exact properties MAME's CHD codec uses for a hunk of `hunk_bytes`
    /// bytes: level 8 with the dictionary reduced to the hunk size
    /// (`chdcodec.cpp:1310`).
    pub fn chd_for_hunk(hunk_bytes: u32) -> Self {
        Self::for_level(8, hunk_bytes)
    }

    /// The 5 decoder property bytes, identical to `LzmaEnc_WriteProperties`
    /// (`LzmaEnc.c:3037`).
    ///
    /// Byte 0 is `(pb*5 + lp)*9 + lc`. Bytes 1..5 are the little-endian dictionary
    /// size **after alignment**: the encoder does not write the raw `dict_size`,
    /// it rounds it up first (next 1 MiB multiple at/above 2 MiB; otherwise the
    /// next value of the form `2^k` or `3·2^(k-1)`). So a 19584-byte hunk yields
    /// an encoded dict of 24576, not 19584.
    pub fn decoder_props(&self) -> [u8; 5] {
        let mut out = [0u8; 5];
        out[0] = ((self.pb as u32 * 5 + self.lp as u32) * 9 + self.lc as u32) as u8;

        let d = self.dict_size;
        let v = if d >= (1u32 << 21) {
            // Round up to the next 1 MiB (2^20) boundary, guarding overflow.
            let mask = (1u32 << 20) - 1;
            let aligned = d.wrapping_add(mask) & !mask;
            if aligned < d { d } else { aligned }
        } else {
            // Smallest value of the form (2 + (i&1)) << (i>>1) that is >= d,
            // starting at i = 22 (LzmaEnc.c:3058-3064).
            let mut i = 11u32 * 2;
            loop {
                let v = (2 + (i & 1)) << (i >> 1);
                i += 1;
                if v >= d {
                    break v;
                }
            }
        };

        out[1..5].copy_from_slice(&v.to_le_bytes());
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chd_level8_params_match_sdk() {
        // After LzmaEncProps_Normalize at level 8: lc=3, lp=0, pb=2, fb=64, mc=48.
        let p = LzmaProps::chd_for_hunk(65536);
        assert_eq!(p.lc, 3);
        assert_eq!(p.lp, 0);
        assert_eq!(p.pb, 2);
        assert_eq!(p.fb, 64);
        assert_eq!(p.mc, 48);
    }

    #[test]
    fn dict_size_reduces_to_hunk() {
        // Common CHD hunk sizes are below 2^26 and at/above the floor, so the
        // dictionary equals the hunk size exactly.
        assert_eq!(LzmaProps::chd_for_hunk(4096).dict_size, 4096);
        assert_eq!(LzmaProps::chd_for_hunk(19584).dict_size, 19584);
        assert_eq!(LzmaProps::chd_for_hunk(65536).dict_size, 65536);
    }

    #[test]
    fn dict_size_honors_floor_and_cap() {
        // Below kReduceMin (4096) the dictionary is floored to 4096.
        assert_eq!(LzmaProps::chd_for_hunk(100).dict_size, K_REDUCE_MIN);
        // A hunk larger than the 64 MiB level-8 default keeps the full default.
        assert_eq!(LzmaProps::chd_for_hunk(1 << 27).dict_size, 1 << 26);
    }

    #[test]
    fn decoder_props_byte0_is_0x5d() {
        // (2*5 + 0)*9 + 3 = 93 = 0x5D for the standard lc3/lp0/pb2.
        let p = LzmaProps::chd_for_hunk(4096);
        assert_eq!(p.decoder_props()[0], 0x5D);
    }

    #[test]
    fn decoder_props_dict_is_aligned_not_raw() {
        // 4096 == 2^12 is already a power of two: stays 4096.
        assert_eq!(
            LzmaProps::chd_for_hunk(4096).decoder_props(),
            [0x5D, 0x00, 0x10, 0x00, 0x00]
        );
        // 19584 rounds up to 24576 (3 * 2^13).
        assert_eq!(
            LzmaProps::chd_for_hunk(19584).decoder_props(),
            [0x5D, 0x00, 0x60, 0x00, 0x00]
        );
        // 65536 == 2^16 stays 65536.
        assert_eq!(
            LzmaProps::chd_for_hunk(65536).decoder_props(),
            [0x5D, 0x00, 0x00, 0x01, 0x00]
        );
        // A >= 2 MiB dict aligns to the next 1 MiB boundary; 2^26 is already
        // aligned.
        assert_eq!(
            LzmaProps::chd_for_hunk(1 << 27).decoder_props(),
            [0x5D, 0x00, 0x00, 0x00, 0x04]
        );
    }
}
