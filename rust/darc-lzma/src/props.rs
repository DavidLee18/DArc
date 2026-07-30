//! Encoder properties: a port of `CLzmaEncProps` + `LzmaEncProps_Init` /
//! `LzmaEncProps_Normalize` (`LzmaEnc.c:57-115`) and `LzmaEnc_WriteProperties`
//! (`LzmaEnc.c:3037`).
//!
//! This crate targets the single-threaded, optimal-parser path (`algo` is always 1;
//! the fast parser is not ported), across all five match finders DArc's
//! `C_LZMA.cpp` accepts -- see [`MatchFinderKind`], and note that DArc's own
//! default is the 5-byte hash chain, not BT4.

/// Smallest dictionary the encoder will use, even after reduction
/// (`kReduceMin`, `LzmaEnc.c:84`).
const K_REDUCE_MIN: u32 = 1 << 12; // 4096

/// `kLzmaMaxHistorySize` (`LzmaEnc.c:545`) — the ceiling `LzmaEnc_SetProps` clamps
/// `dictSize` to before the match finder ever sees it.
///
/// It is load-bearing here, not decoration: the window geometry in
/// [`crate::matchfinder`] is total only over the clamped domain, and
/// `LzmaEnc_WriteProperties` encodes the clamped value.
pub(crate) const K_MAX_HISTORY_SIZE: u32 = 15 << 28;

/// Which match finder `LzmaEnc` runs.
///
/// DArc's five `matchFinder` IDs (`C_LZMA.cpp:16`) do **not** name the SDK's five
/// implementations. They name `(btMode, numHashBytes)` pairs
/// (`C_LZMA.cpp:100-107`), which `MatchFinder_CreateVTable` (`LzFind.c:1664`) then
/// resolves to the search function. The mapping is worth stating because two of the
/// names are actively misleading:
///
/// | DArc ID | `(btMode, numHashBytes)` | SDK function |
/// |---|---|---|
/// | `kBT2` = 0 | (1, 2) | `Bt2_MatchFinder_GetMatches` |
/// | `kBT3` = 1 | (1, 3) | `Bt3_MatchFinder_GetMatches` |
/// | `kBT4` = 2 | (1, 4) | `Bt4_MatchFinder_GetMatches` |
/// | `kHC4` = 3 | (0, 4) | `Hc4_MatchFinder_GetMatches` |
/// | `kHT4` = 4 | (0, **5**) | `Hc5_MatchFinder_GetMatches` |
///
/// `kHT4` is a five-byte hash *chain*, not a four-byte hash table — and it is
/// **DArc's default** (`C_LZMA.cpp:253`), so it is the finder the shipped archiver
/// actually uses. This enum is named for what each variant *is* rather than for the
/// DArc ID, so that the misnomer cannot propagate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchFinderKind {
    /// Binary tree, 2-byte hash.
    Bt2,
    /// Binary tree, 3-byte hash.
    Bt3,
    /// Binary tree, 4-byte hash.
    Bt4,
    /// Hash chain, 4-byte hash.
    Hc4,
    /// Hash chain, 5-byte hash. DArc's default, via the `kHT4` ID.
    Hc5,
}

impl MatchFinderKind {
    /// Resolve a DArc `matchFinder` ID, or `None` if it names no finder.
    ///
    /// Returns `Option` rather than falling back, because the ID reaches us from a
    /// method string that was **stored in the archive** — it is caller-supplied
    /// data, not a value this code computes, so an unrecognized one has to be
    /// refused rather than quietly turned into BT4. (The C's `default:` arm does
    /// silently pick BT4, `C_LZMA.cpp:107`; reproducing that would mean two builds
    /// disagreeing about what a malformed method string means.)
    pub fn from_stream(id: i32) -> Option<Self> {
        match id {
            0 => Some(MatchFinderKind::Bt2),
            1 => Some(MatchFinderKind::Bt3),
            2 => Some(MatchFinderKind::Bt4),
            3 => Some(MatchFinderKind::Hc4),
            4 => Some(MatchFinderKind::Hc5),
            _ => None,
        }
    }

    /// `props.btMode`: 1 for the binary-tree finders, 0 for the hash chains.
    ///
    /// Load-bearing beyond the search itself: it halves the `son` array
    /// (`MatchFinder_Create`: `numSons <<= 1` only when `btMode`), it shifts the
    /// automatic `mc` (`LzmaEnc.c:99`), and it gates multi-threading entirely
    /// (`LzmaEnc.c:2695`: `mtMode = multiThread && !fastMode && btMode != 0`).
    pub fn bt_mode(self) -> bool {
        match self {
            MatchFinderKind::Bt2 | MatchFinderKind::Bt3 | MatchFinderKind::Bt4 => true,
            MatchFinderKind::Hc4 | MatchFinderKind::Hc5 => false,
        }
    }

    /// `MFB.numHashBytes` after `LzmaEnc_SetProps`'s clamping (`LzmaEnc.c:573-581`).
    pub fn num_hash_bytes(self) -> u32 {
        match self {
            MatchFinderKind::Bt2 => 2,
            MatchFinderKind::Bt3 => 3,
            MatchFinderKind::Bt4 | MatchFinderKind::Hc4 => 4,
            MatchFinderKind::Hc5 => 5,
        }
    }

    /// `p->fixedHashSize` (`MatchFinder_Create`, `LzFind.c:450`) with the default
    /// `numHashBytes_Min = 2` (`MatchFinder_SetDefaultSettings`).
    ///
    /// The 4- and 5-byte finders share a value: the C accumulates only for
    /// `numHashBytes > 2` and `> 3`, and its `> 4` term is commented out.
    pub fn fixed_hash_size(self) -> usize {
        let mut n = 0usize;
        if self.num_hash_bytes() > 2 {
            n += 1 << 10; // kHash2Size
        }
        if self.num_hash_bytes() > 3 {
            n += 1 << 16; // kHash3Size
        }
        n
    }

    /// The automatic `mc`, i.e. `LzmaEnc.c:99`:
    /// `mc = (16 + (fb >> 1)) >> (btMode ? 0 : 1)`.
    ///
    /// The shift is why a single "auto mc" formula cannot be shared across finders:
    /// the hash chains get **half** the cut value the trees do. Applying the BT
    /// formula to a chain doubles its search depth and changes the parse.
    pub fn auto_mc(self, fb: u32) -> u32 {
        let mc = 16 + (fb >> 1);
        if self.bt_mode() { mc } else { mc >> 1 }
    }
}

/// Normalized LZMA encoder properties for the optimal-parser path.
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
    ///
    /// If a caller has `mc == 0` — DArc's "auto" sentinel — it must resolve it with
    /// [`MatchFinderKind::auto_mc`] before constructing this, because the formula
    /// depends on `mf` and taking 0 literally underflows the search's cut counter.
    pub mc: u32,
    /// Which match finder to run. See [`MatchFinderKind`]; DArc's default is
    /// [`MatchFinderKind::Hc5`], not `Bt4`.
    pub mf: MatchFinderKind,
    /// `p->fastMode`, i.e. `algo == 0` (`LzmaEnc.c:568`) — the greedy/lazy parser
    /// instead of the optimal one.
    ///
    /// DArc reaches it through the method-string words `fast` and `fastest`
    /// (`C_LZMA.cpp:361-362`), and its own preset `3binary` uses one
    /// (`Compression.hs:475`). `algo` changes nothing else on DArc's path: `btMode`
    /// and `numHashBytes` come from `matchFinder` explicitly, so
    /// `LzmaEncProps_Normalize`'s `algo`-derived defaults never apply.
    pub fast_mode: bool,
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

        // The SDK's own level defaults, which land on the binary tree: with `algo`
        // defaulting to `level < 5 ? 0 : 1` and `btMode` to `algo == 0 ? 0 : 1`,
        // every level >= 5 gives algo=1 / btMode=1 / numHashBytes=4
        // (LzmaEnc.c:95-98). Note this is NOT what DArc's LZMA_METHOD does -- DArc
        // sets its parameters explicitly and defaults to Hc5 (C_LZMA.cpp:246-257).
        let fb = if level < 7 { 32u32 } else { 64u32 };
        let mf = MatchFinderKind::Bt4;
        let mc = mf.auto_mc(fb);

        LzmaProps {
            lc,
            lp,
            pb,
            dict_size,
            fb,
            mc,
            mf,
            // algo defaults to `level < 5 ? 0 : 1` (LzmaEnc.c:95); every level this
            // constructor is used with is >= 5, so the optimal parser.
            fast_mode: false,
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

    /// The dictionary size the encoder actually runs with: `dict_size` clamped to
    /// `kLzmaMaxHistorySize` as `LzmaEnc_SetProps` does (`LzmaEnc.c:545`).
    ///
    /// Everything downstream — the cyclic buffer, the window geometry, the encoded
    /// property bytes — must use *this*, not the raw field, or a caller asking for
    /// a 4 GiB dictionary gets a stream the C would not have produced.
    pub fn history_size(&self) -> u32 {
        self.dict_size.min(K_MAX_HISTORY_SIZE)
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

        let d = self.history_size();
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
    fn darc_match_finder_ids_map_as_c_lzma_does() {
        // The table in C_LZMA.cpp:100-107, checked rather than trusted -- two of the
        // five names do not describe what they select, and kHT4 is DArc's DEFAULT.
        let cases = [
            (0, MatchFinderKind::Bt2, true, 2u32),
            (1, MatchFinderKind::Bt3, true, 3),
            (2, MatchFinderKind::Bt4, true, 4),
            (3, MatchFinderKind::Hc4, false, 4),
            (4, MatchFinderKind::Hc5, false, 5),
        ];
        for (id, kind, bt, nhb) in cases {
            assert_eq!(MatchFinderKind::from_stream(id), Some(kind), "id {id}");
            assert_eq!(kind.bt_mode(), bt, "{kind:?} btMode");
            assert_eq!(kind.num_hash_bytes(), nhb, "{kind:?} numHashBytes");
        }
        // Out of range is refused, not defaulted to BT4 the way the C's `default:`
        // arm does. See from_stream's doc comment for why.
        assert_eq!(MatchFinderKind::from_stream(-1), None);
        assert_eq!(MatchFinderKind::from_stream(5), None);
    }

    #[test]
    fn auto_mc_halves_for_the_hash_chains() {
        // LzmaEnc.c:99 -- `>> (btMode ? 0 : 1)`. Getting this wrong doubles a chain's
        // search depth, which changes the parse without changing anything visible in
        // the parameters.
        for fb in [1u32, 5, 32, 64, 273] {
            let bt = 16 + (fb >> 1);
            assert_eq!(MatchFinderKind::Bt4.auto_mc(fb), bt, "fb {fb}");
            assert_eq!(MatchFinderKind::Bt2.auto_mc(fb), bt, "fb {fb}");
            assert_eq!(MatchFinderKind::Hc4.auto_mc(fb), bt >> 1, "fb {fb}");
            assert_eq!(MatchFinderKind::Hc5.auto_mc(fb), bt >> 1, "fb {fb}");
        }
    }

    #[test]
    fn fixed_hash_size_matches_matchfinder_create() {
        // LzFind.c:450 with numHashBytes_Min = 2: only the `> 2` and `> 3` terms are
        // live, so 4 and 5 share a value and 2 contributes nothing.
        assert_eq!(MatchFinderKind::Bt2.fixed_hash_size(), 0);
        assert_eq!(MatchFinderKind::Bt3.fixed_hash_size(), 1 << 10);
        assert_eq!(MatchFinderKind::Bt4.fixed_hash_size(), (1 << 10) + (1 << 16));
        assert_eq!(MatchFinderKind::Hc4.fixed_hash_size(), (1 << 10) + (1 << 16));
        assert_eq!(MatchFinderKind::Hc5.fixed_hash_size(), (1 << 10) + (1 << 16));
    }

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
