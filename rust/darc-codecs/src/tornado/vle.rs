//! Variable-length length/distance code tables, ported from
//! `Compression/Tornado/LZ77_Coder.cpp` (`VLE` :147, `LengthCoder` :195,
//! `DistanceCoder` :217).
//!
//! These are pure lookup tables derived from the `extra_bits` arrays. The
//! decoder needs only `extra_bits(code)` and `base_value(code)` -- the
//! `value -> code` direction (`xcode`) is the encoder's, so it is not built
//! here. What matters for decoding is that `xbase_value` comes out identical,
//! including `DistanceCoder`'s three-range construction, because a base value
//! off by one silently shifts every match distance.

/// Extra bits per length code, bitcoder (`extra_lbits`, LZ77_Coder.cpp:189).
pub const EXTRA_LBITS: [u32; 8] = [0, 0, 0, 1, 2, 4, 8, 30];

/// Extra bits per length code, huffman/arith (`extra_lbits2`, :190).
pub const EXTRA_LBITS2: [u32; 16] = [0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 8, 30];

/// Extra bits per distance code (`extra_dbits`, :211).
pub const EXTRA_DBITS: [u32; 32] = [
    4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 17,
    18, 19, 21, 23, 30,
];

/// Base value for each length code: the running sum of `1 << extra_bits`, which
/// is what `VLE`'s constructor accumulates in `value`.
pub fn length_bases<const N: usize>(extra: &[u32; N]) -> [u32; N] {
    let mut base = [0u32; N];
    let mut value: u64 = 0;
    for code in 0..N {
        base[code] = value as u32;
        // The C stops filling xcode[] at VLE_SIZE but keeps advancing `value`
        // only while it writes, so the base values follow the unclamped sum for
        // every code that fits -- and the last code (30 extra bits) is never
        // used as a base for anything the decoder reads back.
        value += 1u64 << extra[code];
    }
    base
}

/// `DistanceCoder`'s base values (LZ77_Coder.cpp:233). Three ranges: distances
/// below 512 are counted one by one, then in units of 256, then of 65536 --
/// hence `dist << 8` and `dist << 16` rather than a plain running sum.
pub fn distance_bases() -> [u32; 32] {
    let extra = &EXTRA_DBITS;
    let mut base = [0u32; 32];
    let mut dist: u64 = 0;
    let mut code = 0usize;

    while dist < 512 && code < 32 {
        base[code] = dist as u32;
        dist += 1u64 << extra[code];
        code += 1;
    }
    dist >>= 8; // from here on distances are counted in units of 256
    while dist < 512 && code < 32 {
        base[code] = (dist << 8) as u32;
        dist += 1u64 << (extra[code] - 8);
        code += 1;
    }
    dist >>= 8; // and from here in units of 65536
    while code < 32 {
        base[code] = (dist << 16) as u32;
        dist += 1u64 << (extra[code] - 16);
        code += 1;
    }
    base
}

/// `VLE_SIZE` (LZ77_Coder.cpp:146) -- entries in the `value -> code` table.
/// The three ranges of `DistanceCoder` occupy `[0,512)`, `[512,1024)` and
/// `[1024,..)` of it.
pub const VLE_SIZE: usize = 1024 + 16384 + 1;

/// `VLE`'s `xcode` (LZ77_Coder.cpp:173): the encoder's `value -> code` map, one
/// entry per representable value, filled by walking each code over the values
/// its extra bits span.
pub fn length_codes<const N: usize>(extra: &[u32; N]) -> Vec<u8> {
    let mut xcode = vec![0u8; VLE_SIZE];
    let mut value: usize = 0;
    'outer: for (code, &bits) in extra.iter().enumerate() {
        for _ in 0..(1u64 << bits) {
            if value >= VLE_SIZE {
                break 'outer;
            }
            xcode[value] = code as u8;
            value += 1;
        }
    }
    xcode
}

/// `DistanceCoder`'s `xcode` (LZ77_Coder.cpp:233). Distances below 512 are
/// mapped one by one; then in units of 256 at offset 512; then in units of 65536
/// at offset 1024. `distance_code` does the matching three-way lookup.
pub fn distance_codes() -> Vec<u8> {
    let extra = &EXTRA_DBITS;
    let mut xcode = vec![0u8; VLE_SIZE];
    let mut dist: usize = 0;
    let mut code = 0usize;

    while dist < 512 {
        for _ in 0..(1usize << extra[code]) {
            xcode[dist] = code as u8;
            dist += 1;
        }
        code += 1;
    }
    dist >>= 8;
    while dist < 512 {
        for _ in 0..(1usize << (extra[code] - 8)) {
            xcode[512 + dist] = code as u8;
            dist += 1;
        }
        code += 1;
    }
    dist >>= 8;
    while code < extra.len() {
        for _ in 0..(1u64 << (extra[code] - 16)) {
            if 1024 + dist >= VLE_SIZE {
                break;
            }
            xcode[1024 + dist] = code as u8;
            dist += 1;
        }
        code += 1;
    }
    xcode
}

/// The three tables the coders index, built once per call. They are a few
/// hundred bytes on the decode side and about 34 KB with the encoder's two
/// `xcode` maps, so there is still no reason to reach for a global the way the C
/// does (`lc`, `lc2`, `dc` are file-scope objects with static constructors
/// there, built before `main` whether or not anything uses them).
pub struct Tables {
    pub lc_extra: [u32; 8],
    pub lc_base: [u32; 8],
    pub lc2_extra: [u32; 16],
    pub lc2_base: [u32; 16],
    pub dc_extra: [u32; 32],
    pub dc_base: [u32; 32],
    /// Encoder-only `value -> code` maps; empty on the decode path.
    lc_code: Vec<u8>,
    lc2_code: Vec<u8>,
    dc_code: Vec<u8>,
}

impl Tables {
    pub fn new() -> Self {
        Tables {
            lc_extra: EXTRA_LBITS,
            lc_base: length_bases(&EXTRA_LBITS),
            lc2_extra: EXTRA_LBITS2,
            lc2_base: length_bases(&EXTRA_LBITS2),
            dc_extra: EXTRA_DBITS,
            dc_base: distance_bases(),
            lc_code: Vec::new(),
            lc2_code: Vec::new(),
            dc_code: Vec::new(),
        }
    }

    /// Same tables plus the encoder's `value -> code` direction.
    pub fn for_encoding() -> Self {
        Tables {
            lc_code: length_codes(&EXTRA_LBITS),
            lc2_code: length_codes(&EXTRA_LBITS2),
            dc_code: distance_codes(),
            ..Tables::new()
        }
    }

    /// `LengthCoder::code` (:199) for the bitcoder's 8-code alphabet. Lengths
    /// past 600 all take the last code, whose 30 extra bits cover the rest.
    #[inline]
    pub fn lc_code(&self, length: u32) -> usize {
        if length > 600 {
            EXTRA_LBITS.len() - 1
        } else {
            self.lc_code[length as usize] as usize
        }
    }

    /// `LengthCoder::code` for the huffman/arith 16-code alphabet.
    #[inline]
    pub fn lc2_code(&self, length: u32) -> usize {
        if length > 600 {
            EXTRA_LBITS2.len() - 1
        } else {
            self.lc2_code[length as usize] as usize
        }
    }

    /// `DistanceCoder::code` (:221).
    #[inline]
    pub fn dc_code(&self, distance: u32) -> usize {
        let d = distance as usize;
        if d < 512 {
            self.dc_code[d] as usize
        } else if d < 512 * 256 {
            self.dc_code[512 + (d >> 8)] as usize
        } else {
            self.dc_code[1024 + (d >> 16)] as usize
        }
    }
}

impl Default for Tables {
    fn default() -> Self {
        Self::new()
    }
}
