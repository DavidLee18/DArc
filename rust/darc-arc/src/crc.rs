//! CRC-32, matching `UpdateCRC` in `Compression/Common.cpp`.
//!
//! Written out rather than pulled from a crate: it is twenty lines, and a new
//! dependency in this project carries a licence question (THIRD-PARTY.md, and
//! the tree is GPLv3-or-later).
//!
//! `Compression.hs:333` sets `aINIT_CRC = 0xffffffff` and `finishCRC = xor
//! aINIT_CRC`, so this is the ordinary IEEE 802.3 CRC-32 — the same polynomial
//! zip and gzip use, which is what lets the value be checked against `arc t`.

const POLY: u32 = 0xEDB8_8320;

/// The table, built once at first use.
fn table() -> &'static [u32; 256] {
    use std::sync::OnceLock;
    static TABLE: OnceLock<[u32; 256]> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut t = [0u32; 256];
        let mut i = 0usize;
        while i < 256 {
            let mut c = i as u32;
            let mut k = 0;
            while k < 8 {
                c = if c & 1 != 0 { POLY ^ (c >> 1) } else { c >> 1 };
                k += 1;
            }
            t[i] = c;
            i += 1;
        }
        t
    })
}

/// `updateCRC` — fold more bytes into a running value. The running value is the
/// *un-finished* one, so `aINIT_CRC` starts it and [`finish`] ends it.
pub fn update(mut crc: u32, data: &[u8]) -> u32 {
    let t = table();
    for &b in data {
        crc = t[((crc ^ u32::from(b)) & 0xff) as usize] ^ (crc >> 8);
    }
    crc
}

/// `finishCRC = xor aINIT_CRC`.
pub fn finish(crc: u32) -> u32 {
    crc ^ 0xFFFF_FFFF
}

/// `calcCRC` — the whole thing in one call.
pub fn calc(data: &[u8]) -> u32 {
    finish(update(0xFFFF_FFFF, data))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The standard check value. If this passes, the polynomial, the bit order
    /// and the final xor are all right, which is everything that can be wrong.
    #[test]
    fn matches_the_ieee_check_value() {
        assert_eq!(calc(b"123456789"), 0xCBF4_3926);
    }

    #[test]
    fn the_empty_input_is_zero() {
        assert_eq!(calc(b""), 0);
    }

    /// Updating in pieces must equal updating at once -- the block reader folds
    /// a descriptor's CRC in as it writes, one buffer at a time.
    #[test]
    fn incremental_update_equals_one_shot() {
        let data: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();
        let one = calc(&data);
        let mut running = 0xFFFF_FFFFu32;
        for chunk in data.chunks(7) {
            running = update(running, chunk);
        }
        assert_eq!(finish(running), one);
    }
}
