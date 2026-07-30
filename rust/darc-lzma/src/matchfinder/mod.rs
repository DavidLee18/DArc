//! Match finder — a port of `CMatchFinder` (`LzFind.c`), restricted to the
//! single-threaded BT4 (binary-tree, 4-byte hash) configuration CHD uses.
//!
//! Operates over the full input in memory: CHD reduces the dictionary to the hunk
//! size, so the data always fits in one cyclic buffer (no `ReadBlock`/`MoveBlock`
//! streaming, and `pos` never reaches the 4 GiB normalization point).
//!
//! ## Bit-exactness hazards (see CLAUDE.md)
//! - `pos` starts at **1** (`MatchFinder_Init_4`); hashes are zeroed (any order,
//!   since all to `kEmptyHashValue = 0`).
//! - `matchMaxLen` = `fb` (the length cap), `cyclicBufferSize` = `dictSize + 1`,
//!   `cutValue` = `mc`, `hashMask` = `GetHashMask(dictSize)`.
//! - Match lists match C's `Bt4_MatchFinder_GetMatches` exactly (increasing
//!   length, closest distance per length), verified out-of-tree against the C
//!   oracle (see docs/comparing-against-the-c-oracle.md).

pub mod bt4;
pub mod hash;

use hash::{CRC_SHIFT_1, FIX3_HASH_SIZE, FIX4_HASH_SIZE, HASH2_SIZE, HASH3_SIZE};

use crate::props::LzmaProps;

/// One `(len, dist)` candidate. `dist` is **0-based** (the LZMA encoded distance,
/// i.e. actual back-distance minus 1), matching the C `distances` array.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Match {
    pub len: u32,
    pub dist: u32,
}

const CRC_POLY: u32 = 0xEDB8_8320;

/// `MatchFinder_GetHashMask` for `numHashBytes == 4`.
fn get_hash_mask(history_size: u32) -> u32 {
    let mut hs = history_size.saturating_sub(1);
    hs |= hs >> 1;
    hs |= hs >> 2;
    hs |= hs >> 4;
    hs |= hs >> 8;
    hs >>= 1;
    if hs >= (1 << 24) {
        hs >>= 1; // numHashBytes == 4 branch
    }
    hs |= (1 << 16) - 1;
    hs
}

/// BT4 match finder over an input buffer.
pub struct MatchFinder<'a> {
    input: &'a [u8],
    pos: u32, // 1-based
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    match_max_len: u32,
    cut_value: u32,
    hash_mask: u32,
    hash: Vec<u32>,
    son: Vec<u32>,
    crc: [u32; 256],
}

impl<'a> MatchFinder<'a> {
    /// Create and initialize the finder for `input` with the given properties.
    pub fn new(input: &'a [u8], props: &LzmaProps) -> Self {
        let history_size = props.dict_size;
        let cyclic_buffer_size = history_size + 1;
        let hash_mask = get_hash_mask(history_size);
        let hash_len = FIX4_HASH_SIZE + hash_mask as usize + 1;

        let mut crc = [0u32; 256];
        for (i, slot) in crc.iter_mut().enumerate() {
            let mut r = i as u32;
            for _ in 0..8 {
                r = (r >> 1) ^ (CRC_POLY & 0u32.wrapping_sub(r & 1));
            }
            *slot = r;
        }

        MatchFinder {
            input,
            pos: 1,
            cyclic_buffer_pos: 1,
            cyclic_buffer_size,
            match_max_len: props.fb,
            cut_value: props.mc,
            hash_mask,
            hash: vec![0u32; hash_len],
            son: vec![0u32; 2 * cyclic_buffer_size as usize],
            crc,
        }
    }

    /// Bytes not yet consumed (`GetNumAvailableBytes`).
    #[inline]
    pub fn num_available(&self) -> u32 {
        self.input.len() as u32 - (self.pos - 1)
    }

    /// Data index of the byte the next `get_matches`/`skip` will process
    /// (`GetPointerToCurrentPos` as an offset).
    #[inline]
    pub fn pos_index(&self) -> usize {
        (self.pos - 1) as usize
    }

    #[inline]
    fn move_pos(&mut self) {
        self.cyclic_buffer_pos += 1;
        if self.cyclic_buffer_pos == self.cyclic_buffer_size {
            self.cyclic_buffer_pos = 0;
        }
        self.pos += 1;
    }

    /// Find all matches at the current position into `out` (cleared first), then
    /// advance one byte. Mirrors `Bt4_MatchFinder_GetMatches`.
    pub fn get_matches(&mut self, out: &mut Vec<Match>) {
        out.clear();
        let avail = self.num_available();
        let len_limit = self.match_max_len.min(avail);
        if len_limit < 4 {
            self.move_pos();
            return;
        }

        let cur = (self.pos - 1) as usize;
        let pos = self.pos;

        // HASH4_CALC
        let temp = self.crc[self.input[cur] as usize] ^ self.input[cur + 1] as u32;
        let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
        let temp = temp ^ ((self.input[cur + 2] as u32) << 8);
        let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
        let hv = ((temp ^ (self.crc[self.input[cur + 3] as usize] << CRC_SHIFT_1)) & self.hash_mask)
            as usize;

        let h2i = h2;
        let h3i = FIX3_HASH_SIZE + h3;
        let h4i = FIX4_HASH_SIZE + hv;
        let d2 = pos - self.hash[h2i];
        let d3 = pos - self.hash[h3i];
        let cur_match = self.hash[h4i];
        self.hash[h2i] = pos;
        self.hash[h3i] = pos;
        self.hash[h4i] = pos;

        let mmm = self.cyclic_buffer_size.min(pos);
        let mut max_len = 3u32;

        // Short (2/3-byte) match handling — the C `for(;;)` block, run at most once.
        let mut short_recorded = false;
        let mut chosen_d = 0u32;
        if d2 < mmm && self.input[cur - d2 as usize] == self.input[cur] {
            out.push(Match {
                len: 2,
                dist: d2 - 1,
            });
            if self.input[cur - d2 as usize + 2] == self.input[cur + 2] {
                chosen_d = d2;
                short_recorded = true;
            } else if d3 < mmm && self.input[cur - d3 as usize] == self.input[cur] {
                out.push(Match {
                    len: 0,
                    dist: d3 - 1,
                });
                chosen_d = d3;
                short_recorded = true;
            }
        } else if d3 < mmm && self.input[cur - d3 as usize] == self.input[cur] {
            out.push(Match {
                len: 0,
                dist: d3 - 1,
            });
            chosen_d = d3;
            short_recorded = true;
        }

        if short_recorded {
            // UPDATE_maxLen: extend the chosen match from offset max_len (3).
            let diff = chosen_d as usize;
            let lim = cur + len_limit as usize;
            let mut c = cur + max_len as usize;
            while c != lim && self.input[c - diff] == self.input[c] {
                c += 1;
            }
            max_len = (c - cur) as u32;
            let last = out.len() - 1;
            out[last].len = max_len;
            if max_len == len_limit {
                bt4::skip_matches_spec(
                    len_limit,
                    cur_match,
                    pos,
                    self.input,
                    cur,
                    &mut self.son,
                    self.cyclic_buffer_pos,
                    self.cyclic_buffer_size,
                    self.cut_value,
                );
                self.move_pos();
                return;
            }
        }

        bt4::get_matches_spec1(
            len_limit,
            cur_match,
            pos,
            self.input,
            cur,
            &mut self.son,
            self.cyclic_buffer_pos,
            self.cyclic_buffer_size,
            self.cut_value,
            out,
            max_len,
        );
        self.move_pos();
    }

    /// Advance `num` positions, maintaining the tree but recording nothing
    /// (`MatchFinder_Skip` for BT4).
    pub fn skip(&mut self, num: u32) {
        for _ in 0..num {
            let avail = self.num_available();
            let len_limit = self.match_max_len.min(avail);
            if len_limit < 4 {
                self.move_pos();
                continue;
            }
            let cur = (self.pos - 1) as usize;
            let pos = self.pos;

            let temp = self.crc[self.input[cur] as usize] ^ self.input[cur + 1] as u32;
            let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
            let temp = temp ^ ((self.input[cur + 2] as u32) << 8);
            let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
            let hv = ((temp ^ (self.crc[self.input[cur + 3] as usize] << CRC_SHIFT_1))
                & self.hash_mask) as usize;

            let cur_match = self.hash[FIX4_HASH_SIZE + hv];
            self.hash[h2] = pos;
            self.hash[FIX3_HASH_SIZE + h3] = pos;
            self.hash[FIX4_HASH_SIZE + hv] = pos;

            bt4::skip_matches_spec(
                len_limit,
                cur_match,
                pos,
                self.input,
                cur,
                &mut self.son,
                self.cyclic_buffer_pos,
                self.cyclic_buffer_size,
                self.cut_value,
            );
            self.move_pos();
        }
    }
}
