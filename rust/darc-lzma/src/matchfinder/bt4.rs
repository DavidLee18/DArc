//! BT4 binary-tree match search — a port of `GetMatchesSpec1` and
//! `SkipMatchesSpec` (`LzFind.c:955`, `:1025`).
//!
//! `son` is the binary tree (2 entries per cyclic-buffer slot). Distances are
//! reported 0-based (`delta - 1`), matching the C `distances` array. `cur` is the
//! current byte's index into `input` (= `pos - 1`).
//!
//! ## Bit-exactness hazards (see CLAUDE.md)
//! - Search depth is bounded by `cut_value` (= `mc` = 48 at level 8).
//! - The left/right descent order and equal-length tie-break are part of the
//!   contract: the tree yields strictly increasing lengths, and for a given length
//!   the closest (smallest) distance.

use super::Match;

const EMPTY_HASH_VALUE: u32 = 0;

/// `GetMatchesSpec1`: walk the binary tree from `cur_match`, append `(len, dist0)`
/// pairs of strictly increasing length to `out`, and splice the tree.
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_matches_spec1(
    len_limit: u32,
    mut cur_match: u32,
    pos: u32,
    input: &[u8],
    cur: usize,
    son: &mut [u32],
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    mut cut_value: u32,
    out: &mut Vec<Match>,
    mut max_len: u32,
) {
    let mut ptr0 = ((cyclic_buffer_pos as usize) << 1) + 1; // greater-subtree link
    let mut ptr1 = (cyclic_buffer_pos as usize) << 1; // smaller-subtree link
    let mut len0: u32 = 0;
    let mut len1: u32 = 0;

    let cm_check = pos.saturating_sub(cyclic_buffer_size);

    if cm_check < cur_match {
        loop {
            let delta = pos - cur_match;
            let pair = (((cyclic_buffer_pos + cyclic_buffer_size - delta) % cyclic_buffer_size)
                as usize)
                << 1;
            let pb = cur - delta as usize;
            let mut len = len0.min(len1);
            let pair0 = son[pair];

            if input[pb + len as usize] == input[cur + len as usize] {
                len += 1;
                while len != len_limit && input[pb + len as usize] == input[cur + len as usize] {
                    len += 1;
                }
                if max_len < len {
                    max_len = len;
                    out.push(Match {
                        len,
                        dist: delta - 1,
                    });
                    if len == len_limit {
                        son[ptr1] = pair0;
                        son[ptr0] = son[pair + 1];
                        return;
                    }
                }
            }

            if input[pb + len as usize] < input[cur + len as usize] {
                son[ptr1] = cur_match;
                ptr1 = pair + 1;
                cur_match = son[pair + 1];
                len1 = len;
            } else {
                son[ptr0] = cur_match;
                ptr0 = pair;
                cur_match = son[pair];
                len0 = len;
            }

            cut_value -= 1;
            if cut_value == 0 || cm_check >= cur_match {
                break;
            }
        }
    }

    son[ptr0] = EMPTY_HASH_VALUE;
    son[ptr1] = EMPTY_HASH_VALUE;
}

/// `SkipMatchesSpec`: same tree splice as `get_matches_spec1` but records nothing.
#[allow(clippy::too_many_arguments)]
pub(crate) fn skip_matches_spec(
    len_limit: u32,
    mut cur_match: u32,
    pos: u32,
    input: &[u8],
    cur: usize,
    son: &mut [u32],
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    mut cut_value: u32,
) {
    let mut ptr0 = ((cyclic_buffer_pos as usize) << 1) + 1;
    let mut ptr1 = (cyclic_buffer_pos as usize) << 1;
    let mut len0: u32 = 0;
    let mut len1: u32 = 0;

    let cm_check = pos.saturating_sub(cyclic_buffer_size);

    if cm_check < cur_match {
        loop {
            let delta = pos - cur_match;
            let pair = (((cyclic_buffer_pos + cyclic_buffer_size - delta) % cyclic_buffer_size)
                as usize)
                << 1;
            let pb = cur - delta as usize;
            let mut len = len0.min(len1);

            if input[pb + len as usize] == input[cur + len as usize] {
                len += 1;
                while len != len_limit && input[pb + len as usize] == input[cur + len as usize] {
                    len += 1;
                }
                if len == len_limit {
                    son[ptr1] = son[pair];
                    son[ptr0] = son[pair + 1];
                    return;
                }
            }

            if input[pb + len as usize] < input[cur + len as usize] {
                son[ptr1] = cur_match;
                ptr1 = pair + 1;
                cur_match = son[pair + 1];
                len1 = len;
            } else {
                son[ptr0] = cur_match;
                ptr0 = pair;
                cur_match = son[pair];
                len0 = len;
            }

            cut_value -= 1;
            if cut_value == 0 || cm_check >= cur_match {
                break;
            }
        }
    }

    son[ptr0] = EMPTY_HASH_VALUE;
    son[ptr1] = EMPTY_HASH_VALUE;
}
