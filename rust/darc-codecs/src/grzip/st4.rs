//! Inverse sort-transform of order 4, ported from `Compression/GRZip/ST4.c`
//! (`GRZip_ST4_Decode` :99).
//!
//! ST4 is the BWT's cheaper cousin: instead of sorting full rotations it sorts
//! only by the next four bytes, which is far faster to compute and almost as
//! good for the entropy stage. The encoder picks it over the BWT via
//! `GRZ_Compression_ST4` in the block mode.
//!
//! Inverting it is not the BWT's single LF-mapping walk. Because the sort key
//! is bounded, several rows can tie, and the reconstruction needs a second
//! level of indirection to break those ties in the order the encoder saw them.
//! That is what the `ST_INDIRECT` bit in the table marks, and why the final
//! loop has two branches rather than one.
//!
//! The table packs three things into each 32-bit word: the byte in the top 8
//! bits, `ST_INDIRECT` (bit 23) as the indirection flag, and a position in the
//! low 23 bits. A block is capped at just under 8 MB, which is what keeps
//! positions inside those 23 bits.

use super::{GrzError, GRZ_CRC_ERROR};

const MAX_BYTE: usize = 256;
const MAX_WORD: usize = 65536;
/// `ST_INDIRECT` (:43) -- bit 23, both a flag and the mask for the position.
const INDIRECT: u32 = 0x800000;

/// `GRZip_ST4_Decode`, in place. `fbp` is the first-byte position from the
/// block header; the C left it unchecked until an earlier hardening pass, where
/// out of range it indexed off the end of `Table`.
pub fn decode(buf: &mut [u8], size: usize, fbp: i32) -> Result<(), GrzError> {
    if size == 0 || buf.len() < size {
        return Err(GRZ_CRC_ERROR);
    }
    // Table holds size+1 entries, so fbp == size is legitimate.
    if fbp < 0 || fbp as usize > size {
        return Err(GRZ_CRC_ERROR);
    }
    // Positions must fit the low 23 bits alongside the flag.
    if size >= INDIRECT as usize {
        return Err(GRZ_CRC_ERROR);
    }
    let fbp = fbp as usize;

    let mut t = [0i32; MAX_BYTE];
    let mut context2 = vec![0i32; MAX_WORD];
    // One bit per position, marking where a new order-2 context begins.
    let mut flag = vec![0u8; (size + 8) >> 3];
    let mut table = vec![0u32; size + 1];

    // First-column counts, then their exclusive prefix sums.
    for i in 0..size {
        t[buf[i] as usize] += 1;
    }
    {
        let mut sum = 0i32;
        let mut j = 0usize;
        for i in 0..MAX_BYTE {
            sum += t[i];
            t[i] = sum - t[i];
            while (j as i32) < sum {
                context2[((buf[j] as usize) << 8) | i] += 1;
                j += 1;
            }
        }
    }
    let s_init = t;

    // Walk the order-2 contexts, marking the first row of each run per byte.
    let mut last_seen = [-1i64; MAX_BYTE];
    {
        let mut sum = 0i32;
        let mut j = 0usize;
        // LastSeen starts at -1 (the C memsets 0xFF) so no context matches
        // initially.
        for i in 0..MAX_WORD {
            let cstart = sum;
            sum += context2[i];
            while (j as i32) < sum {
                let c = buf[j] as usize;
                if last_seen[c] != cstart as i64 {
                    last_seen[c] = cstart as i64;
                    let bit = t[c] as usize;
                    flag[bit >> 3] |= 1 << (bit & 7);
                }
                t[c] += 1;
                j += 1;
            }
        }
    }

    // Build the reconstruction table. An entry is either a direct first-column
    // position, or ST_INDIRECT plus the previous occurrence of the same byte --
    // the tie-break the bounded sort key makes necessary.
    let mut s = s_init;
    last_seen = [0i64; MAX_BYTE];  // secure_memzero before the table build
    {
        let mut cstart: i64 = 0;
        for i in 0..size {
            let c = buf[i] as usize;
            if flag[i >> 3] & (1 << (i & 7)) != 0 {
                cstart = i as i64;
            }
            if last_seen[c] <= cstart {
                table[i] = s[c] as u32;
                last_seen[c] = i as i64 + 1;
            } else {
                table[i] = ((last_seen[c] - 1) as u32) | INDIRECT;
            }
            s[c] += 1;
            table[i] |= (c as u32) << 24;
        }
    }
    table[size] = INDIRECT;

    // Follow the chain. Both branches advance a counter stored in the table
    // itself, which is how repeated contexts stay in encoder order.
    let mut j = fbp;
    let mut sum = table[fbp];
    for i in 0..size {
        if sum & INDIRECT != 0 {
            let at = (sum & (INDIRECT - 1)) as usize;
            if at >= table.len() {
                return Err(GRZ_CRC_ERROR);
            }
            j = (table[at] & (INDIRECT - 1)) as usize;
            table[at] = table[at].wrapping_add(1);
            if j >= table.len() {
                return Err(GRZ_CRC_ERROR);
            }
            sum = table[j];
        } else {
            if j >= table.len() {
                return Err(GRZ_CRC_ERROR);
            }
            table[j] = table[j].wrapping_add(1);
            j = (sum & (INDIRECT - 1)) as usize;
            if j >= table.len() {
                return Err(GRZ_CRC_ERROR);
            }
            sum = table[j];
        }
        buf[i] = (sum >> 24) as u8;
    }
    Ok(())
}

/// `GRZip_ST4_Encode` (ST4.c:55). Returns the first-byte position, which the
/// block driver stores in the header for `decode` to start from.
///
/// A counting sort by the two-byte context, then a stable placement pass that
/// carries the four-byte rolling context. `Counter` becomes an EXCLUSIVE prefix
/// sum (`W += c[i], c[i] = W - c[i]`), so each entry starts as the first slot
/// for its context and is bumped as slots are consumed.
///
/// The C is called in place -- `GRZip_ST4_Encode(LZPBuffer, Size, LZPBuffer)` --
/// which is safe there only because every read of `Input` happens before any
/// write to `Output`: the placement loop fills `Context`, and only then do the
/// two output loops run. Separate slices here are equivalent, and the block
/// driver can copy when it wants the in-place form.
pub fn encode(input: &[u8], size: usize, output: &mut [u8]) -> Result<i32, GrzError> {
    // The FBP seed below reads Input[Size-5]; the block driver never passes
    // fewer than 32 bytes.
    if size < 5 || input.len() < size || output.len() < size {
        return Err(GRZ_CRC_ERROR);
    }

    let mut counter = vec![0i32; MAX_WORD];
    let mut context = vec![0u32; size];

    // Histogram of two-byte contexts, wrapping from the last byte into the
    // first -- the transform is over a cyclic string.
    let mut w: u32 = (input[size - 1] as u32) << 8;
    for &b in input[..size].iter() {
        w = (w >> 8) | ((b as u32) << 8);
        counter[w as usize] += 1;
    }

    // Exclusive prefix sum, in place.
    let mut acc: u32 = 0;
    for c in counter.iter_mut() {
        acc = acc.wrapping_add(*c as u32);
        *c = acc.wrapping_sub(*c as u32) as i32;
    }

    let w2 = ((input[size - 4] as u32) << 8) | (input[size - 5] as u32);
    let mut fbp: i32 = if w2 == 0xFFFF {
        size as i32 - 1
    } else {
        counter[w2 as usize + 1] - 1
    };

    // The four-byte rolling context, seeded from the block's tail so the first
    // sample wraps correctly.
    let mut w: u32 = ((input[size - 1] as u32) << 24)
        | ((input[size - 2] as u32) << 16)
        | ((input[size - 3] as u32) << 8)
        | (input[size - 4] as u32);

    for &c in input[..size].iter() {
        let slot = counter[(w & 0x0000FFFF) as usize];
        counter[(w & 0x0000FFFF) as usize] = slot + 1;
        context[slot as usize] = (w & 0xFFFF0000) | c as u32;
        w = (w >> 8) | ((c as u32) << 24);
    }

    // FBP indexes Context below, and C does not check it. It cannot go out of
    // range for real input -- the context it looks up always occurs in the
    // histogram, at i == Size-4, so Counter[W+1] >= 1 -- but this is an
    // exported function and a panic here would unwind across the C ABI.
    if fbp < 0 || fbp as usize >= size {
        return Err(GRZ_CRC_ERROR);
    }

    // Emit backwards, in two runs split at FBP: the counters are consumed from
    // the top, so walking down leaves each context's slots in encounter order.
    let mut i = size as i32 - 1;
    while i >= fbp {
        let e = context[i as usize];
        let k = (e >> 16) as usize;
        counter[k] -= 1;
        output[counter[k] as usize] = (e & 0xFF) as u8;
        i -= 1;
    }
    fbp = counter[(context[fbp as usize] >> 16) as usize];
    while i >= 0 {
        let e = context[i as usize];
        let k = (e >> 16) as usize;
        counter[k] -= 1;
        output[counter[k] as usize] = (e & 0xFF) as u8;
        i -= 1;
    }

    Ok(fbp)
}
