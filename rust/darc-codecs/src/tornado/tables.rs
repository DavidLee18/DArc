//! Data-table diffing, ported from `Compression/Tornado/DataTables.cpp`
//! (`undiff_table` :46, `diff_table` :20, `DataTables` :128).
//!
//! Tornado's encoder spots tables of fixed-width numbers and subtracts each row
//! from the next, which the LZ stage then compresses far better. The decoder
//! has to add them back -- but only just before the bytes leave for the output
//! stream, because until then those same bytes are still the LZ window that
//! later matches copy from. So each table is undiffed, written, and then
//! **diffed again in place** to restore the window. That round trip is what
//! `undiff_tables` / `diff_tables` are for, and it is why a table straddling
//! two write chunks needs its base row saved and restored.
//!
//! Positions here are absolute indices into the decoder's buffer rather than
//! raw pointers. The buffer carries `PAD_FOR_TABLES` of slack at both ends, so
//! a table start may legitimately sit *before* the logical origin after the
//! window wraps -- which is exactly what that padding is reserved for.

use super::MAX_TABLE_ROW_AT_DECOMPRESSION as MAX_ROW;

/// Entries before the list forces a flush (`ENTRIES`, DataTables.cpp:166).
const ENTRIES: usize = 10000;

#[derive(Clone, Copy)]
pub struct TableEntry {
    /// Bytes per row (`table_type`).
    pub row: usize,
    /// Absolute index of the table's first byte.
    pub start: usize,
    /// Number of rows.
    pub len: usize,
}

/// Add each row to the previous one. Byte-wise with carry for widths other than
/// 2 and 4, which is what makes this exactly reversible by `diff_table`.
pub fn undiff_table(buf: &mut [u8], n: usize, start: usize, len: usize) {
    if len == 0 || n == 0 {
        return;
    }
    let end = start + n * len;
    match n {
        2 => {
            let mut v = u16::from_le_bytes([buf[start], buf[start + 1]]);
            let mut r = start + 2;
            while r < end {
                v = v.wrapping_add(u16::from_le_bytes([buf[r], buf[r + 1]]));
                buf[r..r + 2].copy_from_slice(&v.to_le_bytes());
                r += 2;
            }
        }
        4 => {
            let mut v = u32::from_le_bytes([buf[start], buf[start + 1], buf[start + 2], buf[start + 3]]);
            let mut r = start + 4;
            while r < end {
                v = v.wrapping_add(u32::from_le_bytes([buf[r], buf[r + 1], buf[r + 2], buf[r + 3]]));
                buf[r..r + 4].copy_from_slice(&v.to_le_bytes());
                r += 4;
            }
        }
        _ => {
            let mut r = start + n;
            while r < end {
                let mut carry = 0u32;
                for i in 0..n {
                    let t = buf[r + i] as u32 + buf[r + i - n] as u32 + carry;
                    buf[r + i] = t as u8;
                    carry = t >> 8;
                }
                r += n;
            }
        }
    }
}

/// The inverse: subtract each row from the next, restoring the LZ window after
/// a write. Walks backwards so each row still sees its undiffed predecessor.
pub fn diff_table(buf: &mut [u8], n: usize, start: usize, len: usize) {
    if len == 0 || n == 0 {
        return;
    }
    let end = start + n * len;
    match n {
        2 => {
            let mut prev = u16::from_le_bytes([buf[start], buf[start + 1]]);
            let mut r = start + 2;
            while r < end {
                let v = u16::from_le_bytes([buf[r], buf[r + 1]]);
                buf[r..r + 2].copy_from_slice(&v.wrapping_sub(prev).to_le_bytes());
                prev = v;
                r += 2;
            }
        }
        4 => {
            let mut prev =
                u32::from_le_bytes([buf[start], buf[start + 1], buf[start + 2], buf[start + 3]]);
            let mut r = start + 4;
            while r < end {
                let v = u32::from_le_bytes([buf[r], buf[r + 1], buf[r + 2], buf[r + 3]]);
                buf[r..r + 4].copy_from_slice(&v.wrapping_sub(prev).to_le_bytes());
                prev = v;
                r += 4;
            }
        }
        _ => {
            let mut r = end;
            while r > start + n {
                r -= n;
                let mut carry = 0u32;
                for i in 0..n {
                    let sub = buf[r + i - n] as u32 + carry;
                    let newcarry = ((buf[r + i] as u32) < sub) as u32;
                    buf[r + i] = (buf[r + i] as u32).wrapping_sub(sub) as u8;
                    carry = newcarry;
                }
            }
        }
    }
}

pub struct DataTables {
    tables: Vec<TableEntry>,
    /// Base row of a table split across two write chunks, saved before the
    /// re-diff so the next chunk can undiff from it.
    base_data: [u8; MAX_ROW],
    /// The bytes `undiff_tables` temporarily overwrote with `base_data`.
    original: [u8; MAX_ROW],
    original_bytes: usize,
}

impl DataTables {
    pub fn new() -> Self {
        DataTables {
            tables: Vec::with_capacity(64),
            base_data: [0; MAX_ROW],
            original: [0; MAX_ROW],
            original_bytes: 0,
        }
    }

    pub fn add(&mut self, row: usize, start: usize, len: usize) {
        self.tables.push(TableEntry { row, start, len });
    }

    pub fn filled(&self) -> bool {
        self.tables.len() >= ENTRIES
    }

    /// Rows of `e` that lie at or before `write_end`. The C lets this go
    /// negative for a table starting past the write point, where every loop
    /// body is then skipped; the saturating form here has the same effect.
    fn rows_until(e: &TableEntry, write_end: usize) -> usize {
        if e.row == 0 || write_end < e.start {
            return 0;
        }
        let fit = 1 + (write_end - e.start) / e.row;
        fit.min(e.len)
    }

    fn process(&mut self, buf: &mut [u8], write_end: usize, undiff: bool) {
        for e in &self.tables {
            let len = Self::rows_until(e, write_end);
            if undiff {
                undiff_table(buf, e.row, e.start, len);
            } else {
                diff_table(buf, e.row, e.start, len);
            }
        }
    }

    /// Undiff everything up to `write_end`, ready for the bytes to be written.
    pub fn undiff_tables(&mut self, buf: &mut [u8], write_start: usize, write_end: usize) {
        self.original_bytes = 0;
        if let Some(first) = self.tables.first().copied() {
            if first.start < write_start {
                // This table began in the previous chunk. Its first row on disk
                // is a difference, so swap in the base row saved back then, and
                // remember what was there to put back afterwards.
                let n = first.row.min(write_start - first.start).min(MAX_ROW);
                self.original[..n].copy_from_slice(&buf[first.start..first.start + n]);
                buf[first.start..first.start + n].copy_from_slice(&self.base_data[..n]);
                self.original_bytes = n;
            }
        }
        self.process(buf, write_end, true);
    }

    /// Re-diff so the written bytes go back to being a valid LZ window, and
    /// clear the list -- keeping the tail of a table that runs past `write_end`.
    pub fn diff_tables(&mut self, buf: &mut [u8], write_start: usize, write_end: usize) {
        let mut carry_over: Option<TableEntry> = None;
        if let Some(&last) = self.tables.last() {
            if last.row != 0 && write_end >= last.start {
                let processed = (write_end - last.start) / last.row;
                if processed < last.len {
                    // Keep two extra rows: one as the undiff base for the next
                    // chunk, and one because a row can straddle the boundary.
                    let processed = processed.saturating_sub(2);
                    let mut tail = last;
                    tail.start += processed * last.row;
                    tail.len -= processed;
                    let n = tail.row.min(MAX_ROW);
                    self.base_data[..n].copy_from_slice(&buf[tail.start..tail.start + n]);
                    carry_over = Some(tail);
                }
            }
        }
        self.process(buf, write_end, false);
        // Put back the bytes undiff_tables borrowed for the base row.
        if self.original_bytes > 0 {
            if let Some(first) = self.tables.first().copied() {
                if first.start < write_start {
                    let n = self.original_bytes;
                    buf[first.start..first.start + n].copy_from_slice(&self.original[..n]);
                }
            }
        }
        self.original_bytes = 0;
        self.tables.clear();
        if let Some(t) = carry_over {
            self.tables.push(t);
        }
    }

    /// The window wrapped: move table positions back with it, and carry the few
    /// leading bytes that now sit before the origin -- undiffing the next chunk
    /// needs them, which is what `PAD_FOR_TABLES` reserves room for.
    pub fn shift(&mut self, buf: &mut [u8], old_pos: usize, new_pos: usize) {
        let delta = old_pos - new_pos;
        for e in self.tables.iter_mut() {
            let old = e.start;
            e.start -= delta;
            if new_pos > e.start {
                let n = new_pos - e.start;
                buf.copy_within(old..old + n, e.start);
            }
        }
    }
}

impl Default for DataTables {
    fn default() -> Self {
        Self::new()
    }
}
