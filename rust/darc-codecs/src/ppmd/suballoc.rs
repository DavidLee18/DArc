//! PPMd var.H's memory suballocator, ported from `Compression/PPMD/SubAlloc.hpp`.
//!
//! # This is not an allocator you may improve
//!
//! Every other codec in this crate leaves some freedom in how a stage is
//! implemented, so long as the bytes match. libsais was the extreme case: a
//! string's suffix array is unique, so *any* correct construction reproduced
//! the C exactly.
//!
//! PPMd is the opposite, and it is the allocator that makes it so. The model
//! branches on the allocator's own state:
//!
//! ```text
//! Model.cpp:245  GetUsedMemory() < (SubAllocatorSize >> 1)   decides restart
//! Model.cpp:416  if (pText >= UnitsStart) goto RESTART_MODEL
//! Model.cpp:418  if ((BYTE*) FSuccessor < UnitsStart)
//! ```
//!
//! Measured before this was written: the same 200 KB input at order 16 encodes
//! to 204797 / 205303 / 206007 / 206098 bytes under a 1 / 2 / 4 / 8 MB budget,
//! all four with different contents. An allocator that is merely *correct*
//! reports a different `GetUsedMemory()`, crosses `UnitsStart` at a different
//! moment, restarts the model somewhere else, and changes every byte after
//! that. Free-list order, split behaviour and the resulting addresses are all
//! part of the compressed format.
//!
//! So this is a transliteration, deliberately keeping the C's shape -- including
//! its `Stamp == !0` sentinel and its `p + p->NU` block arithmetic -- rather
//! than a Rust-idiomatic rewrite.
//!
//! # Representation
//!
//! The C works in raw pointers into one `malloc`ed block. Here the heap is a
//! `Vec<u8>` and every pointer is a byte offset into it, which is what the C
//! already does for intra-heap references: `RP_BLK` stores `p - HeapStart + 1`
//! so that 0 can mean NULL. Offsets are `usize` internally and `u32` on the
//! heap, matching the C's 4-byte `BLKREF` so `MEM_BLK` stays exactly
//! `UNIT_SIZE` bytes.

pub const UNIT_SIZE: usize = 12;
const N1: usize = 4;
const N2: usize = 4;
const N3: usize = 4;
const N4: usize = (128 + 3 - N1 - 2 * N2 - 3 * N3) / 4;
pub const N_INDEXES: usize = N1 + N2 + N3 + N4;

/// The C's `~0U` free-block marker, written into `MEM_BLK::Stamp`.
const STAMP_FREE: u32 = !0u32;

/// `U2B(NU)`: units to bytes. The C spells it `8*NU + 4*NU`.
#[inline]
fn u2b(nu: usize) -> usize {
    12 * nu
}

/// One free-list head. These live OUTSIDE the heap in the C too (`BList` is a
/// file-scope array), holding refs that point into it.
#[derive(Clone, Copy, Default)]
struct BlkNode {
    stamp: u32,
    next: u32,
}

pub struct SubAllocator {
    heap: Vec<u8>,
    blist: [BlkNode; N_INDEXES],
    indx2units: [u8; N_INDEXES],
    units2indx: [u8; 128],

    pub sub_allocator_size: usize,
    pub glue_count: u32,

    /// All four are byte offsets from the start of `heap`, matching the C's
    /// `pText` / `UnitsStart` / `LoUnit` / `HiUnit` pointers.
    pub p_text: usize,
    pub units_start: usize,
    pub lo_unit: usize,
    pub hi_unit: usize,
}

impl SubAllocator {
    pub fn new() -> Self {
        let mut s = SubAllocator {
            heap: Vec::new(),
            blist: [BlkNode::default(); N_INDEXES],
            indx2units: [0; N_INDEXES],
            units2indx: [0; 128],
            sub_allocator_size: 0,
            glue_count: 0,
            p_text: 0,
            units_start: 0,
            lo_unit: 0,
            hi_unit: 0,
        };
        s.build_tables();
        s
    }

    /// `Model.cpp:99-105`. Unit sizes grow by 1, then 2, then 3, then 4.
    fn build_tables(&mut self) {
        let mut i = 0usize;
        let mut k = 1usize;
        while i < N1 {
            self.indx2units[i] = k as u8;
            i += 1;
            k += 1;
        }
        k += 1;
        while i < N1 + N2 {
            self.indx2units[i] = k as u8;
            i += 1;
            k += 2;
        }
        k += 1;
        while i < N1 + N2 + N3 {
            self.indx2units[i] = k as u8;
            i += 1;
            k += 3;
        }
        k += 1;
        while i < N1 + N2 + N3 + N4 {
            self.indx2units[i] = k as u8;
            i += 1;
            k += 4;
        }
        // `i += (Indx2Units[i] < k+1)` -- note the C reuses `i` as the running
        // index into indx2units here, not the loop counter.
        let mut idx = 0usize;
        for kk in 0..128usize {
            if (self.indx2units[idx] as usize) < kk + 1 {
                idx += 1;
            }
            self.units2indx[kk] = idx as u8;
        }
    }

    // --- raw heap access -------------------------------------------------
    // Offsets are 0-based into `heap`; heap REFS are 1-based (0 = NULL), as
    // `RP_BLK`/`PP_BLK` do.

    #[inline]
    fn rd32(&self, at: usize) -> u32 {
        u32::from_ne_bytes([
            self.heap[at],
            self.heap[at + 1],
            self.heap[at + 2],
            self.heap[at + 3],
        ])
    }

    #[inline]
    fn wr32(&mut self, at: usize, v: u32) {
        self.heap[at..at + 4].copy_from_slice(&v.to_ne_bytes());
    }

    #[inline]
    fn ref_of(&self, off: usize) -> u32 {
        // RP_BLK: 0 stays 0, otherwise offset+1.
        (off + 1) as u32
    }

    #[inline]
    fn off_of(&self, r: u32) -> usize {
        (r - 1) as usize
    }

    // A MEM_BLK at offset `p` is { Stamp:u32, next:u32, NU:u32 }.
    #[inline]
    fn blk_stamp(&self, p: usize) -> u32 {
        self.rd32(p)
    }
    #[inline]
    fn set_blk_stamp(&mut self, p: usize, v: u32) {
        self.wr32(p, v)
    }
    #[inline]
    fn blk_next(&self, p: usize) -> u32 {
        self.rd32(p + 4)
    }
    #[inline]
    fn set_blk_next(&mut self, p: usize, v: u32) {
        self.wr32(p + 4, v)
    }
    #[inline]
    fn blk_nu(&self, p: usize) -> u32 {
        self.rd32(p + 8)
    }
    #[inline]
    fn set_blk_nu(&mut self, p: usize, v: u32) {
        self.wr32(p + 8, v)
    }

    /// `BList[i].Stamp`, which `RestoreModelRare` reads directly:
    /// `GlueCount += !(BList[1].Stamp & 1)`.
    pub fn blist_stamp(&self, i: usize) -> u32 {
        self.blist[i].stamp
    }

    pub fn heap(&self) -> &[u8] {
        &self.heap
    }
    pub fn heap_mut(&mut self) -> &mut [u8] {
        &mut self.heap
    }

    // --- lifecycle -------------------------------------------------------

    /// `StartSubAllocator`. Returns false when the allocation fails, as the C's
    /// `malloc` check does.
    pub fn start(&mut self, t: usize) -> bool {
        if self.sub_allocator_size == t {
            return true;
        }
        self.stop();
        // The C mallocs without zeroing; the model never reads an uninitialised
        // byte, but zeroing keeps this port deterministic under a debugger.
        self.heap = vec![0u8; t];
        self.sub_allocator_size = t;
        true
    }

    pub fn stop(&mut self) {
        if self.sub_allocator_size != 0 {
            self.sub_allocator_size = 0;
            self.heap = Vec::new();
        }
    }

    /// `InitSubAllocator`. The 1/8 : 7/8 split between the text area and the
    /// units area is what `pText >= UnitsStart` later tests against.
    pub fn init(&mut self) {
        self.blist = [BlkNode::default(); N_INDEXES];
        self.p_text = 0;
        self.hi_unit = self.sub_allocator_size;
        let diff = UNIT_SIZE * (self.sub_allocator_size / 8 / UNIT_SIZE * 7);
        self.lo_unit = self.hi_unit - diff;
        self.units_start = self.lo_unit;
        self.glue_count = 0;
    }

    /// `GetUsedMemory`. The model branches on this directly, so the free-list
    /// bookkeeping it subtracts has to match the C exactly -- including where
    /// it goes negative.
    ///
    /// It genuinely does. `BList[i].Stamp` counts the blocks in list `i` and
    /// this charges each of them `Indx2Units[i]` units, but GlueFreeBlocks
    /// inserts remainders whose real size is SMALLER than their list's nominal
    /// one, so the free-list total can exceed what is actually free. Seed 1 on
    /// a 2 KB heap reaches it after 124 operations of the allocator harness.
    ///
    /// The C absorbs that in `DWORD`: the initial expression is evaluated in
    /// signed 64-bit (the pointer differences drag it there), truncated to 32
    /// bits by the assignment, and each loop subtraction then wraps in 32 bits.
    /// Width matters as much as wrapping -- the model compares the result
    /// against `SubAllocatorSize`, and a 64-bit wrap would give a different
    /// answer to a 32-bit one on the very comparison that decides a restart.
    pub fn get_used_memory(&self) -> u32 {
        let mut ret = (self.sub_allocator_size as i64
            - (self.hi_unit as i64 - self.lo_unit as i64)
            - (self.units_start as i64 - self.p_text as i64)) as u32;
        for i in 0..N_INDEXES {
            ret = ret.wrapping_sub(
                UNIT_SIZE as u32 * self.indx2units[i] as u32 * self.blist[i].stamp,
            );
        }
        ret
    }

    // --- free lists ------------------------------------------------------

    #[inline]
    fn avail(&self, i: usize) -> bool {
        self.blist[i].next != 0
    }

    /// `BLK_NODE::insert` -- link the block in and stamp it free.
    fn insert(&mut self, i: usize, pv: usize, nu: usize) {
        let head_next = self.blist[i].next;
        self.set_blk_next(pv, head_next);
        self.blist[i].next = self.ref_of(pv);
        self.set_blk_stamp(pv, STAMP_FREE);
        self.set_blk_nu(pv, nu as u32);
        self.blist[i].stamp += 1;
    }

    /// `BLK_NODE::remove` -- unlink the head block and return its offset.
    fn remove(&mut self, i: usize) -> usize {
        let p = self.off_of(self.blist[i].next);
        self.blist[i].next = self.blk_next(p);
        self.blist[i].stamp -= 1;
        p
    }

    /// `SplitBlock`. The odd-sized remainder is inserted first, then the rest.
    fn split_block(&mut self, pv: usize, old_indx: usize, new_indx: usize) {
        let mut u_diff =
            (self.indx2units[old_indx] - self.indx2units[new_indx]) as usize;
        let mut p = pv + u2b(self.indx2units[new_indx] as usize);
        let mut i = self.units2indx[u_diff - 1] as usize;
        if self.indx2units[i] as usize != u_diff {
            i -= 1;
            let k = self.indx2units[i] as usize;
            self.insert(i, p, k);
            p += u2b(k);
            u_diff -= k;
        }
        let j = self.units2indx[u_diff - 1] as usize;
        self.insert(j, p, u_diff);
    }

    /// `GlueFreeBlocks`: coalesce adjacent free blocks, then redistribute.
    ///
    /// The `p + p->NU` walk is block arithmetic on `MEM_BLK*`, i.e. `NU * 12`
    /// bytes, and it relies on the `Stamp == ~0U` marker sitting in the heap
    /// where a neighbouring free block would start.
    fn glue_free_blocks(&mut self) {
        // The C writes a 0 byte at LoUnit so the coalescing walk cannot run off
        // the end into whatever happens to look like a free stamp.
        if self.lo_unit != self.hi_unit {
            self.heap[self.lo_unit] = 0;
        }

        // `s0` is a stack-local list head in the C; here it is a plain chain of
        // heap offsets, since only its `next` is used.
        let mut s0_next: u32 = 0;
        let mut p0: Option<usize> = None;

        for i in 0..N_INDEXES {
            while self.avail(i) {
                let p = self.remove(i);
                if self.blk_nu(p) == 0 {
                    continue;
                }
                // Absorb every immediately-following free block.
                loop {
                    let p1 = p + self.blk_nu(p) as usize * UNIT_SIZE;
                    if p1 + UNIT_SIZE > self.heap.len() || self.blk_stamp(p1) != STAMP_FREE {
                        break;
                    }
                    // The one deliberate deviation from the C, and it is not
                    // observable. Absorbing a block clears its NU but leaves
                    // its Stamp reading STAMP_FREE, so the heap keeps husks:
                    // twelve bytes that still say "free" and claim zero units.
                    // When a later block's end lands on one, the C's loop is
                    //     p->NU += p1->NU;  p1->NU = 0;
                    // which adds zero and rewrites zero, so `p + p->NU` picks
                    // out the same husk on the next turn -- for ever. Measured
                    // on the allocator harness: seed 42, an 8 KB heap, both the
                    // C and a faithful port spin at operation 146, and the
                    // husks they spin on were zeroed by an earlier absorb in
                    // the same run.
                    //
                    // Stopping here cannot change any output the C can produce,
                    // because in this state the C never leaves the loop and so
                    // never returns a byte at all. Everything the C *does*
                    // return is reached without meeting a husk.
                    if self.blk_nu(p1) == 0 {
                        break;
                    }
                    let merged = self.blk_nu(p) + self.blk_nu(p1);
                    self.set_blk_nu(p, merged);
                    self.set_blk_nu(p1, 0);
                }
                // p0->link(p)
                match p0 {
                    None => s0_next = self.ref_of(p),
                    Some(prev) => {
                        let n = self.blk_next(prev);
                        self.set_blk_next(p, n);
                        self.set_blk_next(prev, self.ref_of(p));
                    }
                }
                if p0.is_none() {
                    self.set_blk_next(p, 0);
                }
                p0 = Some(p);
            }
        }

        // Redistribute the coalesced blocks back into the free lists.
        while s0_next != 0 {
            let mut p = self.off_of(s0_next);
            s0_next = self.blk_next(p);
            let mut sz = self.blk_nu(p) as usize;
            if sz == 0 {
                continue;
            }
            while sz > 128 {
                self.insert(N_INDEXES - 1, p, 128);
                sz -= 128;
                p += 128 * UNIT_SIZE;
            }
            let mut i = self.units2indx[sz - 1] as usize;
            if self.indx2units[i] as usize != sz {
                i -= 1;
                let k = sz - self.indx2units[i] as usize;
                self.insert(k - 1, p + (sz - k) * UNIT_SIZE, k);
            }
            let nu = self.indx2units[i] as usize;
            self.insert(i, p, nu);
        }
        self.glue_count = 1 << 13;
    }

    /// `AllocUnitsRare`. Returns 0 for NULL, matching the C's `NULL` return --
    /// offset 0 is inside the text area and never a valid unit allocation.
    fn alloc_units_rare(&mut self, indx: usize) -> usize {
        let mut i = indx;
        if self.glue_count == 0 {
            self.glue_free_blocks();
            if self.avail(indx) {
                return self.remove(indx);
            }
        }
        loop {
            i += 1;
            if i == N_INDEXES {
                self.glue_count = self.glue_count.wrapping_sub(1);
                let need = u2b(self.indx2units[indx] as usize);
                return if self.units_start - self.p_text > need {
                    self.units_start -= need;
                    self.units_start
                } else {
                    0
                };
            }
            if self.avail(i) {
                break;
            }
        }
        let ret = self.remove(i);
        self.split_block(ret, i, indx);
        ret
    }

    /// `AllocUnits`. 0 means NULL.
    pub fn alloc_units(&mut self, nu: usize) -> usize {
        let indx = self.units2indx[nu - 1] as usize;
        if self.avail(indx) {
            return self.remove(indx);
        }
        let ret = self.lo_unit;
        let sz = u2b(self.indx2units[indx] as usize);
        self.lo_unit += sz;
        if self.lo_unit <= self.hi_unit {
            return ret;
        }
        self.lo_unit -= sz;
        self.alloc_units_rare(indx)
    }

    /// `AllocContext`. 0 means NULL.
    pub fn alloc_context(&mut self) -> usize {
        if self.hi_unit != self.lo_unit {
            self.hi_unit -= UNIT_SIZE;
            return self.hi_unit;
        }
        if self.avail(0) {
            return self.remove(0);
        }
        self.alloc_units_rare(0)
    }

    /// `UnitsCpy`: three words per unit, copied forwards.
    fn units_cpy(&mut self, dest: usize, src: usize, nu: usize) {
        let n = nu * UNIT_SIZE;
        self.heap.copy_within(src..src + n, dest);
    }

    /// `ExpandUnits`. 0 means NULL.
    pub fn expand_units(&mut self, old_ptr: usize, old_nu: usize) -> usize {
        let i0 = self.units2indx[old_nu - 1] as usize;
        let i1 = self.units2indx[old_nu] as usize;
        if i0 == i1 {
            return old_ptr;
        }
        let ptr = self.alloc_units(old_nu + 1);
        if ptr != 0 {
            self.units_cpy(ptr, old_ptr, old_nu);
            self.insert(i0, old_ptr, old_nu);
        }
        ptr
    }

    /// `ShrinkUnits`.
    pub fn shrink_units(&mut self, old_ptr: usize, old_nu: usize, new_nu: usize) -> usize {
        let i0 = self.units2indx[old_nu - 1] as usize;
        let i1 = self.units2indx[new_nu - 1] as usize;
        if i0 == i1 {
            return old_ptr;
        }
        if self.avail(i1) {
            let ptr = self.remove(i1);
            self.units_cpy(ptr, old_ptr, new_nu);
            let nu = self.indx2units[i0] as usize;
            self.insert(i0, old_ptr, nu);
            ptr
        } else {
            self.split_block(old_ptr, i0, i1);
            old_ptr
        }
    }

    /// `FreeUnits`.
    pub fn free_units(&mut self, ptr: usize, nu: usize) {
        let indx = self.units2indx[nu - 1] as usize;
        let n = self.indx2units[indx] as usize;
        self.insert(indx, ptr, n);
    }

    /// `SpecialFreeUnit`. Freeing the unit at `UnitsStart` grows the text area
    /// instead of listing it -- one of the places allocator layout feeds back
    /// into the model.
    pub fn special_free_unit(&mut self, ptr: usize) {
        if ptr != self.units_start {
            self.insert(0, ptr, 1);
        } else {
            self.wr32(ptr, STAMP_FREE);
            self.units_start += UNIT_SIZE;
        }
    }

    /// `MoveUnitsUp`.
    pub fn move_units_up(&mut self, old_ptr: usize, nu: usize) -> usize {
        let indx = self.units2indx[nu - 1] as usize;
        // The C compares the raw pointer against the free-list head pointer.
        // With 1-based refs and NULL == 0, `PP_BLK(next)` is 0 for an empty
        // list, so `OldPtr > NULL` is true and the early return fires -- which
        // is what keeps this from removing off an empty list.
        let head = self.blist[indx].next;
        let head_off = if head == 0 { 0usize } else { self.off_of(head) };
        if old_ptr > self.units_start + 16 * 1024 || old_ptr > head_off {
            return old_ptr;
        }
        let ptr = self.remove(indx);
        self.units_cpy(ptr, old_ptr, nu);
        let n = self.indx2units[indx] as usize;
        if old_ptr != self.units_start {
            self.insert(indx, old_ptr, n);
        } else {
            self.units_start += u2b(n);
        }
        ptr
    }

    /// `ExpandTextArea`: reclaim free blocks sitting at the bottom of the units
    /// area back into the text area.
    pub fn expand_text_area(&mut self) {
        let mut count = [0u32; N_INDEXES];
        while self.blk_stamp(self.units_start) == STAMP_FREE {
            let nu = self.blk_nu(self.units_start) as usize;
            let idx = self.units2indx[nu - 1] as usize;
            count[idx] += 1;
            self.set_blk_stamp(self.units_start, 0);
            self.units_start += nu * UNIT_SIZE;
        }
        for i in 0..N_INDEXES {
            if count[i] == 0 {
                continue;
            }
            // Walk the list, unlinking the entries that were just zero-stamped.
            // `p` starts at the list HEAD, which lives outside the heap, so the
            // head case is handled separately from the in-heap nodes.
            loop {
                let head = self.blist[i].next;
                if head == 0 {
                    break;
                }
                let head_off = self.off_of(head);
                if self.blk_stamp(head_off) != 0 {
                    break;
                }
                self.blist[i].next = self.blk_next(head_off);
                self.blist[i].stamp -= 1;
                count[i] -= 1;
                if count[i] == 0 {
                    break;
                }
            }
            let mut p = self.blist[i].next;
            while count[i] != 0 && p != 0 {
                let p_off = self.off_of(p);
                loop {
                    let nx = self.blk_next(p_off);
                    if nx == 0 {
                        break;
                    }
                    let nx_off = self.off_of(nx);
                    if self.blk_stamp(nx_off) != 0 {
                        break;
                    }
                    self.set_blk_next(p_off, self.blk_next(nx_off));
                    self.blist[i].stamp -= 1;
                    count[i] -= 1;
                    if count[i] == 0 {
                        break;
                    }
                }
                p = self.blk_next(p_off);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The derived constants, which everything else is sized against.
    #[test]
    fn constants_match_the_c() {
        assert_eq!(UNIT_SIZE, 12);
        // N4 = (128 + 3 - 4 - 8 - 12) / 4 = 107 / 4 = 26
        assert_eq!(N4, 26);
        assert_eq!(N_INDEXES, 38);
    }

    /// `Indx2Units` / `Units2Indx` are pure functions of the constants, so they
    /// can be checked without the C: sizes rise 1,2,3,4 per group, and the
    /// index table maps each size to the first unit class that fits it.
    #[test]
    fn index_tables_are_consistent() {
        let a = SubAllocator::new();
        assert_eq!(a.indx2units[0], 1);
        assert_eq!(a.indx2units[N1 - 1], 4);
        // Every size 1..=128 must map to a class at least that large.
        for k in 1..=128usize {
            let idx = a.units2indx[k - 1] as usize;
            assert!(
                a.indx2units[idx] as usize >= k,
                "size {k} mapped to class {idx} of {} units",
                a.indx2units[idx]
            );
            if idx > 0 {
                assert!(
                    (a.indx2units[idx - 1] as usize) < k,
                    "size {k} should have fit class {}",
                    idx - 1
                );
            }
        }
    }

    #[test]
    fn init_splits_the_heap_one_eighth_to_text() {
        let mut a = SubAllocator::new();
        assert!(a.start(1 << 20));
        a.init();
        assert_eq!(a.p_text, 0);
        assert_eq!(a.hi_unit, 1 << 20);
        assert_eq!(a.lo_unit, a.units_start);
        // The units area is 7/8 of the heap, rounded to whole units.
        let diff = UNIT_SIZE * ((1usize << 20) / 8 / UNIT_SIZE * 7);
        assert_eq!(a.units_start, (1 << 20) - diff);
        // Nothing is allocated yet, so used memory is 0: the two subtracted
        // spans -- the untouched units area (HiUnit-LoUnit) and the whole text
        // area (UnitsStart-pText) -- cover the heap exactly.
        assert_eq!(a.get_used_memory(), 0);
    }

    #[test]
    fn alloc_and_free_round_trip_through_the_free_lists() {
        let mut a = SubAllocator::new();
        assert!(a.start(1 << 20));
        a.init();
        let before = a.get_used_memory();
        let p = a.alloc_units(3);
        assert_ne!(p, 0);
        assert!(a.get_used_memory() > before);
        a.free_units(p, 3);
        // Freeing returns the block to a list, which get_used_memory subtracts.
        assert_eq!(a.get_used_memory(), before);
        // The next same-sized request must reuse it, not bump LoUnit.
        let q = a.alloc_units(3);
        assert_eq!(q, p, "a freed block should be handed straight back");
    }

    #[test]
    fn contexts_are_allocated_downwards_from_the_top() {
        let mut a = SubAllocator::new();
        assert!(a.start(1 << 20));
        a.init();
        let c1 = a.alloc_context();
        let c2 = a.alloc_context();
        assert_eq!(c1, (1 << 20) - UNIT_SIZE);
        assert_eq!(c2, c1 - UNIT_SIZE);
    }
}
