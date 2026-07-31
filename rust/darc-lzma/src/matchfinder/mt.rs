//! Multi-threaded match finding — a port of `LzFindMt.c`, restricted to what
//! `LzmaEnc` can actually reach and re-arranged into two stages instead of three.
//!
//! ## What the C does, and what this does instead
//!
//! `LzFindMt.c` splits one match finder across **three** threads:
//!
//! * the *hash thread* (`HashThreadFunc`, `LzFindMt.c:440`) runs `GetHeads*`
//!   (`:296-435`) over the input ahead of everyone else. For each position it
//!   computes the main (`numHashBytes`-byte) hash index, reads `hash[index]`,
//!   writes `pos` back, and emits the **difference** `pos - hash[index]`
//!   (`GetHeads_LOOP`, `:304`);
//! * the *BT thread* (`BtThreadFunc` → `BtGetMatches`, `:571`) turns each of those
//!   differences back into an absolute position — `pos - p->hashBuf[...]`
//!   (`:672`) — and walks the binary tree with it, emitting `(len, dist)` pairs
//!   into `btBuf`;
//! * the *caller* (`MatchFinderMt_GetMatches`, `:1274`) reads `btBuf` and splices
//!   in the 2-/3-byte hash matches with `MixMatches2/3/4` (`:1007`, `:1031`,
//!   `:1169`), which own the `kFix3HashSize`-prefixed tables.
//!
//! **This port uses two stages, not three**, and draws the line one table further
//! along:
//!
//! * a *hash worker* (this module) owns the **whole** `hash` array — the 2-, 3-
//!   and main tables — and emits `(dm, d2, d3)` per position, all three as
//!   `pos - hash[...]` differences in the C's own convention;
//! * the *caller thread* keeps `son` and does the tree walk, which is
//!   `BtGetMatches` and `MixMatches` fused back together.
//!
//! Three reasons for the deviation, in order of weight:
//!
//! 1. **The tree walk is the critical path and cannot be split** — every position
//!    splices the tree, so it is inherently sequential. Whatever else moves, that
//!    stays. Two stages therefore already capture the whole available parallelism;
//!    a third thread would only subdivide the *other* side.
//! 2. Putting `MixMatches` on the worker takes two more random `hash` probes off
//!    the critical path rather than leaving them on it, which is the direction that
//!    helps. The C's three-way split exists partly because its BT thread must hand
//!    `btBuf` to a *separate* consumer anyway; here there is no such consumer.
//! 3. It keeps `hash` under exactly one owner, so normalization
//!    (`MatchFinder_Normalize3`) needs no cross-thread agreement about who has
//!    shifted what.
//!
//! ## What the third thread would buy, and what it would cost
//!
//! Measured, so that the trade-off is on record rather than re-argued: on an
//! otherwise idle machine this arrangement is worth about **7% on the finder**
//! (24 MB, BT4, 16 MiB dictionary: 9.24/9.23/9.16 s at one thread against
//! 8.74/8.56/8.57 s at two), which is roughly **4-5% of a whole encode** because
//! the finder is 63-74% of it. The C's own measurement is about 8% of the encode.
//!
//! The difference is not the hash split — it is that the C's caller thread runs
//! *only* `MixMatches` and the LZMA parser, so the **parser** overlaps the tree
//! walk as well. Here the parser and the tree walk share a thread, and the parser
//! is a quarter to a third of encode time.
//!
//! Closing that gap means a third stage that walks the tree ahead of the parser.
//! It is possible — `SkipMatchesSpec` and `GetMatchesSpec1` splice `son`
//! identically, differing only in whether they record, which is exactly why the
//! C's `MatchFinderMt*_Skip` can just step over a precomputed entry
//! (`SKIP_FOOTER_MT`, `LzFindMt.c:1321`) — but it is not free: the tree walker
//! must produce match lists for positions the parser will skip, so total CPU work
//! goes **up** and only wall-clock goes down. This port does not take that trade;
//! it costs no extra CPU at all.
//!
//! ## Why this cannot change the output
//!
//! Per position the single-threaded `Bt{2,3,4}_MatchFinder_GetMatches` does two
//! separable things: it updates `hash` and reads three values out of it, and it
//! walks and splices `son`. The first touches neither `son` nor anything the walk
//! writes; the second touches no `hash` entry. The `hash` half is still executed
//! strictly in position order (one worker, one pass), so it sees exactly the state
//! the single-threaded pass would, and the tree walk consumes exactly the values
//! the single-threaded pass would have read. The two paths are therefore the same
//! computation with a queue in the middle.
//!
//! ## Which positions the worker must process, and the invariant that pins it
//!
//! The single-threaded finder skips a position entirely when `len_limit <
//! numHashBytes` — `Bt4_MatchFinder_GetMatches` returns at `LzFind.c:1230` before
//! touching `hash`. `MatchFinder_SetLimits` makes `len_limit == min(avail, fb)`,
//! so with `fb >= numHashBytes` (checked by the caller before starting a worker)
//! that condition is exactly `avail < numHashBytes`, i.e. *there are fewer than
//! `numHashBytes` bytes left in the whole stream*.
//!
//! The worker's rule is the same statement from the other side: process position
//! `p` **iff** bytes `p .. p + numHashBytes - 1` exist. So it stops at exactly the
//! position the consumer stops asking about, without either side telling the
//! other. This is also why the worker needs no window: it never looks backwards,
//! only at `numHashBytes` bytes at the cursor, so a plain byte feed suffices and
//! the sliding-window hazard documented in the parent module does not arise here.
//!
//! The same invariant is what makes the feed deadlock-free: the consumer asks for
//! position `p` only when `avail >= numHashBytes`, which means the caller has
//! already *read* those bytes, which means it has already fed them.
//!
//! ## Failure
//!
//! This crate is reached across a C ABI compiled `-D_NO_EXCEPTIONS`, where an
//! unwind is undefined behaviour. Nothing here can unwind into the caller: the
//! worker's whole stack is Rust and a panic on it is contained by the thread
//! boundary, and every channel operation is matched rather than unwrapped. A dead
//! worker turns into a latched [`crate::stream::StreamError`] on the finder plus a
//! stream of "no candidate" answers, which is safe output that the encoder then
//! refuses — the same shape as the C's `p->failure_BT` (`LzFindMt.c:591`), which
//! also degrades to an empty match list rather than aborting.

use std::sync::mpsc::{Receiver, Sender, SyncSender, channel, sync_channel};
use std::thread::JoinHandle;

use super::hash::{FIX3_HASH_SIZE, HASH2_SIZE};
use super::hc;
use crate::props::MatchFinderKind;

/// Positions per batch handed across the queue.
///
/// The C's unit is `kMtHashBlockSize = 1 << 17` heads with `kMtHashNumBlocks = 2`
/// (`LzFindMt.c:34-35`), i.e. two blocks of 128 K single-`UInt32` heads in flight.
/// This carries three `u32` per position instead of one (it also delivers the 2-
/// and 3-byte tables' answers), so the batch is a quarter of the C's length to
/// land in the same order of bytes in flight.
const BATCH_POSITIONS: usize = 1 << 15;

/// Batches the worker may run ahead by, matching `kMtHashNumBlocks`
/// (`LzFindMt.c:35`).
///
/// This is the only bounded queue in the pair, and deliberately so: it is what
/// stops the worker from racing arbitrarily far ahead, and it is safe to block on
/// because the *consumer* never blocks writing to it.
const BATCH_QUEUE: usize = 2;

/// How long the consumer will wait for a batch before declaring the worker lost.
///
/// Not a tuning knob and not a timeout in the usual sense: the worker outruns the
/// tree walk by a wide margin, so a wait this long can only mean a protocol
/// defect. It exists because the alternative failure mode is an **unkillable
/// hang** inside a codec that `arc`, `unarc` and the SFX modules call across a C
/// ABI, and because a hang in CI reads as "slow" rather than "broken" — the first
/// version of the worker loop held a partial batch across `recv` and wedged every
/// test at 0% CPU for as long as it was left running.
///
/// Firing it costs a refused compression, never a wrong archive:
/// [`MtHash::next`] returns `None` and the finder latches an error.
const WATCHDOG: std::time::Duration = std::time::Duration::from_secs(120);

/// The three binary-tree finders — the only ones the multi-threaded finder serves.
///
/// `LzmaEnc.c:2695` gates multi-threading on `btMode != 0`, so the hash chains are
/// out of scope by construction. Modelling that as its own three-variant enum
/// rather than carrying [`MatchFinderKind`] means the worker's dispatch is total
/// without a catch-all arm that would silently run BT4's hashing for a chain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BtKind {
    /// Binary tree, 2-byte hash.
    Bt2,
    /// Binary tree, 3-byte hash.
    Bt3,
    /// Binary tree, 4-byte hash.
    Bt4,
}

impl BtKind {
    /// The binary-tree finder `kind` names, or `None` for a hash chain.
    pub(super) fn of(kind: MatchFinderKind) -> Option<Self> {
        match kind {
            MatchFinderKind::Bt2 => Some(BtKind::Bt2),
            MatchFinderKind::Bt3 => Some(BtKind::Bt3),
            MatchFinderKind::Bt4 => Some(BtKind::Bt4),
            MatchFinderKind::Hc4 | MatchFinderKind::Hc5 => None,
        }
    }

    /// `p->numHashBytes`, and hence how many bytes at the cursor the worker needs
    /// before it may process a position.
    fn num_hash_bytes(self) -> usize {
        match self {
            BtKind::Bt2 => 2,
            BtKind::Bt3 => 3,
            BtKind::Bt4 => 4,
        }
    }
}

/// One position's worth of hash-table output, as **distances** back from `pos`.
///
/// Distances rather than absolute positions, exactly as `GetHeads_LOOP` emits them
/// (`LzFindMt.c:304`) and `BtGetMatches` consumes them (`:672`). The difference is
/// invariant under `MatchFinder_Normalize3`, so producer and consumer cannot
/// disagree about the frame even for the one position where the shift lands
/// between them.
///
/// `d2` / `d3` are unused by the finders that have no such table (`Bt2` has
/// neither, `Bt3` has no 3-byte table) and are zero there; the consumer for those
/// finders does not read them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RawHeads {
    /// `pos - hash[main]` — the tree walk's `curMatch`, as a distance.
    pub dm: u32,
    /// `pos - hash[h2]`, the C's `d2` in `Bt3`/`Bt4`'s short-match prologue.
    pub d2: u32,
    /// `pos - hash[kFix3HashSize + h3]`, the C's `d3` in `Bt4`'s prologue.
    pub d3: u32,
}

/// Everything the worker needs; a snapshot, so nothing is shared but the queues.
pub(super) struct HashConfig {
    /// Which binary-tree finder's hashing to run.
    pub kind: BtKind,
    /// `hash` length, i.e. `fixedHashSize + hashMask + 1` (`LzFind.c:444-455`).
    pub hash_len: usize,
    /// `p->hashMask`, already narrowed by `expectedDataSize` where that applies
    /// (`LzFind.c:434-439`). Passed in rather than recomputed, so the worker
    /// cannot disagree with the finder that created it — that narrowing is
    /// invisible on the plain-LZMA path and decides collisions on LZMA2's.
    pub hash_mask: u32,
    /// `p->historySize`, needed only for the normalization shift.
    pub history_size: u32,
    /// `kMaxValForNormalize` (`LzFind.c:19`). Threaded through rather than read
    /// from the constant so that a test can put normalization within reach.
    pub max_val_for_normalize: u32,
    /// `p->crc`, built once by the finder.
    pub crc: [u32; 256],
}

/// Caller → worker. The bytes of the stream, in order, then a terminator.
enum Feed {
    /// The next run of input bytes, exactly as read from the caller's stream.
    Data(Vec<u8>),
    /// End of stream, or the caller shutting the worker down early.
    End,
}

/// The consumer's handle on a running hash worker.
pub(super) struct MtHash {
    /// Byte feed. **Unbounded, and that is load-bearing**: the consumer must never
    /// block writing here. It can only be blocked reading [`MtHash::recs`], and a
    /// worker blocked writing to `recs` is unblocked by the consumer reading it —
    /// so with a blocking feed the pair could deadlock, each waiting for the
    /// other's queue. Unboundedness costs nothing in practice because the caller
    /// feeds a run only after consuming records that cover the previous run, so at
    /// most two runs are ever outstanding.
    feed: Sender<Feed>,
    /// Record batches. `Option` only so that [`Drop`] can release it before
    /// joining, which is what unblocks a worker parked in `send`.
    recs: Option<Receiver<Vec<RawHeads>>>,
    /// The batch currently being drained.
    batch: Vec<RawHeads>,
    /// Read cursor into [`MtHash::batch`].
    next: usize,
    /// The worker has stopped and everything it produced has been consumed.
    ended: bool,
    /// `Option` so [`Drop`] can take it to join.
    handle: Option<JoinHandle<()>>,
}

impl MtHash {
    /// Start a worker, or `None` if the thread could not be spawned.
    ///
    /// A spawn failure is not an error: the caller keeps its own `hash` and runs
    /// the single-threaded path, which is what `LzmaEnc` does too when
    /// `MatchFinderMt_Create` fails (`LzmaEnc.c:2712` falls back to `MFB`).
    pub(super) fn new(cfg: HashConfig) -> Option<MtHash> {
        let (feed_tx, feed_rx) = channel::<Feed>();
        let (rec_tx, rec_rx) = sync_channel::<Vec<RawHeads>>(BATCH_QUEUE);
        let handle = std::thread::Builder::new()
            .name("darc-lzma-hash".to_owned())
            .spawn(move || hash_thread(cfg, &feed_rx, &rec_tx))
            .ok()?;
        Some(MtHash {
            feed: feed_tx,
            recs: Some(rec_rx),
            batch: Vec::new(),
            next: 0,
            ended: false,
            handle: Some(handle),
        })
    }

    /// Hand the worker the bytes just read into the window.
    ///
    /// A copy, because the window slides under `MatchFinder_MoveBlock` and the
    /// worker outlives any particular window layout. The C shares the window
    /// instead and pays for it with `MatchFinderMt`'s lock-and-repoint dance
    /// around `MatchFinder_MoveBlock` (`LzFindMt.c:465-479`); one `memcpy` of the
    /// input is cheaper than that, and it is the reason this port needs no locks
    /// at all.
    pub(super) fn feed(&mut self, data: &[u8]) {
        // A closed channel means the worker is gone; `next` will report that.
        let _ = self.feed.send(Feed::Data(data.to_vec()));
    }

    /// No more bytes will follow.
    pub(super) fn end(&mut self) {
        let _ = self.feed.send(Feed::End);
    }

    /// The next position's hash output, or `None` if the worker stopped early.
    ///
    /// `None` is a defect, not an end-of-stream: the consumer asks for exactly the
    /// positions the worker produces (see the module docs), so exhaustion means
    /// the worker died. The caller latches an error rather than guessing.
    //
    // `single_match` would rewrite the batch probe as `if let`, which this
    // workspace bans outright (see the totality gate in
    // `.github/workflows/build.yml`).
    #[allow(clippy::single_match)]
    pub(super) fn next(&mut self) -> Option<RawHeads> {
        loop {
            match self.batch.get(self.next) {
                Some(r) => {
                    let r = *r;
                    self.next += 1;
                    return Some(r);
                }
                None => {}
            }
            if self.ended {
                return None;
            }
            let got = match self.recs.as_ref() {
                Some(rx) => rx.recv_timeout(WATCHDOG).map_err(|_| ()),
                None => Err(()),
            };
            match got {
                // An empty batch would spin this loop forever. The worker never
                // sends one; treating it as the end regardless keeps termination a
                // property of this function rather than of the other thread.
                Ok(b) if b.is_empty() => {
                    self.ended = true;
                    return None;
                }
                Ok(b) => {
                    self.batch = b;
                    self.next = 0;
                }
                Err(_) => {
                    self.ended = true;
                    return None;
                }
            }
        }
    }
}

impl Drop for MtHash {
    // As above: `if let` is banned here.
    #[allow(clippy::single_match)]
    fn drop(&mut self) {
        // Both directions, because the worker may be parked in either one: `End`
        // releases a `recv`, dropping the receiver makes a parked `send` fail.
        let _ = self.feed.send(Feed::End);
        self.recs = None;
        match self.handle.take() {
            // A worker that panicked yields `Err` here, which is discarded rather
            // than resumed: re-raising it would unwind through whatever dropped
            // the finder, and that can be a C frame.
            Some(h) => {
                let _ = h.join();
            }
            None => {}
        }
    }
}

/// The worker: hash every position it has enough bytes for, in order, forever.
fn hash_thread(cfg: HashConfig, feed: &Receiver<Feed>, out: &SyncSender<Vec<RawHeads>>) {
    let nhb = cfg.kind.num_hash_bytes();
    // `kEmptyHashValue` is 0, so a zeroed table is an initialized one
    // (`MatchFinder_Init_HighHash`, called at `LzFindMt.c:457`).
    let mut hash = vec![0u32; cfg.hash_len];
    // `MatchFinder_Init_4`: "pos = 1, it's smallest optimal value".
    let mut pos: u32 = 1;
    let mut buf: Vec<u8> = Vec::new();
    let mut bp: usize = 0;
    let mut batch: Vec<RawHeads> = Vec::with_capacity(BATCH_POSITIONS);

    loop {
        while buf.len() - bp >= nhb {
            // `MatchFinder_CheckLimits` (`LzFind.c:833`), the `hash` half of it.
            // The consumer runs the identical test on the identical `pos` and
            // does the `son` half, so the two frames shift together at the same
            // stream position and the `dm` differences stay meaningful across it.
            // (The C instead normalizes each thread at its *own* position with
            // `kMtMaxValForNormalize = 0xFFFFFFFF`, `LzFindMt.c:286`/`:519`/`:660`
            // — a different trigger in each thread, which this port does not
            // reproduce because it would put producer and consumer in different
            // frames for the positions in between.)
            if pos == cfg.max_val_for_normalize {
                let sub = pos.wrapping_sub(cfg.history_size).wrapping_sub(1);
                pos = pos.wrapping_sub(sub);
                super::normalize3(sub, &mut hash);
            }
            batch.push(heads(&cfg, &buf, bp, &mut hash, pos));
            bp += 1;
            pos = pos.wrapping_add(1);
            if batch.len() == BATCH_POSITIONS {
                let full = std::mem::replace(&mut batch, Vec::with_capacity(BATCH_POSITIONS));
                match out.send(full) {
                    Ok(()) => {}
                    // Consumer gone. Nothing left to do and nothing to report.
                    Err(_) => return,
                }
            }
        }

        // Keep only the tail the next position still needs (< numHashBytes bytes).
        if bp != 0 {
            let keep = buf.len() - bp;
            buf.copy_within(bp.., 0);
            buf.truncate(keep);
            bp = 0;
        }

        // **Flush before parking.** The consumer's only source of records is this
        // channel, and it will not feed more bytes until it has advanced past the
        // positions a partial batch covers — so holding one while blocking on
        // `recv` is a deadlock, each side waiting on the other's queue. (Observed,
        // not theorised: the first version of this loop hung every test.)
        //
        // The C carries the same obligation and discharges it the same way: its
        // hash thread emits a block on *every* iteration, header-only when there
        // is nothing to hash (`LzFindMt.c:495-526`, and the comment at `:498-511`
        // spelling out the `{2, 0}` empty-block sequence).
        if !batch.is_empty() {
            let partial = std::mem::replace(&mut batch, Vec::with_capacity(BATCH_POSITIONS));
            match out.send(partial) {
                Ok(()) => {}
                Err(_) => return,
            }
        }

        match feed.recv() {
            Ok(Feed::Data(v)) => buf.extend_from_slice(&v),
            Ok(Feed::End) => break,
            Err(_) => break,
        }
    }

    if !batch.is_empty() {
        let _ = out.send(batch);
    }
}

/// One position: read the three tables, write `pos` back, report the differences.
///
/// This is `Bt{2,3,4}_MatchFinder_GetMatches`'s hash prologue verbatim — the reads
/// all happen before the writes, which matters whenever two of the indices
/// collide. (`h2` and `h3` cannot collide with each other or with the main table:
/// the tables are disjoint regions of `hash`. `Bt4`'s `h3` and the main index can
/// not collide either, for the same reason.)
#[inline(always)]
fn heads(cfg: &HashConfig, buf: &[u8], cur: usize, hash: &mut [u32], pos: u32) -> RawHeads {
    match cfg.kind {
        BtKind::Bt2 => {
            let hv = bt2_index(buf, cur);
            let dm = pos.wrapping_sub(hash[hv]);
            hash[hv] = pos;
            RawHeads { dm, d2: 0, d3: 0 }
        }
        BtKind::Bt3 => {
            let (h2i, hvi) = bt3_indices(&cfg.crc, buf, cur, cfg.hash_mask);
            let d2 = pos.wrapping_sub(hash[h2i]);
            let dm = pos.wrapping_sub(hash[hvi]);
            hash[h2i] = pos;
            hash[hvi] = pos;
            RawHeads { dm, d2, d3: 0 }
        }
        BtKind::Bt4 => {
            let (h2i, h3i, hvi) = bt4_indices(&cfg.crc, buf, cur, cfg.hash_mask);
            let d2 = pos.wrapping_sub(hash[h2i]);
            let d3 = pos.wrapping_sub(hash[h3i]);
            let dm = pos.wrapping_sub(hash[hvi]);
            hash[h2i] = pos;
            hash[h3i] = pos;
            hash[hvi] = pos;
            RawHeads { dm, d2, d3 }
        }
    }
}

/// `HASH2_CALC` (`LzFind.c:35`): `hv = GetUi16(cur)`.
///
/// A plain little-endian 16-bit load — **not** a CRC hash, and **not** masked with
/// `hash_mask`. It stays in range only because `MatchFinder_GetHashMask` returns
/// `0xFFFF` unconditionally for `numHashBytes == 2`, so the table is exactly
/// `1 << 16` entries. Deriving this from the 4-byte hash's machinery, or masking
/// it, assigns different buckets and changes the parse.
#[inline(always)]
pub(super) fn bt2_index(buf: &[u8], cur: usize) -> usize {
    (buf[cur] as usize) | ((buf[cur + 1] as usize) << 8)
}

/// `HASH3_CALC` (`LzFind.c:44`) → `(h2, main)`, both ready to index `hash`.
///
/// Note the main index is masked with `hash_mask`, whereas BT4's `h3` is masked
/// with `kHash3Size - 1`: different values out of the same `temp`, so one cannot
/// stand in for the other.
#[inline(always)]
pub(super) fn bt3_indices(
    crc: &[u32; 256],
    buf: &[u8],
    cur: usize,
    hash_mask: u32,
) -> (usize, usize) {
    let temp = crc[buf[cur] as usize] ^ buf[cur + 1] as u32;
    let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
    let hv = ((temp ^ ((buf[cur + 2] as u32) << 8)) & hash_mask) as usize;
    (h2, FIX3_HASH_SIZE + hv)
}

/// `HASH4_CALC` (`LzFind.c:49`) → `(h2, h3, main)`, all ready to index `hash`.
///
/// Delegates to [`hc::hash4_calc`] and applies the table offsets, so BT4 and HC4
/// cannot drift apart: they are the same macro in the C.
#[inline(always)]
pub(super) fn bt4_indices(
    crc: &[u32; 256],
    buf: &[u8],
    cur: usize,
    hash_mask: u32,
) -> (usize, usize, usize) {
    let (h2, h3, hv) = hc::hash4_calc(crc, buf, cur, hash_mask);
    (
        h2,
        FIX3_HASH_SIZE + h3,
        super::hash::FIX4_HASH_SIZE + hv,
    )
}
