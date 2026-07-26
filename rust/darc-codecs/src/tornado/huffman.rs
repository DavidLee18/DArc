//! Semi-adaptive Huffman codec, ported from
//! `Compression/Tornado/EntropyCoder.cpp` (`HuffmanTree` :278,
//! `HuffmanEncoder` :458, `HuffmanDecoder` :483).
//!
//! ## Why the tree builder has to be ported, not just the code reader
//!
//! Nothing about the tree travels in the stream. Both sides start from
//! `counter[s] = 1` for every symbol, increment the counter of each symbol as
//! it is coded, and rebuild the tree when the encoder emits an EOB code -- the
//! decoder's only signal is that EOB, plus three bits of rescale mode. So the
//! decoder reconstructs the same tree only if it reproduces `build_tree`
//! *exactly*: the same counters, the same tie-breaking, the same rescale. Any
//! divergence and the two sides silently disagree about what the next bits
//! mean.
//!
//! That makes the sort order load-bearing. The C sorts nodes by counter and
//! relies on the sort being **stable** for equal counters, which it gets by
//! packing counter and index into one integer (`make_node`) so that equal
//! counters fall back to comparing indices. Symbols with counters below
//! `CHUNKS` are placed by counting sort instead, which is stable by
//! construction. Both are reproduced here.
//!
//! ## Decoding
//!
//! Two tables. `fast_index` resolves any code of at most `FAST_BITS` bits from
//! the next `FAST_BITS` input bits directly; entries whose prefix belongs to a
//! longer code are marked -1, and those fall back to `index`, which is sized
//! `1 << maxbits` so it always resolves in one step.

use super::out_stream::OutputBitStream;
use super::stream::InputBitStream;

/// Maximum symbols in a tree (EntropyCoder.cpp:262).
pub const MAXHUF: usize = 2048;
/// Codes no longer than this decode straight out of `fast_index` (:263).
pub const FAST_BITS: u32 = 11;
/// Counters below this are placed by counting sort rather than a comparison
/// sort (`CHUNKS`, :341).
const CHUNKS: usize = 250;

/// One node while the tree is being built (`struct Node`, :266).
#[derive(Clone, Copy, Default)]
struct Node {
    cnt: u32,
    code: u32,
    left: u16,
    right: u16,
    bits: u8,
}

/// `CodecDirection` (EntropyCoder.cpp:14). The tree builder is shared; only the
/// last step differs -- an encoder wants `code[]` per symbol, a decoder wants the
/// two reverse-lookup tables.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Encoder,
    Decoder,
}

pub struct HuffmanTree {
    n: usize,
    dir: Direction,
    counter: Vec<u32>,
    /// Longest code in the current table; `index` is sized `1 << maxbits`.
    /// Decoder-only: the C never assigns it on the encoder path.
    pub maxbits: i32,
    pub bits: Vec<u8>,
    /// Encoder-only: the bit sequence for each symbol.
    pub code: Vec<u32>,
    fast_index: Vec<i32>,
    index: Vec<u16>,
}

impl HuffmanTree {
    /// Decoder-side construction: all counters start at 1 and the first tree is
    /// built immediately, matching `HuffmanTree::HuffmanTree` (:308).
    pub fn new(n: usize) -> Self {
        Self::with_direction(n, Direction::Decoder)
    }

    pub fn with_direction(n: usize, dir: Direction) -> Self {
        let mut t = HuffmanTree {
            n,
            dir,
            counter: vec![1u32; n],
            maxbits: 0,
            bits: vec![0u8; n],
            code: match dir {
                Direction::Encoder => vec![0u32; n],
                Direction::Decoder => Vec::new(),
            },
            fast_index: match dir {
                Direction::Encoder => Vec::new(),
                Direction::Decoder => vec![0i32; 1 << FAST_BITS],
            },
            index: Vec::new(),
        };
        t.build_tree(0);
        t
    }

    #[inline]
    pub fn inc(&mut self, s: usize) {
        self.counter[s] += 1;
    }

    /// `Decode` (:296): try the fast table, fall back to the full one.
    #[inline]
    pub fn decode_symbol(&self, code: u32) -> usize {
        let x = self.fast_index[(code & ((1 << FAST_BITS) - 1)) as usize];
        if x >= 0 {
            x as usize
        } else {
            self.index[code as usize] as usize
        }
    }

    /// `build_tree` (:334).
    ///
    /// Two sorted lists rather than a heap: original (symbol) nodes, and
    /// combined nodes which stay sorted because each new one is at least as
    /// large as the last. So the two smallest nodes are always among the first
    /// two of each list, and the merge is three cases.
    pub fn build_tree(&mut self, rescale_mode: u32) {
        let n = self.n;
        // The C allocates 2*MAXHUF+4 and fences the tail with INT_MAX.
        let mut buf = vec![Node::default(); 2 * MAXHUF + 8];

        // Counting sort for counters below CHUNKS; the rest go to a comparison
        // sort. `places` is a running histogram offset table.
        let mut places = [0usize; CHUNKS + 2];
        let mut b = 0usize; // count of nodes (all symbols, since counters start at 1)
        for i in 0..n {
            if (self.counter[i] as usize) < CHUNKS {
                places[self.counter[i] as usize + 1] += 1;
            }
            b += 1;
        }
        for i in 0..CHUNKS {
            places[i + 1] += places[i];
        }

        // `rest` holds (counter, index) packed so that sorting the packed value
        // sorts by counter and breaks ties by index -- the stability the
        // algorithm depends on. The C packs into `uint` via make_node and reuses
        // code[] as scratch; a u64 here keeps large counters from colliding.
        let mut rest: Vec<u64> = Vec::new();
        for i in 0..n {
            let c = self.counter[i] as usize;
            if c < CHUNKS {
                let p = places[c];
                places[c] += 1;
                buf[p].cnt = self.counter[i];
                buf[p].left = i as u16;
            } else {
                rest.push((self.counter[i] as u64) * (MAXHUF as u64) + i as u64);
            }
        }
        rest.sort_unstable();
        for (i, packed) in rest.iter().enumerate() {
            let p = i + places[CHUNKS];
            buf[p].cnt = (packed / MAXHUF as u64) as u32;
            buf[p].left = (packed % MAXHUF as u64) as u16;
        }
        // buf[0..b] now holds nodes stable-sorted by counter.

        for i in 0..b + 4 {
            buf[b + i].cnt = i32::MAX as u32; // fence
        }
        let mut p1 = 0usize; // first unused original node
        let mut p2 = b + 2; // first unused combined node
        let mut p3 = b + 2; // next combined node to create

        while !(p1 == b && p3 - p2 == 1) {
            if buf[p1 + 1].cnt < buf[p2].cnt {
                buf[p3].cnt = buf[p1].cnt + buf[p1 + 1].cnt;
                buf[p3].left = p1 as u16;
                buf[p3].right = (p1 + 1) as u16;
                p1 += 2;
            } else if buf[p1].cnt > buf[p2 + 1].cnt {
                buf[p3].cnt = buf[p2].cnt + buf[p2 + 1].cnt;
                buf[p3].left = p2 as u16;
                buf[p3].right = (p2 + 1) as u16;
                p2 += 2;
            } else {
                buf[p3].cnt = buf[p1].cnt + buf[p2].cnt;
                buf[p3].left = p1 as u16;
                buf[p3].right = p2 as u16;
                p1 += 1;
                p2 += 1;
            }
            p3 += 1;
        }

        // Walk back down assigning codes: left child gets the parent's code,
        // right child gets it with a 1 bit at the parent's depth.
        buf[p2].bits = 0;
        buf[p2].code = 0;
        let mut i = p2;
        while i >= b + 2 {
            let (l, r, pb, pc) = (
                buf[i].left as usize,
                buf[i].right as usize,
                buf[i].bits,
                buf[i].code,
            );
            buf[l].bits = pb + 1;
            buf[l].code = pc;
            buf[r].bits = pb + 1;
            buf[r].code = pc + (1 << pb);
            if i == 0 {
                break;
            }
            i -= 1;
        }

        // Encoder side: all it needs is bits/code indexed by symbol (:412).
        if self.dir == Direction::Encoder {
            for i in 0..b {
                let s = buf[i].left as usize;
                self.bits[s] = buf[i].bits;
                self.code[s] = buf[i].code;
            }
            self.rescale(rescale_mode);
            return;
        }

        // Decoder side: the least frequent symbol has the longest code, and it
        // sorted first, so buf[0].bits is maxbits.
        self.maxbits = buf[0].bits as i32;
        if self.index.len() < (1usize << self.maxbits) {
            self.index.resize(1usize << self.maxbits, 0);
        }
        for i in 0..b {
            let s = buf[i].left as usize;
            let sbits = buf[i].bits as u32;
            let scode = buf[i].code as usize;
            self.bits[s] = sbits as u8;
            if sbits <= FAST_BITS {
                for j in 0..(1usize << (FAST_BITS - sbits)) {
                    self.fast_index[scode + (j << sbits)] = s as i32;
                }
            } else {
                self.fast_index[scode & ((1 << FAST_BITS) - 1)] = -1;
                for j in 0..(1usize << (self.maxbits as u32 - sbits)) {
                    self.index[scode + (j << sbits)] = s as u16;
                }
            }
        }

        self.rescale(rescale_mode);
    }

    /// `rescale(FACTOR)` (:449). Counters are aged so the tree tracks recent
    /// statistics; the encoder does the same, so the factor must match exactly.
    fn rescale(&mut self, mode: u32) {
        let factor = match mode {
            0 => 2,
            1 => 3,
            2 => 4,
            3 => 6,
            4 => 8,
            5 => 10,
            6 => 12,
            7 => 16,
            // The mode arrives as three bits, so 0..7 is exhaustive; the C
            // switch simply falls through and rescales nothing otherwise.
            _ => return,
        };
        for s in 0..self.n {
            let c = self.counter[s];
            self.counter[s] -= if c > 1 && c < factor { 1 } else { c / factor };
        }
    }
}

/// `HUFBLOCKSIZE` (:455) -- symbols between tree rebuilds.
pub const HUFBLOCKSIZE: i32 = 5000;

/// The rescale mode the encoder always picks (:471). It travels in the stream as
/// three bits after every EOB, so the decoder never has to agree in advance --
/// but both sides must age counters by the same factor afterwards.
pub const ENCODER_RESCALE_MODE: u32 = 3;

/// `HuffmanEncoder<EOB>` (:458).
pub struct HuffmanEncoder {
    pub huf: HuffmanTree,
    eob: usize,
    /// Symbols left before the next rebuild. The first block is a quarter length
    /// (:465), so the initial flat tree is replaced quickly.
    remainder: i32,
}

impl HuffmanEncoder {
    pub fn new(n: usize, eob: usize) -> Self {
        HuffmanEncoder {
            huf: HuffmanTree::with_direction(n, Direction::Encoder),
            eob,
            remainder: HUFBLOCKSIZE / 4,
        }
    }

    /// Encode one symbol and count it. On a rebuild the EOB code goes out first,
    /// followed by the three-bit rescale mode, which is how the decoder learns
    /// to rebuild at the same point.
    pub fn encode(&mut self, bits: &mut OutputBitStream, x: usize) {
        self.remainder -= 1;
        if self.remainder == 0 {
            let eob = self.eob;
            bits.putbits(self.huf.bits[eob] as i32, self.huf.code[eob]);
            bits.putbits(3, ENCODER_RESCALE_MODE);
            self.huf.build_tree(ENCODER_RESCALE_MODE);
            self.remainder = HUFBLOCKSIZE;
        }
        self.huf.inc(x);
        bits.putbits(self.huf.bits[x] as i32, self.huf.code[x]);
    }
}

/// `HuffmanDecoder<EOB>` (:483).
pub struct HuffmanDecoder {
    pub huf: HuffmanTree,
    eob: usize,
}

impl HuffmanDecoder {
    pub fn new(n: usize, eob: usize) -> Self {
        HuffmanDecoder { huf: HuffmanTree::new(n), eob }
    }

    /// Decode one symbol, rebuilding the tree whenever EOB appears. The three
    /// bits after an EOB carry the rescale mode the encoder used.
    pub fn decode(&mut self, bits: &mut InputBitStream) -> usize {
        loop {
            let need = bits.needbits(self.huf.maxbits);
            let x = self.huf.decode_symbol(need);
            bits.dumpbits(self.huf.bits[x] as i32);
            if x != self.eob {
                self.huf.inc(x);
                return x;
            }
            let mode = bits.getbits(3);
            self.huf.build_tree(mode);
            // A corrupt stream can hand us EOB forever; the caller's error
            // check on the byte stream bounds that, because each rebuild reads
            // three more bits and the stream reports exhaustion.
            if bits.error().is_some() {
                return self.eob;
            }
        }
    }
}
