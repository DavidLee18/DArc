//! REP decoder, ported from `Compression/REP/rep.cpp` (`rep_decompress`).
//!
//! REP is a huge-dictionary LZ preprocessor. Its block format is explicit
//! rather than bit-packed: a block is a count `num`, three parallel tables
//! (`lens`, `offsets`, each `num` entries; `datalens`, `num+1`), then the
//! literal bytes those tables interleave with matches. Decoding is therefore a
//! straight walk, which is why the decoder ports cleanly ahead of the encoder
//! (the same decode-first order used for Dict).
//!
//! Output goes into a circular buffer of `BlockSize`; a match offset that would
//! reach before the buffer start wraps by subtracting `BlockSize`. The
//! wraparound only ever happens at a block boundary, so a block's decoded bytes
//! are always contiguous in the buffer.
//!
//! Every length and offset read from the stream is untrusted -- the decoder is
//! fed raw archive bytes and runs on `arc t` -- so each is validated against
//! the remaining input and output before use, mirroring the bounds the C added
//! during the v2.0.0 hardening. A single flipped byte in a `-mrep` archive
//! reaches these checks.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO,
                 FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, OK};
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;
const NOMEM: c_int = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY as c_int;

/// Read exactly `buf.len()` bytes, or return an error. The C `READ`/`READ4`
/// macros require the callback to fill the whole request; a short read is EOF
/// or an I/O error, never a partial success.
fn read_exact(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.read(buf) {
        n if n as usize == buf.len() => Ok(()),
        n if n >= 0 => Err(IO), // short read where the format demands a full one
        n => Err(n),
    }
}

fn read_u32(io: &Io) -> Result<u32, c_int> {
    let mut b = [0u8; 4];
    read_exact(io, &mut b)?;
    Ok(u32::from_le_bytes(b))
}

fn i32_at(buf: &[u8], off: usize) -> i32 {
    i32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}

/// Decode a REP stream.
///
/// `declared_block_size` is the block size from the METHOD STRING, and it is
/// the only trustworthy bound available here: everything else this function
/// reads comes out of the stream. It used to be accepted and dropped on the
/// floor, on the reasoning that "the block size that matters is stored in the
/// stream" -- true for decoding, and exactly why the stored one cannot also be
/// its own sanity check.
///
/// `dict` and `lzp` have always threaded theirs through for this reason; rep
/// was simply missed when [`crate::ffi::archive_sized_buffer`] was introduced.
pub fn decompress(io: &Io, declared_block_size: u32) -> c_int {
    match run(io, declared_block_size) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io, declared_block_size: u32) -> Result<(), c_int> {
    // The real dictionary size is the first word of the stream -- so it is
    // attacker-controlled, and `vec![0u8; n]` is infallible. A stream claiming
    // a 4 GiB dictionary used to get one attempted: harmless where the OS hands
    // back lazy zero pages, an abort through `handle_alloc_error` under strict
    // overcommit, a cgroup limit, or a 32-bit target.
    let stream_block_size = read_u32(io)? as usize;
    if stream_block_size == 0 {
        return Err(BAD);
    }
    let mut data = crate::ffi::archive_sized_buffer(stream_block_size, declared_block_size)?;
    let block_size = data.len();
    let mut pos: usize = 0; // current write index into `data` (the circular buffer)

    loop {
        let compr_size = read_u32(io)? as i32;
        if compr_size == 0 {
            break; // EOF marker
        }
        // Smallest legal block is `num` plus `datalens[0]`: two int32s.
        if compr_size < 2 * 4 {
            return Err(BAD);
        }
        let compr_size = compr_size as usize;

        // Bounded for the same reason, against the same figure: the encoder
        // compresses `block_size` bytes at a time, so a compressed block far
        // past it is corrupt input rather than a big block.
        let mut buf = crate::ffi::archive_sized_buffer(compr_size, declared_block_size)?;
        read_exact(io, &mut buf)?;

        // Header: num, then lens[num], offsets[num], datalens[num+1]. num sizes
        // three tables plus itself: 4*(3*num+2) bytes, checked in 64-bit before
        // any table offset is derived so a corrupt num cannot overflow it.
        let num = i32_at(&buf, 0);
        if num < 0 || 4i64 * (3 * num as i64 + 2) > compr_size as i64 {
            return Err(BAD);
        }
        let num = num as usize;
        let lens_off = 4;
        let offsets_off = lens_off + 4 * num;
        let datalens_off = offsets_off + 4 * num;
        let mut bp = datalens_off + 4 * (num + 1); // literal data starts here

        let block_start = pos; // decoded bytes of this block are contiguous from here

        let lens = |i: usize| i32_at(&buf, lens_off + 4 * i);
        let offsets = |i: usize| i32_at(&buf, offsets_off + 4 * i);
        let datalens = |i: usize| i32_at(&buf, datalens_off + 4 * i);

        for i in 0..num {
            // literal run
            let dl = datalens(i);
            if dl < 0 || dl as usize > buf.len() - bp || dl as usize > block_size - pos {
                return Err(BAD);
            }
            let dl = dl as usize;
            data[pos..pos + dl].copy_from_slice(&buf[bp..bp + dl]);
            bp += dl;
            pos += dl;

            // match: offset relative to the current position, wrapping the buffer
            let raw_off = offsets(i);
            let offset = if raw_off as i64 <= pos as i64 {
                raw_off as i64
            } else {
                raw_off as i64 - block_size as i64
            };
            let ln = lens(i);
            if offset <= 0 || offset > pos as i64 || ln < 0 || ln as usize > block_size - pos {
                return Err(BAD);
            }
            let src = pos - offset as usize;
            // Overlapping LZ copy, byte at a time exactly as memcpy_lz_match.
            for k in 0..ln as usize {
                data[pos + k] = data[src + k];
            }
            pos += ln as usize;
        }

        // One trailing literal run (possibly empty).
        let dl = datalens(num);
        if dl < 0 || dl as usize > buf.len() - bp || dl as usize > block_size - pos {
            return Err(BAD);
        }
        let dl = dl as usize;
        data[pos..pos + dl].copy_from_slice(&buf[bp..bp + dl]);
        pos += dl;

        // Flush this block's decoded bytes (contiguous from block_start).
        let out = &data[block_start..pos];
        if !out.is_empty() {
            let n = io.write(out);
            if (n as usize) != out.len() {
                return Err(if n >= 0 { IO } else { n });
            }
        }

        // Wraparound happens only at a full buffer, and only at a block end.
        if pos == block_size {
            pos = 0;
        }
    }
    Ok(())
}

/// Signature-compatible entry with the unused tuning knobs, matching
/// `rep_decompress` so the C wrapper can forward straight through.
#[allow(clippy::too_many_arguments)]
pub fn decompress_full(
    io: &Io,
    block_size: u32,
    _min_compression: c_int,
    _min_match_len: c_int,
    _barrier: c_int,
    _smallest_len: c_int,
    _hash_bits: c_int,
    _amplifier: c_int,
) -> c_int {
    drop(NOMEM); // allocation-failure code, kept for parity with the C
    decompress(io, block_size)
}

// ---------------------------------------------------------------------------
// Encoder
// ---------------------------------------------------------------------------
//
// A hash-based match finder ported line-for-line from `rep_compress`. It is
// deterministic -- the hash table state is a pure function of the input and
// parameters -- so a correct port produces byte-identical output to the C, and
// that (Rust-encode == C-encode) is what rust/difftest/rep_ref.cpp checks.
//
// The arithmetic is all 32-bit `int` in the C, which is 32 bits on every target
// (unlike `long`/`ulong32`, this has no ARM64 surprise). The rolling hash is
// signed and its right shift is arithmetic, so i32 with wrapping ops and `>>`
// reproduces it exactly.

const PRIME: i32 = 153191;

fn power_u32(base: u32, mut n: u32) -> u32 {
    let mut result: u32 = 1;
    while n != 0 {
        result = result.wrapping_mul(base);
        n -= 1;
    }
    result
}

/// Largest power of two not exceeding sqrt(n): sqrtb(36) = 4.
fn sqrtb(mut n: u32) -> u32 {
    let mut result: u32 = 1;
    loop {
        n /= 4; // base*base, base=2
        if n == 0 {
            break;
        }
        result *= 2;
    }
    result
}

/// roundup_to_power_of(n, 2): smallest power of two >= n, with n==1 -> 1.
fn roundup_pow2(n: u32) -> u32 {
    if n <= 1 {
        return 1;
    }
    let mut result: u32 = 1;
    while result < n {
        result <<= 1;
    }
    result
}

/// Encode a REP stream. Signature mirrors `rep_compress`.
#[allow(clippy::too_many_arguments)]
pub fn compress(
    io: &Io,
    block_size: u32,
    _min_compression: c_int,
    min_match_len: c_int,
    barrier: c_int,
    smallest_len: c_int,
    hash_bits: c_int,
    amplifier: c_int,
) -> c_int {
    match encode(io, block_size, min_match_len, barrier, smallest_len, hash_bits, amplifier) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

const MAX_READ: usize = 8 * 1024 * 1024;

fn encode(
    io: &Io,
    block_size: u32,
    min_match_len: c_int,
    barrier: c_int,
    mut smallest_len: c_int,
    hash_bits: c_int,
    amplifier: c_int,
) -> Result<(), c_int> {
    let block_size = block_size as usize;
    let min_match_len = min_match_len as i64;
    let barrier = barrier as i64;
    if smallest_len > min_match_len as c_int {
        smallest_len = min_match_len as c_int;
    }
    // L = roundup_to_power_of(SmallestLen/2, 2); k = sqrtb(L*2)
    let l = roundup_pow2((smallest_len / 2) as u32) as i64;
    let k = sqrtb((l * 2) as u32) as i64;
    let k1 = (k - 1) as i32;
    let test = (k * amplifier as i64).min(l);
    let c_power_prime_l = power_u32(PRIME as u32, l as u32) as i32;

    // Hash size: CalcHashSize
    let hash_size: usize = if hash_bits > 0 {
        1usize << hash_bits
    } else {
        (roundup_pow2((block_size / 3 * 2) as u32) as usize) / (k.max(16) as usize)
    };
    let hash_mask: i32 = (hash_size as i32).wrapping_sub(1);

    let mut hash: i32 = 0;
    let update_hash = |hash: &mut i32, sub: u8, add: u8| {
        *hash = hash
            .wrapping_mul(PRIME)
            .wrapping_add(add as i32)
            .wrapping_sub((sub as i32).wrapping_mul(c_power_prime_l));
    };
    let chksum = |hash: i32| -> i32 { (hash >> 28) & k1 };

    let mut buf = vec![0u8; block_size];
    let mut hasharr = vec![0i32; hash_size];

    // Parallel output buffers, flushed per block.
    let mut lens: Vec<u8> = Vec::new();
    let mut offsets: Vec<u8> = Vec::new();
    let mut datalens: Vec<u8> = Vec::new();
    let mut data_offsets: Vec<i32> = Vec::new(); // addresses into buf; the C stores these then reads buf back

    let mut base: i64 = 0;
    let mut last_i: i64 = 0;
    let mut last_match: i64 = 0;
    let mut first_time = true;

    loop {
        // READ: FirstTime reads up to MAX_READ; later, up to BlockSize/8.
        let want = if first_time {
            (block_size as i64 - base).min(MAX_READ as i64)
        } else {
            (block_size as i64 - base).min((block_size as i64 / 8).min(MAX_READ as i64))
        };
        let got = io.read(&mut buf[base as usize..base as usize + want as usize]);
        if got < 0 {
            return Err(got);
        }
        let size = got as i64;

        if first_time {
            hasharr.iter_mut().for_each(|h| *h = 0);
            write_u32(io, block_size as u32)?; // Put32(BlockSize)
            first_time = false;
        }
        if size == 0 {
            break;
        }

        if base == 0 {
            hash = 0;
            for i in 0..l.min(size) as usize {
                update_hash(&mut hash, 0, buf[i]);
            }
        }

        let mut literals: i64 = 0;
        lens.clear();
        offsets.clear();
        datalens.clear();
        data_offsets.clear();

        // MAIN LOOP
        let mut i: i64 = last_i;
        while i + l * 2 < base + size {
            let mut j = 0i64;
            while j < test {
                if i >= last_match {
                    let mut m = hasharr[(hash & hash_mask) as usize];
                    if m != 0 && chksum(hash) == (m & k1) {
                        m &= !k1;
                        let mmatch = m as i64;
                        if mmatch >= i && mmatch < base + size {
                            // stale -> skip to no_match
                        } else {
                            let low_bound = if mmatch < i {
                                i - mmatch
                            } else if mmatch - (base + size) > i {
                                0
                            } else {
                                i - (mmatch - (base + size))
                            };
                            let high_bound = block_size as i64 - mmatch + i;
                            let start = find_start(&buf, mmatch, i, last_match.max(low_bound));
                            let end = find_end(&buf, mmatch, i, (base + size).min(high_bound));
                            let need = if i - mmatch < barrier { min_match_len } else { smallest_len as i64 };
                            if end - start >= need {
                                let mut offset = i - mmatch;
                                if offset < 0 {
                                    offset += block_size as i64;
                                }
                                data_offsets.push(last_match as i32);
                                push32(&mut datalens, (start - last_match) as i32);
                                push32(&mut offsets, offset as i32);
                                push32(&mut lens, (end - start) as i32);
                                literals += start - last_match;
                                last_match = end;
                            }
                        }
                    }
                }
                // no_match:
                if (i & (k - 1)) == 0 {
                    hasharr[(hash & hash_mask) as usize] = i as i32 + chksum(hash);
                }
                update_hash(&mut hash, buf[i as usize], buf[(i + l) as usize]);
                j += 1;
                i += 1;
            }
            // index every k bytes until end of the L-block
            while (i & (l - 1)) != 0 {
                hasharr[(hash & hash_mask) as usize] = i as i32 + chksum(hash);
                for _ in 0..k {
                    update_hash(&mut hash, buf[i as usize], buf[(i + l) as usize]);
                    i += 1;
                }
            }
            last_i = i; // C: for(...; last_i=i) -- update after each L-block iteration
        }

        base += size;
        if base == block_size as i64 {
            last_i = base;
        }
        if last_match > last_i {
            push32(&mut datalens, 0);
        } else {
            data_offsets.push(last_match as i32);
            push32(&mut datalens, (last_i - last_match) as i32);
            literals += last_i - last_match;
            last_match = last_i;
        }
        if base == block_size as i64 {
            base = 0;
            last_match = 0;
            last_i = 0;
        }

        let outsize = 4 * 2 + lens.len() as i64 + offsets.len() as i64 + datalens.len() as i64 + literals;
        write_u32(io, (outsize - 4) as u32)?;
        write_u32(io, (lens.len() / 4) as u32)?;
        write_all(io, &lens)?;
        write_all(io, &offsets)?;
        write_all(io, &datalens)?;
        // dataOffsets/datalens rewound in lockstep: write buf[off..off+dl]
        let mut dp = 0usize;
        let mut dlp = 0usize;
        while dp < data_offsets.len() {
            let off = data_offsets[dp] as usize;
            let dl = i32::from_le_bytes([datalens[dlp], datalens[dlp + 1], datalens[dlp + 2], datalens[dlp + 3]]) as usize;
            dp += 1;
            dlp += 4;
            write_all(io, &buf[off..off + dl])?;
        }
    }

    // Final block: uncompressed remainder + EOF marker.
    let datalen = base - last_match;
    write_u32(io, (4 * 2 + datalen) as u32)?;
    write_u32(io, 0)?; // 0 matches
    write_u32(io, datalen as u32)?;
    write_all(io, &buf[last_match as usize..(last_match + datalen) as usize])?;
    write_u32(io, 0)?; // EOF flag
    Ok(())
}

fn find_start(buf: &[u8], mut m: i64, mut q: i64, start: i64) -> i64 {
    while q > start {
        m -= 1;
        q -= 1;
        if buf[m as usize] != buf[q as usize] {
            return q + 1;
        }
    }
    q
}
fn find_end(buf: &[u8], mut m: i64, mut q: i64, end: i64) -> i64 {
    while q < end && buf[m as usize] == buf[q as usize] {
        m += 1;
        q += 1;
    }
    q
}
fn push32(v: &mut Vec<u8>, x: i32) {
    v.extend_from_slice(&x.to_le_bytes());
}
fn write_u32(io: &Io, x: u32) -> Result<(), c_int> {
    write_all(io, &x.to_le_bytes())
}
fn write_all(io: &Io, b: &[u8]) -> Result<(), c_int> {
    if b.is_empty() {
        return Ok(());
    }
    let n = io.write(b);
    if (n as usize) == b.len() { Ok(()) } else { Err(if n >= 0 { IO } else { n }) }
}
