//! Fitting a compression method to the data — `LimitDictionary` and friends.
//!
//! This is the half of `arc a` that canonicalisation is not. `-m1` writes
//! `4x4:tor:3:434kb` into the archive, and the `434kb` is not in any table: it
//! comes from `ArcvProcessRead.hs:122`, which shrinks the chain's dictionary to
//! fit the data before the block is compressed.
//!
//! ```haskell
//!   limitDictionary (clipToMaxMemSize $ roundMemUp $
//!                      totalBytes + (totalBytes `div` 100) + 512)
//! ```
//!
//! Confirmed against the reference over four input sizes, which is how the
//! rounding rule was pinned rather than guessed:
//!
//! | data | limit | written |
//! |---|---|---|
//! | 10 KiB | 10 854 → 11 264 | `tor:11kb:h64kb` |
//! | 100 KiB | 103 936 → 104 448 | `tor:102kb:h512kb` |
//! | 1000 KiB | 1 034 752 → 1 035 264 | `tor:1011kb` |
//! | 8000 KiB | 8 274 432 → 8 388 608 | `tor:8mb` |
//!
//! The last row is the one that matters: 8 274 432 rounds to a **megabyte**, not
//! a kilobyte, because `roundMemUp` changes granularity at 4096 KiB.
//!
//! `LimitDictionary` only ever *shrinks* (`Compression.h:287`):
//! `if (GetDictionary() > dict) SetDictionary(dict)`. A method whose dictionary
//! is already smaller than the data is left alone, which is why `dict:56kb`
//! survives on a 438 KiB corpus while `tor` is cut to `434kb`.

use crate::method::{Method, TornadoParams};

const KB: u64 = 1024;
const MB: u64 = 1024 * 1024;

/// `roundMemUp` (`Compression.hs:599`) — kilobytes below 4096 KiB, megabytes at
/// or above it.
pub fn round_mem_up(mem: u64) -> u64 {
    let unit = if mem >= 4096 * KB { MB } else { KB };
    mem.div_ceil(unit) * unit
}

/// The dictionary limit for a solid block of `total_bytes`
/// (`ArcvProcessRead.hs:122`).
///
/// The `+ total/100 + 512` slack is the C's, not a safety margin of this port's:
/// it is what makes a 10 KiB input round to 11 KiB rather than to 10.
pub fn dictionary_limit(total_bytes: u64) -> u32 {
    let raw = total_bytes + total_bytes / 100 + 512;
    // clipToMaxMemSize: MemSize is 32-bit.
    round_mem_up(raw).min(u64::from(u32::MAX)) as u32
}

/// `GetDictionary` — what each method reports, from its `C_*.h`.
///
/// Zero means "no dictionary", and a zero can never exceed a limit, so those
/// methods are never touched. PPMd is one of them: its memory is set by `-m`,
/// not by the data.
pub fn get_dictionary(m: &Method) -> u32 {
    match m {
        // No dictionary, and no memory: they compress nothing.
        Method::Fake | Method::Crc => 0,
        Method::Lzma(p) => p.dictionary_size,
        Method::Tornado(p) => p.buffer,
        Method::Rep(p) => p.block_size,
        Method::Dict(p) => p.block_size,
        Method::Lzp(p) => p.block_size,
        Method::Grzip(p) => p.block_size,
        // 4x4 forwards to its inner method (`C_4x4.h:31`).
        Method::FourX4(p) => get_dictionary(&p.inner),
        // GetDictionary is BlockSize (C_LZ4.h:24); BSC reports its own too.
        Method::Lz4(p) => p.block_size,
        Method::Bsc(p) => p.block_size,
        Method::Ppmd(_)
        // TTA, MM and Zstd all return 0 (C_TTA.h:35, C_MM.h:49, C_Zstd.h:43).
        | Method::Tta(_)
        | Method::Mm(_)
        | Method::Zstd(_)
        | Method::Delta(_)
        | Method::Dispack(_)
        | Method::Exe
        | Method::Storing
        // ENCRYPTION_METHOD::GetDictionary returns 0 (C_Encryption.h:45), so an
        // encrypted chain limits exactly as the same chain unencrypted does.
        | Method::Encryption(_)
        | Method::Unsupported(_) => 0,
    }
}

/// `SetDictionary` — each method's own, which is *not* simply "store the value".
pub fn set_dictionary(m: &mut Method, dict: u32) {
    if dict == 0 {
        return;
    }
    match m {
        // Nothing to shrink.
        Method::Fake | Method::Crc => {}
        Method::Lzma(p) => p.dictionary_size = dict,
        Method::Tornado(p) => set_tornado_dictionary(p, dict),
        Method::Rep(p) => p.block_size = dict,
        Method::Dict(p) => p.block_size = dict,
        // LZP routes through SetBlockSize, which also caps the hash.
        Method::Lzp(p) => {
            p.block_size = dict;
            p.hash_size_log = p.hash_size_log.min(1 + lb(dict.saturating_sub(1)) as i32);
        }
        Method::Grzip(p) => {
            p.block_size = dict.min(GRZ_MAX_BLOCK_SIZE);
            p.hash_size_log =
                p.hash_size_log.min(1 + lb(p.block_size.saturating_sub(1)));
        }
        Method::FourX4(p) => {
            let mut inner = (*p.inner).clone();
            set_dictionary(&mut inner, dict);
            p.inner_name = crate::canonize::show(&inner);
            p.inner = Box::new(inner);
        }
        // `if (dict) BlockSize = dict;` (C_LZ4.h:28) and BSC's SetBlockSize
        // (C_BSC.cpp:204). The `if (dict)` is handled by the early return.
        Method::Lz4(p) => p.block_size = dict,
        Method::Bsc(p) => p.block_size = dict,
        Method::Ppmd(_)
        // SetDictionary is an empty body for all three (C_TTA.h:39, C_MM.h:53).
        | Method::Tta(_)
        | Method::Mm(_)
        | Method::Zstd(_)
        | Method::Delta(_)
        | Method::Dispack(_)
        | Method::Exe
        | Method::Storing
        // SetDictionary is an empty body (C_Encryption.h:49).
        | Method::Encryption(_)
        | Method::Unsupported(_) => {}
    }
}

/// `GRZ_MaxBlockSize`, which bounds what GRZip's `SetBlockSize` accepts.
const GRZ_MAX_BLOCK_SIZE: u32 = 1024 * 1024 * 1024;

/// `lb` — floor(log2), as the C's bit-length helper computes it.
fn lb(n: u32) -> u32 {
    if n == 0 {
        0
    } else {
        31 - n.leading_zeros()
    }
}

/// `roundup_to_power_of(n, 2)`.
fn roundup_pow2(n: u32) -> u32 {
    match n {
        0 | 1 => 1,
        _ => 1u32 << (32 - (n - 1).leading_zeros()),
    }
}

/// `TORNADO_METHOD::SetDictionary` (`C_Tornado.cpp:41`).
///
/// Tornado is the only method whose `SetDictionary` adjusts a *second* field:
/// shrinking the buffer also shrinks the hash, which is why the reference writes
/// `tor:11kb:h64kb` on a 10 KiB input — the `h64kb` is not something the user
/// asked for, it is the hash following the buffer down.
fn set_tornado_dictionary(p: &mut TornadoParams, dict: u32) {
    // `typedef uint32 PtrVal` (Tornado.cpp:10) -- FOUR bytes, not eight. The
    // hash stores OFFSETS into the buffer, not real pointers, so its width does
    // not follow the machine's. Assuming 8 here gave `tor:11kb:h128kb` where the
    // reference writes `h64kb`, on the smallest of the four measured inputs.
    const PTR: u64 = 4;
    if dict < p.buffer {
        let entries = u64::from(p.hashsize) / PTR;
        let capped = entries.min(u64::from(roundup_pow2(dict)));
        p.hashsize = (PTR * capped).min(u64::from(u32::MAX)) as u32;
    } else if p.hashsize > 1024 * 1024 {
        // Growing: first undo a hash tuned to fit the Core2 cache, then scale.
        if p.hashsize < 8 * 1024 * 1024 && u64::from(p.hashsize) < u64::from(p.buffer) / 2 {
            p.hashsize = p.buffer / 2;
        }
        let h = (u64::from(dict) / (u64::from(p.buffer) / 64) * (u64::from(p.hashsize) / 64))
            .min(2 * 1024 * MB);
        let row = u64::from(p.hash_row_width).max(1);
        p.hashsize = (round_to_nearest_pow2(h / row) * row).min(2 * 1024 * MB) as u32;
    }
    p.buffer = dict;
}

/// `round_to_nearest_power_of(n, 2)` (`Common.h:559`) — geometric rounding, so
/// the result is the power of two nearest in RATIO, not in difference.
fn round_to_nearest_pow2(n: u64) -> u64 {
    let mut nn = n.saturating_mul(n) / 2;
    if nn == 0 {
        return 1;
    }
    let mut result: u64 = 2;
    loop {
        nn /= 4;
        if nn == 0 {
            break;
        }
        result = result.saturating_mul(2);
    }
    result
}

/// `LimitDictionary` (`Compression.h:287`) over a whole chain — shrink only.
pub fn limit_dictionary(chain: &mut [Method], dict: u32) {
    for m in chain.iter_mut() {
        if get_dictionary(m) > dict {
            set_dictionary(m, dict);
        }
    }
}

/// The convenience the writer wants: fit a canonical chain to a block of data.
pub fn fit_to_data(chain: &str, total_bytes: u64) -> Option<String> {
    let names: Vec<String> = chain.split('+').map(str::to_string).collect();
    let mut methods = Method::parse_chain(&names)?;
    limit_dictionary(&mut methods, dictionary_limit(total_bytes));
    Some(methods.iter().map(crate::canonize::show).collect::<Vec<_>>().join("+"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The granularity change at 4096 KiB is the whole subtlety of roundMemUp.
    #[test]
    fn round_mem_up_switches_from_kilobytes_to_megabytes_at_4096kb() {
        assert_eq!(round_mem_up(1), KB);
        assert_eq!(round_mem_up(10_854), 11 * KB);
        assert_eq!(round_mem_up(4096 * KB), 4 * MB);
        // Just under the threshold: still kilobytes.
        assert_eq!(round_mem_up(4096 * KB - 1), 4096 * KB);
        // Just over: megabytes, so this jumps a long way.
        assert_eq!(round_mem_up(4096 * KB + 1), 5 * MB);
        assert_eq!(round_mem_up(8_274_432), 8 * MB);
    }

    /// The four sizes measured against the reference. Each row is a limit the
    /// reference demonstrably used, not an arithmetic identity.
    #[test]
    fn the_dictionary_limit_matches_what_the_reference_used() {
        assert_eq!(dictionary_limit(10 * 1024), 11 * KB as u32);
        assert_eq!(dictionary_limit(100 * 1024), 102 * KB as u32);
        assert_eq!(dictionary_limit(1000 * 1024), 1011 * KB as u32);
        assert_eq!(dictionary_limit(8000 * 1024), 8 * MB as u32);
        // The generated corpus.
        assert_eq!(dictionary_limit(438_744), 434 * KB as u32);
    }

    /// LimitDictionary only shrinks. A method already smaller than the data is
    /// untouched -- which is why `dict:56kb` survives a 438 KiB corpus.
    #[test]
    fn limiting_never_grows_a_dictionary() {
        let before = "dict:56kb";
        assert_eq!(fit_to_data(before, 438_744).as_deref(), Some(before));
    }

    /// The end-to-end shape, against strings the reference wrote.
    #[test]
    fn a_chain_is_fitted_the_way_the_reference_fitted_it() {
        // -mtor on a 438,744-byte corpus.
        assert_eq!(fit_to_data("tor", 438_744).as_deref(), Some("tor:434kb"));
        // ...and on 10 KiB, where the hash follows the buffer down.
        assert_eq!(fit_to_data("tor", 10 * 1024).as_deref(), Some("tor:11kb:h64kb"));
        assert_eq!(fit_to_data("tor", 100 * 1024).as_deref(), Some("tor:102kb:h512kb"));
        assert_eq!(fit_to_data("tor", 1000 * 1024).as_deref(), Some("tor:1011kb"));
        assert_eq!(fit_to_data("tor", 8000 * 1024).as_deref(), Some("tor:8mb"));
    }

    /// PPMd has no dictionary, so the data size cannot touch it.
    #[test]
    fn methods_without_a_dictionary_are_untouched() {
        assert_eq!(fit_to_data("ppmd:10:48mb", 1024).as_deref(), Some("ppmd:10:48mb"));
        assert_eq!(fit_to_data("delta", 1024).as_deref(), Some("delta"));
        assert_eq!(fit_to_data("exe", 1024).as_deref(), Some("exe"));
    }

    /// 4x4 forwards to its inner method, so the limit reaches through it.
    #[test]
    fn the_limit_reaches_through_4x4_into_its_inner_method() {
        let got = fit_to_data("4x4:tor:3", 438_744).expect("fits");
        assert!(got.starts_with("4x4:"), "{got}");
        assert!(got.contains("434kb"), "the inner tornado was not limited: {got}");
    }

    #[test]
    fn a_whole_chain_is_fitted_link_by_link() {
        let got = fit_to_data("rep+exe+delta+lzma", 438_744).expect("fits");
        assert_eq!(got, "rep:434kb+exe+delta+lzma:434kb");
    }
}


// ---------------------------------------------------------------------------
// Memory limits
// ---------------------------------------------------------------------------

/// `sizeof(BYTE*)`, which LZP's memory formula multiplies its hash by.
///
/// Eight here, unlike Tornado's `PtrVal`: LZP's hash really is an array of
/// pointers, so it follows the machine's width.
const PTR_SIZE: u64 = 8;

/// `GetCompressionThreads()` — the thread count GRZip's memory formula scales
/// by. Set from the processor count by default; only GRZip's arithmetic reads
/// it, and only to divide the limit it is given.
fn compression_threads() -> u64 {
    std::thread::available_parallelism().map(|n| n.get() as u64).unwrap_or(1)
}

/// `GetDecompressionMem` — what each method needs to UNPACK, from `C_*.h`.
pub fn get_decompression_mem(m: &Method) -> u64 {
    match m {
        // Nothing is stored, so nothing is needed to unpack it.
        Method::Fake | Method::Crc => 0,
        Method::Lzma(p) => u64::from(p.dictionary_size) + 2 * MB,
        Method::Ppmd(p) => u64::from(p.mem),
        Method::Tornado(p) => u64::from(p.buffer),
        Method::Rep(p) => u64::from(p.block_size),
        // Flat constants (C_TTA.h:34, C_MM.h:48).
        Method::Tta(_) | Method::Mm(_) => MB,
        // BSC needs the block plus its working set; the C reports BlockSize.
        Method::Bsc(p) => u64::from(p.block_size),
        // `BlockSize*2` (C_LZ4.h:23).
        Method::Lz4(p) => u64::from(p.block_size) * 2,
        // `(1 << (WindowLog ?: 23)) + 128kb` (C_Zstd.cpp:96) -- dominated by the
        // window, which is only known when long-range mode named it.
        Method::Zstd(p) => {
            let wl = match p.window_log {
                0 => 23,
                w => w,
            };
            (1u64 << wl.min(40)) + 128 * 1024
        }
        // A flat 1 MB: the C comments out the BlockSize*2 it used to return.
        Method::Dict(_) => MB,
        Method::Lzp(p) => {
            u64::from(p.block_size) * 2 + (1u64 << p.hash_size_log.clamp(0, 40)) * PTR_SIZE
        }
        Method::Grzip(p) => u64::from(p.block_size) * 5 * compression_threads(),
        Method::Delta(p) => u64::from(p.block_size),
        Method::Dispack(p) => {
            2 * u64::from(p.block_size) + u64::from(p.block_size) / 4 + 1024
        }
        // LARGE_BUFFER_SIZE in C_BCJ.h.
        Method::Exe => 8 * MB,
        Method::FourX4(p) => {
            let t = if p.num_threads == 0 { compression_threads() } else { u64::from(p.num_threads) };
            let d = get_dictionary(&p.inner);
            let bs = if p.block_size != 0 {
                u64::from(p.block_size)
            } else if d > 0 {
                u64::from(d)
            } else {
                8 * MB
            };
            t * get_decompression_mem(&p.inner) + (t + 2) * 2 * bs
        }
        // GetCompressionMem and GetDecompressionMem are both 0
        // (C_Encryption.h:43).
        Method::Storing | Method::Encryption(_) | Method::Unsupported(_) => 0,
    }
}

/// `SetDecompressionMem` — each method's own, and several are NOT the inverse
/// of the getter.
pub fn set_decompression_mem(m: &mut Method, mem: u64) {
    match m {
        // No memory to limit.
        Method::Fake | Method::Crc => {}
        // `if (mem > 2mb) dictionarySize = mem - 2mb` -- silently does nothing
        // below 2 MB rather than clamping.
        Method::Lzma(p) => {
            if mem > 2 * MB {
                p.dictionary_size = (mem - 2 * MB).min(u64::from(u32::MAX)) as u32;
            }
        }
        // PPMd adjusts its ORDER with its memory, which is why reducing memory
        // changes the method string in two places at once.
        Method::Ppmd(p) => set_ppmd_mem(p, mem),
        Method::Tornado(p) => set_tornado_dictionary(p, mem.min(u64::from(u32::MAX)) as u32),
        Method::Rep(p) => {
            if mem > 0 {
                p.block_size = mem.min(u64::from(u32::MAX)) as u32;
            }
        }
        Method::Dict(p) => {
            if mem > 0 {
                p.block_size = (mem / 2).min(u64::from(u32::MAX)) as u32;
            }
        }
        Method::Lzp(p) => set_lzp_mem(p, mem),
        // `SetBlockSize (mem/5/threads)`, which also caps the hash.
        Method::Grzip(p) => {
            let bs = (mem / 5 / compression_threads()).min(u64::from(u32::MAX)) as u32;
            if bs > 0 {
                p.block_size = bs.min(GRZ_MAX_BLOCK_SIZE);
                p.hash_size_log =
                    p.hash_size_log.min(1 + lb(p.block_size.saturating_sub(1)));
            }
        }
        Method::Delta(p) => {
            if mem > 0 {
                p.block_size = mem.min(u64::from(u32::MAX)) as u32;
            }
        }
        // `virtual void SetDecompressionMem (MemSize) {}` -- 4x4 does nothing,
        // so a limit never reaches its inner method this way. TTA's and MM's
        // are empty too (C_TTA.h:38, C_MM.h:52), and Zstd's is `{}`
        // (C_Zstd.h:45).
        // LZ4's `SetDecompressionMem` forwards to `SetCompressionMem`
        // (C_LZ4.h:27), which resizes the block. Nothing here models that
        // heuristic, so the block is left alone rather than resized wrongly --
        // this only ever loosens a memory limit, never changes what is written.
        Method::Lz4(_)
        | Method::Bsc(_)
        | Method::Tta(_)
        | Method::Mm(_)
        | Method::Zstd(_)
        | Method::FourX4(_)
        | Method::Dispack(_)
        | Method::Exe
        | Method::Storing
        // SetDecompressionMem is an empty body (C_Encryption.h:48).
        | Method::Encryption(_)
        | Method::Unsupported(_) => {}
    }
}

/// `PPMD_METHOD::SetCompressionMem` (`C_PPMD.cpp:111`).
///
/// The order moves with the memory: `order += int(log2(new/old) * 4)`,
/// truncated toward zero, then clamped to 2..=128. Halving the memory therefore
/// costs three or four orders, and BOTH numbers appear in the method string --
/// which is why `ppmd:25:2047m` limited to 1 GB prints as `ppmd:22:1gb`.
fn set_ppmd_mem(p: &mut crate::method::PpmdParams, mem: u64) {
    if mem == 0 {
        return;
    }
    let ratio = mem as f64 / f64::from(p.mem);
    let delta = (ratio.ln() / 2f64.ln() * 4.0) as i32; // int() truncates toward zero
    p.order = (p.order + delta).clamp(2, 128);
    p.mem = mem.min(u64::from(u32::MAX)) as u32;
}

/// `LZP_METHOD::SetCompressionMem` (`C_LZP.cpp:88`).
///
/// Shrinks the HASH first and returns early if that alone is enough; only then
/// does it touch the block size.
fn set_lzp_mem(p: &mut crate::method::LzpParams, mem: u64) {
    let mut hashsize = (1u64 << p.hash_size_log.clamp(0, 40)) * PTR_SIZE;
    if hashsize > mem / 4 {
        p.hash_size_log = lb((mem / 16).min(u64::from(u32::MAX)) as u32) as i32;
        let now = u64::from(p.block_size) * 2
            + (1u64 << p.hash_size_log.clamp(0, 40)) * PTR_SIZE;
        if now <= mem {
            return;
        }
        hashsize = (1u64 << p.hash_size_log.clamp(0, 40)) * PTR_SIZE;
    }
    let bs = mem.saturating_sub(hashsize) / 2;
    p.block_size = bs.min(u64::from(u32::MAX)) as u32;
    p.hash_size_log = p.hash_size_log.min(1 + lb(p.block_size.saturating_sub(1)) as i32);
}

/// `LimitDecompressionMem` over a chain — shrink only, method by method.
///
/// Each link is limited INDEPENDENTLY against the same figure; the C's
/// `limitDecompressionMem = map . limitDecompressionMem` (`Compression.hs:198`)
/// maps over the chain rather than budgeting across it.
pub fn limit_decompression_mem(chain: &mut [Method], mem: u64) {
    for m in chain.iter_mut() {
        if get_decompression_mem(m) > mem {
            set_decompression_mem(m, mem);
        }
    }
}

/// The `-ld` default for the ADD command: a flat 1 GB (`Cmdline.hs:300`).
///
/// Not a percentage of anything, so it is the same on every machine — and it is
/// what reduces `-m9`'s `ppmd:25:2047m` to `ppmd:22:1gb`.
pub const ADD_DECOMPRESSION_LIMIT: u64 = 1024 * MB;

/// Fit a chain to the data AND to the decompression memory limit, in the order
/// `Cmdline.hs` applies them: the memory limits run when the command line is
/// parsed, the dictionary limit when the block's size is known.
pub fn fit_for_add(chain: &str, total_bytes: u64) -> Option<String> {
    let names: Vec<String> = chain.split('+').map(str::to_string).collect();
    let mut methods = Method::parse_chain(&names)?;
    limit_decompression_mem(&mut methods, ADD_DECOMPRESSION_LIMIT);
    limit_dictionary(&mut methods, dictionary_limit(total_bytes));
    Some(methods.iter().map(crate::canonize::show).collect::<Vec<_>>().join("+"))
}
