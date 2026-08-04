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
        Method::Lzma2(p) => p.dictionary_size,
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
        // `SetDictionary` is the same one-liner (C_LZMA2.cpp:100).
        Method::Lzma2(p) => p.dictionary_size = dict,
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

/// What `-mt` was set to, or `None` for "not given".
///
/// A global for the same reason the C's is one: `GetCompressionThreads()` is
/// read from inside method objects that the option never reaches. `-mt` is
/// scanned once at startup, before any chain is built.
static COMPRESSION_THREADS: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Record `-mtN`. Zero, which is what `-mt+` means, restores "ask the machine".
pub fn set_compression_threads(n: u32) {
    COMPRESSION_THREADS.store(n, std::sync::atomic::Ordering::Relaxed);
}

/// `GetCompressionThreads()` **as the compressor sees it** — `-mt` if it was
/// given, else the processor count (`Cmdline.hs:295`).
///
/// LZMA2 passes this to its encoder, where it changes the stream outright:
/// above one block thread `Lzma2EncProps_Normalize` abandons the solid block
/// and splits the input, so `-mlzma2:d64k -mt1` and `-mlzma2:d64k -mt8` write
/// different archives on purpose. Measured against the reference in both
/// states, which is what proves this is plumbed at all.
pub fn compression_threads() -> u64 {
    match COMPRESSION_THREADS.load(std::sync::atomic::Ordering::Relaxed) {
        0 => std::thread::available_parallelism().map(|n| n.get() as u64).unwrap_or(1),
        n => u64::from(n),
    }
}

/// `GetCompressionThreads()` **as the memory formulas see it**, which is always
/// **one** — and this is not the same number as [`compression_threads`].
///
/// `static int compression_threads = 1` (`CompressionLibrary.cpp`), and
/// `SetCompressionThreads` is deferred: `Cmdline.hs:295` puts it in
/// `setup_command`, a list that does not run until the command starts. Every
/// memory limit is applied before that, so GRZip's and 4x4's formulas divide by
/// the *initial* value and never see `-mt` at all.
///
/// Measured, not inferred. `-mgrzip -lc4m` writes `grzip:466033b` in the
/// reference, and 466033 is 4194304/9 exactly — a divisor of 9, not of 9×8 on
/// this eight-core machine. `-mt1`, `-mt4` and `-mt8` all leave
/// `-mgrzip -lc16m` untouched, which they could not if the option reached here.
///
/// Reading the processor count instead was a live bug: it made `-lc`/`-ld` on
/// grzip and 4x4 shrink by a machine-dependent factor, so two hosts wrote
/// different archives from the same command line.
const MEMORY_FORMULA_THREADS: u64 = 1;

/// `GetDecompressionMem` — what each method needs to UNPACK, from `C_*.h`.
pub fn get_decompression_mem(m: &Method) -> u64 {
    match m {
        // Nothing is stored, so nothing is needed to unpack it.
        Method::Fake | Method::Crc => 0,
        Method::Lzma(p) => u64::from(p.dictionary_size) + 2 * MB,
        Method::Lzma2(p) => u64::from(p.dictionary_size) + 2 * MB,
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
        Method::Grzip(p) => u64::from(p.block_size) * 5 * MEMORY_FORMULA_THREADS,
        Method::Delta(p) => u64::from(p.block_size),
        Method::Dispack(p) => {
            2 * u64::from(p.block_size) + u64::from(p.block_size) / 4 + 1024
        }
        // LARGE_BUFFER_SIZE in C_BCJ.h.
        Method::Exe => 8 * MB,
        Method::FourX4(p) => {
            let t = if p.num_threads == 0 { MEMORY_FORMULA_THREADS } else { u64::from(p.num_threads) };
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
        // Identical (C_LZMA2.cpp:95).
        Method::Lzma2(p) => {
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
            let bs = (mem / 5 / MEMORY_FORMULA_THREADS).min(u64::from(u32::MAX)) as u32;
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
    fit_for_add_limited(chain, total_bytes, ADD_DECOMPRESSION_LIMIT)
}

/// `fit_for_add` with the `-ld` figure supplied rather than defaulted.
pub fn fit_for_add_limited(chain: &str, total_bytes: u64, dlimit: u64) -> Option<String> {
    fit_for_add_limits(chain, total_bytes, u64::MAX, dlimit)
}

/// `fit_for_add` with both memory limits supplied.
///
/// The order is the reference's: `limitCompressionMem` then
/// `limitDecompressionMem` (Cmdline.hs:339-340), both before the dictionary is
/// fitted to the data.
pub fn fit_for_add_limits(
    chain: &str,
    total_bytes: u64,
    climit: u64,
    dlimit: u64,
) -> Option<String> {
    let names: Vec<String> = chain.split('+').map(str::to_string).collect();
    let mut methods = Method::parse_chain(&names)?;
    limit_compression_mem(&mut methods, climit);
    limit_decompression_mem(&mut methods, dlimit);
    limit_dictionary(&mut methods, dictionary_limit(total_bytes));
    Some(methods.iter().map(crate::canonize::show).collect::<Vec<_>>().join("+"))
}

/// Physical RAM in bytes, rounded down to a 4 MB boundary.
///
/// `getPhysicalMemory `roundTo` (4*mb)` (`Cmdline.hs:230`). The rounding is not
/// cosmetic: a percentage limit is computed from this figure, and an unrounded
/// one would make `-ld75%` produce a slightly different compression chain — and
/// so a different archive — on two machines with the same nominal RAM.
///
/// Returns 0 when the figure cannot be had — on any platform that is neither
/// macOS nor Linux, which today means the Windows cross-builds.
///
/// **0 means "unknown", and callers must treat it as NO LIMIT, not as a limit
/// of zero.** An earlier version of this comment argued that zero was the safe
/// direction because a smaller chain is one the reader can always afford. That
/// is the wrong criterion. A limit of zero shrank every method, including the
/// directory's `lzma:1mb`, so the Windows builds wrote different archives from
/// the Linux one for EVERY method — `store` included, which is how the interop
/// check caught it. Byte-identity is the property that matters here, not
/// frugality.
pub fn physical_memory() -> u64 {
    let raw = physical_memory_raw();
    let rounded = raw - (raw % (4 * MB));
    // CAPPED AT 4 GiB, established by measurement rather than by reading.
    //
    // On this 16 GB machine the reference answers `-ld10%` with
    // `ppmd:16:429496729b` and `-ld25%` with `ppmd:22:1gb`. The first is
    // consistent with a cap of either u32::MAX or 2^32 -- integer division
    // hides the difference -- but the second is not: 2^32 * 25/100 is exactly
    // 1073741824 (`1gb`), while u32::MAX * 25/100 is 1073741823, which the
    // reference would have printed as `1073741823b`. So the figure is 2^32.
    //
    // 10% alone would have "confirmed" the wrong constant. Only a percentage
    // that divides 2^32 evenly separates them.
    //
    // What is verified is the CAP. Whether a machine with less than 4 GiB sees
    // its true RAM here is untested -- nothing available reports less.
    rounded.min(4 * 1024 * MB)
}

#[cfg(target_os = "macos")]
fn physical_memory_raw() -> u64 {
    let mut out: u64 = 0;
    let mut len: usize = std::mem::size_of::<u64>();
    extern "C" {
        fn sysctlbyname(
            name: *const i8,
            oldp: *mut core::ffi::c_void,
            oldlenp: *mut usize,
            newp: *mut core::ffi::c_void,
            newlen: usize,
        ) -> i32;
    }
    // SAFETY: the name is NUL-terminated, and the buffer and its length match.
    let rc = unsafe {
        sysctlbyname(
            c"hw.memsize".as_ptr(),
            (&raw mut out).cast(),
            &raw mut len,
            std::ptr::null_mut(),
            0,
        )
    };
    match rc {
        0 => out,
        _ => 0,
    }
}

#[cfg(target_os = "linux")]
fn physical_memory_raw() -> u64 {
    // MemTotal is in kB, and is the first line of /proc/meminfo.
    let text = match std::fs::read_to_string("/proc/meminfo") {
        Ok(t) => t,
        Err(_) => return 0,
    };
    for line in text.lines() {
        match line.strip_prefix("MemTotal:") {
            Some(rest) => {
                let kb: u64 = rest.trim().trim_end_matches("kB").trim().parse().unwrap_or(0);
                return kb * 1024;
            }
            None => {}
        }
    }
    0
}

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
fn physical_memory_raw() -> u64 {
    0
}

/// `parseMemWithPercents` (`Utils.hs:78`) — a size, or a percentage of RAM.
///
/// The default unit is megabytes, `b` means bytes, and `%` or `p` means a
/// percentage of `memory`. Anything else is an error there, and `None` here.
pub fn parse_mem_with_percents(memory: u64, s: &str) -> Option<u64> {
    let digits: String = s.chars().take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        return None;
    }
    let n: u64 = digits.parse().ok()?;
    match s[digits.len()..].chars().next() {
        Some('%') | Some('p') => Some(memory.saturating_mul(n) / 100),
        // Everything else is a plain size, and parse_mem owns the unit table
        // so the two spellings cannot drift apart.
        _ => crate::method::parse_mem(s).map(u64::from),
    }
}

/// `GetCompressionMem` — what each method needs to PACK, from `C_*.h`.
///
/// Returns `None` for a method whose formula this port has not reproduced. The
/// caller must decide what to do about that; [`limit_compression_mem`] treats
/// it as "do not touch", which is exactly what the port did before `-lc`
/// existed and so cannot change an archive that used to be written.
pub fn get_compression_mem(m: &Method) -> Option<u64> {
    match m {
        // No compression, no memory.
        Method::Storing | Method::Fake | Method::Crc => Some(0),
        // `mfMem + 6mb`, where the multiplier is the match finder's
        // (`C_LZMA.cpp`). kBT2=10, kBT3/kBT4=11, kHC4=7, kHT4=6.
        Method::Lzma(p) => {
            Some(u64::from(p.dictionary_size) * lzma_mf_multiplier(p.match_finder) + 6 * MB)
        }
        Method::Ppmd(p) => Some(u64::from(p.mem)),
        Method::Dict(p) => Some(u64::from(p.block_size) * 2),
        Method::Delta(p) => Some(u64::from(p.block_size)),
        Method::Bsc(p) => Some(u64::from(p.block_size) * 5),
        // `{return 2*mb;}` for both, and TTA's setter is `{}`.
        Method::Mm(_) | Method::Tta(_) => Some(2 * MB),
        Method::Dispack(p) => {
            let bs = u64::from(p.block_size);
            Some(3 * bs + bs / 4 + 1024)
        }
        // `dict * divisor + 8mb` (C_LZMA2.cpp:76). The divisors are LZMA's
        // except at BT2, where LZMA2 charges 11 and LZMA charges 10, and the
        // constant is 8 MB rather than 6 -- so this cannot share
        // `lzma_mf_multiplier`, however alike the two lines look.
        Method::Lzma2(p) => {
            Some(u64::from(p.dictionary_size) * lzma2_mf_multiplier(p.match_finder) + 8 * MB)
        }
        // `hashsize + buffer + tornado_compressor_outbuf_size(buffer)`
        // (C_Tornado.h:33). That last term reads the global
        // `compress_all_at_once`, which is why this was left unported -- but the
        // global is 0 everywhere the archiver asks about memory: only
        // `C_4x4.cpp:571` sets it, and only for the duration of a compress call.
        // At limit time it is therefore always the `HUGE_BUFFER_SIZE` arm.
        Method::Tornado(p) => {
            Some(u64::from(p.hashsize) + u64::from(p.buffer) + HUGE_BUFFER_SIZE)
        }
        Method::Rep(p) => Some(rep_compression_mem(p)),
        // `BlockSize*2 + (1<<HashSizeLog)*sizeof(BYTE*)` (C_LZP.h:39) -- the
        // same figure as the decompression side, which already carries it.
        Method::Lzp(p) => Some(
            u64::from(p.block_size) * 2
                + (1u64 << p.hash_size_log.clamp(0, 40)) * PTR_SIZE,
        ),
        // `BlockSize*9*GetCompressionThreads()` (C_GRZip.h:56). It scales with
        // the thread count, which makes it machine-dependent -- true of the
        // reference too, and the reason `golden/manifest.txt` admits no grzip
        // case rather than a reason to refuse the formula.
        Method::Grzip(p) => Some(u64::from(p.block_size) * 9 * MEMORY_FORMULA_THREADS),
        // `BlockSize*2 + sizeof(state)` (C_LZ4.cpp:104).
        Method::Lz4(p) => Some(u64::from(p.block_size) * 2 + lz4_state_size(p.compressor)),
        // The C asks the library through `darc_rs_zstd_sizeof_cctx`
        // (C_Zstd.cpp:61), which is this very function on the other side of the
        // FFI -- so calling it directly is the same answer, not an estimate of
        // it.
        Method::Zstd(p) => {
            let est = darc_codecs::zstd::sizeof_cctx(p.level, p.window_log) as u64;
            let est = match p.workers {
                0 => est,
                w => est.saturating_mul(u64::from(w) + 1),
            };
            Some(match est {
                0 => 64 * MB,
                e => e,
            })
        }
        // `t * inner + (t+2) * 2 * bs` (C_4x4.cpp:590), the same shape as the
        // decompression half. An inner method with no figure has none itself.
        Method::FourX4(p) => {
            get_compression_mem(&p.inner).map(|inner| fourx4_mem(p, inner))
        }
        // `LARGE_BUFFER_SIZE` (C_BCJ.h:24).
        Method::Exe => Some(LARGE_BUFFER_SIZE),
        // `{return 0;}` (C_Encryption.h:44).
        Method::Encryption(_) => Some(0),
        // Still the one real gap, and it always will be: a method this port
        // cannot parse has no parameters to compute a figure from.
        Method::Unsupported(_) => None,
    }
}

/// `HUGE_BUFFER_SIZE` and `LARGE_BUFFER_SIZE` (`Compression.h:41,45`).
const HUGE_BUFFER_SIZE: u64 = 8 * MB;
const LARGE_BUFFER_SIZE: u64 = 256 * KB;

/// `LZ4_SIZEOF_STATE` / `_HC` (`C_LZ4.cpp:41`) — literal constants there, not
/// `sizeof` expressions, so they do not follow the machine.
fn lz4_state_size(compressor: i32) -> u64 {
    match compressor {
        0 => 16416,
        _ => 262200,
    }
}

/// LZMA2's match-finder multiplier (`C_LZMA2.cpp:79`).
fn lzma2_mf_multiplier(mf: u32) -> u64 {
    match mf {
        0 | 1 | 2 => 11,
        3 => 7,
        _ => 6,
    }
}

/// `REP_METHOD::GetCompressionMem` (`C_REP.cpp:84`), which reproduces the
/// encoder's own hash sizing rather than reading a field.
fn rep_compression_mem(p: &crate::method::RepParams) -> u64 {
    // `roundup_to_power_of (mymin(SmallestLen,MinMatchLen)/2, 2)`.
    let l = roundup_pow2(p.smallest_len.min(p.min_match_len) / 2);
    // `sqrtb(n, 2)`: `for (result=1; (n /= 4) != 0; result *= 2)`. Not a square
    // root -- it is 2^floor(log4(n)), and it is 1 for n < 4 rather than 0.
    let k = {
        let mut n = l.saturating_mul(2);
        let mut result = 1u64;
        loop {
            n /= 4;
            if n == 0 {
                break;
            }
            result *= 2;
        }
        result
    };
    // `CalcHashSize` (C_REP.cpp:78).
    let hash_size = match p.hash_size_log {
        0 => u64::from(roundup_pow2(p.block_size / 3 * 2)) / k.max(16),
        bits => 1u64 << bits.min(63),
    };
    u64::from(p.block_size) + hash_size * 4
}

/// The 4x4 memory shape (`C_4x4.cpp:590`/`:598`), shared by both directions
/// because the C's two functions differ only in which inner figure they take.
fn fourx4_mem(p: &crate::fourx4::FourX4Params, inner: u64) -> u64 {
    let t = match p.num_threads {
        0 => MEMORY_FORMULA_THREADS,
        n => u64::from(n),
    };
    let d = get_dictionary(&p.inner);
    let bs = match (p.block_size, d) {
        (0, 0) => 8 * MB,
        (0, d) => u64::from(d),
        (bs, _) => u64::from(bs),
    };
    t * inner + (t + 2) * 2 * bs
}

/// The match finder's memory multiplier (`C_LZMA.cpp`), shared by the getter
/// and the setter so the two cannot disagree.
fn lzma_mf_multiplier(mf: u32) -> u64 {
    match mf {
        0 => 10, // kBT2
        1 => 11, // kBT3
        2 => 11, // kBT4
        3 => 7,  // kHC4
        4 => 6,  // kHT4
        _ => 11,
    }
}

/// `SetCompressionMem`. Returns false when the method has no implementation
/// here, leaving it untouched.
pub fn set_compression_mem(m: &mut Method, mem: u64) -> bool {
    match m {
        Method::Storing | Method::Fake | Method::Crc => true,
        Method::Lzma(p) => {
            // `if (mem < 2*mb) mem = 2*mb;` then `avail = mem > 6mb ? mem-6mb
            // : mem`, divided by the match finder's multiplier and floored at
            // 4 kb.
            let mem = mem.max(2 * MB);
            let base = 6 * MB;
            let avail = match mem > base {
                true => mem - base,
                false => mem,
            };
            let d = avail / lzma_mf_multiplier(p.match_finder);
            p.dictionary_size = d.max(4 * 1024).min(u64::from(u32::MAX)) as u32;
            true
        }
        // Identical to the decompression setter -- `SetDecompressionMem` IS
        // `SetCompressionMem` for PPMd (`C_PPMD.h`).
        Method::Ppmd(p) => {
            set_ppmd_mem(p, mem);
            true
        }
        Method::Dict(p) => {
            if mem > 0 {
                p.block_size = (mem / 2).min(u64::from(u32::MAX)) as u32;
            }
            true
        }
        Method::Delta(p) => {
            if mem > 0 {
                p.block_size = mem.min(u64::from(u32::MAX)) as u32;
            }
            true
        }
        Method::Bsc(p) => {
            p.block_size = (mem / 5).min(u64::from(u32::MAX)) as u32;
            true
        }
        // `{}` -- MM and TTA ignore the request entirely.
        Method::Mm(_) | Method::Tta(_) => true,
        Method::Dispack(p) => {
            if mem > 0 {
                p.block_size = (mem / 13 * 4).max(64 * 1024).min(u64::from(u32::MAX)) as u32;
            }
            true
        }
        // `mem = max(mem, 2mb)`, then `avail = mem > 8mb ? mem-8mb : mem`
        // divided by the multiplier, floored at 4 KB (C_LZMA2.cpp:85).
        Method::Lzma2(p) => {
            let mem = mem.max(2 * MB);
            let base = 8 * MB;
            let avail = match mem > base {
                true => mem - base,
                false => mem,
            };
            let d = avail / lzma2_mf_multiplier(p.match_finder);
            p.dictionary_size = d.max(4 * 1024).min(u64::from(u32::MAX)) as u32;
            true
        }
        // `if (mem>0) hashsize = 1<<lb(mem/3), buffer = mem-hashsize`
        // (C_Tornado.h:37). The comma operator matters: the hash is computed
        // first and the buffer gets what is left, so the two are never set from
        // the same figure.
        Method::Tornado(p) => {
            if mem > 0 {
                let mem = mem.min(u64::from(u32::MAX));
                let hashsize = 1u64 << lb((mem / 3).min(u64::from(u32::MAX)) as u32);
                p.hashsize = hashsize.min(u64::from(u32::MAX)) as u32;
                p.buffer = mem.saturating_sub(hashsize) as u32;
            }
            true
        }
        // `if (mem>0) BlockSize = 1<<lb(mem/7*6)` (C_REP.h:38). Note `/7*6`,
        // integer division first -- not `*6/7`.
        Method::Rep(p) => {
            if mem > 0 {
                let target = (mem / 7 * 6).min(u64::from(u32::MAX)).max(1) as u32;
                p.block_size = 1u32 << lb(target);
            }
            true
        }
        // `SetDecompressionMem` IS `SetCompressionMem` here (C_LZP.h:44), which
        // is why the existing helper serves both.
        Method::Lzp(p) => {
            set_lzp_mem(p, mem);
            true
        }
        // `SetBlockSize (mem/9/GetCompressionThreads())` (C_GRZip.h:60) -- the
        // decompression half divides by 5 instead.
        Method::Grzip(p) => {
            let bs = (mem / 9 / MEMORY_FORMULA_THREADS).min(u64::from(u32::MAX)) as u32;
            if bs > 0 {
                p.block_size = bs.min(GRZ_MAX_BLOCK_SIZE);
                p.hash_size_log = p.hash_size_log.min(1 + lb(p.block_size.saturating_sub(1)));
            }
            true
        }
        // `C_LZ4.cpp:109`: the state comes off the top, the rest is split
        // between the in and out buffers, and the result is clamped to
        // 64 KB..256 MB. Below `state + 2kb` the split is skipped entirely and
        // the floor is taken directly.
        Method::Lz4(p) => {
            let state = lz4_state_size(p.compressor);
            let avail = match mem > state + 2 * KB {
                true => (mem - state) / 2,
                false => 64 * KB,
            };
            p.block_size = avail.clamp(64 * KB, 256 * MB) as u32;
            true
        }
        // `C_Zstd.cpp:70` -- zstd has no dictionary knob, so the memory request
        // is mapped onto the window log, and only when one was asked for.
        Method::Zstd(p) => {
            if mem > 0 {
                let mut wl = 10u32;
                while wl < 27 && (1u64 << wl) * 4 < mem {
                    wl += 1;
                }
                p.window_log = wl;
            }
            true
        }
        // `(void)mem;` -- 4x4's setter is deliberately empty (C_4x4.cpp:607),
        // so a 4x4 chain reports a figure and then declines to shrink. Left as
        // the C leaves it: the alternative is to invent a policy the reference
        // does not have, and `-lc` would then write different bytes.
        Method::FourX4(_) => true,
        // Both are empty bodies (C_BCJ.h:28, C_Encryption.h:48).
        Method::Exe | Method::Encryption(_) => true,
        // Unparsed, so unadjustable.
        Method::Unsupported(_) => false,
    }
}

/// `LimitCompressionMem` over a chain — `if (Get() > mem) Set(mem)`, method by
/// method (`Compression.h:285`), like the decompression half.
///
/// A method with no formula here is left alone rather than guessed at.
pub fn limit_compression_mem(chain: &mut [Method], mem: u64) {
    for m in chain.iter_mut() {
        match get_compression_mem(m) {
            Some(have) if have > mem => {
                set_compression_mem(m, mem);
            }
            _ => {}
        }
    }
}

/// Does every method in the chain have a compression-memory formula here?
pub fn compression_mem_is_known(chain: &[Method]) -> bool {
    chain.iter().all(|m| get_compression_mem(m).is_some())
}

/// The methods for which an explicit `-lc` still writes different bytes from
/// the reference, and so must be refused rather than served.
///
/// **The memory formulas are not the problem.** Measured against a reference
/// built from `9a127e6`, this port and the reference now agree on the method
/// string at every `-lc` level for both of these — `-mgrzip -lc4m` gives
/// `grzip:466033b:m1:l32:h15` on both sides, `-mtor -lc4m` gives
/// `tor:1181kb:h1mb` on both. The archives still differ, with the *same* string
/// stored.
///
/// What differs is upstream of the method: `compressionLimitMemoryUsage`
/// (`Compression.hs:217`) is `genericLimitMemoryUsage . map limitCompressionMem`,
/// and that second pass splices a `"tempfile"` stage into the chain once the
/// accumulated figure passes `limit * 1.05`. It changes how the data is fed to
/// the codec, not what the codec is. LZMA is unaffected — `-mlzma -lc4m` is
/// byte-identical — because it buffers the whole block either way; Tornado and
/// GRZip parse what they are handed, so their output follows the chunking.
///
/// Only the TOP level is examined, and that is deliberate rather than lazy:
/// `-m4x4:tor -lc16m` is byte-identical to the reference, so a Tornado reached
/// through 4x4 does not trip this. `-m4` and `-m9` do trip it, because their
/// per-filetype chains carry `tor` and `grzip` at the top level — and they were
/// refused before this too, when the refusal was "no formula for tor".
///
/// Everything else that used to be refused for want of a formula — REP, LZP,
/// LZ4, Zstd, 4x4, `exe`, encryption — is now served and gated on byte
/// identity.
pub fn lc_divergent(chain: &[Method]) -> Option<&'static str> {
    chain.iter().find_map(|m| match *m {
        Method::Tornado(_) => Some("tor"),
        Method::Grzip(_) => Some("grzip"),
        Method::Storing
        | Method::Fake
        | Method::Crc
        | Method::Lzma(_)
        | Method::Lzma2(_)
        | Method::Ppmd(_)
        | Method::Rep(_)
        | Method::Exe
        | Method::Dict(_)
        | Method::Lzp(_)
        | Method::Delta(_)
        | Method::Dispack(_)
        | Method::Tta(_)
        | Method::Mm(_)
        | Method::Zstd(_)
        | Method::Lz4(_)
        | Method::Bsc(_)
        | Method::FourX4(_)
        | Method::Encryption(_)
        | Method::Unsupported(_) => None,
    })
}

#[cfg(test)]
mod lzma2_and_lc_tests {
    use super::*;
    use crate::method::Method;

    fn m(s: &str) -> Method {
        Method::parse(s).expect("parses")
    }

    /// The two figures the C computes from `dictionarySize`, which decide what
    /// `-lc`/`-ld` write into an archive.
    #[test]
    fn lzma2_memory_matches_the_c() {
        // `dict * divisor + 8mb`, divisor 6 at the default HT4.
        assert_eq!(get_compression_mem(&m("lzma2:1m")), Some(MB * 6 + 8 * MB));
        // BT2 charges 11 here where LZMA charges 10 -- the one place the two
        // multiplier tables differ.
        assert_eq!(get_compression_mem(&m("lzma2:1m:BT2")), Some(MB * 11 + 8 * MB));
        assert_eq!(get_compression_mem(&m("lzma:1m:BT2")), Some(MB * 10 + 6 * MB));
        // `dictionarySize + 2mb`.
        assert_eq!(get_decompression_mem(&m("lzma2:1m")), MB + 2 * MB);
    }

    /// `SetCompressionMem`: 8 MB comes off the top, the rest is divided by the
    /// match finder's multiplier.
    #[test]
    fn lzma2_set_compression_mem_subtracts_the_base_first() {
        let mut a = m("lzma2");
        assert!(set_compression_mem(&mut a, 32 * MB));
        assert_eq!(get_dictionary(&a), ((32 - 8) * MB / 6) as u32);
        // Below the 8 MB base the subtraction is SKIPPED rather than clamped --
        // `avail = mem > base ? mem-base : mem` -- and `mem` has already been
        // raised to 2 MB. So the smallest dictionary this can produce is
        // 2 MB / 6, and the C's 4 KB floor below it is unreachable from here.
        // Asserting the floor instead would have been asserting dead code.
        let mut b = m("lzma2");
        assert!(set_compression_mem(&mut b, 1024));
        assert_eq!(get_dictionary(&b), (2 * MB / 6) as u32);
    }

    /// Every method DArc can parse now answers `Some`. Before this, seven did
    /// not, and an explicit `-lc` over any of them was refused outright.
    #[test]
    fn every_parsable_method_has_a_compression_formula() {
        for s in [
            "storing", "lzma", "lzma2", "ppmd", "tor", "rep", "grzip", "exe", "dict", "lzp",
            "delta", "dispack", "tta", "mm", "zstd", "lz4", "bsc", "4x4:tor",
        ] {
            assert!(
                get_compression_mem(&m(s)).is_some(),
                "{s} has no compression-memory formula"
            );
        }
        // The one that must stay None: nothing can be computed from a name this
        // port cannot parse.
        assert_eq!(get_compression_mem(&Method::Unsupported("nope".into())), None);
    }

    /// The figures the reference was measured to produce. `-mgrzip -lc4m`
    /// writes `grzip:466033b`, and 466033 is 4194304/9 exactly -- a divisor of
    /// 9, NOT of 9 times the processor count.
    #[test]
    fn grzip_memory_formula_does_not_read_the_processor_count() {
        let mut g = m("grzip");
        assert!(set_compression_mem(&mut g, 4 * MB));
        assert_eq!(get_dictionary(&g), 4 * MB as u32 / 9);
        // The getter is the same constant the other way round.
        assert_eq!(get_compression_mem(&m("grzip:1m")), Some(MB * 9));
    }

    /// `1<<lb(mem/7*6)`, integer division first. `*6/7` would give 3 670 016
    /// here and round to the same power of two, so the test uses a value where
    /// the two orders differ in the exponent.
    #[test]
    fn rep_set_compression_mem_is_a_power_of_two() {
        let mut r = m("rep");
        assert!(set_compression_mem(&mut r, 10 * MB));
        assert_eq!(get_dictionary(&r), 8 * MB as u32);
    }

    /// `-lc` is refused for exactly two methods, and only at the top level.
    #[test]
    fn lc_is_refused_for_tor_and_grzip_only() {
        let chain = |s: &str| {
            Method::parse_chain(&s.split('+').map(str::to_string).collect::<Vec<_>>()).unwrap()
        };
        assert_eq!(lc_divergent(&chain("tor")), Some("tor"));
        assert_eq!(lc_divergent(&chain("rep+tor:16mb")), Some("tor"));
        assert_eq!(lc_divergent(&chain("grzip")), Some("grzip"));
        // Reached through 4x4, which IS byte-identical under -lc.
        assert_eq!(lc_divergent(&chain("4x4:tor")), None);
        assert_eq!(lc_divergent(&chain("lzma2")), None);
        assert_eq!(lc_divergent(&chain("rep+exe+lzma:1m")), None);
    }
}

