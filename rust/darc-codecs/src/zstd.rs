//! Zstandard, delegating to the `zstd-safe` crate in place of the vendored
//! `Compression/Zstd/libzstd` tree (32,375 lines).
//!
//! This is a *binding* crate: the C library still compiles, it is simply
//! fetched and versioned by cargo instead of living in the repository. That is
//! the point of the substitution -- the maintenance moves upstream -- rather
//! than any claim that zstd is now Rust.
//!
//! ## Version
//!
//! The vendored tree is **1.5.6**; `zstd-sys 2.0.16` bundles **1.5.7**. The
//! frame format is unchanged between them, so archives written by either are
//! readable by both, which is the property that matters. Encoder output may
//! differ where 1.5.7 changed a heuristic. Nothing recorded moves: `-mzstd`
//! appears in no `-m` preset and in no case in `Tests/fingerprints.txt`, both
//! checked rather than assumed.
//!
//! ## Framing
//!
//! `C_Zstd.cpp` drives the streaming API and writes a bare zstd frame with no
//! wrapper of its own -- unlike LZ4, which DArc surrounds with a version byte
//! and per-block lengths. So the bytes on disk are exactly what
//! `ZSTD_compressStream2` produced, and interoperability is the zstd frame
//! format itself.
//!
//! The parameters mirror `zstd_stream_compress`: compression level always;
//! `windowLog` plus `enableLongDistanceMatching` when a window log is given;
//! `nbWorkers` when workers are requested. Long-distance matching is tied to
//! the window log there, not offered separately, and that pairing is
//! reproduced rather than reinterpreted.

use crate::ffi::Io;
use std::os::raw::c_int;
use zstd_safe::{CCtx, DCtx, InBuffer, OutBuffer};

/// `ZSTD_IN_BUFSZ` / `ZSTD_OUT_BUFSZ` in `C_Zstd.cpp` -- 128 KiB each.
const IN_BUFSZ: usize = 1 << 17;
const OUT_BUFSZ: usize = 1 << 17;

/// `zstd_stream_compress`: the callback-driven streaming compressor.
///
/// Mirrors the C loop exactly -- read a chunk, drive `ZSTD_compressStream2`
/// until the chunk is consumed (or, on the final empty read, until the frame is
/// flushed), writing whatever comes out. A zero-length read is what signals end
/// of input, and it is that read which switches the directive to `e_end`.
pub fn compress_stream(io: &Io, params: Params) -> c_int {
    let general = crate::ffi::FREEARC_ERRCODE_GENERAL as c_int;
    let mut cctx = CCtx::create();
    if cctx
        .set_parameter(zstd_safe::CParameter::CompressionLevel(params.level))
        .is_err()
    {
        return general;
    }
    if params.window_log > 0 {
        if cctx
            .set_parameter(zstd_safe::CParameter::WindowLog(params.window_log))
            .is_err()
        {
            return general;
        }
        if cctx
            .set_parameter(zstd_safe::CParameter::EnableLongDistanceMatching(true))
            .is_err()
        {
            return general;
        }
    }
    if params.workers > 0 {
        // The C ignores the result too: a build without multithreading simply
        // stays single-threaded rather than failing the compression.
        let _ = cctx.set_parameter(zstd_safe::CParameter::NbWorkers(params.workers));
    }

    let mut in_buf = vec![0u8; IN_BUFSZ];
    let mut out_buf = vec![0u8; OUT_BUFSZ];

    loop {
        let got = io.read(&mut in_buf);
        if got < 0 {
            return got;
        }
        let got = got as usize;
        let end = got == 0;
        let mode = if end {
            zstd_safe::zstd_sys::ZSTD_EndDirective::ZSTD_e_end
        } else {
            zstd_safe::zstd_sys::ZSTD_EndDirective::ZSTD_e_continue
        };

        let mut input = InBuffer::around(&in_buf[..got]);
        loop {
            let (produced, remaining) = {
                let mut output = OutBuffer::around(&mut out_buf[..]);
                let remaining = match cctx.compress_stream2(&mut output, &mut input, mode) {
                    Ok(r) => r,
                    Err(_) => return general,
                };
                (output.pos(), remaining)
            };
            if produced > 0 {
                match io.write_all(&out_buf[..produced]) {
                    Err(e) => {
                        return e;
                    }
                    Ok(_) => {}
                }
            }
            let drained = if end { remaining == 0 } else { input.pos() == got };
            if drained {
                break;
            }
        }

        if end {
            break;
        }
    }
    crate::ffi::OK
}

/// `zstd_stream_decompress`: the callback-driven streaming decompressor.
///
/// `last_ret` carries zstd's "bytes still expected in this frame" across reads,
/// so an input that stops mid-frame is rejected rather than silently truncating
/// the output -- the C does the same, and corrupt archives reach here through an
/// ordinary `arc t`.
pub fn decompress_stream(io: &Io) -> c_int {
    let bad = crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
    let mut dctx = DCtx::create();

    let mut in_buf = vec![0u8; IN_BUFSZ];
    let mut out_buf = vec![0u8; OUT_BUFSZ];
    let mut last_ret: usize = 0;

    loop {
        let got = io.read(&mut in_buf);
        if got < 0 {
            return got;
        }
        let got = got as usize;
        if got == 0 {
            // End of input: a frame still expecting bytes is a truncated stream.
            return if last_ret != 0 { bad } else { crate::ffi::OK };
        }

        let mut input = InBuffer::around(&in_buf[..got]);
        while input.pos() < got {
            let produced = {
                let mut output = OutBuffer::around(&mut out_buf[..]);
                last_ret = match dctx.decompress_stream(&mut output, &mut input) {
                    Ok(r) => r,
                    Err(_) => return bad,
                };
                output.pos()
            };
            if produced > 0 {
                match io.write_all(&out_buf[..produced]) {
                    Err(e) => {
                        return e;
                    }
                    Ok(_) => {}
                }
            }
        }
    }
}

/// Parameters as `C_Zstd.cpp` understands them.
#[derive(Clone, Copy, Debug)]
pub struct Params {
    pub level: i32,
    /// 0 means "leave at the library default", matching `if (WindowLog > 0)`.
    pub window_log: u32,
    /// 0 means single-threaded, matching `if (Workers > 0)`.
    pub workers: u32,
}

/// `ZSTD_minCLevel()` / `ZSTD_maxCLevel()`, used by `parse_ZSTD` to clamp a
/// user-supplied level to the range the library advertises.
pub fn min_c_level() -> c_int {
    zstd_safe::min_c_level()
}

pub fn max_c_level() -> c_int {
    zstd_safe::max_c_level()
}

/// `ZSTD_sizeof_CCtx` for a context configured the way `ZSTD_METHOD` would,
/// which is how `GetCompressionMem` estimates the compressor's footprint.
/// Returns 0 if the context cannot be configured; the caller substitutes its
/// own default, exactly as the C does.
pub fn sizeof_cctx(level: i32, window_log: u32) -> usize {
    let mut cctx = CCtx::create();
    if cctx
        .set_parameter(zstd_safe::CParameter::CompressionLevel(level))
        .is_err()
    {
        return 0;
    }
    if window_log > 0
        && cctx
            .set_parameter(zstd_safe::CParameter::WindowLog(window_log))
            .is_err()
    {
        return 0;
    }
    cctx.sizeof()
}

/// Compress `src` into a zstd frame.
///
/// One-shot rather than streaming: the caller reads a block and hands it over
/// whole. The C version streams because it is fed by a read callback, but the
/// frame produced is the same either way -- zstd's framing does not depend on
/// how the input was chunked, only on the parameters.
pub fn compress(src: &[u8], params: Params) -> Result<Vec<u8>, c_int> {
    let mut cctx = CCtx::create();
    cctx.set_parameter(zstd_safe::CParameter::CompressionLevel(params.level))
        .map_err(|_| crate::ffi::FREEARC_ERRCODE_GENERAL as c_int)?;
    if params.window_log > 0 {
        cctx.set_parameter(zstd_safe::CParameter::WindowLog(params.window_log))
            .map_err(|_| crate::ffi::FREEARC_ERRCODE_GENERAL as c_int)?;
        cctx.set_parameter(zstd_safe::CParameter::EnableLongDistanceMatching(true))
            .map_err(|_| crate::ffi::FREEARC_ERRCODE_GENERAL as c_int)?;
    }
    if params.workers > 0 {
        // Not fatal if the build lacks multithreading: the C code likewise
        // ignores the result of this call.
        let _ = cctx.set_parameter(zstd_safe::CParameter::NbWorkers(params.workers));
    }

    let mut out: Vec<u8> = Vec::with_capacity(zstd_safe::compress_bound(src.len()));
    cctx.compress2(&mut out, src)
        .map_err(|_| crate::ffi::FREEARC_ERRCODE_GENERAL as c_int)?;
    Ok(out)
}

/// Decompress a zstd frame produced by any conformant encoder, including the
/// vendored 1.5.6 the archiver currently links.
///
/// `capacity` is the caller's known output size. Streaming rather than
/// `decompress` in one call, because a frame that lies about its content size
/// -- which a corrupt or hostile archive can -- must not be able to make this
/// allocate on the strength of a header field.
pub fn decompress(src: &[u8], capacity: usize) -> Result<Vec<u8>, c_int> {
    let bad = crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
    let mut dctx = DCtx::create();
    let mut out = vec![0u8; capacity];

    let mut input = InBuffer::around(src);
    let mut output = OutBuffer::around(&mut out[..]);

    loop {
        let remaining = dctx.decompress_stream(&mut output, &mut input).map_err(|_| bad)?;
        if remaining == 0 {
            break; // frame complete
        }
        if input.pos() == src.len() {
            return Err(bad); // input exhausted mid-frame: truncated
        }
        if output.pos() == capacity {
            return Err(bad); // more output than the caller said was possible
        }
    }

    let n = output.pos();
    out.truncate(n);
    Ok(out)
}
