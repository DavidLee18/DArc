//! The block driver that stands in for `MtCoder` (`Compression/LZMA/7z24/MtCoder.c`)
//! on the LZMA2 encode path.
//!
//! ## What is reproduced, and what is not
//!
//! `MtCoder` is a read thread, a ring of block buffers, a semaphore and a coder
//! thread pool. **None of that is reproduced, and none of it needs to be**, because
//! only two of its properties are visible in the output:
//!
//! * **Where the block boundaries fall.** `MtCoder_ThreadFunc` reads
//!   `mtc->blockSize` bytes per block through `SeqInStream_ReadMax`
//!   (`MtCoder.c:118`, `:129`) and sets `finished = (size != mtc->blockSize)`
//!   (`:140`). `Lzma2Enc_Encode2` assigns `mtCoder.blockSize` straight from
//!   `props.blockSize` with no adjustment (`Lzma2Enc.c:770`), so the split is
//!   exactly the normalized block size, and the last block is whatever is left --
//!   possibly zero bytes, when the input is a whole multiple of the block size.
//! * **The order they are written in.** Blocks are handed out in index order
//!   (`MtCoder.c:170-173`) and `Lzma2Enc_MtCallback_Write` is called in index order
//!   (`Lzma2Enc.c:695`, driven from `MtCoder.c:526` / `:568`).
//!
//! So the stream is the ordered concatenation of independently encoded blocks, and
//! any schedule that preserves that order emits the same bytes. That is what makes
//! the parallelism here safe to add *after* the block splitting was already
//! byte-identical, and what makes `in_flight == 1` a genuine reference
//! implementation rather than a degraded one.
//!
//! ## The schedule
//!
//! Waves: read up to `in_flight` blocks, encode them on scoped threads, write the
//! results in order, repeat. Chosen over a pipeline with a dedicated reader because
//! it needs no channel, no shared mutable state and no ordering buffer -- the
//! ordering is the `Vec`'s -- so there is nothing that can reorder blocks under
//! load. The cost is that reading a wave does not overlap encoding the previous one;
//! with blocks of at least 1 MiB and typically 4 MiB or more, reading is a small
//! fraction of encoding.
//!
//! Peak memory is `in_flight * (block_size + out_lim)` for the buffers, plus one
//! LZMA encoder per thread. See [`Lzma2Enc::with_mt_memory_budget`].

use crate::lzma2_enc::{CONTROL_EOF, Lzma2Enc, Lzma2Error};
use crate::stream::{InStream, OutStream};

/// Encode `source` in blocks and write them to `sink` in block order.
///
/// `Lzma2Enc_Encode2`'s `MtCoder_Code` arm (`Lzma2Enc.c:739-800`), including the
/// single `LZMA2_CONTROL_EOF` terminator that the last block's `finished` flag
/// causes `Lzma2Enc_EncodeMt1` to write (`:631-646`).
pub(crate) fn encode_blocks_in_order(
    enc: &Lzma2Enc,
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    block_size: usize,
    out_lim: usize,
    in_flight: usize,
) -> Result<(), Lzma2Error> {
    let width = in_flight.max(1);
    let mut wave: Vec<Vec<u8>> = Vec::with_capacity(width);

    loop {
        // Fill a wave. `last` marks the block after which MtCoder would stop
        // reading: `finished = (size != blockSize)` (MtCoder.c:140).
        let mut last = false;
        wave.clear();
        while wave.len() < width {
            let block = read_block(source, block_size)?;
            let full = block.len() == block_size;
            wave.push(block);
            if !full {
                last = true;
                break;
            }
        }

        match wave.len() {
            // Cannot happen -- the loop above always pushes at least once -- but
            // returning rather than indexing keeps that a fact about this function
            // instead of a panic waiting on a future edit.
            0 => return Err(Lzma2Error::Fail),
            1 => {
                let out = enc.encode_one_block(&wave[0], out_lim)?;
                sink.write(&out).map_err(Lzma2Error::Stream)?;
            }
            _ => {
                for out in encode_wave(enc, &wave, out_lim) {
                    sink.write(&out?).map_err(Lzma2Error::Stream)?;
                }
            }
        }

        if last {
            break;
        }
    }

    sink.write(&[CONTROL_EOF]).map_err(Lzma2Error::Stream)
}

/// Encode every block of `wave` concurrently, returning the results **in wave
/// order** whatever order they finished in.
///
/// A worker that panics re-panics on this thread rather than being reported as a
/// codec error: a panic here is a bug in the encoder, and laundering it into
/// `Lzma2Error` would let a corrupt block look like a refused one.
fn encode_wave(
    enc: &Lzma2Enc,
    wave: &[Vec<u8>],
    out_lim: usize,
) -> Vec<Result<Vec<u8>, Lzma2Error>> {
    std::thread::scope(|scope| {
        let handles: Vec<_> = wave
            .iter()
            .map(|block| scope.spawn(move || enc.encode_one_block(block, out_lim)))
            .collect();
        handles
            .into_iter()
            .map(|h| match h.join() {
                Ok(res) => res,
                Err(payload) => std::panic::resume_unwind(payload),
            })
            .collect()
    })
}

/// `SeqInStream_ReadMax` (`7zStream.c:11`): read until the buffer is full or the
/// stream reports end of input, and report how much arrived.
///
/// The short read is the signal, not an error -- it is what tells `MtCoder` this is
/// the final block (`MtCoder.c:140`).
fn read_block(source: &mut dyn InStream, block_size: usize) -> Result<Vec<u8>, Lzma2Error> {
    let mut buf = vec![0u8; block_size];
    let mut filled = 0usize;
    while filled < block_size {
        let n = source
            .read(&mut buf[filled..])
            .map_err(Lzma2Error::Stream)?;
        if n == 0 {
            break;
        }
        filled += n;
    }
    buf.truncate(filled);
    Ok(buf)
}
