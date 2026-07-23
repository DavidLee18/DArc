//! The tagged-chunk stream driver, ported from
//! `Compression/DisPack/C_DisPack.cpp` (`DISPACK_METHOD::decompress` :72).
//!
//! A DisPack stream is a 4-byte `CHUNK_SIZE` followed by chunks, each led by a
//! 4-byte tag:
//!
//! | tag | chunk |
//! |---|---|
//! | any non-tag dword | a full `CHUNK_SIZE` raw block whose first four bytes *are* that dword |
//! | `TAG_DATA` | an explicit-length raw block (the last, partial chunk) |
//! | `TAG_EXE` | a filtered block: out size, in size, `in` bytes → `dis_unfilter` |
//!
//! `is_tag(x)` is `(x ^ TAG_DATA) < 0x10`, i.e. the sixteen values
//! `0xC71B3AE0..=0xC71B3AEF`. A dword in that range that is neither TAG_DATA
//! nor TAG_EXE is corrupt data. Raw data whose leading dword happens to fall in
//! that range would be ambiguous, which is why the encoder only ever emits the
//! two defined tags and this rejects the rest.
//!
//! A running `base_address` (the x86 image address of the current output
//! position) starts at `1<<30`, advances by each chunk's length, and wraps at
//! `3<<30` back by `2<<30` -- the filter needs it to turn relative targets
//! absolute and back.

use super::filter::dis_unfilter;
use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;

const TAG_DATA: u32 = 0xC71B_3AE1;
const TAG_EXE: u32 = 0xC71B_3AE2;

#[inline]
fn is_tag(x: u32) -> bool {
    (x ^ TAG_DATA) < 0x10
}

fn read_exact(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.read(buf) {
        n if n as usize == buf.len() => Ok(()),
        n if n >= 0 => Err(IO),
        n => Err(n),
    }
}

/// `READ4_OR_EOF`: `Ok(None)` on a clean end, `Ok(Some)` on a full word.
fn read_u32_or_eof(io: &Io) -> Result<Option<u32>, c_int> {
    let mut b = [0u8; 4];
    match io.read(&mut b) {
        0 => Ok(None),
        4 => Ok(Some(u32::from_le_bytes(b))),
        n if n >= 0 => Err(IO),
        n => Err(n),
    }
}

fn read_u32(io: &Io) -> Result<u32, c_int> {
    let mut b = [0u8; 4];
    read_exact(io, &mut b)?;
    Ok(u32::from_le_bytes(b))
}

fn write_out(io: &Io, buf: &[u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.write(buf) {
        n if n < 0 => Err(n),
        _ => Ok(()),
    }
}

/// `DISPACK_METHOD::decompress`. `block_size` is the method's block size, which
/// bounds `CHUNK_SIZE` and the buffers -- an untrusted length otherwise sizes an
/// allocation.
pub fn decompress(io: &Io, block_size: u32) -> c_int {
    match run(io, block_size) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io, block_size: u32) -> Result<(), c_int> {
    let chunk_size = match read_u32_or_eof(io)? {
        None => return Ok(()), // empty input
        Some(c) => c,
    };
    if chunk_size > block_size {
        return Err(BAD);
    }
    let chunk_size = chunk_size as usize;

    let mut base_address: u32 = 1 << 30;

    loop {
        let tag = match read_u32_or_eof(io)? {
            None => return Ok(()), // clean EOF between chunks
            Some(t) => t,
        };

        if !is_tag(tag) || tag == TAG_DATA {
            // Raw block. For a non-tag dword the four bytes already read are the
            // block's first four and the block is a full CHUNK_SIZE; for
            // TAG_DATA an explicit length follows.
            let (mut buf, len) = if tag == TAG_DATA {
                let len = read_u32(io)? as usize;
                if len > block_size as usize {
                    return Err(BAD);
                }
                (Vec::new(), len)
            } else {
                let mut v = tag.to_le_bytes().to_vec();
                v.resize(chunk_size.max(4), 0);
                (v, chunk_size)
            };
            if len < 4 && tag != TAG_DATA {
                // A non-tag raw chunk always carries the four bytes just read.
                return Err(BAD);
            }
            if tag == TAG_DATA {
                buf.resize(len, 0);
                read_exact(io, &mut buf[..len])?;
            } else {
                // The first four bytes are `tag`; read the remaining len-4.
                read_exact(io, &mut buf[4..len])?;
            }
            write_out(io, &buf[..len])?;
            base_address = base_address.wrapping_add(len as u32);
        } else if tag == TAG_EXE {
            let out_size = read_u32(io)? as usize;
            let in_size = read_u32(io)? as usize;
            if out_size > block_size as usize
                || in_size > (block_size + block_size / 4 + 1024) as usize
            {
                return Err(BAD);
            }
            let mut in_buf = vec![0u8; in_size];
            read_exact(io, &mut in_buf)?;
            let out = dis_unfilter(&in_buf, out_size, base_address).ok_or(BAD)?;
            if out.len() != out_size {
                return Err(BAD);
            }
            write_out(io, &out)?;
            base_address = base_address.wrapping_add(out_size as u32);
        } else {
            // is_tag but neither TAG_DATA nor TAG_EXE.
            return Err(BAD);
        }

        if base_address >= 3 << 30 {
            base_address -= 2 << 30;
        }
    }
}
