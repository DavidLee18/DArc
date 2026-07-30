//! The streaming I/O boundary — DArc's side of `ISeqInStream` / `ISeqOutStream`.
//!
//! Upstream `lzma-sdk-rs` encodes `&[u8]` to `Vec<u8>`, so its memory cost is
//! O(input). DArc's `lzma_compress` is callback-driven and a solid block can be
//! larger than RAM, so the encoder has to consume its input through a window and
//! push its output away as it goes. These two traits are what the encoder pulls
//! from and pushes to; [`crate::encode`] keeps the in-memory API by wiring a slice
//! and a `Vec` into them.
//!
//! `StreamError` carries the caller's own error code rather than an enum of this
//! crate's invention: the C SDK propagates `SRes` and DArc propagates a
//! `FREEARC_ERRCODE_*`, and re-encoding either into a local taxonomy would lose
//! the distinction the caller has to act on.

/// An error raised by the caller's stream, carrying the caller's own code.
///
/// `SRes` in the C SDK; a `FREEARC_ERRCODE_*` in DArc. Values are opaque here and
/// are returned to the caller unchanged.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StreamError(pub i32);

/// The configuration asked for is not implemented by this crate.
///
/// `SZ_ERROR_UNSUPPORTED` (`7zTypes.h:32`), so a caller that forwards `SRes` codes
/// stays consistent with the C. Callers that speak FreeArc codes should map it to
/// `FREEARC_ERRCODE_NOT_IMPLEMENTED`.
///
/// This exists so that an unported match finder is a **refusal**, not a silent
/// substitution: encoding with a different finder than the caller asked for would
/// produce an archive no other build reproduces, and it would look like success.
pub const ERR_UNSUPPORTED: StreamError = StreamError(4);

/// The encoder's input side — `ISeqInStream`.
pub trait InStream {
    /// Read into `buf`, returning how many bytes were stored.
    ///
    /// **`Ok(0)` means end of stream**, exactly as `ISeqInStream_Read`'s
    /// `size == 0` does. A short read (`0 < n < buf.len()`) is *not* end of
    /// stream: the match finder simply asks again. An implementation that returns
    /// `Ok(0)` while more data exists will silently truncate the archive, which is
    /// the same contract — and the same hazard — as in the C.
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError>;
}

/// The encoder's output side — `ISeqOutStream`.
pub trait OutStream {
    /// Write all of `data`. Partial writes are the implementation's problem, not
    /// the encoder's: `ISeqOutStream_Write` returning less than requested is an
    /// error there too.
    fn write(&mut self, data: &[u8]) -> Result<(), StreamError>;
}

/// A `&[u8]` as an [`InStream`], for the in-memory [`crate::encode`] path.
pub struct SliceIn<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> SliceIn<'a> {
    /// Wrap `data`.
    pub fn new(data: &'a [u8]) -> Self {
        SliceIn { data, pos: 0 }
    }
}

impl InStream for SliceIn<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
        let n = buf.len().min(self.data.len() - self.pos);
        buf[..n].copy_from_slice(&self.data[self.pos..self.pos + n]);
        self.pos += n;
        Ok(n)
    }
}

/// A growable buffer as an [`OutStream`], for the in-memory [`crate::encode`] path.
#[derive(Default)]
pub struct VecOut {
    /// Everything written so far.
    pub data: Vec<u8>,
}

impl OutStream for VecOut {
    fn write(&mut self, data: &[u8]) -> Result<(), StreamError> {
        self.data.extend_from_slice(data);
        Ok(())
    }
}
