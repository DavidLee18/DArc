//! Dict: dictionary preprocessor, ported from Compression/Dict/dict.cpp.
//!
//! Dict replaces frequent words in the input with one- or two-byte codes and
//! prepends the dictionary needed to reverse it. Decompression is ported first,
//! for the same reason as in Delta: the dictionary is carried in the stream, so
//! this code only replays the encoder's decisions, and it can be validated
//! against output the C encoder produced.
//!
//! The C decoder trusts its input completely, and this is where three of the
//! defects fixed on the previous branch lived:
//!
//!   * `get_byte()` is `*ptr++` with no check against `end`. The dictionary
//!     header alone reads hundreds of bytes before the decode loop's `ptr<end`
//!     test is ever consulted.
//!   * `put_word` is a `memcpy` into `outbuf` with no capacity check at all.
//!   * the word-text buffer is sized, in the original's own words, "by
//!     guesswork, but with a big margin", and the two-byte word loop then reads
//!     until a separator byte with no bound.
//!
//! Every one of those is a heap overflow reachable from a corrupt or hostile
//! archive. This port bounds all three and returns
//! FREEARC_ERRCODE_BAD_COMPRESSED_DATA instead.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

/// A `dict[i].len` of exactly 1 means "this byte introduces a two-byte code"
/// rather than "a one-byte word of length 1" -- the encoder never emits a
/// single-character word, which is what makes the overload unambiguous.
const USE_DICT2: u32 = 1;
const N: usize = 256;

#[derive(Clone, Copy, Default)]
struct Entry {
    len: u32,
    /// Offset into the word-text buffer; `len == 0` means unused.
    at: usize,
}

struct Reader<'a> {
    buf: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn byte(&mut self) -> Result<u8, c_int> {
        // The C original is `*ptr++`, unchecked. This is the check.
        let b = *self.buf.get(self.pos).ok_or(FREEARC_ERRCODE_BAD_COMPRESSED_DATA)?;
        self.pos += 1;
        Ok(b)
    }
    fn at_end(&self) -> bool {
        self.pos >= self.buf.len()
    }
}

/// Port of `DictDecode`. Returns the decoded bytes.
pub fn decode(input: &[u8], out_limit: usize) -> Result<Vec<u8>, c_int> {
    let mut r = Reader { buf: input, pos: 0 };
    let mut dict = [Entry::default(); N];
    let mut dict2 = vec![Entry::default(); N * N];
    let mut words: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();

    // 1. Lengths of the one-byte-coded words.
    for e in dict.iter_mut() {
        e.len = r.byte()? as u32;
    }
    // 2. Lengths of the two-byte-coded words, for each introducer byte.
    for i in 0..N {
        if dict[i].len == USE_DICT2 {
            for j in 0..N {
                dict2[i * N + j].len = r.byte()? as u32;
            }
        }
    }
    // 3. Text of the one-byte words.
    for i in 0..N {
        if dict[i].len == USE_DICT2 {
            continue;
        }
        dict[i].at = words.len();
        for _ in 0..dict[i].len {
            let b = r.byte()?;
            words.push(b);
        }
    }
    // 4. Text of the two-byte words. Each is a prefix copied from the previous
    //    word plus a tail read up to `word_sep`.
    let word_sep = r.byte()?;
    let mut prev: Option<(usize, u32)> = None; // (offset, len) of the previous word
    for i in 0..N {
        if dict[i].len != USE_DICT2 {
            continue;
        }
        for j in 0..N {
            let start = words.len();
            let take = dict2[i * N + j].len;
            if take > 0 {
                // Copying from a word that does not exist is malformed input;
                // the C version dereferences a NULL prevptr here, which it does
                // at least check for -- but not the length.
                let (poff, plen) = prev.ok_or(FREEARC_ERRCODE_BAD_COMPRESSED_DATA)?;
                if take > plen {
                    return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
                }
                for k in 0..take as usize {
                    let b = words[poff + k];
                    words.push(b);
                }
            }
            loop {
                let c = r.byte()?;
                if c == word_sep {
                    break;
                }
                words.push(c);
                // The C word buffer is sized by guesswork; this bounds it
                // against the only thing that can legitimately fill it.
                if words.len() > input.len().saturating_add(1 << 20) {
                    return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
                }
            }
            dict2[i * N + j].at = start;
            dict2[i * N + j].len = (words.len() - start) as u32;
            prev = Some((start, dict2[i * N + j].len));
        }
    }

    // 5. Pseudo-words for characters that gave their code away to a word.
    let prefix = r.byte()? as usize;
    dict[prefix].len = USE_DICT2;
    for j in 0..N {
        dict2[prefix * N + j].len = 1;
        dict2[prefix * N + j].at = words.len();
        words.push(j as u8);
    }

    // Decode the text.
    let push = |out: &mut Vec<u8>, src: &[u8]| -> Result<(), c_int> {
        if out.len() + src.len() > out_limit {
            // `put_word` is an unchecked memcpy in the original.
            return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        }
        out.extend_from_slice(src);
        Ok(())
    };
    while !r.at_end() {
        let c = r.byte()? as usize;
        let d = dict[c];
        if d.len == 0 {
            push(&mut out, &[c as u8])?;
        } else if d.len == USE_DICT2 {
            let c2 = r.byte()? as usize;
            let e = dict2[c * N + c2];
            let end = e.at.checked_add(e.len as usize).ok_or(FREEARC_ERRCODE_BAD_COMPRESSED_DATA)?;
            if end > words.len() {
                return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            }
            let w = words[e.at..end].to_vec();
            push(&mut out, &w)?;
        } else {
            let end = d.at.checked_add(d.len as usize).ok_or(FREEARC_ERRCODE_BAD_COMPRESSED_DATA)?;
            if end > words.len() {
                return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            }
            let w = words[d.at..end].to_vec();
            push(&mut out, &w)?;
        }
    }
    Ok(out)
}

/// Port of `dict_decompress` (C_Dict.cpp). Block framing:
///   i32 InSize   negative -> that many bytes follow, stored uncompressed
///                zero-length read -> clean end of stream
#[allow(clippy::too_many_arguments)]
pub fn decompress(io: &Io, block_size: u32) -> c_int {
    let block_size = block_size.max(1) as usize;
    loop {
        let mut hdr = [0u8; 4];
        match io.read(&mut hdr) {
            0 => return OK,
            4 => {}
            n if n < 0 => return n,
            _ => return FREEARC_ERRCODE_IO,
        }
        let in_size = i32::from_le_bytes(hdr);

        if in_size < 0 {
            // Stored block: copied through unchanged.
            let n = (-(in_size as i64)) as usize;
            let mut raw = match crate::ffi::archive_sized_buffer(n, block_size as u32) {
                Ok(b) => b,
                Err(e) => return e,
            };
            if io.read(&mut raw) as usize != n {
                return FREEARC_ERRCODE_IO;
            }
            // Propagate, do not substitute: a negative write is not
            // necessarily an error. See the note on Io::write.
            let w = io.write(&raw);
            if w < 0 {
                return w;
            }
            continue;
        }

        let n = in_size as usize;
        let mut packed = match crate::ffi::archive_sized_buffer(n, block_size as u32) {
            Ok(b) => b,
            Err(e) => return e,
        };
        if n != 0 {
            let got = io.read(&mut packed);
            if got < 0 {
                return got;
            }
            if got as usize != n {
                return FREEARC_ERRCODE_IO;
            }
        }
        match decode(&packed, block_size) {
            Ok(out) => {
                if !out.is_empty() {
                    let w = io.write(&out);
                    if w < 0 {
                        return w;
                    }
                }
            }
            Err(e) => return e,
        }
    }
}
