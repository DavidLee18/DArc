//! PPMd var.H, ported from `Compression/PPMD/`.
//!
//! Unlike the other codecs here, this one admits no algorithmic latitude: the
//! model branches on its own allocator's state, so the suballocator's layout is
//! part of the compressed format. See [`suballoc`] for the measurement that
//! establishes this.

pub mod suballoc;
