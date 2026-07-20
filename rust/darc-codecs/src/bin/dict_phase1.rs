//! Dump the Rust phase1's result in the same format as
//! rust/difftest/dict_phase1_ref.cpp, so the two can be diffed directly.
//!
//! Exists because DictEncode runs all seven phases: without a per-phase
//! comparison the port has no validation until it is finished, which means
//! debugging ~600 lines at once.
use std::io::Read;

fn main() {
    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf).expect("read stdin");
    let mut e = darc_codecs::dict_encode::Encoder::new();
    e.phase1(&buf);
    print!("{}", e.dump_phase1());
}
