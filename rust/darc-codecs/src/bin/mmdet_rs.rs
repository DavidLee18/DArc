//! The Rust side of `rust/difftest/mmdet-check.sh`.
//!
//! Prints exactly what `mmdet_ref.cpp` prints, so the harness can `cmp` the two
//! lines:
//!
//!     <datatype> <is_mm-fast> <is_mm-thorough> <is_mm_header> <mm_bytes>

use darc_codecs::mmdet;
use std::io::Read;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) == Some("--types") {
        println!("{}", mmdet::DETECTABLE_TYPES);
        return;
    }

    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf).expect("read stdin");

    let ty = mmdet::detect_datatype(&buf).name();
    let fast = u8::from(mmdet::detect::is_mm(1, &buf));
    let thorough = u8::from(mmdet::detect::is_mm(3, &buf));
    let header = u8::from(mmdet::detect::is_mm_header(1, &buf));
    let bytes = mmdet::detect::mm_bytes(3, buf.len() as i64);

    println!("{ty} {fast} {thorough} {header} {bytes}");
}
