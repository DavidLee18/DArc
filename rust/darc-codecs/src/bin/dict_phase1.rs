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
    let upto: u32 = std::env::var("DICT_PHASE").ok().and_then(|v| v.parse().ok()).unwrap_or(1);
    let mut e = darc_codecs::dict_encode::Encoder::new();
    e.phase1(&buf);
    if upto == 1 {
        print!("{}", e.dump_phase1());
        return;
    }
    // Same tuning defaults as the C driver (parse_DICT).
    if e.phase2(200, 200, 200, 0).is_err() {
        println!("phase2 rejected");
        return;
    }
    if upto == 2 {
        print!("{}", e.dump_words());
        return;
    }
    match e.phase3(0) {
        Err(_) => { println!("phase3 rejected"); }
        Ok(nodes) => {
            if upto == 3 {
                println!("nodes {} prefix {}", nodes, e.prefix());
                print!("{}", e.dump_words());
                print!("{}", e.dump_char_counts());
                return;
            }
            if e.phase4(nodes).is_err() { println!("phase4 rejected"); return; }
            if upto == 4 { print!("{}", e.dump_coded_words()); return; }
            match e.phase5() {
                Err(_) => println!("phase5 rejected"),
                Ok(mut dict) => {
                    use std::io::Write;
                    if upto == 5 {
                        std::io::stdout().write_all(&dict).unwrap();
                        return;
                    }
                    if e.phase6().is_err() { println!("phase6 rejected"); return; }
                    dict.extend_from_slice(&e.phase7());
                    std::io::stdout().write_all(&dict).unwrap();
                }
            }
        }
    }
}
