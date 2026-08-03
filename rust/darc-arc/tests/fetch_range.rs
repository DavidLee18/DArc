//! `--original http://...`, end to end.
//!
//! What has to be true is not just "the repair worked" — slurping the whole
//! copy would also make the repair work. It is that the copy is read BY RANGE,
//! because that is the only reason `-rr0.1%` ("for recovery over the internet
//! only") is worth having: a few KB of recovery records plus a few ranged reads
//! instead of re-fetching a file that may be gigabytes.
//!
//! So the server here counts what it is asked for and refuses to serve a
//! whole-file GET, and the test asserts on the request log as much as on the
//! recovered bytes. A port that fetched the archive once and indexed into it
//! would pass every byte-comparison in this file and fail on the log.
#![cfg(feature = "url")]
// An integration test is its own crate, so it does not inherit lib.rs's lints:
// `if let` is banned workspace-wide, which makes `single_match` the shape every
// one-armed destructure here has to take.
#![allow(clippy::single_match)]

use std::io::{BufRead, BufReader, Write};
use std::sync::{Arc, Mutex};

/// What the server was asked for, so the test can prove the reads were ranged.
#[derive(Default)]
struct Log {
    /// One entry per GET: `Some((first, last))` for a ranged request, `None`
    /// for a request for the whole file.
    gets: Vec<Option<(u64, u64)>>,
    heads: usize,
}

/// A minimal HTTP/1.1 server that honours a single `Range: bytes=A-B`.
///
/// Hand-rolled rather than pulled in: a test server is not worth a dependency,
/// and this one has to misbehave on demand (`serve_whole`) in ways a real one
/// will not.
///
/// One request per connection, so every response says `Connection: close`.
/// Without it ureq keeps the socket alive and sends the next request down a
/// connection this loop has already dropped — which fails intermittently,
/// depending on whether the pool happened to reuse it.
fn serve(body: Vec<u8>, serve_whole: bool) -> (String, Arc<Mutex<Log>>) {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = listener.local_addr().expect("addr").port();
    let log = Arc::new(Mutex::new(Log::default()));
    let log_thread = Arc::clone(&log);

    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let mut stream = match stream {
                Ok(s) => s,
                Err(_) => continue,
            };
            let mut reader = BufReader::new(stream.try_clone().expect("clone"));
            let mut request = String::new();
            if reader.read_line(&mut request).is_err() {
                continue;
            }
            let is_head = request.starts_with("HEAD");
            let mut range = None;
            loop {
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(0) => break,
                    Ok(_) => {}
                    Err(_) => break,
                }
                let trimmed = line.trim_end();
                if trimmed.is_empty() {
                    break;
                }
                let lower = trimmed.to_ascii_lowercase();
                match lower.strip_prefix("range:") {
                    Some(v) => {
                        let spec = v.trim().trim_start_matches("bytes=");
                        match spec.split_once('-') {
                            Some((a, b)) => {
                                range = a
                                    .trim()
                                    .parse::<u64>()
                                    .ok()
                                    .zip(b.trim().parse::<u64>().ok());
                            }
                            None => {}
                        }
                    }
                    None => {}
                }
            }

            let mut log = log_thread.lock().expect("lock");
            match is_head {
                true => {
                    log.heads += 1;
                    drop(log);
                    let head = format!(
                        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nAccept-Ranges: bytes\r\nConnection: close\r\n\r\n",
                        body.len()
                    );
                    drop(stream.write_all(head.as_bytes()));
                    continue;
                }
                false => log.gets.push(range),
            }
            drop(log);

            // `serve_whole` models the server that ignores Range and answers
            // 200 with everything -- legal HTTP, and the case `read_at` has to
            // cut the wanted bytes out of by hand.
            let (status, slice) = match range {
                Some((a, b)) if !serve_whole => {
                    let a = a as usize;
                    let b = (b as usize).min(body.len().saturating_sub(1));
                    ("206 Partial Content", &body[a..=b])
                }
                _ => ("200 OK", &body[..]),
            };
            let head = format!(
                "HTTP/1.1 {status}\r\nContent-Length: {}\r\nAccept-Ranges: bytes\r\nConnection: close\r\n\r\n",
                slice.len()
            );
            drop(stream.write_all(head.as_bytes()).and_then(|()| stream.write_all(slice)));
        }
    });

    (format!("http://127.0.0.1:{port}/copy.arc"), log)
}

fn body() -> Vec<u8> {
    (0..8192u32).map(|i| (i.wrapping_mul(31) >> 3) as u8).collect()
}

#[test]
fn a_ranged_read_returns_exactly_the_bytes_asked_for() {
    use darc_arc::recovery::Original;

    let data = body();
    let (url, log) = serve(data.clone(), false);
    let mut src = darc_arc::fetch::Url::new(&url);

    assert_eq!(src.size(), Some(data.len() as u64), "Content-Length from the HEAD");
    // Asked once and remembered: a second call must not cost a second request.
    assert_eq!(src.size(), Some(data.len() as u64));
    assert_eq!(log.lock().expect("lock").heads, 1, "the size is cached");

    for (offset, len) in [(0u64, 1usize), (0, 512), (512, 512), (4095, 2), (8191, 1)] {
        let got = src.read_at(offset, len).expect("range served");
        let at = offset as usize;
        assert_eq!(got, &data[at..at + len], "bytes at {offset}+{len}");
    }

    // The ranges are INCLUSIVE at both ends. An off-by-one would still return
    // `len` bytes for most of the rows above, so check the wire form directly.
    let log = log.lock().expect("lock");
    assert_eq!(
        log.gets,
        vec![
            Some((0, 0)),
            Some((0, 511)),
            Some((512, 1023)),
            Some((4095, 4096)),
            Some((8191, 8191)),
        ],
        "every GET carried the exact inclusive range"
    );
}

#[test]
fn a_server_that_ignores_range_is_still_read_correctly() {
    use darc_arc::recovery::Original;

    let data = body();
    let (url, log) = serve(data.clone(), true);
    let mut src = darc_arc::fetch::Url::new(&url);

    let got = src.read_at(1000, 64).expect("200 fallback");
    assert_eq!(got, &data[1000..1064], "the wanted bytes cut out of a 200");
    assert_eq!(log.lock().expect("lock").gets.len(), 1);
}

#[test]
fn a_read_past_the_end_is_refused_rather_than_padded() {
    use darc_arc::recovery::Original;

    let data = body();
    let (url, _log) = serve(data.clone(), false);
    let mut src = darc_arc::fetch::Url::new(&url);

    // The server clamps to the last byte, so this comes back short. Padding or
    // truncating it would hand `recover_with` a sector that is not what was
    // asked for; it must come back as "unavailable" instead.
    assert_eq!(src.read_at(8190, 64), None, "a short range is not a partial success");
    assert_eq!(src.read_at(0, 0), Some(Vec::new()), "an empty read costs no request");
}

/// The whole path: `darc r --original http://...` over an archive damaged past
/// what its own parity can repair.
#[test]
fn arc_r_repairs_from_a_remote_copy_without_downloading_it() {
    let darc = env!("CARGO_BIN_EXE_darc");
    let w = std::env::temp_dir().join(format!("darc-fetch-{}", std::process::id()));
    drop(std::fs::remove_dir_all(&w));
    std::fs::create_dir_all(w.join("src")).expect("mkdir");
    for (name, fill) in [("a.bin", 0x41u8), ("b.bin", 0x42), ("c.bin", 0x43)] {
        std::fs::write(w.join("src").join(name), vec![fill; 3000]).expect("write");
    }

    let run = |args: &[&str]| {
        std::process::Command::new(darc)
            .args(args)
            .current_dir(&w)
            .output()
            .expect("run darc")
    };
    let out = run(&["a", "--nodates", "-y", "-m0", "-rr8%", "arc.arc", "src"]);
    assert!(out.status.success(), "create: {}", String::from_utf8_lossy(&out.stderr));

    let good = std::fs::read(w.join("arc.arc")).expect("read archive");
    assert!(good.len() > 4096, "archive is {} bytes", good.len());

    // Damage every sector in the FIRST HALF, which is well inside the protected
    // data: the recovery records live at the tail, and `recover_with` copies
    // everything outside the protected region through verbatim -- damage there
    // would survive the repair and has nothing to do with `--original`.
    //
    // Every sector rather than one, because parity repairs a group that has
    // exactly one bad member. Damaging them all leaves the copy as the only way
    // back, so the repaired file must come out equal to `good` byte for byte.
    let mut broken = good.clone();
    let half = good.len() / 2;
    for (i, byte) in broken.iter_mut().enumerate().take(half) {
        if i % 512 == 7 {
            *byte ^= 0xFF;
        }
    }
    assert!(broken != good, "the damage loop changed nothing");
    std::fs::write(w.join("arc.arc"), &broken).expect("write damaged");

    // First WITHOUT the copy, to prove the damage is past what parity can do.
    // Skip this and the test would still pass on a build that never fetched
    // anything, because the parity alone would have been enough.
    let out = run(&["r", "arc.arc"]);
    let bare = std::fs::read(w.join("fixed.arc.arc")).ok();
    assert!(
        bare.as_ref() != Some(&good),
        "parity alone repaired it -- this test proves nothing about --original\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // `r` refuses to overwrite an existing `fixed.` file.
    drop(std::fs::remove_file(w.join("fixed.arc.arc")));

    let (url, log) = serve(good.clone(), false);
    let out = run(&["r", &format!("--original={url}"), "arc.arc"]);
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    let fixed = std::fs::read(w.join("fixed.arc.arc"))
        .unwrap_or_else(|e| panic!("no fixed.arc.arc: {e}\nstderr: {stderr}"));

    assert_eq!(fixed.len(), good.len(), "size preserved");
    assert!(fixed == good, "the repaired archive is not the original\nstderr: {stderr}");

    // ...and it was never downloaded. Every GET carried a Range, and no single
    // one asked for more than a sector.
    let log = log.lock().expect("lock");
    assert!(log.gets.len() > 1, "expected many ranged reads, saw {}", log.gets.len());
    for get in &log.gets {
        match get {
            Some((a, b)) => assert!(
                b - a < 4096,
                "a single request asked for {} bytes -- that is a download, not a range",
                b - a + 1
            ),
            None => panic!("a GET was issued with no Range header"),
        }
    }

    drop(std::fs::remove_dir_all(&w));
}
