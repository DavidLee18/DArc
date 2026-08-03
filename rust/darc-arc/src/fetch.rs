//! `--original http://...` — the remote second copy an archive is repaired from.
//!
//! The port of `URL.cpp`, whose whole design is that the archive is never
//! downloaded. `url_seek` moves a cursor and nothing else; every read issues one
//! `CURLOPT_RANGE` GET for exactly the bytes wanted, and `url_size` asks for the
//! length alone. `ArcRecover.hs` then reads only the sectors the parity could
//! not repair — usually a few KB out of a file that may be gigabytes.
//!
//! Reproducing that is the point. Fetching the whole copy would work and would
//! make `-rr0.1%`, documented as "for recovery over the internet only",
//! pointless.
//!
//! # Why ureq
//!
//! libcurl is a build-time system dependency; `-DFREEARC_NOURL` exists because
//! it is so often absent. ureq is pure Rust and blocking, and its TLS is rustls
//! with the CA roots compiled in — no OpenSSL, no system trust store — so the
//! mingw and aarch64 cross-builds get HTTPS with nothing on the far side. The
//! `url` cargo feature switches the whole module off the way `FREEARC_NOURL`
//! switches off the C's.

use crate::recovery::Original;

/// A copy of the archive reachable over HTTP, read by range.
pub struct Url {
    url: String,
    agent: ureq::Agent,
    /// `Content-Length`, asked for once and remembered. The outer `Option` is
    /// "not yet asked", the inner is "asked, and the server would not say".
    size: Option<Option<u64>>,
}

impl Url {
    pub fn new(url: &str) -> Self {
        Self {
            url: url.to_string(),
            // The reference sets a user agent (URL.cpp:295) and honours the
            // proxy environment, which ureq does by default.
            agent: ureq::Agent::config_builder()
                .user_agent(concat!("DArc/", env!("CARGO_PKG_VERSION")))
                .build()
                .into(),
            size: None,
        }
    }

    /// `url_size` — `HEAD`, for the same-size check.
    fn fetch_size(&mut self) -> Option<u64> {
        let resp = self.agent.head(&self.url).call().ok()?;
        resp.headers()
            .get("content-length")?
            .to_str()
            .ok()?
            .trim()
            .parse()
            .ok()
    }
}

impl Original for Url {
    fn size(&mut self) -> Option<u64> {
        match self.size {
            Some(cached) => cached,
            None => {
                let got = self.fetch_size();
                self.size = Some(got);
                got
            }
        }
    }

    fn read_at(&mut self, offset: u64, len: usize) -> Option<Vec<u8>> {
        if len == 0 {
            return Some(Vec::new());
        }
        // `sprintf (Range, "%.0lf-%.0lf", offset, offset+size-1)` (URL.cpp:327)
        // -- HTTP ranges are INCLUSIVE at both ends, so the last byte is
        // offset+len-1 and an off-by-one here would silently shift every
        // repaired sector by a byte. The CRC check downstream would reject the
        // sector rather than corrupt it, but it would reject every one.
        let last = offset.checked_add(len as u64)?.checked_sub(1)?;
        let resp = self
            .agent
            .get(&self.url)
            .header("Range", format!("bytes={offset}-{last}"))
            .call()
            .ok()?;
        // 206 is a served range. A 200 means the server ignored the header and
        // is sending the WHOLE file, in which case the body starts at zero and
        // the wanted bytes have to be cut out of it -- correct, but it defeats
        // the point, so say so once rather than quietly downloading gigabytes.
        let partial = resp.status().as_u16() == 206;
        let body = resp.into_body().read_to_vec().ok()?;
        match partial {
            true => match body.len() == len {
                true => Some(body),
                // A short or over-long range is a server that answered a
                // different question. Refuse it rather than pad or truncate.
                false => None,
            },
            false => {
                let at = usize::try_from(offset).ok()?;
                body.get(at..at.checked_add(len)?).map(<[u8]>::to_vec)
            }
        }
    }
}
