//! Recovery records — `writeRecoveryBlocks` (`ArcRecover.hs:76`).
//!
//! # What is stored
//!
//! The archive is cut into sectors of a chosen size. For each sector a CRC32 is
//! recorded, and each sector is XORed into recovery sector `i mod N`. A sector
//! whose CRC no longer matches is damaged; if no more than one damaged sector
//! maps onto a given recovery sector, its contents are recoverable by XORing
//! the recovery sector with the other sectors that share it.
//!
//! Two blocks are written, both `RECOVERY_BLOCK` and both stored:
//!
//! * the **XOR sectors**, `rec_sectors * sector_size` bytes and nothing else;
//! * the **CRCs**, preceded by a small header describing the geometry.
//!
//! They are separate so that the second alone is enough to *check* an archive,
//! and so that damage to the bulky first block does not cost the ability to
//! detect damage elsewhere: a sector "recovered" from a corrupt XOR sector
//! simply fails its CRC.
//!
//! # The layout this produces
//!
//! ```text
//!   [header][data blocks][dir block][footer #1]   <- the protected region
//!   [recovery block 0][recovery block 1]
//!   [footer #2]                                   <- lists the recovery blocks
//! ```
//!
//! The footer is written TWICE (`ArcvProcessRead.hs:93`): once to close the
//! archive, and again after the recovery blocks so that they appear in the
//! block list. The first footer is inside the protected region, which is why
//! `arcsize` is measured up to it and not to the end of the file.

use crate::block::{ArchiveBlock, BlockType};
use crate::bytestream::OutStream;
use crate::crc;

/// One CRC is four bytes — `sizeOf (undefined::CRC)`.
const CRC_SIZE: u64 = 4;

/// `aRarRecSectorSize` — the sector size a bare number in `-rr` counts in,
/// kept only because the option is RAR-compatible.
const RAR_SECTOR: u64 = 512;

/// The geometry `writeRecoveryBlocks` computes before it touches any bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Geometry {
    pub sector_size: u64,
    /// Sectors the protected region is cut into.
    pub arc_sectors: u64,
    /// XOR sectors. May be **zero**: `-rr0%` stores CRCs and nothing else,
    /// which detects damage without being able to repair it.
    pub rec_sectors: u64,
    /// Bytes of CRCs, including one per XOR sector.
    pub crcs_size: u64,
}

/// `aRecVersion` — `"0.39"` when there are no XOR sectors, `"0.36"` otherwise.
///
/// The version is the first thing in the CRC block's header and is checked
/// against a whitelist on read, so it is format rather than provenance.
pub fn version(rec_sectors: u64) -> &'static str {
    match rec_sectors {
        0 => "0.39",
        _ => "0.36",
    }
}

/// `recommended_rr` — the default amount, by archive size.
pub fn recommended(arcsize: u64) -> &'static str {
    match arcsize {
        n if n < 300_000 => "4%",
        n if n < 2_000_000 => "2%",
        _ => "1%",
    }
}

/// Resolve the `-rr` option against what the input archive recorded
/// (`ArcRecover.hs:89`).
///
/// `""` means "add none". The `0.1%`/`0.01%` spellings are rewritten in
/// `Cmdline.hs` before they reach here, so they are not repeated.
pub fn resolve(option: &str, old: &str, arcsize: u64) -> String {
    match option {
        // -rr-: never add recovery info.
        "-" => String::new(),
        // the default: whatever the archive already said.
        "--" => old.to_string(),
        // -rr / -rr+: the archive's setting, or the recommendation if it had
        // none.
        "" | "+" => match old.is_empty() {
            true => recommended(arcsize).to_string(),
            false => old.to_string(),
        },
        r => r.to_string(),
    }
}

/// `rr_ok` (`Cmdline.hs:641`) — whether the `-rr` value is one the command line
/// accepts, checked BEFORE any of it is interpreted.
///
/// `"+"` is accepted here because `writeRecoveryBlocks` has a case for it
/// (`ArcRecover.hs:93`): `-rr+` means the archive's own setting, or the
/// recommended amount if it had none — the same as a bare `-rr`. The validation
/// runs first, so omitting `"+"` made that case unreachable and rejected a
/// documented spelling; both this and `Cmdline.hs:641` now list it.
///
/// The size suffixes all pass because `parseNumber` reports `'b'` for every one
/// of `b k m g t ^` — only a character it does not know comes back as itself.
pub fn option_is_valid(s: &str) -> bool {
    if matches!(s, "" | "+" | "-" | "--") || s.contains(';') || s.contains('*') {
        return true;
    }
    let lowered = s.to_ascii_lowercase();
    let digits = lowered.chars().take_while(char::is_ascii_digit).count();
    // `span isDigit (num ++ [default_specifier])`: with nothing after the
    // digits the appended 'b' is what is examined.
    matches!(
        lowered[digits..].chars().next().unwrap_or('b'),
        'b' | 'k' | 'm' | 'g' | 't' | '^' | '%' | 'p'
    )
}

/// `lb` (`Utils.hs:103`) — floor(log2), with `lb 0 = lb 1 = 0`.
fn lb(mut n: u64) -> u32 {
    let mut r = 0;
    while n > 1 {
        n /= 2;
        r += 1;
    }
    r
}

/// `parseNumber recovery_amount 's'` — the amount of recovery info wanted, in
/// bytes.
///
/// The default specifier is `'s'`, so a bare number counts 512-byte RAR
/// sectors; `%` and `p` are both a percentage of the archive.
fn wanted_size(amount: &str, arcsize_1p: u64) -> Option<u64> {
    let lowered = amount.to_ascii_lowercase();
    let digits: String = lowered.chars().take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        return None;
    }
    let n: u64 = digits.parse().ok()?;
    match lowered[digits.len()..].chars().next().unwrap_or('s') {
        'b' => Some(n),
        'k' => n.checked_mul(1024),
        'm' => n.checked_mul(1024 * 1024),
        'g' => n.checked_mul(1024 * 1024 * 1024),
        's' => n.checked_mul(RAR_SECTOR),
        '%' | 'p' => n.checked_mul(arcsize_1p),
        _ => None,
    }
}

/// Work out the geometry for `recovery` over an archive of `arcsize` bytes.
///
/// `mem_limit` caps the wanted size at half the physical memory, as the C does.
/// Passing a large value effectively disables the cap; it is a parameter so the
/// result does not depend on the machine the test runs on.
pub fn geometry(recovery: &str, arcsize: u64, mem_limit: u64) -> Option<Geometry> {
    let arcsize_1p = arcsize.div_ceil(100);

    // "amount;sector_size" fixes the sector size; "sectors*sector_size" fixes
    // both the size and the count and leaves no amount to compute.
    let (amount, explicit_rec_size, explicit_sector_size) = match (
        recovery.split_once(';'),
        recovery.split_once('*'),
    ) {
        (Some((r, ss)), _) => (r.to_string(), None, crate::filter::parse_size(ss)),
        (None, Some((ns, ss))) => {
            let n = crate::filter::parse_size(ns)?;
            let s = crate::filter::parse_size(ss)?;
            (String::new(), n.checked_mul(s), Some(s))
        }
        (None, None) => (recovery.to_string(), None, None),
    };

    // An empty amount is 0 rather than an error: "N*SS" leaves it empty.
    let wanted = match amount.is_empty() {
        true => 0,
        false => wanted_size(&amount, arcsize_1p)?,
    }
    .min(mem_limit);

    let sector_size = match explicit_sector_size {
        Some(s) if s > 0 => s,
        // -rr0%: CRCs of 4 KB sectors and no XOR sectors at all.
        _ => match wanted {
            0 => 4096,
            _ => (1u64 << lb(40 * arcsize / wanted)).max(512),
        },
    };

    let arc_sectors = arcsize.div_ceil(sector_size);
    let crcs_size0 = arc_sectors * CRC_SIZE;
    // The block must at least hold the CRCs; a smaller request is raised.
    let rec_size = explicit_rec_size.unwrap_or_else(|| wanted.max(crcs_size0));
    let rec_sectors = rec_size.saturating_sub(crcs_size0).div_ceil(sector_size);

    Some(Geometry {
        sector_size,
        arc_sectors,
        rec_sectors,
        crcs_size: crcs_size0 + rec_sectors * CRC_SIZE,
    })
}

/// The two blocks' bodies, given the protected bytes.
///
/// `protected` is the archive from `init_pos` up to where the recovery info
/// starts — everything through the first footer.
pub struct Bodies {
    /// The XOR sectors. Empty when `rec_sectors` is 0.
    pub sectors: Vec<u8>,
    /// The header and the CRCs.
    pub crcs: Vec<u8>,
}

/// Build both bodies. `crcs_offset` is the distance from the START of the CRC
/// block back to `init_pos`, which is what the header records so a reader can
/// find the protected region from the block's own position.
pub fn build(g: &Geometry, protected: &[u8], crcs_offset: u64) -> Bodies {
    let ss = g.sector_size as usize;
    let mut sectors = vec![0u8; (g.rec_sectors * g.sector_size) as usize];

    // `i' <- ref ((-arc_sectors) `mod` rec_sectors)` -- the counter does NOT
    // start at zero. Starting it so that the LAST archive sector lands on the
    // last recovery sector is what guarantees recovery from damage to any run
    // of `rec_sectors` consecutive sectors, including a run that straddles the
    // boundary between the data and the recovery info itself.
    let mut idx = match g.rec_sectors {
        0 => 0,
        n => (n - (g.arc_sectors % n)) % n,
    };

    let mut out = OutStream::new();
    // The CRC block's header. The version guards against reading the rest as
    // meta-information when it is not.
    out.string(version(g.rec_sectors));
    assert!(out.varint(protected.len() as u64), "protected size is unrepresentable");
    assert!(out.varint(crcs_offset), "offset is unrepresentable");
    // A list of (sector_size, rec_sectors) "compartments"; DArc writes one.
    out.list(&[(g.sector_size, g.rec_sectors)], |o, (ss, rs)| {
        assert!(o.varint(*ss), "sector size is unrepresentable");
        assert!(o.varint(*rs), "recovery sector count is unrepresentable");
    });

    for chunk in protected.chunks(ss) {
        out.crc(crc::calc(chunk));
        if g.rec_sectors > 0 {
            let base = idx as usize * ss;
            for (i, b) in chunk.iter().enumerate() {
                sectors[base + i] ^= b;
            }
            idx = (idx + 1) % g.rec_sectors;
        }
    }
    // …then the CRCs of the XOR sectors themselves, so that a damaged recovery
    // sector is detectable rather than silently used.
    for i in 0..g.rec_sectors as usize {
        out.crc(crc::calc(&sectors[i * ss..(i + 1) * ss]));
    }

    Bodies { sectors, crcs: out.into_bytes() }
}

/// What the CRC block's header describes — `readControlInfo`
/// (`ArcRecover.hs:200`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Control {
    /// Where the protected region starts. Recorded as a distance BACK from the
    /// CRC block, so it survives the archive being moved or an SFX stub
    /// changing size.
    pub init_pos: u64,
    pub arcsize: u64,
    pub sector_size: u64,
    pub rec_sectors: u64,
}

/// Why an archive could not be scanned.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// Fewer than two RECOVERY blocks: there is no recovery info.
    Absent,
    /// The header names a version this build does not know. `aRecVersions` is
    /// a whitelist, so a newer writer is refused rather than guessed at.
    Version(String),
    /// The header did not parse.
    Malformed,
    /// The recovery info describes a region the file does not contain.
    Truncated,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Absent => write!(f, "recovery data absent or corrupt"),
            Error::Version(v) => {
                write!(f, "you need FreeArc {v} or above to process this recovery info")
            }
            Error::Malformed => write!(f, "the recovery info header did not parse"),
            Error::Truncated => write!(f, "the archive is shorter than its recovery info says"),
        }
    }
}

/// Parse the CRC block's header, returning it and the offset of the CRCs that
/// follow it.
pub fn read_control(body: &[u8], crcs_block_pos: u64) -> Result<(Control, usize), Error> {
    let mut s = crate::bytestream::InStream::new(body);
    let version = s.string().map_err(|_| Error::Malformed)?;
    // `version notElem aRecVersions` -- a whitelist, checked before anything
    // else is read, so meta-information from a format we do not know is never
    // interpreted as if it were ours.
    if version != "0.36" && version != "0.39" {
        return Err(Error::Version(version));
    }
    let arcsize = s.varint().map_err(|_| Error::Malformed)?;
    let offset = s.varint().map_err(|_| Error::Malformed)?;
    let n = s.count().map_err(|_| Error::Malformed)?;
    if n == 0 {
        return Err(Error::Malformed);
    }
    let sector_size = s.varint().map_err(|_| Error::Malformed)?;
    let rec_sectors = s.varint().map_err(|_| Error::Malformed)?;
    if sector_size == 0 {
        return Err(Error::Malformed);
    }
    // Only the first "compartment" is used; the format allows more and no
    // writer produces them.
    let init_pos = crcs_block_pos.checked_sub(offset).ok_or(Error::Malformed)?;
    Ok((Control { init_pos, arcsize, sector_size, rec_sectors }, s.pos()))
}

/// The result of scanning an archive against its recovery info.
pub struct Scan {
    pub control: Control,
    /// Indices of sectors whose CRC no longer matches.
    pub bad: Vec<u64>,
    /// The XOR sectors, after every archive sector has been XORed back in.
    ///
    /// For a recovery sector with exactly ONE damaged archive sector, this now
    /// holds `S_damaged ^ S_correct` — everything else cancels — so XORing it
    /// with the damaged sector produces the correct one. That identity is the
    /// entire recovery scheme.
    pub parity: Vec<u8>,
    /// The stored CRC of each archive sector, in order.
    ///
    /// Kept rather than re-read: the repair pass needs them again to check each
    /// repair, and a second parse could disagree with the first about where the
    /// header ends.
    pub stored_crcs: Vec<u32>,
}

/// `scanArchive` (`ArcRecover.hs:243`) — compare every sector against its
/// stored CRC.
///
/// `blocks` is the footer's block list and `data` the whole file.
pub fn scan(blocks: &[ArchiveBlock], data: &[u8]) -> Result<Scan, Error> {
    let rec: Vec<&ArchiveBlock> =
        blocks.iter().filter(|b| b.block_type == BlockType::Recovery).collect();
    // "The current version can only process a single pair of recovery blocks."
    if rec.len() < 2 {
        return Err(Error::Absent);
    }
    let (sectors_block, crcs_block) = (rec[0], rec[1]);

    let body = |b: &ArchiveBlock| -> Result<&[u8], Error> {
        let start = b.pos as usize;
        let end = start.checked_add(b.comp_size as usize).ok_or(Error::Truncated)?;
        data.get(start..end).ok_or(Error::Truncated)
    };
    let crcs_body = body(crcs_block)?;
    let (control, crcs_at) = read_control(crcs_body, crcs_block.pos)?;
    let mut parity = body(sectors_block)?.to_vec();

    let ss = control.sector_size as usize;
    let arc_sectors = control.arcsize.div_ceil(control.sector_size);
    let start = control.init_pos as usize;
    let end = start
        .checked_add(control.arcsize as usize)
        .ok_or(Error::Truncated)?;
    let protected = data.get(start..end).ok_or(Error::Truncated)?;

    let mut idx = match control.rec_sectors {
        0 => 0,
        n => (n - (arc_sectors % n)) % n,
    };
    let mut bad = Vec::new();
    let mut stored_crcs = Vec::with_capacity(arc_sectors as usize);
    let mut crcs = crate::bytestream::InStream::new(&crcs_body[crcs_at..]);
    for (n, chunk) in protected.chunks(ss).enumerate() {
        if control.rec_sectors > 0 {
            let base = idx as usize * ss;
            match parity.get_mut(base..base + chunk.len()) {
                Some(dst) => {
                    for (d, s) in dst.iter_mut().zip(chunk) {
                        *d ^= s;
                    }
                }
                // The XOR block is shorter than its geometry claims.
                None => return Err(Error::Truncated),
            }
            idx = (idx + 1) % control.rec_sectors;
        }
        let stored = crcs.crc().map_err(|_| Error::Malformed)?;
        stored_crcs.push(stored);
        if crc::calc(chunk) != stored {
            bad.push(n as u64);
        }
    }
    Ok(Scan { control, bad, parity, stored_crcs })
}

/// Which damaged sectors can be repaired, and which cannot.
///
/// A recovery sector holds the XOR of everything mapped to it, so it can supply
/// exactly ONE unknown. Two damaged sectors sharing a recovery sector are both
/// lost — `partition (null.tail)` over the groups.
pub fn partition_bad(bad: &[u64], rec_sectors: u64) -> (Vec<u64>, Vec<u64>) {
    // "If the RR contains no recovery sectors, then no archive sector can be
    // recovered with their help :D"
    if rec_sectors == 0 {
        return (Vec::new(), bad.to_vec());
    }
    let mut groups: std::collections::BTreeMap<u64, Vec<u64>> =
        std::collections::BTreeMap::new();
    for n in bad {
        groups.entry(n % rec_sectors).or_default().push(*n);
    }
    let mut recoverable = Vec::new();
    let mut lost = Vec::new();
    for (_, g) in groups {
        match g.len() {
            1 => recoverable.extend(g),
            _ => lost.extend(g),
        }
    }
    (recoverable, lost)
}

/// `runArchiveRecovery`'s copy loop — produce the repaired archive.
///
/// Returns the new file's bytes and the sectors that remain broken. A sector is
/// only accepted after its repaired contents match the stored CRC: the parity
/// itself may be damaged, and a "repair" that fails that check is undone rather
/// than written.
/// A second copy of the archive, for `--original`.
///
/// A sector the parity cannot reconstruct may still be readable from an intact
/// copy — the point of `--original` is that a damaged download can be repaired
/// from the source *without fetching the whole thing again*.
///
/// Hence a reader rather than a buffer. The copy is usually REMOTE, and the
/// reference never downloads it: `url_seek` only moves a cursor and each read
/// issues one ranged GET (`URL.cpp`, `CURLOPT_RANGE`). That is what makes
/// `-rr0.1%` — documented as "for recovery over the internet only" — worth
/// having: a handful of KB of recovery records plus a few ranged reads repairs
/// an archive that would otherwise have to be re-fetched in full.
pub trait Original {
    /// The copy's total size, or `None` if it cannot be determined.
    ///
    /// Checked against the archive being repaired before any sector is taken: a
    /// different size is a different build, whose sectors would not line up.
    fn size(&mut self) -> Option<u64>;

    /// Exactly `len` bytes at `offset`, or `None` if they cannot be read.
    ///
    /// `None` is not fatal — a sector that cannot be fetched simply stays in the
    /// unrecovered list, the same as one whose CRC does not match.
    fn read_at(&mut self, offset: u64, len: usize) -> Option<Vec<u8>>;
}

/// A copy already in memory: a local file, read once.
pub struct Bytes(pub Vec<u8>);

impl Original for Bytes {
    fn size(&mut self) -> Option<u64> {
        Some(self.0.len() as u64)
    }

    fn read_at(&mut self, offset: u64, len: usize) -> Option<Vec<u8>> {
        let at = usize::try_from(offset).ok()?;
        self.0.get(at..at.checked_add(len)?).map(<[u8]>::to_vec)
    }
}

pub fn recover(scan: &Scan, data: &[u8]) -> Result<(Vec<u8>, Vec<u64>), Error> {
    recover_with(scan, data, None)
}

/// As [`recover`], but also pulling unrecoverable sectors from a second copy.
///
/// The copy must be the SAME SIZE as the archive being repaired
/// (`ArcRecover.hs:405`); a different size means a different build of the
/// archive, whose sectors would not line up, and it is rejected rather than
/// used at an offset. Each sector taken from it is still CRC-checked, so a copy
/// that is the right size and the wrong contents changes nothing.
pub fn recover_with(
    scan: &Scan,
    data: &[u8],
    // `dyn Original + '_`, not a bare `dyn Original`: elision would tie the
    // trait object's lifetime to the `&mut`'s, and because `&mut T` is
    // invariant in `T` that makes the caller's Box borrowed for as long as it
    // exists — which its own destructor then contradicts.
    mut original: Option<&mut (dyn Original + '_)>,
) -> Result<(Vec<u8>, Vec<u64>), Error> {
    let c = &scan.control;
    let ss = c.sector_size as usize;
    let arc_sectors = c.arcsize.div_ceil(c.sector_size);
    let (recoverable, mut still_bad) = partition_bad(&scan.bad, c.rec_sectors);

    let start = c.init_pos as usize;
    let end = start.checked_add(c.arcsize as usize).ok_or(Error::Truncated)?;
    let protected = data.get(start..end).ok_or(Error::Truncated)?;

    // Everything before the protected region -- an SFX stub, if any -- is
    // copied through unchanged.
    let mut out = data.get(..start).ok_or(Error::Truncated)?.to_vec();

    let mut idx = match c.rec_sectors {
        0 => 0,
        n => (n - (arc_sectors % n)) % n,
    };

    for (n, chunk) in protected.chunks(ss).enumerate() {
        let mut sector = chunk.to_vec();
        let this = idx;
        if c.rec_sectors > 0 {
            idx = (idx + 1) % c.rec_sectors;
        }
        if recoverable.contains(&(n as u64)) {
            let base = this as usize * ss;
            match scan.parity.get(base..base + sector.len()) {
                Some(p) => {
                    for (d, s) in sector.iter_mut().zip(p) {
                        *d ^= s;
                    }
                }
                None => return Err(Error::Truncated),
            }
            // "If the CRC still does not match after that (which is possible
            // when the reference sector itself is in error), then restore the
            // sector's original contents."
            match scan.stored_crcs.get(n) {
                Some(want) if crc::calc(&sector) == *want => {}
                _ => {
                    sector.copy_from_slice(chunk);
                    still_bad.push(n as u64);
                }
            }
        }
        // `--original`: a sector still broken after the parity pass may be
        // readable from an intact copy. Tried for EVERY sector in `errors`,
        // which by now includes both the ones the parity could not address and
        // the ones whose repair failed its own CRC.
        //
        // The sector is taken only if it passes the stored CRC, so a copy that
        // is the right size and the wrong contents cannot make things worse.
        match original {
            Some(ref mut o) if still_bad.contains(&(n as u64)) => {
                // `fileSeek original (arcPos n); fileReadBuf original temp
                // bytes` (ArcRecover.hs:413) -- one ranged read per damaged
                // sector, so a remote copy costs only the sectors actually
                // needed.
                let at = c.init_pos + (n * ss) as u64;
                match o.read_at(at, sector.len()) {
                    Some(fresh) => match scan.stored_crcs.get(n) {
                        Some(want) if crc::calc(&fresh) == *want => {
                            sector.copy_from_slice(&fresh);
                            still_bad.retain(|x| *x != n as u64);
                        }
                        _ => {}
                    },
                    None => {}
                }
            }
            _ => {}
        }
        out.extend_from_slice(&sector);
    }

    // "Copy the recovery blocks (or rather, the entire remainder of the old
    // archive file after the protected data)."
    out.extend_from_slice(data.get(end..).ok_or(Error::Truncated)?);
    still_bad.sort_unstable();
    Ok((out, still_bad))
}

/// A `RECOVERY_BLOCK` record for the footer's block list.
pub fn block(pos: u64, body: &[u8]) -> ArchiveBlock {
    ArchiveBlock {
        block_type: BlockType::Recovery,
        compressor: crate::writer::no_compression(),
        pos,
        orig_size: body.len() as u64,
        comp_size: body.len() as u64,
        crc: crc::calc(body),
        files: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lb_is_floor_log2_with_zero_and_one_at_zero() {
        assert_eq!(lb(0), 0);
        assert_eq!(lb(1), 0);
        assert_eq!(lb(2), 1);
        assert_eq!(lb(3), 1);
        assert_eq!(lb(4), 2);
        assert_eq!(lb(1023), 9);
        assert_eq!(lb(1024), 10);
    }

    /// The documented relation: "4% -> 512, 2% -> 1024, 1% -> 2048".
    #[test]
    fn the_sector_size_follows_the_percentage() {
        let arcsize = 10_000_000;
        let big = u64::MAX;
        assert_eq!(geometry("4%", arcsize, big).expect("4%").sector_size, 512);
        assert_eq!(geometry("2%", arcsize, big).expect("2%").sector_size, 1024);
        assert_eq!(geometry("1%", arcsize, big).expect("1%").sector_size, 2048);
    }

    /// `-rr0%` is the CRC-only mode: 4 KB sectors, no XOR sectors, and so the
    /// other version number.
    #[test]
    fn zero_percent_stores_crcs_only() {
        let g = geometry("0%", 1_000_000, u64::MAX).expect("0%");
        assert_eq!(g.sector_size, 4096);
        assert_eq!(g.rec_sectors, 0);
        assert_eq!(g.arc_sectors, 1_000_000u64.div_ceil(4096));
        assert_eq!(g.crcs_size, g.arc_sectors * 4);
        assert_eq!(version(g.rec_sectors), "0.39");
        assert_eq!(version(1), "0.36");
    }

    /// The two explicit forms: `amount;sector` and `sectors*sector`.
    #[test]
    fn the_explicit_forms_pin_what_they_name() {
        let g = geometry("4%;1024", 1_000_000, u64::MAX).expect("amount;sector");
        assert_eq!(g.sector_size, 1024);

        let g = geometry("8*4096", 1_000_000, u64::MAX).expect("sectors*sector");
        assert_eq!(g.sector_size, 4096);
        // rec_size is 8*4096; the CRCs come out of that budget.
        let crcs0 = 1_000_000u64.div_ceil(4096) * 4;
        assert_eq!(g.rec_sectors, (8 * 4096u64 - crcs0).div_ceil(4096));

        // "0*4096" is the -rr0.1% spelling: no XOR sectors, 4 KB sectors.
        let g = geometry("0*4096", 1_000_000, u64::MAX).expect("0*4096");
        assert_eq!(g.rec_sectors, 0);
        assert_eq!(g.sector_size, 4096);
    }

    /// A request smaller than the CRCs is raised to hold them, rather than
    /// producing a block that cannot describe the archive.
    #[test]
    fn the_block_always_holds_at_least_the_crcs() {
        let g = geometry("1b", 10_000_000, u64::MAX).expect("tiny");
        assert!(g.crcs_size >= g.arc_sectors * 4);
        assert_eq!(g.rec_sectors, 0, "nothing is left over for XOR sectors");
    }

    /// `-rr+` is documented in ArcRecover and now also accepted by Cmdline,
    /// which runs first — previously it rejected the value and made
    /// `ArcRecover.hs:93` unreachable.
    #[test]
    fn the_option_validator_accepts_plus_and_every_size_suffix() {
        assert!(option_is_valid(""));
        assert!(option_is_valid("-"));
        assert!(option_is_valid("--"));
        assert!(option_is_valid("+"), "rr_ok lets + through to ArcRecover");
        for s in ["4", "100b", "1k", "2m", "1g", "1t", "10^", "1%", "5p"] {
            assert!(option_is_valid(s), "{s} should be a valid -rr value");
        }
        assert!(option_is_valid("2%;1024"));
        assert!(option_is_valid("0*4096"));
        assert!(!option_is_valid("x"));
        assert!(!option_is_valid("1z"));
    }

    #[test]
    fn resolve_follows_the_option_table() {
        assert_eq!(resolve("-", "2%", 1000), "");
        assert_eq!(resolve("--", "2%", 1000), "2%");
        assert_eq!(resolve("--", "", 1000), "");
        assert_eq!(resolve("", "2%", 1000), "2%");
        assert_eq!(resolve("", "", 100), "4%", "small archives get 4%");
        assert_eq!(resolve("+", "", 1_000_000), "2%");
        assert_eq!(resolve("", "", 10_000_000), "1%");
        assert_eq!(resolve("5%", "2%", 1000), "5%");
    }

    /// The counter does not start at zero, and the offset is what makes the
    /// LAST archive sector land on the last recovery sector.
    #[test]
    fn the_last_archive_sector_lands_on_the_last_recovery_sector() {
        // 10 archive sectors over 4 recovery sectors: the last must be index 3.
        let g = Geometry { sector_size: 4, arc_sectors: 10, rec_sectors: 4, crcs_size: 56 };
        let protected = vec![0u8; 40];
        // Re-derive the walk the builder performs.
        let mut idx = (g.rec_sectors - (g.arc_sectors % g.rec_sectors)) % g.rec_sectors;
        let mut last = 0;
        for _ in 0..g.arc_sectors {
            last = idx;
            idx = (idx + 1) % g.rec_sectors;
        }
        assert_eq!(last, g.rec_sectors - 1);
        // And the builder itself runs without going out of bounds.
        let b = build(&g, &protected, 0);
        assert_eq!(b.sectors.len(), 16);
    }

    /// XORing every sector that shares a recovery sector reproduces it, which
    /// is the property the whole scheme rests on.
    #[test]
    fn a_recovery_sector_is_the_xor_of_its_archive_sectors() {
        let g = Geometry { sector_size: 8, arc_sectors: 5, rec_sectors: 2, crcs_size: 28 };
        let protected: Vec<u8> = (0..40u8).collect();
        let b = build(&g, &protected, 0);

        let mut expect = vec![0u8; 16];
        let mut idx = (g.rec_sectors - (g.arc_sectors % g.rec_sectors)) % g.rec_sectors;
        for chunk in protected.chunks(8) {
            let base = idx as usize * 8;
            for (i, x) in chunk.iter().enumerate() {
                expect[base + i] ^= x;
            }
            idx = (idx + 1) % g.rec_sectors;
        }
        assert_eq!(b.sectors, expect);
        assert_ne!(b.sectors, vec![0u8; 16], "the sectors were never written");
    }
}
