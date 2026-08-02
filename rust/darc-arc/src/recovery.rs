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
/// This rejects `"+"`, even though `writeRecoveryBlocks` has a case for it
/// (`ArcRecover.hs:93`). The validation runs first, so that case is unreachable
/// and `arc a -rr+` is `INVALID_OPTION_VALUE`. Reproducing the reachable half
/// only would make `-rr+` write an archive the reference refuses to.
///
/// The size suffixes all pass because `parseNumber` reports `'b'` for every one
/// of `b k m g t ^` — only a character it does not know comes back as itself.
pub fn option_is_valid(s: &str) -> bool {
    if matches!(s, "" | "-" | "--") || s.contains(';') || s.contains('*') {
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

    /// `-rr+` is documented in ArcRecover and rejected by Cmdline, which runs
    /// first. Accepting it would write archives the reference refuses to.
    #[test]
    fn the_option_validator_rejects_plus_and_accepts_every_size_suffix() {
        assert!(option_is_valid(""));
        assert!(option_is_valid("-"));
        assert!(option_is_valid("--"));
        assert!(!option_is_valid("+"), "rr_ok rejects + before ArcRecover sees it");
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
