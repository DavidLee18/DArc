//! Option defaults, derived exactly as `srep.cpp`'s `main()` derives them.
//!
//! These are not cosmetic. `L`, `min_match`, `BASE_LEN` and `ACCELERATOR` all
//! feed the match finder and the record encoding, so a wrong default produces a
//! valid-looking stream that no other build reproduces. They are computed here
//! rather than hard-coded so the `-l`/`-c`/`-a` options land in one place.
//!
//! # The three "base length" values are different numbers
//!
//! This is the trap in this file, and it is worth stating before the code:
//!
//! | | value for `-m3f` | used by |
//! |---|---|---|
//! | `L` (chunk size) | 512 | the hash table; `record_match` rounds lengths by it |
//! | `BASE_LEN` | 512 | `ENCODE_LZ_MATCH` inside `compress()` |
//! | `FUTURELZ_BASE_LEN` | **0** | `ENCODE_LZ_MATCH` when *writing* Future-LZ records |
//!
//! `FUTURELZ_BASE_LEN` is `IO_LZ ? BASE_LEN : 0` (`srep.cpp:386`). Under
//! Future-LZ and Index-LZ a match's source may be split across two blocks,
//! producing fragments shorter than `BASE_LEN` — down to length 1 — so the
//! written records cannot carry a nonzero base. Using `BASE_LEN` there would
//! reject exactly the fragments the format exists to express.

/// `DEFAULT_MIN_MATCH` (`srep.cpp:38`).
pub const DEFAULT_MIN_MATCH: u32 = 32;
/// `DEFAULT_ACCEL` (`:225`).
pub const DEFAULT_ACCEL: u32 = 4;
/// The default `-b` block size and `-mem` budget: 8 MB (`:230`).
pub const DEFAULT_BUFSIZE: usize = 8 << 20;

/// Which SREP method was selected.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Method {
    /// `-m0` — in-memory (REP).
    InMemory,
    /// `-m1` — content-defined chunking.
    Cdc,
    /// `-m2` — ZPAQ content-defined chunking.
    ZpaqCdc,
    /// `-m3` — precomputed digests, rounded matches.
    Digests,
    /// `-m4` — compare by re-reading old data.
    Reread,
    /// `-m5` — exhaustive search.
    Exhaustive,
}

impl Method {
    /// `CONTENT_DEFINED_CHUNKING` (`:367`).
    pub fn cdc(self) -> bool {
        matches!(self, Method::Cdc | Method::ZpaqCdc)
    }
    /// `COMPARE_DIGESTS` (`:369`) — true for `-m0..-m3`.
    pub fn compare_digests(self) -> bool {
        !matches!(self, Method::Reread | Method::Exhaustive)
    }
    /// `PRECOMPUTE_DIGESTS` (`:370`) — `-m3` only.
    pub fn precompute_digests(self) -> bool {
        self == Method::Digests
    }
    /// `EXHAUSTIVE_SEARCH` (`:372`).
    pub fn exhaustive(self) -> bool {
        self == Method::Exhaustive
    }
}

/// Which record layout the file uses — the `o`/`f`/none suffix on `-mN`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Layout {
    /// `-mN` — the default. Match list in a footer.
    IndexLz,
    /// `-mNf` — matches stored with their source block.
    FutureLz,
    /// `-mNo` — matches point backwards.
    IoLz,
}

/// What the user asked for on the command line, before defaults are applied.
#[derive(Clone, Copy, Debug, Default)]
pub struct Options {
    /// `-c` chunk size. 0 means "not given".
    pub l: u32,
    /// `-l` minimum match. 0 means "not given".
    pub min_match: u32,
    /// `-a{accel}` — `None` for the C's 9000 sentinel.
    pub accel: Option<u32>,
    /// `-a.../{ACCELERATOR}`.
    pub accelerator: Option<u32>,
    /// `-d` dictionary size.
    pub dictsize: u64,
}

/// The derived, ready-to-use parameter set.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Derived {
    pub l: u32,
    pub min_match: u32,
    pub dict_min_match: u32,
    pub dict_chunk: u32,
    /// `BASE_LEN` — used by `compress()`'s `ENCODE_LZ_MATCH`.
    pub base_len: u32,
    /// `FUTURELZ_BASE_LEN` — used when *writing* Future-LZ / Index-LZ records.
    pub futurelz_base_len: u32,
    pub accel: u32,
    pub bitarr_accelerator: u32,
    pub accelerator: u32,
    /// `ROUND_MATCHES` (`:371`).
    pub round_matches: bool,
    /// The format version written to the header (`:574`).
    pub format_version: u32,
}

/// `rounddown_to_power_of(n, 2)` — only reached on the `-m5` path.
fn rounddown_to_power_of_2(n: u32) -> u32 {
    match n {
        0 => 0,
        _ => 1u32 << (31 - n.leading_zeros()),
    }
}

/// Apply `srep.cpp:374-386` and `:505-507`.
pub fn derive(method: Method, layout: Layout, opt: Options) -> Derived {
    let cdc = method.cdc();
    let mut l = opt.l;
    let mut min_match = opt.min_match;

    // ":376" -- the default -l value, chosen before L is defaulted.
    if l == 0 && min_match == 0 {
        min_match = match cdc {
            true => 4096,
            false => 512,
        };
    }
    if l == 0 {
        match cdc {
            // "-lX === -l0 -cX" for -m1/-m2.
            true => {
                l = min_match;
                min_match = 0;
            }
            false => {
                l = match method.exhaustive() {
                    // -m5 searches for ALL matches of min_match or longer, so
                    // its chunks are half the largest power of two that fits.
                    true => rounddown_to_power_of_2(min_match + 1) / 2,
                    false => min_match,
                };
            }
        }
    }
    if min_match == 0 {
        min_match = match cdc {
            true => DEFAULT_MIN_MATCH,
            false => l,
        };
    }
    let dict_min_match = 512u32;
    let dict_chunk = dict_min_match / 8;

    let base_len = min_match.min(dict_min_match);
    // ":386" -- 0 unless I/O-LZ. See the module docs.
    let futurelz_base_len = match layout {
        Layout::IoLz => base_len,
        Layout::FutureLz | Layout::IndexLz => 0,
    };

    // ":505-507" -- accel is reduced for small L; the main loop caps at 16,
    // while larger -a values still enlarge bitarr[].
    let accel = opt
        .accel
        // `mymin(mymax(L/32,1), DEFAULT_ACCEL)` -- clamp is the same thing, and
        // its bounds are constants here so it cannot panic on an inverted range.
        .unwrap_or_else(|| (l / 32).clamp(1, DEFAULT_ACCEL));
    let bitarr_accelerator = accel * 8;
    let accelerator = opt.accelerator.unwrap_or_else(|| accel.min(16));

    // ":371" -- rounding is a property of the METHOD, not of the layout.
    let round_matches = method == Method::Digests && opt.dictsize == 0;

    // ":574" -- and note round_matches can be true while the version is 3 or 4.
    let format_version = match layout {
        Layout::IndexLz => 4,
        Layout::FutureLz => 3,
        Layout::IoLz => match round_matches {
            true => 1,
            false => 2,
        },
    };

    Derived {
        l,
        min_match,
        dict_min_match,
        dict_chunk,
        base_len,
        futurelz_base_len,
        accel,
        bitarr_accelerator,
        accelerator,
        round_matches,
        format_version,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn m3f_defaults_are_what_arc_ini_actually_runs() {
        // `default = m3f` in Installer/bin/arc.ini:323, no other options.
        let d = derive(Method::Digests, Layout::FutureLz, Options::default());
        assert_eq!(d.l, 512, "chunk size");
        assert_eq!(d.min_match, 512);
        assert_eq!(d.base_len, 512);
        assert_eq!(d.accel, 4, "min(max(512/32,1), DEFAULT_ACCEL)");
        assert_eq!(d.bitarr_accelerator, 32);
        assert_eq!(d.accelerator, 4, "min(accel,16)");
        assert_eq!(d.format_version, 3);
        // The two that are easy to get wrong:
        assert!(d.round_matches, "-m3 with no dict rounds, even at version 3");
        assert_eq!(d.futurelz_base_len, 0, "Future-LZ writes with base 0");
    }

    #[test]
    fn round_matches_is_true_while_the_version_is_not_1() {
        // The interaction that had to be settled: ROUND_MATCHES is a property of
        // the METHOD (:371) and the version of the LAYOUT (:574), so -m3f and
        // -m3 both round while writing v3/v4.
        for (layout, want_version) in [
            (Layout::FutureLz, 3u32),
            (Layout::IndexLz, 4),
            (Layout::IoLz, 1),
        ] {
            let d = derive(Method::Digests, layout, Options::default());
            assert!(d.round_matches, "{layout:?}");
            assert_eq!(d.format_version, want_version, "{layout:?}");
        }
        // -m4 does not round, so its I/O-LZ form is version 2 rather than 1.
        let d = derive(Method::Reread, Layout::IoLz, Options::default());
        assert!(!d.round_matches);
        assert_eq!(d.format_version, 2);
    }

    #[test]
    fn a_dictionary_disables_rounding() {
        // ":371" -- `(method == SREP_METHOD3) && (dictsize == 0)`.
        let d = derive(
            Method::Digests,
            Layout::IoLz,
            Options { dictsize: 1 << 20, ..Options::default() },
        );
        assert!(!d.round_matches);
        assert_eq!(d.format_version, 2, "not 1, because rounding is off");
    }

    #[test]
    fn io_lz_is_the_only_layout_with_a_nonzero_futurelz_base() {
        // Under Future-LZ/Index-LZ a match source can straddle two blocks and
        // produce fragments as short as 1 byte; a nonzero base would reject
        // exactly those.
        assert_eq!(
            derive(Method::Digests, Layout::IoLz, Options::default()).futurelz_base_len,
            512
        );
        for layout in [Layout::FutureLz, Layout::IndexLz] {
            assert_eq!(
                derive(Method::Digests, layout, Options::default()).futurelz_base_len,
                0,
                "{layout:?}"
            );
        }
    }

    #[test]
    fn explicit_options_override_every_default() {
        let d = derive(
            Method::Digests,
            Layout::FutureLz,
            Options {
                l: 4096,
                min_match: 64,
                accel: Some(9),
                accelerator: Some(32),
                dictsize: 0,
            },
        );
        assert_eq!(d.l, 4096);
        assert_eq!(d.min_match, 64);
        assert_eq!(d.base_len, 64, "min(min_match, dict_min_match)");
        assert_eq!(d.accel, 9);
        assert_eq!(d.bitarr_accelerator, 72);
        assert_eq!(d.accelerator, 32, "explicit, so NOT capped at 16");
    }

    #[test]
    fn accel_is_capped_at_16_only_when_defaulted() {
        // ":507" -- "larger -a values only increase bitarr[] size", so the cap
        // applies to the derived value and not to an explicit one.
        let big = derive(
            Method::Digests,
            Layout::FutureLz,
            Options { l: 1 << 20, ..Options::default() },
        );
        assert_eq!(big.accel, DEFAULT_ACCEL, "capped by DEFAULT_ACCEL, not by L");
        assert_eq!(big.accelerator, 4);
    }

    #[test]
    fn cdc_swaps_the_meaning_of_l_and_min_match() {
        // ":379" -- for -m1/-m2, `-lX === -l0 -cX`.
        let d = derive(Method::Cdc, Layout::IndexLz, Options::default());
        assert_eq!(d.l, 4096, "the -l default became the chunk size");
        assert_eq!(d.min_match, DEFAULT_MIN_MATCH);
    }

    #[test]
    fn exhaustive_search_halves_the_chunk_size() {
        // ":380" -- rounddown_to_power_of(min_match+1,2)/2.
        let d = derive(Method::Exhaustive, Layout::IoLz, Options::default());
        assert_eq!(d.min_match, 512);
        assert_eq!(d.l, 256, "rounddown_to_power_of(513,2)/2");
        assert!(!d.round_matches, "-m5 is not -m3");
    }
}
