//! How files are grouped into solid blocks — `-s` (`parseSolidOption`,
//! `Cmdline.hs:757`) and the split it drives (`ArhiveFileList.hs:312-380`).
//!
//! A *solid block* is a run of files compressed as one stream. Bigger blocks
//! compress better and cost more to extract from, because getting at any file
//! means decompressing everything before it. `-s` is where that trade is made.
//!
//! # The criteria are a LIST, and the shortest wins
//!
//! `-se100f10m` is three criteria at once: by extension, by 100 files, and by
//! 10 MB. A block ends as soon as *any* of them says so — `newLen = minimum $
//! map (`splitLen` files) crits`. So they are limits, not alternatives, and
//! adding one can only make blocks smaller.

/// `Grouping` (`Options.hs:350`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Grouping {
    /// Each file in its own block.
    None,
    /// A new block whenever the extension changes.
    ByExt,
    /// A minimum data size per block.
    BySize(u64),
    /// A MAXIMUM size, for block algorithms (BWT, ST) whose window is fixed.
    /// Not the same rule as `BySize`, despite the similar name — see
    /// [`split_len`].
    ByBlockSize(u64),
    ByNumber(u64),
    /// Everything in one block. The default.
    All,
}

/// What one file contributes to the split.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub size: u64,
    /// The lower-cased extension, as `fpLCExtension` produces it.
    pub ext: String,
}

/// `parseNumber s 'f'` — the grouping-specific number parser, where a bare
/// number counts FILES rather than bytes.
fn parse_group_number(s: &str) -> Option<Grouping> {
    let lowered = s.to_ascii_lowercase();
    let digits: String = lowered.chars().take_while(char::is_ascii_digit).collect();
    if digits.is_empty() {
        return None;
    }
    let n: u64 = digits.parse().ok()?;
    match lowered[digits.len()..].chars().next().unwrap_or('f') {
        'b' => Some(Grouping::BySize(n)),
        'k' => Some(Grouping::BySize(n * 1024)),
        'm' => Some(Grouping::BySize(n * 1024 * 1024)),
        'g' => Some(Grouping::BySize(n * 1024 * 1024 * 1024)),
        't' => Some(Grouping::BySize(n * 1024u64.pow(4))),
        '^' if n < 64 => Some(Grouping::BySize(1u64 << n)),
        // `(1,'f') -> GroupNone`: one file per block is spelled as a count, and
        // it is NOT ByNumber(1) -- the two behave the same here, but the
        // Haskell distinguishes them and so does this.
        'f' => match n {
            1 => Some(Grouping::None),
            _ => Some(Grouping::ByNumber(n)),
        },
        _ => None,
    }
}

/// Split a grouping description into its pieces.
///
/// `spanBreak` takes a run of digits and then a run of non-digits, so
/// `"100f10m"` is `["100f", "10m"]`; a leading `'e'` is peeled off on its own,
/// so `"e100f"` is `["e", "100f"]`.
fn pieces(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == 'e' {
            out.push("e".to_string());
            i += 1;
            continue;
        }
        let start = i;
        while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == 'e') {
            i += 1;
        }
        while i < chars.len() && !(chars[i].is_ascii_digit() || chars[i] == 'e') {
            i += 1;
        }
        // A piece that consumed nothing would loop for ever.
        if i == start {
            i += 1;
        }
        out.push(chars[start..i].iter().collect());
    }
    out
}

fn parse_groupings(s: &str) -> Option<Vec<Grouping>> {
    match s.is_empty() {
        true => Some(vec![Grouping::All]),
        false => pieces(s)
            .into_iter()
            .map(|p| match p.as_str() {
                "" => Some(Grouping::All),
                "e" => Some(Grouping::ByExt),
                other => parse_group_number(other),
            })
            .collect(),
    }
}

/// What `-s` resolved to: the grouping for the directory, for the data, and an
/// override for `-dm` that three of the presets carry.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Solid {
    pub dirs: Vec<Grouping>,
    pub data: Vec<Grouping>,
    /// `"0"` for the presets that also force an uncompressed directory.
    pub dir_method: String,
}

/// `aDEFAULT_DIR_GROUPING` — one directory block per archive.
const DEFAULT_DIR: Grouping = Grouping::All;

/// `parseSolidOption` (`Cmdline.hs:757`).
///
/// The four named presets are shorthands for other archivers' layouts: `7z` is
/// one solid block and one compressed directory, `cab` the same with the
/// directory stored, `zip` a block per file, `arj` a block AND a directory per
/// file.
pub fn parse_solid(opt: &str) -> Option<Solid> {
    let parts: Vec<&str> = opt.split(';').collect();
    let one = |dirs, data, dm: &str| {
        Some(Solid { dirs, data, dir_method: dm.to_string() })
    };
    match parts.as_slice() {
        // `split ';' ""` is [""] in the Haskell, and the [] case is unreachable
        // from a command line; both mean "-s" with no value.
        [""] => one(vec![DEFAULT_DIR], vec![Grouping::All], ""),
        ["-"] => one(vec![DEFAULT_DIR], vec![Grouping::None], ""),
        ["7z"] => one(vec![Grouping::All], vec![Grouping::All], ""),
        ["cab"] => one(vec![Grouping::All], vec![Grouping::All], "0"),
        ["zip"] => one(vec![Grouping::All], vec![Grouping::None], "0"),
        ["arj"] => one(vec![Grouping::None], vec![Grouping::None], "0"),
        [dat] => one(vec![DEFAULT_DIR], parse_groupings(dat)?, ""),
        [dir, dat] => one(parse_groupings(dir)?, parse_groupings(dat)?, ""),
        _ => None,
    }
}

/// `special` (`ArhiveFileList.hs:379`) — "a temporary transformation to raise
/// the -m2t compression speed on multi-core machines". A block-size criterion
/// under 8 MB is quadrupled.
fn special(size: u64) -> u64 {
    match size > 8 * 1024 * 1024 {
        true => size,
        false => 4 * size,
    }
}

/// `splitLen` — how many files from the head of the list this criterion allows
/// in one block.
///
/// `BySize` and `ByBlockSize` are NOT the same rule. `BySize` is a MINIMUM: it
/// counts files while the running total is still below the limit and then adds
/// one more (`(1+) . groupLen … (< size)`), so a block reaches at least the
/// requested size. `ByBlockSize` is a MAXIMUM with no `+1`, because the
/// algorithm's window cannot be exceeded — and it runs its limit through
/// [`special`] first.
pub fn split_len(g: Grouping, files: &[Item]) -> u64 {
    match g {
        Grouping::None => 1,
        Grouping::ByExt => match files.first() {
            None => 0,
            Some(first) => files.iter().take_while(|f| f.ext == first.ext).count() as u64,
        },
        Grouping::BySize(size) => 1 + running_below(files, size),
        Grouping::ByBlockSize(size) => running_below(files, special(size)).max(1),
        Grouping::ByNumber(n) => n.max(1),
        // `const maxBound`: never the minimum, so it never ends a block.
        Grouping::All => u64::MAX,
    }
}

/// `groupLen (fiSize) (+) (< limit)` — how many running totals stay below the
/// limit.
fn running_below(files: &[Item], limit: u64) -> u64 {
    let mut total: u64 = 0;
    let mut n = 0;
    for f in files {
        total = total.saturating_add(f.size);
        if total >= limit {
            break;
        }
        n += 1;
    }
    n
}

/// `splitLenMin` — the smallest block this criterion tolerates: half the
/// nominal size or count, and a THIRD for block algorithms.
fn split_len_min(g: Grouping, files: &[Item]) -> u64 {
    match g {
        Grouping::BySize(size) => split_len(Grouping::BySize(size / 2), files),
        Grouping::ByBlockSize(size) => split_len(Grouping::ByBlockSize(size / 3), files),
        Grouping::ByNumber(n) => split_len(Grouping::ByNumber(n / 2), files),
        // `splitLenMin x = splitLen x` for the rest: None, ByExt and All have
        // no size to halve, so their minimum IS their nominal length.
        other @ (Grouping::None | Grouping::ByExt | Grouping::All) => split_len(other, files),
    }
}

/// Split `files` into solid blocks.
///
/// `splitBy [] _ files = [files]` — with no criteria there is nothing to split,
/// which is what makes the default `-s` (GroupAll) produce one block.
///
/// The old-solid-block fitting in `splitBy` (`ArhiveFileList.hs:344`) is NOT
/// here. It only does anything when the list contains files that came from an
/// archive, and by the time this is called those have either been pulled out
/// into their original blocks (under `keep_original`) or are being recompressed
/// anyway. Adding it would need `cfArcBlock` to survive the merge; until then a
/// caller with archive-origin files and a real grouping criterion would get
/// blocks that are correct but not the reference's, so `arc a` is what this is
/// gated on.
pub fn split_blocks(crits: &[Grouping], files: &[Item]) -> Vec<usize> {
    if crits.is_empty() || files.is_empty() {
        return match files.is_empty() {
            true => Vec::new(),
            false => vec![files.len()],
        };
    }
    let mut out = Vec::new();
    let mut rest = files;
    while !rest.is_empty() {
        let n = crits
            .iter()
            .map(|c| split_len(*c, rest))
            .min()
            .unwrap_or(u64::MAX)
            .max(1)
            .min(rest.len() as u64) as usize;
        out.push(n);
        rest = &rest[n..];
    }
    out
}

/// `addBlockSizeCrit` (`ArhiveFileList.hs:322`) — criteria the COMPRESSOR
/// imposes on top of the user's.
///
/// Multimedia chains are forced non-solid; a `dict` chain is capped at its
/// dictionary block size; and a lone block algorithm is capped at its own block
/// size. `block_size` is the codec's, 0 when it has none.
pub fn add_block_size_crit(
    chain: &[String],
    non_solid: bool,
    dict_block: Option<u64>,
    lone_block: Option<u64>,
    crits: &[Grouping],
) -> Vec<Grouping> {
    if chain.is_empty() {
        return crits.to_vec();
    }
    if non_solid {
        return vec![Grouping::None];
    }
    let mut out = Vec::new();
    match (dict_block, lone_block) {
        (Some(b), _) => out.push(Grouping::ByBlockSize(b)),
        (None, Some(b)) if chain.len() == 1 => out.push(Grouping::ByBlockSize(b)),
        _ => {}
    }
    out.extend_from_slice(crits);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn items(sizes: &[u64]) -> Vec<Item> {
        sizes.iter().map(|s| Item { size: *s, ext: "txt".to_string() }).collect()
    }

    #[test]
    fn the_presets_are_the_layouts_they_name() {
        assert_eq!(
            parse_solid("7z"),
            Some(Solid { dirs: vec![Grouping::All], data: vec![Grouping::All], dir_method: String::new() })
        );
        assert_eq!(parse_solid("cab").expect("cab").dir_method, "0");
        assert_eq!(parse_solid("zip").expect("zip").data, vec![Grouping::None]);
        assert_eq!(parse_solid("arj").expect("arj").dirs, vec![Grouping::None]);
        assert_eq!(parse_solid("-").expect("-").data, vec![Grouping::None]);
        assert_eq!(parse_solid("").expect("bare -s").data, vec![Grouping::All]);
    }

    /// `spanBreak` groups digits with the suffix that follows them, and a
    /// leading 'e' stands alone.
    #[test]
    fn a_description_splits_into_number_plus_suffix_runs() {
        assert_eq!(pieces("100f10m"), vec!["100f", "10m"]);
        assert_eq!(pieces("e100f"), vec!["e", "100f"]);
        assert_eq!(pieces("e"), vec!["e"]);
        assert_eq!(pieces("10m"), vec!["10m"]);
    }

    /// A bare number is FILES here, not bytes -- the default specifier is 'f'.
    /// And "1f" is GroupNone rather than ByNumber(1).
    #[test]
    fn a_bare_number_counts_files() {
        assert_eq!(parse_groupings("100"), Some(vec![Grouping::ByNumber(100)]));
        assert_eq!(parse_groupings("100f"), Some(vec![Grouping::ByNumber(100)]));
        assert_eq!(parse_groupings("1f"), Some(vec![Grouping::None]));
        assert_eq!(parse_groupings("10m"), Some(vec![Grouping::BySize(10 * 1024 * 1024)]));
        assert_eq!(parse_groupings("512b"), Some(vec![Grouping::BySize(512)]));
        assert_eq!(
            parse_groupings("e100f10m"),
            Some(vec![Grouping::ByExt, Grouping::ByNumber(100), Grouping::BySize(10 * 1024 * 1024)])
        );
    }

    /// BySize is a MINIMUM -- it overshoots by one file -- and ByBlockSize is a
    /// maximum. Reading the two as the same rule is the easy mistake.
    #[test]
    fn by_size_overshoots_and_by_block_size_does_not() {
        let f = items(&[100, 100, 100, 100]);
        // Running totals 100,200,300,400; two are below 250, so 2+1 = 3.
        assert_eq!(split_len(Grouping::BySize(250), &f), 3);
        // ByBlockSize(250) is quadrupled by `special` to 1000, so every total
        // below 1000 counts: 100..400 are all below, giving 4.
        assert_eq!(split_len(Grouping::ByBlockSize(250), &f), 4);
        // Above 8 MB `special` leaves it alone.
        let big = items(&[9 * 1024 * 1024, 9 * 1024 * 1024]);
        assert_eq!(split_len(Grouping::ByBlockSize(9 * 1024 * 1024), &big), 1);
    }

    #[test]
    fn the_shortest_criterion_wins() {
        let f = items(&[10; 10]);
        // ByNumber(3) is shorter than BySize(1000), so blocks of 3.
        assert_eq!(
            split_blocks(&[Grouping::ByNumber(3), Grouping::BySize(1000)], &f),
            vec![3, 3, 3, 1]
        );
        // GroupAll alone never splits.
        assert_eq!(split_blocks(&[Grouping::All], &f), vec![10]);
        // GroupNone always splits.
        assert_eq!(split_blocks(&[Grouping::None], &f), vec![1; 10]);
    }

    #[test]
    fn by_extension_breaks_where_the_extension_changes() {
        let f = vec![
            Item { size: 1, ext: "txt".into() },
            Item { size: 1, ext: "txt".into() },
            Item { size: 1, ext: "dat".into() },
            Item { size: 1, ext: "txt".into() },
        ];
        assert_eq!(split_blocks(&[Grouping::ByExt], &f), vec![2, 1, 1]);
    }

    /// A criterion that returns 0 would loop for ever; every path is clamped to
    /// at least one file.
    #[test]
    fn a_block_always_takes_at_least_one_file() {
        let f = items(&[1_000_000, 1_000_000]);
        assert_eq!(split_blocks(&[Grouping::BySize(1)], &f), vec![1, 1]);
        assert_eq!(split_blocks(&[Grouping::ByNumber(0)], &f), vec![1, 1]);
        assert_eq!(split_len_min(Grouping::ByNumber(1), &f), 1);
    }

    #[test]
    fn a_multimedia_chain_is_forced_non_solid() {
        let chain = vec!["tta".to_string()];
        assert_eq!(
            add_block_size_crit(&chain, true, None, None, &[Grouping::All]),
            vec![Grouping::None]
        );
        // A dict chain gains a cap ahead of the user's criteria.
        assert_eq!(
            add_block_size_crit(&chain, false, Some(64), None, &[Grouping::All]),
            vec![Grouping::ByBlockSize(64), Grouping::All]
        );
        // A lone block algorithm likewise, but only when it IS alone.
        let two = vec!["a".to_string(), "b".to_string()];
        assert_eq!(
            add_block_size_crit(&two, false, None, Some(64), &[Grouping::All]),
            vec![Grouping::All]
        );
    }
}
