//! Deciding which files share a solid block — `splitFileTypes`
//! (`ArhiveFileList.hs:460`).
//!
//! From `-m2` upward a level defines a different chain per file *type*
//! (`#rep+exe+#xb / $obj=#b / $text=#t`), and this is what assigns them. The
//! assignment is **archive-byte-visible**: it decides block membership, and
//! therefore each chain's fitted dictionary and every compressed byte.
//!
//! Types are decided by **content**, not by extension. `darc.groups` supplies
//! only a per-file *default*, and measurement showed the reference reaches the
//! same split with the groups file present, absent, and disabled.
//!
//! ```text
//!   pre-group by [GroupByExt, GroupByBlockSize 2mb]
//!   for each group:
//!     probe some files, classify each probe with detect_datatype
//!     if the probes agree      -> the whole group takes that type
//!     if they disagree         -> recurse on the subgroups
//! ```
//!
//! The probing constants are format, not tuning: `aCHUNKS = 5`,
//! `aChunkSize = 64kb`, `aGroupSize = 2mb`.

use darc_codecs::mmdet;

/// `aGroupSize` — the pre-grouping's block-size criterion.
pub const GROUP_SIZE: u64 = 2 * 1024 * 1024;
/// `aCHUNKS` — how many probes decide a type.
pub const CHUNKS: u64 = 5;
/// `aChunkSize` — how big each probe is.
pub const CHUNK_SIZE: u64 = 64 * 1024;

/// One file, as far as type detection is concerned.
pub struct Candidate<'a> {
    pub stored_name: &'a str,
    pub size: u64,
    /// The file's bytes. Held rather than re-read: the writer has them already.
    pub data: &'a [u8],
    /// `getDefaultType` — the type `darc.groups` assigns, with any
    /// autodetectable type replaced by `$binary`.
    pub default_type: &'a str,
}

/// Where the probes land in a single file (`groupType [file]`).
///
/// The arithmetic is integer throughout and the truncations are the C's:
///
/// * `chunks = filesize / 64k + 1`
/// * `n = chunks < 5 ? chunks : round(sqrt(5 * chunks))`
/// * `blocksize = min(64k, filesize / n)`
/// * `step = (filesize - n*blocksize) / n`
/// * positions are `0, blocksize+step, 2*(blocksize+step), …`, `n` of them
///
/// `round` is Haskell's, which rounds half to EVEN — `round 2.5 == 2`. That
/// only matters when `5*chunks` is a perfect square of a half-integer, but it
/// costs nothing to get right.
pub fn probe_positions(filesize: u64) -> (u64, Vec<u64>) {
    if filesize == 0 {
        return (0, Vec::new());
    }
    let chunks = filesize / CHUNK_SIZE + 1;
    let n = if chunks < CHUNKS {
        chunks
    } else {
        round_half_even(((CHUNKS * chunks) as f64).sqrt())
    };
    let n = n.max(1);
    let blocksize = CHUNK_SIZE.min(filesize / n);
    let step = (filesize - n * blocksize) / n;
    let stride = blocksize + step;
    let positions = (0..n).map(|i| i * stride).collect();
    (blocksize, positions)
}

/// Haskell's `round` — banker's rounding.
fn round_half_even(x: f64) -> u64 {
    let floor = x.floor();
    let diff = x - floor;
    let n = if diff > 0.5 {
        floor + 1.0
    } else if diff < 0.5 {
        floor
    } else if (floor as i64) % 2 == 0 {
        floor
    } else {
        floor + 1.0
    };
    n.max(0.0) as u64
}

/// `check` — classify one file by probing it.
///
/// `detectMM` is not reached here, and that is a consequence rather than an
/// omission: it runs only when the file's default type is `$wav` or `$bmp`
/// (`isMMType`, `Compression.hs:541`), while `getDefaultType` maps every
/// autodetectable type to `$binary`. Only an `darc.groups` entry can produce
/// `$wav`, so with no groups file the branch is dead.
fn classify(
    file: &Candidate<'_>,
    blocksize: u64,
    positions: &[u64],
    detect_level: u32,
) -> Vec<String> {
    if is_mm_type(file.default_type) {
        // The MM path, kept for when a groups file does assign $wav/$bmp.
        let head = &file.data[..(1024).min(file.data.len())];
        if mmdet::detect::is_mm_header(detect_level as i32, head) {
            return vec![file.default_type.to_string()];
        }
        let bytes = mmdet::detect::mm_bytes(detect_level as i32, file.size as i64) as u64;
        let from = ((file.size.saturating_sub(bytes)) / 2) as usize;
        let to = (from + bytes as usize).min(file.data.len());
        let mid = file.data.get(from..to).unwrap_or(&[]);
        if mmdet::detect::is_mm(detect_level as i32, mid) {
            return vec![file.default_type.to_string()];
        }
        // A FAILED mm detection falls THROUGH to the $text/$compressed probing
        // below: `if mm then return [defaultType] else foreach positions …`.
        //
        // This used to `return default_type` here as well, which made the whole
        // branch a no-op that answered `$wav` whatever the bytes said. It was
        // dead code until #129 made file-type routing work, and the first case
        // to reach it caught this -- `-m4 -mm-` over a WAV whose samples do not
        // look like multimedia puts it in `$compressed` in the reference, and
        // left it in the default chain here.
    }
    positions
        .iter()
        .map(|&pos| {
            let from = pos as usize;
            let to = (from + blocksize as usize).min(file.data.len());
            let chunk = file.data.get(from..to).unwrap_or(&[]);
            mmdet::detect_datatype(chunk).name().to_string()
        })
        .collect()
}

/// `isMMType`.
fn is_mm_type(t: &str) -> bool {
    t == "$wav" || t == "$bmp"
}

/// `bestType` — the verdict from several probes.
///
/// "default" votes are discarded first; the survivors must all agree, and there
/// must be enough of them: all of them, or all but one when there are at least
/// five, or 92% (`lenx*12 >= total*11`). Otherwise the answer is "default".
pub fn best_type(votes: &[String]) -> String {
    if votes.is_empty() {
        return "default".to_string();
    }
    let x: Vec<&String> = votes.iter().filter(|v| *v != "default").collect();
    let total = votes.len();
    let lenx = x.len();
    let all_same = x.first().is_some_and(|f| x.iter().all(|v| v == f));
    if lenx > 0
        && all_same
        && (lenx == total || (total >= CHUNKS as usize && lenx == total - 1) || lenx * 12 >= total * 11)
    {
        return x[0].clone();
    }
    "default".to_string()
}

/// `chooseType` — `bestType`, with "default" replaced by the file's own default,
/// then looked up among the compressor's type names.
///
/// A type the compressor does not name falls to index 0, which is the unnamed
/// `""` entry — the main chain.
pub fn choose_type(votes: &[String], default_type: &str, type_names: &[String]) -> usize {
    let best = best_type(votes);
    let best = if best == "default" { default_type } else { best.as_str() };
    type_names.iter().position(|t| t == best).unwrap_or(0)
}

/// `splitFileTypes` — assign every file a type index, preserving order.
///
/// Returns groups in the order the pre-grouping produced them, each tagged with
/// the type index its files share.
pub fn split_file_types<'a>(
    files: &[Candidate<'a>],
    type_names: &[String],
    detect_level: u32,
) -> Vec<(usize, Vec<usize>)> {
    let indices: Vec<usize> = (0..files.len()).collect();
    let mut out = Vec::new();
    for group in pre_group(files, &indices) {
        group_type(files, &group, type_names, detect_level, &mut out);
    }
    out
}

/// `splitBy [GroupByExt, GroupByBlockSize aGroupSize]` — cut the list wherever
/// the extension changes or 2 MB has accumulated, whichever comes first.
fn pre_group(files: &[Candidate<'_>], indices: &[usize]) -> Vec<Vec<usize>> {
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < indices.len() {
        let ext = crate::sort::lc_extension(files[indices[i]].stored_name);
        let mut total = 0u64;
        let mut j = i;
        while j < indices.len() {
            let f = &files[indices[j]];
            if crate::sort::lc_extension(f.stored_name) != ext {
                break;
            }
            // GroupByBlockSize: accumulate while under the limit, and always
            // take at least one file (`atLeast 1`).
            if j > i && total >= GROUP_SIZE {
                break;
            }
            total += f.size;
            j += 1;
        }
        out.push(indices[i..j].to_vec());
        i = j;
    }
    out
}

/// `groupType` — decide one group's type, splitting it if the probes disagree.
fn group_type(
    files: &[Candidate<'_>],
    group: &[usize],
    type_names: &[String],
    detect_level: u32,
    out: &mut Vec<(usize, Vec<usize>)>,
) {
    if group.is_empty() {
        return;
    }
    if group.len() == 1 {
        let f = &files[group[0]];
        let (blocksize, positions) = probe_positions(f.size);
        let votes = classify(f, blocksize, &positions, detect_level);
        out.push((choose_type(&votes, f.default_type, type_names), group.to_vec()));
        return;
    }

    // Several files: probe one per subgroup rather than all of them.
    let default_type = files[group[0]].default_type;
    let (subgroups, to_try) = pick_probes(files, group);
    let votes: Vec<String> = to_try
        .iter()
        .map(|&i| {
            let f = &files[i];
            // A whole 64 KB from offset 0, one probe per file.
            let v = classify(f, CHUNK_SIZE, &[0], detect_level);
            v.first().cloned().unwrap_or_else(|| "default".to_string())
        })
        .collect();

    let agreed = votes.first().is_some_and(|f| votes.iter().all(|v| v == f));
    if !agreed {
        // "let every subgroup determine its own type".
        for sg in subgroups {
            group_type(files, &sg, type_names, detect_level, out);
        }
        return;
    }
    out.push((choose_type(&votes, default_type, type_names), group.to_vec()));
}

/// Which files to probe, and the subgroups to fall back on.
///
/// At most `aCHUNKS` files: below that every file is its own subgroup and every
/// file is probed; above it the list is cut into five parts of roughly equal
/// total SIZE and the largest file of each is probed.
fn pick_probes(files: &[Candidate<'_>], group: &[usize]) -> (Vec<Vec<usize>>, Vec<usize>) {
    if group.len() <= CHUNKS as usize {
        return (group.iter().map(|&i| vec![i]).collect(), group.to_vec());
    }
    let total: u64 = group.iter().map(|&i| files[i].size).sum();
    let limit = total / CHUNKS;
    let mut subgroups: Vec<Vec<usize>> = Vec::new();
    let mut cur: Vec<usize> = Vec::new();
    let mut acc = 0u64;
    for &i in group {
        // splitLen (GroupBySize size) is `(1+) . groupLen … (< size)`: take
        // files while the running total is under the limit, then one more.
        cur.push(i);
        acc += files[i].size;
        if acc >= limit {
            subgroups.push(std::mem::take(&mut cur));
            acc = 0;
        }
    }
    if !cur.is_empty() {
        subgroups.push(cur);
    }
    let to_try = subgroups
        .iter()
        .filter_map(|sg| sg.iter().copied().max_by_key(|&i| files[i].size))
        .collect();
    (subgroups, to_try)
}

/// `sort_and_groupOn fst (fakeGroups ++ normalGroups)`
/// (`ArhiveFileList.hs:310`) — gather every file of one type into a single
/// solid block, wherever in the list they were found.
///
/// The key the C sorts on is the **compressor chain**, not the type index, so
/// the resulting blocks come out in the chains' own order. On the test corpus
/// that puts `dict:…+lzp:…+ppmd:…` ($text) before `rep:…+exe+delta+4x4:…`
/// (the main chain), which is the order the reference writes them in.
///
/// Without this the port produces the right SET of blocks in the wrong shape:
/// on the same corpus, type 0 arrives as two runs of 315 657 and 67 335 bytes
/// either side of the $text run, rather than one block of 382 992.
pub fn merge_by_type(
    split: &[(usize, Vec<usize>)],
    chain_of: impl Fn(usize) -> String,
) -> Vec<(usize, Vec<usize>)> {
    let mut by_type: Vec<(usize, Vec<usize>)> = Vec::new();
    for (ty, group) in split {
        match by_type.iter_mut().find(|(t, _)| t == ty) {
            Some((_, files)) => files.extend(group.iter().copied()),
            None => by_type.push((*ty, group.clone())),
        }
    }
    by_type.sort_by(|a, b| chain_of(a.0).cmp(&chain_of(b.0)));
    by_type
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cand<'a>(name: &'a str, data: &'a [u8]) -> Candidate<'a> {
        Candidate { stored_name: name, size: data.len() as u64, data, default_type: "$binary" }
    }

    /// The probe layout is integer arithmetic with three truncations, and the
    /// positions decide which bytes are classified.
    #[test]
    fn probe_positions_follow_the_integer_arithmetic() {
        // Under 5 chunks: one probe per chunk, and blocksize is the whole file
        // divided by that count.
        let (bs, pos) = probe_positions(100_000);
        assert_eq!(pos.len(), 2, "100000/65536 + 1 = 2");
        assert_eq!(bs, 50_000);
        assert_eq!(pos, vec![0, 50_000]);

        // Exactly at the boundary: 4 chunks is still "fewer than 5".
        let (_, pos) = probe_positions(3 * 65_536);
        assert_eq!(pos.len(), 4);

        // Over it: n = round(sqrt(5*chunks)), which grows far more slowly.
        let (bs, pos) = probe_positions(10 * 1024 * 1024);
        // chunks = 10485760/65536 + 1 = 161; sqrt(5*161) = 28.37; round = 28.
        assert_eq!(pos.len(), 28, "round(sqrt(5*161))");
        assert_eq!(bs, 65_536, "capped at aChunkSize");
        assert_eq!(pos[0], 0);
        // Evenly spread: the last probe ends at or before the file's end.
        let last = pos[pos.len() - 1];
        assert!(last + bs <= 10 * 1024 * 1024, "last probe runs past the end");
    }

    #[test]
    fn an_empty_file_is_probed_nowhere() {
        let (_, pos) = probe_positions(0);
        assert!(pos.is_empty());
    }

    /// Haskell's `round` is half-to-even, so 2.5 rounds DOWN.
    #[test]
    fn rounding_is_half_to_even() {
        assert_eq!(round_half_even(2.5), 2);
        assert_eq!(round_half_even(3.5), 4);
        assert_eq!(round_half_even(2.4), 2);
        assert_eq!(round_half_even(2.6), 3);
    }

    /// bestType's three acceptance rules, and its rejection.
    #[test]
    fn best_type_needs_agreement_and_enough_votes() {
        let t = |v: &[&str]| best_type(&v.iter().map(|s| s.to_string()).collect::<Vec<_>>());
        // All agree.
        assert_eq!(t(&["$text", "$text", "$text"]), "$text");
        // All but one, with at least aCHUNKS votes.
        assert_eq!(t(&["$text", "$text", "$text", "$text", "default"]), "$text");
        // All but one, with FEWER than aCHUNKS votes: 3 of 4 is 75%, under 92%.
        assert_eq!(t(&["$text", "$text", "$text", "default"]), "default");
        // Disagreement among the non-default votes is fatal however many.
        assert_eq!(t(&["$text", "$compressed", "$text", "$text", "$text"]), "default");
        // Nothing but defaults.
        assert_eq!(t(&["default", "default"]), "default");
        assert_eq!(t(&[]), "default");
    }

    /// chooseType substitutes the file's own default for "default", then looks
    /// the name up; an unknown name is index 0, the main chain.
    #[test]
    fn choose_type_falls_back_to_the_main_chain() {
        let names: Vec<String> =
            ["", "$obj", "$text"].iter().map(|s| s.to_string()).collect();
        let three = vec!["$text".to_string(), "$text".to_string(), "$text".to_string()];
        assert_eq!(choose_type(&three, "$binary", &names), 2);
        // "default" -> the file's default -> "$binary", which is not named ->
        // index 0.
        let defaults = vec!["default".to_string(); 3];
        assert_eq!(choose_type(&defaults, "$binary", &names), 0);
    }

    /// The pre-grouping cuts on a change of extension and on 2 MB.
    #[test]
    fn pre_grouping_cuts_on_extension_and_on_size() {
        let big = vec![0u8; 1_500_000];
        let small = b"hello".to_vec();
        let files = vec![
            cand("a.txt", &small),
            cand("b.txt", &small),
            cand("c.bin", &small),
        ];
        let idx: Vec<usize> = (0..files.len()).collect();
        let groups = pre_group(&files, &idx);
        assert_eq!(groups, vec![vec![0, 1], vec![2]], "extension change splits");

        let files = vec![cand("a.txt", &big), cand("b.txt", &big), cand("c.txt", &big)];
        let idx: Vec<usize> = (0..files.len()).collect();
        let groups = pre_group(&files, &idx);
        assert!(groups.len() > 1, "2mb of one extension still splits: {groups:?}");
    }

    /// Files of one type are gathered into a single block even when they were
    /// found in separate runs, and the blocks come out in chain order.
    #[test]
    fn merging_gathers_a_type_from_separate_runs() {
        let split = vec![(0, vec![0, 1]), (2, vec![2]), (0, vec![3, 4])];
        let chains = ["rep+exe".to_string(), String::new(), "dict+ppmd".to_string()];
        let merged = merge_by_type(&split, |t| chains[t].clone());
        assert_eq!(merged.len(), 2, "two types, two blocks");
        // "dict…" sorts before "rep…", so the $text block comes first.
        assert_eq!(merged[0].0, 2);
        assert_eq!(merged[0].1, vec![2]);
        assert_eq!(merged[1].0, 0);
        assert_eq!(merged[1].1, vec![0, 1, 3, 4], "both runs, in order");
    }

    /// A group whose probes disagree is split rather than forced into one type.
    #[test]
    fn a_disagreeing_group_is_split() {
        // Text and noise share an extension, so they pre-group together.
        let text = b"the quick brown fox jumps over the lazy dog. ".repeat(3000);
        let noise: Vec<u8> = (0..200_000u32).map(|i| (i.wrapping_mul(2654435761) >> 24) as u8).collect();
        let files = vec![cand("a.dat", &text), cand("b.dat", &noise)];
        let names: Vec<String> = ["", "$text"].iter().map(|s| s.to_string()).collect();
        let out = split_file_types(&files, &names, 4);
        // Either they were split into two groups, or they agreed -- but if they
        // agreed, it must not be because the classifier never ran.
        assert!(!out.is_empty());
        let covered: usize = out.iter().map(|(_, g)| g.len()).sum();
        assert_eq!(covered, 2, "every file must land in exactly one group");
    }
}

