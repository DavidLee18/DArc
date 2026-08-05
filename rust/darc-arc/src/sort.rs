//! The solid sort order — `sortFiles` (`ArhiveFileList.hs:28`).
//!
//! Every compression level except `-m0` reorders files before packing them, with
//! `aDEFAULT_SOLID_SORT_ORDER = "gerpn"` (`Options.hs:416`). The point is
//! compression: putting similar files next to each other means the later ones
//! match against the earlier ones inside the solid block.
//!
//! It is also **archive-visible**, which is why it is here rather than treated
//! as a heuristic. A different order is a different archive.
//!
//! ```text
//!   g   group by darc.groups         a 409-line list of wildcards, in order
//!   e   then by lowercase extension
//!   r   then run `reorder`          the similarity pass, applied AFTER "pn"
//!   p   then by directory
//!   n   then by base name
//! ```
//!
//! Directories are not sorted at all: `sortFiles` splits them off first and
//! puts them back at the head (`dirs ++ sortBy sort_order files`).

use crate::directory::Entry;

// ---------------------------------------------------------------------------
// darc.groups
// ---------------------------------------------------------------------------

/// The parsed groups file: an ordered list of wildcards, plus where `$default`
/// sits among them.
#[derive(Clone, Debug)]
pub struct Groups {
    patterns: Vec<String>,
    default_index: usize,
}

impl Groups {
    /// Parse `darc.groups`.
    ///
    /// Comment lines (`;…`) and empty lines are dropped, and `'\'` becomes
    /// `'/'`. Type markers like `$text` stay in the list even though no file
    /// can match them — they still occupy a group index, and dropping them
    /// would renumber every group after them.
    pub fn parse(text: &str) -> Groups {
        let patterns: Vec<String> = text
            .lines()
            .map(|l| l.trim_end_matches('\r').replace('\\', "/"))
            .filter(|l| !l.is_empty() && !l.starts_with(';'))
            .collect();
        // `lower_group_strings = map strLower group_strings ++ ["$default"]`,
        // so an absent $default is the index one past the end.
        let default_index = patterns
            .iter()
            .position(|p| p.to_lowercase() == "$default")
            .unwrap_or(patterns.len());
        Groups { patterns, default_index }
    }

    /// The number of groups `partitionList` is told about:
    /// `length group_strings`, and `$default` may be one past that when the
    /// file does not name it.
    pub fn count(&self) -> usize {
        self.patterns.len().max(self.default_index + 1)
    }

    /// `findGroup` — the first matching wildcard's index, else `$default`'s.
    pub fn group_of(&self, stored_name: &str) -> usize {
        for (i, p) in self.patterns.iter().enumerate() {
            if match_fp(p, stored_name) {
                return i;
            }
        }
        self.default_index
    }

    /// Every file in one group, for when no groups file is used
    /// (`--groups-` gives `[reANY_FILE]`).
    pub fn single() -> Groups {
        Groups { patterns: vec!["*".to_string()], default_index: 1 }
    }
}

/// `match_FP fpBasename` (`FileInfo.hs:141`) — what a groups-file line matches.
///
/// Three shapes, and which one applies is decided by splitting the pattern into
/// directory, name and extension:
///
/// * `*` — everything.
/// * `*.ext` — the file's extension only.
/// * a bare name (no directory) — the file's BASE NAME.
/// * anything with a directory — the full stored name.
fn match_fp(pattern: &str, stored_name: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    let base = stored_name.rsplit('/').next().unwrap_or(stored_name);
    let has_dir = pattern.contains('/');
    let stem = pattern.rsplit('/').next().unwrap_or(pattern);
    let name_part = match stem.rfind('.') {
        Some(i) => &stem[..i],
        None => stem,
    };
    if !has_dir && name_part == "*" {
        // "*.ext": compare against the extension alone, without its dot.
        let ext = lc_extension(stored_name);
        let pat_ext = match stem.rfind('.') {
            Some(i) => &stem[i + 1..],
            None => "",
        };
        return glob(&pat_ext.to_lowercase(), &ext);
    }
    let target = if has_dir { stored_name } else { base };
    glob(&pattern.to_lowercase(), &target.to_lowercase())
}

/// `match_filespecs` (`FileInfo.hs:149`) — does the name match ANY of them?
///
/// `opt_match_with` decides what is matched: `fpBasename` by default and
/// `fpFullname` under `--fullnames` (`Cmdline.hs:420`). So `arc d a.arc x.txt`
/// deletes every `x.txt` in the archive, at any depth, unless `-fn` is given.
pub fn match_filespecs(specs: &[String], stored_name: &str, full_names: bool) -> bool {
    specs.iter().any(|spec| {
        if full_names {
            glob(&spec.to_lowercase(), &stored_name.to_lowercase())
        } else {
            match_fp(spec, stored_name)
        }
    })
}

/// `fpLCExtension` — the lowercase extension WITHOUT its dot, empty if none.
pub fn lc_extension(stored_name: &str) -> String {
    let base = stored_name.rsplit('/').next().unwrap_or(stored_name);
    match base.rfind('.') {
        Some(i) => base[i + 1..].to_lowercase(),
        None => String::new(),
    }
}

/// `fpPackedDirectory` — everything before the last '/'.
fn directory_of(stored_name: &str) -> &str {
    match stored_name.rfind('/') {
        Some(i) => &stored_name[..i],
        None => "",
    }
}

/// `fpPackedBasename`.
fn basename_of(stored_name: &str) -> &str {
    stored_name.rsplit('/').next().unwrap_or(stored_name)
}

/// `match_RE (compile_RE re)` (`Utils.hs:1055`) — DArc's own wildcard matcher.
///
/// Only `*` and `?` are special. The one subtlety is `compile_RE`'s split: a
/// pattern with a single leading `*` and no other `*` is compiled as
/// `RE_FromEnd`, which reverses BOTH the pattern and the string and matches
/// forwards. That is an optimisation, not a different language, so this matches
/// the same set — but it is why `*.txt` anchors at the end rather than being a
/// general substring search.
fn glob(pattern: &str, text: &str) -> bool {
    glob_bytes(pattern.as_bytes(), text.as_bytes())
}

fn glob_bytes(p: &[u8], t: &[u8]) -> bool {
    match p.first() {
        None => t.is_empty(),
        Some(b'*') => {
            let rest = &p[1..];
            if rest.is_empty() {
                return true;
            }
            // Try every suffix, shortest match first -- `any re (tails s)`.
            (0..=t.len()).any(|i| glob_bytes(rest, &t[i..]))
        }
        Some(b'?') => !t.is_empty() && glob_bytes(&p[1..], &t[1..]),
        Some(c) => match t.first() {
            Some(x) if x == c => glob_bytes(&p[1..], &t[1..]),
            Some(_) | None => false,
        },
    }
}

// ---------------------------------------------------------------------------
// sortFiles
// ---------------------------------------------------------------------------

/// `sortFiles` — directories first, unsorted, then the files by `order`.
pub fn sort_files(order: &str, groups: &Groups, entries: &[Entry]) -> Vec<Entry> {
    let (dirs, files): (Vec<Entry>, Vec<Entry>) =
        entries.iter().cloned().partition(|e| e.is_dir);
    let mut out = dirs;
    out.extend(sort_by(order, groups, files));
    out
}

/// The recursive half of `sortFiles`.
fn sort_by(order: &str, groups: &Groups, files: Vec<Entry>) -> Vec<Entry> {
    match order.chars().next() {
        // "" -- no sorting at all, which is what -m0 gets.
        None => files,
        Some('g') => {
            let rest = &order[1..];
            // partitionList: bucket by group number, then sort each bucket by
            // the remaining criteria. Buckets come out in group order.
            let mut buckets: Vec<Vec<Entry>> = vec![Vec::new(); groups.count()];
            for f in files {
                let g = groups.group_of(&f.stored_name).min(groups.count() - 1);
                buckets[g].push(f);
            }
            buckets.into_iter().flat_map(|b| sort_by(rest, groups, b)).collect()
        }
        // `'c':xs -> (\(small,large) -> sortBy xs small ++ sortBy "s" large)
        //   . partition (\fi -> fiSize fi < i (128*kb))`
        // (ArhiveFileList.hs:48).
        //
        // NOT a sort key: it splits the list at 128 kb and sorts the halves by
        // DIFFERENT orders -- the small one by whatever follows, the large one
        // by size -- then concatenates. `partition` is stable, so each half
        // keeps its incoming order before its own sort.
        Some('c') => {
            let rest = &order[1..];
            let (small, large): (Vec<Entry>, Vec<Entry>) =
                files.into_iter().partition(|e| e.size < 128 * 1024);
            let mut out = sort_by(rest, groups, small);
            out.extend(sort_by("s", groups, large));
            out
        }
        Some('e') => {
            let rest = &order[1..];
            // sort_and_groupOn' by extension, then sort each group by the rest.
            let mut v = files;
            v.sort_by(|a, b| {
                lc_extension(&a.stored_name).cmp(&lc_extension(&b.stored_name))
            });
            let mut out = Vec::with_capacity(v.len());
            let mut i = 0;
            while i < v.len() {
                let key = lc_extension(&v[i].stored_name);
                let mut j = i;
                while j < v.len() && lc_extension(&v[j].stored_name) == key {
                    j += 1;
                }
                out.extend(sort_by(rest, groups, v[i..j].to_vec()));
                i = j;
            }
            out
        }
        // `'r':xs -> reorder . sortBy xs` -- the rest FIRST, then reorder.
        Some('r') => reorder(sort_by(&order[1..], groups, files)),
        // Everything else is a plain key comparison over the whole string.
        Some(_) => {
            let mut v = files;
            v.sort_by(|a, b| key_of(order, groups, a).cmp(&key_of(order, groups, b)));
            v
        }
    }
}

/// One component of a sort key. The variants exist so that keys of different
/// kinds never compare against each other, exactly as `SortOrder` does.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Key {
    Str(String),
    Size(u64),
    Time(i64),
    Group(usize),
}

/// `keyFunc` (`ArhiveFileList.hs:78`) — the sort key, for callers that need to
/// MERGE two already-sorted lists rather than sort one.
///
/// `mergeFilelists` must order by exactly what `sortFiles` ordered by, or the
/// merge silently produces an unsorted list. Sharing this function is what
/// guarantees that.
pub fn sort_key(order: &str, groups: &Groups, e: &Entry) -> Vec<Key> {
    key_of(order, groups, e)
}

/// `keyFunc` (`ArhiveFileList.hs:78`).
fn key_of(order: &str, groups: &Groups, e: &Entry) -> Vec<Key> {
    order
        .chars()
        .map(|c| match c {
            'p' => Key::Str(directory_of(&e.stored_name).to_string()),
            'n' => Key::Str(basename_of(&e.stored_name).to_string()),
            'e' => Key::Str(lc_extension(&e.stored_name)),
            's' | 'c' | 'i' => Key::Size(e.size),
            't' => Key::Time(e.time),
            'g' => Key::Group(groups.group_of(&e.stored_name)),
            // `key 'r' = OrderGroup . const 1` -- a constant, so it contributes
            // nothing to the ordering. Kept so the character is not an error.
            'r' => Key::Group(1),
            _ => Key::Group(0),
        })
        .collect()
}

/// `reorder` (`ArhiveFileList.hs:104`) — gather identical or similar files.
///
/// Two similarity notions, tried in order:
///
/// * same extension AND same size — probably the same file under two names.
///   Only for files over 16 KiB.
/// * same base name AND close size — probably two versions of one file. "Close"
///   means equal below 16 KiB and within a factor of two above it.
///
/// Files of 1024 bytes or fewer are left alone entirely: they are too small for
/// the guess to be worth anything.
///
/// A file adopts the group number of the first similar file already seen, so
/// the group is keyed on its EARLIEST member and later members follow it.
pub fn reorder(files: Vec<Entry>) -> Vec<Entry> {
    // (extension, size) -> group number
    let mut by_ext_size: Vec<((String, u64), usize)> = Vec::new();
    // (basename, size) -> group number, matched by `near_size`
    let mut by_name_size: Vec<((String, u64), usize)> = Vec::new();

    let near_size = |a: u64, b: u64| -> bool {
        if a <= 16 * 1024 {
            a == b
        } else {
            a >= b / 2 && a <= b.saturating_mul(2)
        }
    };

    let mut numbered: Vec<(usize, Entry)> = Vec::with_capacity(files.len());
    for (num, file) in files.into_iter().enumerate() {
        if file.size <= 1024 {
            numbered.push((num, file));
            continue;
        }
        let ext = lc_extension(&file.stored_name);
        let base = basename_of(&file.stored_name).to_string();
        let key1 = (ext, file.size);
        let key2 = (base, file.size);

        let found = if file.size <= 16 * 1024 {
            None
        } else {
            by_ext_size.iter().find(|(k, _)| *k == key1).map(|(_, n)| *n)
        };
        let found = match found {
            Some(n) => Some(n),
            None => by_name_size
                .iter()
                .find(|((n2, s2), _)| *n2 == key2.0 && near_size(key2.1, *s2))
                .map(|(_, n)| *n),
        };
        let newnum = found.unwrap_or(num);
        by_ext_size.push((key1, newnum));
        by_name_size.push((key2, newnum));
        numbered.push((newnum, file));
    }

    // "Sort the files by group number/size/name/path".
    numbered.sort_by(|(na, a), (nb, b)| {
        (na, a.size, basename_of(&a.stored_name), directory_of(&a.stored_name)).cmp(&(
            nb,
            b.size,
            basename_of(&b.stored_name),
            directory_of(&b.stored_name),
        ))
    });
    numbered.into_iter().map(|(_, f)| f).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, size: u64) -> Entry {
        Entry {
            stored_name: name.to_string(),
            size,
            time: 0,
            is_dir: false,
            crc: 0,
            block: 0,
            pos_in_block: 0,
        }
    }

    #[test]
    fn the_glob_matcher_handles_the_forms_arc_groups_uses() {
        assert!(glob("*", "anything"));
        assert!(glob("*.txt", "a.txt"));
        assert!(!glob("*.txt", "a.txtx"));
        assert!(glob("file_id.diz", "file_id.diz"));
        assert!(!glob("file_id.diz", "file_id.diz.bak"));
        assert!(glob("a?c", "abc"));
        assert!(!glob("a?c", "ac"));
        // Two stars: a genuine substring search.
        assert!(glob("*mid*", "xxmidyy"));
        assert!(!glob("*mid*", "xxmydyy"));
        assert!(glob("", ""));
        assert!(!glob("", "x"));
    }

    /// A bare pattern matches the BASE NAME, not the full path -- so a groups
    /// entry cannot accidentally match on a directory component.
    #[test]
    fn a_bare_pattern_matches_the_base_name_only() {
        assert!(match_fp("readme.txt", "docs/readme.txt"));
        assert!(!match_fp("docs", "docs/readme.txt"));
        // ...while one with a separator matches the whole stored name.
        assert!(match_fp("docs/*", "docs/readme.txt"));
    }

    #[test]
    fn an_extension_pattern_matches_the_extension() {
        assert!(match_fp("*.txt", "a/b/c.txt"));
        assert!(match_fp("*.TXT", "a/b/c.txt"));
        assert!(!match_fp("*.txt", "a/b/c.bin"));
    }

    /// The type markers are not droppable: they occupy group indices, and
    /// removing them would renumber every group after them.
    #[test]
    fn type_markers_occupy_a_group_index_although_nothing_matches_them() {
        let g = Groups::parse("$text\na.txt\n$binary\nb.bin\n");
        assert_eq!(g.group_of("a.txt"), 1);
        assert_eq!(g.group_of("b.bin"), 3);
        // Nothing is named "$text", so index 0 is unreachable -- and still
        // reserved.
        assert_eq!(g.group_of("other.dat"), g.default_index);
    }

    #[test]
    fn comments_and_blank_lines_are_dropped() {
        let g = Groups::parse("; a comment\n\n*.txt\n; another\n*.bin\n");
        assert_eq!(g.group_of("x.txt"), 0);
        assert_eq!(g.group_of("x.bin"), 1);
    }

    /// An absent $default sits one past the end, so unmatched files sort after
    /// every named group.
    #[test]
    fn an_absent_default_group_is_last() {
        let g = Groups::parse("*.txt\n");
        assert_eq!(g.default_index, 1);
        assert_eq!(g.group_of("x.bin"), 1);
        assert_eq!(g.count(), 2);
    }

    #[test]
    fn an_explicit_default_group_sits_where_it_is_written() {
        let g = Groups::parse("*.txt\n$default\n*.bin\n");
        assert_eq!(g.default_index, 1);
        assert_eq!(g.group_of("x.dat"), 1);
        assert_eq!(g.group_of("x.bin"), 2);
    }

    /// Directories are not sorted -- they are split off and put back in front.
    #[test]
    fn directories_keep_their_order_at_the_head() {
        let g = Groups::single();
        let mut items = vec![
            Entry { is_dir: true, ..entry("zzz", 0) },
            entry("b.txt", 10),
            Entry { is_dir: true, ..entry("aaa", 0) },
            entry("a.txt", 10),
        ];
        let out = sort_files("n", &g, &items);
        assert_eq!(out[0].stored_name, "zzz", "directory order is preserved");
        assert_eq!(out[1].stored_name, "aaa");
        assert_eq!(out[2].stored_name, "a.txt", "files are sorted");
        assert_eq!(out[3].stored_name, "b.txt");
        items.clear();
    }

    /// The case that exposed the missing sort: a .bin among .txt files moves to
    /// the front, because darc.groups puts them in different groups.
    #[test]
    fn a_different_group_moves_a_file_to_the_front() {
        let g = Groups::parse("*.bin\n*.txt\n");
        let items = vec![
            entry("aaa-file.txt", 3),
            entry("a/shallow.txt", 4),
            entry("a/b/zero-length.bin", 0),
            entry("a/b/c/d/deep.txt", 4),
        ];
        let out = sort_files("gpn", &g, &items);
        assert_eq!(out[0].stored_name, "a/b/zero-length.bin");
    }

    /// Sorting by "pn" is directory first, then base name.
    #[test]
    fn pn_sorts_by_directory_then_name() {
        let g = Groups::single();
        let items = vec![
            entry("b/1.txt", 1),
            entry("a/2.txt", 1),
            entry("a/1.txt", 1),
            entry("1.txt", 1),
        ];
        let out = sort_files("pn", &g, &items);
        let names: Vec<&str> = out.iter().map(|e| e.stored_name.as_str()).collect();
        assert_eq!(names, ["1.txt", "a/1.txt", "a/2.txt", "b/1.txt"]);
    }

    /// reorder leaves small files alone: at 1024 bytes or less the similarity
    /// guess is not worth making.
    #[test]
    fn reorder_ignores_files_of_1024_bytes_or_fewer() {
        let items = vec![entry("a.txt", 1000), entry("b.txt", 1000), entry("c.txt", 1024)];
        let out = reorder(items.clone());
        let names: Vec<&str> = out.iter().map(|e| e.stored_name.as_str()).collect();
        assert_eq!(names, ["a.txt", "b.txt", "c.txt"], "order unchanged");
    }

    /// Two files with the same extension and size are presumed to be the same
    /// file under two names, and are brought together.
    #[test]
    fn reorder_gathers_same_extension_and_size() {
        let items = vec![
            entry("one.dat", 100_000),
            entry("other.txt", 50_000),
            entry("two.dat", 100_000),
        ];
        let out = reorder(items);
        let names: Vec<&str> = out.iter().map(|e| e.stored_name.as_str()).collect();
        // one.dat and two.dat share a group number, so they end up adjacent.
        let i = names.iter().position(|n| *n == "one.dat").expect("present");
        let j = names.iter().position(|n| *n == "two.dat").expect("present");
        assert_eq!(j, i + 1, "{names:?}");
    }

    /// "Close in size" is a factor of two above 16 KiB and exact equality below,
    /// so a 100 KB and a 150 KB file with one name are grouped and a 100 KB and
    /// a 300 KB one are not.
    #[test]
    fn near_size_is_a_factor_of_two_above_16kb() {
        let close = reorder(vec![
            entry("a/v.bin", 100_000),
            entry("filler.zzz", 20_000),
            entry("b/v.bin", 150_000),
        ]);
        let n: Vec<&str> = close.iter().map(|e| e.stored_name.as_str()).collect();
        let i = n.iter().position(|x| *x == "a/v.bin").expect("present");
        let j = n.iter().position(|x| *x == "b/v.bin").expect("present");
        assert_eq!(j, i + 1, "{n:?}");
    }
}

impl Groups {
    /// `group_type_names` (`Cmdline.hs:404`) — the file TYPE each group line
    /// belongs to.
    ///
    /// ```text
    ///   go t (x:xs) = case x of
    ///     '$':_ | x /= "$default" -> x : go x xs   -- names itself AND becomes t
    ///     _                       -> t : go t xs   -- a wildcard inherits t
    /// ```
    ///
    /// So in `$wav` / `*.wav` / `*.wave` all three lines have type `$wav`: the
    /// marker names the type and every wildcard under it inherits it, until
    /// the next marker. The walk starts at `$binary`, so lines before any
    /// marker are binary.
    ///
    /// `$default` is excluded because it marks a POSITION in the order, not a
    /// type — treating it as one would give every unmatched file the type
    /// `$default`, which no compressor list contains.
    fn group_type_names(&self) -> Vec<String> {
        let mut t = "$binary".to_string();
        let mut out = Vec::with_capacity(self.patterns.len());
        for p in &self.patterns {
            let lower = p.to_lowercase();
            match lower.starts_with('$') && lower != "$default" {
                true => {
                    t = lower.clone();
                    out.push(lower);
                }
                false => out.push(t.clone()),
            }
        }
        out
    }

    /// `getDefaultType` (`ArhiveFileList.hs:520`) — the type `darc.groups`
    /// assigns to a file.
    ///
    /// A type the detector can work out for itself becomes `$binary`, so that
    /// autodetection decides it rather than the file's name. `DETECTABLE_TYPES`
    /// names exactly `$text` and `$compressed`.
    ///
    /// **This is what makes `$wav` and `$bmp` reachable.** Until it existed the
    /// caller passed a hardcoded `$binary`, so the multimedia branch in
    /// `filetype::classify` was dead and `-m9` compressed a WAV with the
    /// general binary chain where the reference used `tta`.
    /// The RAW type a file's group carries, without the `$binary`
    /// substitution.
    ///
    /// `cfType` (`ArhiveDirectory.hs:408`) uses this one: it maps the group
    /// straight to a compressor index through `opt_group2type`. Only
    /// [`Self::default_type`] applies the substitution, and only because
    /// `getDefaultType` feeds the DETECTOR, which must be free to disagree with
    /// the filename. Using the substituted name for partitioning would send
    /// every `$text` file to the binary chain.
    pub fn group_type_name(&self, stored_name: &str) -> String {
        self.group_type_names().get(self.group_of(stored_name)).cloned().unwrap_or_default()
    }

    /// `cfType` (`ArhiveDirectory.hs:408`) — the file's index in the COMPRESSOR
    /// LIST, via `opt_group2type`.
    ///
    /// `typeNum t = t elemIndex (map fst dataCompressor) defaultVal 0`: a group
    /// type the list does not name falls to 0, the unnamed default entry.
    pub fn cf_type(&self, stored_name: &str, type_names: &[String]) -> usize {
        let raw = self.group_type_name(stored_name);
        type_names.iter().position(|t| *t == raw).unwrap_or(0)
    }

    /// `getDefaultType` (`ArhiveFileList.hs:520`).
    ///
    /// ```haskell
    ///   typ = fst (opt_data_compressor command !! cfType command file)
    /// ```
    ///
    /// **It reads the COMPRESSOR LIST, not the groups file**, and that
    /// indirection is load-bearing rather than incidental. `-mm-` removes the
    /// `$wav`/`$bmp` entries, so a `.wav` then indexes to 0 — whose type is
    /// `""` — and comes back `$binary`. The file stops being a multimedia type
    /// at all, `detectMM` never runs, and it is probed like anything else.
    ///
    /// Reading the groups file directly instead kept answering `$wav` after
    /// `-mm-` had removed that entry, so `-m4 -mm-` left a WAV in the default
    /// chain where the reference put it in `$compressed`.
    pub fn default_type(&self, stored_name: &str, type_names: &[String]) -> String {
        let idx = self.cf_type(stored_name, type_names);
        let typ = type_names.get(idx).cloned().unwrap_or_default();
        match typ.is_empty()
            || darc_codecs::mmdet::DETECTABLE_TYPES.split_whitespace().any(|d| d == typ)
        {
            true => "$binary".to_string(),
            false => typ,
        }
    }
}

#[cfg(test)]
mod default_type_tests {
    use super::Groups;

    /// A compressor list that NAMES the multimedia types. getDefaultType reads
    /// this list, not the groups file, so a list without $wav can never return
    /// $wav however the groups file matches -- which is exactly what `-mm-`
    /// exploits.
    fn names() -> Vec<String> {
        ["", "$obj", "$text", "$wav", "$bmp"].iter().map(|s| s.to_string()).collect()
    }

    fn without_mm() -> Vec<String> {
        ["", "$obj", "$text"].iter().map(|s| s.to_string()).collect()
    }

    const SAMPLE: &str = "\
;a comment
*.exe
$wav
*.wav
*.wave
$bmp
*.bmp
$default
$text
*.txt
";

    /// A marker names its own type and every wildcard under it inherits.
    #[test]
    fn a_marker_types_the_wildcards_that_follow_it() {
        let g = Groups::parse(SAMPLE);
        assert_eq!(g.default_type("a.wav", &names()), "$wav");
        assert_eq!(g.default_type("a.wave", &names()), "$wav");
        assert_eq!(g.default_type("a.bmp", &names()), "$bmp");
        // Before any marker the walk is still at its initial $binary.
        assert_eq!(g.default_type("a.exe", &names()), "$binary");
    }

    /// A type the detector can find itself is handed back as `$binary`, so
    /// autodetection decides it instead of the name.
    #[test]
    fn detectable_types_become_binary() {
        let g = Groups::parse(SAMPLE);
        assert_eq!(g.default_type("a.txt", &names()), "$binary");
    }

    /// `$default` is a POSITION, not a type -- and an unmatched file inherits
    /// whatever marker was in force where `$default` sits.
    ///
    /// That is a quirk of the reference, not of this port: `go` has no case for
    /// `$default`, so it falls through to `t : go t xs` and emits the CURRENT
    /// type. In SAMPLE below `$default` follows `$bmp`, so unmatched files are
    /// typed `$bmp` -- which looks wrong and is faithful.
    ///
    /// The shipped `darc.groups` puts `$default` immediately after `$binary`
    /// (lines 162 and 164), so in practice unmatched files get `$binary` and
    /// the quirk never shows. A test that only used the real file would not
    /// have found this, and a "fix" would have been a divergence.
    #[test]
    fn an_unmatched_file_inherits_the_type_in_force_at_default() {
        let g = Groups::parse(SAMPLE);
        assert_eq!(g.default_type("a.unknown", &names()), "$bmp");
        // With `$default` after `$binary`, as the shipped file has it.
        let real = Groups::parse("*.exe\n$binary\n*.bin\n$default\n$wav\n*.wav\n");
        assert_eq!(real.default_type("a.unknown", &names()), "$binary");
        assert_eq!(real.default_type("a.wav", &names()), "$wav");
        // And with no groups file at all, which is `--groups-`.
        assert_eq!(Groups::single().default_type("a.wav", &names()), "$binary");
    }
}
