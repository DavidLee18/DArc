//! Merging an archive's contents with what is on disk — `joinLists`
//! (`ArhiveFileList.hs:145`).
//!
//! This is what `u`, `f`, `d` and `--sync` are: `a` replaces, `u` and `f` take
//! whichever file is newer, `f` refuses to add anything new, and `--sync` makes
//! the archive match the disk exactly, deletions included.
//!
//! Every one of those decisions is archive-visible, and so is the ORDER the two
//! lists are woven together in.
//!
//! ```text
//!   update_type  existing file      file only in archive   file only on disk
//!   'a'          take the disk one  keep it                add it
//!   'u'          take the newer     keep it                add it
//!   'f'          take the newer     keep it                DROP it
//!   's'          keep if same time  DROP it                add it
//! ```

use crate::directory::Entry;

/// Where an entry came from. The merge has to keep them apart because an
/// archived file is copied and a disk file is packed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    Archive,
    Disk,
}

/// One candidate for the output archive.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Candidate {
    pub entry: Entry,
    pub origin: Origin,
    /// Which input archive this came from, for `Origin::Archive`.
    ///
    /// `j` merges several archives and a block number is local to the archive
    /// that carried it, so two inputs both have a block 0. The merge picks a
    /// winner per name by TIMESTAMP under -u/-f, which is why this rides along
    /// with the entry instead of being reconstructed afterwards from the name:
    /// there is no way to tell from the outside which copy won.
    ///
    /// Meaningless for `Origin::Disk`; 0 there.
    pub archive: usize,
}

/// `opt_update_type` — the letter `Cmdline.hs` derives from the command.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UpdateType {
    /// `a` — always take the file from disk.
    Add,
    /// `u` — take whichever is newer.
    Update,
    /// `f` — take whichever is newer, but add nothing new.
    Freshen,
    /// `--sync` — make the archive match the disk, deletions included.
    Sync,
}

/// `removeDuplicates` — keep the FIRST of any run sharing a stored name.
///
/// First, not last: the C's comment says so explicitly, and it is why the
/// archive's own earlier entry survives when a directory block lists a name
/// twice.
fn dedup(list: &[Candidate]) -> Vec<Candidate> {
    let mut seen: Vec<&str> = Vec::new();
    let mut out = Vec::new();
    for c in list {
        if seen.contains(&c.entry.stored_name.as_str()) {
            continue;
        }
        seen.push(&c.entry.stored_name);
        out.push(c.clone());
    }
    out
}

/// `joinLists` — merge the archive's file list with the incoming one.
///
/// `merge_sorted` is `mergeFilelists`: it is used only when a sort order is in
/// effect AND `--append` is off. Otherwise the new files are appended, which is
/// what keeps an `-m0` update from reshuffling an existing archive.
pub fn join_lists(
    main_list: &[Candidate],
    added_list: &[Candidate],
    update_type: UpdateType,
    append: bool,
    sort_order: &str,
    merge_sorted: impl Fn(&[Candidate], &[Candidate]) -> Vec<Candidate>,
) -> Vec<Candidate> {
    let main_list = dedup(main_list);
    let added_list = dedup(added_list);

    // "If one of the lists is empty - simply return the other one".
    if main_list.is_empty() {
        return added_list;
    }
    if added_list.is_empty() {
        return match update_type {
            // --sync with nothing on disk empties the archive.
            UpdateType::Sync => Vec::new(),
            UpdateType::Add | UpdateType::Update | UpdateType::Freshen => main_list,
        };
    }

    // 3. Walk the archive's files, replacing them where the disk wins.
    let mut list1: Vec<Candidate> = Vec::new();
    for arcfile in &main_list {
        let diskfile = added_list
            .iter()
            .find(|d| d.entry.stored_name == arcfile.entry.stored_name);
        let chosen = match (diskfile, update_type) {
            (None, UpdateType::Sync) => None,
            (None, UpdateType::Add | UpdateType::Update | UpdateType::Freshen) => {
                Some(arcfile.clone())
            }
            (Some(d), UpdateType::Add) => Some(d.clone()),
            // sync keeps the ARCHIVED file when the times are equal, so an
            // unchanged file is copied rather than repacked.
            (Some(d), UpdateType::Sync) => Some(if arcfile.entry.time == d.entry.time {
                arcfile.clone()
            } else {
                d.clone()
            }),
            // `>=` keeps the archived file when the times are equal.
            (Some(d), UpdateType::Update | UpdateType::Freshen) => Some(if arcfile.entry.time >= d.entry.time {
                arcfile.clone()
            } else {
                d.clone()
            }),
        };
        match chosen {
            Some(c) => list1.push(c),
            None => {}
        }
    }

    // 4. Files that were not in the archive at all.
    let list2: Vec<Candidate> = match update_type {
        // "Mode f: don't take the files that were absent from the input archive".
        UpdateType::Freshen => Vec::new(),
        UpdateType::Add | UpdateType::Update | UpdateType::Sync => added_list
            .iter()
            .filter(|d| {
                !main_list.iter().any(|m| m.entry.stored_name == d.entry.stored_name)
            })
            .cloned()
            .collect(),
    };

    if append || sort_order.is_empty() {
        let mut out = list1;
        out.extend(list2);
        out
    } else {
        merge_sorted(&list1, &list2)
    }
}

/// `mergeFilelists` — interleave two ALREADY SORTED lists, directories first.
///
/// Directories are merged on `"pn"` regardless of the file sort order, which is
/// the same split `sortFiles` makes.
pub fn merge_filelists<K: Ord>(
    sort_order: &str,
    key: impl Fn(&str, &Entry) -> K,
    a: &[Candidate],
    b: &[Candidate],
) -> Vec<Candidate> {
    let (dirs1, files1): (Vec<Candidate>, Vec<Candidate>) =
        a.iter().cloned().partition(|c| c.entry.is_dir);
    let (dirs2, files2): (Vec<Candidate>, Vec<Candidate>) =
        b.iter().cloned().partition(|c| c.entry.is_dir);
    let mut out = merge_on("pn", &key, &dirs1, &dirs2);
    out.extend(merge_on(sort_order, &key, &files1, &files2));
    out
}

/// The classic two-list merge: take from whichever side compares smaller, and
/// from the FIRST on a tie, which is what makes the archive's own entry win.
fn merge_on<K: Ord>(
    order: &str,
    key: &impl Fn(&str, &Entry) -> K,
    a: &[Candidate],
    b: &[Candidate],
) -> Vec<Candidate> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0usize, 0usize);
    while i < a.len() && j < b.len() {
        if key(order, &b[j].entry) < key(order, &a[i].entry) {
            out.push(b[j].clone());
            j += 1;
        } else {
            out.push(a[i].clone());
            i += 1;
        }
    }
    out.extend_from_slice(&a[i..]);
    out.extend_from_slice(&b[j..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn c(name: &str, time: i64, origin: Origin) -> Candidate {
        Candidate {
            entry: Entry {
                stored_name: name.to_string(),
                size: 10,
                time,
                is_dir: false,
                crc: 0,
                block: 0,
                pos_in_block: 0,
            },
            origin,
            archive: 0,
        }
    }

    fn join(main: &[Candidate], added: &[Candidate], t: UpdateType) -> Vec<(String, Origin)> {
        join_lists(main, added, t, false, "", |a, b| {
            let mut v = a.to_vec();
            v.extend_from_slice(b);
            v
        })
        .into_iter()
        .map(|c| (c.entry.stored_name, c.origin))
        .collect()
    }

    /// The four update types, on a file present in both places where the
    /// ARCHIVE's copy is newer.
    #[test]
    fn an_older_disk_file_is_taken_only_by_mode_a() {
        let main = [c("f.txt", 200, Origin::Archive)];
        let added = [c("f.txt", 100, Origin::Disk)];
        assert_eq!(join(&main, &added, UpdateType::Add)[0].1, Origin::Disk);
        assert_eq!(join(&main, &added, UpdateType::Update)[0].1, Origin::Archive);
        assert_eq!(join(&main, &added, UpdateType::Freshen)[0].1, Origin::Archive);
        // sync takes the disk file because the times DIFFER, newer or not.
        assert_eq!(join(&main, &added, UpdateType::Sync)[0].1, Origin::Disk);
    }

    /// Equal times keep the archived copy under `u` and `--sync` alike, so an
    /// unchanged file is copied rather than repacked.
    #[test]
    fn an_equal_timestamp_keeps_the_archived_copy() {
        let main = [c("f.txt", 100, Origin::Archive)];
        let added = [c("f.txt", 100, Origin::Disk)];
        assert_eq!(join(&main, &added, UpdateType::Update)[0].1, Origin::Archive);
        assert_eq!(join(&main, &added, UpdateType::Sync)[0].1, Origin::Archive);
        // ...but `a` replaces unconditionally.
        assert_eq!(join(&main, &added, UpdateType::Add)[0].1, Origin::Disk);
    }

    /// `f` adds nothing new; the others do.
    #[test]
    fn freshen_adds_nothing_that_was_not_already_there() {
        let main = [c("old.txt", 100, Origin::Archive)];
        let added = [c("new.txt", 100, Origin::Disk)];
        let names = |t| -> Vec<String> {
            join(&main, &added, t).into_iter().map(|(n, _)| n).collect()
        };
        assert_eq!(names(UpdateType::Freshen), vec!["old.txt"]);
        assert_eq!(names(UpdateType::Update), vec!["old.txt", "new.txt"]);
        assert_eq!(names(UpdateType::Add), vec!["old.txt", "new.txt"]);
    }

    /// `--sync` DELETES: a file in the archive with no counterpart on disk is
    /// dropped. No other mode does that.
    #[test]
    fn sync_removes_files_that_are_gone_from_disk() {
        let main = [c("gone.txt", 100, Origin::Archive), c("kept.txt", 100, Origin::Archive)];
        let added = [c("kept.txt", 100, Origin::Disk)];
        let names: Vec<String> =
            join(&main, &added, UpdateType::Sync).into_iter().map(|(n, _)| n).collect();
        assert_eq!(names, vec!["kept.txt"]);
        // Every other mode keeps it.
        for t in [UpdateType::Add, UpdateType::Update, UpdateType::Freshen] {
            assert_eq!(join(&main, &added, t).len(), 2, "{t:?}");
        }
    }

    /// An empty disk list under --sync empties the archive, and is a no-op
    /// under the others. This is the early-exit branch, which is separate code.
    #[test]
    fn an_empty_disk_list_empties_the_archive_only_under_sync() {
        let main = [c("f.txt", 100, Origin::Archive)];
        assert!(join(&main, &[], UpdateType::Sync).is_empty());
        assert_eq!(join(&main, &[], UpdateType::Update).len(), 1);
    }

    #[test]
    fn an_empty_archive_takes_the_disk_list_whole() {
        let added = [c("a.txt", 1, Origin::Disk), c("b.txt", 1, Origin::Disk)];
        assert_eq!(join(&[], &added, UpdateType::Sync).len(), 2);
        assert_eq!(join(&[], &added, UpdateType::Freshen).len(), 2, "even f");
    }

    /// Duplicates keep the FIRST occurrence, not the last.
    #[test]
    fn duplicate_names_keep_the_first() {
        let main = [
            c("dup.txt", 111, Origin::Archive),
            c("dup.txt", 222, Origin::Archive),
        ];
        let out = join(&main, &[], UpdateType::Update);
        assert_eq!(out.len(), 1);
        // The survivor is the one with the first timestamp.
        let joined = join_lists(&main, &[], UpdateType::Update, false, "", |a, _| a.to_vec());
        assert_eq!(joined[0].entry.time, 111);
    }

    /// With a sort order and no --append the two lists are INTERLEAVED, not
    /// concatenated -- which is what keeps an updated archive in sorted order.
    #[test]
    fn a_sort_order_interleaves_rather_than_appends() {
        let key = |_order: &str, e: &Entry| vec![e.stored_name.clone()];
        let main = vec![c("a.txt", 1, Origin::Archive), c("m.txt", 1, Origin::Archive)];
        let added = vec![c("b.txt", 1, Origin::Disk), c("z.txt", 1, Origin::Disk)];
        let out = join_lists(&main, &added, UpdateType::Update, false, "n", |a, b| {
            merge_filelists("n", key, a, b)
        });
        let names: Vec<&str> = out.iter().map(|c| c.entry.stored_name.as_str()).collect();
        assert_eq!(names, ["a.txt", "b.txt", "m.txt", "z.txt"]);
    }

    /// ...while --append concatenates even when a sort order is set.
    #[test]
    fn append_concatenates_regardless_of_the_sort_order() {
        let key = |_o: &str, e: &Entry| vec![e.stored_name.clone()];
        let main = vec![c("m.txt", 1, Origin::Archive)];
        let added = vec![c("a.txt", 1, Origin::Disk)];
        let out = join_lists(&main, &added, UpdateType::Update, true, "n", |a, b| {
            merge_filelists("n", key, a, b)
        });
        let names: Vec<&str> = out.iter().map(|c| c.entry.stored_name.as_str()).collect();
        assert_eq!(names, ["m.txt", "a.txt"], "append order, not sorted order");
    }

    /// Directories are merged on "pn" whatever the file order is, and come out
    /// ahead of every file.
    #[test]
    fn directories_are_merged_separately_and_come_first() {
        let key = |_o: &str, e: &Entry| vec![e.stored_name.clone()];
        let mut d1 = c("zzz", 1, Origin::Archive);
        d1.entry.is_dir = true;
        let mut d2 = c("aaa", 1, Origin::Disk);
        d2.entry.is_dir = true;
        let a = vec![d1, c("f.txt", 1, Origin::Archive)];
        let b = vec![d2, c("g.txt", 1, Origin::Disk)];
        let out = merge_filelists("n", key, &a, &b);
        let names: Vec<&str> = out.iter().map(|c| c.entry.stored_name.as_str()).collect();
        assert_eq!(names, ["aaa", "zzz", "f.txt", "g.txt"]);
    }
}
