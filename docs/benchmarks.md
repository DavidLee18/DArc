# Historical benchmarks (2005)

Carried over verbatim from `Documentation/tests`. These are the measurements
that drove the diff/merge engine's memory representation — the custom hash
table, the pooled allocation, the packed strings — on a drive holding 252,000
files. `docs/architecture.md` cites them where it says to treat that
representation as deliberate.

They predate the Rust port by two decades and are a record of intent, not a
claim about current performance.

```text
Test results for the version dated 14.12.2005 on drive C, holding 252 thousand files.
Time and memory required by the various archivers to find all the files
and get ready for compression (en/gen - sort type; -c == +RTS -c - memory compaction algorithm)

7z   en     150 mb  40 sec  (of which 17 sec/80 mb - the search itself, of which 8 sec - in the OS)

rar          50 mb  60 sec  (of which 14 sec - the search itself, of which 8 sec - in the OS)
rar  en      50 mb  80 sec
rar gen      50 mb 120 sec

ARC 16.12  (with sortOn')
arc     -c   57 mb  37 sec  (64m/47s with directory writing)
arc  en -c   65 mb  47 sec
arc gen -c   65 mb  55 sec  (65m/63s with directory writing)
arc l   -c   59 mb  42 sec  (13m/16s=6+10gc - listing a nonexistent file)

ARC 16.12  (with UTF8Z and case-sensitive path/name sorting)
arc     -c   58 mb  38 sec
arc  en -c   86 mb  48 sec
arc gen -c   79 mb  58 sec

ARC 14.12  (with PackedString)
arc     -c   60 mb  35 sec  (of which 27 sec - the search itself, 5.5 sec - removing duplicates from the file list and 2.5 sec - creating the empty archive)
arc  en -c  120 mb  59 sec
arc gen -c   81 mb  68 sec

with the directory written out in parts without compression ("-s64mb -dm0"):
arc     -c   69 mb  50 sec
arc gen -c   82 mb  82 sec
arc          99 mb  49 sec
arc gen     114 mb  75 sec

Old arc (before the memory optimization)
arc     -c  185 mb  46 sec  (of which 31 sec - the search itself, 8 sec - removing duplicates from the file list)


======= C:\Base\Compiler (62 thousand files + 5 thousand directories) ========================

uharc gs-   4.5 mb  6.5 sec  (of which 4.5 sec - in the OS)

7z   en      39 mb    6 sec  (of which 4.4 sec/28 mb - the search itself, of which 1.4 sec - in the OS)

rar          11 mb    4 sec  (of which 2.7 sec - the search itself)
rar  en      11 mb    7 sec
rar gen      11 mb   18 sec

ARC 16.12  (with UTF8Z + case-sensitive sortOn')
arc  en -c   20 mb   10 sec  (6.5+3.5gc)

ARC 14.12  (with PackedString)
arc     -c   18 mb  7.5 sec  (5+2.5gc) (of which 1.2 sec - removing duplicates from the file list)
arc  es -c   21 mb   10 sec  (6.3+3.8gc)
arc  en -c   34 mb   12 sec  (7+5gc)
arc gen -c   33 mb 13.3 sec  (8.3+5gc)

ARC 9.12 (with wfindfiles)
arc     -c   43 mb  8.8 sec  (5+3.8gc)
arc  en -c   58 mb   14 sec  (7.5+6.5gc)
arc gen -c   53 mb 15.5 sec  (9+6.5gc)

arc          48 mb  8.2 sec  (5.2+3gc)
arc  en      79 mb   13 sec  (7+6gc)
arc gen      77 mb   14 sec  (9+5gc)

ARC 18.05
arc          58 mb 18.5 sec  (15.5+3gc) (of which 8.5 sec - in the OS, 2 sec - removing duplicates from the file list)
arc  en      81 mb   24 sec  (19+5gc)
arc gen      74 mb   25 sec  (20+5gc)

```
