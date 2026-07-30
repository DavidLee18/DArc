# darc-lzma — provenance and the gap to DArc's LZMA

Fork of **`lzma-sdk-rs` 0.2301.1** (BSD-3-Clause,
<https://github.com/danifunker/lzma-sdk-rs>), vendored at 3,011 lines of Rust.
`Cargo.toml.upstream` and `LICENSE.upstream` are the crate's own files, kept
verbatim so upstream diffs stay readable.

## Why vendored and not depended on

DArc's LZMA is **not stock LZMA**. `Compression/LZMA/readme` is titled *"List of
changes made"* and lists ten, several of which alter encoder output or the stream
layout:

| # | change | consequence |
|---|---|---|
| 1 | parameter interfaces **and how parameters are written into the compressed stream** replaced with `SetupProperties()` | stream property encoding differs from stock |
| 4 | range-coder buffer 1 MB → 64 KB | |
| 5 | decompression writes every 256 KB (`LZ_CHUNKS`, `_flushPos`) | |
| 6 | `BinTreeMain.h` hash size and allocation (`CalcHashSize`) | match finder geometry |
| 7 | `kDicLogSizeMaxCompress = 31`, `HASH_TABLE`, `hashSize` | |
| 8 | `compress_all_at_once` | |
| 9 | `maxDist[] = {0, 0, 128, 2048, 64<<10, 2<<20, 12<<20}` | **parse decisions** |
| 10 | `len + 1 >= lenMain ...` | **parse decisions** |

Items 9 and 10 change which matches the optimal parser picks, so DArc's encoder
emits different bytes than stock 7-Zip for the same input and parameters. A
crates.io dependency cannot express that; a fork can.

## What upstream already gives us

Genuinely strong, and verified the same way DArc verifies its own ports —
differentially against the C, not by round-tripping. From its ROADMAP, all
`[x] done & verified`:

* **L0** range coder, byte-exact including the empty-stream flush anchor
* **L1** `LzmaDec` port, verified against real C streams; full encoder symbol layer
* **L2** BT4 match finder, match lists byte-identical to `Bt4_MatchFinder_GetMatches`
* **L3** price models + `GetOptimum` DP + `Backward`, **byte-exact vs
  `LzmaEnc_MemEncode`**
* **L4** byte-exact across a corpus (empty/1/2-byte, zeros, 0xFF runs, text)

## The gap to what DArc needs

Ordered by how much each blocks a drop-in replacement:

1. **No streaming.** Upstream states plainly: *"In-memory buffer (full input; CHD
   data fits one cyclic buffer, no streaming)"*. DArc's LZMA is callback-driven —
   `CbIn_Read`/`CbOut_Write` in `C_LZMA.cpp` adapt DArc's `CALLBACK_FUNC` to the
   SDK's `ISeqInStream`/`ISeqOutStream`, and archives are far larger than memory.
   This is architectural, not a feature flag.
2. **One match finder of five.** Upstream has BT4. `C_LZMA.cpp`'s
   `kMatchFinderIDs` accepts **BT2, BT3, BT4, HC4, HT4**.
3. **DArc's ten divergences**, above. None of upstream's source mentions
   `maxDist`, `kDicLogSizeMaxCompress` or `lenMain`, so they cannot be applied as
   patches — they must be re-derived in upstream's idiom.
4. **Version skew.** Upstream ports SDK **23.01**; `Compression/7z/C_7z.c` names
   **26.00** for the reader, and the compression tree is a fork of an older SDK
   again. Encoder output can differ between SDK versions independently of DArc's
   own changes.
5. **LZMA2 and BCJ.** `C_LZMA2.cpp` (322 lines) and `C_BCJ.cpp` (67) have no
   upstream counterpart.

## Consequence for sequencing

This is not a patch job. It is a port on the scale of Tornado or BSC — each of
which took several PRs with a full differential oracle — on the codec that is both
DArc's **default method** and its **archive-catalog compressor**, i.e. the highest
blast radius in the repo.

The first step is therefore a **measurement, not code**: build a difftest that
compresses a corpus with DArc's pinned C LZMA and with this fork's stock encoder,
and report where they diverge. That says whether DArc's ten changes perturb every
stream or only some parameter combinations, which decides whether the fork is
tractable incrementally or needs the whole parser re-derived first.

Until that measurement exists, no estimate here is worth quoting.

## Status

Vendored, wired as a workspace member, builds, and upstream's own 17 tests pass
under DArc's toolchain (1.97.1) with `--features decode`. Nothing is wired into
the archiver; `Compression/LZMA` is untouched and remains the implementation in
use.
