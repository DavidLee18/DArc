# darc-lzma — provenance and the measured gap to DArc's LZMA

Fork of **`lzma-sdk-rs` 0.2301.1** (BSD-3-Clause,
<https://github.com/danifunker/lzma-sdk-rs>), 3,011 lines of Rust.
`Cargo.toml.upstream` and `LICENSE.upstream` are the crate's own files, kept
verbatim so upstream diffs stay readable.

## The gap is far smaller than the readme suggests — measured

`rust/difftest/lzma-gap-check.sh` compresses a corpus with DArc's own
`lzma_compress` and with this crate's encoder, at eight parameter sets × 11
inputs = 88 comparisons. Result:

| input | first divergence | of | verdict |
|---|---|---|---|
| noise 40 KB | byte 40546 | 40550 (**99.99%**) | payload identical |
| text 31 KB | byte 107 | 111 (96.4%) | payload identical |
| zeros 40 KB | byte 66 | 70 (94.3%) | payload identical |

**Every divergence is in the last 4–6 bytes**, and the C output is consistently
+5..+7 bytes longer across all 88 comparisons — one end-of-payload marker. DArc
sets `props.writeEndMark = 1` ("FreeArc streams with EOPM"); this crate emits
none, and the marker also changes the range-coder flush, so the tail differs
rather than merely being appended.

**Conclusion: DArc's LZMA encoder produces the same parse as stock 7-Zip.** The
optimal parser does not need re-deriving.

## Why the readme misled me, and it misled me four times

`Compression/LZMA/readme` is titled "List of changes made" and lists ten
divergences, several of which would change parse decisions (`maxDist[]`,
`len + 1 >= lenMain`). I read it as describing the live encoder. It does not.

Seven of its eight identifiers — `BinTreeMain`, `CalcHashSize`,
`kDicLogSizeMaxCompress`, `maxDist`, `lenMain`, `LZ_CHUNKS`, `_flushPos` — live in
`Compression/LZMA/7zip/`, the **old C++ SDK**. `Compression/LZMA/makefile`
references `7zip/` **zero times**. It is not compiled. The live encoder is
`7z24/`, the modern C SDK, essentially stock.

So the readme documents a predecessor that is still in the tree but out of the
build. `7zip/` is 5,552 lines and a deletion candidate on the same footing as
LibTomCrypt was — but see `libtomcrypt-is-held-by-the-oracle` for why a makefile
grep is not sufficient proof of deadness.

## What is actually left to do

Ordered by what blocks a drop-in replacement:

1. **`writeEndMark`.** The only thing between this crate and byte-identity.
   Small, well-specified, and immediately verifiable: the measurement above should
   go to 88/88 identical.
2. **Streaming.** Upstream is in-memory by design ("full input, no streaming").
   DArc's LZMA is callback-driven — `CbIn_Read`/`CbOut_Write` adapt
   `CALLBACK_FUNC` to `ISeqInStream`/`ISeqOutStream` — and archives exceed memory.
   This is the largest remaining piece.
3. **Four match finders.** Upstream has BT4; `C_LZMA.cpp`'s `kMatchFinderIDs`
   accepts BT2, BT3, BT4, HC4, HT4.
4. **LZMA2 and BCJ.** `C_LZMA2.cpp` (322 lines) and `C_BCJ.cpp` (67) have no
   upstream counterpart.
5. **Multi-threaded match finder.** DArc sets
   `numThreads = GetCompressionThreads() > 1 ? 2 : 1`; the harness forces 1 to
   isolate that axis. Whether the mt path emits different bytes is unmeasured.

## Two harness traps worth keeping

* **`mc = 0` is DArc's "auto" sentinel**, resolved by the SDK
  (`LzmaEnc.c:99`: `mc = (16 + (fb >> 1)) >> (btMode ? 0 : 1)`). This crate takes
  `mc` literally, so 0 makes `cut_value` 0 and the BT4 tree walk underflows. Before
  that was fixed, **47 of 88 comparisons never ran** — a divergence rate computed
  from half a corpus.
* **BSD `cmp` says "char N", GNU says "byte N".** The offset parse assumed GNU, so
  the first runs printed `?` for the number that turned out to be the entire
  finding.

## Status

Vendored, wired as a workspace member, builds, upstream's 17 tests pass under
DArc's toolchain with `--features decode`. Nothing is wired into the archiver;
`Compression/LZMA` is untouched and remains the implementation in use.
