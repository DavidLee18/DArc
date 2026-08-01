# Testing DArc

Load this before changing a codec or writing a harness. The short version: the Rust layer has a real gate, the Haskell layer has none.

## What is and is not covered

**The Haskell/archiver layer has no automated test suite. The Rust layer has a
thorough one, and it is the gate.** Do not confuse the two — a change to
`Arc*.hs` is covered by nothing but your own round-trip.

## `rust/difftest` — 33 differential harnesses

Each `<codec>-check.sh` builds the C original **from a pinned revision**
(`DARC_C_REF_SHA` in `c-reference.sh`, currently `5c2c6ce`) alongside the Rust port
and requires identical bytes. The C is taken from git history rather than the
working tree so the oracle survives the C being deleted and cannot drift.

```bash
rust/difftest/lzma-decode-check.sh     # exit 0 or the port diverged
cd rust && cargo nextest run --profile ci   # 356 unit tests
```

Two properties every harness here is expected to have, learned the hard way:

* **It must be able to fail.** Sabotage the port and confirm it goes red. Four
  harnesses once passed while never invoking the ported code at all.
* **It must refuse to pass over an empty corpus.** A check that silently tests
  nothing reads as coverage.

`sevenz-check.sh` is the one deliberate exception to byte-identity: DArc never
*writes* `.7z`, so it compares behaviour (same entries, same extracted bytes, same
`SRes`) and is asymmetric — the port failing where the C succeeded is a failure,
the reverse is recorded, because the vendored SDK was compiled with PPMd and the
ARM64/ARMT filters switched off.

## The Haskell layer: `arc-cli-check.sh` and the GHC probe

`rust/difftest/arc-cli-check.sh <reference-arc> <port-arc>` is the acceptance
gate for the application layer. It runs 18 argv cases through two `arc`
binaries and compares five observables — archive bytes, exit code, stdout,
stderr, and the whole extracted tree. Only three things are normalised (timing
lines, progress redraws, the sandbox path); everything else, including the file
count and the ratio, is compared verbatim. It ends with a self-test that proves
the comparison can fail.

`./compile-ghc-probe` builds today's tree with GHC and `-threaded` into
`Tests/arc-ghc`. It is a probe, not a build path: the GHC build was deleted at
`af8dd3c`. It exists because the MicroHs build cannot answer whether real
Haskell parallelism is archive-visible — `compat-ghc/GHC/Conc.hs` makes
`setNumCapabilities` a no-op and MicroHs has no `-threaded`, so every
determinism measurement taken on `Tests/arc` was taken on a serial build.

Measured 2026-08-01, GHC 9.10.3:

- **Archives are byte-identical** between the two builds in all 15
  archive-producing cases, as are the extracted trees and the listings.
- The threaded binary is identical to itself across `+RTS -N1/-N2/-N8` ×
  `-mt1/-mt8` — six real-parallelism settings, one archive. Consistent with
  `ArcvProcessRead.hs:104`: `splitToSolidBlocks` is a *pure* function
  (`ArhiveFileList.hs:291`), so block boundaries are decided before any
  concurrency and nothing downstream can move them.

The 12 differences it *did* find are all MicroHs-only regressions in the
application layer, and each is a constraint on the port:

| what | why |
|---|---|
| `Compressed 8 files` where GHC says `226` | `ArcvProcessCompress.hs:106` — under `__MHS__` a data block is one `darc_compress_solid_block_w` call. It never emits `FileStart`, so `uiStartFile` never runs and only directory entries reach the counter. The **whole Haskell compression pipeline is bypassed**; the GHC branch runs `compressa`, a real multi-stage `de_compress_PROCESS` chain. |
| `uncaught exception: …` / exit 1, vs `ERROR: …` / exit 2 | `Arc.hs:75` guards `setUncaughtExceptionHandler handler` behind `#ifdef __GLASGOW_HASKELL__`, and the `compat-ghc` shim is `\_ -> return ()`. MicroHs prints its runtime's own message — leaking a `System/IO/Internal.hs` path — and exits 1 where the contract is 2. |
| one global callback slot | `CompressionLib.hs:333` — MicroHs has no `wrapper` FFI, so every `mkCALL_BACK` writes the same `IORef` instead of minting a `FunPtr`. Harmless while single-threaded; a port that restores concurrent codec invocation must not reproduce it. |

So `Tests/arc` is a sound *format* reference and a poor *behaviour* reference.
Gate archive bytes against it; gate the summary line and the error paths
against `Tests/arc-ghc`.

## End-to-end

```bash
./compile-O2
cd Tests
./arc a -r test.arc <some-directory>
./arc t test.arc
./arc x test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.
