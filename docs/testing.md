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

## End-to-end

```bash
./compile-O2
cd Tests
./arc a -r test.arc <some-directory>
./arc t test.arc
./arc x test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.
