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

## The archiver's oracle is gone; read this before touching archive bytes

Every `arc-*-check.sh` compares the port against a Haskell build. That build was
deleted with the rest of the Haskell layer, so those harnesses no longer run out
of the box: each takes a reference binary as `$1` and exits 2 without one.

`9a127e6` is the last commit that can produce one:

```bash
git worktree add /tmp/darc-ref 9a127e6
(cd /tmp/darc-ref && ./compile-ghc-probe)          # -> Tests/arc-ghc
bash rust/difftest/arc-filter-check.sh /tmp/darc-ref/Tests/arc-ghc
```

Note that `compile-ghc-probe` writes objects into the shared `/tmp/out/`, so run
`rm -rf /tmp/out` afterwards before building the current tree.

### What runs without a reference: `arc-golden-check.sh`

43 cases, one SHA-256 each, recorded from `Tests/arc-ghc` at `d6ebeb6` while all
19 harnesses were green. It is the only archive-format gate CI runs, and the
only one that will still be meaningful in a year.

Two rules:

- **Never re-record it from the port.** That swaps the thing being checked for
  the thing doing the checking. Build a reference from `9a127e6` and record
  from that; the manifest header says so too.
- **New cases must be machine-independent**, which the differential harnesses
  never had to be — they ran two binaries on one host, so anything host-varying
  cancelled. That means `--nodates`, explicitly parameterised chains rather than
  presets, `-rr` in absolute bytes, and **no `grzip` and no `4x4`**: those two
  are the only methods whose memory formulas read the processor count
  (`memlimit.rs:288`), so they are the only two that could bake the recording
  machine into a hash.

It earned its place on its first run by finding three divergences nothing else
had: filespec group ordering, the missing `isVeryFastCompressor` clause in the
sort-order decision, and `addBlockSizeCrit` having been ported and never called.

### What the port cannot do

`Tests/run-tests.sh` reports 17 passed / 7 skipped against the reference's 24
passed. The seven are configurations the port refuses outright — `mm`, `tta`,
`bsc`, `-ms`, `-mt1` — because `darc-arc`'s method table has no variant to emit,
not because they produce wrong bytes. They are matched on the binary's own
"cannot write yet" wording, so a case that starts failing for any other reason
is a failure, not a skip.

## End-to-end

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
D=rust/target/release/darc
$D a -r /tmp/test.arc <some-directory>
$D t /tmp/test.arc
$D x /tmp/test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.
