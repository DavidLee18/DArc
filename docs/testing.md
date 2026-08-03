# Testing DArc

Load this before changing a codec or writing a harness. The short version: the Rust layer has a real gate, the Haskell layer has none.

## What is and is not covered

**The Haskell/archiver layer has no automated test suite. The Rust layer has a
thorough one, and it is the gate.** Do not confuse the two — a change to
`Arc*.hs` is covered by nothing but your own round-trip.

## `rust/difftest` — 53 differential harnesses

35 are codec-level and 18 are the CLI-level `arc-*-check.sh` described further
down. Each `<codec>-check.sh` builds the C original **from a pinned revision**
(`DARC_C_REF_SHA` in `c-reference.sh`, currently `5c2c6ce`) alongside the Rust port
and requires identical bytes. The C is taken from git history rather than the
working tree so the oracle survives the C being deleted and cannot drift.

```bash
rust/difftest/lzma-decode-check.sh     # exit 0 or the port diverged
cd rust && cargo nextest run --profile ci   # 648 unit tests
```

Two properties every harness here is expected to have, learned the hard way:

* **It must be able to fail.** Sabotage the port and confirm it goes red. Four
  harnesses once passed while never invoking the ported code at all.
* **It must refuse to pass over an empty corpus.** A check that silently tests
  nothing reads as coverage.

### Corpora and helpers are Rust; orchestration is shell

No harness invokes `python3`. Every one used to, for corpus generation and in
three cases for the case-building logic itself, and none of it was declared
anywhere — which is the whole reason this section exists. The replacements are
cargo binaries in `rust/darc-codecs/src/bin/`:

| binary | what it produces |
|---|---|
| `corpusgen <name> <outdir> [aux]` | 37 names: 33 corpus directories, plus `sine`/`prng`/`repeat`/`grzip-big` which write one stream to stdout. The optional third argument is auxiliary input a corpus cannot synthesise, such as a compiled object for `dispack` |
| `difftest-util <subcommand> …` | answers *about* bytes: `lzma2-blocks`, `genhex`, `all-zeros`, `elf-text`, `x86-bytes`, `bcj-manifest`, `srep-tie-order` |
| `lzma2-cases <workdir> <driver> <quick>` | the LZMA2 harness's content, streams and both manifests |
| `lzma-dec-cases <workdir> <encoder> <quick>` | the same for the LZMA decoder, including hand-built streams from a transcribed range coder |

The split is deliberate: a corpus or a pure function is easier to get right —
and to *keep* right — in a typed language, while the orchestration stays in
shell where the clang builds and the external binaries already live.

**Adding or changing a corpus.** `verify-corpus.sh <harness.sh> <corpus-name>`
extracts the `python3` heredoc that used to stand in that harness — from git
history if it is already gone — runs both, and `cmp`s every file. That is how
each conversion was accepted, and it caught three transcription errors that
review did not, including a `randbelow` that used `(n-1).bit_length()` where
CPython uses `n.bit_length()` and so diverged only on exact powers of two. It
is the one script here that still needs `python3`, for exactly this reason.

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

65 cases, one SHA-256 each, recorded from `Tests/arc-ghc` while all
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

`Tests/run-tests.sh` scores 24 passed / 0 failed / 0 skipped — the same as the
reference. The five methods that had no `Method` variant (`mm`, `tta`, `bsc`,
`lz4`, `zstd`) and the `-m` value grammar (`-mt`, `-ms`, `-md`) are implemented
and gated by the golden corpus.

The SKIP machinery in `run-tests.sh` and `sfx-roundtrip.sh` is kept even though
nothing trips it now. It matches on the binary's own "cannot write yet" wording,
so the next unimplemented method gets listed rather than silently failing — and
a case that fails for any *other* reason is still a failure.

Still refused rather than implemented: `-mm` (multimedia mode), `-ma`
(autodetect level) and `-mc` (disable an algorithm) each change the chain, so
they are rejected outright rather than ignored, on the same rule the HONOURED
option list follows. `-lc-`/`-ld-` are not accepted at all.

## End-to-end

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
D=rust/target/release/darc
$D a -r /tmp/test.arc <some-directory>
$D t /tmp/test.arc
$D x /tmp/test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.
