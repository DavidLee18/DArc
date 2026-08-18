# Testing DArc

Load this before changing a codec or writing a harness. The short version: everything is gated now, and the archive format is gated twice.

## What is and is not covered

**Both layers have a gate.** That was not true for most of the port's life —
this file used to open "the Rust layer has a real gate, the Haskell layer has
none", and the archiver layer was covered by nothing but your own round-trip.
It now has:

* `arc-golden-check.sh` — **118 recorded cases**, needs no reference, the only
  archive-format gate that runs unaided.
* all 20 `arc-*-check.sh` in CI, against the published oracle: the
  `arc-harnesses` job runs 17, `unarc-sfx` runs `arc-sfx-check` (it needs the
  SFX module that job builds), and `arc-golden-check` has its own.
* `unarc-check.sh` — `unarc` against `darc`, over 9 archives: same exit code,
  same extracted tree, same listing, and the tree really is the input back, plus
  `-e` and `-d<path>`.

  It used to compare the Rust extractor against the C++ one in `Unarc/`. That
  gated the migration, passed 9/9, and then the C++ was deleted, so it cannot be
  run again — this is a **different property**, not a weakened version of the
  old one. `unarc` owns no format knowledge; it reads argv and calls `darc-arc`.
  So the only layer it can independently get wrong is option parsing and
  `Layout`, and that is what is now gated — including the `-e`/`-d` cases the
  C++ comparison never covered.

  It refuses to run if handed the same binary twice. Both live in
  `target/release` and the SFX story copies `unarc` around under other names, so
  without that guard every comparison in it is trivially true. That is not
  hypothetical: the previous revision, pointed at `Unarc/unarc` after `make
  linux` started writing the Rust binary there, would have compared the
  extractor with itself and reported `9 archives, 0 differing`.
* `arc-cli-check.sh` — 24 cases comparing behaviour, plus per-binary checks that
  each build's own summaries are true.
* `sfx-autorun-check.sh` — 19 checks on the installer SFX (`--autorun`). It has
  **no reference**: FreeArc's installer hardcoded `setup.exe`, ran silently, was
  Windows-only and discarded the exit code, so there is no case where both
  behaviours are defined. It is a property harness, and the properties that
  matter are the negative ones — 9 of the 19 pass when *nothing happened*.

  That shape is the danger. A payload that fails to run leaves no sentinel, and
  so does an SFX that never started; both read as "did not execute". So every
  negative check also asserts that the command it ran **worked** (`-x` extracted,
  `-l` listed, `-t` tested), and four self-tests at the end prove the sentinel
  can appear at all, that the file under test really is `[unarc][archive]`, that
  a well-formed `--autorun` is still accepted, and that the byte comparison can
  tell two archives apart. Falsified by deleting the `!c.explicit_cmd` guard in
  `darc-unarc`: 7 of 19 fail.

Until 2026-08-05, CI ran **`arc-golden-check` alone**. The other 18 existed and
nothing executed them, which is how `arc-cli-check` came to be failing 18 of 18
unnoticed. Wiring them in found two more harness bugs on the first two runs, both
portability rather than port defects.

## `rust/difftest` — 55 harnesses

35 are codec-level and 20 are the CLI-level `arc-*-check.sh` described further
down. All but one are DIFFERENTIAL, comparing two binaries; `arc-golden-check.sh`
is the exception and is described below. Each `<codec>-check.sh` builds the C original **from a pinned revision**
(`DARC_C_REF_SHA` in `c-reference.sh`, currently `5c2c6ce`) alongside the Rust port
and requires identical bytes. The C is taken from git history rather than the
working tree so the oracle survives the C being deleted and cannot drift.

**Both sides come from a pin, and they are DIFFERENT pins.** `Compression/` is
not in the working tree at all.

| constant | what it holds | role |
|---|---|---|
| `DARC_C_REF_SHA` (`5c2c6ce`) | the original C, engines intact | the oracle |
| `DARC_WRAPPER_REF_SHA` (`f0c859d`) | the same wrappers with their engines deleted — thin forwarders into `libdarc_codecs.a` / `libdarc_crypto.a` | the code under test |

They are not interchangeable, and swapping one for the other changes what is
being tested without failing: at `5c2c6ce` `C_LZP.cpp` is 257 lines longer,
`C_Encryption.cpp` 193 different, `C_Dict.cpp` 73. `darc_c_reference` and
`darc_wrapper_tree` in `c-reference.sh` extract them; both copy the **live**
shims in beside the extracted C, and that copy is the whole trick — a shim
`#include`s `"../../Compression/…"` by relative path, so putting it inside a
tree makes the include resolve to *that* tree. It is how one
`crypto_ccodec.cpp` can be both sides of a comparison.

**A shim with such an include must therefore be compiled from inside an
extracted tree, never from `$ROOT`.** `4x4-check.sh` passed
`$ROOT/rust/difftest/4x4_ref.cpp` and broke the moment `Compression/` left the
tree; it now uses `$tree/rust/difftest/4x4_ref.cpp`, so the same line serves
both sides. `mmdet_ref.cpp` is the variant to watch: it includes
`"Compression/MM/mmdet.cpp"` with no `../../`, resolved by `-I"$CREF"`, so it is
correct while that flag names a pinned tree and silently wrong if it ever names
`$ROOT`.

Two traps here have each produced a wrong "this is dead code" conclusion:

* **Those includes live inside `.cpp` files, not the shell scripts.** Grepping
  `rust/difftest/*.sh` finds `Common.cpp` and nothing else, understating the
  live set by an order of magnitude. Grep the `.cpp` shims for
  `../../Compression/` as well.
* **`Compression/BSC/libbsc/` is not in `HEAD`** — 38 files that exist only at
  `DARC_C_REF_SHA`. (It is not *gitignored*, as an earlier version of this file
  claimed; `Compression/BSC/.gitignore` covers only `/build`.) So `clang -MM`
  over the BSC shims fails silently against the working tree and their
  dependencies never appear in a computed reachability set. BSC has worked from
  the pin all along, which is the precedent the rest now follows.

`c-header-check.sh` guards the one thing that could not be pinned: `bindgen`
reads `rust/include/Compression.h` and `Common.h` at cargo-build time, and a
build script cannot `git archive` without breaking offline and vendored builds.
Those two must stay byte-identical to the pinned copies, or Rust and C disagree
about `CALLBACK_FUNC`, `MemSize` and the `FREEARC_ERRCODE_*` values, and the
symptom is a wrong number crossing the boundary rather than a compile error.

```bash
rust/difftest/lzma-decode-check.sh     # exit 0 or the port diverged
cd rust && cargo nextest run --profile ci   # 696 unit tests
```

Two properties every harness here is expected to have, learned the hard way:

* **It must be able to fail.** Sabotage the port and confirm it goes red. Four
  harnesses once passed while never invoking the ported code at all.
* **It must refuse to pass over an empty corpus.** A check that silently tests
  nothing reads as coverage.

### The one harness that is not differential: `debug-assert-check.sh`

It compares nothing. It builds the codecs **without `--release`** — the only
place `-C overflow-checks` is on — and fails if a panic reaches stderr. Scope is
Tornado, GRZip, BSC and lz4hc, the crate's densest arithmetic, at ~2850
invocations in ~13 minutes.

It exists because clippy can prove 2249 `as` casts in the workspace might
truncate. Most are lossless by construction (`(u & 0xFF) as u8`) and clippy
cannot carry the bound; a handful genuinely discard bits and are safe only
because the decoder discards the same bits — `bsc/qlfc_enc.rs`'s `rank_history`
feeds the probability model, not the payload, so a wrap costs ratio rather than
correctness. That is a weaker guarantee than it sounds, and `qlfc_enc.rs:55`
records the bug already paid for: `n_symbols` typed `u8` overflowed on a
full-alphabet block. Testing the casts empirically is cheaper and safer than
converting thousands of them.

**Its failure signal is stderr, never the exit status**, and this is not a
stylistic choice. Every codec entry point goes through `ffi::guard`, whose
`catch_unwind` turns a panic into `FREEARC_ERRCODE_GENERAL` — indistinguishable
from a codec declining an input. A deliberate `assert!(false)` in
`bsc::qlfc_enc::transform` exits **0**. A harness written as `if ! cmd; then
fail; fi` would report a clean sweep while every block panicked. With the stderr
grep, that same sabotage produces 630 named panics and exit 1.

### The boundary audit, which found three defects on its second pass

The other half of the cast problem is the casts the harness cannot reach by
running code: those on the **data path**, where no encoder/decoder symmetry
argument applies — archive-header fields, allocation sizes, FFI lengths. Clippy
flags 77 possibly-truncating casts in `darc-arc`, and every one was checked.

The first pass concluded "no defects" and was wrong about three of them. Each
site is one of:

* **Lossless by construction.** `u64 as usize` on the only supported targets
  (all 64-bit); `(hi * 16 + lo) as u8` where both are `to_digit(16)` nibbles;
  `orig as u32` in `fourx4.rs`, bounded because the block size upstream is
  already `u32`.
* **Guarded before the narrowing.** `bytestream.rs:244` is the model:
  `if n > remaining as u64 { return Err(ImplausibleLength) }` proves `n` fits a
  `usize` before casting. `codec_io.rs` rejects `size < 0` on entry, so every
  later `as c_int` round-trips a value that arrived as one.
* **A transliteration whose truncation is visible in the reference too.** Three
  of these read as obvious defects, and the reference reproduces all three — so
  the audit's first pass recorded them as conformant and left them.

  **That was the wrong call, and they are now fixed.** DArc is not a drop-in
  replacement (see `CLAUDE.md`): where conformance and correctness conflict, the
  better behaviour wins. What the reference does is evidence about *why* the
  code is shaped that way, not a reason to keep a defect.

  - `decompress.rs`'s LZMA2 thread count clamped to `u32::MAX` and cast to
    `i32`, so anything above `i32::MAX` wrapped negative and silently fell back
    to one block thread. Now clamped to `i32::MAX`. A clamp whose bound does not
    match the type it guards is not a clamp.
  - `lc`/`lp`/`pb` were parsed as `u32` and narrowed to `u8` late, so
    `-mlzma:lc300` became `lc44` and **aborted the process** on a
    hundreds-of-terabytes probability allocation (rc=134, in both builds), and
    `-mlzma:lc259` became `lc3` and wrote an archive whose header claimed
    `lc259`. Now bounded at parse time by `parse_int_max`, so both are refused
    cleanly and the narrowing downstream is lossless.

The lasting lesson is not "the reference is always right" but the opposite: the
reference settles *what the code does*, never *what it should do*. Measuring is
still mandatory — three sites that looked obvious were not what I assumed — but
the measurement is an input to the decision, not the decision.

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
gate for the application layer. It runs 24 cases through two `arc`
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

The argument may be relative — `bash rust/difftest/arc-test-check.sh
Tests/arc-ghc` works. It did not until `rust/difftest/arc-reference.sh` was
split out: nearly every harness runs its comparisons from inside the work
directory, so a relative `$1` passed the executable check and then resolved to
nothing after the first `cd`, and every case degraded to a SKIP. The
`checked -gt 0` guard caught it — `nothing was compared` — so no run was ever
silently green, but the message named the reference rather than the path.

`arc-reference.sh` is **sourced, not executed**, the way `c-reference.sh` is. It
holds the executable check, the "build one from `9a127e6`" message and the
resolution to an absolute path, and it replaced sixteen copies of that block —
fifteen of which wrote `as $1` unescaped inside a double-quoted string, so the
message that tells you how to pass the reference had an empty gap where `$1`
should be. Add it to a new harness with:

```bash
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
. "$ROOT/rust/difftest/arc-reference.sh"
```

### What runs without a reference: `arc-golden-check.sh`

118 cases. It is the only archive-format gate CI runs, and the only one that
will still be meaningful in a year.

Since compatibility stopped being the bar (`CLAUDE.md`), it holds three kinds,
tagged in column 3 of the manifest:

| kind | count | what it asserts |
|---|---|---|
| `ref` (or absent) | 105 | byte-identical to `9a127e6`. A move is a regression. |
| `port` | 1 | the port deliberately writes different bytes. Column 4 is what the *reference* writes. |
| `refuse` | 12 | the input must be rejected, or an extraction name neutralised. |

Recording the reference's hash **beside** ours on a `port` line is what makes a
silently reverted divergence detectable — otherwise the gate catches "it
changed" but not "it changed back". `refuse` distinguishes `crashed` (died on a
signal) from `refused`, because the reference *aborts* on `-mlzma:lc300` and
collapsing those two would have passed on the crashing build.

Five of the refusals are **extraction** cases, and they check the result rather
than the verdict: everything that lands must land inside the destination, under
a name that is not itself an escape. Refusing is only one acceptable answer —
`strip_root` *sanitises* a rooted name instead, so `\evil.txt` becomes
`evil.txt`, which is better than refusing because the data survives. An
expectation of "refused" would have failed the port for doing the right thing.

They are buildable end-to-end because `\` is an ordinary Unix filename
character: `..\..\evil.txt` is a legal file here, is archived under that exact
name, and becomes a path only when opened on Windows. On Unix that means the
case cannot demonstrate the escape itself, only that the dangerous name never
survives to disk — the escape is what `extract.rs`'s unit tests cover.

The self-test is running the whole thing against `Tests/arc-ghc`: it must fail,
with RECONVERGED on the `port` case, ACCEPTED on `lc259` and the dictionary
overflow, CRASHED on `lc300`/`lp300`, and UNSAFE NAME on all five extraction
cases — while all 105 `ref` cases pass. Ten failures, and the reference earns
every one of them.

Rules:

- **Never re-record `ref` lines from the port.** That swaps the thing being
  checked for the thing doing the checking. Build a reference from `9a127e6`
  and record from that; the manifest header says so too. `--record` regenerates
  only `ref` lines and carries `port`/`refuse` over verbatim, so one re-record
  cannot quietly delete every divergence.
- **`port` and `refuse` lines are written by hand**, with a reason, because only
  a person can say why something diverges. The header pins `# ref-cases: N`, so
  reclassifying a case shows up in the diff next to that reason. This is a
  review aid, not enforcement — nothing can enforce it when the port is the only
  thing that can produce the new bytes.
- **New cases must be machine-independent**, which the differential harnesses
  never had to be — they ran two binaries on one host, so anything host-varying
  cancelled. That means `--nodates`, explicitly parameterised chains rather than
  presets, and `-rr` in absolute bytes. `grzip`, `4x4` and `lzma2` are allowed
  only with an explicit `-mtN`: their output depends on the thread count, so
  pinning it is what keeps the recording machine out of the hash.
- **A limit that never binds tests nothing.** Half the cases now run on a ~12 MB
  tree, because `limitDictionary` fits every chain to the DATA size first — on
  the original 200 KB tree no `-lc`/`-ld`/`-md` figure was ever reached, and
  four format bugs lived behind that.

It earned its place on its first run by finding three divergences nothing else
had: filespec group ordering, the missing `isVeryFastCompressor` clause in the
sort-order decision, and `addBlockSizeCrit` having been ported and never called.

### What the port can and cannot do

`Tests/run-tests.sh` scores 24 passed / 0 failed / 0 skipped — the same as the
reference. The six methods that had no `Method` variant (`mm`, `tta`, `bsc`,
`lz4`, `zstd`, `lzma2`) and the `-m` value grammar (`-mt`, `-ms`, `-md`) are
implemented and gated by the golden corpus.

`lzma2` was the last of them, and it is the one to copy if another turns up. It
was gated on 23 method strings byte-identical to the reference, on `-mt1` versus
`-mt8` — which write *different* archives on both sides, so the pair proves the
thread count is really plumbed rather than merely accepted — and on all four
cross-extractions per case, since a missing variant makes an archive unreadable
as well as unwritable. That last direction is the half a write-side matrix
cannot see.

The SKIP machinery in `run-tests.sh` and `sfx-roundtrip.sh` is kept even though
nothing trips it now. It matches on the binary's own "cannot write yet" wording,
so the next unimplemented method gets listed rather than silently failing — and
a case that fails for any *other* reason is still a failure.

Every `-m` knob is implemented: `-mm`, `-mc`, `-md`, `-ms`, `-mt` and `-ma`.
`-lc-`/`-ld-` are still not accepted at all.

`-ma` is the one worth reading the harness for. It selects between the TWO
paths of `splitFileTypes`, and the port had implemented neither clause of
`quick_and_dirty` -- it used `chains.len() > 1`, which agreed everywhere
reachable only because the cases that separate them need `-ma`, and `-ma` was
refused. On the quick path the blocks are ordered by their CHAIN STRING
(`merge_by_type`), not by type index and not by first appearance; both of those
were tried and both wrote valid archives that were not the reference's.

`-lc` is served for every method, and getting there is the cautionary tale in
this file.

Every method has a `GetCompressionMem`/`SetCompressionMem`, checked by reading
the stored method string back out of the archive rather than by trusting the
arithmetic — `-mgrzip -lc4m` stores `grzip:466033b` on both sides, and 466033 is
4194304/9 exactly. **All of those agreed, and the archives still differed.**

Agreeing on the stored method string is not evidence that the same bytes were
written, because under `-lc` the stored chain is *not* the chain that
compresses. `-lc` is applied three times: at parse time to what is stored, to
the solid-block grouping (a shrunk block method moves where blocks end), and per
block after the dictionary is fitted to produce `real_compressor`
(`ArcvProcessRead.hs:134`), which also sees a different thread count because
`setup_command` has run by then. The port implemented only the first, so it
stored the right string and compressed with the wrong chain.

Two habits earned here:

* **A canonicalisation check would have called this green.** So would any
  harness comparing method strings. Only comparing archive bytes caught it.
* **When the artefacts differ and every intermediate agrees, ask the reference
  what it did rather than modelling it.** Two threshold models were fitted and
  both were wrong — one fitted Tornado and mispredicted GRZip, the other the
  reverse, and a `tempfile` mechanism was blamed that turned out to have no
  effect on the bytes at all. `-di'$'` makes the reference print
  `"Using " ++ real_compressor` per block (`ArcvProcessRead.hs:170`), which
  showed both the real chain and the block split in one run and ended the
  guessing immediately.

## End-to-end

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
D=rust/target/release/darc
$D a -r /tmp/test.arc <some-directory>
$D t /tmp/test.arc
$D x /tmp/test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.

## Traps that have already cost time

Carried over from `RUST_PORT_PROGRESS.md` when that file was removed: the port
it tracked is finished, but these were paid for and every one is about testing.

**A green differential run may be testing nothing.** Hit at least four times
(Tornado tables, SREP block size, DisPack `detect()`, BSC coder segmentation).
For any codec with a flush/window/block/detection granularity, the corpus
**must straddle it** — otherwise the interesting path never runs. Prove it with
a sabotage that breaks *only* the inputs past the boundary.

**Look at the corrupt artefact before theorising about the cause.** Four rounds
went into memory-error hypotheses for the #78 bug -- overflow, uninitialised
heap, use-after-free, data race -- because the *previous* SREP bug was a heap
overflow that ASan found on run one. That precedent was anchoring: each round
reached for a sharper version of the same tool. Diffing a captured bad archive
against a good one took ten minutes and identified the mechanism outright. The
artefact constrains the explanation far more tightly than another sanitiser can.

**To localise an intermittent failure, isolate each half before theorising.**
The "flaky SREP test" was a real heap overflow. What found it: the compressor
alone was byte-identical over 120 runs; a *fixed* archive decompressed 400 times
without one failure; yet the interleaved loop failed ~1-6%. The failing archive
had the **same size but different bytes** and failed *deterministically* on
retry — which rules out the decompressor and points at heap corruption during
compression. Then let a sanitizer name the line instead of guessing: the compile
scripts honour `OPT`, so `OPT="-O1 -g -fsanitize=address -fno-omit-frame-pointer"
./srep/compile` found it on the first run. (A sanitizer build drops a `.dSYM`
bundle beside the binary; `Tests/srep` in `.gitignore` does not match it, and
`git add -A` will happily commit 964 KB of DWARF.)

**An uncaught sabotage is a claim about the test, not the port.** Tornado's
data-table sabotage first reported 0 differences because the corpus topped out
under 900 KB while the flush granularity is 8 MB.

**Read the constant; do not infer it.** Every constant, signature or index
packing inferred from surrounding shape has been wrong — `IMPOSSIBLE_LEN` is
`INT_MAX/2` not `1<<30`; GRZip's strong BWT is a different transform, not a
reinterpretation; BSC's `model_run_state` takes four arguments with two clamped,
not two. For bulk data (opcode tables, tuning constants) **generate** it: parse
the header or compile a program that prints every value.

**Round-tripping is not proof the codec works.** LZ4's encoder produced *no
compression at all* — `lz4_flex` wants ~110% of input as output buffer while
`LZ4_compressBound` gives ~100.4%, so every block was rejected as
`OutputTooSmall` before compressing and `C_LZ4.cpp` dutifully stored it raw. A
real archive went 70,996 → 485,324 bytes (~7×) while round-tripping perfectly.
**Compare the archive size against the stock build**, and assert output size in
tests.

**Verify the build succeeded before trusting any test.** A failed codec build
left the previous *stock* binary in place, and a test script then reported six
green "Rust decodes it" lines that were really the C reading its own archives.

**An absent log line is a diagnostic.** Twice a fix "didn't work" because the
message it should have printed never appeared — the code path had not run at
all. Do not read silence as success.

**Link order matters on GNU ld.** A staticlib placed before the objects that
reference it links on macOS and fails on Linux.

**A glob is not a survey — enumerate every consumer.** When the C reference
moved to the pinned tree, the harnesses were converted by globbing `*-check.sh`,
which silently skipped `rust/difftest/run.sh` (CI's Delta/Dict/LZP "Codec
differential test" + sabotage steps, named differently). Deleting `Delta.cpp`
then failed only that job: `no such file: Compression/Delta/Delta.cpp`. The same
shape cost three CI rounds in one session (`compile` vs `compile-mhs-win64`,
toolchain vs `LIBCLANG_PATH`, `*-check.sh` vs `run.sh`). When a file moves or is
deleted, list *every* consumer, not the ones matching the obvious pattern.

**`long` is 4 bytes on Windows and 8 on LP64.** This family has produced a dozen
bugs here. It is also why bindgen must be given an explicit `--target` when
cross-compiling: host-generated bindings would be quietly wrong about every
`long` in the ABI.

