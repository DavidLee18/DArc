# CLAUDE.md

DArc ("Distended Arc") is a fork of FreeArc 0.67 — a solid-compression archiver.
Console binary `darc`.

**DArc is not a drop-in replacement, and compatibility is not a requirement.**
It is largely wire-compatible with [DArc86](https://github.com/YadeWira/DArc86)
and that is worth keeping when it is free — but where compatibility conflicts
with being correct, coherent, efficient, or resilient against corruption, **take
the better behaviour.** The owner's instruction, verbatim: *"If there's a better,
more coherent, or more efficient/effective/correct/corruption-resilient, take
it. I mean it."*

Two divergences already exist and are the model for the rest: encrypted archives
carry `:h1` in the encryption method because the old key/IV hex decoding was
broken and weakened the key (archives written without it are still read); and
`-mlzma:lc300` is refused rather than aborting the process the way the reference
does. Both are *marked* — a deliberate break is documented at the site and in
the commit, never made silently. See `docs/architecture.md`.

**The port is finished: the archiver is Rust, end to end.** Rust ~78,000 lines
(every codec, the crypto, the `.7z` reader, SREP, and the whole application
layer). **No C++ is on any shipped path any more**, and none is compiled into
any shipped binary: `Unarc/` — the extractor, the SFX modules, the GUI and the
FAR plugin, 8,008 lines — was deleted once `rust/darc-unarc` replaced it. The
extractor and every SFX module are now one cargo binary, which decides at
runtime whether an archive is appended to it.

18,445 lines of C/C++ remain and **nothing links them**: `Compression/` 11,935,
`rust/difftest`'s oracles 4,076, `rust/cryptref` 2,434. Only two of those files
are load-bearing — `Compression/Compression.h` and `Common.h`, which
`darc-codecs`/`darc-crypto` `build.rs` read through bindgen. The `.cpp` are
compiled by `./compile-c` (which now links into nothing) and, separately, by
the difftest harnesses out of a **pinned SHA in git history**
(`c-reference.sh`, `5c2c6ce`) rather than the working tree. No Haskell remains.

**The reference the port was gated against is not in the tree.** Every
`rust/difftest/arc-*-check.sh` compares two binaries and needs a Haskell build
passed in; `9a127e6` is the last commit that can produce one. It is **published
per platform and fetched by `rust/difftest/haskell-reference.sh`**, which is
what lets CI run all of them (`arc-harnesses`, plus `arc-sfx-check` in
`unarc-sfx`). The one that needs no reference is `arc-golden-check.sh`, which
replays **118 recorded cases** — 105 the reference's bytes, 1 a deliberate
divergence, 12 refusals. **Read `docs/testing.md` before changing anything that
touches archive bytes.**

**Every method the reference can write, this can write** -- `Tests/run-tests.sh`
round-trips all 24. Its FORMAT column is a different matter: it reports **21
DRIFT for the port and 0 for the reference**, and that is a harness defect, not
a port one. `arc.groups` was renamed to `darc.groups` in #129, `Tests/` holds
only the new name, so `darc` reads a grouping table there and `arc-ghc` does
not — the two are compared under different configurations, and
`Tests/fingerprints.txt` was blessed before the rename. Measured: the same
`darc` writes the baseline's bytes from a directory without `darc.groups` and
the "drift" bytes from one with it. The three cases that still agree —
`store`, `nonsolid`, `dict-nonsolid` — are exactly the ones where solid-block
grouping cannot matter. **Do not `--bless` this away.** `mm`, `tta`, `bsc`,
`lz4`, `zstd` and
`lzma2` had no `Method` variant until recently, which made archives using them
unreadable as well as unwritable; the `-m` VALUE grammar (`-mt`, `-ms`, `-md`,
`-ma`, `-mc`, `-mm`) was read as method NAMES. Both are fixed and gated, and
every `-m` knob now does what the reference does. `-lc-`/`-ld-` are accepted
too, and mean "no limit" — this file claimed otherwise until the docs were
audited; `-lc2m` and `-lc-` on `-mgrzip` over 300 KB give 300535 and 300241
bytes, so the dash form is honoured rather than tolerated.

**`-mt` is archive-visible through LZMA2 and nothing else.** Above one block
thread the encoder abandons the solid block and splits the input, so
`-mlzma2:d64k -mt1` and `-mlzma2:d64k -mt8` write different archives -- in the
reference too. Note that the *memory formulas* never see `-mt`: the C's
`compression_threads` global starts at 1 and `SetCompressionThreads` is deferred
to `setup_command`, which runs after every limit has been applied. Two different
numbers, same C function.

**`-lc` is applied THREE times, to three different things**, and missing any of
them writes a valid archive that is not the reference's:

1. at parse time, to the chain that gets **stored** in the block header;
2. to the **solid-block grouping**, because a shrunk block method moves where
   blocks end (`-mgrzip -lc2m` writes two data blocks where `-lc4m` writes one);
3. per block, *after* the dictionary is fitted, to produce the chain that
   actually **compresses** -- `real_compressor`, which `ArcvProcessRead.hs:134`
   passes separately from the stored one, and which sees a **different thread
   count** because `setup_command` has run by then.

The second pass is why an archive can store `grzip:1181kb` and be compressed
with `grzip:233016b`. Do not assume the stored method string describes what
compressed the bytes -- for a DATA_BLOCK under `-lc`, it frequently does not.
Control blocks are exempt: `writeControlBlock` passes its chain twice.

## Deeper references — load when the work calls for it

| | |
|---|---|
| `docs/architecture.md` | command dispatch, the archive format, UI, and the Haskell layer this was ported from |
| `docs/rust-workspace.md` | crates, the FFI boundary, lint gates, link order |
| `docs/testing.md` | `rust/difftest` and what a harness here has to prove |
| `THIRD-PARTY.md` | licences, and why the project is GPLv3 |

## Building

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
cargo build --release --manifest-path rust/Cargo.toml -p darc-unarc
```

That is the whole build. `unarc` doubles as the SFX module — `darc a -sfx<path>`
prepends it and it notices at runtime. `./compile-c` still compiles
`Compression/`, but nothing links the result; it is not needed to build or test
the archiver.

Windows is a target flag, not a script:
`cargo build --release --target x86_64-pc-windows-gnu …`, or
`aarch64-pc-windows-gnullvm` (needs llvm-mingw, not Debian's). Both targets are
in `rust-toolchain.toml`.

`cargo` is mandatory. `rust/difftest/sevenz-check.sh` needs a `7z` binary.
`--original` over HTTP is the default `url` feature; `--no-default-features`
drops it and its whole dependency subtree, the way `-DFREEARC_NOURL` used to.

**No harness needs `python3`.** Corpora come from `corpusgen`, pure functions
from `difftest-util`, and the two large case builders are `lzma2-cases` and
`lzma-dec-cases` — all cargo binaries under `rust/darc-codecs/src/bin/`. The
sole exception is `verify-corpus.sh`, which exists to check a conversion
against the Python it replaced and so cannot outlive it. See `docs/testing.md`
before adding or changing a corpus.

`Tests/` is a build *output* directory despite the name.

### Build gotchas

- **`compile-c` has no consumer left.** It generated `common.mak` for
  `Unarc/makefile`, and `Unarc/` is gone. It still compiles `Compression/` into
  `/tmp/out/`, and nothing links those objects; CI runs it only so that a
  C-side syntax break is still noticed. Deleting it and `Compression/**/*.cpp`
  is a real option — keep `Compression.h` and `Common.h`, which bindgen needs.
- **Object files are shared through `/tmp/out/`, and the makefiles do not
  rebuild when a `-D` changes.** `Compression/compile` is a hand-rolled loop,
  not a dependency graph, so it also misses header changes across directories.
  After editing `Compression/*.h` or switching defines, `rm -rf /tmp/out`. Stale
  objects here have produced phantom regressions more than once.
- An OS define and a byte-order define are both mandatory in the C —
  `Compression/Common.h` raises `#error` without them.

## What will bite you

- **`arc-golden-check.sh` detects change; it does not forbid it.** A change that
  compresses fine but writes different archive bytes passes every other build
  check, so this is the only thing that will tell you the bytes moved. Its
  manifest carries three kinds of case, in column 3:
  - **`ref`** (or absent — all 105 original lines) the reference's bytes. A move
    is a regression.
  - **`port`** the port deliberately writes different bytes. Column 4 records
    what the *reference* writes, so re-converging on it is caught too; column 5
    on is why. **Added by hand, never by `--record`.**
  - **`refuse`** the input must be rejected — non-zero exit under 128, no
    archive. A signal death is `crashed`, and is not a pass.
- **A docs-only change does not run CI.** `build.yml` has a `paths-ignore` for
  `**.md`, `docs/**` and `LICENSE`; the full matrix is ~25 minutes and none of
  those can change an archive byte. GitHub skips only when *every* changed path
  matches, so prose mixed with code still builds. `.github/**` is deliberately
  not in the list — a workflow edit is the thing that most needs proving.
  Consequence: such a commit has **no `Build CI` run at all**, which
  `watch-merge.sh` refuses to treat as green. Push docs straight to main rather
  than opening a PR for them.
- **Merge with `.github/watch-merge.sh <pr> <branch>`, not by eyeballing
  `gh pr checks`.** That command reports whatever check set exists *right now*,
  so straight after a push it answers with the previous commit's checks — or
  none. A poll loop that waits for "nothing pending" is satisfied before the new
  run starts and reads a stale all-green answer. That is how **#140 was merged
  with `arc-harnesses` failing on both of its commits**. The script pins on the
  PR's head SHA, refuses to answer until a run exists *for that SHA*, and
  refuses to merge a commit with no `Build CI` run at all.
- **The CLI harnesses run in CI now**, in the `arc-harnesses` job: it fetches
  the published oracle and runs every reference-dependent `arc-*-check.sh`.
  `arc-sfx-check` runs in `unarc-sfx` instead, because it needs the gitignored
  SFX modules that job already builds.
  Before it, `arc-golden-check.sh` was the only one CI ran — and
  `arc-cli-check.sh` had been failing **18 of 18 cases**, unnoticed, for an
  unknown length of time.
- **`arc-cli-check.sh` compares behaviour, not wording.** Every one of those 18
  failures was verbosity — banner, progress, timing, which stream a diagnostic
  lands on — while the archives were byte-identical. It now compares archive
  bytes, the extracted tree, the exit code, the listing *data*, and that a
  failure is reported at all; it does not compare message text. Two self-tests
  keep it honest: `-m1` vs `-m4` must produce different archives, and two
  archives with different contents must reduce to different data — otherwise a
  filter one line too greedy would leave it comparing `All OK` with `All OK`.
- **It also checks that each build's own summaries are TRUE**, against the disk
  and against its own listing — because not comparing them left exactly the hole
  #140 fixed: `Extracted 1 files` printed under the ERROR saying it had refused.
  This is per-binary, not differential. The **reference fails it**: it reports
  entries rather than files written, so `Extracted 226` on a tree of 218 files
  and 8 directories. That is printed as a NOTE and not gated — this harness
  gates the port, and no change here can fix the reference.
- **Do not regenerate `golden/manifest.txt` to make a red run go green.** That
  replaces the thing being checked with the thing doing the checking, and it is
  how an accident gets laundered into a baseline. `--record` regenerates only
  `ref` lines and carries the rest over verbatim; the header pins
  `# ref-cases: N` so reclassifying a case shows up in the diff.
- **The self-test is running it against `Tests/arc-ghc`**, which must FAIL —
  RECONVERGED, ACCEPTED and CRASHED on the divergent and refusal cases, while
  every `ref` case still passes.
- **A deliberate divergence must be marked.** At the site, in the commit, and in
  `CLAUDE.md` if it is user-visible. Silent divergence is the failure mode now,
  not divergence itself.
- **A compression method is a `String` that gets parsed, not an ADT**, on both
  sides of the FFI. `darc-arc`'s `Method` enum is the Rust half; a method it has
  no variant for is `Unsupported` and refused at write time rather than silently
  dropped.
- **Two directories look dead and are not**: `Compression/LZMA/Common` +
  `Windows` (included by `MultiThreading.h` and `CompressionLibrary.cpp`), and
  `Compression/Tornado/Tornado.cpp` (`C_Tornado.cpp` `#include`s it).
- **Link order matters to GNU ld and not to macOS ld**, so a broken link passes
  locally and fails every Linux and mingw job. See `docs/rust-workspace.md`.
- **CI enforces one lint gate no `cargo build` will show you**: every enum
  `match` must name its arms (`wildcard_enum_match_arm`), workspace-wide,
  tests included. `if let` and `let _` are allowed. See
  `docs/rust-workspace.md`.
- **`Unarc/` is deleted, and with it the only second reader.** `unarc.cpp` +
  `ArcStructure.h` + `CUI.h` were an independent implementation of the archive
  format, and it disagreed with the writer: the per-file time field was read as
  **4 bytes** where `ByteStream` writes a 64-bit `CTime`, so directory flags and
  CRCs came out of the wrong offset — directories became zero-byte *files* and
  every CRC failed, while a listing looked perfect because names and sizes are
  stored *before* the time. `rust/darc-unarc` owns no format knowledge: it reads
  argv and calls `darc-arc`. The Windows GUI SFX, the installer stub and the FAR
  plugin went with it; `make windows` had never been run by any CI job.
- **`unarc` IS the SFX module, on every platform.** No tiers, no separate stub,
  no `-DFREEARC_SFX` build: it calls `current_exe()` and extracts itself if an
  archive is appended. `arc-sfx-check.sh` prepends
  `rust/target/release/unarc`. Windows gets its module from the `windows-cross`
  job, which builds `-p darc-unarc` — **if that step is ever dropped, `darc
  a -sfx` has nothing to prepend on Windows and no other job will notice.**
- **`unarc-check.sh` no longer has a C side to differ against**, so it gates
  `unarc` against `darc`: same tree, same listing, same exit code, plus `-e` and
  `-d<path>`, which the C comparison never covered. Since both binaries live in
  `target/release` and the SFX story copies one of them around, it refuses to
  run when handed the same file twice — otherwise every comparison in it is
  trivially true.
- **There is no AddressSanitizer job any more.** It sanitized the C++ extractor,
  which was the last C binary on Linux; `-fsanitize=address` finds nothing to
  instrument in a Rust binary, so keeping it pointed anywhere would have been a
  green check proving nothing. If C ever returns to a shipped path, it comes
  back with it.

## Conventions

- Prefer adapting DArc's `Compression/C_*.cpp` wrapper over patching vendored
  sources. What is still vendored (libbsc) is kept close to pristine so it
  can be re-synced.
- Commit messages are `Component: what changed`, with bodies that explain *why* —
  including what was measured and what was ruled out.
- **The project is GPLv3-or-later**, so a new dependency carries a licence
  question. Check it against `THIRD-PARTY.md`.
- Comments are English throughout and sources are UTF-8. Before rewriting an old
  comment, read the encoding-damage history in `docs/architecture.md`: some files
  were corrupted by a bad conversion and restored from upstream.
