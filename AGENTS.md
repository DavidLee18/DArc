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

Three more were added with `--autorun` (#154), all in that spirit:

* **`darc a -sfx<module>` chmods the result executable** on Unix. The reference
  does not, and on Windows never had to; here it produced a file that was a
  program in every respect but the bit the kernel checks. `chmod +x` semantics,
  not a fixed 0755, so a restrictive umask survives.
* **The autorun command lives in the ARCHIVE**, as a trailing footer field, not
  in the stub. FreeArc hardcoded `setup.exe` into a `-DFREEARC_INSTALLER` build;
  a second cargo output is what caused #149. The field is written **only when
  non-empty**, so it moves no bytes in any archive that does not use it — which
  is what keeps all 105 `ref` cases valid.
* **It asks before running, and propagates the exit code.** The reference ran
  the payload silently on the bare double-click and discarded the child's
  status. `-y` says yes in advance; EOF on stdin means no.

  "A DArc SFX never executes anything" *was* a documented property of this
  project, which is why changing it is marked here and not only at the site. It
  is now narrower rather than gone: **`darc l`, `darc t` and `darc x` are still
  inert on an SFX file**, and so is `unarc` given any explicit command. Only the
  stub, run as itself, with no command named, after a `y`, runs anything.

A sixth was forced by issue #165: **REP decodes a match whose source wraps the
circular buffer; the reference rejects it.** `rep`'s output buffer is cyclic, so
once the input passes the rep block size a match can reach back past the start
of the current cycle and its source lands *ahead* of the write position. The
v2.0.0 bounds check (`offset <= 0`) treated exactly that as corruption, so
**`darc t`/`darc x` failed with `rep failed: codec returned -7` on any archive
larger than its own rep block** — the default `-m4` is `rep:96m`, so two files
totalling 113 MB were enough. The archives were always *written* correctly;
v2.0.0 through v3.0.1 could not read them back. The C at `DARC_C_REF_SHA` and
the Haskell reference both still carry the broken bound, which is why
`rep-check.sh` must NOT be extended with a wrapping case — the oracle is wrong
there. Gated in `rust/darc-codecs/tests/rep.rs`.

A seventh came from issue #177: **`unarc` accepts BOTH `-dp<path>` and
`-d<path>` for the destination, in both of its roles.** The C had two option
loops behind `#ifdef FREEARC_SFX` — an SFX module read `-d` (`unarc.cpp:128`),
a plain `unarc` read `-dp` (`unarc.cpp:181`) — and neither binary knew the
other's spelling. This is **one** binary in both roles, and it had kept only the
SFX loop, so `unarc x -dpFolder` extracted into **`pFolder`**. `-dp` is now
matched before `-d`. The cost is the ambiguity the `#ifdef` used to hide: a
destination whose name really does begin with `p` must be written `-d./pFolder`,
because `-dpFolder` is now `-dp`. The alternative — picking the spelling from
the role we booted into — makes one executable answer to different flags
depending on how it was started, and breaks every caller that passes `-d` to a
plain `unarc`. `--noarcext` is accepted and ignored in the same commit, which is
what the C did too — `unarc.cpp:178` sets the flag and nothing reads it, since
appending `.arc` was an unimplemented TODO. Here it had been a usage error,
exiting 2. Gated in `rust/difftest/unarc-check.sh` and in `darc-unarc`'s own tests.

**The port is finished: the archiver is Rust, end to end.** Rust ~78,000 lines
(every codec, the crypto, the `.7z` reader, SREP, and the whole application
layer). **No C++ is on any shipped path any more**, and none is compiled into
any shipped binary: `Unarc/` — the extractor, the SFX modules, the GUI and the
FAR plugin, 8,008 lines — was deleted once `rust/darc-unarc` replaced it. The
extractor and every SFX module are now one cargo binary, which decides at
runtime whether an archive is appended to it.

**There is no C or C++ source in the tree.** Four headers remain and all four
live under `rust/`: `rust/include/{Compression,Common}.h`, the ABI contract
`bindgen` reads, and the two `wrapper.h` translation units that pull them in.
`Compression/` — the codec wrappers, the vendored engines, everything — is
deleted; the difftest harnesses read it from **two pins in git history**
(`DARC_C_REF_SHA` for the original C, `DARC_WRAPPER_REF_SHA` for the
forwarders). No Haskell remains either.

That is the rule the reference side always followed, now applied to both:
**a test does not justify keeping source in the tree when git already holds
it.** `libbsc` had worked this way for a long time already.

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
scores **24/0/0 with no format drift**, byte-identical to the reference.

That was not true until the groups file was passed explicitly, and the way it
failed is worth keeping: both binaries look for a grouping table beside their
own executable under **different names** — the reference wants `arc.groups`,
the port `darc.groups` (renamed in #129) — and `Tests/` holds only the new one.
So the port read a grouping table there and the reference did not, the suite
reported **21 of 24 as FORMAT DRIFT**, and `fingerprints.txt` had been blessed
before the rename. It was comparing two configurations, not two
implementations. `run-tests.sh` now passes `--groups=` to every fingerprinted
create and self-tests that the option reaches the archiver at all (grouping vs
`--groups-` must produce different bytes). The baseline is re-blessed **from
`Tests/arc-ghc`**, so it still records oracle bytes and the port is checked
against them, rather than the port being allowed to define its own answer.
`mm`, `tta`, `bsc`, `lz4`, `zstd` and
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

That is the whole build — there is no second step and no `make`. `unarc` doubles
as the SFX module: `darc a -sfx<path>` prepends it and it notices at runtime.
There is no C to build: `rust/difftest` fetches it from a pin when a harness
runs.

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

- **There is no `make` in this project any more.** `compile-c`, `compile-win64-c`,
  `unix-common.mak`, `win32-common.mak`, `Compression/compile` and all 22
  `Compression/*/makefile` are deleted, along with the `/tmp/out/` shared object
  directory and the stale-object hazard that came with it. Nothing linked what
  they produced once `Unarc/` went. The C that still matters is compiled by the
  difftest harnesses, with their own flags, from their own shims.
- An OS define and a byte-order define are both mandatory in the C —
  `rust/include/Common.h` raises `#error` without them. The harnesses and both
  `build.rs` pass them; an editor or clangd opening that header bare will show
  `"You must define OS!"`, which is expected, not a break.
- **`/.cargo/config.toml` is at the repo ROOT deliberately, not beside
  `rust/Cargo.toml`.** Cargo discovers its config by walking up from the
  **current working directory**, not from `--manifest-path`, and every workflow
  builds from the root — so `rust/.cargo/config.toml` would be silently ignored
  there while appearing to work when run from inside `rust/`. It carries the
  static-link flags for `aarch64-pc-windows-gnullvm`; **setting `RUSTFLAGS` in a
  workflow overrides it wholesale**, which is why neither workflow does any more.
- **The version lives once, in `[workspace.package]`**; every crate takes
  `version.workspace = true`. It is user-visible — `fetch.rs` sends it as the
  HTTP `User-Agent` — and it read `0.1.0` through v1, v2 and the whole port
  because nothing compared it to the tag. `release.yml` now refuses to build a
  tag that disagrees with it, before the matrix starts.

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
- **Two directories look dead and are not** — relevant when reading the *pinned*
  tree, since neither is in the working one: `Compression/LZMA/Common` +
  `Windows` (included by `MultiThreading.h` and `CompressionLibrary.cpp`), and
  `Compression/Tornado/Tornado.cpp` (`C_Tornado.cpp` `#include`s it). Both are
  still compiled, out of `DARC_WRAPPER_REF_SHA`.
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

- The C is frozen at two pins and nothing in the tree compiles it, so there is
  no wrapper here to adapt any more. Changing what a harness compiles means
  bumping `DARC_C_REF_SHA` or `DARC_WRAPPER_REF_SHA` — a deliberate act that
  belongs in a commit saying why.
- Commit messages are `Component: what changed`, with bodies that explain *why* —
  including what was measured and what was ruled out.
- **The project is GPLv3-or-later**, so a new dependency carries a licence
  question. Check it against `THIRD-PARTY.md`.
- Comments are English throughout and sources are UTF-8. Before rewriting an old
  comment, read the encoding-damage history in `docs/architecture.md`: some files
  were corrupted by a bad conversion and restored from upstream.
