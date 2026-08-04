# CLAUDE.md

DArc ("Distended Arc") is a fork of FreeArc 0.67 — a solid-compression archiver.
Console binary `darc`. Archive format is
wire-compatible with [DArc86](https://github.com/YadeWira/DArc86) **except for
encrypted archives**: those now carry `:h1` in the encryption method, because
the old key/IV hex decoding was broken and weakened the key. Archives written
without it are still read. See `docs/architecture.md`.

**The port is finished: the archiver is Rust, end to end.** Rust ~68,000 lines
(every codec, the crypto, the `.7z` reader, SREP, and the whole application
layer); C/C++ ~23,800, none of which the archiver runs — it is `Unarc/` and the
SFX modules, the codec sources they compile, and `rust/difftest`'s C oracles.
No Haskell remains.

**The reference the port was gated against no longer exists.** Every
`rust/difftest/arc-*-check.sh` compares two binaries and needs a Haskell build
passed in; `9a127e6` is the last commit that can produce one. What runs without
it is `arc-golden-check.sh`, which replays 65 recorded cases against the bytes
that reference actually wrote. **Read `docs/testing.md` before changing anything
that touches archive bytes.**

**Every method the reference can write, this can write** -- `Tests/run-tests.sh`
scores 24/0/0, the same as the reference. `mm`, `tta`, `bsc`, `lz4`, `zstd` and
`lzma2` had no `Method` variant until recently, which made archives using them
unreadable as well as unwritable; the `-m` VALUE grammar (`-mt`, `-ms`, `-md`,
`-ma`, `-mc`, `-mm`) was read as method NAMES. Both are fixed and gated. What
is still refused rather than implemented: `-mm`/`-ma`/`-mc` change the chain
and are rejected outright, and `-lc-`/`-ld-` are not accepted at all.

**`-mt` is archive-visible through LZMA2 and nothing else.** Above one block
thread the encoder abandons the solid block and splits the input, so
`-mlzma2:d64k -mt1` and `-mlzma2:d64k -mt8` write different archives -- in the
reference too. Note that the *memory formulas* never see `-mt`: the C's
`compression_threads` global starts at 1 and `SetCompressionThreads` is deferred
to `setup_command`, which runs after every limit has been applied. Two different
numbers, same C function.

**`-lc` is served for every method except top-level `tor` and `grzip`**, which
are refused. Their memory formulas are ported and produce exactly the
reference's method strings; what still differs is `genericLimitMemoryUsage`
splicing a `tempfile` stage into the chain, which changes how the data is fed to
a chunk-sensitive codec. LZMA is unaffected. `-m4`/`-m9` carry `tor`/`grzip` at
the top level and so are refused under `-lc` as well.

## Deeper references — load when the work calls for it

| | |
|---|---|
| `docs/architecture.md` | command dispatch, the archive format, UI, and the Haskell layer this was ported from |
| `docs/rust-workspace.md` | crates, the FFI boundary, lint gates, link order |
| `docs/testing.md` | `rust/difftest` and what a harness here has to prove |
| `RUST_PORT_PROGRESS.md` | what has been ported, and how each was gated |
| `THIRD-PARTY.md` | licences, and why the project is GPLv3 |

## Building

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
./compile-c           # the C side: common.mak + the codec objects
make -C Unarc linux   # unarc and the SFX modules (needs compile-c first)
```

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

- **`common.mak` is generated, not committed.** `make -C Unarc` in a clean tree
  fails until `./compile-c` has run once. That is now the only reason
  `compile-c` exists.
- **Object files are shared through `/tmp/out/`, and the makefiles do not
  rebuild when a `-D` changes.** `Compression/compile` is a hand-rolled loop,
  not a dependency graph, so it also misses header changes across directories.
  After editing `Compression/*.h` or switching defines, `rm -rf /tmp/out`. Stale
  objects here have produced phantom regressions more than once.
- An OS define and a byte-order define are both mandatory in the C —
  `Compression/Common.h` raises `#error` without them.

## What will bite you

- **Format compatibility is the highest-risk failure mode.** A change that
  compresses fine but writes archives older builds cannot read passes every
  build check. `arc-golden-check.sh` is what stands between you and that; it is
  also the only archive gate CI runs.
- **Do not regenerate `golden/manifest.txt` from the port.** That replaces the
  thing being checked with the thing doing the checking. Build a reference from
  `9a127e6` and re-record from that.
- **A compression method is a `String` that gets parsed, not an ADT**, on both
  sides of the FFI. `darc-arc`'s `Method` enum is the Rust half; a method it has
  no variant for is `Unsupported` and refused at write time rather than silently
  dropped.
- **Two directories look dead and are not**: `Compression/LZMA/Common` +
  `Windows` (included by `MultiThreading.h` and `CompressionLibrary.cpp`), and
  `Compression/Tornado/Tornado.cpp` (`C_Tornado.cpp` `#include`s it).
- **Link order matters to GNU ld and not to macOS ld**, so a broken link passes
  locally and fails every Linux and mingw job. See `docs/rust-workspace.md`.
- **CI enforces lint gates no `cargo build` will show you**, including greps for
  `if let` and `let _` anywhere under `rust/` — tests included. See
  `docs/rust-workspace.md`.
- **The C is no longer on the archiver's path at all.** It is reached only by
  `Unarc/`, the SFX modules and the difftest oracles, which is why the ASan job
  now builds `unarc` rather than the archiver.

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
