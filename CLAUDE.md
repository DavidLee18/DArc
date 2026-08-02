# CLAUDE.md

DArc ("Distended Arc") is a fork of FreeArc 0.67 — a solid-compression archiver.
Console binary `arc`, optional GTK binary `freearc`. Archive format is
wire-compatible with [DArc86](https://github.com/YadeWira/DArc86) **except for
encrypted archives**: those now carry `:h1` in the encryption method, because
the old key/IV hex decoding was broken and weakened the key. Archives written
without it are still read. See `docs/architecture.md`.

**It is mid-port from C to Rust, codec by codec, and the codec half is done.**
Rust ~53,000 lines (every codec, the crypto, the `.7z` reader, SREP);
Haskell ~20,300 (application logic, archive format, UI — untouched by the port);
C/C++ ~41,600, of which only ~2,600 is still a codec *engine* — `mmdet.cpp`,
`4x4`, and a Tornado remnant. Everything else is vendored Lua, `Unarc/`, the FFI
wrappers, the framework, and `rust/difftest`'s C oracles.

**The remaining work is the Haskell layer**, and it is the harder half: every
codec had a byte-exact C oracle to differential-test against, and the application
layer has none.

## Deeper references — load when the work calls for it

| | |
|---|---|
| `docs/architecture.md` | the Haskell layer: command dispatch, the process pipeline, archive format, UI, and the source-encoding history |
| `docs/rust-workspace.md` | crates, the FFI boundary, lint gates, link order |
| `docs/testing.md` | `rust/difftest` and what a harness here has to prove |
| `RUST_PORT_PROGRESS.md` | what has been ported, and how each was gated |
| `THIRD-PARTY.md` | licences, and why the project is GPLv3 |

## Building

```bash
./compile-O2          # console  -> Tests/arc
./compile-GUI-O2      # GTK      -> Tests/freearc
./compile-mhs-win64   # cross    -> Tests/arc-mhs-win64.exe
DARC_WIN_ARCH=aarch64 ./compile-mhs-win64   # needs llvm-mingw, not Debian's
```

**The compiler is MicroHs (`mhs`), which emits C. There is no GHC build.**
`cargo` is mandatory. `libcurl` is optional (absent adds `-DFREEARC_NOURL`).
`rust/difftest/sevenz-check.sh` needs a `7z` binary. CI pins MicroHs to a commit
SHA in `.github/workflows/build.yml` — match it when reproducing a CI failure
locally.

`Tests/` is a build *output* directory despite the name.

### Build gotchas

- **`common.mak` is generated, not committed.** `make` in a clean tree fails
  until a `compile` script has run once.
- **Object files are shared through `/tmp/out/`, and the makefiles do not
  rebuild when a `-D` changes.** `Compression/compile` is a hand-rolled loop,
  not a dependency graph, so it also misses header changes across directories.
  After editing `Compression/*.h` or switching defines, `rm -rf /tmp/out`. Stale
  objects here have produced phantom regressions more than once.
- An OS define and a byte-order define are both mandatory — `Utils.hs:31` raises
  `#error` without them. The full define table is in `docs/architecture.md`.

## What will bite you

- **Format compatibility is the highest-risk failure mode.** A change that
  compresses fine but writes archives older builds cannot read passes every
  build check. `ByteStream.hs` is format-breaking by default.
- **`__MHS__` is the define that bites most often.** MicroHs has real gaps versus
  GHC (FFI return-value truncation, missing `base` corners). Assume a library
  function is *absent* until proven otherwise; when an import resolves under GHC
  but not MicroHs, the fix belongs in `compat-ghc/`, not the calling module.
- **`Process.hs`'s hand-rolled `OurChan` is load-bearing.** MicroHs's `put_mvar`
  does not wake threads blocked in `readMVar`, so stock `readChan` deadlocks when
  the reader arrives first. Do not simplify it back to `Chan`.
- **A compression method is a `String` that C parses, not an ADT.** Adding a
  codec is mostly registering a name and a parameter grammar C-side.
- **Two directories look dead and are not**: `Compression/LZMA/Common` +
  `Windows` (included by `MultiThreading.h` and `CompressionLibrary.cpp`), and
  `Compression/Tornado/Tornado.cpp` (`C_Tornado.cpp` `#include`s it).
- **Link order matters to GNU ld and not to macOS ld**, so a broken link passes
  locally and fails every Linux and mingw job. See `docs/rust-workspace.md`.
- **CI enforces lint gates no `cargo build` will show you**, including a grep for
  `if let` anywhere under `rust/`. See `docs/rust-workspace.md`.
- `ArhiveDirectory.hs` imports only `debugLog` from `UI`, a deliberate
  cycle-break. Don't widen it.
- The module names really are spelled "Arhive".

## Conventions

- Haskell here predates AMP and modern `base`, and compiles with a long list of
  `-X` flags plus `-w`. Match the surrounding style rather than modernising — a
  cleanup assuming `Applicative f => Monad f` breaks the build.
- Prefer adapting DArc's `Compression/C_*.cpp` wrapper over patching vendored
  sources. What is still vendored (libbsc, Lua) is kept close to pristine so it
  can be re-synced.
- Commit messages are `Component: what changed`, with bodies that explain *why* —
  including what was measured and what was ruled out.
- **The project is GPLv3-or-later**, so a new dependency carries a licence
  question. Check it against `THIRD-PARTY.md`.
- Comments are English throughout and sources are UTF-8. Before rewriting an old
  comment, read the encoding-damage history in `docs/architecture.md`: some files
  were corrupted by a bad conversion and restored from upstream.
