# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

DArc ("Distended Arc") is a fork of FreeArc 0.67 — a solid-compression archiver. The console binary is `arc` (Unix) / `Arc.exe` (Windows); the optional GUI binary is `freearc` / `FreeArc.exe`. Archive format is wire-compatible with [DArc86](https://github.com/YadeWira/DArc86).

The codebase is roughly half Haskell (application logic, archive format, UI) and half C/C++ (every compression codec, plus OS-level primitives). Most inherited comments are in Russian; new code and commit messages are in English.

## Building

**The compiler is MicroHs (`mhs`).** MicroHs is a lightweight Haskell compiler that emits C. There is no GHC build any more: `compile-ghc` and `compile-ghc-win64` were removed once the Haskell layer was slated for porting to Rust, and neither was built by CI or by the release workflow.

```bash
./compile-O2          # console binary  -> Tests/arc      (MicroHs, the default)
./compile-GUI-O2      # GUI binary      -> Tests/freearc
./compile-mhs-win64   # cross-compile   -> Tests/arc-mhs-win64.exe (mingw-w64)
make clean            # remove object files from all tempdirs

# Windows on ARM64. Same script, different toolchain: Debian's mingw-w64
# packages x86_64 and i686 only, so this target needs llvm-mingw (Clang) on
# PATH -> Tests/arc-mhs-win-arm64.exe
DARC_WIN_ARCH=aarch64 ./compile-mhs-win64
```

`DARC_WIN_ARCH` (`x86_64` by default, or `aarch64`) is read by both `compile-mhs-win64` and `compile-win64-c` and picks the cross toolchain, the target Windows version and the output name. Clang needs two flags GCC does not — see the `cc_quirks` block in `compile-mhs-win64` for what each is for.

Since Wine has no ARM64 emulation, the ARM64 binary cannot be exercised on the machine that cross-builds it. CI runs `Tests/win-test.sh` against it on a real `windows-11-arm` runner instead, and the release workflow's `publish` job waits on that result. `win-test.sh` detects whether it is on Windows or on a Unix host and adds Wine only in the latter case; it works in relative paths throughout, because MSYS rewrites POSIX-looking arguments before handing them to a native `.exe`.

Binaries land in `Tests/`, which despite the name is a build *output* directory, not a test suite — it holds the produced binaries (gitignored via `Tests/*arc`) alongside the committed `arc.groups` solid-ordering config.

Prerequisites: `mhs`, `clang`, `make`, `cargo`, `liblua5.1-dev`, `libncurses-dev`. **The Rust codecs are mandatory** — `cargo` is required. The `DARC_NO_RUST=1` opt-out was removed once the codecs it compared against started being deleted; byte-identity is now proved per codec by the harnesses in `rust/difftest`, which compare against a *pinned* revision of the C rather than the working tree. `libcurl` is optional and auto-detected — its absence adds `-DFREEARC_NOURL` and drops URL-archive support. CI pins MicroHs to a specific commit SHA with a checksum (`.github/workflows/build.yml`); match that commit when reproducing CI failures locally.

### Build-system gotchas

- **`common.mak` is generated, not committed.** The `compile` scripts do `cp unix-common.mak common.mak` (or `win32-common.mak`) before invoking `make`. Running `make` directly in a clean tree fails until you've run a `compile` script at least once.
- **Object files are shared across build paths via `/tmp/out/`.** `compile` uses `/tmp/out/FreeArc`; each Windows target gets its own directory. The makefiles do **not** rebuild when a `-D` changes, so switching between a stock and a `DARC_RUST=1` build without `rm -rf /tmp/out` links stale objects against the wrong libraries.
- **`Compression/compile` is a hand-rolled loop**, not a real dependency graph — it just `cd`s into each codec directory and runs `make`. It does not detect header changes across directories. After editing anything in `Compression/*.h`, wipe `/tmp/out/` rather than trusting an incremental build.
- The README's note about Git LFS is stale — no files are LFS-tracked any more, so `git lfs pull` is unnecessary.

### Conditional compilation

CPP defines are the main axis of variation, threaded through both the Haskell and C sides. Any nontrivial change touching platform behavior needs checking against several of these:

| Define | Meaning |
|---|---|
| `FREEARC_UNIX` / `FREEARC_WIN` | Target OS |
| `FREEARC_64BIT` | Set from `getconf LONG_BIT` |
| `__MHS__` | Building under MicroHs rather than GHC |
| `DARC_RUST` | Codec entry points come from the Rust crates. Always set — `Unarc/` too, since the `DARC_NO_RUST` opt-out and the `#ifndef DARC_RUST` C fallbacks are both gone |
| `FREEARC_GUI` | Build the GUI binary instead of console |
| `FREEARC_NOURL` | No libcurl/WinInet — URL support compiled out |
| `FREEARC_NO_LUA` | No Lua — `Options.hs` stubs the interpreter out |
| `FREEARC_INTEL_BYTE_ORDER` / `FREEARC_MOTOROLA_BYTE_ORDER` | Byte order |
| `FREEARC_PACKED_STRINGS` | `FileInfo` uses packed UTF-8 strings (`UTF8Z.hs`) |

An OS define and a byte-order define are both **mandatory** — `Utils.hs:31` raises `#error` without them.

`__MHS__` is the one that bites most often. MicroHs has real gaps versus GHC (FFI return-value truncation, missing `base` corners), and the codebase works around them in-place — see the `#ifdef __MHS__` blocks in `ArcvProcess*.hs`, `ByteStream.hs`, `CUI.hs`, and `Encryption.hs`. When adding code on the MicroHs path, assume a library function is *absent* until proven otherwise.

### The compat shims

`compat-ghc/` supplies GHC/`base` modules MicroHs lacks, injected via `-icompat-ghc`: `GHC.Conc` (`setNumCapabilities` etc., used at `Arc.hs:73–76`), `Data.Array.*` and `GHC.Base` (re-exports of `Mhs.Array`), `Data.Map.Strict`, `Foreign.Marshal.Pool` (used heavily by `ArhiveStructure`/`ArhiveDirectory`), `System.Process`, `System.Time`/`System.Locale` (the old-time API reimplemented over C `time.h`), and `System.Posix.*`. Note `System/Posix/Signals.hs` **fakes async signals** with a C volatile flag plus a polling thread, since MicroHs has no real signal delivery.

If an import resolves under GHC but not MicroHs, the fix usually belongs in `compat-ghc/`, not in the calling module.

`compat-oldtime/` holds `System/Time.hs` and `System/Locale.hs` — the removed `old-time`/`old-locale` API, reimplemented over C `time.h`. These are split out from `compat-ghc/` because **two different builds need them for different reasons**, and `-i` works at directory granularity:

| Build | Include flags | Why |
|---|---|---|
| `compile` (MicroHs, Linux) | `-icompat-ghc -icompat-oldtime` | MicroHs lacks both sets |
| `compile-mhs-win64` | `-icompat-ghc -icompat-oldtime` | same |

The split existed because the Wine GHC build had to see `compat-oldtime/` but *not* `compat-ghc/`, whose `System.Process`/`GHC.Conc`/`Data.Array` would have shadowed GHC's real modules. With the GHC builds gone, both remaining builds take both directories, so the split is now vestigial and the two could be folded together. Left alone deliberately: the Haskell layer is being ported to Rust, and both directories disappear with it.

## Testing

There is no automated test suite. Verify changes by round-tripping real archives with the built binary:

```bash
./compile-O2
cd Tests
./arc a -r test.arc <some-directory>
./arc t test.arc
./arc x test.arc -dp /tmp/extracted
```

Exercise the specific codec you touched via `-m` (e.g. `-m4x`, `-m9`), and test solid grouping (`-s`), encryption (`-p`), and recovery records (`rr`) when relevant — these are the paths most easily broken by changes to the compression pipeline. Format-compatibility regressions are the highest-risk failure mode in this repo: a change that compresses fine but produces archives older builds can't read will pass every build check.

## Architecture

### Entry point and command dispatch

`Arc.hs` is the whole top level and is short (~270 lines). `main` → `doMain` (`Arc.hs:71`) sets up the ^Break handler and UI, calls `parseCmdline` (`Cmdline.hs:35`) to turn `argv` into a *list* of `Command` records (argv is split on `;`), then `mapM_ run` over them. `run` (`Arc.hs:110`) is a flat `case` on the command letter routing to `runAdd`, `runCopy`, `runExtract`, `runList`, `runRecover`, `runDelete`, `runJoin`.

**The single most important structural fact: nearly every mutating command is the same operation.** `create`/`a`/`f`/`m`/`u` (`runAdd`), `j` (`runJoin`), `ch`/`c`/`k`/`rr`/`s` (`runCopy`), and `d` (`runDelete`) all converge on `runArchiveAdd` → `runArchiveCreate` (`ArcCreate.hs:51`). Deleting, adding, re-compressing, locking, and joining are all *"rewrite the archive with a different file-selection filter"* — see `setArcFilter` (`Arc.hs:243`), where delete is simply an inverted filter. Only extract/test/list (`ArcExtract.hs`) and recover (`ArcRecover.hs:301`) are genuinely separate paths. Adding a command usually means a case plus a filter, not a new pipeline.

`Command` (`Options.hs:39`) is a large record carrying both the parsed command and every relevant option (`opt_*` fields). It is threaded through essentially the entire codebase, and drivers pass modified copies downward (see `findArchives` at `Arc.hs:149`, which rewrites `cmd_arcname`/`opt_disk_basedir` per matched archive). Options are declared in `Options.hs` and parsed generically by `Cmdline.hs`. Learn this record early.

Two dispatch escapes sit ahead of the main table: `.7z` archives are detected by `is7zArchive` and diverted to `Arc7z.hs` (native read via the vendored 7-Zip SDK; *writing* shells out to the system `7zz`/`7z`), and under `FREEARC_GUI` an invocation with fewer than two arguments launches the file manager instead of running a command.

### The process pipeline (the core abstraction)

This is the part worth understanding before touching compression or extraction. `Process.hs` implements Hoare-style **communicating sequential processes**. These are real OS-level concurrent threads (`forkIO`), not lazy-list stages. Read the header comment block at `Process.hs:21–52` before editing anything in `ArcvProcess*`.

A `Pipe` carries a forward channel *and a backward channel*, used for flow control and **buffer recycling**: a producer does `sendP pipe (buf,len)` then `receive_backP pipe` to learn the buffer is free for reuse. Two combinators compose stages — `p1 |> p2` uses a one-element `MVar` (back-pressure), `p1 |>>> p2` uses an unbounded channel (read-ahead). Drivers are `runP` (synchronous), `runAsyncP` (returns a pushable `Pipe`), and `runFuncP`.

Archive creation is a two-stage pipeline (`ArcCreate.hs:165`):

```haskell
runP (read_files backdoor |>>> compress_AND_write backdoor)
```

- **`ArcvProcessRead.hs`** — `createArchiveAtructureAndReadFilesProcess` (`:58`). Designs the layout: decides solid-block boundaries, emits an `Instruction` stream, reads file data from disk (or copies existing blocks from input archives). Also owns `writeControlBlock`, reused by `ArcRecover`.
- **`ArcvProcessCompress.hs`** — `compressAndWriteToArchiveProcess` (`:39`). Note that it **builds a sub-pipeline dynamically, one thread per method in the `+`-chain** (`:206–210`): a `Compressor` of `[m1,m2,m3]` becomes `storingProcess |> de_compress_PROCESS m1 |> ... |> de_compress_PROCESS m3`.
- **`ArcvProcessExtract.hs`** — the mirror image, and the shared engine: `deCompressProcess` (`:163`) is parameterized by `freearcCompress` vs `freearcDecompress` and serves *both* directions, which is why `ArcvProcessCompress` imports this module. Driven from `ArcExtract.hs` and also from `ArcCreate.hs` (recompression, and the `-t` post-archive test).

Consequences that matter in practice: back-pressure and buffer sizing are load-bearing; errors must surface across thread boundaries (hence `Errors.hs`'s `registerError` machinery rather than plain exceptions); and **changing a stage's send/receive protocol deadlocks rather than type-errors**. `-mt` interacts with this layer.

One fork-specific landmine (`Process.hs:60–100`): `Control.Concurrent.Chan` is replaced by a hand-rolled `OurChan`, because **MicroHs's `put_mvar` does not wake threads blocked in `readMVar`**, so stock `readChan` deadlocks when the reader arrives first. `OurChan` uses `takeMVar` on holes instead. Do not "simplify" this back to `Chan`.

`ByteStream.hs` is the serialization layer underneath — encoding Haskell structures to byte streams with buffered read/write. It is directly responsible for on-disk format bytes, so **changes here are format-breaking by default**; the history contains at least one fix for `Int`/`CTime` fields not being written as fixed-width 64-bit.

### Archive format layer

Three modules, in increasing abstraction:

- **`ArhiveStructure.hs`** — the physical layout. An archive is a sequence of blocks: `BlockType` (`:313`) is `DESCR_BLOCK`, `HEADER_BLOCK`, `DATA_BLOCK`, `DIR_BLOCK`, `FOOTER_BLOCK`, `RECOVERY_BLOCK`. Every non-data block is followed by a *descriptor* that allows locating and reading it even when the archive is partially corrupt — this is what makes `arc r` possible. See `aSIGNATURE` (`:37`), `ArchiveBlock` (`:273`), `FooterBlock` (`:214`).
- **`ArhiveDirectory.hs`** — reads/writes the archive catalog (directory blocks) via `ByteStream`, and opens/closes archives. It imports only `debugLog` from `UI` — a deliberate cycle-break; don't widen that import.
- **`ArhiveFileList.hs`** — despite the name, this is the **diff/merge engine**, and the largest module in the repo (~46KB). It reconciles files on disk against files in the input archive(s), applies filters, decides per file whether it is *copied, re-compressed, or added fresh*, and assigns files to solid blocks and groups. It is also heavily memory-optimized (custom `HashTable`, `Foreign.Marshal.Pool`, packed strings, `unsafePerformIO`) — `Documentation/tests` records the historical benchmarks that drove those choices. Treat the representation as deliberate.

Note the inherited misspelling "Arhive" (not "Archive") in these module names.

### Compression layer and the FFI boundary

All actual compression is C/C++. The Haskell side never implements a codec.

**Compression configuration is a string that C parses, not a Haskell data type.** This surprises everyone. `type Method = String` (`CompressionLib.hs:370`); a method is a spec like `lzma:96m:normal:bt4` or `tor:8m:c3`. `Compression.hs` layers on `type Compressor = [CompressionMethod]` — a `+`-joined chain such as `rep+delta+lzma` (`join_compressor`/`split_compressor`, `:569`/`:572`) — and `type UserCompressor = [(String, Compressor)]`, mapping data-type groups to compressors (`$text->m3t`, `$exe->m3x`, `$compressed->m0`).

Because parameters never become ADTs, algorithm *properties* are queried from C through one generic entry point, `compressionService` (`CompressionLib.hs:110`): `canonizeCompressionMethod` normalizes a spec, and `compressionIs "encryption?"` / `"VeryFast?"` / `"nosolid?"` / `"MemoryBarrierCompression?"` ask C to classify it (`Compression.hs:60–70`). Adding a codec is therefore mostly registering a name and parameter grammar C-side, not changing pipeline code. Three methods never reach C and are handled purely in Haskell: `storing`, `fake`, `crc` (`Compression.hs:39–41`, dispatched at `:268–275`).

`Compression/CompressionLib.hs` is the FFI wall:

```haskell
foreign import ccall safe "Compression.h Compress"
  c_compress :: CMethod -> FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> ...
```

Streaming is **callback-driven**: `type CALLBACK_FUNC = CString -> Ptr CChar -> CInt -> VoidPtr -> IO CInt` (`:332`). C drives the loop and calls back into Haskell with `"read"`/`"write"` requests. `CompressionLibrary.cpp` dispatches on the method name to the right `Compression/C_*.cpp` wrapper, each adapting a vendored codec (`LZMA/`, `BSC/`, `PPMD/`, `Tornado/`, …) or, increasingly, forwarding to `rust/darc-codecs`. `Compression/External/C_External.cpp` handles methods implemented by spawning external binaries (precomp, ecm, ppmonstr, srep).

> **MicroHs constraint worth knowing before you touch this.** GHC creates callbacks with `foreign import ccall "wrapper"`. MicroHs has no dynamic-wrapper FFI, so `CompressionLib.hs:334–360` substitutes a single `foreign export ccall darc_haskell_callback` plus a **global single-slot `IORef`** holding the current Haskell callback (the function pointer is fetched from C via `darc_get_haskell_callback_ptr` in `Environment.cpp`, to dodge a forward-declaration ordering bug in mhs-generated C). Single-slot means concurrent FFI compression calls are constrained on the MicroHs path — a real limit, not an implementation detail.

`Encryption.hs` / `EncryptionLib.hs` follow the identical pattern: **encryption is just another method in the chain** (`isEncryption = compressionIs "encryption?"`), with PBKDF2-HMAC and a Fortuna PRNG imported from `Compression.h` and OS entropy via `systemRandomData` in `Environment.cpp`.

`Encryption.hs` + `Compression/_Encryption/` follow the same shape for AES/Blowfish/Serpent/Twofish.

`Environment.cpp` (58KB, the largest C++ file) provides OS-level services to the Haskell side — file/console/memory primitives that differ across Windows and Unix.

### UI layer

`UI.hs:5–11` is the backend switch, and the *only* place the GUI/console choice is made:

```haskell
#ifdef FREEARC_GUI
module UI (module UI, module UIBase, module GUI) where
import GUI
#else
module UI (module UI, module UIBase, module CUI) where
import CUI
#endif
```

`UI` **re-exports its backend**, so every caller just writes `import UI` and gets `uiStartProgram`, `uiScanning`, `askPassword`, etc. regardless of build. `UIBase.hs` holds backend-independent state (progress counters, timers, terminal detection); `CUI.hs` is console, `GUI.hs` is GTK. Adding a UI operation means adding it to `UIBase` or to *both* backends.

The GUI build additionally pulls in the GTK Archive Manager — `FileManager.hs`, `FileManPanel.hs`, `FileManDialogs.hs`, `FileManDialogAdd.hs`, `FileManUtils.hs` — a two-pane browser layered on `ArhiveDirectory` and `ArcExtract`. `Arc.hs` imports `FileManager` only under `#ifdef FREEARC_GUI`, so a GUI build exercises considerably more code than the console build CI covers.

`Charsets.hs`, `UTF8Z.hs`, and `FilePath.hs` handle the encoding minefield — archives store filenames that must round-trip across Windows/Unix and across codepages. `-sc` and `--language` route through here.

### Foundation modules

`Utils.hs` (~50KB) is a large grab-bag of combinators used everywhere — check it before writing a helper. `Errors.hs` defines the error taxonomy and the `registerError` mechanism (errors are *registered* rather than thrown, so pipeline stages can report without unwinding). `Files.hs`/`FileInfo.hs`/`Win32Files.hs` abstract filesystem access.

### Suggested reading order

`Arc.hs:71` (`doMain`) → `Cmdline.hs:35` (`parseCmdline`) → `Arc.hs:110` (`run`) → `ArcCreate.hs:51` (`runArchiveCreate`) → `ArcCreate.hs:165` (the two-process pipeline). Read `Options.hs:39` (`Command`) and `Process.hs:21–52` early.

### A note on source encoding and comment history

Comments were originally Russian and are now English throughout; sources are UTF-8 (in practice ASCII). Some history you need before touching old comments:

**Commit `0e441ae` ("convert encoding to utf8", 2025-10-15) destroyed comments in five files** by decoding them as UTF-8 when they were CP1251, turning every non-ASCII byte into U+FFFD. It converted the rest correctly, so this was a partial failure, not a systemic split. Those 478 lines were restored from `01f6bc4` (the last pre-conversion revision) and translated.

`Compression/DisPack/C_DisPack.{cpp,h}` had the same damage (1,492 U+FFFD) but was **not** recoverable from this repository — DisPack arrived already corrupted at `b328824` ("Port DisPack codec from FreeArc 0.67", 2026-04-12), whose first revision here already has `fffd=1134`. It was restored from the upstream release instead:

```
FreeArc-0.67-alpha-sources.tar.bz2   sha1 d79f57e48f31b57674c26b4d8b12b7f5ccd7f159
```

from the Wayback Machine capture of freearc.org, checksum-matched against the checksum published with the release; `M-Gonzalo/FreeArc`, `mirror/freearc` and `j2969719/freearc-old` on GitHub hold byte-identical copies. If you need other 0.67-era originals, that tarball is the source of truth — the site itself has been down since ~2016.

When restoring comments from an old revision, note two traps that have already caused bugs here:

- **Match lines order-aware, never by text similarity alone.** An earlier attempt matched on each line's ASCII skeleton. For a standalone Russian comment that skeleton is just `--`, so the matcher compared empty against empty and stamped one arbitrary comment across 69 lines. Use `difflib.SequenceMatcher` over the whole line sequence: map equal runs 1:1, and inside replace regions match forward only so a line can never pair with an earlier one.
- **Counting Cyrillic after an `errors='replace'` decode silently reports zero.** CP1251 bytes become U+FFFD, not Cyrillic, so a "0 Russian chars" result may just mean you decoded with the wrong codec. Check `utf-8` decodability separately from character counts.

Historical trap: revisions between `7778f77` (2025-10-15) and `c379c8b` (2026-04-10) store `.hs` files as **Git LFS pointer stubs**, so `git show` in that range returns a 3-line pointer rather than source. Reach past it to `01f6bc4`. LFS is no longer used.

Separately, an earlier tool **truncated comments at a `--` appearing inside the comment text** (e.g. `режим "--sync"` became `mode "`). Six such lines in `ArhiveFileList.hs` were restored; a repo-wide sweep found no others, but the pattern is worth recognising.

## Related components

These build separately from the main binary and are not covered by `./compile-O2`:

- **`Unarc/`** — standalone extractor and the SFX modules embedded into self-extracting archives (`arc.linux.sfx`, `arc.sfx`, `freearc.sfx`, …). Built with `cd Unarc && make linux` (or `make windows`); also produces `FreeArc.fmt`, a FAR Manager plugin. It is a **`DARC_RUST` build** and links `rust/target/release/libdarc_codecs.a` (built with the `dropin` feature, and placed *after* the objects — GNU ld resolves an archive only against undefineds it has already seen). Most codecs it needs no longer have a C decoder, so restoring the C to feed it was never an option.

  **It builds but it does not work, and this branch does not claim otherwise.** Reading the archive structure is fine — `unarc l` lists names, sizes and the compressed total correctly — but extraction fails on every target tried:

  | Symptom | Where |
  |---|---|
  | `unarc x` hangs indefinitely on every method, including `-m0` | Linux x86-64 (Debian bookworm, clang) |
  | SIGBUS in `___chkstk_darwin` — a 512 KB pthread stack overflowing in `MultiDecompress`'s worker | macOS arm64 |
  | Directory entries are written as zero-byte *files*, so the next member cannot be created (`can't open file in/a.txt`) | both |

  `MultiDecompress` (`CompressionLibrary.cpp:259`) is the suspect for the first two: it starts one `CThread` per method in the chain and coordinates them with semaphores, and **nothing else in the tree calls it any more** — the archiver drives `Compress`/`Decompress` one method at a time through `Environment.cpp`'s own pipeline. So it has been dead code for as long as Unarc has been unbuilt. Raising the thread stack to 8 MB in `Compression/LZMA/Windows/Thread.h` stops the macOS crash and then makes previously-correct single-file extractions fail their CRC, which is behaviour changing with stack *layout* — undefined behaviour that AddressSanitizer does not flag. Do not treat that stack bump as a fix.
- **`srep/`** — SREP 3.93a, a huge-dictionary LZ77 preprocessor, invoked as an external compressor. Vendored repackage of Bulat Ziganshin's original; sources also mirrored under `Compression/SREP/`. Its `srep/Compression/*.h` are an older, diverged vintage of the root `Compression/` headers (19–62% similar) — they are not interchangeable, so fix them independently.
- **`HsLua/`** — vendored Lua 5.1 plus Haskell bindings, used by `Options.hs` for `arc.*.lua` config scripts. `./compile` builds the vendored Lua from `HsLua/src`; the Windows cross-build sets `FREEARC_NO_LUA` and links none of it.
- **`Installer/`** — NSIS installer scripts and packaging assets (Windows).

## Conventions

- Commit messages are plain English, imperative, occasionally prefixed with a gitmoji on merges. Recent history uses a `Component: what changed` shape (`Win64 build: add LZMA/7z/zstd SDK sources, fix link`).
- Codecs vendored from upstream projects (LZMA/7-Zip SDK, libbsc, Lua) are kept close to pristine so they can be re-synced. (zstd and LZ4 are no longer vendored: zstd comes from the `zstd-safe` crate, and LZ4 from `lz4_flex` plus DArc's own LZ4-HC port in `rust/darc-codecs/src/lz4hc.rs`.) Prefer adapting DArc's wrapper (`Compression/C_*.cpp`) over patching vendored sources.
- Haskell here predates AMP and modern `base`, and is compiled with a long list of `-X` flags (`NoMonomorphismRestriction`, `OverlappingInstances`, `NondecreasingIndentation`, …) plus `-w` to accept it. Match the surrounding style rather than modernizing — a "cleanup" that assumes `Applicative f => Monad f` will break the build.
