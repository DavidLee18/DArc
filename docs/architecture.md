# DArc architecture — the design the port inherited

Load this when changing command dispatch, the archive format, encryption, file selection, or the UI.

**Every `.hs` file this document names is gone.** The port is finished; there are zero Haskell sources in the tree. What is described here is the *design*, which the Rust implements, and the `Module.hs:line` references are to the **reference at `9a127e6`** — still buildable, and fetchable per platform (`rust/difftest/haskell-reference.sh`). They are kept deliberately: when the port and the reference disagree, this is the map to what the reference did and why.

Where a module has a direct Rust counterpart, it is named beside it. The rough correspondence:

| reference | port |
|---|---|
| `Arc.hs`, `Cmdline.hs`, `Options.hs` | `darc-arc/src/bin/darc.rs`, `options.rs` |
| `ArhiveStructure.hs` | `darc-arc/src/block.rs`, `writer.rs` |
| `ArhiveDirectory.hs` | `darc-arc/src/directory.rs` |
| `ArhiveFileList.hs` | `darc-arc/src/sort.rs`, `grouping.rs`, `filetype.rs` |
| `ByteStream.hs` | `darc-arc/src/bytestream.rs` |
| `Compression.hs`, `CompressionLib.hs` | `darc-arc/src/method.rs`, `memlimit.rs`, `canonize.rs` |
| `Encryption.hs` | `darc-arc/src/encryption.rs`, `darc-crypto` |
| `UI.hs`, `CUI.hs` | the reporting in `darc.rs` |
| the C codecs | `darc-codecs`, `darc-lzma` |

Two things that were true of the Haskell and are **not** true of the port: MicroHs is gone with it, so the constraints noted below as MicroHs-specific are history rather than rules; and compatibility with FreeArc/DArc86 is no longer a requirement — see `CLAUDE.md`.

## Entry point and command dispatch

`Arc.hs` is the whole top level and is short (~270 lines). `main` → `doMain` (`Arc.hs:71`) sets up the ^Break handler and UI, calls `parseCmdline` (`Cmdline.hs:35`) to turn `argv` into a *list* of `Command` records (argv is split on `;`), then `mapM_ run` over them. `run` (`Arc.hs:110`) is a flat `case` on the command letter routing to `runAdd`, `runCopy`, `runExtract`, `runList`, `runRecover`, `runDelete`, `runJoin`.

**The single most important structural fact: nearly every mutating command is the same operation.** `create`/`a`/`f`/`m`/`u` (`runAdd`), `j` (`runJoin`), `ch`/`c`/`k`/`rr`/`s` (`runCopy`), and `d` (`runDelete`) all converge on `runArchiveAdd` → `runArchiveCreate` (`ArcCreate.hs:51`). Deleting, adding, re-compressing, locking, and joining are all *"rewrite the archive with a different file-selection filter"* — see `setArcFilter` (`Arc.hs:243`), where delete is simply an inverted filter. Only extract/test/list (`ArcExtract.hs`) and recover (`ArcRecover.hs:301`) are genuinely separate paths. Adding a command usually means a case plus a filter, not a new pipeline.

`Command` (`Options.hs:39`) is a large record carrying both the parsed command and every relevant option (`opt_*` fields). It is threaded through essentially the entire codebase, and drivers pass modified copies downward (see `findArchives` at `Arc.hs:149`, which rewrites `cmd_arcname`/`opt_disk_basedir` per matched archive). Options are declared in `Options.hs` and parsed generically by `Cmdline.hs`. Learn this record early.

Two dispatch escapes sit ahead of the main table: `.7z` archives are detected by `is7zArchive` and diverted to `Arc7z.hs` (native read via the `darc-sevenz` crate, which replaced the vendored 7-Zip SDK; *writing* shells out to the system `7zz`/`7z`). (A second escape used to launch a GTK file manager when given fewer than two arguments; the GUI has been removed.)

## The process pipeline (the core abstraction)

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

One fork-specific landmine, now history: `Process.hs:60–100` replaced `Control.Concurrent.Chan` with a hand-rolled `OurChan`, because **MicroHs's `put_mvar` does not wake threads blocked in `readMVar`**, so stock `readChan` deadlocked when the reader arrived first. MicroHs went with the Haskell, so this constrains nothing today; it is recorded because a reader comparing against the reference will meet `OurChan` and wonder.

`ByteStream.hs` is the serialization layer underneath — encoding Haskell structures to byte streams with buffered read/write. It is directly responsible for on-disk format bytes, so **changes here are format-breaking by default**; the history contains at least one fix for `Int`/`CTime` fields not being written as fixed-width 64-bit.

## Archive format layer

Three modules, in increasing abstraction:

- **`ArhiveStructure.hs`** — the physical layout. An archive is a sequence of blocks: `BlockType` (`:313`) is `DESCR_BLOCK`, `HEADER_BLOCK`, `DATA_BLOCK`, `DIR_BLOCK`, `FOOTER_BLOCK`, `RECOVERY_BLOCK`. Every non-data block is followed by a *descriptor* that allows locating and reading it even when the archive is partially corrupt — this is what makes `arc r` possible. See `aSIGNATURE` (`:37`), `ArchiveBlock` (`:273`), `FooterBlock` (`:214`).
- **`ArhiveDirectory.hs`** — reads/writes the archive catalog (directory blocks) via `ByteStream`, and opens/closes archives. It imports only `debugLog` from `UI` — a deliberate cycle-break; don't widen that import.
- **`ArhiveFileList.hs`** — despite the name, this is the **diff/merge engine**, and the largest module in the repo (~46KB). It reconciles files on disk against files in the input archive(s), applies filters, decides per file whether it is *copied, re-compressed, or added fresh*, and assigns files to solid blocks and groups. It is also heavily memory-optimized (custom `HashTable`, `Foreign.Marshal.Pool`, packed strings, `unsafePerformIO`) — `Documentation/tests` records the historical benchmarks that drove those choices. Treat the representation as deliberate.

Note the inherited misspelling "Arhive" (not "Archive") in these module names.

## Compression layer and the FFI boundary

**All actual compression is Rust.** The Haskell side never implemented a codec either — it reached them over the C ABI, and the `Compression/C_*.cpp` files it called are thin forwarding wrappers with the C engines behind almost all of them deleted. The port calls the same Rust crates directly, so for the archiver that ABI is gone too; it survives for `Unarc/` and the SFX modules, which still cross it.

**Compression configuration is a string that C parses, not a Haskell data type.** This surprises everyone. `type Method = String` (`CompressionLib.hs:370`); a method is a spec like `lzma:96m:normal:bt4` or `tor:8m:c3`. `Compression.hs` layers on `type Compressor = [CompressionMethod]` — a `+`-joined chain such as `rep+delta+lzma` (`join_compressor`/`split_compressor`, `:569`/`:572`) — and `type UserCompressor = [(String, Compressor)]`, mapping data-type groups to compressors (`$text->m3t`, `$exe->m3x`, `$compressed->m0`).

Because parameters never become ADTs, algorithm *properties* are queried from C through one generic entry point, `compressionService` (`CompressionLib.hs:110`): `canonizeCompressionMethod` normalizes a spec, and `compressionIs "encryption?"` / `"VeryFast?"` / `"nosolid?"` / `"MemoryBarrierCompression?"` ask C to classify it (`Compression.hs:60–70`). Adding a codec is therefore mostly registering a name and parameter grammar C-side, not changing pipeline code. Three methods never reach C and are handled purely in Haskell: `storing`, `fake`, `crc` (`Compression.hs:39–41`, dispatched at `:268–275`).

`Compression/CompressionLib.hs` is the FFI wall:

```haskell
foreign import ccall safe "Compression.h Compress"
  c_compress :: CMethod -> FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> ...
```

Streaming is **callback-driven**: `type CALLBACK_FUNC = CString -> Ptr CChar -> CInt -> VoidPtr -> IO CInt` (`:332`). C drives the loop and calls back into Haskell with `"read"`/`"write"` requests. `CompressionLibrary.cpp` dispatches on the method name to the right `Compression/C_*.cpp` wrapper, which forwards to `rust/darc-codecs` (or, for `-mlzma`/`-mlzma2`, to `rust/darc-lzma`). Only three directories under `Compression/` still hold a real C engine: `MM/mmdet.cpp` (the multimedia detector), `4x4/` (not ported, by decision), and `Tornado/Tornado.cpp` (a remnant still `#include`d by its wrapper). `Compression/External/C_External.cpp` handles methods implemented by spawning external binaries (precomp, ecm, ppmonstr, srep).

Two directories look dead and are **not** — check before deleting: `Compression/LZMA/Common` and `Windows` (a 7-Zip portability layer that `Compression/MultiThreading.h` and `CompressionLibrary.cpp` still include), and `Compression/Tornado/Tornado.cpp` (`C_Tornado.cpp` does `#include "Tornado.cpp"`).

> **A MicroHs constraint, now history — MicroHs went with the Haskell.** Recorded because it explains the shape of the reference's FFI, not because it binds anything here. GHC creates callbacks with `foreign import ccall "wrapper"`. MicroHs has no dynamic-wrapper FFI, so `CompressionLib.hs:334–360` substitutes a single `foreign export ccall darc_haskell_callback` plus a **global single-slot `IORef`** holding the current Haskell callback (the function pointer is fetched from C via `darc_get_haskell_callback_ptr` in `Environment.cpp`, to dodge a forward-declaration ordering bug in mhs-generated C). Single-slot means concurrent FFI compression calls are constrained on the MicroHs path — a real limit, not an implementation detail.

`Encryption.hs` / `EncryptionLib.hs` follow the identical pattern: **encryption is just another method in the chain** (`isEncryption = compressionIs "encryption?"`), with PBKDF2-HMAC and a Fortuna PRNG imported from `Compression.h` and OS entropy via `systemRandomData` in `Environment.cpp`. AES/Blowfish/Serpent/Twofish now come from `rust/darc-crypto`; `Compression/_Encryption/` is the wrapper that forwards to it.

> **The `:h1` parameter, and the archives that predate it.** `decode16` in `C_Encryption.cpp` decodes the encryption key and IV from the hex in the method string. It used to do that through `char2int` (`Common.h`), which is missing its `+10`: `'a'` decoded to `0` and `'f'` to `5`, folding the key's 16 hex values onto 10 and costing roughly 0.75 bits per nibble — about 208 bits of entropy in a nominally 256-bit AES key, with the IV folded the same way. It stayed invisible because the same function ran when writing and when reading, so every build agreed with itself.
>
> Archives now carry `:h1` in the encryption method, meaning "the key and IV are real hexadecimal", inserted by `addHexFix` (`Cmdline.hs`) immediately after the algorithm name. Archives *without* it are still read the old way, via `char2int_broken` — the default in `ENCRYPTION_METHOD`'s constructor is `hexfix = 0` precisely because that constructor is also what parses a method string read from an archive. `-ae aes:h0` writes the old format on purpose; the parameter goes after the name so a user's own `h` overrides it rather than the reverse.
>
> A build without a case for `'h'` hits `default: error=1` and refuses the whole method string, so an old binary meeting a new archive reports "invalid compression method or parameters" rather than a corrupt archive. **Encrypted archives are therefore not wire-compatible with DArc86 or with FreeArc 0.67**; unencrypted ones are unaffected.
>
> The trap for anyone changing this: the salt and the check code never went through the broken decoder — they are decoded in Haskell by `Utils.hs`'s `decode16`, which was always real hex. So a build that decodes all four fields correctly **verifies every password** and then fails every CRC. The check code cannot detect the mismatch.

> **PBKDF2 iterations, and why they cost what they cost.** The default moved from FreeArc's `1000` to **`210000`**, OWASP's recommendation for PBKDF2-HMAC-SHA512. Unlike `hexfix`, this default is safe to move: `ShowCompressionMethod` always writes `:n%d`, so every stored method names its own count and an old archive is decrypted with the 1000 it recorded. Nothing is retroactive — a weak archive stays weak until it is repacked.
>
> The cost is **per block, not per archive**, because `generateEncryption` draws a fresh salt for every block and therefore derives a fresh key. Measured on a 200-file tree: a normal archive has one data block (three under `-hp`), so creating it goes 89 ms → 131 ms and testing it 42 ms → 112 ms. But `-s-` makes every file its own solid block, and there the same tree goes **227 ms → 13.9 s**. That combination is the one to know about; `-ae aes:n1000` opts out, per archive.
>
> Making a high count cheap would mean deriving the salt once per archive and keeping the per-block IVs — the standard design, and a behaviour change beyond a default. It is not done here.

> **File selection is one predicate, reused.** `opt_file_filter` (`Cmdline.hs:493`) is built once from `-n`/`-x` and the size and time options; what varies per command is only whether the filespecs are ANDed in and whether the result is negated (`Arc.hs:243-272`) — the disk scan uses the filter alone, `ch`/`c`/`k`/`t`/`l` use filespecs AND filter, `d` uses its negation, and `a`/`u`/`f`/`j` set `cmd_archive_filter = const True` because for them the filespecs select *disk* files.
>
> Directories never go through the name filter, on either side: `test_dirs` (`Arc.hs:270`) when reading and `accept_f` (`FileInfo.hs:462`) when writing both decide them from `--dirs`/`--nodirs`, or failing that from whether any *n/s/t* filter exists. `-x` is not one of those (`nst_filters` lists `-n` and the size/time filters only), which is why `arc a -x*.dat` keeps a `sub` entry and `arc a -n*.txt` drops it. Reading those two outcomes as "directories are filtered by name" fits both and is wrong; `--dirs -n*.txt` is what separates the readings.
>
> `c`, `ch`, `k`, `s…` and `rr…` take **no filespecs** — `is_CMD_WITHOUT_ARGS` (`Options.hs:305`). `d` and `j` are the exceptions, and their arguments mean different things: archive members for `d`, archive names for `j`.
>
> The size and time filters are part of the same predicate, with comparisons that are not uniform: `-sm`/`-sl` are strict, `-ta`/`-tn` are inclusive, `-tb`/`-to` are exclusive. `-sm`/`-sl` take a `parseSize` argument (a bare number is **bytes**), `-ta`/`-tb` a positional `YYYYMMDDHHMMSS` in local time, and `-tn`/`-to` a period where a bare number means **days**. A note on reaching them: `-ta`, `-tb`, `-tn` and `-to` are all ambiguous with `--type`, which is in `aPREFFERED_OPTIONS` and wins — `arc a -ta20240101 …` is `--type=a20240101: only arc format is supported`, in the reference as much as in this port. Use the long spellings. `-sm`/`-sl` are themselves preferred and so survive their own clash with `-s`.
>
> **A directory named outright is a separate pass.** `find_filter_and_process_files` (`FileInfo.hs:403`) rewrites a filespec that names a directory into two — the directory itself, scanned non-recursively by the *addDir* pass, and `dir/`, scanned by the main walk — so `arc a x.arc work/data` stores an entry for `work/data` as well as its contents. The two passes share `accept_f` but must not share an answer: the addDir pass tests `include_dirs `defaultVal` True`, decided by `--dirs`/`--nodirs` alone and never by the *n/s/t* filters, which is why `arc a -n*.txt x.arc work/data` keeps `work/data` while dropping `work/data/sub`.
>
> Letting `--dirs` force *both* passes true was a bug on the Haskell side, fixed in `FileInfo.hs:462`. It made the addDir pass accept every *sibling* of the named directory, so `arc a --dirs x.arc .` wrote the top-level entry twice and `arc a --dirs x.arc work/data` also stored `work/other` — a directory the user never named, and the same leak the `recursive` guard beside it already fixed for `-r`. The two builds are byte-identical under `--dirs` again.

`Environment.cpp` (62KB, 1,917 lines — the largest C++ file left) provided OS-level services to the Haskell side: file/console/memory primitives that differ across Windows and Unix. It belonged with the Haskell layer rather than the codecs, which is why the archiver no longer reaches it at all — the port uses `std` for the same work. It survives because `Unarc/` and the SFX modules still compile against it.

## UI layer

`UI.hs` re-exports `CUI`, the console backend:

```haskell
module UI (module UI, module UIBase, module CUI) where
import CUI
```

Every caller writes `import UI` and gets `uiStartProgram`, `uiScanning`, `askPassword` and the rest. `UIBase.hs` holds backend-independent state (progress counters, timers, terminal detection); `CUI.hs` is the console implementation.

**There used to be a second backend.** `GUI.hs` and the GTK Archive Manager — `FileManager.hs`, `FileManPanel.hs`, `FileManDialogs.hs`, `FileManDialogAdd.hs`, `FileManUtils.hs`, about 3,500 lines — were selected by `#ifdef FREEARC_GUI`, which also made `Arc.hs` launch the file manager when invoked with fewer than two arguments. All of it was **removed**: CI never built it, it carried no format risk, and it was the only thing keeping `UI.hs` a switch rather than a re-export. `uiScanning` is now the no-op the console build always saw — its body only ever ran under the GUI.

`Charsets.hs`, `UTF8Z.hs`, and `FilePath.hs` handle the encoding minefield — archives store filenames that must round-trip across Windows/Unix and across codepages. `-sc` and `--language` route through here.

## Foundation modules

`Utils.hs` (~50KB) is a large grab-bag of combinators used everywhere — check it before writing a helper. `Errors.hs` defines the error taxonomy and the `registerError` mechanism (errors are *registered* rather than thrown, so pipeline stages can report without unwinding). `Files.hs`/`FileInfo.hs`/`Win32Files.hs` abstract filesystem access.

## Suggested reading order

**In the port**, which is what you will be editing: `darc-arc/src/bin/darc.rs` (`main`, and the command `match`) → `options.rs` (the argv grammar) → `method.rs` (`Method`, and the `-m` parse) → `memlimit.rs` (the three limits, and why `-lc` is applied three times) → `writer.rs` and `block.rs` (the bytes).

**In the reference**, when you need to know what it did: `Arc.hs:71` (`doMain`) → `Cmdline.hs:35` (`parseCmdline`) → `Arc.hs:110` (`run`) → `ArcCreate.hs:51` (`runArchiveCreate`) → `ArcCreate.hs:165` (the two-process pipeline). Read `Options.hs:39` (`Command`) and `Process.hs:21–52` early. Build it per `rust/difftest/haskell-reference.sh`.

## A note on source encoding and comment history

Comments were originally Russian and are now English throughout; sources are UTF-8 (in practice ASCII). Some history you need before touching old comments:

**Commit `0e441ae` ("convert encoding to utf8", 2025-10-15) destroyed comments in five files** by decoding them as UTF-8 when they were CP1251, turning every non-ASCII byte into U+FFFD. It converted the rest correctly, so this was a partial failure, not a systemic split. Those 478 lines were restored from `01f6bc4` (the last pre-conversion revision) and translated.

`Compression/DisPack/C_DisPack.{cpp,h}` had the same damage (1,492 U+FFFD) but was **not** recoverable from this repository — DisPack arrived already corrupted at `b328824` ("Port DisPack codec from FreeArc 0.67", 2026-04-12), whose first revision here already has `fffd=1134`. It was restored from the upstream release instead:

```
FreeArc-0.67-alpha-sources.tar.bz2   sha1 d79f57e48f31b57674c26b4d8b12b7f5ccd7f159
```

from the Wayback Machine capture of freearc.org, checksum-matched against the checksum published with the release; `M-Gonzalo/FreeArc`, `mirror/freearc` and `j2969719/freearc-old` on GitHub hold byte-identical copies. If you need other 0.67-era originals, that tarball is the source of truth — the site itself has been down since ~2016.

When restoring comments from an old revision, note two traps that have already caused bugs here:

- **Match lines order-aware, never by text similarity alone.** An earlier attempt matched on each line's ASCII skeleton. For a standalone Russian comment that skeleton is just `--`, so the matcher compared empty against empty and stamped one arbitrary comment across 69 lines. Use a longest-common-subsequence diff over the whole line sequence: map equal runs 1:1, and inside replace regions match forward only so a line can never pair with an earlier one. (The original note named `difflib.SequenceMatcher`; the repo does not use Python, so treat that as naming the algorithm, not the tool.)
- **Counting Cyrillic after an `errors='replace'` decode silently reports zero.** CP1251 bytes become U+FFFD, not Cyrillic, so a "0 Russian chars" result may just mean you decoded with the wrong codec. Check `utf-8` decodability separately from character counts.

Historical trap: revisions between `7778f77` (2025-10-15) and `c379c8b` (2026-04-10) store `.hs` files as **Git LFS pointer stubs**, so `git show` in that range returns a 3-line pointer rather than source. Reach past it to `01f6bc4`. LFS is no longer used.

Separately, an earlier tool **truncated comments at a `--` appearing inside the comment text** (e.g. `режим "--sync"` became `mode "`). Six such lines in `ArhiveFileList.hs` were restored; a repo-wide sweep found no others, but the pattern is worth recognising.

## Related components

These build separately from the main binary and are not covered by `cargo build` (see `CLAUDE.md` for the build commands; `./compile-O2` was the Haskell-era script and is gone):

- **`Unarc/`** — standalone extractor and the SFX modules embedded into self-extracting archives (`arc.linux.sfx`, `arc.sfx`, `freearc.sfx`, …). Built with `cd Unarc && make linux` (or `make windows`); also produces `FreeArc.fmt`, a FAR Manager plugin. It is a **`DARC_RUST` build** and links `rust/target/release/libdarc_codecs.a` (built with the `dropin` feature, and placed *after* the objects — GNU ld resolves an archive only against undefineds it has already seen). Most codecs it needs no longer have a C decoder, so restoring the C to feed it was never an option.

  **Unarc is a second, independent implementation of the archive READER, and that is why it is still C++.** Its codecs are already Rust — the makefile builds `-DDARC_RUST` and links `libdarc_codecs.a`, and the `unarc-sfx` job greps the stripped binary for `darc-codecs/src/` panic markers to prove it. What stays C++ is the ~3,700 lines of structure parser and CLI shell. It shares the codecs with `arc` but *not* the structure parser: that is Rust (`darc-arc/src/directory.rs`) in the archiver and C++ (`Unarc/ArcStructure.h`) here. Porting it would collapse two independent readers of the format into one and lose the cross-check. Nothing else in CI exercises the C++ one, which is why it was able to drift out of sync with the format and stay that way. The `unarc-sfx` job now builds it and round-trips through it, including running a real self-extracting archive; a build-only job would not have caught the bug that made it useless.

  The one to know about, because it is the shape the next one will take: `ArcStructure.h` read the per-file time field as **4 bytes** while `ByteStream.hs:599` writes `CTime` as a fixed 64-bit value. Everything stored after it — the directory flags and the CRCs — therefore came out of the wrong offset, so directories were recreated as zero-byte *files* and every extracted file failed its CRC. Sizes and names, which are stored *before* the time field, were perfect. A reader that lists an archive correctly can still be reading the second half of every directory block from nowhere.
- **SREP** — a huge-dictionary LZ77 preprocessor, and the one codec DArc reaches by spawning a binary rather than calling a symbol (`darc.toml`'s `[external.srep]`; it was `arc.ini`'s `[External compressor:srep]` until #129, and a leftover `arc.ini` is now *refused* rather than ignored). Both directions are now `rust/darc-codecs/src/srep/`, and `./compile` installs the port as `Tests/srep`; the vendored C (`Compression/SREP/` and `srep/`, 16,075 lines) is deleted. Its oracle lives on in the pinned reference — `rust/difftest/srep-check.sh` and `srep-encode-check.sh` build the C from `git archive` of `DARC_C_REF_SHA` rather than from the tree.
- **Lua scripting — REMOVED.** `HsLua/` held a vendored Lua 5.1 (16,338 lines) plus Haskell bindings, and `Options.hs` dispatched eight advisory events (`ProgramStart`/`Done`, `CommandStart`/`Done`, `ArchiveStart`/`Done`, `Error`, `Warning`) to handlers registered by `arc.*.lua` config scripts. Every exception from a handler was swallowed, so a script could not affect an archive; nothing in the format or the CLI depended on it. The `luaLevel`/`luaEvent` call sites remain as no-ops — the stubs the `FREEARC_NO_LUA` build always used.
- **`Installer/`** — NSIS installer scripts and packaging assets (Windows).
