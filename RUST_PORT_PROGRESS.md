# Rust port — progress and open work

Working notes for the ongoing port of DArc to Rust. Written to survive across
sessions: it records what is done, what is *not* done, and the traps that have
already cost time. Update it as things land.

**Goal:** port DArc to Rust — codecs first, then the Haskell application layer —
and delete the C and Haskell that the Rust replaces.

**Standing constraints**

- `cargo` as a hard build requirement is acceptable.
- "Format-valid" is acceptable for *standard* formats (LZ4, zstd, …), which
  loosens the **encoder** only. **Decoders must still read every archive ever
  written.** That is why the port is decode-first everywhere.
- DArc's own formats (REP, Dict, Tornado, GRZip, BSC, …) require **byte-exact**
  round-trips, proven by differential tests against the C.
- `cargo-nextest` for tests.

---

## 1. Where things stand

### Wired ≠ pruned

Keep these separate when reporting progress. Four deletions have landed --
zstd's `libzstd` (52,856 lines), Delta/Dict/REP (2,912), LZ4's `lz4.c`/
`lz4hc.c` (6,319), LZP (259) and DisPack (1,018) -- for a running total of
**63,364 lines of C removed**.
Those four codecs are Rust-only. Everything
else is still present: every other `#ifndef DARC_RUST` block and vendored tree
remains, because `Unarc/` still builds them.

| codec | Rust module | wired under `DARC_RUST` | C pruned |
|---|---|---|---|
| BSC | `bsc/` | yes | no |
| Delta | `delta` | yes (both directions) | **YES** |
| Dict | `dict`, `dict_encode` | yes (both directions) | **YES** |
| DisPack | `dispack` (both directions) | yes (encode + decode) | **YES — 1,018 lines deleted** |
| GRZip | `grzip/` (both directions) | yes (encode + decode) | partly — 1,994 encoder lines deleted; decoders stay for Unarc |
| LZ4 | `lz4` (`lz4_flex`) + `lz4hc` (own HC port) | yes (decode + both encoders) | **YES — 6,319 lines deleted** |
| LZP | `lzp` | yes (both directions) | **YES — 259 lines deleted** |
| MM | `mm`, `mmdet` (both directions) | yes (encode + decode) | partly — `mm_compress` excluded; `mmdet.cpp` stays for TTA |
| REP | `rep` | yes (both directions, byte-exact) | **YES** |
| SREP | `srep` (decode only) | external binary, no `DARC_RUST` wiring | no — see §14 before porting the encoder |
| Tornado | `tornado` (both directions) | yes (encode + decode, all 9 instantiations) | **YES — the whole C codec is gone, 3,183 lines** |
| TTA | `tta` | yes | no |
| zstd | `zstd` (`zstd-safe` binding) | yes | **YES — 2.2 MB deleted** |
| Encryption | `darc-crypto` | yes | no |

**Not ported at all:** PPMD (1,065 lines), 4x4 (700), LZMA (25,385 — stays on
the 7-Zip SDK), and the Haskell layer (17,843).

**zstd is a binding, not a port.** `zstd-safe`/`zstd-sys` compiles the same C,
fetched by cargo instead of vendored. The value is 2.2 MB leaving the repo and
maintenance moving upstream — that is the accepted rationale; do not re-litigate
it. LZ4 by contrast is genuinely Rust: `lz4_flex` for the fast encoder and the
decoder, and DArc's own HC port, byte-identical to the C for levels 1-9.

### Build paths and Rust support

| path | targets | Rust support |
|---|---|---|
| `./compile` | linux-amd64, linux-arm64, macos-arm64 | yes |
| `compile-mhs-win64` | windows-amd64, windows-arm64 | yes, both green |
| `compile-ghc`, `compile-ghc-win64` | *removed* | deleted; the Haskell layer goes to Rust |

### Branch state

`main` = `e753591` (PR #74 merged). Recent landings, each post-merge CI green
12/12: #71 Delta/Dict/REP deleted, #72 LZ4-HC ported and `Compression/LZ4`'s C
deleted, #73 the `lz4opt` optimal parser (LZ4 now byte-identical to the C at all
twelve levels), #74 the SREP heap overflow fixed.

Prior landmarks: `588522d` (PR #66: LZ4 + zstd wired, Rust codecs
cross-compiling for both Windows targets, every action SHA-pinned, Rust
toolchain pinned, CI caching); `5c2c6ce` is the pinned C-reference SHA
(`DARC_C_REF_SHA`), the last revision holding the full C codec set.

### Scale, so effort goes where the code is

Line counts over tracked source, which reframe "68% of this repo is C":

| bucket | lines | share |
|---|---|---|
| C/C++ **vendored** (libbsc, LZMA SDK, LibTomCrypt, Lua, 7-Zip SDK, SREP) | 106,330 | 60.4% |
| C/C++ **DArc's own** | 29,442 | 16.7% |
| Haskell (to port) | 20,262 | 11.5% |
| Rust (the port) | 20,100 | 11.4% |

The vendored 60% is mostly out of scope by decision (LZMA stays on the 7-Zip
SDK; libbsc and LibTomCrypt are kept pristine). **The real target is ~29k, not
135k.** DArc's own C/C++ concentrates in `Compression/GRZip` (4,148),
`Tornado` (4,051), `MM` (3,524), `BSC` wrapper (1,316), `DisPack` (1,168) and
`PPMD` (1,065) — and the first five of those are already decode-ported, with
only their **encoders** keeping the files alive.

The Rust port has also nearly drawn level with the Haskell layer (20,100 vs
20,262 lines), which makes the Haskell the largest single coherent chunk left.

---

## 2. Open work

Ordered roughly by what unblocks what.

### 1. Windows Rust cross-build — DONE

`compile-mhs-win64` cross-compiles the crates for
`aarch64-pc-windows-gnullvm` via llvm-mingw. x86-64
(`x86_64-pc-windows-gnu`, mingw-w64) is **green**, Wine round-trip included.
ARM64 has taken four attempts, each a real and different failure:

1. `__builtin_clzg` undeclared — bindgen parses DArc's headers as **C++**
   (`build.rs` passes `-x c++`, because `Common.h`/`Compression.h` are C++), so
   it reads llvm-mingw's libc++, which needs Clang 19+.
2. Same error: the "use llvm-mingw's own libclang" fix never fired — llvm-mingw
   ships the clang *driver* but **no `libclang.so`**.
3. `unknown type name 'uint64_t'` — LLVM 21's libclang was now used (that fix
   worked), but `stdint.h` is a **compiler builtin** in the resource dir, so
   feeding one toolchain's `-I` list to another's libclang strips clang of its
   own builtins.
4. Fixed by `--target` + `--sysroot`, letting clang supply its own builtins.

Both targets are green: x86-64 builds, links and round-trips archives under
Wine; ARM64 builds and links, and is exercised on a real `windows-11-arm`
runner. Each job asserts the `DARC_RUST` binary is **not** byte-identical to
the C one — the "staticlibs built then silently ignored" failure.

**If this keeps failing, stop patching the toolchain and remove the cause:**
have bindgen parse a small **C-only** shim header. The bindings only need
`CALLBACK_FUNC`, `MemSize`, `FREEARC_OK` and `FREEARC_ERRCODE_*`; parsing as C++
is what drags the entire C++ standard library into a cross-compile that
otherwise would not need it, and every one of these failures came from that.

### 2. Delete `compile-ghc` and `compile-ghc-win64` — DONE

Removed rather than given Rust support, since the GHC path goes away with the
Haskell port. Neither was built by CI or by the release workflow. `README.md`
and `CLAUDE.md` updated with them.

`compat-ghc/` and `compat-oldtime/` **stay** — both MicroHs builds use both.
Note the *reason* for keeping them in separate directories is now gone (it was
that the Wine GHC build had to see one and not the other), so they could be
folded together; left alone because both disappear with the Haskell port.

Also worth knowing: `HsLua/` is **not** GHC-only, whatever CLAUDE.md used to
say — `./compile` builds the vendored Lua from `HsLua/src`. Only the Windows
cross-build sets `FREEARC_NO_LUA`.

**Every remaining build path now has Rust support, so the gate is open:
`DARC_RUST` can become the default.**

### 3. Toolchain plumbing for the flip — DONE

Landed on `rust-prune-prep`: `release.yml` had **no Rust setup at all** (it
builds every shipped binary), `build.yml`'s build matrix had none either, the
Rust toolchain was pinned nowhere, and **not one of the 39 action uses was
pinned by SHA**. Now: `rust-toolchain.toml` pins 1.97.1 plus both Windows cross
targets (single source — no `targets:` inputs in the workflows), every action is
pinned to a full commit SHA, and cargo/rustup/`rust/target` plus `~/.mcabal` are
cached. The MicroHs rebuild is guarded inside the script on `mhs` already
existing, so the `PATH` export still runs on a cache hit.

### 4. Flip `DARC_RUST` to the default — DONE

The Rust codecs are mandatory; the `DARC_NO_RUST=1` opt-out was removed.
The opt-out is deliberately kept: CI builds both ways and asserts the archives
are byte-identical, and that comparison is what licenses deleting the C. Both
Windows jobs invert the same way, and their uploaded artifacts — the ones the
interop jobs and the real ARM64 runner exercise — are now the Rust build.

Verified locally: both builds succeed (4.47 MB Rust vs 2.68 MB C-only), the
binaries differ, and all 24 fingerprints are identical across them.

### 5. Prune, in dependency order:

- ~~**`Compression/Zstd`** (2.2 MB)~~ — **DONE.** No difftest referenced it and
  `-mzstd` is in no fingerprint case, so the oracle-pinning prerequisite did not
  apply; that only blocks codecs that *have* a harness. zstd is now Rust-only:
  there is no C to fall back to, so its entry points left the `dropin` feature
  gate and the Rust staticlib is always linked (which
  otherwise still binds every other codec's C).

  **Worth knowing:** before this, the vendored 1.5.6 objects were `ld -r`-merged
  into `C_Zstd.o`, which defines the same `ZSTD_*` symbols as zstd-sys's 1.5.7
  inside the staticlib. An object always beats an archive member, so the "Rust"
  path was in fact still executing the vendored 1.5.6. Deleting the tree is what
  actually switched it to the crate -- which is why `-mzstd:long20` moved by 9
  bytes (an LDM heuristic differing between 1.5.6 and 1.5.7) while every other
  setting stayed identical. Decode is unaffected and verified: all six archives
  written by the vendored build still extract byte-identically.
- ~~**Delta, Dict, REP**~~ — **DONE.** `Delta.cpp`, `dict.cpp` and `rep.cpp`
  deleted (2,815 lines). Like zstd they are now Rust-only, so their drop-ins
  left the `dropin` feature gate; LZP's stayed gated, because its C survives and
  ungating it would be a multiple definition in the Unarc build.

  Two things the wrappers still needed from the deleted files, neither of which
  a "which entry points does it call?" check would have found: `C_REP.cpp` uses
  `sqrtb` and `CalcHashSize` for its memory estimate (moved into the wrapper
  verbatim), and `C_Dict.cpp` still had a guarded C `dict_decompress` calling
  `DictDecode`. Both surfaced only as build errors.
- ~~**LZP**~~ — **DONE.** The one codec ported in both directions whose C had
  survived: `LZPEncode`, `LZPDecode`, their rotate/hash helpers and both
  callback wrappers are gone (259 lines; `C_LZP.cpp` 392 → 145). Only
  `LZP_METHOD::*` and `parse_LZP` remain, because `lzp_compress`/
  `lzp_decompress` are bound by the Haskell FFI (`CompressionLib.hs:299-304`)
  and `facompress.def` — the Rust crate now supplies those symbols.

  **It nearly went out unverified.** `C_LZP.cpp` claimed "verified
  byte-identical over 8 inputs in both directions; see rust/difftest", and the
  table above said both directions were ported — but **no `lzp-check.sh` ever
  existed**. `lzp_ref.cpp` sat orphaned, `run.sh` covers Delta only, and CI ran
  nothing for LZP. The harness was written *before* the deletion: 84/84
  comparisons byte-identical across four block sizes (8 MB → 16 KB), four
  sabotages caught (36/13/8/12 failures). A codec's claim to be "verified" is
  worth checking against the CI job list, not the comment above it.
- Everything else is decode-only, so its encoder keeps the file alive; pruning
  there is surgical — the `#ifndef DARC_RUST` blocks total only **361 lines**
  of entry points, because the decode logic is interleaved with encode logic in
  shared files. **The encoders, not the decoders, are what unlock file
  deletion** from here.
- **Leave vendored trees pristine** (libbsc, LZMA SDK) per `CLAUDE.md`, or make
  that an explicit, recorded exception.

Two mechanical prerequisites, neither done:

- **Stop `Compression/Zstd/makefile` compiling libzstd.** The wrapper no longer
  calls it, but the makefile still builds the whole tree into `C_Zstd.o`, so the
  directory cannot simply be deleted.
- **Pin the difftest harnesses to a git revision** (item 6) before any C decoder
  goes.

### 6. Preserve the differential-test oracle — DONE

`rust/difftest/c-reference.sh` extracts `Compression/` at a pinned revision
(`DARC_C_REF_SHA`) with `git archive`, copies the CURRENT shims in beside it so
their `#include "../../Compression/..."` paths resolve into the pinned tree, and
exports `CREF`. The ten Compression-based harnesses compile their C reference
from `$CREF` while the Rust staticlib and harness logic stay current.
`srep-check.sh` is deliberately excluded: its oracle is the `srep` **binary**
from `srep/compile`, not sources under `Compression/`.

The reference is **always** the pinned revision, even while the C is still in
the tree. A fallback that only engaged after deletion would sit untested until
it became load-bearing — how the MicroHs cache guard shipped broken. A fixed
oracle also cannot drift, so a concurrent C change cannot mask a Rust
regression.

Verified by deleting `rep.cpp`, `Delta.cpp` and `dict.cpp` from the working
tree and re-running: all harnesses still pass. Bumping `DARC_C_REF_SHA` changes
what "correct" means for every harness, so it is a deliberate act.

### 7. Port `lz4hc.c` to Rust — DONE, all three strategies

`rust/darc-codecs/src/lz4hc.rs` ports **every** strategy — `lz4mid` (levels
1-2), the `lz4hc` hash chain (3-9) and the `lz4opt` optimal parser (10-12) —
and is **byte-identical to the C at all twelve levels** (26 inputs per level,
`rust/difftest/lz4hc-check.sh`, which gates on identity rather than size).

The optimal parser landed in a follow-up. It also needed `chainSwap`, skipped
the first time round because the hash-chain parser passes 0 for it at every
call site; `LZ4HC_FindLongerMatch` is its only caller.

All 292 KB of vendored C is gone (`lz4.c`, `lz4.h`, `lz4hc.c`, `lz4hc.h`,
6,319 lines) and LZ4 is Rust-only, like zstd. `LZ4_compressBound` became an
inline formula in the wrapper; `LZ4_sizeofState{,HC}` became the two measured
constants 16416 and 262200 — **not** free to re-derive from what Rust
allocates, because `SetCompressionMem` subtracts them before sizing
`BlockSize`, which decides where block boundaries land in the archive.

Two details decided byte-identity, neither visible from the signatures, and
both caught *only* by the repetitive corpus inputs:

- **`patternAnalysis`** (enabled when `nbSearches > 128`, i.e. levels 9+)
  short-circuits chains of one repeated byte. Without it output was still valid
  and only 0.08% larger — comfortably inside any ratio budget, so a
  size-threshold harness would have passed and hidden it. That is the argument
  for gating on byte-identity wherever it is achievable.
- **`chainSwap`'s accelerating stride** is part of the result, not a speed
  trick: changing `kTrigger` from 4 to 3 changes which matches are found.
- **`lz4mid` fills its hash tables with a stale `ipIndex`** — the one captured
  at the top of the loop, which its own catch-back invalidates
  (`lz4hc.c:677-679`). Recomputing it is the obvious "cleanup" and makes output
  diverge on exactly the repetitive inputs.

**One measured blind spot, recorded rather than papered over.**
`literalsPrice`'s `1 + (litlen - RUN_MASK)/255` term is the only part of the
port the harness cannot exercise: `/255`→`/254`, `>=`→`>`, and even
multiplying the term by 10 all leave every input byte-identical, while a change
to `sequencePrice`'s token cost is caught on 5 inputs. The cause is structural,
not a thin corpus — at a given position every candidate path shares the same
`llen`, so a constant added there cancels out of every comparison. Two corpus
inputs were built specifically to break that (`priced`, `competing`) and did
not. Those three lines are verified by transcription against the C only.

### 8. DisPack — DONE, both directions, C deleted

The first encoder to take, and the only one whose completion deletes a whole
directory. Cheaper than `DisPack.cpp`'s 31 KB suggests: most of that file is
opcode tables that are **already ported** for the decode side
(`rust/darc-codecs/src/dispack/tables.rs`), so the encoder can reuse them.

The encoder is one contiguous `#ifndef FREEARC_DECOMPRESS_ONLY` block,
`DisPack.cpp:328-656` — **329 lines** — which also makes the eventual deletion
clean rather than surgical:

| piece | lines | what it is |
|---|---|---|
| `DataBuffer` | 332-370 | the multi-stream output buffer (`ST_MAX` streams) |
| `DisFilterCtx` | 371-599 | `DetectJumpTable`, `ProcessInstr` (418-564, the bulk), `Flush` (565-598) |
| `DisFilter` | 600-654 | driver: main loop, then a checkpoint/undo tail so the last `MAXINSTR` bytes never read past the end, then escape-encodes any remainder |
| `detect()` | `C_DisPack.cpp:142` | ~35 lines, EXE-type detection that decides whether to filter at all |

**Result: byte-identical to the C over 76 comparisons across four load
origins**, and the `dispack` fingerprint `6a46351e39373082` is unchanged, so
archives are the same end to end. Wired under `DARC_RUST` in `C_DisPack.cpp`;
the C `DisFilter` stays for `Unarc/` until the directory is deleted.

Seven sabotages, all caught — but **two were blind until the corpus was fixed,
and the second fix needed a mechanism, not more data**:

- *Jump-table threshold 3→2* was unobservable because the corpus had only a
  run of 64 **consecutive** in-range dwords, which both thresholds accept
  identically. Only a run of **exactly two** distinguishes them.
- *MTF search bound 255→254* survived a first attempt that simply threw 400
  distinct targets at it: every distinct target is a **miss**, so `find_mtf`
  never returns a hit and the bound is unreachable. It only matters on a lookup
  landing at exactly index 254. `add_mtf` pushes to the front, so inserting
  `t0..t299` leaves `mtf[k] == t(299-k)` and puts `t45` at 254; referencing
  `t45` next is found with the real bound and missed with a smaller one. The
  catch confirms it exactly — 1581 vs 1585 bytes, the four extra bytes of a
  full address where a one-byte index belonged.

Two things to get right, both already known from the decode port:

- **`detect()` gates everything.** DisPack only filters what it sees as x86
  code; everything else is stored and the filter never runs. A corpus of
  ordinary data tests the store path and nothing else — this already produced a
  green-but-empty first pass once. The `dispack-check.sh` corpus cross-compiles
  real i386 `.text` and rewrites E8 placeholders into backward calls, which is
  what `detect()` keys on; reuse it.
- **Byte-exactness is required**, not format-validity: DisPack is DArc's own
  format. `-mdispack` has a fingerprint case (`6a46351e39373082`).

### 9. Hand-port PPMD (1,065 lines)

The last real hand-portable codec besides 4x4. **No crate path:** `ppmd-rust`
was measured and rejected — DArc's PPMD is Shkarin var.H with **Subbotin's**
carryless range coder (32-bit `low`, `TOP=1<<24`, `MAX_O` 128); `ppmd-rust` is
7-Zip's Ppmd7 with **Pavlov's** coder (64-bit `Low`, `MAX_O` 64). Same model,
different stream. Do not revisit the crate. `-mppmd` already has a fingerprint
case.

### 10. Decide explicitly whether to port 4x4 — recommendation: no

Threading meta-codec; its decode delegates to the library dispatcher
`Decompress()` per block, so the only portable logic is block framing
(`C_4x4.cpp:436`, call at `:237`). Its value is parallelism, which a decode-first
port drops. There is **no fingerprint case** (the suite's `-m4x` is an unrelated
exe preset, not this codec). A Rust decode would be an FFI shim calling C
`Decompress`, which under `DARC_RUST` dispatches back to Rust drop-ins
(Rust→C→Rust). Record the decision so it is not re-litigated.

### 11. Port the Haskell application layer (17,843 lines, 41 files)

The largest remaining piece. Suggested order (from `CLAUDE.md`):
`Arc.hs:71` (`doMain`) → `Cmdline.hs:35` (`parseCmdline`) → `Arc.hs:110` (`run`)
→ `ArcCreate.hs:51` (`runArchiveCreate`) → `ArcCreate.hs:165` (the two-process
pipeline). Read `Options.hs:39` (the `Command` record, threaded through
everything) and `Process.hs:21-52` early.

Facts that shape the port:

- **Nearly every mutating command is the same operation.** `create/a/f/m/u`,
  `j`, `ch/c/k/rr/s` and `d` all converge on `runArchiveAdd` →
  `runArchiveCreate`; delete is just an inverted file-selection filter
  (`Arc.hs:243`). Only extract/test/list and recover are separate paths.
- **`Process.hs` is Hoare-style CSP over real OS threads**, not lazy lists.
  Pipes carry a *backward* channel for buffer recycling. Changing a stage's
  send/receive protocol **deadlocks rather than type-errors**.
- `OurChan` exists only because MicroHs's `put_mvar` does not wake `readMVar`
  (`Process.hs:60-100`); a Rust port makes that workaround unnecessary.
- **`ByteStream.hs` is directly responsible for on-disk format bytes** — changes
  there are format-breaking by default. `ArhiveStructure.hs` is the physical
  block layout; `ArhiveFileList.hs` (~46 KB) is the diff/merge engine and is
  deliberately memory-optimised.
- `type Method = String` — compression config is a **string parsed by C**, not
  an ADT, and properties are queried through `compressionService`. This does not
  need redesigning to port.
- `UI.hs` re-exports either `CUI` or `GUI` by `#ifdef`; the GUI build pulls in
  the GTK file manager (`FileMan*.hs`), which the console build never compiles.

When this lands, MicroHs and the `compat-ghc`/`compat-oldtime` shims become
removable too, and the build collapses to cargo.

### 12. Performance: measure before optimising

Several Rust ports are deliberately scalar where the C is vectorised: BSC's LZP
and adler32, the QLFC SIMD variants, and the BSC fast coder's SIMD MTF shuffles.
**Measure first** — LZP is one of four decode stages and may not be the
bottleneck. Scalar is a *shipped configuration* of libbsc (i386,
`-DLIBBSC_NO_UNALIGNED_ACCESS`), not a subset, so correctness is not at issue.

### 13. C-side bugs

**Fixed: TWO distinct SREP bugs with ONE symptom (PRs #74 and #78).** Both
produced corrupt `-msrep` archives that fail their own checksum on extraction.
Do not treat the first as the whole story -- #74 was real, ASan-confirmed, and
did **not** stop the failures; #78 is the one that closed it. Same signature is
not the same bug, and declaring victory on the first fix cost several rounds.

#78 (the one that actually ended it): `srep.cpp` handed `header[]` and
`statbuf[]` back to the background thread via `bg_thread.write()` and *then*
`memcpy`'d out of them. They are rotating buffers only `BUFFERS(=2)` deep, so
the producer refilled the slot mid-copy -- a **use-after-release**. Moving the
copy above the write closes it; widening the ring would only have narrowed it.

**No sanitiser can find this class.** Every byte written is in-bounds,
initialised and mutex-free; only the buffer's *lifetime* is wrong. ASan (144
compress+decompress pairs), TSan (verified linked, 36 runs), hand-poisoned
`BigAlloc` (3 fill values x 150) and `MallocPreScribble`/`MallocScribble` (200
each) were all clean. What identified it was the **corrupt artefact**: a bad
archive differed from a good one in exactly 16 bytes -- an MD5 digest -- and
that digest belonged to the block two positions later, exactly the ring size.

**Reproduction is load-dependent.** An idle machine gives 0/250 even when
broken, so any before/after comparison must run under scheduling pressure or
its control never fires. Better still, amplify: a 3 ms delay in the gap makes
the broken order fail 40/40 and the fixed order 0/40, which is proof rather
than statistics.

#74 (real, but not the end): a truncating `filesize/L` sized `SliceHash`'s array
one entry short, so a file whose size is not a multiple of `L` wrote one past
the end. Recorded
because the diagnosis is reusable and two obvious hypotheses were both wrong.
`SliceHash` stores one entry per `L` input bytes indexed by `offset/L`, but
sized the array with a truncating `filesize/L * sizeof(entry)` — so a file whose
size is not a multiple of `L` had no slot for its final partial chunk and
`prepare_buffer` wrote one entry past the end of `h[]`. It corrupts the heap
rather than crashing, so the symptom was an archive failing **its own checksum**
on decompression, intermittently (~1-6%). DArc runs `srep` as an external
compressor, so `-msrep` archives could be unextractable — caught by the
checksum, so not silent data loss, but produced by a compress that reported
success.

Ruled out on the way, so they are not re-tried: **not** the fixed-name temp
files (`srep-data.tmp`, `srep-virtual-memory.tmp`, both relative to CWD — the
obvious suspect from reading), and **not** a threading race (`-t1` reproduces it
at the same rate).

**Fixed: the ARM64 `ulong32` miscompilation, and the self-tests that hid it.**

`tomcrypt_macros.h` typed `ulong32` as `unsigned` only for `__x86_64__`/sparc64
and `unsigned long` otherwise — 64 bits on every other LP64 target, ARM64
included. LibTomCrypt's own comment says "at least 32-bits" and most primitives
mask their shifts, but `serpent.c`'s key expansion rotates with a raw
`(lk<<11)|(lk>>21)`: a rotate at 32 bits, garbage at 64. Now `typedef uint32_t
ulong32`, which is exact on every target — the platform conditional is gone
rather than having ARM64 bolted onto it.

**Blast radius, previously unknown, now measured.** The question "are
AES/Twofish/Blowfish/HMAC/PBKDF2 hit too?" was answered by running LibTomCrypt's
shipped self-tests, whose vectors are authoritative:

| primitive | ulong32 = 64 bits | ulong32 = 32 bits |
|---|---|---|
| blowfish | PASS | PASS |
| **serpent** | **FAIL** | **PASS** |
| twofish | PASS | PASS |
| sha1 | PASS | PASS |

**Serpent was the only casualty** — the others mask correctly. AES could not be
self-tested here (`ENCRYPT_ONLY` omits the decrypt half, so `rijndael_test` is
commented out upstream), and `hmac_test`/`ctr_test`/`cfb_test` are likewise
commented out in `C_Encryption.cpp`; those remain unverified by vectors, though
the cross-implementation check below covers them end to end.

**Compatibility consequence, accepted deliberately.** The ciphertext changes on
ARM64 (`e1acc75e…` fixed vs `3ad490df…` before), so `-ae serpent` archives
written by the shipped v2.0.0/v2.1.0 arm64 binaries cannot be opened by a fixed
build. They could never be opened by an x86 build either — only by the same
broken platform — so the fix is right, but say so when releasing.

**`LTC_NO_TEST` is no longer defined.** The self-tests run once from
`register_all()`'s static initialiser and cost microseconds; `serpent_test`
fails outright with the old typedef, so this is the guard that would have caught
it on the first ARM64 build. Note the whole block is `#ifndef DARC_RUST`, so
only the C-crypto comparison build pays even that.

**srep's second copy of the header was checked and deliberately NOT changed.**
`srep/Compression/_Encryption/headers/tomcrypt_macros.h` still has the old
typedef, but srep compiles no serpent, and its md5/sha1 pass at 64-bit width.
`CLAUDE.md` records that tree as a diverged vintage to be fixed independently.

### 14. If SREP's ENCODER is ever ported: model the ring, do not transliterate it

The bug fixed in #78 is the strongest argument in this repo for the port, and
also a warning about *how* to port. It is a producer/consumer **lifetime** error:
a buffer is released back to the producer and then read. Rust makes that class
unrepresentable -- if `write()` consumed the buffer handle, or the slot were held
by a guard whose `Drop` performs the release, using `header` afterwards would
fail to compile. Borrowck enforces exactly the property the C left to author
discipline and got backwards.

But that safety is **not automatic**, and two caveats matter:

- **Decode-first would not have caught it.** The Rust SREP work is decode-only;
  this bug is in the compressor. The natural next step would have left it
  untouched.
- **A faithful transliteration would inherit it.** Keeping the same
  `write()`-then-`memcpy` order with raw pointers or indices into a shared `Vec`
  reproduces the bug exactly. Since the porting rule here is byte-exact
  transliteration, that is a live risk, not a theoretical one. The safety only
  appears if the *ownership* is modelled -- a consumed handle or an RAII guard --
  rather than the index arithmetic copied across.

So: port the buffer ring by making the release a move or a `Drop`, and let the
byte-exactness requirement apply to the output stream, not to the internal
plumbing.

### 15. Build/quality odds and ends

- **`-mtor` is 35% larger on llvm-mingw builds** (34,771 → 47,043 for the
  identical spec `tor:434kb`; 11 other methods differ by <0.5%). Output is
  correct, compression is worse. Established by measurement that it is the
  **toolchain, not the arch**. Already ruled out: arch, thread count, memory
  limit, parameter selection, and strict aliasing (adding
  `-fno-strict-aliasing` left output byte-for-byte unchanged). **Next untried:**
  build only `Compression/Tornado` with mingw-gcc while the rest uses
  llvm-mingw, to confirm the miscompile is in that TU.
- **`ub-flags-pass`** local branch has 2 unmerged commits (`f8285ce` difftest
  harness, `0fe2953` bindgen ABI bindings). Both predate the current harnesses
  and `darc-crypto`'s bindgen setup, so check whether either still adds anything
  before deciding. Do not delete the branch blindly.

---

## 3. How to work on this

```bash
# Build
./compile-O2                     # stock C build      -> Tests/arc
DARC_RUST=1 ./compile-O2         # against Rust       -> Tests/arc
DARC_RUST=1 ./compile-mhs-win64  # Windows x86-64
DARC_RUST=1 DARC_WIN_ARCH=aarch64 ./compile-mhs-win64

# Test
cd rust && cargo nextest run     # 150 tests
Tests/run-tests.sh "$PWD/Tests/arc"   # 24 round-trips + archive fingerprints
rust/difftest/<codec>-check.sh        # C vs Rust, byte for byte
```

**Always `rm -rf /tmp/out` between stock and `DARC_RUST` builds.** Objects are
shared across build paths and the makefile does not rebuild on a define change,
so you will otherwise link a stale object against the wrong libraries.

### The recipe for porting a codec

1. Port decode first.
2. Write `<codec>_ref.cpp` + `<codec>-check.sh`: C compresses, C **and** Rust
   decode, both must reproduce the original byte-for-byte.
3. **Wire the script into the CI `rust-codecs` job** — a differential test
   nothing invokes is documentation, not a check.
4. Sabotage the port and require the test to *fail*. Re-verify each sabotage
   actually applied; an unapplied patch reads as "no difference".
5. Add `nextest` cases for malformed input (`arc t` runs decoders on hostile
   data; a panic across the C ABI is undefined behaviour).
6. `#ifndef DARC_RUST` exclusion **and** the drop-in in the **same commit**.
   Leaving both is a multiple definition on GNU ld and silently prefers the C
   one on macOS.
7. Add/confirm a fingerprint case in `Tests/run-tests.sh`, and check the
   `DARC_RUST` build reproduces it.

---

## 4. Traps that have already cost time

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

---

## 5. Process notes

- PRs: the author reviews once, then says merge. **Never merge unprompted.**
- **Never install anything** (`brew`, `apt`, `cargo install`, `rustup target
  add`, …) without asking first.
- Do not poll CI in the foreground; background the wait.
- `Build CI` only triggers on push/PR to `main`, plus `workflow_dispatch`. For a
  branch, dispatch it explicitly and **confirm a run exists** — a PR's
  `pull_request` trigger has silently failed to fire before while
  `gh pr checks` reported success from CodeQL alone.
