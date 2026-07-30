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

Keep these separate when reporting progress.

Don't hand-maintain a running total here -- the one that used to sit in this
paragraph drifted wrong within two PRs, because each deletion updated its own
row and not the sum. Measure it:

```bash
for rev in 5c2c6ce HEAD; do
  git ls-tree -r --name-only $rev -- Compression/ | rg '\.(c|cpp|h|hpp)$' \
    | while read -r f; do git show "$rev:$f" | wc -l; done | awk -v r=$rev '{s+=$1} END {print r, s}'
done
```

`5c2c6ce` is the pinned C-reference SHA, the last revision holding the full C
codec set, so the difference is what the port has removed since. At `7a054bd`
that is 104,052 → 61,338, i.e. **42,714 lines**. (It read 38,991 at `0159485`;
the figure is a moving target, which is why the command above is the answer and
this sentence is only an illustration.)

The old claim that everything unpruned "is still present because `Unarc/` still
builds them" is no longer a reason for anything: `Unarc/makefile` links
`libdarc_codecs.a` built with the `dropin` feature, so Unarc gets its decoders
from Rust like the archiver does. Anything still here needs a reason that is
true today -- see the GRZip row.

| codec | Rust module | wired under `DARC_RUST` | C pruned |
|---|---|---|---|
| BSC | `bsc/` | yes (both directions) | **YES — the vendored libbsc is gone, 26,700 lines** |
| Delta | `delta` | yes (both directions) | **YES** |
| Dict | `dict`, `dict_encode` | yes (both directions) | **YES** |
| DisPack | `dispack` (both directions) | yes (encode + decode) | **YES — 1,018 lines deleted** |
| GRZip | `grzip/` (both directions) | yes (encode + decode) | **YES — 1,994 encoder lines, then 2,105 more; only the method object and parser are left** |
| LZ4 | `lz4` (`lz4_flex`) + `lz4hc` (own HC port) | yes (decode + both encoders) | **YES — 6,319 lines deleted** |
| LZP | `lzp` | yes (both directions) | **YES — 259 lines deleted** |
| MM | `mm`, `mmdet` (both directions) | yes (encode + decode) | **YES — `mm.cpp` deleted; only `mmdet.cpp` remains, and it is NOT dead (the Haskell FFI calls it, see below)** |
| PPMd | `ppmd/` (both directions) | yes (encode + decode) | **YES — 1,146 lines, the whole engine** |
| REP | `rep` | yes (both directions, byte-exact) | **YES** |
| SREP | `srep` (decode only) | external binary, no `DARC_RUST` wiring | no — see §14 before porting the encoder |
| Tornado | `tornado` (both directions) | yes (encode + decode, all 9 instantiations) | **YES — the whole C codec is gone, 3,183 lines** |
| TTA | `tta` | yes (both directions) | **YES — `tta.cpp`, `entropy.cpp` and `filters.cpp` deleted, 1,190 lines** |
| zstd | `zstd` (`zstd-safe` binding) | yes | **YES — 2.2 MB deleted** |
| Encryption | `darc-crypto` | yes | no |

**Not ported at all:** 4x4 (700), LZMA (25,385 — stays on the 7-Zip SDK), and
the Haskell layer (17,843).

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

### Dead C that a stale comment is keeping alive

A codec whose entry points are Rust does not automatically shrink: the support
code around them stays compiled, and the comment explaining why tends to outlive
the reason. Two were found false by reading the tree instead of the comment:

- **GRZip's decoder** was kept because Unarc "does not link the Rust crate".
  `Unarc/makefile` links `libdarc_codecs.a` with the `dropin` feature, which is
  exactly what exports `grzip_decompress`. Its worker pool was kept "for the
  encoder, which still uses them" -- in the same file whose encoder had already
  been deleted. Both are gone now (2,105 lines), and all 13 `-mgrzip` parameter
  combinations still extract through Unarc.
- **`Compression/MM/`**: `mm.cpp`, `tta.cpp`, `entropy.cpp` and `filters.cpp`
  were support code for entry points that are now Rust, and their own comments
  said so -- the encode halves "stay ... cost only a few unused bytes".
  `C_MM.cpp` and `C_TTA.cpp` called nothing from them but `mm_compress`,
  `mm_decompress`, `tta_compress` and `tta_decompress`, all four supplied by the
  Rust crate. Gone, 1,627 lines, plus `ttaenc.h` trimmed to the two entry points
  it still declares.

**The check that settles it** is not reading the comment but asking who calls
the symbol, with the codec's own directory excluded from the search -- a file
whose only callers are itself and its header is dead:

```bash
rg -n --glob '!Compression/<Codec>/**' --glob '!rust/**' '\b<symbol>\b'
```

**Run it over the file's whole exported surface, not over the symbols the
comment happens to name.** `mmdet.cpp` looked dead by exactly this check and is
not: its comment justifies it by a `tta.cpp` call to
`autodetect_wav_header`/`autodetect_by_entropy` that no longer exists, and
searching for those two names finds nothing. But `mmdet.h` exports four more --
`detect_datatype`, `detect_mm`, `detect_mm_header`, `detect_mm_bytes` -- and all
four are bound by the Haskell FFI at `ArhiveFileList.hs:588-598`, where they
drive `$text`/`$exe`/`$compressed` grouping and MM autodetection. The file is
load-bearing for solid-block layout. A stale comment can name the wrong reason
for a conclusion that is still correct; enumerate the header, then search.

**And then prove the test can fail.** After `mm.cpp` went, `mmdet.cpp`'s include
moved into `C_MM.cpp`, and the evidence that this was done right is that `-m4`,
`-m5` and `-m9` archives stayed byte-identical -- which is only evidence if
those bytes depend on the detector at all. They might not have: `arc.groups`
also groups by extension (`$wav` covers `*.wav`/`*.pcm`), so the grouping
visible in a listing can be extension-driven. Forcing `detect_datatype` to
always answer `"default"` for a non-NULL buffer moved the `-m9` archive by 1,422
bytes, so the comparison is sensitive to it and the identity result means
something. That check costs one rebuild and is the difference between a test and
a formality.

### Branch state

`main` = `a71a717` (PR #114 merged), post-merge CI green.

**Do not trust a hand-written SHA here for long.** This section was stale by
fourteen PRs once, while the codec table above had been kept current -- which is
worse than uniform staleness, because a reader cannot tell which half to believe.
Check with `git log --oneline origin/main -1`.

Recent landings, newest first:

* **#114** the vendored LibTomCrypt deleted -- 47 files, 9,349 lines. See below;
  what kept it alive was not what three earlier analyses claimed.
* **#113** `unwrap`/`expect` denied in production paths, with three argued
  exceptions and two documented non-goals.
* **#112** Tornado's 78-line encode if-chain became an exhaustive `match`. The
  claim in #111 that its order was load-bearing was measured and found wrong.
* **#105, #106/#107, #109, #110, #111** item 4: mode bytes modelled as types, one
  codec per PR -- DisPack, GRZip, MM, TTA, Tornado. See section 10b.
* **#108** no `if let` anywhere in the Rust workspace; totality enforced by
  `deny(clippy::wildcard_enum_match_arm)` plus a CI grep. See section 10c.
* **#103, #104** crate-level lint gates, and the eleven `_ => {}` catch-all arms
  resolved against the pinned C.
* **#102** Tornado presets 7-11 diverged: `Hash3` implemented `update_hash1`
  only inherently, so `CombineMF`'s `Box<dyn MatchFinder>` reached the trait
  default -- a `debug_assert!` that compiles out in release.
* **#99** Dict/LZP reported Unarc's stop signal as an I/O error, so every `-m9`
  archive containing text was unreadable by the standalone extractor and every
  SFX module.
* **#97, #98** the GRZip and MM/TTA C engines deleted (3,700 lines).
* **#100, #101** `dict-check.sh` written (Dict had no difftest at all) and
  `grzip-stage-check.sh` wired into CI (it had never run).

Earlier: #86-#92 the BSC encoder ported stage by stage, #93 the vendored libbsc
deleted, #94 PPMd ported, #95 every difftest oracle rebuilt with its codec
makefile's flags, #96 `-mppmd` routed through Rust and the C engine deleted.

Earlier: #71 Delta/Dict/REP deleted, #72 LZ4-HC ported and `Compression/LZ4`'s C
deleted, #73 the `lz4opt` optimal parser (LZ4 now byte-identical to the C at all
twelve levels), #74 the SREP heap overflow fixed, #83 Tornado.

Prior landmarks: `588522d` (PR #66: LZ4 + zstd wired, Rust codecs
cross-compiling for both Windows targets, every action SHA-pinned, Rust
toolchain pinned, CI caching); `5c2c6ce` is the pinned C-reference SHA
(`DARC_C_REF_SHA`), the last revision holding the full C codec set.

### How much of the port is actually closed

Measured at `7a054bd` against `5c2c6ce`, the pinned reference holding the full C
codec set. Regenerate rather than trusting these:

```bash
for rev in 5c2c6ce HEAD; do
  git ls-tree -r --name-only $rev | rg '\.(c|cpp|h|hpp)$' \
    | while read -r f; do git show "$rev:$f" | wc -l; done \
    | awk -v r=$rev '{s+=$1} END {print r, s}'
done
```

| | pinned `5c2c6ce` | `a71a717` | change |
|---|---|---|---|
| C/C++ under `Compression/` | 104,052 | 51,553 | **−52,499 (−50%)** |
| C/C++ whole repo | 143,739 | 93,259 | −50,480 (−35%) |
| Haskell | 20,262 | 20,265 | **+3 (untouched)** |
| Rust | 16,285 | ~33,500 | +17,200 |

**Read that carefully before calling the port "50% done".** The remaining 51,553
lines under `Compression/` are dominated by code nobody intends to port -- LZMA and
7z alone are 39,180 of them, **76%**:

| lines | what | status |
|---|---|---|
| 25,391 | `LZMA` | 7-Zip SDK, still the implementation in use -- but see below |
| 13,789 | `7z` | 7-Zip SDK for `.7z` reading, kept |
| 3,168 | `SREP` | external tool; section 14 before touching the encoder |
| 2,329 | top level | `CompressionLibrary.cpp` and the dispatcher |
| 709 | `4x4` | not ported by decision (section 10), and NOT dead code |
| ~4,000 | everything else | thin `C_*.cpp` wrappers for ported codecs, plus `mmdet.cpp` |

Both stay. So the codec *engines* are done, and after #114 there is very little
prunable C left anywhere: what remains in `Compression/` is vendored SDKs, an
external tool's glue, `4x4` (deliberately unported, and not dead), `mmdet.cpp`
(Haskell FFI-bound), and the thin entry-point layer that can only go when the
Haskell side does.

**The honest summary: the codecs are done, the application layer has not been
started.** 20,265 lines of Haskell are within 3 lines of where they were. By any
line-count measure that is the majority of the remaining work, and it is at
roughly zero percent. Outside `Compression/` the C/C++ is `HsLua` 16,338
(vendored Lua, kept), `srep` 12,907 (vendored tool), `Unarc` 5,389 (the second,
independent archive reader), `rust/` 3,887 (difftest drivers, deliberately C) and
2,677 at the root (`Environment.cpp` and friends).

**`_Encryption` was pruned in #114, and getting there took three wrong answers.**
Worth reading as a method failure, because each wrong answer was plausible:

* *"SREP's hashing holds it."* `Compression/SREP/hashes.cpp` does `#include` five
  LibTomCrypt `.c` files -- but **no makefile compiles it.** It is the mirrored
  copy CLAUDE.md describes, and the real `srep/` build carries its own diverged
  tree. Repointing it would have changed nothing that gets built.
* *"~10,000 lines of live C."* `_Encryption/makefile` builds only
  `C_Encryption.o`, and that file includes nothing but `C_Encryption.h`. The
  LibTomCrypt sources never entered `arc`. The `-Iheaders` in that makefile and in
  Unarc's was vestigial.
* *"The Haskell FFI holds it."* `EncryptionLib.hs`/`EncryptionFFI.h` do name
  `fortuna_start` and `sha512_init` -- but `C_Encryption.cpp` **defines those
  itself** as shims onto `darc_rs_random_fill` (`:49-56`).

What actually held it was the crypto difftest oracle, through **one line**:
`crypto-check.sh` overlaid `headers/tomcrypt_macros.h` from the working tree onto
the pinned reference. That header is a **fix, not a copy** -- `uint32_t` where the
pinned one says `unsigned`, which is 64 bits on LP64 targets other than x86-64 and
turns serpent's key-expansion rotate into garbage; its comment records the shipped
linux-arm64 builds that were affected. It now lives in `rust/cryptref/` beside its
only consumer.

An intermediate plan to take that header *from the pinned tree instead* would have
silently reduced a two-reference check to one. The harness's own
`grep 'typedef uint32_t ulong32'` guard would have caught it -- **a guard that
asserts what a fixture is FOR, not merely that it exists.**

Every claim above cost one command to check and none of them were checked first.
Same family as section 4's `mmdet.cpp` note.

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
`Tornado` (4,051), `MM` (3,524), `BSC` wrapper (1,316) and `DisPack` (1,168) —
already decode-ported, with only their **encoders** keeping the files alive.
`PPMD` is gone: 1,065 lines deleted, `C_PPMD.cpp` reduced to the
COMPRESSION_METHOD wrapper.

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
- ~~Everything else is decode-only, so its encoder keeps the file alive.~~
  **Superseded.** That was written when encoders were the blocker. Every codec in
  the table above is now ported in both directions and its C engine deleted, and
  the `#ifndef DARC_RUST` fallbacks are gone with the `DARC_NO_RUST` opt-out. What
  keeps the remaining `Compression/` files alive is no longer a missing encoder:
  it is the vendored SDKs (LZMA, 7z), the external tools (SREP), `mmdet.cpp`'s
  Haskell FFI bindings, and the thin `C_*.cpp` entry points, which can only go
  when the Haskell layer does. See "How much of the port is actually closed".
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

### 9. Hand-port PPMD (1,065 lines) — DONE

Ported and the C deleted. **No crate path, and that assessment held:**
`ppmd-rust` was measured and rejected — DArc's PPMD is Shkarin var.H with
**Subbotin's** carryless range coder (32-bit `low`, `TOP=1<<24`, `MAX_O` 128);
`ppmd-rust` is 7-Zip's Ppmd7 with **Pavlov's** coder (64-bit `Low`, `MAX_O` 64).
Same model, different stream. Do not revisit the crate.

Two things this port established that generalise:

* **The C's compiler flags were part of the archive format.** `StateCpy`/`SWAP`
  type-pun through `(WORD&)`, so `rescale()`'s `if (p->Freq == 0)` re-reads the
  heap under `-fno-strict-aliasing` and reuses a cached value without it — with
  different compressed bytes either way. Every harness now builds its oracle
  from the codec makefile's flags via `darc_codec_cflags`. Swept across all 23
  harnesses: PPMd was the only codec sensitive to it.
* **`GlueFreeBlocks` does not always terminate**, in Shkarin's C. Absorbing a
  block clears its `NU` but leaves `Stamp` reading `~0U`, and a later block
  whose end lands on that husk loops for ever. The port breaks out, which cannot
  change any output the C can produce.

### 10. Decide explicitly whether to port 4x4 — recommendation: no, but it is NOT dead code

Threading meta-codec; its decode delegates to the library dispatcher
`Decompress()` per block, so the only portable logic is block framing
(`C_4x4.cpp:436`, call at `:237`). Its value is parallelism, which a decode-first
port drops. A Rust decode would be an FFI shim calling C `Decompress`, which
under `DARC_RUST` dispatches back to Rust drop-ins (Rust→C→Rust). The decision
stands: do not port it.

**Do not read that as "4x4 is unused".** An earlier version of this entry said
there is "no fingerprint case", which is true of the *test suite* — the `-m4x`
there is an unrelated exe preset — and was repeatedly misread as "nothing uses
this codec". It is on the DEFAULT path:

```
Compression.hs:474-481   3binary = 4x4:b8m:lzma:8m:h64m:fast:mc8
                         ...
                         9binary = 4x4:b254m:lzma:254m:max
Compression.hs:468-469   1xb = 4x4:tor:3     2xb = 4x4:tor:6
```

So `-m3` through `-m9` route the `$binary` group — usually the largest files in
an archive — through 4x4. Its C is load-bearing and must not be deleted as part
of any "prune what the port replaced" sweep.

Because it is not ported, `4x4-check.sh` asks a different question from every
other harness: 4x4's stream is its own framing wrapped around whatever the
dispatcher resolves the inner method to, and those inner codecs are now Rust.
The harness therefore builds the SAME pinned C driver twice — once over pinned C
codecs, once with `-DDARC_RUST` over the Rust staticlib — and requires identical
streams. It needs the `dropin` cargo feature, unlike the others, because it
reaches Rust through the C dispatcher rather than calling `darc_rs_*` directly.
`lzma` is excluded as an inner method: `C_LZMA.cpp` still routes to the C SDK, so
that comparison would compare a build against itself.

### 10d. LZMA: a Rust encoder exists, and it is byte-identical

`rust/darc-lzma` is a fork of `lzma-sdk-rs` (BSD-3-Clause), and
`rust/difftest/lzma-gap-check.sh` reports **100/100 streams byte-identical** to
DArc's own `lzma_compress`, 12 of those with a sliding window. So the line above
("kept pristine by decision") is now a decision about *wiring*, not about
feasibility.

Two findings changed the shape of this work:

* **`Compression/LZMA/readme` describes the wrong encoder.** Its ten "changes made"
  name identifiers that live in `Compression/LZMA/7zip/`, which
  `Compression/LZMA/makefile` references zero times. The live encoder is `7z24/`,
  essentially stock — which is why the fork's parse matched with no re-derivation.
  `7zip/` was deleted in #115.
* **The only real difference was `writeEndMark`**, one flag DArc sets and upstream
  had no field for. Adding it took the corpus from "diverges in the last 4-6 bytes"
  to byte-identical.

What blocks calling it from `C_LZMA.cpp`: BT2, BT3, HC4 and HT4 (the wrapper accepts
five match finders; the fork implements BT4), plus LZMA2 and BCJ, which have their
own wrappers. Streaming is done — `encode_stream` is O(dictionary), and
`rust/darc-codecs/src/lzma.rs` adapts the `CALLBACK_FUNC` in both directions. See
`rust/darc-lzma/PROVENANCE.md`.

### 10b. Make the silent-no-op bug class a COMPILER error -- DONE

Prompted by the Tornado presets 7-11 divergence (#102), whose cause was a trait
default that did nothing in release: `MatchFinder::update_hash1` defaulted to
`debug_assert!(false, ...)`, `Hash3` implemented it only inherently, and
`CombineMF`'s `Box<dyn MatchFinder>` call landed on the default. Every difftest
builds `--release`, so the guard never ran.

**0. No defaulted trait methods.** Zero remain crate-wide. Removing the two on
`MatchFinder` turned the omission into a compile error and immediately surfaced
two more of the same shape -- `MatchFinderN` (inherent-only) and
`CachingMatchFinder` (absent, though the C has one at `MatchFinder.cpp:462`).
`debug-assert-check.sh` now runs the codecs in a debug profile, which nothing did
before.

**1-3, #103/#104.** Crate-level lint gates added (`wildcard_enum_match_arm`,
`todo`, `unimplemented`, `mem_forget`, `unused_must_use`); the eleven `_ => {}`
arms resolved against the pinned C; the release-dead `debug_assert!(false, ...)`
bodies promoted.

#104 is the one to read for method. The eleven arms did NOT split the way the
plan assumed:

* DisPack's `flags & F_TYPE` is a **two-bit mask** (`DisPack.cpp:158`), so it has
  exactly four values and the four named arms already covered them. Three
  catch-alls were dead **by arithmetic**, provable without running anything.
* GRZip's rec mode looks identical and must stay a documented no-op: the C is
  four independent `if (Mode==n)` tests with no `else` (`Rec_Flt.c:211..:269`),
  and on decode the mode comes **from the compressed stream**. An
  `unreachable!()` there turns a corrupt archive the C tolerates into a panic
  across the FFI boundary -- hardening that adds a denial of service.
* It also found a real latent bug: `dispack/encode.rs` used `_ =>` **as** the
  F_DR arm, so a catch-all was carrying real logic, correct only because F_DR
  happened to be the fourth of four.

**The general rule that came out of it:** whether an unhandled value is
impossible or merely unusual depends on **where the value comes from**. A mask
the code computes is provable; a byte the archive supplies is attacker-controlled.
Same `match`, opposite answer.

**4. Mode bytes as types -- DONE, one PR per codec.** All five candidates landed,
and they needed **four different designs**, each forced by that rule rather than
chosen:

| codec | PR | shape | why |
|---|---|---|---|
| DisPack | #105 | exhaustive, no `Option` | two-bit mask: four values by arithmetic |
| GRZip | #106, #107 | four variants + `Option` | rec mode read from the stream |
| MM | #109 | four variants + `Option` | width from an **unvalidated** header byte |
| TTA | #110 | exhaustive, classify at the guard | width validated before the filtered path |
| Tornado | #111 | four variants + `Option` | method from the stream; STORING names no back-end |

Worth knowing per codec:

* **#106 first modelled GRZip's unknown mode as an `Unknown` variant, and that
  was wrong.** `test()` returns only 0..=4 and the encode call site filters 0, so
  the variant was unreachable on the encode path and carried a documented no-op
  body -- a can't-happen arm reintroduced by the refactor meant to delete them.
  #107 replaced it with `Option`: "not one of the four" is a **parse failure**,
  not a mode.
* **MM's `byte_size` spans 0..=32, not 1..=4.** `word_size` is a single header
  byte and nothing validates it, so unfiltered widths are ordinary input.
* **TTA's is validated** (`:545-549`, `:568`, and the encoder's fallback at
  `:1141-1143`), so the classification moved to those guards -- the one place an
  unsupported width has a real answer -- and all four catch-alls were deleted
  rather than documented.
* **Tornado's gain was the smallest, and saying so matters.** Both dispatch sites
  already handled an unknown method correctly. The enum only turns a *future*
  coder from a silent rejection into a compile error.

**4b. The Tornado encode dispatch chain -- DONE.** The 78-line, 7-arm
`if`/`else if` chain that selected the match-finder instantiation is now an
exhaustive `match` on a `Shape` enum.

The claim it "could not be converted because its order is the semantics" was
**measured and found wrong**: over all 7,776 combinations of the five parameters
it dispatches on, exactly **one** overlap exists -- the first arm's bare
`|| e == STORING` against the last arm's silence about `e`, on 21 points, all
with `encoding_method == 0`. Making that explicit (`e != STORING`) leaves the six
conditions disjoint, after which order carries no meaning.

Two sweep tests hold it, and **neither is sufficient alone** -- a sabotage probe
showed why. Deleting the `e != STORING` guard makes `conditions_are_disjoint`
fail but leaves `matches_the_original_ordered_chain` **passing**, because the
classifier is itself written as ordered `if`s and reproduces the same precedence.
Equivalence proves the behaviour; disjointness proves the ordering is not
load-bearing.

**What is deliberately NOT modelled:** `caching_finder` as a whole (an
encoder-side preset parameter feeding that cascade, never in the stream), and
GRZip's other mode bits, which are a **bitfield** -- `&`-tested, combined, passed
down to sub-blocks. That wants a bitflags type, not an enum, and is a different
risk profile.

Item 4's original framing mentioned ~1968 `as` casts as part of the same project.
They were not touched, and the count has since grown; treat casts as separate
work and **measure before quoting a number**:

```bash
rg -oE ' as (u8|u16|u32|u64|usize|i8|i16|i32|i64|isize|c_int|f32|f64)\b' \
  rust/darc-codecs/src | wc -l
```

### 10c. Totality: no `if let` in the Rust workspace -- DONE (#108)

An exhaustive `match` with every arm named is preferred over `if let`, so a branch
carrying archive or crypto behaviour has to be written down and cannot be
silently absent. 33 sites converted; the workspace has none left.

**There is no clippy lint that bans `if let`,** and the whole related family
points the other way -- `single_match` (warn by default, via `clippy::all`),
`single_match_else`, `manual_let_else`, `option_if_let_else`. `single_match` asked
by name for `grzip/block.rs`'s mode dispatch to become an `if let`. So totality is
three things together:

* `deny(clippy::wildcard_enum_match_arm)` -- no `_ =>`, every arm named
* `allow(clippy::single_match)` -- documented at both crate roots, or clippy
  fights the convention
* a CI grep step in `build.yml`, covering all of `rust/` with
  `--exclude-dir=target`

`while let` and `let ... else` are deliberately **not** covered: a loop's
termination condition and a diverging `else` already state the other case.

The conversion found one live bug: `lz4hc.rs` used `if let (true,
Strategy::Optimal(..))` on a **tuple**, followed by `else if input_size >= ...`,
so a new `Strategy` variant would have fallen silently into the hash-chain
parser. It is now a `match` over all four `(bool, Strategy)` combinations --
`(false, _)` deliberately avoided -- so a new variant fails to compile.

Two traps in writing the CI gate, both of which produce a gate that stops
measuring rather than one that fails: backticks inside double quotes are command
substitution (`` `if let` `` gets *executed*), and `--exclude-dir=target` is
load-bearing because `rust/target` holds vendored crate sources full of `if let`.
**Test a gate all four ways**: passes clean, rejects a planted `if let` in each
crate, ignores the phrase in a comment, ignores `while let`.

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
