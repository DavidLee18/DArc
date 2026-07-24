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

Keep these separate when reporting progress. **Nothing has been deleted from
the repository yet.** Every `#ifndef DARC_RUST` block and every vendored tree is
still present, because the stock build is still the default and uses all of it.

| codec | Rust module | wired under `DARC_RUST` | C pruned |
|---|---|---|---|
| BSC | `bsc/` | yes | no |
| Delta | `delta` | yes (both directions) | no |
| Dict | `dict`, `dict_encode` | yes (both directions) | no |
| DisPack | `dispack` | yes | no |
| GRZip | `grzip` | yes | no |
| LZ4 | `lz4` (`lz4_flex`, pure Rust) | yes (decode + default encode) | no |
| LZP | `lzp` | yes (both directions) | no |
| MM | `mm` | yes | no |
| REP | `rep` | yes (both directions, byte-exact) | no |
| SREP | `srep` | external binary, no `DARC_RUST` wiring | no |
| Tornado | `tornado` | yes | no |
| TTA | `tta` | yes | no |
| zstd | `zstd` (`zstd-safe` binding) | yes | no |
| Encryption | `darc-crypto` | yes | no |

**Not ported at all:** PPMD (1,065 lines), 4x4 (700), LZMA (25,385 — stays on
the 7-Zip SDK), and the Haskell layer (17,843).

**zstd is a binding, not a port.** `zstd-safe`/`zstd-sys` compiles the same C,
fetched by cargo instead of vendored. The value is 2.2 MB leaving the repo and
maintenance moving upstream — that is the accepted rationale; do not re-litigate
it. LZ4 by contrast is genuinely Rust.

### Build paths and Rust support

| path | targets | Rust support |
|---|---|---|
| `./compile` | linux-amd64, linux-arm64, macos-arm64 | yes |
| `compile-mhs-win64` | windows-amd64, windows-arm64 | yes, both green |
| `compile-ghc`, `compile-ghc-win64` | *removed* | deleted; the Haskell layer goes to Rust |

### Branch state

`main` = `588522d` (PR #66 merged: LZ4 + zstd wired, the Rust codecs
cross-compiling for both Windows targets, every action SHA-pinned, the Rust
toolchain pinned, CI caching). Post-merge CI green, 12/12.

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

### 4. Flip `DARC_RUST` to the default, then prune

In dependency order once the gate above is open:

- **`Compression/Zstd`** (2.2 MB) — the one tree with no blocking gap. The
  wrapper no longer calls it; the makefile still *compiles* it, so that must
  stop first.
- **Delta, Dict, REP** — the three codecs ported in both directions, so whole
  files go (`Delta.cpp`, `dict.cpp`, `rep.cpp`).
- Everything else is decode-only, so its encoder keeps the file alive; pruning
  there is surgical, not file deletion.
- **Leave vendored trees pristine** (libbsc, LZMA SDK) per `CLAUDE.md`, or make
  that an explicit, recorded exception.

Two mechanical prerequisites, neither done:

- **Stop `Compression/Zstd/makefile` compiling libzstd.** The wrapper no longer
  calls it, but the makefile still builds the whole tree into `C_Zstd.o`, so the
  directory cannot simply be deleted.
- **Pin the difftest harnesses to a git revision** (item 5) before any C decoder
  goes.

### 5. Preserve the differential-test oracle

11 harnesses and 16 `_ref`/`_ccodec` shims compile the **C** codec as the
reference Rust is compared against. Deleting the C decoders destroys the only
thing that can prove Rust ≡ C. Decision taken: **pin the harnesses to a git
revision** (build the C reference from `git show <sha>:path`) rather than
keeping a second copy in the tree.

### 6. Port `lz4hc.c` to Rust — blocks pruning `Compression/LZ4`

`lz4_flex` has no high-compression mode, and `lz4hc.c` does `#include "lz4.c"`
for shared code (`lz4hc.c:56-66`), so the two files are a unit: **`lz4.c` cannot
be deleted while HC is kept**, and all 292 KB is all-or-nothing. Decided: port
HC rather than drop it.

LZ4-HC is **encoder-only** and emits standard LZ4 blocks — the Rust decoder
already reads HC-produced archives. So this is purely about preserving
compression ratio when creating archives (66,063 vs 71,029 bytes on the test
corpus, ~7.5%). Also needed: `LZ4_compressBound` (trivial formula,
`C_LZ4.cpp:66`).

### 7. Hand-port PPMD (1,065 lines)

The last real hand-portable codec besides 4x4. **No crate path:** `ppmd-rust`
was measured and rejected — DArc's PPMD is Shkarin var.H with **Subbotin's**
carryless range coder (32-bit `low`, `TOP=1<<24`, `MAX_O` 128); `ppmd-rust` is
7-Zip's Ppmd7 with **Pavlov's** coder (64-bit `Low`, `MAX_O` 64). Same model,
different stream. Do not revisit the crate. `-mppmd` already has a fingerprint
case.

### 8. Decide explicitly whether to port 4x4 — recommendation: no

Threading meta-codec; its decode delegates to the library dispatcher
`Decompress()` per block, so the only portable logic is block framing
(`C_4x4.cpp:436`, call at `:237`). Its value is parallelism, which a decode-first
port drops. There is **no fingerprint case** (the suite's `-m4x` is an unrelated
exe preset, not this codec). A Rust decode would be an FFI shim calling C
`Decompress`, which under `DARC_RUST` dispatches back to Rust drop-ins
(Rust→C→Rust). Record the decision so it is not re-litigated.

### 9. Port the Haskell application layer (17,843 lines, 41 files)

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

### 10. Performance: measure before optimising

Several Rust ports are deliberately scalar where the C is vectorised: BSC's LZP
and adler32, the QLFC SIMD variants, and the BSC fast coder's SIMD MTF shuffles.
**Measure first** — LZP is one of four decode stages and may not be the
bottleneck. Scalar is a *shipped configuration* of libbsc (i386,
`-DLIBBSC_NO_UNALIGNED_ACCESS`), not a subset, so correctness is not at issue.

### 11. Deferred C-side bugs (both verified still present)

- **ARM64 `ulong32` miscompilation.** `tomcrypt_macros.h:13` types `ulong32` as
  `unsigned` only for `__x86_64__`/sparc64 and `unsigned long` otherwise — 64-bit
  on ARM64 LP64. `serpent.c`'s key expansion rotates with a raw
  `(lk<<11)|(lk>>21)`, a rotate at 32 bits and garbage at 64. **`-ae serpent` is
  broken on the shipped v2.0.0/v2.1.0 linux-arm64 and macos-arm64 binaries**
  (no data loss — the same binary round-trips — but those archives do not move
  between architectures). Still unchecked: whether AES/Twofish/Blowfish/HMAC/
  PBKDF2 are hit too. The Rust crypto port is correct and therefore *disagrees*
  with a C ARM64 build until this is fixed; `Tests/enc-roundtrip.sh` shows it.
- **`LTC_NO_TEST`** (`C_Encryption.cpp:77`) compiles out the cipher self-tests,
  which ship with correct vectors and would have caught the above on the first
  ARM64 build. Re-enable at least in CI (it will fail until the bug is fixed).

### 12. Build/quality odds and ends

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
