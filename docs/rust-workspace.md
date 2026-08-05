# The Rust workspace

Load this when adding a crate, touching the FFI boundary, or debugging a link failure.

## Crates and why they are separate

`rust/` is a cargo workspace. Members are separate crates for link reasons, not
taste:

| crate | lines | linked by |
|---|---|---|
| `darc-codecs` | 33,700 | `arc` **and** `Unarc`/every SFX module |
| `darc-lzma` | 13,200 | via `darc-codecs` |
| `darc-crypto` | 1,350 | `arc` |
| `darc-sevenz` | 570 | `arc` only |

**Why `darc-sevenz` is not a module inside `darc-codecs`:** Unarc and every SFX
module link `libdarc_codecs.a`, and none of them can open a `.7z`. Putting it
there would grow every self-extracting archive; feature-gating it instead would
make `compile` and `Unarc/makefile` disagree about features through the shared
`rust/target` and thrash rebuilds. Weigh that before adding a member.

**Link order is load-bearing, and it has bitten twice.** GNU ld resolves a static
archive only against undefined symbols it has *already* seen; macOS ld rescans. So
a staticlib placed before its callers links locally and fails on Linux and mingw.
Both instances are worth recognising:

* a library listed before the sources in a difftest script (three had never linked);
* symbols referenced **only** by mhs-generated C, which lands after every `-optl`
  argument — `compile` injects `-Wl,--undefined=` / `-Wl,-u,_` before the archive
  for the `darc_7z_*` exports. `--whole-archive` is the wrong fix: it would force
  in the copy of Rust `std` bundled in *every* staticlib.

**Lint gates CI enforces**, so match them or the build goes red:

* **Every enum `match` names its arms** — `wildcard_enum_match_arm`, denied on
  the clippy command line so it binds the whole workspace and not just the
  crates carrying the attribute. This is the only style rule CI enforces.
  `if let` and `let _` were also banned, by grep; both bans were dropped at the
  owner's direction, because totality comes from forbidding `_ =>` and not from
  the shape of a single-branch conditional.
* `deny(clippy::unwrap_used, clippy::expect_used)` outside tests.
* `overflow-checks = true` in the release profile, so unchecked arithmetic
  *traps* rather than wrapping.
* Every `c_int`-returning `extern "C"` export is wrapped in a `catch_unwind`
  firewall (`ffi::guard`) — unwinding across the C ABI is UB, and these frames are
  reached from `unarc` and the SFX modules on archive input an attacker wrote.
* Allocations sized from archive data go through `ffi::archive_sized_buffer`,
  which caps against the method's block size and uses `try_reserve`.
