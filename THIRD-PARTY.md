# Third-party components

DArc itself is GPLv3-or-later (see [LICENSE](LICENSE)). This file records the
components that arrive under different terms and what those terms require of a
distributor. It covers the dependencies whose licence imposes an obligation
beyond attribution; permissive MIT/BSD components are listed for completeness
but ask nothing except that their notices survive.

## Why the project is GPLv3 and not GPLv2

The `.7z` reader is the [`sevenz-rust2`](https://crates.io/crates/sevenz-rust2)
crate, which is **Apache-2.0**. Apache-2.0 is one-way compatible with GPLv3 and
is *incompatible* with GPLv2-only, so a build that links it can only be
conveyed under GPLv3 or later. That is the whole reason for the change; nothing
else about the project required it.

Before this, `LICENSE` held the bare GPLv2 text. It was added in DArc's own
initial commit rather than inherited — FreeArc 0.67's source distribution
carries no licence file and no per-file licence headers, and neither does
Bulat Ziganshin's successor repository.

## Apache-2.0 components

The Apache licence text is bundled at
[`LICENSES/Apache-2.0.txt`](LICENSES/Apache-2.0.txt). Section 4 requires that
recipients of the work — including recipients of a **binary** release — get a
copy of it, so release artifacts must ship that file.

| Component | Role |
|---|---|
| `sevenz-rust2` | the `.7z` container reader behind `darc_7z_*` |
| `lzma-rust2` | LZMA/LZMA2 decoding *inside* `.7z` streams (transitive) |

None of these ships a `NOTICE` file, so the propagation duty in Apache-2.0
§4(d) does not attach. Checked, not assumed.

Note that `lzma-rust2` decodes only the LZMA embedded in `.7z` containers. It is
**not** DArc's LZMA: `-mlzma` archives go through `rust/darc-lzma`, DArc's own
port, which is byte-identical to the 7-Zip SDK it replaced.

## Permissive components

Attribution only; no source-provision or notice-propagation duty.

| Component | Licence |
|---|---|
| `ppmd-rust` | CC0-1.0 OR MIT-0 |
| `crc32fast` | MIT OR Apache-2.0 |
| `lz4_flex` | MIT |
| `zstd-safe` / `zstd-sys` | BSD-3-Clause (bundles Facebook's zstd) |
| RustCrypto (`aes`, `sha2`, `hmac`, `pbkdf2`, `blowfish`, `serpent`, `twofish`, `cipher`) | MIT OR Apache-2.0 |
| 7-Zip SDK derivation in `rust/darc-lzma` | BSD-3-Clause — see `rust/darc-lzma/PROVENANCE.md` |
| `toml`, `toml_parser`, `toml_datetime`, `serde_spanned` | MIT OR Apache-2.0 |
| `serde`, `serde_core`, `serde_derive`, `syn` | MIT OR Apache-2.0 |
| `winnow` | MIT |

`toml` and `serde` arrived with `darc.toml`, which replaced `arc.ini`. Nine
crates are new — the four `toml`, the four `serde`, and `winnow`, the parser
combinator `toml_parser` is built on. `proc-macro2`, `quote` and
`unicode-ident` were already in the tree, so no new Unicode-3.0 obligation
comes with them. All are permissive and one-way compatible with GPLv3.

## The HTTP client, for `--original`

Pulled in by `darc-arc`'s `url` feature (on by default) so `arc r --original
http://…` can repair an archive from a remote copy. Turning the feature off
drops all of it, the way `-DFREEARC_NOURL` drops the C's libcurl path.

| Component | Licence |
|---|---|
| `ureq`, `ureq-proto` | MIT OR Apache-2.0 |
| `rustls` | Apache-2.0 OR ISC OR MIT |
| `rustls-webpki` | ISC |
| `rustls-pki-types` | MIT OR Apache-2.0 |
| `ring` | Apache-2.0 AND ISC |
| `webpki-roots` | **CDLA-Permissive-2.0** |

`webpki-roots` is the odd one and is worth stating plainly: it is Mozilla's CA
root *data*, not code, under the Community Data License Agreement — Permissive
2.0. That licence attaches no conditions to sharing the data and no
notice-propagation duty, so it is compatible with conveying this project under
GPLv3. It is called out because CDLA is not one of the licences the rest of this
file deals in, not because it is a problem.

`ring` carries an OpenSSL-derived permissive notice alongside ISC; both are
GPL-compatible and neither is copyleft.

TLS here is rustls with the roots **compiled in** — no `native-tls`, no
`platform-verifier`. That is a deliberate choice rather than a default: it keeps
the mingw and aarch64 cross-builds free of any system TLS library or trust
store, which is the whole reason this is not libcurl.

## Copyleft components vendored in-tree

Both predate this change and both grant "or later", which is what allows the
project as a whole to be conveyed under GPLv3:

| Component | Licence |
|---|---|
| `Compression/MM` (TTA) | GPL-2.0-**or-later** |
| `Compression/GRZip` | LGPL-2.1-**or-later** |

## Not audited here

`srep/` is a vendored third-party tree with its own upstream
terms; they were not reviewed as part of the `.7z` change and are listed so
their absence from the tables above is not read as a clean bill of health.
