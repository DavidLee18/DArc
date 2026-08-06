# DArc
Distended Arc - Based on FreeArc

## Overview

DArc is a command-line archiver based on [FreeArc](http://freearc.org). It supports solid compression, strong encryption, recovery records, SFX archives, and a wide variety of compression algorithms.

The console binary is named `darc` (`darc.exe` on Windows).  
Archives produced by DArc are largely format-compatible with [DArc86](https://github.com/YadeWira/DArc86) — but **compatibility is not a design requirement**, and where it conflicts with being correct or corruption-resilient, DArc takes the better behaviour and marks the divergence. Encrypted archives are already not compatible: they carry `:h1`, because the old key/IV hex decoding was broken and weakened the key.
---

## Building

> **Build System Overview:**
> DArc's archiver is a single Rust binary, `darc`, built with `cargo`. Clang
> still builds the C/C++ under `Compression/` (C++17), but only `unarc` and the
> SFX modules link it — the archiver reaches every codec as a Rust crate.
> There is no Haskell in the build any more; `mhs`, `cpphs` and GHC are all
> unnecessary.

### Every platform

```bash
cargo build --release --manifest-path rust/Cargo.toml -p darc-arc --bin darc
```

That is the whole archiver, at `rust/target/release/darc`. It needs nothing
outside the Rust toolchain — no ncurses, no libcurl, no system TLS.

`--original http://…` fetches a remote copy by byte range and is on by default;
`--no-default-features` removes it and its entire dependency subtree.

For Windows, cross-compile rather than building on Windows:

```bash
cargo build --release --manifest-path rust/Cargo.toml \
  --target x86_64-pc-windows-gnu      -p darc-arc --bin darc   # needs mingw-w64
cargo build --release --manifest-path rust/Cargo.toml \
  --target aarch64-pc-windows-gnullvm -p darc-arc --bin darc   # needs llvm-mingw
```

Both targets are listed in `rust-toolchain.toml`, so rustup installs them on
demand.

### The C side: `unarc` and the SFX modules (optional)

```bash
./compile-c            # generates common.mak, then builds the codec objects
make -C Unarc linux    # or `make -C Unarc windows`
```

`./compile-c` must run first even on a clean checkout that only wants `unarc`:
`common.mak` is generated rather than committed, and every makefile under
`Compression/` and `Unarc/` begins by including it.

This produces `unarc` and the SFX module `arc.linux.sfx` — both copies of the
Rust extractor, which `make` builds with `cargo`. No C++ is compiled by the
`linux` target; `make -C Unarc oracle` still builds the C++ extractor, as
`unarc-c`, for the differential harness and the sanitizer job only.

There used to be three Linux SFX tiers. `arc-mini` and `arc-tiny` linked
progressively fewer C decoders, so a smaller stub was worth shipping; the Rust
extractor is one binary carrying every codec, so the tiers could only have been
identical copies under three names.

### Troubleshooting

- **`make` fails immediately with "common.mak: No such file or directory"**:
  run `./compile-c` once. It is generated from `unix-common.mak` /
  `win32-common.mak` and is deliberately not committed.
- **Stale object files after switching build paths**: the makefiles do not
  rebuild when a `-D` changes, so remove `/tmp/out/` when switching defines.
- **`cargo` cannot reach crates.io**: the archiver has real dependencies now
  (rayon, and ureq/rustls for `--original`). `--no-default-features` drops the
  HTTP half but not the rest.

**macOS specifics.** Covered by CI (`macos-latest`) and needs only the Xcode
command line tools beyond `cargo`. Handled automatically by the C build:
- Apple's clang ships no OpenMP, so `-fopenmp`/`-lgomp` are dropped on Darwin.
  Nothing is lost — libbsc's OpenMP paths are compiled out regardless.
- `objcopy` does not exist; the 7z codec uses `ld -r -exported_symbols_list` to
  achieve the same symbol localization.
- There is no `/proc`, so physical-memory queries go through `sysctl`.

### What the archiver cannot do yet

Every compression method is supported for both reading and writing, and
`Tests/run-tests.sh` scores the same 24/24 as the pre-port reference.

`-mm` (multimedia mode), `-ma` (file-type autodetection level) and `-mc`
(disable an algorithm) are implemented and gated by
`rust/difftest/arc-multimedia-check.sh`; so are the memory limits `-lc`/`-ld`,
including their `-lc-`/`-ld-` "no limit" forms. An earlier version of this
section said all five were refused, which stopped being true in #129/#130.

A limit that cannot physically be met is now reported rather than quietly
missed — `-ld1m` on LZMA warns, because LZMA needs its dictionary *plus* a fixed
~2 MB and no dictionary satisfies 1 MB.

---
## CLI Usage

```
darc <command> [options...] <archive> [files... @listfiles...]
```

- **`<command>`** — one of the commands listed below.
- **`[options...]`** — zero or more options (each prefixed with `-`).
- **`<archive>`** — path to the archive file. The default extension `.arc` is added automatically unless `--noarcext` is used.
- **`[files...]`** — files or directories to process. Wildcards are supported. If omitted, all files are processed (`*`).
- **`[@listfiles...]`** — text files containing lists of filenames to process, one per line.

Multiple commands can be chained with `;` as a separator, for example:
```
darc "a archive -r ; t archive ; x archive"
```

---

## Commands

| Command   | Description |
|-----------|-------------|
| `a`       | Add files to archive |
| `c`       | Add comment to archive |
| `ch`      | Modify archive (recompress, encrypt, etc.) |
| `create`  | Create new archive |
| `cw`      | Write archive comment to file |
| `d`       | Delete files from archive |
| `e`       | Extract files from archive, ignoring pathnames |
| `f`       | Freshen archive (update files that are newer on disk) |
| `j`       | Join archives |
| `k`       | Lock archive |
| `l`       | List files in archive |
| `lb`      | Bare list of files in archive (filenames only) |
| `lt`      | Technical archive listing |
| `m`       | Move files and directories to archive |
| `mf`      | Move only files to archive |
| `r`       | Recover archive using recovery record |
| `rr`      | Add recovery record to archive |
| `s`       | Convert archive to SFX (self-extracting) |
| `t`       | Test archive integrity |
| `u`       | Update files in archive |
| `v`       | Verbosely list files in archive |
| `x`       | Extract files from archive (preserving paths) |

### Command Examples

```sh
# Add all files in the current directory recursively
darc a archive.arc -r .

# Extract all files from an archive
darc x archive.arc

# Extract, ignoring directory paths
darc e archive.arc

# Test archive integrity
darc t archive.arc

# List archive contents
darc l archive.arc

# Delete a file from an archive
darc d archive.arc unwanted.txt

# Add a recovery record (5% of archive size)
darc rr archive.arc -rr5%

# Recover a damaged archive
darc r archive.arc

# Convert to self-extracting archive
darc s archive.arc

# Join multiple archives
darc j output.arc part1.arc part2.arc

# Lock archive (prevent modifications)
darc k archive.arc
```

---

## Options

Options use the short form `-<opt>` or long form `--<option>`.  
Options that take a parameter use `-<opt><value>` or `--<option>=<value>`.

### General

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-y`  | `--yes`           | Answer Yes to all queries |
| `--`  |                   | Stop processing options |
| `-cfg FILE` | `--config=FILE` | Use config FILE (default: `darc.toml`) |
| `-env VAR`  |                 | Read default options from environment variable VAR (default: `DARC`) |

### File Selection

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-r`  | `--recursive`     | Recursively collect files from subdirectories |
| `-x FILESPECS` | `--exclude=FILESPECS` | Exclude matching files from operation |
| `-n FILESPECS` | `--include=FILESPECS` | Include only files matching FILESPECS |
| `-ep MODE` | `--ExcludePath=MODE` | Exclude/expand path (1, 2, or 3) |
| `-fn` | `--fullnames`     | Match filespecs against full file paths |
| `-sm SIZE` | `--SizeMore=SIZE` | Select files larger than SIZE |
| `-sl SIZE` | `--SizeLess=SIZE` | Select files smaller than SIZE |
| `-tb TIME` | `--TimeBefore=TIME` | Select files modified before TIME |
| `-ta TIME` | `--TimeAfter=TIME`  | Select files modified after TIME |
| `-tn PERIOD` | `--TimeNewer=PERIOD` | Select files newer than PERIOD |
| `-to PERIOD` | `--TimeOlder=PERIOD` | Select files older than PERIOD |

### Paths

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-ap DIR` | `--arcpath=DIR`  | Base directory inside archive |
| `-dp DIR` | `--diskpath=DIR` | Base directory on disk |
| `-ad`  | `--adddir`        | Add archive name to extraction path |
| `-w DIR` | `--workdir=DIR`  | Directory for temporary files |

### Compression

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-m METHOD` | `--method=METHOD` | Compression method (`-m0`–`-m9`, `-m1x`–`-m9x`) |
| `-dm METHOD` | `--dirmethod=METHOD` | Compression method for archive directory |
| `-md N` | `--dictionary=N` | Set compression dictionary to N MB |
| `-ms`  | `--StoreCompressed` | Store already-compressed files without recompression |
| `-mt N` | `--MultiThreaded=N` | Number of compression threads |
| `-mc`  |                   | Disable specific compression algorithms (e.g., `-mcd-`, `-mc-rep`) |
| `-mm MODE` | `--multimedia=MODE` | Multimedia compression mode |
| `-ma LEVEL` |               | File-type auto-detection level (0–9, `+`, `-`) |
| `-mx`  |                   | Maximum internal compression mode |
| `-max` |                   | Maximum compression (uses external tools: precomp, ecm, ppmonstr) |
| `-s GROUPING` | `--solid=GROUPING` | Solid compression grouping |
| `-ds ORDER` | `--sort=ORDER`   | Sort files in ORDER before compressing |
| `--groups=FILE` |            | Name of file-groups definition file |
| `-lc N` | `--LimitCompMem=N` | Limit memory for compression to N MB |
| `-ld N` | `--LimitDecompMem=N` | Limit memory for decompression to N MB |

#### Compression Levels

| Option | Description |
|--------|-------------|
| `-m0`  | No compression (store only) |
| `-m1`–`-m9` | Compression levels 1–9 (increasing compression/time) |
| `-m1x`–`-m9x` | Extra-mode compression at levels 1–9 |
| `-mx` or `-max` | Maximum compression |

#### Solid Grouping Values (`-s`)

| Value | Description |
|-------|-------------|
| _(empty)_ | All files in one solid block |
| `-` | No solid compression |
| `e` | Group by file extension |
| `s<size>` | Group by block size |

### Encryption

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-p PASSWORD` | `--password=PASSWORD` | Encrypt/decrypt data with PASSWORD |
| `-hp PASSWORD` | `--HeadersPassword=PASSWORD` | Encrypt/decrypt archive headers and data |
| `-ae ALGO` | `--encryption=ALGO` | Encryption algorithm: `aes` (default), `blowfish`, `serpent`, `twofish` |
| `-kf FILE` | `--keyfile=FILE`  | Encrypt/decrypt using KEYFILE |
| `-op PASSWORD` | `--OldPassword=PASSWORD` | Old password used only for decryption |
| `-okf FILE` | `--OldKeyfile=FILE` | Old keyfile used only for decryption |

### Archive Management

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-f`  | `--freshen`       | Update only files that are newer on disk |
| `-u`  | `--update`        | Update only files not present or newer on disk |
| `--sync` |                | Synchronize archive and disk contents |
| `-o MODE` | `--overwrite=MODE` | Overwrite mode: `+` (always), `-` (never), `p` (prompt) |
| `-k`  | `--lock`          | Lock archive to prevent modifications |
| `-rr SIZE` | `--recovery=SIZE` | Add recovery information of SIZE to archive (`-rr`/`-rr+` reuse the archive's own setting, or a recommended amount if it had none) |
| `-sfx MODULE` |            | Add SFX module (`freearc.sfx` by default) |
| `--noarcext` |              | Do not add the default `.arc` extension to archive name |
| `-ag FMT` | `--autogenerate=FMT` | Autogenerate archive name using a time format string |
| `--recompress` |            | Force recompression of all files |
| `--append` |                | Add new files to the end of archive only |
| `-z FILE` | `--arccmt=FILE` | Read archive comment from FILE or stdin |
| `--archive-comment=TEXT` |  | Specify archive comment directly on the command line |
| `-t`  | `--test`          | Test archive integrity after archiving |
| `-tp MODE` | `--pretest=MODE` | Test archive before operation (0=none, 1=recovery only, 2=recovery or full, 3=full) |
| `-d`  | `--delete`        | Delete files and directories after successful archiving |
| `-df` | `--delfiles`      | Delete only files after successful archiving |
| `-kb` | `--keepbroken`    | Keep broken extracted files |
| `-ba MODE` | `--BrokenArchive=MODE` | Handle badly broken archives (`-`, `0`, or `1`) |
| `-tk` | `--keeptime`      | Keep original archive modification time |
| `-tl` | `--timetolast`    | Set archive time to the latest file's modification time |
| `--dirs` |                  | Add empty directories to archive |
| `-ed` | `--nodirs`        | Do not add empty directories to archive |

### Windows-Only Options

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-ac` | `--ClearArchiveBit` | Clear Archive attribute on successfully (de)archived files |
| `-ao` | `--SelectArchiveBit` | Select only files with Archive attribute set |

### Display and Logging

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-i TYPE` | `--indicator=TYPE` | Progress indicator type: `0` (none), `1` (default), `2` (per-file) |
| `-di AMOUNT` | `--display=AMOUNT` | Control amount of information displayed |
| `--logfile=FILE` |          | Duplicate all output to FILE |
| `--print-config` |          | Display built-in compression method definitions |

### Network/URL Options

| Long              | Description |
|-------------------|-------------|
| `--proxy=PROXY`   | Set proxy server(s) for URL access |
| `--bypass=LIST`   | Set proxy bypass list for URL access |
| `--original=URL`  | Re-download broken archive parts from URL — a local path, an `http(s)://` URL fetched by byte range, `?CMD` to run `CMD <archive>` for the URL, or empty to read it from `files.bbs`/`descript.ion` |
| `--save-bad-ranges=FILE` | Save list of broken archive parts to FILE |
| `--cache=N`       | Use N MB for read-ahead cache |

### Reproducibility and Advanced

| Short | Long              | Description |
|-------|-------------------|-------------|
|       | `--nodates`       | Don't store file timestamps in the archive. Makes archive bytes reproducible for a given input |
|       | `--create-in-workdir` | Create the archive in the work directory, then move it to its final location |
|       | `--queue`         | Serialize operations across multiple concurrent DArc processes |
| `-ioff` | `--shutdown`    | Shut the computer down when the operation completes |
|       | `--pause-before-exit` | Pause just before closing the program window |
|       | `--nodata`        | Don't store file data in the archive (directory only) |
|       | `--crconly`       | Save and check CRCs, but don't store file data |

### Charset

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-sc CHARSETS` | `--charset=CHARSETS` | Character sets for list files and comment files |
| `--language=FILE` |          | Load localization strings from FILE |

---

## Configuration File (`darc.toml`)

By default, DArc reads options from `darc.toml` beside the executable. You can override the config file with `-cfg <file>` or disable it with `-cfg-`.

**Default options** are set per-command under `[defaults]`. A key may name several commands, and `all` applies to every one:

```toml
[defaults]
all = "-mx"
a   = "--display"
ch  = "-m4x -ms"
```

**Compression methods** are defined under `[methods]`, and external compressors under `[external.NAME]` — that is how `-msrep` is wired up.

The `DARC` environment variable is also read for default options (override with `-env <VAR>` or disable with `-env-`).

> **`arc.ini` and `$FREEARC` are gone, and a leftover one is refused rather than ignored.** They were FreeArc's names; DArc uses `darc.toml` and `$DARC`. If an `arc.ini` is found beside the executable the run stops and names the replacement, because a config that is silently skipped changes the archive you get — `-cfg-`/`-env-` silence it deliberately. The old `[Default options]` / `[Compression methods]` INI sections have no equivalent spelling; use the TOML above.

---

## List Files

You can pass a file containing a list of filenames (one per line) to any command by prefixing the filename with `@`:

```sh
darc a archive.arc @myfiles.txt
```

---

## Examples

```sh
# Create archive with maximum compression
darc a -mx myarchive.arc documents/

# Create encrypted archive
darc a -p"my secret" secure.arc private/

# Extract archive to a specific directory
darc x archive.arc -dp /home/user/extracted/

# Add recovery record (10% of archive size)
darc ch myarchive.arc -rr10%

# List archive contents verbosely
darc v myarchive.arc

# Update archive with changed files
darc u myarchive.arc documents/

# Create self-extracting archive
darc s myarchive.arc

# Freshen archive, then test it
darc a archive.arc -r src/ -t

# Compress with specific algorithm and dictionary size
darc a -m4 -md128m myarchive.arc bigfiles/

# Exclude certain file types
darc a myarchive.arc docs/ -x"*.tmp" -x"*.log"
```

---

## License

DArc is distributed under the **GNU General Public License, version 3 or later**
— see [LICENSE](LICENSE).

It was GPLv2 until the `.7z` reader moved to the `sevenz-rust2` crate, which is
Apache-2.0; Apache-2.0 is compatible with GPLv3 and not with GPLv2. Bundled and
depended-on components carry their own terms — see
[THIRD-PARTY.md](THIRD-PARTY.md).
