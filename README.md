# DArc
Distended Arc - Based on FreeArc

## Overview

DArc is a command-line (and optional GUI) archiver based on [FreeArc](http://freearc.org). It supports solid compression, strong encryption, recovery records, SFX archives, and a wide variety of compression algorithms.

The console binary is named `arc` (Unix) or `Arc.exe` (Windows).  
The optional GUI binary is named `freearc` (Unix) or `FreeArc.exe` (Windows).

---

## Building

### On Windows

1. Install GHC 6.6.1 or 6.8.2 bundled with a C++ compiler.
2. Install it into `C:\Base\Compiler\ghc`.
3. Make sure `make.exe` is available on your `PATH`.
4. Install [Gtk2Hs](http://sourceforge.net/project/showfiles.php?group_id=49207&package_id=42440) (required for the GUI version only).
5. Install HsLua:
   ```
   cd HsLua
   make
   cd ..
   ```
6. Compile the console version (`Arc.exe`):
   ```
   compile-O2.cmd
   ```
7. Compile the GUI version (`FreeArc.exe`):
   ```
   compile-GUI-O2.cmd
   ```
8. The compiled binaries are placed in the `Tests\` subdirectory.
9. To compile SFX modules, Unarc, and the FAR plugin:
   ```
   cd Unarc
   make
   ```

### On Unix (tested on Fedora 7)

1. Install GHC 6.6.1 or 6.8.2, GCC, `make`, and [Gtk2Hs](http://darcs.haskell.org/gtk2hs/download).
2. Make compile scripts executable:
   ```
   chmod +x compile*
   ```
3. Install HsLua:
   ```
   cd HsLua && ghc --make Setup.hs && ./Setup configure && ./Setup build && ./Setup install && cd ..
   ```
4. Compile the console version (`arc`):
   ```
   ./compile-O2
   ```
5. Compile the GUI version (`freearc`):
   ```
   ./compile-GUI-O2
   ```
6. The compiled binaries are placed in the `Tests/` subdirectory.
7. To compile SFX modules and Unarc:
   ```
   cd Unarc
   make linux
   ```

---

## CLI Usage

```
arc <command> [options...] <archive> [files... @listfiles...]
```

- **`<command>`** — one of the commands listed below.
- **`[options...]`** — zero or more options (each prefixed with `-`).
- **`<archive>`** — path to the archive file. The default extension `.arc` is added automatically unless `--noarcext` is used.
- **`[files...]`** — files or directories to process. Wildcards are supported. If omitted, all files are processed (`*`).
- **`[@listfiles...]`** — text files containing lists of filenames to process, one per line.

Multiple commands can be chained with `;` as a separator, for example:
```
arc "a archive -r ; t archive ; x archive"
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
arc a archive.arc -r .

# Extract all files from an archive
arc x archive.arc

# Extract, ignoring directory paths
arc e archive.arc

# Test archive integrity
arc t archive.arc

# List archive contents
arc l archive.arc

# Delete a file from an archive
arc d archive.arc unwanted.txt

# Add a recovery record (5% of archive size)
arc rr archive.arc -rr5%

# Recover a damaged archive
arc r archive.arc

# Convert to self-extracting archive
arc s archive.arc

# Join multiple archives
arc j output.arc part1.arc part2.arc

# Lock archive (prevent modifications)
arc k archive.arc
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
| `-cfg FILE` | `--config=FILE` | Use config FILE (default: `arc.ini`) |
| `-env VAR`  |                 | Read default options from environment variable VAR (default: `FREEARC`) |

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
| `-rr SIZE` | `--recovery=SIZE` | Add recovery information of SIZE to archive |
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
| `--original=URL`  | Re-download broken archive parts from URL |
| `--save-bad-ranges=FILE` | Save list of broken archive parts to FILE |
| `--cache=N`       | Use N MB for read-ahead cache |

### Charset

| Short | Long              | Description |
|-------|-------------------|-------------|
| `-sc CHARSETS` | `--charset=CHARSETS` | Character sets for list files and comment files |
| `--language=FILE` |          | Load localization strings from FILE |

---

## Configuration File (`arc.ini`)

By default, DArc reads options from `arc.ini` in standard search paths. You can override the config file with `-cfg <file>` or disable it with `-cfg-`.

**Default options** can be set per-command in the `[Default options]` section:

```ini
[Default options]
a = --display
ch = -m4x -ms
```

**Compression methods** can be defined in the `[Compression methods]` section.

The `FREEARC` environment variable is also read for default options (override with `-env <VAR>` or disable with `-env-`).

---

## List Files

You can pass a file containing a list of filenames (one per line) to any command by prefixing the filename with `@`:

```sh
arc a archive.arc @myfiles.txt
```

---

## Examples

```sh
# Create archive with maximum compression
arc a -mx myarchive.arc documents/

# Create encrypted archive
arc a -p"my secret" secure.arc private/

# Extract archive to a specific directory
arc x archive.arc -dp /home/user/extracted/

# Add recovery record (10% of archive size)
arc ch myarchive.arc -rr10%

# List archive contents verbosely
arc v myarchive.arc

# Update archive with changed files
arc u myarchive.arc documents/

# Create self-extracting archive
arc s myarchive.arc

# Freshen archive, then test it
arc a archive.arc -r src/ -t

# Compress with specific algorithm and dictionary size
arc a -m4 -md128m myarchive.arc bigfiles/

# Exclude certain file types
arc a myarchive.arc docs/ -x"*.tmp" -x"*.log"
```

---

## License

See [LICENSE](LICENSE) for details.
