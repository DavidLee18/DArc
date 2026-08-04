//! `[external.NAME]` — compressors DArc shells out to (`C_External.cpp`).
//!
//! An external compressor is a program that reads one file and writes another.
//! DArc spills the block to a temporary file, runs the command, and reads the
//! result back. `srep` is the reason this exists: it is how the reference
//! provides `-msrep`, and without it that method cannot work here either — the
//! reference cannot write it without an `srep` binary either.
//!
//! # What is and is not archive-visible
//!
//! The **command line is not**. An archive records the method name and its
//! parameters — `srep:m3` — and nothing about how the bytes were produced. So
//! the spelling of this config section is a free choice in a way almost
//! nothing else in this port is, and `$in`/`$out` are used here where
//! `C_External.cpp` uses `$$arcdatafile$$.tmp` and `$$arcpackedfile$$.tmp`.
//!
//! What IS archive-visible is the output of the external program. Two machines
//! with different `srep` versions write different archives from the same
//! command line, and neither this port nor the reference can do anything about
//! that. It is the one place where an archive's contents depend on software
//! outside the project.

use std::collections::BTreeMap;

/// A registered external compressor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct External {
    /// The command that compresses. `$in` is the source file, `$out` the
    /// destination.
    pub packcmd: String,
    /// The command that decompresses, with the same two placeholders.
    pub unpackcmd: String,
}

/// The table, installed once at startup from `darc.toml`.
///
/// A global for the same reason the method table is one: the compressor is
/// reached from deep inside block writing, and the reference likewise
/// registers these before any command runs (`Cmdline.hs:223`).
static TABLE: std::sync::OnceLock<BTreeMap<String, External>> = std::sync::OnceLock::new();

pub fn set_table(table: BTreeMap<String, External>) {
    drop(TABLE.set(table));
}

/// The external for a method string, if one is registered.
///
/// The lookup key is the name before the first `:`, so `srep:m3` finds the
/// `srep` section and the parameters are passed through to the command
/// untouched — the same split `parse_EXTERNAL` makes.
pub fn lookup(method: &str) -> Option<&'static External> {
    let name = method.split(':').next().unwrap_or(method);
    TABLE.get()?.get(name)
}

/// Is this method name handled by an external compressor?
pub fn is_external(method: &str) -> bool {
    lookup(method).is_some()
}

/// Run `packcmd` over `src`.
pub fn compress(method: &str, src: &[u8]) -> Result<Vec<u8>, String> {
    let ext = lookup(method).ok_or_else(|| format!("{method}: no external compressor"))?;
    run(method, &ext.packcmd, src)
}

/// Run `unpackcmd` over `src`.
pub fn decompress(method: &str, src: &[u8]) -> Result<Vec<u8>, String> {
    let ext = lookup(method).ok_or_else(|| format!("{method}: no external compressor"))?;
    run(method, &ext.unpackcmd, src)
}

/// Spill to a file, run the command, read the result.
///
/// Errors are returned rather than ignored at every step. A failed external
/// compressor that returned an empty output would otherwise be written into
/// the archive as a perfectly valid empty block.
fn run(method: &str, cmd: &str, src: &[u8]) -> Result<Vec<u8>, String> {
    if cmd.is_empty() {
        return Err(format!("{method}: the command for this direction is empty"));
    }
    let dir = TempDir::new(method)?;
    let infile = dir.path.join("in");
    let outfile = dir.path.join("out");
    std::fs::write(&infile, src).map_err(|e| format!("{method}: writing {}: {e}", infile.display()))?;

    let line = substitute(cmd, &infile, &outfile);
    let status = spawn(&line).map_err(|e| format!("{method}: running `{line}`: {e}"))?;
    if !status.success() {
        return Err(format!("{method}: `{line}` exited with {status}"));
    }
    // A command that succeeds without producing its output file has not done
    // the job, whatever it printed.
    std::fs::read(&outfile)
        .map_err(|e| format!("{method}: `{line}` wrote no output to {}: {e}", outfile.display()))
}

/// `$in` and `$out`, quoted, so a temp path containing a space survives the
/// shell. Longest placeholder first is not needed — neither is a prefix of the
/// other — but the order is fixed so the result cannot depend on it.
fn substitute(cmd: &str, infile: &std::path::Path, outfile: &std::path::Path) -> String {
    cmd.replace("$in", &quote(infile)).replace("$out", &quote(outfile))
}

#[cfg(not(windows))]
fn quote(p: &std::path::Path) -> String {
    // Single quotes, with any embedded single quote closed and reopened. This
    // is the only form that is safe for every other character a path may hold.
    format!("'{}'", p.display().to_string().replace('\'', r"'\''"))
}

#[cfg(windows)]
fn quote(p: &std::path::Path) -> String {
    format!("\"{}\"", p.display())
}

#[cfg(not(windows))]
fn spawn(line: &str) -> std::io::Result<std::process::ExitStatus> {
    std::process::Command::new("sh").arg("-c").arg(line).status()
}

#[cfg(windows)]
fn spawn(line: &str) -> std::io::Result<std::process::ExitStatus> {
    std::process::Command::new("cmd").arg("/C").arg(line).status()
}

/// A temporary directory that removes itself.
///
/// Named from the process id and a counter rather than randomness: the port
/// has no RNG dependency and does not need one here, and a collision within
/// one process is what the counter rules out.
struct TempDir {
    path: std::path::PathBuf,
}

impl TempDir {
    fn new(method: &str) -> Result<TempDir, String> {
        static N: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let n = N.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!("darc-ext-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&path)
            .map_err(|e| format!("{method}: creating {}: {e}", path.display()))?;
        Ok(TempDir { path })
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        drop(std::fs::remove_dir_all(&self.path));
    }
}

#[cfg(test)]
mod tests {
    //! These set a process-global OnceLock, so they only hold when each test
    //! runs in its OWN process. That is what `cargo nextest run` does and what
    //! `cargo test` does not -- under the latter the second `set_table` here
    //! would be silently ignored and the test would assert against the first
    //! table.

    use super::{External, compress, decompress, is_external, set_table, substitute};
    use std::collections::BTreeMap;
    use std::path::Path;

    /// A path with a space must survive the shell, or an external compressor
    /// works for one user and not for another purely by where their temp
    /// directory is.
    #[test]
    fn paths_are_quoted() {
        let line = substitute("prog $in $out", Path::new("/tmp/a b/in"), Path::new("/tmp/a b/out"));
        #[cfg(not(windows))]
        assert_eq!(line, "prog '/tmp/a b/in' '/tmp/a b/out'");
        #[cfg(windows)]
        assert_eq!(line, "prog \"/tmp/a b/in\" \"/tmp/a b/out\"");
    }

    /// The registry keys on the name before ':', so parameters reach the
    /// command rather than defeating the lookup.
    #[test]
    fn a_parameterised_method_finds_its_section() {
        let mut t = BTreeMap::new();
        t.insert(
            "fakezip".to_string(),
            External {
                // `cat` is a compressor that compresses nothing, which is all
                // this needs to prove the plumbing.
                packcmd: "cat $in > $out".to_string(),
                unpackcmd: "cat $in > $out".to_string(),
            },
        );
        set_table(t);
        assert!(is_external("fakezip"));
        assert!(is_external("fakezip:m3"), "parameters defeated the lookup");
        assert!(!is_external("srep"));

        let round = compress("fakezip:m3", b"hello world").expect("packs");
        assert_eq!(round, b"hello world");
        assert_eq!(decompress("fakezip", &round).expect("unpacks"), b"hello world");
    }

    /// A command that fails must not be mistaken for one that produced
    /// nothing: an empty block is a perfectly valid archive entry, so a silent
    /// failure here is data loss.
    #[test]
    fn a_failing_command_is_an_error_not_an_empty_block() {
        let mut t = BTreeMap::new();
        t.insert(
            "brokenzip".to_string(),
            External {
                packcmd: "exit 3".to_string(),
                unpackcmd: "true".to_string(),
            },
        );
        set_table(t);
        let err = compress("brokenzip", b"data").expect_err("must fail");
        assert!(err.contains("brokenzip"), "unhelpful: {err}");
        // Succeeded but wrote no output file -- also an error.
        let err2 = decompress("brokenzip", b"data").expect_err("must fail");
        assert!(err2.contains("no output"), "unhelpful: {err2}");
    }
}
