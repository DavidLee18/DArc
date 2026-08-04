//! `arc.ini` and `$FREEARC` — the default options every command starts with.
//!
//! This is not a convenience feature. `Cmdline.hs:49-102` reads the config file
//! and the environment variable BEFORE parsing the command line and prepends
//! what it finds, so a machine with `-mx` in its `arc.ini` gets a different
//! archive from the same command line. A port that ignores them writes
//! different bytes than the reference and says nothing about it.
//!
//! Order is `config_1st_line ++ [Default options] ++ $FREEARC ++ argv`
//! (`Cmdline.hs:102`). The user's own arguments come LAST, so for every
//! last-wins option they override the defaults.
//!
//! # What is read, and what is not
//!
//! Only the default-options parts. `arc.ini` also carries a
//! `[Compression methods]` section that defines the method table itself, and
//! `[External compressor:…]` sections; neither is applied here — the port's
//! method table is built in, and it was derived from that very section. A
//! config whose method table has been EDITED would therefore be ignored, which
//! is why [`Config::has_unapplied_sections`] exists to warn rather than let it
//! pass silently.

/// The file name searched for beside the executable (`Options.hs:407`).
pub const CONFIG_FILE: &str = "arc.ini";

/// The environment variable holding default options (`Options.hs:410`).
pub const CONFIG_ENV_VAR: &str = "FREEARC";

/// A parsed `arc.ini`.
pub struct Config {
    /// Significant lines: trimmed, with blanks and `;` comments dropped.
    lines: Vec<String>,
}

impl Config {
    /// `parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]`
    /// (`Cmdline.hs:65`).
    pub fn parse(text: &str) -> Config {
        Config {
            lines: text
                .lines()
                .map(|l| l.trim().to_string())
                .filter(|l| !l.is_empty() && !l.starts_with(';'))
                .collect(),
        }
    }

    /// `configFilePlaces` (`Files.hs:224`) — beside the executable, and
    /// nowhere else on this platform.
    pub fn find() -> Option<std::path::PathBuf> {
        let exe = std::env::current_exe().ok()?;
        let path = exe.parent()?.join(CONFIG_FILE);
        match path.is_file() {
            true => Some(path),
            false => None,
        }
    }

    /// The options common to EVERY command: the first significant line, but
    /// only when it is not a section heading (`Cmdline.hs:92-94`).
    pub fn global_options(&self) -> &str {
        match self.lines.first() {
            Some(l) if !l.starts_with('[') => l,
            _ => "",
        }
    }

    /// The `[Default options]` entry for one command.
    ///
    /// `sectionElement` (`Cmdline.hs:85`): a line is `NAMES = VALUE`, the left
    /// side may list SEVERAL commands, and a command may appear on several
    /// lines — in which case every value applies, joined. Both were easy to
    /// miss and the shipped `arc.ini` documents them in its own comments.
    pub fn command_options(&self, command: &str) -> String {
        let mut out: Vec<&str> = Vec::new();
        let mut in_section = false;
        for line in &self.lines {
            if line.starts_with('[') {
                in_section = section_name(line) == "default options";
                continue;
            }
            if !in_section {
                continue;
            }
            match line.split_once('=') {
                Some((names, value)) => {
                    if names.split_whitespace().any(|n| n == command) {
                        out.push(value.trim());
                    }
                }
                None => {}
            }
        }
        out.join(" ")
    }

    /// Sections this port reads the command line from but does NOT apply.
    ///
    /// Returns the section headings found, so the caller can say which. An
    /// edited `[Compression methods]` would otherwise change what `-m9` means
    /// in the reference and not here, with nothing to show for it.
    pub fn has_unapplied_sections(&self) -> Vec<String> {
        self.lines
            .iter()
            .filter(|l| l.starts_with('['))
            .filter(|l| {
                let n = section_name(l);
                n == "compression methods" || n.starts_with("external compressor")
            })
            .cloned()
            .collect()
    }
}

/// `cleanupSectionName` — the heading without its brackets, lower-cased so
/// comparisons do not depend on how the user capitalised it.
fn section_name(heading: &str) -> String {
    heading.trim_matches(['[', ']']).trim().to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
    use super::Config;

    const SAMPLE: &str = "\
;a comment
-mx --display=hnwftsr

[Default options]
;another comment
a create = -m5
a create t e x = -di+$
x = -o+
[Compression methods]
9 = lzma:64m
";

    /// The first significant line is global options only when it is not a
    /// heading. Comments and blanks must not count as significant, or a file
    /// that opens with a comment would take the comment as its options.
    #[test]
    fn the_first_significant_line_is_the_global_options() {
        let c = Config::parse(SAMPLE);
        assert_eq!(c.global_options(), "-mx --display=hnwftsr");
        // A file that opens with a section heading has no global options.
        let c2 = Config::parse("[Default options]\na = -m1\n");
        assert_eq!(c2.global_options(), "");
    }

    /// A command may appear on several lines and beside other commands; every
    /// matching line contributes.
    #[test]
    fn command_options_merge_across_lines_and_name_lists() {
        let c = Config::parse(SAMPLE);
        assert_eq!(c.command_options("a"), "-m5 -di+$");
        assert_eq!(c.command_options("create"), "-m5 -di+$");
        assert_eq!(c.command_options("x"), "-di+$ -o+");
        assert_eq!(c.command_options("l"), "");
    }

    /// Lines outside `[Default options]` must not leak in -- the
    /// `[Compression methods]` entry `9 = lzma:64m` is not an option for
    /// command `9`.
    #[test]
    fn other_sections_do_not_contribute_options() {
        let c = Config::parse(SAMPLE);
        assert_eq!(c.command_options("9"), "");
        assert_eq!(c.has_unapplied_sections(), vec!["[Compression methods]".to_string()]);
    }
}
