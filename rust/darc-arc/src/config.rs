//! `darc.toml` and `$DARC` — the defaults every command starts with, the
//! compression method table, and the external compressors.
//!
//! This is not a convenience feature. `Cmdline.hs:49-102` reads the config file
//! and the environment variable BEFORE parsing the command line and prepends
//! what it finds, so a machine with `-mx` in its config gets a different
//! archive from the same command line. A port that ignores them writes
//! different bytes and says nothing about it.
//!
//! Order is `global ++ [defaults] ++ $DARC ++ argv` (`Cmdline.hs:102`). The
//! user's own arguments come LAST, so for every last-wins option they override
//! the defaults.
//!
//! # Why document order matters
//!
//! Two `[defaults]` keys can name the same command — `"a create"` and
//! `"a create t e x"` both contribute to `a`. They are prepended in the order
//! the file lists them, and for a last-wins option that order decides the
//! result. So the TOML is parsed with `preserve_order`: a hash-ordered table
//! would make the same config produce different archives between runs, which is
//! the worst kind of bug this project can have.
//!
//! # Replacing `arc.ini`
//!
//! The old INI is **not read**. A file that is present and ignored changes the
//! archive you get with nothing to show for it, which is the silent no-op this
//! project refuses everywhere else — so a leftover `arc.ini` is reported and
//! the run stops, rather than quietly proceeding on built-in defaults. The same
//! rule applies to `$FREEARC`.

use serde::Deserialize;
use std::collections::BTreeMap;

/// The file searched for beside the executable (`Options.hs:407`).
pub const CONFIG_FILE: &str = "darc.toml";

/// The environment variable holding default options (`Options.hs:410`).
pub const CONFIG_ENV_VAR: &str = "DARC";

/// The names this replaced. Detected only so they can be reported.
pub const LEGACY_CONFIG_FILE: &str = "arc.ini";
pub const LEGACY_CONFIG_ENV_VAR: &str = "FREEARC";

/// The `[defaults]` key that applies to every command.
///
/// It stands for the INI's bare first line. `all` is reserved: no DArc command
/// is spelled that way, and a `[defaults]` key naming a command called `all`
/// would be unreachable anyway.
pub const ALL_COMMANDS: &str = "all";

/// One `[external.NAME]` section — a compressor DArc shells out to.
#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct External {
    /// The command that compresses. `$in` and `$out` are substituted.
    pub packcmd: String,
    /// The command that decompresses.
    pub unpackcmd: String,
}

/// A parsed `darc.toml`.
#[derive(Debug, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct Config {
    /// Command name (or a whitespace-separated list of them) to options.
    /// A value may be one string or several, and several are joined in order.
    #[serde(default)]
    defaults: toml::Table,
    /// The compression method table. Rows override the built-in ones by key.
    #[serde(default)]
    methods: BTreeMap<String, crate::toml_table::Row>,
    /// External compressors, by name.
    #[serde(default)]
    external: BTreeMap<String, External>,
}

impl Config {
    /// Parse, refusing anything malformed rather than salvaging part of it.
    ///
    /// A half-applied config is worse than none: the options that did survive
    /// still change the archive.
    pub fn parse(text: &str) -> Result<Config, String> {
        toml::from_str(text).map_err(|e| e.to_string())
    }

    /// `configFilePlaces` (`Files.hs:224`) — beside the executable.
    pub fn find() -> Option<std::path::PathBuf> {
        beside_exe(CONFIG_FILE)
    }

    /// A leftover `arc.ini`, so the caller can refuse rather than ignore it.
    pub fn find_legacy() -> Option<std::path::PathBuf> {
        beside_exe(LEGACY_CONFIG_FILE)
    }

    /// The options common to EVERY command.
    pub fn global_options(&self) -> String {
        self.options_for(ALL_COMMANDS)
    }

    /// The `[defaults]` entries for one command.
    ///
    /// A key may name SEVERAL commands (`"a create t e x"`), and several keys
    /// may name the same one; every match contributes, in document order. Both
    /// were true of the INI and are easy to lose in translation.
    pub fn command_options(&self, command: &str) -> String {
        match command {
            // `all` is the global line, already prepended by the caller. A
            // command cannot also be named `all`, so nothing is lost.
            ALL_COMMANDS => String::new(),
            c => self.options_for(c),
        }
    }

    fn options_for(&self, command: &str) -> String {
        let mut out: Vec<String> = Vec::new();
        for (names, value) in &self.defaults {
            if !names.split_whitespace().any(|n| n == command) {
                continue;
            }
            match value {
                toml::Value::String(s) => out.push(s.clone()),
                toml::Value::Array(a) => {
                    for v in a {
                        match v.as_str() {
                            Some(s) => out.push(s.to_string()),
                            None => {}
                        }
                    }
                }
                // Named rather than wildcarded: the crate denies
                // `wildcard_enum_match_arm` so that a value shape added
                // later shows up here as a compile error, not as a
                // silently dropped default.
                toml::Value::Integer(_)
                | toml::Value::Float(_)
                | toml::Value::Boolean(_)
                | toml::Value::Datetime(_)
                | toml::Value::Table(_) => {}
            }
        }
        out.join(" ")
    }

    /// Any `[defaults]` value that is not a string or a list of strings.
    ///
    /// Reported rather than skipped: `a = 5` is a typo whose author meant
    /// something, and silently dropping it writes an archive they did not ask
    /// for.
    pub fn bad_defaults(&self) -> Vec<String> {
        self.defaults
            .iter()
            .filter(|(_, v)| match v {
                toml::Value::String(_) => false,
                toml::Value::Array(a) => !a.iter().all(toml::Value::is_str),
                toml::Value::Integer(_)
                | toml::Value::Float(_)
                | toml::Value::Boolean(_)
                | toml::Value::Datetime(_)
                | toml::Value::Table(_) => true,
            })
            .map(|(k, _)| k.clone())
            .collect()
    }

    /// The `[methods]` rows, rendered to the value strings
    /// [`crate::methodtable`] substitutes.
    pub fn method_rows(&self) -> Result<Vec<(String, String)>, String> {
        self.methods
            .iter()
            .map(|(k, row)| row.render().map(|v| (k.clone(), v)).map_err(|e| format!("[methods.\"{k}\"]: {e}")))
            .collect()
    }

    /// The `[external.NAME]` sections.
    pub fn external(&self) -> &BTreeMap<String, External> {
        &self.external
    }
}

fn beside_exe(name: &str) -> Option<std::path::PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let path = exe.parent()?.join(name);
    match path.is_file() {
        true => Some(path),
        false => None,
    }
}

#[cfg(test)]
mod tests {
    use super::Config;

    const SAMPLE: &str = r#"
[defaults]
all = "-mx --display=hnwftsr"
"a create" = "-m5"
"a create t e x" = "-di+$"
x = "-o+"

[methods]
"9" = { alias = "lzma:64m" }

[external.srep]
packcmd = "srep -m3 $in $out"
unpackcmd = "srep -d $in $out"
"#;

    #[test]
    fn global_options_come_from_the_all_key() {
        let c = Config::parse(SAMPLE).expect("parses");
        assert_eq!(c.global_options(), "-mx --display=hnwftsr");
        // `all` is not itself a command, so asking for it as one yields
        // nothing -- the caller prepends the global line separately and it must
        // not be added twice.
        assert_eq!(c.command_options("all"), "");
    }

    /// A key may name several commands, and several keys may name the same
    /// one. Both were true of `arc.ini` and both are load-bearing: dropping
    /// either silently changes which defaults a command gets.
    #[test]
    fn command_options_merge_across_keys_and_name_lists() {
        let c = Config::parse(SAMPLE).expect("parses");
        assert_eq!(c.command_options("a"), "-m5 -di+$");
        assert_eq!(c.command_options("create"), "-m5 -di+$");
        assert_eq!(c.command_options("x"), "-di+$ -o+");
        assert_eq!(c.command_options("l"), "");
    }

    /// Document order decides the result for a last-wins option, so it must
    /// survive parsing. With a hash-ordered table this assertion passes or
    /// fails depending on the run.
    #[test]
    fn defaults_keep_document_order() {
        let c = Config::parse("[defaults]\nz = \"-m1\"\na = \"-m2\"\n\"a z\" = \"-m3\"\n")
            .expect("parses");
        assert_eq!(c.command_options("z"), "-m1 -m3");
        assert_eq!(c.command_options("a"), "-m2 -m3");
    }

    /// A value may be a list, which is the TOML spelling of the INI's repeated
    /// lines for one command.
    #[test]
    fn a_defaults_value_may_be_a_list() {
        let c = Config::parse("[defaults]\na = [\"-m5\", \"-r\"]\n").expect("parses");
        assert_eq!(c.command_options("a"), "-m5 -r");
    }

    /// A malformed file is refused whole. Half of a config is not a safer
    /// subset of it -- the options that did apply still change the archive.
    #[test]
    fn a_malformed_file_is_refused_rather_than_salvaged() {
        assert!(Config::parse("[defaults\na = \"-m5\"").is_err());
        // An unknown top-level section is a typo, and a typo that parses is a
        // config that silently does nothing.
        assert!(Config::parse("[methodz]\n\"9\" = { alias = \"lzma\" }\n").is_err());
        // A value of the wrong type is reported, not skipped.
        let c = Config::parse("[defaults]\na = 5\nb = \"-r\"\n").expect("parses");
        assert_eq!(c.bad_defaults(), vec!["a".to_string()]);
    }

    /// The sections that used to be parsed and then ignored now mean
    /// something.
    #[test]
    fn methods_and_external_are_read() {
        let c = Config::parse(SAMPLE).expect("parses");
        assert_eq!(c.method_rows().expect("renders"), vec![("9".to_string(), "lzma:64m".to_string())]);
        let ext = c.external();
        assert_eq!(ext.len(), 1);
        assert_eq!(ext["srep"].packcmd, "srep -m3 $in $out");
    }

    /// A method row that cannot be rendered names itself, because the message
    /// is the only thing standing between the user and a silently ignored row.
    #[test]
    fn an_unrenderable_method_row_names_itself() {
        let c = Config::parse("[methods]\n\"9\" = { }\n").expect("parses");
        let err = c.method_rows().expect_err("must not render");
        assert!(err.contains("[methods.\"9\"]"), "unhelpful: {err}");
    }
}
