//! `darc.toml`'s `[methods]` section — the compression table, structured.
//!
//! # Why this is a front end and not a replacement
//!
//! The method table is a macro language. `9binary = 4x4:b254m:lzma:254m:max` is
//! one row; `#p = #rep+exe+#xb / $obj=#pb / $text=#pt` is another, and the `#`
//! is a level wildcard while `#rep` inside the value is a *reference to another
//! row*. [`crate::methodtable`] already reduces all of that to a flat
//! `Vec<(key, value)>` and expands `#` into nine rows at load time.
//!
//! So this module does exactly one thing: turn a structured TOML row back into
//! the **value string** that machinery already understands. Nothing downstream
//! changes, and the correctness question collapses to a single checkable
//! property — *does rendering produce the string the table would have held?*
//!
//! That matters because **the rendered string is written into the archive
//! header**. `4x4:b254m:lzma:254m:max` is recorded verbatim and re-parsed on
//! read, so a schema that renders even slightly differently writes archives the
//! reference does not. Rendering is therefore deliberately dumb: join with the
//! separator, never normalise, never reorder, never canonicalise.

use serde::Deserialize;

/// One link in a chain: either a real codec, or a reference to another row.
///
/// The distinction is load-bearing. In `#rep+exe+#xb`, `exe` is a codec and
/// `#rep`/`#xb` are rows that get substituted later — collapsing them into one
/// "name" field would make `{codec = "#rep"}` and `{ref = "#rep"}` render the
/// same while meaning different things to a reader of the config.
#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct Link {
    /// A codec name — `lzma`, `exe`, `4x4`.
    #[serde(default)]
    pub codec: Option<String>,
    /// Another row of the table, substituted before use.
    #[serde(default, rename = "ref")]
    pub reference: Option<String>,
    /// The codec's `:`-separated parameters, in order.
    #[serde(default)]
    pub params: Vec<String>,
}

impl Link {
    /// `codec:p1:p2`, or the reference verbatim.
    fn render(&self) -> Result<String, String> {
        match (&self.codec, &self.reference) {
            (Some(_), Some(_)) => {
                Err("a link has both `codec` and `ref`; it must have exactly one".to_string())
            }
            (None, None) => {
                Err("a link has neither `codec` nor `ref`; it must have exactly one".to_string())
            }
            // A reference takes no parameters: `#rep` names a whole row, and
            // that row carries its own.
            (None, Some(r)) => match self.params.is_empty() {
                true => Ok(r.clone()),
                false => Err(format!("`ref = \"{r}\"` cannot take params")),
            },
            (Some(c), None) => {
                let mut s = c.clone();
                for p in &self.params {
                    s.push(':');
                    s.push_str(p);
                }
                Ok(s)
            }
        }
    }
}

/// One alternative of a `/`-separated row.
#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct Alternative {
    /// `$obj`, `$text` — the filetype group this alternative applies to.
    /// Absent for the leading alternative, which is the general case.
    #[serde(default)]
    pub group: Option<String>,
    #[serde(default)]
    pub chain: Vec<Link>,
    /// A whole-value reference to another row.
    #[serde(default)]
    pub alias: Option<String>,
}

impl Alternative {
    fn render(&self) -> Result<String, String> {
        let body = match (&self.alias, self.chain.is_empty()) {
            (Some(_), false) => {
                return Err("an alternative has both `alias` and `chain`".to_string());
            }
            (None, true) => return Err("an alternative has neither `alias` nor `chain`".to_string()),
            (Some(a), true) => a.clone(),
            (None, false) => {
                let links: Result<Vec<String>, String> =
                    self.chain.iter().map(Link::render).collect();
                links?.join("+")
            }
        };
        match &self.group {
            Some(g) => Ok(format!("{g}={body}")),
            None => Ok(body),
        }
    }
}

/// One row of `[methods]`.
#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct Row {
    #[serde(default)]
    pub chain: Vec<Link>,
    #[serde(default)]
    pub alias: Option<String>,
    #[serde(default)]
    pub alternatives: Vec<Alternative>,
}

impl Row {
    /// The value string this row stands for.
    pub fn render(&self) -> Result<String, String> {
        let forms = u8::from(!self.chain.is_empty())
            + u8::from(self.alias.is_some())
            + u8::from(!self.alternatives.is_empty());
        match forms {
            0 => return Err("a row must have one of `chain`, `alias` or `alternatives`".to_string()),
            1 => {}
            _ => {
                return Err(
                    "a row must have exactly one of `chain`, `alias` or `alternatives`".to_string()
                );
            }
        }
        match (&self.alias, self.alternatives.is_empty()) {
            (Some(a), _) => Ok(a.clone()),
            (None, false) => {
                let alts: Result<Vec<String>, String> =
                    self.alternatives.iter().map(Alternative::render).collect();
                Ok(alts?.join("/"))
            }
            (None, true) => {
                let links: Result<Vec<String>, String> =
                    self.chain.iter().map(Link::render).collect();
                Ok(links?.join("+"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Row;

    fn render(toml_src: &str) -> Result<String, String> {
        let row: Row = toml::from_str(toml_src).map_err(|e| e.to_string())?;
        row.render()
    }

    /// Each shape the built-in table uses, rendered back to the exact string
    /// that table holds. These are copied from `methodtable.rs`'s BUILTIN, not
    /// invented, because the strings are what archives record.
    #[test]
    fn every_row_shape_renders_to_the_table_string() {
        // `9binary = 4x4:b254m:lzma:254m:max` -- 4x4 nests with ':', so the
        // inner method is a PARAMETER, not a second link.
        assert_eq!(
            render(r#"chain = [{ codec = "4x4", params = ["b254m", "lzma:254m:max"] }]"#),
            Ok("4x4:b254m:lzma:254m:max".to_string())
        );
        // `5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0`
        // (whitespace inside the table is stripped before use).
        assert_eq!(
            render(
                r#"chain = [
                     { codec = "dict", params = ["p", "64m", "80%"] },
                     { codec = "lzp",  params = ["64m", "32", "h22", "85%"] },
                     { codec = "pmm",  params = ["8", "160m", "r0"] },
                   ]"#
            ),
            Ok("dict:p:64m:80%+lzp:64m:32:h22:85%+pmm:8:160m:r0".to_string())
        );
        // `wav = tta`, `bcj = exe`, `#bx = #xb`.
        assert_eq!(render(r#"alias = "tta""#), Ok("tta".to_string()));
        assert_eq!(render(r##"alias = "#xb""##), Ok("#xb".to_string()));
        // `#p = #rep+exe+#xb / $obj=#pb / $text=#pt`
        assert_eq!(
            render(
                r##"alternatives = [
                     { chain = [{ ref = "#rep" }, { codec = "exe" }, { ref = "#xb" }] },
                     { group = "$obj",  alias = "#pb" },
                     { group = "$text", alias = "#pt" },
                   ]"##
            ),
            Ok("#rep+exe+#xb/$obj=#pb/$text=#pt".to_string())
        );
    }

    /// A row that says two contradictory things is refused rather than
    /// silently resolved. Picking one would write an archive the config did
    /// not ask for, which is the failure this project refuses everywhere.
    #[test]
    fn contradictory_rows_are_refused() {
        assert!(render(r#"alias = "tta""#).is_ok());
        assert!(render(r#"chain = [{ codec = "tta" }]
alias = "lzma""#)
            .is_err());
        assert!(render("").is_err());
        assert!(render(r#"chain = [{ codec = "a", ref = "b" }]"#).is_err());
        assert!(render(r#"chain = [{ params = ["x"] }]"#).is_err());
        // A ref carries no params of its own.
        assert!(render(r##"chain = [{ ref = "#rep", params = ["8m"] }]"##).is_err());
        // An unknown key is a typo, and a typo that parses is a silent
        // misconfiguration.
        assert!(render(r#"chian = [{ codec = "tta" }]"#).is_err());
    }
}

#[cfg(test)]
mod round_trip {
    //! The gate the whole schema rests on.
    //!
    //! Every value string the built-in table holds is parsed into [`Row`] form
    //! and rendered back. If any one of them does not come back byte-identical,
    //! the schema cannot express the table, and a `darc.toml` written in it
    //! would produce archives that differ from the reference's.
    //!
    //! This tests the SCHEMA's expressive power, not the config reader: it
    //! builds the Row values directly from the table's own strings, so it
    //! covers rows no hand-written test would think to include.
    use super::{Alternative, Link, Row};

    /// Parse one table value back into the structured form. Deliberately the
    /// inverse of `render`, and deliberately naive — it only has to handle what
    /// the table actually contains.
    fn parse_value(v: &str) -> Row {
        let alts: Vec<&str> = v.split('/').collect();
        match alts.len() {
            1 => parse_alternative(alts[0]).into_row(),
            _ => Row {
                chain: Vec::new(),
                alias: None,
                alternatives: alts.into_iter().map(parse_alternative).collect(),
            },
        }
    }

    impl Alternative {
        fn into_row(self) -> Row {
            match (self.alias, self.chain.len()) {
                (Some(a), _) => Row { chain: Vec::new(), alias: Some(a), alternatives: Vec::new() },
                (None, _) => {
                    Row { chain: self.chain, alias: None, alternatives: Vec::new() }
                }
            }
        }
    }

    fn parse_alternative(a: &str) -> Alternative {
        let (group, body) = match a.starts_with('$') {
            true => match a.split_once('=') {
                Some((g, b)) => (Some(g.to_string()), b),
                None => (None, a),
            },
            false => (None, a),
        };
        // A single link with no ':' and no '+' is an alias -- `wav = tta`.
        match !body.contains('+') && !body.contains(':') {
            true => Alternative { group, chain: Vec::new(), alias: Some(body.to_string()) },
            false => Alternative {
                group,
                chain: body.split('+').map(parse_link).collect(),
                alias: None,
            },
        }
    }

    fn parse_link(l: &str) -> Link {
        let mut it = l.split(':');
        let head = it.next().unwrap_or("");
        let params: Vec<String> = it.map(str::to_string).collect();
        // A '#'-prefixed head with no params is a reference to another row.
        match head.starts_with('#') && params.is_empty() {
            true => Link { codec: None, reference: Some(head.to_string()), params },
            false => Link { codec: Some(head.to_string()), reference: None, params },
        }
    }

    #[test]
    fn every_builtin_value_survives_the_schema() {
        let table = crate::methodtable::builtin_substs();
        assert!(table.len() > 100, "table looks empty: {} rows", table.len());
        let mut checked = 0;
        for (key, value) in &table {
            if value.is_empty() {
                continue;
            }
            let rendered = parse_value(value)
                .render()
                .unwrap_or_else(|e| panic!("{key} = {value}: {e}"));
            assert_eq!(&rendered, value, "row `{key}` did not survive the round trip");
            checked += 1;
        }
        assert!(checked > 100, "only {checked} rows checked");
        println!("round-tripped {checked} built-in rows");
    }
}
