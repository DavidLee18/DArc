//! `-m` levels into compression chains — `decode_method` (`Compression.hs:375`).
//!
//! `-m4` is not a chain, it is a lookup that expands into several: one for the
//! default type, one for `$obj`, one for `$text`. The expansion is a small
//! substitution language, and it runs in two distinct passes:
//!
//! ```text
//!   subst              "4"        -> "4rep+exe+4xb/$obj=4b/$text=4t"
//!   split_to_methods              -> [("","exe+4rep+exe+4xb"), ("$obj",…), ("$text",…)]
//!   keepOnlyLastOn fst            -> one entry per type, the last wins
//!   mapSnds (subst2)              -> each chain expanded link by link
//! ```
//!
//! `subst` expands the whole string and recurses; `subst2` splits a chain on
//! `'+'` and expands each link's *name*, keeping its parameters
//! (`"4rep" -> "rep:96m"`, and `"lzma:8m" -> lookup "lzma" then re-append
//! ":8m"`).
//!
//! # Two ordering rules that decide which definition wins
//!
//! * definitions **without** `#` come before those with it, so `1x = 1` beats
//!   `#x = #xb/#xt` expanded to `1x = 1xb/1xt`.
//! * `lookup` takes the FIRST match, while `keepOnlyLastOn` takes the last —
//!   the two run at different stages and pull in opposite directions.

/// One `name = value` definition.
type Subst = (String, String);

/// `prepareSubsts` (`Compression.hs:418`).
///
/// Strips a `;` comment and then **all** whitespace — `"3t  = dict:p: 64m:85%"`
/// becomes `"3t=dict:p:64m:85%"`, which is why the table can be written with
/// alignment spaces inside method strings.
///
/// A line containing `#` becomes nine lines, one per digit.
fn prepare(lines: &[&str]) -> Vec<Subst> {
    let mut out = Vec::new();
    for line in lines {
        let no_comment = line.split(';').next().unwrap_or("");
        let squeezed: String = no_comment.chars().filter(|c| !c.is_whitespace()).collect();
        if squeezed.is_empty() {
            continue;
        }
        let expanded: Vec<String> = if squeezed.contains('#') {
            ('1'..='9').map(|d| squeezed.replace('#', &d.to_string())).collect()
        } else {
            vec![squeezed]
        };
        for e in expanded {
            match e.split_once('=') {
                Some((k, v)) => out.push((k.to_string(), v.to_string())),
                None => out.push((e, String::new())),
            }
        }
    }
    out
}

/// The substitution list, in lookup order.
///
/// `reorder` puts the definitions without `#` first *before* expansion, so the
/// specific ones shadow the general ones.
pub fn builtin_substs() -> Vec<Subst> {
    let (plain, hashed): (Vec<&&str>, Vec<&&str>) =
        BUILTIN.iter().partition(|l| !l.contains('#'));
    let ordered: Vec<&str> =
        plain.into_iter().chain(hashed).copied().collect();
    prepare(&ordered)
}

fn lookup<'a>(list: &'a [Subst], key: &str) -> Option<&'a str> {
    list.iter().find(|(k, _)| k == key).map(|(_, v)| v.as_str())
}

/// `subst` (`Compression.hs:387`) — expand the main method, then append any
/// per-group definitions that start with it.
fn subst(list: &[Subst], method: &str) -> String {
    let mut parts = method.split('/');
    let main = parts.next().unwrap_or("");
    let user: Vec<&str> = parts.collect();

    let main_methods = match lookup(list, main) {
        Some(x) => subst(list, x),
        None => main.to_string(),
    };

    // Definitions named "<main>$type", contributed as "$type=..." entries. The
    // list is deduplicated on the KEY first, keeping the first of each.
    let mut seen: Vec<&str> = Vec::new();
    let mut group_methods: Vec<String> = Vec::new();
    for (k, v) in list {
        if seen.contains(&k.as_str()) {
            continue;
        }
        seen.push(k);
        match k.strip_prefix(main) {
            Some(rest) if rest.starts_with('$') => {
                group_methods.push(format!("{rest}={v}"));
            }
            Some(_) | None => {}
        }
    }

    let mut all = vec![main_methods];
    all.extend(group_methods);
    all.extend(user.iter().map(|s| s.to_string()));
    all.join("/")
}

/// `split_to_methods` (`Compression.hs:412`) — turn a `'/'`-separated spec into
/// `(type, chain)` pairs.
///
/// The three shapes are not interchangeable. With two leading chains and no
/// `$`, the FIRST becomes `"exe+" ++ b` for the default type — a filter is
/// prepended that the user never wrote.
fn split_to_methods(spec: &str) -> Vec<(String, String)> {
    let parts: Vec<&str> = spec.split('/').collect();
    match parts.len() {
        0 => Vec::new(),
        1 => vec![(String::new(), parts[0].to_string())],
        _ if parts[1].starts_with('$') => {
            let mut out = vec![(String::new(), parts[0].to_string())];
            out.extend(parts[1..].iter().map(|p| split2(p)));
            out
        }
        _ => {
            let b = parts[0];
            let t = parts[1];
            let mut out = vec![
                (String::new(), format!("exe+{b}")),
                ("$obj".to_string(), b.to_string()),
                ("$text".to_string(), t.to_string()),
            ];
            out.extend(parts[2..].iter().map(|p| split2(p)));
            out
        }
    }
}

fn split2(s: &str) -> (String, String) {
    match s.split_once('=') {
        Some((a, b)) => (a.to_string(), b.to_string()),
        None => (s.to_string(), String::new()),
    }
}

/// `subst2` — expand one chain, link by link.
///
/// Each link is split at the first `':'`; the name is looked up and, on a hit,
/// the ORIGINAL parameters are re-appended to the replacement before recursing.
/// So `4binary` becomes `4x4:b16m:lzma:16m:h64m:normal:mc16`, and `lzma:8m`
/// stays `lzma:8m` because "lzma" is not a key.
fn subst2(list: &[Subst], chain: &str) -> Vec<String> {
    let mut out = Vec::new();
    for method in chain.split('+') {
        let (head, params) = match method.find(':') {
            Some(i) => (&method[..i], &method[i..]),
            None => (method, ""),
        };
        match lookup(list, head) {
            Some(new_head) => out.extend(subst2(list, &format!("{new_head}{params}"))),
            None => out.push(method.to_string()),
        }
    }
    out
}

/// `decode_method` — a `-m` spec into one chain per file type.
///
/// Returns `(type name, chain)` pairs with the default type first, and the
/// chains **canonicalised**, which is what `decode_one_method` does at the end
/// of `subst2`.
pub fn decode_method(spec: &str) -> Vec<(String, Vec<String>)> {
    decode_method_with(spec, &substs_with_user(user_rows()))
}

/// `darc.toml`'s `[methods]`, set once at startup.
///
/// A global for the same reason the C's table is one: `decode_method` is
/// reached from inside `add`, several layers below where the config was read,
/// and the reference's table is likewise a program-wide thing built before any
/// command runs. Set exactly once, before any archive work begins.
static USER_ROWS: std::sync::OnceLock<Vec<Subst>> = std::sync::OnceLock::new();

/// Install the config's method rows. Later calls are ignored, so a second
/// caller cannot change the table out from under a command in progress.
pub fn set_user_rows(rows: Vec<(String, String)>) {
    drop(USER_ROWS.set(rows));
}

fn user_rows() -> &'static [Subst] {
    USER_ROWS.get().map_or(&[], Vec::as_slice)
}

/// Expand a `(key, value)` pair the way [`prepare`] expands a table line:
/// squeeze whitespace, and turn one `#` row into nine.
///
/// `darc.toml`'s `[methods]` arrives already split into key and value, so it
/// skips the `=`-splitting and comment-stripping half of `prepare` — but it
/// must go through the same `#` expansion, or a user's `#$wav` row would be
/// looked up under a literal `#` and never match.
fn prepare_pairs(pairs: &[(String, String)]) -> Vec<Subst> {
    let mut out = Vec::new();
    for (k, v) in pairs {
        let squeeze = |s: &str| -> String { s.chars().filter(|c| !c.is_whitespace()).collect() };
        let (k, v) = (squeeze(k), squeeze(v));
        match k.contains('#') || v.contains('#') {
            true => {
                for d in '1'..='9' {
                    let d = d.to_string();
                    out.push((k.replace('#', &d), v.replace('#', &d)));
                }
            }
            false => out.push((k, v)),
        }
    }
    out
}

/// The substitution list with `darc.toml`'s `[methods]` rows applied.
///
/// `reorder` (`Compression.hs:418`) puts rows WITHOUT `#` before rows with it,
/// so a specific definition shadows a general one; `lookup` then takes the
/// first match. Applying that to the combined list — user rows before built-in
/// rows *within each half* — is what makes a user row win a tie without letting
/// a general user row shadow a specific built-in one.
///
/// Getting the halves wrong is not a compile error and not a test failure on a
/// config that overrides nothing; it silently changes which chain a preset
/// expands to, and that chain is written into the archive.
pub fn substs_with_user(user: &[(String, String)]) -> Vec<Subst> {
    // Partition BEFORE expansion. `prepare` turns one `#` row into nine rows
    // that contain no `#` at all, so a partition applied afterwards sees every
    // row as specific and the ordering rule collapses -- every user row then
    // shadows every built-in one, including the specific built-ins it must not
    // touch. That was this function's first version, and the test below is
    // what caught it.
    let general = |k: &str, v: &str| k.contains('#') || v.contains('#');
    let (u_plain, u_hash): (Vec<(String, String)>, Vec<(String, String)>) =
        user.iter().cloned().partition(|(k, v)| !general(k, v));
    let (b_plain, b_hash): (Vec<&&str>, Vec<&&str>) =
        BUILTIN.iter().partition(|l| !l.contains('#'));
    let b_plain: Vec<&str> = b_plain.into_iter().copied().collect();
    let b_hash: Vec<&str> = b_hash.into_iter().copied().collect();

    let mut out = prepare_pairs(&u_plain);
    out.extend(prepare(&b_plain));
    out.extend(prepare_pairs(&u_hash));
    out.extend(prepare(&b_hash));
    out
}

/// [`decode_method`] against a supplied substitution list.
pub fn decode_method_with(spec: &str, list: &[Subst]) -> Vec<(String, Vec<String>)> {
    let expanded = subst(list, spec);
    let pairs = split_to_methods(&expanded);

    // keepOnlyLastOn fst: a later definition of a type replaces an earlier one,
    // and the surviving entry keeps its ORIGINAL position.
    let mut kept: Vec<(String, String)> = Vec::new();
    for (k, v) in pairs.into_iter().rev() {
        if !kept.iter().any(|(k2, _)| *k2 == k) {
            kept.push((k, v));
        }
    }
    kept.reverse();

    kept.into_iter()
        .filter(|(_, v)| !v.is_empty())
        .map(|(k, v)| {
            let chain: Vec<String> = subst2(list, &v)
                .into_iter()
                .map(|m| crate::canonize::canonize(&m).unwrap_or(m))
                .collect();
            (k, chain)
        })
        .collect()
}

/// `builtinMethodSubsts` (`Compression.hs:427`), verbatim including the
/// alignment spaces — `prepare` strips them, and keeping the text as written
/// makes it diffable against the Haskell.
const BUILTIN: &[&str] = &[
    ";High-level method definitions",
    "x  = 9            ;highest compression mode using only internal algorithms",
    "ax = 9p           ;highest compression mode involving external compressors",
    "0  = storing",
    "1  = 1b  / $exe=exe+1b",
    "1x = 1",
    "#  = #rep+exe+#xb / $obj=#b / $text=#t",
    "#x = #xb/#xt",
    "",
    ";Text files compression with slow decompression",
    "1t  = 1b",
    "2t  = grzip:m4:8m:32:h15",
    "3t  = dict:p: 64m:85% + lzp: 64m: 24:h20        :92% + grzip:m3:8m:l",
    "4t  = dict:p: 64m:80% + lzp: 64m: 65:d1m:s16:h20:90% + ppmd:8:96m",
    "5t  = dict:p: 64m:80% + lzp: 80m:105:d1m:s32:h22:92% + ppmd:12:192m",
    "6t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m",
    "7t  = dict:p:128m:80% + lzp:320m:185:d1m:s32:h24:92% + ppmd:20:768m",
    "8t  = dict:p:128m:80% + lzp:640m:225:d1m:s32:h25:92% + ppmd:24:1536m",
    "9t  = dict:p:128m:80% + lzp:800m:235:d1m:s32:h26:92% + ppmd:25:2047m",
    "",
    ";Binary files compression with slow and/or memory-expensive decompression",
    "1b  = 1xb",
    "#b  = #rep+#bx",
    "2rep  = rep:  96m",
    "3rep  = rep:  96m",
    "4rep  = rep:  96m",
    "5rep  = rep: 128m",
    "6rep  = rep: 256m",
    "7rep  = rep: 512m",
    "8rep  = rep:1024m",
    "9rep  = rep:2047m",
    "",
    ";Text files compression with fast decompression",
    "1xt = 1xb",
    "2xt = 2xb",
    "3xt = dict:  64m:80% + tor:7:96m:h64m",
    "4xt = dict:  64m:75% + 4binary",
    "#xt = dict: 128m:75% + #binary",
    "",
    ";Binary files compression with fast decompression",
    "1xb = 4x4:tor:3",
    "2xb = 4x4:tor:6",
    "#xb = delta + #binary",
    "",
    ";Binary files compression with fast decompression",
    "1binary = tor:3",
    "2binary = tor:6",
    "3binary = 4x4:b8m:lzma:8m:h64m:fast:mc8",
    "4binary = 4x4:b16m:lzma:16m:h64m:normal:mc16",
    "5binary = 4x4:b16m:lzma:16m:max",
    "6binary = 4x4:b32m:lzma:32m:max",
    "7binary = 4x4:b64m:lzma:64m:max",
    "8binary = 4x4:b128m:lzma:128m:max",
    "9binary = 4x4:b254m:lzma:254m:max",
    "",
    ";Synonyms",
    "bcj = exe",
    "#bx = #xb",
    "#tx = #xt",
    "x#  = #x",
    "",
    ";Compression modes involving external PPMONSTR.EXE",
    "#p  = #rep+exe+#xb / $obj=#pb / $text=#pt",
    "5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0",
    "6pt = dict:p: 64m:80% + lzp: 64m:64:h22:85% + pmm:16:384m:r1",
    "7pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:20:768m:r1",
    "8pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:24:1536m:r1",
    "9pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:25:2047m:r1",
    "#pt = #t",
    "#pb = #b",
    "",
    "#q  = #qb/#qt",
    "5qt = dict:p:64m:80% + lzp:64m:64:d1m:24:h22:85% + pmm:10:160m:r1",
    "5qb = rep: 128m      + delta                     + pmm:16:160m:r1",
    "6qb = rep: 256m      + delta                     + pmm:20:384m:r1",
    "7qb = rep: 512m      + delta                     + pmm:22:768m:r1",
    "8qb = rep:1024m      + delta                     + pmm:24:1536m:r1",
    "9qb = rep:2047m      + delta                     + pmm:25:2047m:r1",
    "#qt = #pt",
    "#qb = #pb",
    "",
    ";Sound wave files are compressed best with TTA",
    "wav     = tta      ;best compression",
    "wavfast = tta:m1   ;faster compression and decompression",
    "1$wav  = wavfast",
    "2$wav  = wavfast",
    "#$wav  = wav",
    "#x$wav = wavfast",
    "#p$wav = wav",
    "",
    ";Bitmap graphic files are compressed best with GRZip",
    "bmp        = mm    + grzip:m1:l:a  ;best compression",
    "bmpfast    = mm    + grzip:m4:l:a  ;faster compression",
    "bmpfastest = mm:d1 + tor:3:t0      ;fastest one",
    "1$bmp  = bmpfastest",
    "2$bmp  = bmpfastest",
    "3$bmp  = bmpfast",
    "#$bmp  = bmp",
    "1x$bmp = bmpfastest",
    "2x$bmp = bmpfastest",
    "#x$bmp = mm+#binary",
    "#p$bmp = bmp",
    "",
    ";Quick & dirty compression for data already compressed",
    "4$compressed   = rep:96m + tor:c3",
    "3$compressed   = rep:96m + tor:3",
    "2$compressed   = rep:96m + tor:3",
    "4x$compressed  = tor:8m:c3",
    "3x$compressed  = rep:8m  + tor:3",
    "2x$compressed  = rep:8m  + tor:3",
];

/// `aCOMPRESSED_METHOD` (`Compression.hs:81`) — what `-ms` gives `$compressed`.
const COMPRESSED_METHOD: &str = "tor:8m:c3";

/// Everything the `-m` options say, after `Cmdline.hs:241` has taken them apart.
///
/// `-m` is not one option. Its VALUE carries a second grammar, and the
/// command-line parser cannot see that: `-mt1` matches both `m` and `mt` by
/// prefix, `aPREFFERED_OPTIONS` breaks the tie in favour of `method`, and the
/// value `"t1"` is then scanned HERE and turns into a thread count.
///
/// The port implemented only the last rule — "anything else is the main
/// method" — so `-mt1`, `-ms`, `-md16m`, `-ma1` and `-mc-` all became method
/// names, and were rejected as codecs that do not exist.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct MOptions {
    /// `method` — the main compression method. Empty when none was given.
    pub method: String,
    /// `methods` — the per-type suffixes, each already `/`-prefixed.
    pub methods: String,
    /// `mc'` — algorithms to disable.
    pub disabled: Vec<String>,
    /// `dict` — `-md`, in bytes. 0 is "not given".
    pub dictionary: u64,
    /// `mm'` — multimedia mode. `"--"` is "not given".
    pub multimedia: String,
    /// `threads` — `-mt`. 0 is "as many as the machine has".
    pub threads: u32,
    /// `ma'` — autodetection level. `"--"` is "not given".
    pub autodetect: String,
}

/// `parseDict` (`Cmdline.hs:232`) — the `-md` value, or `None` if this is not
/// the `-md` option at all but a method whose name begins with `d`.
///
/// That second case is the whole reason this returns an Option: `-mdict:32k`
/// and `-mdelta` both start with `d`, and both must fall through to the main
/// method rather than be read as dictionary sizes.
fn parse_dict(s: &str) -> Option<u64> {
    let mut chars = s.chars();
    let first = chars.next()?;
    match (first.is_ascii_alphabetic(), chars.next()) {
        // `-mda`..`-mdz`: a single letter, 2^(16 + c - 'a').
        (true, None) => Some(1u64 << (16 + (first as u8 - b'a') as u32)),
        _ => match first.is_ascii_digit() {
            true => crate::method::parse_mem(s).map(u64::from),
            false => None,
        },
    }
}

/// `changeTo` — replace the whole string when it matches, else keep it.
fn change_to<'a>(s: &'a str, table: &[(&str, &'a str)]) -> &'a str {
    match table.iter().find(|(from, _)| *from == s) {
        Some((_, to)) => to,
        None => s,
    }
}

/// The `forM_ compression_options` loop (`Cmdline.hs:241`).
///
/// Order matters and so do the guards: each arm applies only when its value
/// parses, and falls through to "this is the main method" when it does not.
/// `-mmm` reaches the mm codec exactly because `"m"` is not one of the
/// multimedia modes, and `-mtor:3` reaches Tornado because `"or:3"` is not all
/// digits.
pub fn scan_m_options(values: &[&str]) -> Result<MOptions, String> {
    let mut o = MOptions {
        multimedia: "--".to_string(),
        autodetect: "--".to_string(),
        ..MOptions::default()
    };
    for value in values {
        let value: &str = value;
        let (head, rest) = match value.chars().next() {
            Some(c) => (c, &value[c.len_utf8()..]),
            None => (' ', ""),
        };
        // -mcd-, -mc-rep: disable an algorithm. Only when the value is fenced
        // by a dash, so `-mcabbage` is still a method name.
        if head == 'c' && (rest.starts_with('-') || rest.ends_with('-')) {
            let name = rest.trim_start_matches('-').trim_end_matches('-');
            o.disabled.push(
                change_to(
                    name,
                    &[
                        ("d", "delta"),
                        ("e", "exe"),
                        ("l", "lzp"),
                        ("r", "rep"),
                        ("z", "dict"),
                        ("a", "$wav"),
                        ("c", "$bmp"),
                        ("t", "$text"),
                    ],
                )
                .to_string(),
            );
            continue;
        }
        if head == 'd' {
            match parse_dict(rest) {
                Some(md) => {
                    o.dictionary = md;
                    continue;
                }
                None => {}
            }
        }
        if head == 'm' {
            let flag = rest.strip_prefix('=').unwrap_or(rest);
            if matches!(flag, "" | "--" | "+" | "-" | "max" | "fast") {
                o.multimedia = flag.to_string();
                continue;
            }
        }
        // -ms / -ms-, which are exact matches rather than prefixes.
        if value == "s" {
            o.methods += &format!("/$compressed={COMPRESSED_METHOD}");
            continue;
        }
        if value == "s-" {
            o.disabled.push("$compressed".to_string());
            continue;
        }
        if head == 'a' {
            let flag = rest.strip_prefix('=').unwrap_or(rest);
            let flag = change_to(flag, &[("+", "--"), ("", "--"), ("-", "0")]);
            if flag == "--" || (flag.len() == 1 && flag.chars().all(|c| c.is_ascii_digit())) {
                o.autodetect = flag.to_string();
                continue;
            }
        }
        if head == 't' {
            let flag = rest.strip_prefix('=').unwrap_or(rest);
            let flag = change_to(flag, &[("-", "1"), ("+", "0"), ("", "0"), ("--", "0")]);
            if !flag.is_empty() && flag.chars().all(|c| c.is_ascii_digit()) {
                o.threads = flag.parse().map_err(|_| format!("-mt{rest}: not a number"))?;
                continue;
            }
        }
        // -m$type=method.
        if head == '$' {
            match value.find(['=', ':', '.']) {
                Some(i) if value.as_bytes()[i] == b'=' => {
                    o.methods += &format!("/{value}");
                    continue;
                }
                _ => return Err(format!("bad option format: -m{value}")),
            }
        }
        // Everything else is the main method. `-m0=…` is a spelling of it.
        o.method = value.strip_prefix("0=").unwrap_or(value).to_string();
    }
    Ok(o)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Measured against the reference: `-m4`, `-m4 -mt1`, `-m4 -ms`,
    /// `-m4 -md16m` and `-m4 -mc-` all produce the SAME archive over a corpus
    /// with no `$compressed` files, because each of those is its own knob and
    /// none of them is the method name.
    #[test]
    fn the_m_value_grammar_separates_the_knobs_from_the_method() {
        let scan = |v: &[&str]| scan_m_options(v).expect("scans");

        assert_eq!(scan(&["4"]).method, "4");
        assert_eq!(scan(&["4", "t1"]).threads, 1);
        assert_eq!(scan(&["4", "t1"]).method, "4", "-mt1 is not a method");
        assert_eq!(scan(&["t1"]).method, "", "and does not become one on its own");
        assert_eq!(scan(&["t-"]).threads, 1);
        assert_eq!(scan(&["t+"]).threads, 0);

        assert_eq!(scan(&["4", "s"]).methods, "/$compressed=tor:8m:c3");
        assert_eq!(scan(&["4", "s"]).method, "4");
        assert_eq!(scan(&["s-"]).disabled, vec!["$compressed"]);

        assert_eq!(scan(&["4", "d16m"]).dictionary, 16 << 20);
        assert_eq!(scan(&["da"]).dictionary, 1 << 16);
        assert_eq!(scan(&["dz"]).dictionary, 1 << 41);

        assert_eq!(scan(&["m"]).multimedia, "");
        assert_eq!(scan(&["m=max"]).multimedia, "max");
        assert_eq!(scan(&["a1"]).autodetect, "1");
        assert_eq!(scan(&["a-"]).autodetect, "0");
        assert_eq!(scan(&["cd-"]).disabled, vec!["delta"]);
        assert_eq!(scan(&["c-rep"]).disabled, vec!["rep"]);

        // The fall-through cases, which are what make the codecs reachable at
        // all. Every one of these begins with a letter that also names a knob.
        for m in ["mm", "tta", "tor:3", "dict:32k", "delta", "dispack", "as", "cabbage"] {
            assert_eq!(scan(&[m]).method, m, "-m{m} must stay a method name");
        }
        assert_eq!(scan(&["$text=ppmd"]).methods, "/$text=ppmd");
        assert!(scan_m_options(&["$text"]).is_err(), "-m$type with no = is an error");
    }


    fn chain_for(spec: &str, ty: &str) -> String {
        decode_method(spec)
            .into_iter()
            .find(|(k, _)| k == ty)
            .map(|(_, v)| v.join("+"))
            .unwrap_or_default()
    }

    /// prepareSubsts strips ALL whitespace, not just the leading kind -- the
    /// table aligns its columns inside method strings.
    #[test]
    fn whitespace_inside_a_method_string_is_stripped() {
        let list = builtin_substs();
        assert_eq!(lookup(&list, "4rep"), Some("rep:96m"));
        assert_eq!(lookup(&list, "9rep"), Some("rep:2047m"));
    }

    /// A definition without '#' shadows the '#'-expanded one: "1x = 1" wins
    /// over "#x = #xb/#xt".
    #[test]
    fn specific_definitions_shadow_general_ones() {
        let list = builtin_substs();
        assert_eq!(lookup(&list, "1x"), Some("1"));
        assert_eq!(lookup(&list, "2x"), Some("2xb/2xt"));
    }

    #[test]
    fn storing_is_its_own_chain() {
        assert_eq!(chain_for("0", ""), "storing");
    }

    /// -m1 is the level this port already writes byte-identically, so its chain
    /// is known from the archives: 4x4:tor:3.
    #[test]
    fn m1_expands_to_the_chain_its_archives_contain() {
        assert_eq!(chain_for("1", ""), "4x4:tor:3");
    }

    /// The chains -m4 writes, taken from `arc lt` on a reference archive. These
    /// are before the data-size fitting, so they carry the table's own sizes.
    #[test]
    fn m4_expands_to_the_chains_its_archives_contain() {
        // The default type: rep, the exe filter, delta, then 4x4 over lzma.
        // 4x4's inner method stays VERBATIM here: _4x4_METHOD's printer emits
        // the stored string, and only set_dictionary rebuilds it. So the
        // canonical inner form appears after fitting, not before.
        assert_eq!(
            chain_for("4", ""),
            "rep:96mb+exe+delta+4x4:b16mb:lzma:16m:h64m:normal:mc16"
        );
        // $text: dict, lzp, ppmd.
        assert_eq!(
            chain_for("4", "$text"),
            "dict:64mb:80%:l8192:m400:s100+lzp:64mb:90%:65:h20:d1mb:s16+ppmd:8:96mb"
        );
    }

    /// The end-to-end check: expanding -m4 and then fitting it to the block the
    /// reference actually produced must give the string the reference wrote.
    /// 382 992 bytes is the main-chain block on the generated corpus.
    #[test]
    fn m4_fitted_to_its_block_matches_the_reference_string() {
        let chain = chain_for("4", "");
        let fitted = crate::memlimit::fit_to_data(&chain, 382_992).expect("fits");
        assert_eq!(fitted, "rep:379kb+exe+delta+4x4:b16mb:lzma:379kb:mc16");

        let text = chain_for("4", "$text");
        let fitted = crate::memlimit::fit_to_data(&text, 55_752).expect("fits");
        assert_eq!(
            fitted,
            "dict:56kb:80%:l8192:m400:s100+lzp:56kb:90%:65:h16:d1mb:s16+ppmd:8:96mb"
        );
    }

    /// subst2 keeps a link's parameters when it substitutes its NAME.
    #[test]
    fn substitution_preserves_a_links_parameters() {
        let list = builtin_substs();
        // "4binary" has no parameters of its own.
        assert_eq!(subst2(&list, "4binary").join("+"), "4x4:b16m:lzma:16m:h64m:normal:mc16");
        // A key with parameters keeps them: "bcj = exe" applied to "bcj".
        assert_eq!(subst2(&list, "bcj").join("+"), "exe");
    }

    /// split_to_methods' two-chain form PREPENDS the exe filter to the default
    /// type -- something no user wrote.
    #[test]
    fn the_two_chain_form_prepends_the_exe_filter() {
        let got = split_to_methods("A/B");
        assert_eq!(got[0], (String::new(), "exe+A".to_string()));
        assert_eq!(got[1], ("$obj".to_string(), "A".to_string()));
        assert_eq!(got[2], ("$text".to_string(), "B".to_string()));
    }

    /// ...while a form whose second part starts with '$' does not.
    #[test]
    fn a_dollar_form_leaves_the_default_chain_alone() {
        let got = split_to_methods("A/$text=B");
        assert_eq!(got[0], (String::new(), "A".to_string()));
        assert_eq!(got[1], ("$text".to_string(), "B".to_string()));
    }

    /// Every level the port might be asked for must expand to something, and to
    /// methods the port can actually parse.
    #[test]
    fn every_numbered_level_expands_to_parseable_methods() {
        for level in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "x"] {
            let decoded = decode_method(level);
            assert!(!decoded.is_empty(), "-m{level} expanded to nothing");
            for (ty, chain) in &decoded {
                assert!(!chain.is_empty(), "-m{level} {ty} is empty");
                for m in chain {
                    assert!(
                        crate::method::Method::parse(m).is_some(),
                        "-m{level} {ty}: {m} does not parse"
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod table_pin {
    //! A snapshot of what the built-in table means TODAY, taken before it is
    //! rewritten from a `&[&str]` array into structured data.
    //!
    //! Steps 2 and 3 of the darc.toml work change how the table is stored and
    //! must change nothing about what it decodes to. This is the gate that says
    //! so: it renders every preset the table can express and compares against a
    //! fixture recorded from the current implementation. It is deliberately
    //! mechanical -- no cleverness, because its only job is to notice movement.
    //!
    //! If a row here changes, the refactor is wrong. Re-blessing the fixture to
    //! make it pass would discard the only evidence that the rewrite preserved
    //! meaning, exactly as re-recording golden/manifest.txt from the port would.
    use super::decode_method;

    /// Every spec the presets and their modifiers can name.
    fn all_specs() -> Vec<String> {
        let mut out = Vec::new();
        for lvl in 0..=9 {
            out.push(lvl.to_string());
            for m in ["x", "p", "q", "b", "t", "xb", "xt", "pb", "pt", "qb", "qt"] {
                out.push(format!("{lvl}{m}"));
            }
        }
        // The named groups and synonyms, which no level reaches directly.
        for n in [
            "wav", "wavfast", "bmp", "bmpfast", "bmpfastest", "bcj", "exe", "lzma", "tor",
            "$wav", "$bmp", "$obj", "$text", "$compressed", "$binary",
        ] {
            out.push(n.to_string());
        }
        out
    }

    /// One line per spec: `spec => type=chain|type=chain|...`.
    fn render() -> String {
        let mut s = String::new();
        for spec in all_specs() {
            let decoded = decode_method(&spec);
            let body: Vec<String> = decoded
                .iter()
                .map(|(ty, chain)| format!("{ty}={}", chain.join("+")))
                .collect();
            s.push_str(&format!("{spec} => {}\n", body.join("|")));
        }
        s
    }

    #[test]
    fn the_builtin_table_decodes_as_it_always_has() {
        let fixture = include_str!("../tests/method-table.pin");
        let now = render();
        if now != fixture {
            // Name the first differing line rather than dumping both files.
            for (i, (a, b)) in now.lines().zip(fixture.lines()).enumerate() {
                assert_eq!(a, b, "method table moved at line {}", i + 1);
            }
            assert_eq!(now.lines().count(), fixture.lines().count(), "row count moved");
        }
    }

    /// Writes the fixture. Run with the env var set ONLY when the table is
    /// deliberately being given new content -- never to silence a failure.
    #[test]
    fn bless() {
        match std::env::var("DARC_BLESS_METHOD_TABLE") {
            Ok(_) => std::fs::write(
                concat!(env!("CARGO_MANIFEST_DIR"), "/tests/method-table.pin"),
                render(),
            )
            .expect("write fixture"),
            Err(_) => {}
        }
    }
}

#[cfg(test)]
mod user_rows {
    //! `darc.toml`'s `[methods]` applied over the built-in table.
    use super::{decode_method, decode_method_with, substs_with_user};

    fn row(k: &str, v: &str) -> (String, String) {
        (k.to_string(), v.to_string())
    }

    fn chain(spec: &str, rows: &[(String, String)]) -> String {
        let list = substs_with_user(rows);
        decode_method_with(spec, &list)
            .into_iter()
            .find(|(ty, _)| ty.is_empty())
            .map(|(_, c)| c.join("+"))
            .unwrap_or_default()
    }

    /// No rows means the built-in table, unchanged. This is the property the
    /// 93 golden hashes depend on.
    #[test]
    fn an_empty_config_changes_nothing() {
        for spec in ["1", "4", "9", "5x", "9binary"] {
            assert_eq!(
                decode_method_with(spec, &substs_with_user(&[])),
                decode_method(spec),
                "-m{spec} moved with no user rows"
            );
        }
    }

    /// A user row shadows the built-in of the same key.
    #[test]
    fn a_user_row_wins_over_the_builtin() {
        let before = chain("9", &[]);
        let after = chain("9", &[row("9", "lzma:1m")]);
        assert_ne!(before, after, "the override did nothing");
        assert_eq!(after, "lzma:1mb");
    }

    /// The ordering rule: rows WITHOUT `#` beat rows with it, and that must
    /// hold ACROSS the user/built-in boundary in both directions.
    ///
    /// A general user row must not shadow a specific built-in one, and a
    /// specific user row must beat a general built-in one. Get the halves
    /// wrong and nothing fails to compile — the wrong chain is simply written
    /// into the archive.
    #[test]
    fn specific_beats_general_across_the_user_boundary() {
        // `1x` exists as a plain built-in row. A general `#x` from the user
        // must NOT displace it.
        let plain_builtin = chain("1x", &[]);
        assert_eq!(
            chain("1x", &[row("#x", "lzma:1m")]),
            plain_builtin,
            "a general user row displaced a specific built-in one"
        );
        // The reverse: a specific user row beats a general built-in one.
        // `2$wav` is only defined by the general `#$wav` row.
        let with_specific = substs_with_user(&[row("2$wav", "tta:m9")]);
        let got = decode_method_with("2", &with_specific);
        let wav = got.iter().find(|(ty, _)| ty == "$wav").map(|(_, c)| c.join("+"));
        assert_eq!(wav, Some("tta:m9".to_string()), "a specific user row lost to a general built-in");
    }

    /// A `#` row from the user expands into nine, exactly as a table line
    /// does. Without that, `#$wav` would be filed under a literal `#`.
    #[test]
    fn a_user_hash_row_expands_into_nine() {
        let rows = [row("#$wav", "tta:m1")];
        for lvl in ["3", "7", "9"] {
            let got = decode_method_with(lvl, &substs_with_user(&rows));
            let wav = got.iter().find(|(ty, _)| ty == "$wav").map(|(_, c)| c.join("+"));
            assert_eq!(wav, Some("tta:m1".to_string()), "-m{lvl} did not take the user's #$wav");
        }
    }
}
