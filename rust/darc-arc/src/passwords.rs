//! Turning `-p`, `-hp`, `-kf`, `-op` and `-okf` into the two passwords a run
//! uses — `cookPasswords` (`Cmdline.hs:558`) and the `(dpwd, hpwd)` table above
//! it (`Cmdline.hs:534`).
//!
//! This is more than "read the option". Three things happen that a
//! straightforward reading would miss, and each of them is archive-visible:
//!
//! * **`-hp` sets both passwords.** `-hpSECRET` alone encrypts the data too,
//!   because the `("--", p) -> (p, p)` row of the table copies the headers
//!   password into the data slot. `-p` alone does *not* do the reverse.
//! * **Keyfile contents are appended to the password**, not used as a separate
//!   input. `-pPW -kfFILE` derives its key from `PW` ++ the file's bytes.
//! * **One prompt serves both passwords.** If either needs asking, the user is
//!   asked once and the answer is used for both.
//!
//! Prompting is injected rather than done here, so the table can be tested
//! without a terminal.

/// The passwords a command runs with, after cooking.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Passwords {
    /// `opt_data_password` — empty means data blocks are not encrypted.
    pub data: Vec<u8>,
    /// `opt_headers_password` — empty means directory and footer blocks are
    /// not encrypted.
    pub headers: Vec<u8>,
    /// The candidates tried when *reading* an encrypted block, in order.
    pub unpack: Vec<Vec<u8>>,
    /// Keyfile contents appended to each candidate while trying it. The empty
    /// keyfile is always tried as well and is not listed here.
    pub keyfiles: Vec<Vec<u8>>,
    /// `dont_ask_passwords` — `-p-`, `-hp-`, `-op-` all set it, and it means a
    /// block that needs an unknown password is an error rather than a prompt.
    pub dont_ask: bool,
}

/// What a run needs from the command line, so the table below can be exercised
/// without an `options::Parsed`.
pub struct Raw<'a> {
    /// `-p`, defaulting to `"--"` when absent.
    pub password: &'a str,
    /// `-hp`, defaulting to `"--"` when absent.
    pub headers_password: &'a str,
    /// Every `-p` given, in order.
    pub password_list: Vec<&'a str>,
    /// Every `-hp` given, in order.
    pub headers_list: Vec<&'a str>,
    /// Every `-op` given, plus the `-op…` spellings that arrive through `-o`.
    pub old_password_list: Vec<&'a str>,
    /// Contents of every `-okf` and `-kf` file — the DECRYPTION keyfiles.
    pub old_keyfiles: Vec<Vec<u8>>,
    /// Contents of the last `-kf` file — the ENCRYPTION keyfile. Empty when
    /// `-kf` was not given.
    pub keyfile: Vec<u8>,
    /// Whether `-op-`, `-p-` or `-hp-` was given.
    pub dont_ask: bool,
}

/// Which prompt the command needs: `ADD_CMD` asks for an encryption password
/// (twice, to catch typos), everything else asks for a decryption one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Prompt {
    Encryption,
    Decryption,
}

/// The `(dpwd, hpwd)` table, `Cmdline.hs:534`.
///
/// `"--"` is the marker for "not given" and, after `changeTo`, also for `-p-`.
/// The rows are tried in order and the first match wins, which is why
/// `("--","--")` falls into the first row and stays disabled rather than
/// reaching the catch-all.
pub fn split_passwords<'a>(password: &'a str, headers: &'a str) -> (&'a str, &'a str) {
    // changeTo [("-", "--")] -- `-p-` means the same as absent here; it is
    // `dont_ask` that carries its other meaning.
    let d = match password {
        "-" => "--",
        p => p,
    };
    let h = match headers {
        "-" => "--",
        p => p,
    };
    match (d, h) {
        (p, "--") => (p, "--"),
        ("--", p) => (p, p),
        (p, "") => (p, p),
        ("", p) => (p, p),
        (p1, p2) => (p1, p2),
    }
}

/// `cookPasswords` — resolve the raw options into [`Passwords`].
///
/// `ask` is called at most once, and only when a password has to be typed:
/// `-p?` always asks, and a bare `-p` asks only when no keyfile was given
/// (`askPwd`, `Cmdline.hs:553`). Its answer serves both passwords.
pub fn cook<F>(raw: &Raw<'_>, prompt: Prompt, ask: F) -> Passwords
where
    F: FnOnce(Prompt) -> String,
{
    let (dpwd, hpwd) = split_passwords(raw.password, raw.headers_password);

    // askPwd: "?" always, and "" only when there is no keyfile to stand in for
    // the password.
    let ask_pwd = |p: &str| p == "?" || (p.is_empty() && raw.keyfile.is_empty());
    let asked = match ask_pwd(dpwd) || ask_pwd(hpwd) {
        true => ask(prompt),
        false => String::new(),
    };

    let cook_one = |p: &str| -> Vec<u8> {
        match p {
            // `cook "--" = ""` -- encryption is disabled for this slot.
            "--" => Vec::new(),
            _ => {
                let mut out = match ask_pwd(p) {
                    true => crate::encryption::password_bytes(&asked),
                    false => crate::encryption::password_bytes(p),
                };
                out.extend_from_slice(&raw.keyfile);
                out
            }
        }
    };

    // The extraction candidates, in the order Cmdline.hs:547 builds them:
    // -op first, then -p, then -hp, with the placeholders removed.
    let mut unpack: Vec<Vec<u8>> = Vec::new();
    for p in raw
        .old_password_list
        .iter()
        .chain(raw.password_list.iter())
        .chain(raw.headers_list.iter())
    {
        match *p {
            "" | "?" | "-" | "--" => {}
            real => unpack.push(crate::encryption::password_bytes(real)),
        }
    }
    // A password the user typed goes to the FRONT, and so does the empty
    // password once any decryption keyfile exists -- a keyfile alone can be the
    // whole credential.
    if !raw.old_keyfiles.is_empty() {
        unpack.insert(0, Vec::new());
    }
    if !asked.is_empty() {
        unpack.insert(0, crate::encryption::password_bytes(&asked));
    }

    Passwords {
        data: cook_one(dpwd),
        headers: cook_one(hpwd),
        unpack,
        keyfiles: raw.old_keyfiles.clone(),
        dont_ask: raw.dont_ask,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raw<'a>(p: &'a str, hp: &'a str) -> Raw<'a> {
        Raw {
            password: p,
            headers_password: hp,
            password_list: match p {
                "--" => vec![],
                v => vec![v],
            },
            headers_list: match hp {
                "--" => vec![],
                v => vec![v],
            },
            old_password_list: vec![],
            old_keyfiles: vec![],
            keyfile: Vec::new(),
            dont_ask: false,
        }
    }

    fn never(_: Prompt) -> String {
        panic!("this case must not prompt")
    }

    /// The asymmetry that surprises: `-hp` encrypts the data as well, `-p`
    /// leaves the headers alone. Both directions are asserted so a "fix" that
    /// made them symmetric would fail here.
    #[test]
    fn hp_alone_sets_both_passwords_and_p_alone_does_not() {
        let d = cook(&raw("SECRET", "--"), Prompt::Encryption, never);
        assert_eq!(d.data, b"SECRET");
        assert_eq!(d.headers, b"", "-p must not encrypt the headers");

        let h = cook(&raw("--", "SECRET"), Prompt::Encryption, never);
        assert_eq!(h.data, b"SECRET", "-hp must encrypt the data too");
        assert_eq!(h.headers, b"SECRET");
    }

    #[test]
    fn two_different_passwords_stay_apart() {
        let both = cook(&raw("DATA", "HEAD"), Prompt::Encryption, never);
        assert_eq!(both.data, b"DATA");
        assert_eq!(both.headers, b"HEAD");
    }

    /// `-p-` disables encryption and additionally forbids prompting on the way
    /// back in.
    #[test]
    fn a_bare_dash_disables_encryption() {
        let none = cook(&raw("--", "--"), Prompt::Encryption, never);
        assert_eq!(none.data, b"");
        assert_eq!(none.headers, b"");
        let disabled = cook(&raw("-", "--"), Prompt::Encryption, never);
        assert_eq!(disabled.data, b"", "-p- is the same as absent for the slot");
    }

    /// The keyfile is appended to the password, not substituted for it, and a
    /// keyfile alone suppresses the prompt for a bare `-p`.
    #[test]
    fn the_keyfile_is_appended_and_suppresses_the_prompt() {
        let mut r = raw("PW", "--");
        r.keyfile = b"FILEBYTES".to_vec();
        assert_eq!(cook(&r, Prompt::Encryption, never).data, b"PWFILEBYTES".to_vec());

        let mut bare = raw("", "--");
        bare.keyfile = b"FILEBYTES".to_vec();
        assert_eq!(
            cook(&bare, Prompt::Encryption, never).data,
            b"FILEBYTES".to_vec(),
            "a keyfile alone is the whole credential"
        );
    }

    /// One prompt, both slots. Counted rather than assumed: asking twice would
    /// still produce a working archive and a worse experience, and asking once
    /// per slot with different answers would produce two different keys.
    #[test]
    fn a_single_prompt_answers_both_passwords() {
        let asks = std::cell::Cell::new(0);
        let r = raw("?", "?");
        let out = cook(&r, Prompt::Encryption, |_| {
            asks.set(asks.get() + 1);
            "TYPED".to_string()
        });
        assert_eq!(asks.get(), 1);
        assert_eq!(out.data, b"TYPED");
        assert_eq!(out.headers, b"TYPED");
        assert_eq!(out.unpack, vec![b"TYPED".to_vec()], "the typed password is a candidate");
    }

    /// A bare `-p` with no keyfile asks; `-pPW` does not.
    #[test]
    fn only_an_empty_or_question_mark_password_prompts() {
        let out = cook(&raw("", "--"), Prompt::Decryption, |_| "TYPED".to_string());
        assert_eq!(out.data, b"TYPED");
        assert_eq!(cook(&raw("PW", "--"), Prompt::Decryption, never).data, b"PW");
    }

    /// The candidate order is `-op`, then `-p`, then `-hp`, with the
    /// placeholders dropped — and keyfile bytes are NOT appended here, because
    /// the check tries each keyfile itself.
    #[test]
    fn the_extraction_candidates_keep_their_order_and_drop_the_placeholders() {
        let r = Raw {
            password: "P",
            headers_password: "H",
            password_list: vec!["P", "?"],
            headers_list: vec!["H", "--"],
            old_password_list: vec!["OLD", ""],
            old_keyfiles: vec![],
            keyfile: b"KF".to_vec(),
            dont_ask: false,
        };
        let out = cook(&r, Prompt::Decryption, never);
        assert_eq!(
            out.unpack,
            vec![b"OLD".to_vec(), b"P".to_vec(), b"H".to_vec()],
            "candidates carry no keyfile bytes"
        );
    }

    /// With decryption keyfiles present, the empty password becomes a candidate
    /// — the keyfile can be the entire credential on the way back in too.
    #[test]
    fn a_decryption_keyfile_adds_the_empty_password_as_a_candidate() {
        let mut r = raw("P", "--");
        r.old_keyfiles = vec![b"KF".to_vec()];
        let out = cook(&r, Prompt::Decryption, never);
        assert_eq!(out.unpack, vec![Vec::new(), b"P".to_vec()]);
        assert_eq!(out.keyfiles, vec![b"KF".to_vec()]);
    }
}
