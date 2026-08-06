#!/usr/bin/env bash
# The installer SFX: `darc a -sfx<module> --autorun'CMD'` must extract itself,
# ask, run CMD, propagate its exit code, and wipe the scratch directory -- and
# must run NOTHING in every other case.
#
#   usage: sfx-autorun-check.sh [darc] [unarc]
#
# There is no reference to compare against: FreeArc's installer SFX hardcoded
# `setup.exe`, was Windows-only, ran silently, and discarded the exit code, so
# there is no case where its behaviour and this one's are both defined. That
# makes this a property harness, not a differential one, and the properties
# that matter are the negative ones.
#
# The dangerous regression here is not "the payload did not run". It is "a
# payload ran when nothing asked for one", which would turn every ordinary SFX
# archive into a code-execution vector. So most of what follows checks that
# nothing happened, and the self-tests at the end prove those checks could have
# noticed if it had.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DARC="${1:-$ROOT/rust/target/release/darc}"
RUNARC="${2:-$ROOT/rust/target/release/unarc}"

( cd "$ROOT/rust" && cargo build --release -q -p darc-unarc -p darc-arc ) || {
  echo "cargo build failed" >&2; exit 1; }
for b in "$DARC" "$RUNARC"; do
  [ -x "$b" ] || { echo "no binary at $b" >&2; exit 1; }
done

W="${TMPDIR:-/tmp}/sfx-autorun-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

SENTINEL="$W/it-ran"
fail=0 checked=0

note()  { echo "  $*"; }
bad()   { echo "  FAIL: $*" >&2; fail=$((fail + 1)); }

# The tree that goes into the installer. `setup.sh` records that it ran, where
# it ran from, and what arguments it was handed; `fails.sh` exits 7.
mkdir -p "$W/src/bin"
cat > "$W/src/setup.sh" <<EOF
#!/bin/sh
printf 'ran cwd=%s args=%s\n' "\$(pwd)" "\$*" > "$SENTINEL"
exit 0
EOF
cat > "$W/src/fails.sh" <<EOF
#!/bin/sh
printf 'ran\n' > "$SENTINEL"
exit 7
EOF
printf 'payload\n' > "$W/src/bin/data.txt"
chmod +x "$W/src/setup.sh" "$W/src/fails.sh"

# How many scratch directories exist before any of this, so the wipe can be
# checked by counting rather than by guessing a name.
scratches() { find "${TMPDIR:-/tmp}" -maxdepth 1 -name 'darc-sfx-*' 2>/dev/null | wc -l | tr -d '[:space:]'; }
before_scratches=$(scratches)

# `-sfx` drops the .arc extension on Unix, so `x.arc` is written as `x`.
make_sfx() { # make_sfx <name> [extra darc options...]
  local name="$1"; shift
  rm -f "$W/$name" "$W/$name.arc"
  ( cd "$W/src" && "$DARC" a --nodates -y -r -m1 "-sfx$RUNARC" "$@" "$W/$name.arc" . )
}

echo "darc:  $DARC"
echo "unarc: $RUNARC (also the SFX module)"

# ── 1. it runs, from the right place, with the right arguments ──────────────
checked=$((checked + 1))
rm -f "$SENTINEL"
if ! make_sfx inst "--autorun=setup.sh --quiet" >"$W/create.log" 2>&1; then
  bad "creating an --autorun SFX failed"; sed 's/^/    /' "$W/create.log" >&2
else
  [ -x "$W/inst" ] || bad "no SFX at $W/inst"
  "$W/inst" -y >"$W/run.log" 2>&1; code=$?
  [ "$code" -eq 0 ] || { bad "autorun exited $code, expected 0"; sed 's/^/    /' "$W/run.log" >&2; }
  if [ -f "$SENTINEL" ]; then
    note "payload said: $(cat "$SENTINEL")"
    grep -q 'args=--quiet' "$SENTINEL" || bad "the payload's arguments were not passed through"
    # It must have run from the scratch directory, not from wherever the SFX
    # happened to be invoked: a payload that refers to its own files by
    # relative name depends on this.
    grep -q 'cwd=.*darc-sfx-' "$SENTINEL" || bad "the payload did not run in the scratch directory"
  else
    bad "the payload did not run"; sed 's/^/    /' "$W/run.log" >&2
  fi
fi

# ── 2. the child's exit code is propagated ──────────────────────────────────
# FreeArc discarded it, so a failed install reported success.
checked=$((checked + 1))
rm -f "$SENTINEL"
make_sfx failer "--autorun=fails.sh" >/dev/null 2>&1
"$W/failer" -y >/dev/null 2>&1; code=$?
[ "$code" -eq 7 ] || bad "a payload exiting 7 gave $code"

# ── 3. the scratch directory is gone ────────────────────────────────────────
checked=$((checked + 1))
after_scratches=$(scratches)
[ "$after_scratches" -eq "$before_scratches" ] || \
  bad "scratch directories leaked: $before_scratches before, $after_scratches after"

# ── 4. it does NOT run without a confirmation ───────────────────────────────
# stdin is /dev/null, so the prompt reads EOF. EOF must mean no: a
# non-interactive context is exactly where a prompt is not seen.
checked=$((checked + 1))
rm -f "$SENTINEL"
"$W/inst" </dev/null >"$W/noconfirm.log" 2>&1; code=$?
[ -f "$SENTINEL" ] && bad "it ran without being confirmed"
[ "$code" -ne 0 ] || bad "declining reported success (exit 0)"

# ── 5. naming a command disables it ─────────────────────────────────────────
# `x`, `e`, `l` and `t` mean what they say. Extracting an installer to look at
# it must not install it.
for c in x e t l; do
  checked=$((checked + 1))
  rm -f "$SENTINEL"; rm -rf "$W/look"; mkdir -p "$W/look"
  ( cd "$W/look" && "$W/inst" "-$c" -y ) >"$W/look.log" 2>&1; code=$?
  [ -f "$SENTINEL" ] && bad "-$c ran the payload"
  # Not vacuous: the command has to have WORKED. An SFX that failed to start
  # would also leave no sentinel, and would pass the line above having proved
  # nothing.
  [ "$code" -eq 0 ] || { bad "-$c on the SFX exited $code"; sed 's/^/    /' "$W/look.log" >&2; }
  case "$c" in
    x) [ -f "$W/look/setup.sh" ]     || bad "-x extracted nothing" ;;
    e) [ -f "$W/look/data.txt" ]     || bad "-e did not flatten bin/data.txt out" ;;
    # `l` names the files; `t` only counts them, so each is checked for what it
    # actually prints rather than for a string one of them never emits.
    l) grep -q 'setup\.sh' "$W/look.log" || bad "-l did not list the files" ;;
    *) grep -q 'Tested 4 files' "$W/look.log" || bad "-t did not test the files" ;;
  esac
done
# and the same through an explicit `unarc <cmd> <file>`
checked=$((checked + 1))
rm -f "$SENTINEL"; rm -rf "$W/look2"; mkdir -p "$W/look2"
( cd "$W/look2" && "$RUNARC" x "$W/inst" ) >/dev/null 2>&1
[ -f "$SENTINEL" ] && bad "unarc x on the SFX ran the payload"
[ -f "$W/look2/setup.sh" ] || bad "unarc x on the SFX extracted nothing"

# ── 6. darc never runs anything ─────────────────────────────────────────────
# The archiver reads an SFX file as an ordinary archive. Opening one to see
# what is inside must never be able to execute it.
for c in l t; do
  checked=$((checked + 1))
  rm -f "$SENTINEL"
  "$DARC" "$c" -y --noarcext "$W/inst" >/dev/null 2>&1
  [ -f "$SENTINEL" ] && bad "darc $c ran the payload"
done
checked=$((checked + 1))
rm -f "$SENTINEL"; rm -rf "$W/dx"; mkdir -p "$W/dx"
( cd "$W/dx" && "$DARC" x -y --noarcext "$W/inst" ) >/dev/null 2>&1
[ -f "$SENTINEL" ] && bad "darc x ran the payload"
[ -f "$W/dx/setup.sh" ] || bad "darc x on the SFX extracted nothing"

# ── 7. an SFX with no autorun runs nothing ──────────────────────────────────
# The regression that would make every ordinary SFX archive a code-execution
# vector. It has to extract, and it has to extract only.
checked=$((checked + 1))
rm -f "$SENTINEL"; rm -rf "$W/plain-out"; mkdir -p "$W/plain-out"
make_sfx plain >/dev/null 2>&1
( cd "$W/plain-out" && "$W/plain" -y ) >/dev/null 2>&1; code=$?
[ -f "$SENTINEL" ] && bad "an SFX with no --autorun ran the payload"
[ "$code" -eq 0 ] || bad "a plain SFX exited $code"
[ -f "$W/plain-out/setup.sh" ] || bad "a plain SFX extracted nothing"

# ── 8. the field costs nothing when it is not used ──────────────────────────
# `--autorun-` clears it, so the archive must be byte-identical to one written
# without the option at all. This is the property that keeps every recorded
# `ref` case in arc-golden-check.sh valid.
checked=$((checked + 1))
make_sfx cleared "--autorun-" >/dev/null 2>&1
cmp -s "$W/plain" "$W/cleared" || \
  bad "an archive with an empty autorun differs from one written without the option"

# ── 9. an escaping command is refused when the archive is WRITTEN ───────────
for evil in "../../../bin/sh" "/bin/sh" "a/../../b"; do
  checked=$((checked + 1))
  rm -f "$W/evil" "$W/evil.arc"
  make_sfx evil "--autorun=$evil" >"$W/evil.log" 2>&1; code=$?
  [ "$code" -eq 0 ] && bad "--autorun'$evil' was accepted"
  [ "$code" -ge 128 ] && bad "--autorun'$evil' crashed (exit $code)"
  [ -e "$W/evil" ] && bad "--autorun'$evil' still wrote an archive"
done

# ── 10. --autorun without an SFX module is refused ──────────────────────────
# Recording a command nothing will ever run is the silent no-op this project
# refuses everywhere else.
checked=$((checked + 1))
rm -f "$W/nosfx.arc"
( cd "$W/src" && "$DARC" a --nodates -y -r -m1 "--autorun=setup.sh" "$W/nosfx.arc" . ) \
  >/dev/null 2>&1; code=$?
[ "$code" -eq 0 ] && bad "--autorun without -sfx was accepted"
[ -e "$W/nosfx.arc" ] && bad "--autorun without -sfx still wrote an archive"

# ── 11. an update keeps it ──────────────────────────────────────────────────
# An update that silently disarmed an installer would be the same class of bug
# as the one that shortened archives by dropping the comment.
checked=$((checked + 1))
printf 'more\n' > "$W/src/extra.txt"
# `u`, not `a`: `darc a` on an existing archive REPLACES it -- the old files,
# the comment and the SFX stub all go -- which is a separate defect from
# anything this harness is about, and not one to paper over here.
( cd "$W/src" && "$DARC" u --nodates -y -m1 --noarcext "$W/inst" extra.txt ) >/dev/null 2>&1
rm -f "$SENTINEL"
"$W/inst" -y >/dev/null 2>&1
[ -f "$SENTINEL" ] || bad "an update dropped the autorun command"

# ── the self-tests: could any of the above have failed? ─────────────────────
#
# Nine of the eleven checks above pass when the sentinel is absent. If the
# sentinel could never appear -- a payload that does not run when run, a
# `$SENTINEL` path nothing can write, a `make_sfx` that quietly produces
# nothing -- then all nine are vacuous and this harness is green while testing
# nothing at all.

# The payload works when run directly.
rm -f "$SENTINEL"
( cd "$W" && "$W/src/setup.sh" self-test ) >/dev/null 2>&1
[ -f "$SENTINEL" ] || {
  echo "SELF-TEST FAILED: the payload does not write the sentinel even when run" >&2
  echo "directly, so every 'it did not run' check above was vacuous" >&2
  exit 1; }

# The SFX really is an SFX: a stub, then an archive, and the stub is unarc.
[ -s "$W/inst" ] || { echo "SELF-TEST FAILED: no SFX was built" >&2; exit 1; }
stub=$(wc -c < "$RUNARC" | tr -d '[:space:]')
head -c "$stub" "$W/inst" > "$W/prefix.bin"
cmp -s "$W/prefix.bin" "$RUNARC" || {
  echo "SELF-TEST FAILED: $W/inst does not begin with the unarc binary, so it is" >&2
  echo "not the SFX these checks think they are running" >&2
  exit 1; }

# The refusals in check 9 are refusals, not a broken command line that would
# fail whatever it was handed.
make_sfx sanity "--autorun=setup.sh" >/dev/null 2>&1 || {
  echo "SELF-TEST FAILED: a well-formed --autorun was refused too, so the" >&2
  echo "refusals above prove nothing about the path check" >&2
  exit 1; }

# And the byte comparison in check 8 can tell two archives apart.
cmp -s "$W/plain" "$W/sanity" && {
  echo "SELF-TEST FAILED: archives with and without an autorun command compare" >&2
  echo "equal, so check 8 was comparing nothing" >&2
  exit 1; }

echo "sfx-autorun: $checked checks, $fail failed"
[ "$fail" -eq 0 ] || exit 1
echo "the installer SFX runs what it says, only when asked, and nothing otherwise"
