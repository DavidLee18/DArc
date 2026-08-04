#!/usr/bin/env bash
# The config file's EFFECT, gated against the Haskell reference.
#
# `darc.toml` replaced `arc.ini`, so there is no oracle for the config syntax —
# nothing else in the world reads a darc.toml. But the syntax is not what
# matters: what matters is the archive that comes out. So each case states the
# same override twice, once as the reference's `[Compression methods]` and once
# as this port's `[methods]`, and requires the two archives to be identical.
#
# That is what catches a divergence in how the table is APPLIED — shadowing
# order, parameter preservation, multi-hop resolution — none of which the
# round-trip test in toml_table.rs can see, because that only proves a row
# renders to the right string.
#
# ── Two properties every case here needs ────────────────────────────────────
#
#   * The corpus must be big enough that the override survives. On a small tree
#     `limitDictionary` fits every chain to the data size, and `lzma:1m`,
#     `lzma:4m` and `lzma:8m` all collapse to the same archive — nine cases
#     produced four distinct hashes on the first attempt at this file.
#   * Distinct methods, not distinct parameters. Cases that differ only in a
#     dictionary size prove far less than cases that differ in codec.
#
# The run prints the distinct-hash count for exactly that reason. If it is
# lower than the case count, some cases are testing the same thing.
#
# Usage:  arc-config-check.sh /path/to/arc-ghc
set -uo pipefail

REF="${1:-}"
[ -n "$REF" ] || { echo "usage: $0 <reference-arc>" >&2; exit 2; }
[ -x "$REF" ] || { echo "no reference binary at $REF" >&2; exit 2; }
case "$REF" in /*) ;; *) REF="$(cd "$(dirname "$REF")" && pwd)/$(basename "$REF")" ;; esac

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || exit 1
PORT="$ROOT/rust/target/release/darc"

W="${TMPDIR:-/tmp}/arc-config-check.$$"; mkdir -p "$W/c"
trap 'rm -rf "$W"' EXIT

# ~11 MB of compressible-but-distinct text. Generated, not random, so it is the
# same on every machine; large enough that a dictionary setting still shows.
for i in 1 2 3; do
  awk -v n="$i" 'BEGIN{for(j=0;j<60000;j++) printf "block %d row %d of text\n", n, j%1000}' \
    > "$W/c/f$i.txt"
done

sha() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

pass=0; fail=0; hashes=()

# gate <name> <darc-args> <ini-body> <toml-body>
gate() {
  local name="$1" spec="$2" ini="$3" toml="$4"
  { echo "[Compression methods]"; printf '%s\n' "$ini"; } > "$W/arc.ini"
  printf '%s\n' "$toml" > "$W/darc.toml"
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/c" && "$REF"  a --nodates -y -r -cfg"$W/arc.ini"   $spec "$W/r.arc" . ) >/dev/null 2>&1
  ( cd "$W/c" && "$PORT" a --nodates -y -r -cfg"$W/darc.toml" $spec "$W/p.arc" . ) >/dev/null 2>&1
  if [ ! -f "$W/r.arc" ]; then
    echo "SKIP $name -- the reference wrote no archive"
    return
  fi
  if [ ! -f "$W/p.arc" ]; then
    printf 'DIFF [%s]: the port wrote no archive\n' "$name"
    fail=$((fail+1)); return
  fi
  local r p
  r="$(sha "$W/r.arc")"; p="$(sha "$W/p.arc")"
  hashes+=("$r $name")
  if [ "$r" = "$p" ]; then
    pass=$((pass+1))
  else
    printf 'DIFF [%s]: expected %s, got %s\n' "$name" "$r" "$p"
    fail=$((fail+1))
  fi
}

# A level redefined to each of four different codecs, so no two cases can
# collapse into one another.
gate alias-lzma   "-m9"    '9 = lzma:1m'        '[methods]
"9" = { alias = "lzma:1m" }'
gate alias-ppmd   "-m9"    '9 = ppmd:6:8m'      '[methods]
"9" = { alias = "ppmd:6:8m" }'
gate alias-tor    "-m9"    '9 = tor:3'          '[methods]
"9" = { alias = "tor:3" }'
gate alias-bsc    "-m9"    '9 = bsc:2m'         '[methods]
"9" = { alias = "bsc:2m" }'
# The structured form must mean the same as the string one.
gate chain-rep    "-m9"    '9 = rep:8m+lzma:1m' '[methods]
"9" = { chain = [ { codec = "rep", params = ["8m"] }, { codec = "lzma", params = ["1m"] } ] }'
# A name the built-in table does not have at all.
gate new-name     "-mmine" 'mine = lz4:hc'       '[methods]
mine = { alias = "lz4:hc" }'
# A `#` row: one line standing for nine.
gate general-row  "-m5x"   '#x = lzma:4m'       '[methods]
"#x" = { alias = "lzma:4m" }'
# Two hops -- the resolver must follow a row to another row.
gate indirect     "-m9"    '9 = 9z
9z = lzma:2m:BT4'                               '[methods]
"9" = { alias = "9z" }
"9z" = { alias = "lzma:2m:BT4" }'
# `-m9:8m` -- the parameters on the command line survive the substitution.
gate params-kept  "-m9:8m" '9 = lzma'           '[methods]
"9" = { alias = "lzma" }'
# A level the override must NOT touch. Shadowing that reaches too far is the
# failure this catches.
gate untouched-m1 "-m1"    '9 = lzma:1m'        '[methods]
"9" = { alias = "lzma:1m" }'

echo "arc config: $((pass+fail)) cases, $fail differing"
u=$(printf '%s\n' "${hashes[@]}" | cut -d' ' -f1 | sort -u | wc -l | tr -d ' ')
echo "distinct reference archives: $u of ${#hashes[@]}"
if [ "$u" -lt "${#hashes[@]}" ]; then
  # Naming them is the point: "some cases collide" is not actionable, and a
  # case that duplicates another is a case that tests nothing new.
  # `uniq -w` is GNU-only, so group with awk instead: BSD uniq on macOS takes
  # no width flag and would have printed its usage instead of the answer.
  echo "WARNING: these cases produce the SAME archive, so one tests nothing new:" >&2
  printf '%s\n' "${hashes[@]}" \
    | awk '{ n[$1]++; who[$1] = who[$1] " " $2 }
           END { for (h in n) if (n[h] > 1) printf "  %s:%s\n", substr(h,1,12), who[h] }' >&2
fi

# The self-test: the comparison must be able to fail. Without this the whole
# file could be passing because it compares nothing.
rm -f "$W/r.arc" "$W/p.arc"
{ echo "[Compression methods]"; echo "9 = lzma:1m"; } > "$W/arc.ini"
printf '[methods]\n"9" = { alias = "ppmd:6:8m" }\n' > "$W/darc.toml"
( cd "$W/c" && "$REF"  a --nodates -y -r -cfg"$W/arc.ini"   -m9 "$W/r.arc" . ) >/dev/null 2>&1
( cd "$W/c" && "$PORT" a --nodates -y -r -cfg"$W/darc.toml" -m9 "$W/p.arc" . ) >/dev/null 2>&1
if [ "$(sha "$W/r.arc")" = "$(sha "$W/p.arc")" ]; then
  echo "SELF-TEST FAILED: two deliberately different configs compared equal" >&2
  exit 1
fi

[ "$fail" -eq 0 ] || exit 1
echo "the Rust arc applies darc.toml exactly as the Haskell one applies arc.ini"
