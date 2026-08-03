#!/usr/bin/env bash
# Prove a corpusgen corpus is byte-identical to the python3 heredoc it replaces.
#
#   verify-corpus.sh <harness.sh> <corpus-name> [heredoc-index]
#
# This exists because "the corpus looks equivalent" is not a standard anything
# here can rely on. The difftest corpora are what the Rust codecs are compared
# against the C over; a corpus that quietly changes does not fail, it tests
# something else, and the harness still prints a pass. So each conversion is
# accepted on `cmp` over every file, against the bytes the Python actually
# wrote -- run BEFORE the heredoc is deleted from the harness.
#
# It reads the heredoc out of the harness with `git show HEAD:` when the file
# has already been converted in the working tree, so a conversion can be
# re-verified after the fact.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
harness="${1:?usage: verify-corpus.sh <harness.sh> <corpus-name> [index]}"
corpus="${2:?usage: verify-corpus.sh <harness.sh> <corpus-name> [index]}"
index="${3:-1}"

command -v python3 >/dev/null || {
  echo "verify-corpus: no python3 -- this script exists only to check the" >&2
  echo "  conversion against it, and cannot run once it is gone." >&2
  exit 2
}

W="${TMPDIR:-/tmp}/verify-corpus.$$"; mkdir -p "$W/py" "$W/rs"
trap 'rm -rf "$W"' EXIT

# The heredoc as it stands in the file, or as it stood at HEAD if the working
# tree no longer has it.
src="$ROOT/$harness"
[ -f "$src" ] || src="$harness"
extract() {
  awk -v want="$index" '
    /^[[:space:]]*python3 - .*<</ { n++; if (n==want) { grab=1; next } }
    grab && /^(PY|CORPUS|PYEOF|EOF|BIG|KDF)$/ { grab=0 }
    grab { print }
  ' "$1"
}
extract "$src" > "$W/gen.py"
if [ ! -s "$W/gen.py" ]; then
  # Already converted in the working tree: walk this file's history back to the
  # last revision that still had the heredoc. HEAD alone is not enough once the
  # conversion itself has been committed.
  for rev in $( cd "$ROOT" && git log --format=%H -- "$harness" | head -20 ); do
    ( cd "$ROOT" && git show "$rev:$harness" ) > "$W/old.sh" 2>/dev/null || continue
    extract "$W/old.sh" > "$W/gen.py"
    [ -s "$W/gen.py" ] && { echo "(heredoc recovered from ${rev:0:8})"; break; }
  done
fi
[ -s "$W/gen.py" ] || {
  echo "verify-corpus: no python3 heredoc #$index found in $harness" >&2
  exit 2
}

python3 "$W/gen.py" "$W/py" || { echo "verify-corpus: the python failed" >&2; exit 1; }
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) >/dev/null 2>&1 || exit 1
"$ROOT/rust/target/release/corpusgen" "$corpus" "$W/rs" || exit 1

npy=$(find "$W/py" -type f | wc -l | tr -d ' ')
nrs=$(find "$W/rs" -type f | wc -l | tr -d ' ')
if diff -r "$W/py" "$W/rs" >/dev/null 2>&1; then
  echo "$corpus: BYTE-IDENTICAL ($npy files)"
  exit 0
fi
echo "$corpus: MISMATCH ($npy python files, $nrs rust files)"
diff -rq "$W/py" "$W/rs" 2>&1 | head -20
exit 1
