#!/usr/bin/env bash
# Cross-build archive interoperability.
#
# run-tests.sh asks whether a build can read back its OWN archives. This asks
# the different question of whether builds can read each OTHER'S, which is the
# property users actually depend on and the one nothing was checking:
#
#     linux binary  --writes-->  archive  --reads-->  windows binary
#     windows binary --writes--> archive  --reads-->  linux binary
#
# It matters here because the Windows targets are built with a different
# compiler from everything else -- Debian's mingw-w64 (GCC) for amd64,
# llvm-mingw (Clang) for arm64 -- over the same codec sources. compile-win64-c
# already warns that PPMD in particular produces cross-incompatible streams if
# its flags do not match, and a build can pass every round-trip test while
# writing archives no other build can open. That is the highest-risk failure
# mode in this project precisely because everything looks green.
#
# The two halves run on different machines, so they are separate subcommands
# communicating through a directory that CI ships between jobs as an artifact:
#
#   interop-test.sh make  <arc> <corpus-dir> <out-dir>
#   interop-test.sh check <arc> <corpus-dir> <archive-dir>
#
# The corpus is passed in rather than regenerated on each side. make-corpus.sh
# is deterministic by design, but "deterministic" is exactly the assumption a
# cross-platform test should not be resting on -- if it ever drifted, this
# would report a format incompatibility that did not exist. Shipping the same
# bytes to both sides removes the question.
set -u

MODE="${1:-}"
ARC="${2:-}"
CORPUS="${3:-}"
DIR="${4:-}"

usage () {
  echo "usage: $0 make  <arc> <corpus-dir> <out-dir>" >&2
  echo "       $0 check <arc> <corpus-dir> <archive-dir>" >&2
  exit 2
}
[ -n "$MODE" ] && [ -n "$ARC" ] && [ -n "$CORPUS" ] && [ -n "$DIR" ] || usage

# Absolutise before anything cds, and so the executable check below names the
# path the user actually passed.
#
# This must NOT require the path to exist: in make mode the output directory
# legitimately does not yet. The obvious "cd $(dirname "$1") && pwd" form does
# require it, and fails in the worst possible way -- with cd's error
# suppressed, a missing parent makes the command substitution empty, so
# "interop/archives" becomes "/archives": still absolute, still plausible, and
# wrong. It got past a local test suite because every path used there happened
# to have an existing parent, and surfaced on CI as the archiver failing to
# open a temporary file.
abspath () {
  case "$1" in
    /*) echo "${1%/}" ;;
    *)  echo "${PWD%/}/${1#./}" ;;
  esac
}
ARC="$(abspath "$ARC")"
CORPUS="$(abspath "$CORPUS")"
DIR="$(abspath "$DIR")"

[ -x "$ARC" ]   || { echo "error: archiver not found or not executable: $ARC" >&2; exit 2; }
[ -d "$CORPUS" ] || { echo "error: corpus not found: $CORPUS" >&2; exit 2; }

command -v sha256sum >/dev/null && SHA=sha256sum || SHA="shasum -a 256"
WORK="${DARC_INTEROP_WORK:-${TMPDIR:-/tmp}/darc-interop-work}"

# Paths given to a native Windows .exe have to be Windows paths. MSYS rewrites
# some arguments on the way through and gets others wrong -- "-dp/tmp/x" is
# exactly the shape it mishandles, which is why win-test.sh sidesteps the
# question by staying in relative paths. That is not available here: the corpus
# and the archives deliberately live in different trees, so the paths must be
# absolute. Convert them explicitly instead of hoping. cygpath exists only on
# Windows shells, so this is the identity everywhere else.
winpath () {
  if command -v cygpath >/dev/null 2>&1; then cygpath -w "$1"; else echo "$1"; fi
}

# One archive per codec. Solid-mode and thread-count variants are deliberately
# absent: they change how data is grouped, not how any codec encodes it, so
# they add runtime without adding coverage of the thing under test. Every
# method that reaches C is represented once.
CASES="
-m0:store
-m1:m1
-m4:m4
-m9:m9
-m4x:m4x
-mtor:tor
-mlzp:lzp
-mtta:tta
-mlzma:lzma
-mppmd:ppmd
-mgrzip:grzip
-mdict -s-:dict
-mrep+lzma:chain-rep-lzma
-mdelta+lzma:chain-delta-lzma
"

hash_of  () { $SHA "$1" 2>/dev/null | cut -d' ' -f1; }
tree_hash () {   # order-independent hash of a directory's file contents+names
  [ -d "$1" ] || { echo "no-such-dir"; return; }
  ( cd "$1" && find . -type f -print0 2>/dev/null | LC_ALL=C sort -z |
    while IFS= read -r -d '' f; do printf '%s  %s\n' "$(hash_of "$f")" "$f"; done ) | $SHA | cut -d' ' -f1
}

rm -rf "$WORK"; mkdir -p "$WORK"
EXPECTED_TREE="$(tree_hash "$CORPUS")"
n_want="$(find "$CORPUS" -type f | wc -l | tr -d ' ')"

case "$MODE" in

# ---------------------------------------------------------------------------
make)
  mkdir -p "$DIR" || { echo "error: cannot create output directory: $DIR" >&2; exit 2; }
  rm -f "$DIR"/*.arc "$DIR"/SHA256SUMS 2>/dev/null
  echo "writing archives with $ARC"
  echo "corpus: $n_want files, tree $EXPECTED_TREE"
  # Herestring, not a pipeline: a "| while read" loop runs in a subshell in
  # most shells, so anything it counted would be discarded at the done.
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    opts="${line%%:*}"; label="${line##*:}"
    arc="$DIR/$label.arc"
    # --nodates so the bytes do not carry mtimes, and created from inside the
    # corpus so entries are stored corpus-relative -- both exactly as
    # run-tests.sh does it, so archives are comparable with its fingerprints.
    if ( cd "$CORPUS" && "$ARC" a --nodates -r -y $opts "$(winpath "$arc")" . ) >"$WORK/$label.log" 2>&1; then
      printf '  %-20s %s\n' "$label" "$(hash_of "$arc")"
    else
      printf '  %-20s CREATE FAILED\n' "$label"
      tail -3 "$WORK/$label.log" | sed 's/^/      /'
    fi
  done <<< "$CASES"
  # Counted from disk rather than from the loop, so the pass condition rests on
  # files that exist rather than on a variable. "CREATE FAILED printed three
  # times, exit status 0" is a defect this repo has already shipped once.
  made="$(ls "$DIR"/*.arc 2>/dev/null | wc -l | tr -d ' ')"
  want="$(echo "$CASES" | grep -c ':')"
  ( cd "$DIR" && $SHA ./*.arc > SHA256SUMS 2>/dev/null )
  echo "wrote $made/$want archives to $DIR"
  [ "$made" -eq "$want" ] || { echo "error: $((want-made)) archive(s) were not created" >&2; exit 1; }
  ;;

# ---------------------------------------------------------------------------
check)
  echo "reading archives with $ARC"
  echo "corpus: $n_want files, tree $EXPECTED_TREE"
  n_arc="$(ls "$DIR"/*.arc 2>/dev/null | wc -l | tr -d ' ')"
  # A check with nothing to check must fail. An earlier version of the Windows
  # smoke test went green while performing zero operations; the shape is easy
  # to reintroduce whenever the inputs arrive from another job.
  [ "$n_arc" -gt 0 ] || { echo "error: no .arc files in $DIR -- nothing was checked" >&2; exit 2; }

  fail=0
  for arc in "$DIR"/*.arc; do
    label="$(basename "$arc" .arc)"
    out="$WORK/x-$label"
    rm -rf "$out"; mkdir -p "$out"

    if ! "$ARC" t -y "$(winpath "$arc")" >"$WORK/$label.t.log" 2>&1; then
      printf '  %-20s FAIL  integrity test rejected the archive\n' "$label"
      tail -3 "$WORK/$label.t.log" | sed 's/^/      /'
      fail=$((fail+1)); continue
    fi
    if ! "$ARC" x -y -dp"$(winpath "$out")" "$(winpath "$arc")" >"$WORK/$label.x.log" 2>&1; then
      printf '  %-20s FAIL  extract\n' "$label"
      tail -3 "$WORK/$label.x.log" | sed 's/^/      /'
      fail=$((fail+1)); continue
    fi

    # Entries are stored corpus-relative, so the tree lands at the extraction
    # root itself. Derived, not searched for by name: "find -name corpus -quit"
    # is what once made this class of check report phantom empty directories.
    n_got="$(find "$out" -type f 2>/dev/null | wc -l | tr -d ' ')"
    if [ "$n_got" -eq 0 ]; then
      printf '  %-20s FAIL  extracted no files (expected %s)\n' "$label" "$n_want"
      fail=$((fail+1)); continue
    fi
    got="$(tree_hash "$out")"
    if [ "$got" != "$EXPECTED_TREE" ]; then
      detail="content differs"
      [ "$n_got" -ne "$n_want" ] && detail="file count $n_got, expected $n_want"
      printf '  %-20s FAIL  extracted tree differs: %s\n' "$label" "$detail"
      ( cd "$CORPUS" && find . -type f | LC_ALL=C sort ) > "$WORK/$label.want" 2>/dev/null
      ( cd "$out"    && find . -type f | LC_ALL=C sort ) > "$WORK/$label.got"  2>/dev/null
      diff "$WORK/$label.want" "$WORK/$label.got" 2>/dev/null | head -4 | sed 's/^/      /'
      fail=$((fail+1)); continue
    fi
    printf '  %-20s ok    %s files\n' "$label" "$n_got"
    rm -rf "$out"
  done

  echo
  if [ "$fail" -eq 0 ]; then
    echo "interop: all $n_arc archive(s) read back identically"
    exit 0
  fi
  echo "interop: $fail of $n_arc archive(s) failed" >&2
  exit 1
  ;;

*) usage ;;
esac
