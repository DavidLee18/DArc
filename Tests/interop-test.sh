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

# Codecs allowed to fail, named by the caller in DARC_INTEROP_XFAIL. Kept in
# CASES rather than deleted from it, because a quietly shortened list is
# indistinguishable from full coverage six months later -- every run prints
# these as xfail, so the gap stays visible in the log.
#
# An xfail that PASSES is reported as an error, not a success: the list is a
# claim about what is broken, and a stale entry understates coverage just as
# badly as deleting the case would.
#
# The default is empty, and it has to be, because whether a codec interoperates
# is a property of the PAIR of builds involved. tta below is fine when a build
# reads its own archives and only fails across the Linux/Windows boundary; a
# list baked into this script would turn every same-build run into a false
# XPASS. So each CI call site names what it expects to fail.
#
# Currently expected, set by the cross-platform check steps in build.yml:
#
#   tta   -mtta does not work on Windows at all. This started as an interop
#         failure -- both Windows builds rejected a TTA archive written by the
#         Linux build -- but the self-round-trip step settled it: neither
#         Windows build can even CREATE a tta archive, so there is no interop
#         question here. It is a codec that is broken on Windows.
#
#         It had simply never been run there. win-test.sh covers -m0/-m1/-m4
#         and run-tests.sh does not run on Windows, so nothing ever invoked
#         -mtta on a Windows build; this suite is the first thing to try.
#
#         Both targets fail identically -- GCC-mingw on amd64 and Clang on
#         arm64 -- so it is a Windows/Linux difference rather than a compiler
#         one, which points at LLP64 (long is 4 bytes) against LP64 (8). TTA
#         has a long history of exactly that family: nine such bugs were fixed
#         in it for v2.0.0, every one validated by round-tripping on LP64
#         hosts only. Tracked as follow-up work; it is a pre-existing defect,
#         not a regression from anything in this suite.
XFAIL="${DARC_INTEROP_XFAIL:-}"

is_xfail () {
  case " $XFAIL " in *" $1 "*) return 0 ;; *) return 1 ;; esac
}

hash_of  () { $SHA "$1" 2>/dev/null | cut -d' ' -f1; }
tree_hash () {   # order-independent hash of a directory's file contents+names
  [ -d "$1" ] || { echo "no-such-dir"; return; }
  ( cd "$1" && find . -type f -print0 2>/dev/null | LC_ALL=C sort -z |
    while IFS= read -r -d '' f; do printf '%s  %s\n' "$(hash_of "$f")" "$f"; done ) | $SHA | cut -d' ' -f1
}

# Show why the archiver failed. "tail -3" is not enough: DArc prints a version
# and host banner after an error, so the last three lines of a failed run are
# "Version: Windows 10 / Host system: Linux / Host version: ..." and the actual
# message scrolls past. Prefer lines that look like the diagnosis, and fall
# back to the tail only when none match.
why () {  # why <logfile>
  grep -iE 'error|exception|failed|cannot|unable|not supported' "$1" 2>/dev/null |
    grep -viE '^ *(Version|Host system|Host version):' | head -3 |
    sed 's/^/      /'
  grep -qiE 'error|exception|failed|cannot|unable|not supported' "$1" 2>/dev/null ||
    tail -3 "$1" | sed 's/^/      /'
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
  xfail=0; xpass=0
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
      if is_xfail "$label"; then
        printf '  %-20s XPASS created -- remove it from XFAIL\n' "$label"
        xpass=$((xpass+1))
      else
        printf '  %-20s %s\n' "$label" "$(hash_of "$arc")"
      fi
    elif is_xfail "$label"; then
      printf '  %-20s xfail cannot create (known, see XFAIL in %s)\n' "$label" "$(basename "$0")"
      why "$WORK/$label.log"
      xfail=$((xfail+1))
    else
      printf '  %-20s CREATE FAILED\n' "$label"
      why "$WORK/$label.log"
    fi
  done <<< "$CASES"
  # Counted from disk rather than from the loop, so the pass condition rests on
  # files that exist rather than on a variable. "CREATE FAILED printed three
  # times, exit status 0" is a defect this repo has already shipped once.
  made="$(ls "$DIR"/*.arc 2>/dev/null | wc -l | tr -d ' ')"
  want="$(echo "$CASES" | grep -c ':')"
  ( cd "$DIR" && $SHA ./*.arc > SHA256SUMS 2>/dev/null )
  echo "wrote $made/$want archives to $DIR ($xfail known-broken)"
  if [ "$xpass" -gt 0 ]; then
    echo "error: $xpass archive(s) marked xfail were created -- update XFAIL in $0" >&2
    exit 1
  fi
  [ "$made" -eq "$((want - xfail))" ] ||
    { echo "error: $((want - xfail - made)) archive(s) were not created" >&2; exit 1; }
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

  fail=0; xfail=0; xpass=0

  # Report an outcome, routing it through the xfail list. Everything below
  # reports by calling this rather than by touching the counters directly, so
  # a known-broken codec cannot accidentally be counted as a hard failure or
  # -- worse -- pass silently once it is fixed.
  bad () {   # bad <label> <reason>
    if is_xfail "$1"; then
      printf '  %-20s xfail %s (known, see XFAIL in %s)\n' "$1" "$2" "$(basename "$0")"
      xfail=$((xfail+1))
    else
      printf '  %-20s FAIL  %s\n' "$1" "$2"
      fail=$((fail+1))
    fi
  }

  for arc in "$DIR"/*.arc; do
    label="$(basename "$arc" .arc)"
    out="$WORK/x-$label"
    rm -rf "$out"; mkdir -p "$out"

    if ! "$ARC" t -y "$(winpath "$arc")" >"$WORK/$label.t.log" 2>&1; then
      bad "$label" "integrity test rejected the archive"
      why "$WORK/$label.t.log"
      continue
    fi
    if ! "$ARC" x -y -dp"$(winpath "$out")" "$(winpath "$arc")" >"$WORK/$label.x.log" 2>&1; then
      bad "$label" "extract failed"
      why "$WORK/$label.x.log"
      continue
    fi

    # Entries are stored corpus-relative, so the tree lands at the extraction
    # root itself. Derived, not searched for by name: "find -name corpus -quit"
    # is what once made this class of check report phantom empty directories.
    n_got="$(find "$out" -type f 2>/dev/null | wc -l | tr -d ' ')"
    if [ "$n_got" -eq 0 ]; then
      bad "$label" "extracted no files (expected $n_want)"
      continue
    fi
    got="$(tree_hash "$out")"
    if [ "$got" != "$EXPECTED_TREE" ]; then
      detail="content differs"
      [ "$n_got" -ne "$n_want" ] && detail="file count $n_got, expected $n_want"
      bad "$label" "extracted tree differs: $detail"
      ( cd "$CORPUS" && find . -type f | LC_ALL=C sort ) > "$WORK/$label.want" 2>/dev/null
      ( cd "$out"    && find . -type f | LC_ALL=C sort ) > "$WORK/$label.got"  2>/dev/null
      diff "$WORK/$label.want" "$WORK/$label.got" 2>/dev/null | head -4 | sed 's/^/      /'
      continue
    fi
    if is_xfail "$label"; then
      printf '  %-20s XPASS %s files -- remove it from XFAIL\n' "$label" "$n_got"
      xpass=$((xpass+1))
    else
      printf '  %-20s ok    %s files\n' "$label" "$n_got"
    fi
    rm -rf "$out"
  done

  echo
  # An xfail that started passing is reported as an error on purpose. The list
  # is a claim about what is broken; leaving a stale entry in it understates
  # coverage just as badly as deleting the case would.
  if [ "$xpass" -gt 0 ]; then
    echo "interop: $xpass archive(s) marked xfail now pass -- update XFAIL in $0" >&2
    # Named separately rather than returned to: a summary that mentioned only
    # the stale xfail would bury a genuine regression in the same run.
    [ "$fail" -gt 0 ] && echo "interop: and $fail archive(s) failed outright" >&2
    exit 1
  fi
  if [ "$fail" -eq 0 ]; then
    if [ "$xfail" -gt 0 ]; then
      echo "interop: $((n_arc-xfail))/$n_arc read back identically, $xfail known-broken (xfail)"
    else
      echo "interop: all $n_arc archive(s) read back identically"
    fi
    exit 0
  fi
  echo "interop: $fail of $n_arc archive(s) failed ($xfail known-broken)" >&2
  exit 1
  ;;

*) usage ;;
esac
