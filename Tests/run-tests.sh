#!/usr/bin/env bash
# Round-trip and archive-format regression tests.
#
# Two separate questions are being asked here, and they fail for different
# reasons:
#
#   1. ROUND-TRIP  -- does "create, test, extract" return the original bytes?
#      Catches ordinary bugs. A failure means data loss.
#
#   2. FINGERPRINT -- do we still emit the *same archive bytes* as before?
#      Catches format drift. A change here is not necessarily a bug, but it
#      means archives written by this build may not be readable by other
#      builds of DArc/FreeArc, which is the highest-risk failure mode in this
#      project because everything still compiles and every round-trip passes.
#
# Fingerprints depend on --nodates: without it the archive embeds mtimes and
# the bytes differ on every run.
#
# Usage:
#   ./run-tests.sh [path-to-arc]          run the suite
#   ./run-tests.sh [path-to-arc] --bless  regenerate fingerprints.txt
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ARC="${1:-$HERE/arc}"
BLESS="${2:-}"
CORPUS="$HERE/corpus"
WORK="$HERE/.work"
FP="$HERE/fingerprints.txt"

[ -x "$ARC" ] || { echo "error: archiver not found or not executable: $ARC" >&2; exit 2; }

command -v sha256sum >/dev/null && SHA=sha256sum || SHA="shasum -a 256"
hash_of () { $SHA "$1" | cut -d' ' -f1; }

# Hash a directory tree by content AND relative path, so a file landing in the
# wrong place fails even if its bytes are intact.
tree_hash () {
  [ -d "$1" ] || { echo "no-such-dir"; return; }
  ( cd "$1" && find . -type f -print0 2>/dev/null | LC_ALL=C sort -z |
    while IFS= read -r -d '' f; do printf '%s  %s\n' "$(hash_of "$f")" "$f"; done ) | $SHA | cut -d' ' -f1
}

[ -d "$CORPUS" ] || "$HERE/make-corpus.sh" "$CORPUS" >/dev/null
EXPECTED_TREE="$(tree_hash "$CORPUS")"

# method:label pairs. Labels are used as filenames and fingerprint keys, so
# they must stay stable even if the switch spelling changes.
CASES="
-m0:store
-m1:m1
-m4:m4
-m9:m9
-m4x:m4x
-mtor:tor
-mlzma:lzma
-mppmd:ppmd
-mgrzip:grzip
-mrep+lzma:chain-rep-lzma
-mdelta+lzma:chain-delta-lzma
-m4 -s-:nonsolid
-m4 -s:fullsolid
-m4 -se:solid-by-ext
-m4 -ms:store-compressed
-m4 -mt1:single-thread
"

pass=0; fail=0; drift=0
declare -a NEWFP=()

rm -rf "$WORK"; mkdir -p "$WORK"

# ---------------------------------------------------------------------------
# Preflight. When every configuration fails identically the useful question is
# not "which codec broke" but "what is the smallest input that still breaks".
# These narrow it down in one CI round-trip instead of several.
# ---------------------------------------------------------------------------
probe () {  # probe <description> <dir> <extra-opts...>
  local desc="$1" src="$2"; shift 2
  local a="$WORK/probe.arc"; rm -f "$a"
  if "$ARC" a -y "$@" "$a" "$src" >"$WORK/probe.log" 2>&1; then
    printf '  %-38s ok\n' "$desc"
  else
    printf '  %-38s FAIL: %s\n' "$desc" \
      "$(grep -iE 'exception|error' "$WORK/probe.log" | head -1 | cut -c1-70)"
  fi
}

mkdir -p "$WORK/tiny" && echo hello > "$WORK/tiny/one.txt"
mkdir -p "$WORK/few"  && for i in 1 2 3; do echo "file $i" > "$WORK/few/f$i.txt"; done

echo "preflight (input axis):"
probe "1 file,  -m0, no --nodates"      "$WORK/tiny"
probe "corpus,  -m0, -r, --nodates"     "$CORPUS"    -r --nodates

# Input size, file count and --nodates were already ruled out: a single
# six-byte file fails exactly like the full corpus. So bisect the *stage*
# instead. Each of these disables one thing that runs between the banner and
# the compression pipeline; whichever one starts passing names the culprit.
echo
echo "preflight (stage bisect):"
probe "-i0   no progress indicator"     "$WORK/tiny" -i0
probe "-di   minimal display"           "$WORK/tiny" -di
probe "-lc-  no compression mem limit"  "$WORK/tiny" -lc-
probe "-ld-  no decompression limit"    "$WORK/tiny" -ld-
probe "-mt1  single thread"             "$WORK/tiny" -mt1
probe "-s-   non-solid"                 "$WORK/tiny" -s-

# Partition the remaining space. No stage flag helped, and `l` fails with a
# path error rather than an overflow, so the fault is specific to creation.
# These separate container setup from per-file work, and I/O from pure setup.
raw () {  # raw <label> <args...>
  local label="$1"; shift
  if "$ARC" "$@" >"$WORK/probe.log" 2>&1; then
    printf '  %-38s ok\n' "$label"
  else
    printf '  %-38s %s\n' "$label" \
      "$(grep -iE 'overflow|exception|error' "$WORK/probe.log" | head -1 | cut -c1-58)"
  fi
}

mkdir -p "$WORK/empty"
echo
echo "preflight (partition):"
raw "--print-config (no archive at all)"  --print-config
raw "a on an EMPTY dir"                   a -y -m0 "$WORK/e0.arc" "$WORK/empty"
raw "a on ONE named file (no dir scan)"   a -y -m0 "$WORK/e1.arc" "$WORK/tiny/one.txt"
raw "a writing to /tmp"                   a -y -m0 /tmp/e2.arc "$WORK/tiny/one.txt"
raw "t on a non-archive"                  t -y "$WORK/tiny/one.txt"
raw "lb on a non-archive"                 lb "$WORK/tiny/one.txt"

# How far did it get? DArc writes into a temp file and renames on success, so
# whatever is left behind after the crash bounds the failure point far more
# precisely than reading source does:
#   nothing        -> died before opening the output
#   zero bytes     -> opened it, died before the first write
#   some bytes     -> got through the SFX/header write, died later
echo
echo "  forensics after a failed create:"
rm -rf "$WORK/forensic"; mkdir -p "$WORK/forensic"
( cd "$WORK/forensic" && "$ARC" a -y -m0 out.arc "$WORK/tiny/one.txt" >/dev/null 2>&1 )
if [ -z "$(ls -A "$WORK/forensic" 2>/dev/null)" ]; then
  echo "    (no files left behind - died before creating any output)"
else
  ls -la "$WORK/forensic" | tail -n +2 | sed 's/^/    /'
fi
echo

printf '%-24s %-10s %-10s %s\n' TEST ROUNDTRIP FORMAT DETAIL
printf '%s\n' "------------------------------------------------------------------"

while IFS= read -r line; do
  [ -z "$line" ] && continue
  opts="${line%%:*}"; label="${line##*:}"
  arc="$WORK/$label.arc"; out="$WORK/out-$label"

  # Show why a step failed, right here. A harness that says "see some.log"
  # is useless on CI, where that file is never seen by anyone.
  show_fail () {  # show_fail <stage> <logfile> [exit-status]
    local why="$1 failed"
    # Decode signal deaths. 128+N is a signal; bash prints its own "Bus error"
    # line to the terminal but that never reaches the CI log, so name it here.
    case "${4:-}" in
      134) why="$1 died on SIGABRT (128+6)" ;;
      138) why="$1 died on SIGBUS (128+10) - likely unaligned access" ;;
      139) why="$1 died on SIGSEGV (128+11)" ;;
      *) ;;
    esac
    printf '%-24s %-10s %-10s %s\n' "$label" FAIL - "$why"
    echo "    command: $2"
    sed 's/^/    | /' "$3" | head -12
  }

  # --nodates is what makes the archive bytes reproducible.
  "$ARC" a --nodates -r -y $opts "$arc" "$CORPUS" >"$WORK/$label.create.log" 2>&1
  st=$?
  if [ $st -ne 0 ]; then
    show_fail create "$ARC a --nodates -r -y $opts $arc $CORPUS" "$WORK/$label.create.log" "$st"
    fail=$((fail+1)); continue
  fi

  if ! "$ARC" t -y "$arc" >"$WORK/$label.test.log" 2>&1; then
    show_fail "integrity test" "$ARC t -y $arc" "$WORK/$label.test.log"
    fail=$((fail+1)); continue
  fi

  mkdir -p "$out"
  if ! "$ARC" x -y -dp"$out" "$arc" >"$WORK/$label.extract.log" 2>&1; then
    show_fail extract "$ARC x -y -dp$out $arc" "$WORK/$label.extract.log"
    fail=$((fail+1)); continue
  fi

  # The extracted tree is rooted wherever the corpus path put it; find the
  # directory that actually contains the payload rather than assuming a layout.
  # -print -quit rather than piping to head, which closed the pipe under find
  # and produced "find: write error" noise on every case.
  root="$(find "$out" -type d -name "$(basename "$CORPUS")" -print -quit 2>/dev/null)"
  [ -n "$root" ] || root="$out"

  # Distinguish "extracted nothing" from "extracted the wrong bytes". Both used
  # to report as "tree differs", which hid the difference between a failed
  # extraction and a corrupted one.
  n_got="$(find "$root" -type f 2>/dev/null | wc -l | tr -d ' ')"
  n_want="$(find "$CORPUS" -type f 2>/dev/null | wc -l | tr -d ' ')"
  if [ "$n_got" -eq 0 ]; then
    printf '%-24s %-10s %-10s %s\n' "$label" FAIL - "extract produced no files (expected $n_want)"
    fail=$((fail+1)); continue
  fi

  got="$(tree_hash "$root")"
  if [ "$got" != "$EXPECTED_TREE" ]; then
    detail="content differs"
    [ "$n_got" -ne "$n_want" ] && detail="file count $n_got, expected $n_want"
    printf '%-24s %-10s %-10s %s\n' "$label" FAIL - "extracted tree differs: $detail"
    # name the first few files that actually differ, so the log says which
    ( cd "$CORPUS" && find . -type f | LC_ALL=C sort ) > "$WORK/$label.want" 2>/dev/null
    ( cd "$root"   && find . -type f | LC_ALL=C sort ) > "$WORK/$label.got"  2>/dev/null
    diff "$WORK/$label.want" "$WORK/$label.got" 2>/dev/null | head -4 | sed 's/^/      /'
    fail=$((fail+1)); continue
  fi

  # Round-trip is good; now check format stability.
  afp="$(hash_of "$arc")"
  NEWFP+=("$label $afp")
  if [ -n "$BLESS" ]; then
    printf '%-24s %-10s %-10s %s\n' "$label" ok blessed "${afp:0:16}"
    pass=$((pass+1))
  elif [ -f "$FP" ]; then
    want="$(awk -v k="$label" '$1==k{print $2}' "$FP")"
    if [ -z "$want" ]; then
      printf '%-24s %-10s %-10s %s\n' "$label" ok NEW "no baseline yet: ${afp:0:16}"
      pass=$((pass+1))
    elif [ "$want" = "$afp" ]; then
      printf '%-24s %-10s %-10s %s\n' "$label" ok ok "${afp:0:16}"
      pass=$((pass+1))
    else
      printf '%-24s %-10s %-10s %s\n' "$label" ok DRIFT "${want:0:16} -> ${afp:0:16}"
      drift=$((drift+1))
    fi
  else
    printf '%-24s %-10s %-10s %s\n' "$label" ok "no-baseline" "${afp:0:16}"
    pass=$((pass+1))
  fi
done <<< "$CASES"

if [ -n "$BLESS" ]; then
  printf '%s\n' "${NEWFP[@]}" > "$FP"
  echo
  echo "wrote $(wc -l < "$FP" | tr -d ' ') fingerprints to $FP"
  exit 0
fi

echo
echo "round-trip: $pass passed, $fail failed"
[ "$drift" -gt 0 ] && cat <<EOF

$drift archive(s) changed format.

This is not automatically a bug -- a codec improvement legitimately changes
output. But it does mean archives from this build differ byte-for-byte from
the baseline, so verify old builds can still read them before accepting.
Re-bless with:  ./run-tests.sh <arc> --bless
EOF

[ "$fail" -gt 0 ] && exit 1
[ "$drift" -gt 0 ] && exit 3
echo "all good"
exit 0
