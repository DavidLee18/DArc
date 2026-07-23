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
# Archives are created from inside "$CORPUS" (see the create step), so a
# relative "$ARC" such as ./arc would resolve against the wrong directory once
# we cd. Absolutise it here, before the executable check below reports on it.
case "$ARC" in
  /*) ;;
  *)  ARC="$(cd "$(dirname "$ARC")" 2>/dev/null && pwd)/$(basename "$ARC")" ;;
esac
# The work tree lives OUTSIDE "$HERE", not at "$HERE/.work".
#
# Creating from inside the corpus (see the create step) means only the corpus
# is ever archived, so the original reason for this no longer applies. It is
# kept because it is the stronger guarantee: no output of this suite can be
# reachable from the tree the suite archives, whatever the create step later
# becomes.
#
# The original failure is worth knowing, because it presented as a codec bug.
# (The archiver defect behind it is FIXED -- see "recursive" in FileInfo.hs and
# the -r basename-mask check further down -- but archiving from inside the corpus
# is kept regardless, since it is also what pins the deterministic scan order.)
# "arc a -r ... $CORPUS" treated the final path component as a mask and searched
# for it recursively beneath the corpus's parent -- which was "$HERE". Extraction
# reproduces the full stored path, so every "$HERE/.work/out-<label>" ended up
# containing another directory named "corpus", and the next create matched those
# too. The effect compounds: out-m4 carried copies of out-store's and out-m1's
# trees, so the entry count grew far faster than the corpus did (228, 290,
# 482 ... across the suite). That is what exhausted the MicroHs heap on the last
# few configurations.
WORK="${DARC_TEST_WORK:-${TMPDIR:-/tmp}/darc-tests-work}"
FP="$HERE/fingerprints.txt"

[ -x "$ARC" ] || { echo "error: archiver not found or not executable: $ARC" >&2; exit 2; }

command -v sha256sum >/dev/null && SHA=sha256sum || SHA="shasum -a 256"
hash_of () { $SHA "$1" | cut -d' ' -f1; }

# Hash a directory tree by content AND relative path, so a file landing in the
# wrong place fails even if its bytes are intact.
tree_hash () {
  [ -d "$1" ] || { echo "no-such-dir"; return; }
  # One hash process per BATCH of files, not one per file.
  #
  # The per-file form spawned a process for every entry: 218 for this corpus,
  # and tree_hash runs once per configuration plus once for the corpus itself.
  # That measured 22.8s per call on macOS and dominated the run on the Windows
  # ARM64 runner, where process creation is far more expensive -- a single
  # interop pass there took ~7 minutes against ~17 seconds for the same work
  # under Wine on Linux. Batching is 0.33s per call, a 69x improvement.
  #
  # $SHA prints exactly "<hash>  <path>" per file, which is the same text the
  # old printf composed, so the digest is unchanged -- verified byte-for-byte
  # against the previous implementation on this corpus. -r keeps an empty
  # directory from making GNU xargs run the hasher once with no arguments,
  # where it would read stdin instead.
  ( cd "$1" && find . -type f -print0 2>/dev/null | LC_ALL=C sort -z |
    xargs -0 -r $SHA ) | $SHA | cut -d' ' -f1
}

# Regenerate the corpus every run rather than reusing whatever is on disk.
#
# Fingerprints are taken over archives built from this tree, so ANY stray file
# in it changes every fingerprint at once. That is not hypothetical: a mistyped
# option (`-s epn`, where -s is the solid flag) made the archiver treat "epn"
# as the output name and write epn.arc into the corpus. The next --bless picked
# up 219 files instead of 218 and recorded a baseline that no clean checkout
# could reproduce -- presenting as "the fix is not deterministic after all"
# rather than as "the corpus is dirty".
#
# make-corpus.sh does "rm -rf $DIR" first and is deterministic, so this is
# cheap (218 small files) and makes the suite hermetic.
"$HERE/make-corpus.sh" "$CORPUS" >/dev/null

EXPECTED_TREE="$(tree_hash "$CORPUS")"

# method:label pairs. Labels are used as filenames and fingerprint keys, so
# they must stay stable even if the switch spelling changes.
#
# This block is data, not script: every non-blank line is parsed as a case, so
# it cannot carry "#" comments. Explain entries here instead.
#
# "-mdict -s-" earns its place: it is the only configuration that drives the
# Dict codec over an incompressible block on its own, which is what leaves
# phase2 with good_words==0. That branch used to realloc to size zero, keep the
# pointer realloc had already freed, and free it again. Instrumenting the
# branch showed zero hits across every other configuration here -- which is why
# the double-free only ever appeared on a CI runner, whose longer checkout
# paths change the stored block contents, and never reproduced locally.
#
# MM appears three times because the encoder chooses which decoder branch runs.
# "-mmm:2*16" pins the channel count and word size, so autodetection is skipped
# entirely and every block is filtered. "-mmm:d1" runs the fast detector ({8,16}
# models) and stores whatever it cannot classify. Plain "-mmm" is the archiver's
# default and runs the full {8,16,24,32} model set.
#
# Plain "-mmm" was absent until mmdet's Model::_32bit_run/_32bit_diff_run were
# fixed: they walked the buffer with a `long *`, 64-bit on LP64, so they read
# pairs of samples as one and slotted a value up to 2^63>>24 into a 1024-entry
# stats row. `arc a -mmm` segfaulted on this very corpus. Its fingerprint is
# therefore new, with no older archives to be compatible with -- nothing could
# write one.
CASES="
-m0:store
-m1:m1
-m4:m4
-m9:m9
-m4x:m4x
-mtor:tor
-mlzp:lzp
-mtta:tta
-mmm:2*16:mm
-mmm:d1:mm-auto
-mmm:mm-d9
-mlzma:lzma
-mppmd:ppmd
-mgrzip:grzip
-mdispack:dispack
-mdict -s-:dict-nonsolid
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
# Deliberately outside $WORK, to catch anything that depends on writing next to
# the source. The path must still be unique and removed first: a fixed
# /tmp/e2.arc survives every run, so the second invocation onwards became an
# *update* of an existing archive rather than a create -- a different operation
# that can block waiting on the earlier archive's contents. That turned this
# step into an intermittent multi-minute hang whose cause looked like whatever
# had last been changed.
rm -f "/tmp/darc-e2-$$.arc"
raw "a writing to /tmp"                   a -y -m0 "/tmp/darc-e2-$$.arc" "$WORK/tiny/one.txt"
rm -f "/tmp/darc-e2-$$.arc"
raw "t on a non-archive"                  t -y "$WORK/tiny/one.txt"
raw "lb on a non-archive"                 lb "$WORK/tiny/one.txt"

# "arc a -r <dir>" must archive exactly that directory, not every directory
# sharing its basename. The final component used to be treated as a mask and
# matched recursively beneath the *parent*, so naming "w/data" also stored a bare
# entry for "w/sub/data" -- someone else's directory, without its contents, which
# both archived something the user never asked for and leaked the surrounding
# layout. Checked by content, not exit status: the buggy version succeeded.
echo
echo "  -r names one directory, not a basename mask:"
rm -rf "$WORK/rmask"; mkdir -p "$WORK/rmask/w/data" "$WORK/rmask/w/sub/data"
echo a > "$WORK/rmask/w/data/a.txt"
echo b > "$WORK/rmask/w/sub/data/b.txt"
rm -f "$WORK/rmask/r.arc"
if ( cd "$WORK/rmask" && "$ARC" a -r -y -m0 --nodates r.arc w/data ) >"$WORK/rmask.log" 2>&1; then
  listing=$("$ARC" l "$WORK/rmask/r.arc" 2>/dev/null)
  if printf '%s' "$listing" | grep -q "sub/data"; then
    printf '  %-38s FAIL: also stored w/sub/data\n' "excludes same-named sibling dir"
  else
    printf '  %-38s ok\n' "excludes same-named sibling dir"
  fi
  if printf '%s' "$listing" | grep -q "data/a.txt"; then
    printf '  %-38s ok\n' "keeps the named directory's contents"
  else
    printf '  %-38s FAIL: lost w/data/a.txt\n' "keeps the named directory's contents"
  fi
else
  printf '  %-38s FAIL: create failed\n' "-r basename-mask check"
fi

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
  # Split on the LAST colon, not the first: method strings carry their own
  # parameters after a colon ("-mmm:2*16"), while labels never contain one.
  opts="${line%:*}"; label="${line##*:}"
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

  # Two things make these archive bytes reproducible:
  #
  #   --nodates     no mtimes in the archive.
  #
  #   cd "$CORPUS"  entries are stored corpus-relative. Archiving by absolute
  #                 path stores each path with the leading "/" stripped, so the
  #                 checkout location ends up in the archive bytes: the same
  #                 corpus at the same commit fingerprinted differently under
  #                 /home/runner/... on CI and /src/Tests/... in a container.
  #
  # A plain recursive scan is used deliberately, with no explicit sort order
  # and no file list. This is the path a user gets, and file order used to come
  # straight from readdir -- filesystem-specific, so the same tree produced
  # different archives on APFS and ext4. That is fixed in the archiver now
  # (getDirectoryContents_FileInfo in FileInfo.hs sorts by name), and archiving
  # this way is what holds it fixed: these fingerprints fail if the scan ever
  # stops being deterministic.
  #
  # An earlier version of this suite worked around the problem here instead,
  # with "--sort=epn" and an explicit "@listfile". That produced a stable
  # baseline while leaving the actual defect in place and untested, and it
  # stored no directory entries, so the directory half of the layout was never
  # fingerprinted at all.
  ( cd "$CORPUS" && "$ARC" a --nodates -r -y $opts "$arc" . ) >"$WORK/$label.create.log" 2>&1
  st=$?
  if [ $st -ne 0 ]; then
    show_fail create "(cd $CORPUS && $ARC a --nodates -r -y $opts $arc .)" "$WORK/$label.create.log" "$st"
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

  # The payload lands directly in "$out": entries are stored corpus-relative,
  # so extraction reproduces the corpus tree at the extraction root itself.
  #
  # This used to be "$out/${CORPUS#/}" -- derived, because absolute-path
  # archives recreate the whole checkout path under the extraction directory.
  # Searching for it by name instead was actively wrong: archiving a directory
  # with "-r" makes the trailing name a *mask*, so the archive carried a
  # directory entry for every directory of that name under the base dir,
  # extraction recreated them as empty dirs, and
  #     find "$out" -type d -name corpus -print -quit
  # returned whichever readdir yielded first -- the real tree on some
  # filesystems, an empty phantom on others. That is why this suite once
  # reported "extract produced no files" on one CI runner and passed on another
  # with a byte-identical archive. Nothing was wrong with extraction.
  root="$out"

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
