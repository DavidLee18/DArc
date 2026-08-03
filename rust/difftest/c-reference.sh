# Materialise the C codec sources at a pinned revision, for the differential
# harnesses to compile as their reference. Sourced, not executed.
#
# ── Why ──────────────────────────────────────────────────────────────────────
#
# Every <codec>-check.sh proves the Rust port matches the C by compiling BOTH
# and requiring identical output. As the port deletes the C it replaces, that
# reference disappears from the working tree — and with it the only thing that
# can demonstrate a replacement is correct. So the C is taken from git history
# instead of from the checkout.
#
# The reference is ALWAYS the pinned revision, even while the C is still present
# in the tree. Two reasons:
#
#   * A fallback that only engages once the C is deleted would sit untested
#     until the moment it becomes load-bearing. That is exactly how the MicroHs
#     cache guard shipped broken — the run that introduced it populated the
#     cache rather than restoring it, so it never took the path it broke.
#   * A fixed oracle cannot drift. Comparing against whatever C happens to be in
#     the tree lets a concurrent C change mask a Rust regression.
#
# ── How ──────────────────────────────────────────────────────────────────────
#
# `git archive` extracts Compression/ at the pinned SHA, then the CURRENT
# difftest shims are copied in beside it. That combination matters: the shims
# `#include "../../Compression/..."` by relative path, so placing them inside
# the extracted tree makes those includes resolve to the pinned C with no source
# edits at all, while leaving the harness logic itself free to evolve.
#
# Bumping the pin is a deliberate act: it changes what "correct" means for every
# harness. Do it only to pick up a genuine C-side fix, and say so in the commit.

# Last revision containing the full C codec set (zstd's libzstd was removed in
# this commit itself, and has no harness).
#
# The FULL 40-character hash, not an abbreviation: `git fetch origin <sha>`
# rejects a short SHA outright ("couldn't find remote ref"), which is how the
# shallow-clone fetch below is able to work at all. Abbreviations can also grow
# ambiguous as history does.
DARC_C_REF_SHA="5c2c6ce1244db759a17aea61cb243f3ace41fe61"

# Fetch a pinned revision, retrying, and say what actually went wrong.
#
# Usage: darc_fetch_pinned <repo-root> <sha> <label>
#
# Both callers below used to do this inline as a single attempt with stderr
# sent to /dev/null. That turns a transient network failure into the message
# "pinned revision is not available", which names a cause that is not the
# cause -- on 2026-08-03 it took rust-codecs-ppmd red on a run where nothing
# relevant had changed, and the suggested fix in that message (fetch-depth: 0)
# would not have helped. Keeping the last attempt's stderr is the point: a
# harness that reports the wrong reason costs more than one that just fails.
#
# The post-fetch existence check is not redundant. `git fetch <sha>` can exit 0
# on a server that accepted the request without delivering the object, and the
# old code then fell through to a confusing failure much later.
darc_fetch_pinned() {
  local root="$1" sha="$2" label="$3"
  local attempt err
  err="$(mktemp)" || return 1
  for attempt in 1 2 3; do
    if git -C "$root" cat-file -e "$sha^{commit}" 2>/dev/null; then
      rm -f "$err"; return 0
    fi
    # 2>| and not 2>: mktemp has already created $err, and under `noclobber`
    # a plain 2> onto an existing file FAILS -- which would skip the fetch
    # entirely and leave the retry reporting a failure it never attempted.
    if git -C "$root" fetch --quiet --depth=1 origin "$sha" 2>|"$err"; then
      if git -C "$root" cat-file -e "$sha^{commit}" 2>/dev/null; then
        rm -f "$err"; return 0
      fi
      echo "$label: fetch of $sha reported success but the object is still absent" >&2
    fi
    if [ "$attempt" -lt 3 ]; then
      sleep $((attempt * 5))
    fi
  done
  echo "$label: could not fetch pinned revision $sha after 3 attempts." >&2
  if [ -s "$err" ]; then
    echo "$label: the last attempt said:" >&2
    sed 's/^/  /' "$err" >&2
  fi
  rm -f "$err"
  return 1
}

# Usage: darc_c_reference <repo-root>   → echoes the reference tree's path
darc_c_reference() {
  local root="$1"
  local sha="$DARC_C_REF_SHA"
  local cref="${TMPDIR:-/tmp}/darc-c-ref-$sha"

  # Rebuild the shim copy every time (cheap, and the shims are live source);
  # extract the pinned C only once.
  #
  # The guard is a MARKER FILE, not the directory. It used to be `[ ! -d
  # "$cref/Compression" ]`, and a partial extraction therefore persisted forever:
  # a cache holding the subdirectories but none of Compression/LZMA/*.cpp survived
  # every later run, and the harnesses failed with "file not found" for sources
  # that exist perfectly well at the pinned revision. Worse than the failure is the
  # near miss -- a partial cache silently changes what the oracle IS, and a harness
  # comparing against a different C than it claims is a harness proving nothing.
  # The marker is written last, so it exists only after tar has succeeded.
  if [ ! -f "$cref/.extracted-ok" ]; then
    rm -rf "$cref"; mkdir -p "$cref"
    # CI checks out shallow (actions/checkout defaults to fetch-depth: 1), so
    # the pinned commit is usually absent. Fetch just that one commit rather
    # than making every job clone full history.
    darc_fetch_pinned "$root" "$sha" "c-reference" || {
      echo "c-reference: if this is CI and the fetch itself is fine, the checkout" >&2
      echo "may need more history (fetch-depth: 0)." >&2
      return 1; }
    git -C "$root" archive "$sha" Compression | tar -x -C "$cref" || {
      echo "c-reference: could not extract Compression/ at $sha" >&2
      rm -rf "$cref"
      return 1; }
    # Cheap sanity check on the result before blessing it: one file that the
    # pinned revision certainly has. Catches a truncated stream that tar still
    # exited 0 on.
    [ -f "$cref/Compression/LZMA/C_LZMA.cpp" ] || {
      echo "c-reference: extraction at $sha looks incomplete" >&2
      rm -rf "$cref"
      return 1; }
    : > "$cref/.extracted-ok"
  fi

  mkdir -p "$cref/rust/difftest"
  cp "$root"/rust/difftest/*.cpp "$cref/rust/difftest/" 2>/dev/null

  echo "$cref"
}

# ── The oracle's optimisation flags ──────────────────────────────────────────
#
# Usage: darc_codec_cflags <Compression subdirectory>  → echoes its OPT_FLAGS
#
# These codecs are compiled by Compression/<codec>/makefile, and for at least
# one of them THE OPTIMISATION FLAGS ARE PART OF THE ARCHIVE FORMAT. PPMd
# type-puns through `(WORD&)` in StateCpy/SWAP, so a compiler permitted to
# assume those writes cannot alias a neighbouring BYTE read reuses a cached
# value where the source says to reload -- and emits different compressed bytes
# for it. Measured, on the shape that discriminates:
#
#     -O1                        reuses      -O0                        re-reads
#     -O2                        reuses      -O1 -fno-strict-aliasing   re-reads
#
# Every Compression/*/makefile passes -fno-strict-aliasing; every harness here
# used to build its reference without it. So each oracle was a build DArc does
# not ship, and for PPMd that produced a port matching the wrong C. Reading the
# flags from the makefile instead of picking an -O level is the fix, and it is
# the safer oracle besides: under -fno-strict-aliasing the answer stops
# depending on how clever a given compiler's alias analysis is.
#
# Checked, not assumed: every harness was re-run against these flag sets, and
# all 23 still pass. Only PPMd was ever sensitive to the difference.
#
# OPT_FLAGS only. CODE_FLAGS (-fno-exceptions/-fno-rtti/-W...) change codegen
# and diagnostics but not the value semantics a byte-comparison can see, and
# forcing them on the drivers would only constrain how the drivers themselves
# may be written.
#
# Two makefile flags are deliberately absent because clang has no equivalent:
# -fforce-addr (DisPack, LZ4 -- removed from GCC long ago) and
# --param inline-unit-growth=999 (Tornado -- GCC-only).
darc_codec_cflags() {
  case "$1" in
    PPMD)                    echo "-O1 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops" ;;
    GRZip)                   echo "-O2 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops" ;;
    4x4|Dict)                echo "-O3 -fomit-frame-pointer -fno-strict-aliasing" ;;
    # LZMA is the one codec with TWO flag sets. This is the C++ wrapper set
    # (C_LZMA.cpp and friends). The vendored SDK under 7z24/ is compiled with
    # `C7Z_CFLAGS = -std=c11 -O2 -DNDEBUG -D_REENTRANT` and NOTABLY WITHOUT
    # -fno-strict-aliasing -- see darc_lzma_sdk_cflags below. Do not merge them:
    # PPMd is the standing proof that an alias-analysis difference can change
    # compressed bytes.
    LZMA)                    echo "-O2 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops" ;;
    BSC|Delta|DisPack|LZ4|LZP|MM|REP|Tornado|_Encryption)
                             echo "-O3 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops" ;;
    *) echo "darc_codec_cflags: no flag set recorded for '$1' -- read it from" >&2
       echo "Compression/$1/makefile and add it here rather than guessing" >&2
       return 1 ;;
  esac
}


# The flags Compression/LZMA/makefile uses for the vendored 7-Zip SDK sources
# (`C7Z_CFLAGS`), which differ from the wrapper's: C11, NDEBUG, _REENTRANT, and
# no -fno-strict-aliasing. Kept separate so a harness cannot accidentally build
# the SDK the way DArc builds the wrapper.
darc_lzma_sdk_cflags() {
  echo "-std=c11 -O2 -DNDEBUG -D_REENTRANT"
}

# ── SREP's oracle has its own, LATER pin ────────────────────────────────────
#
# SREP is not an in-process codec: its oracle is a `srep` BINARY built from
# source, not a shim linked against a pinned .cpp. And the shared pin above is
# too old to serve as ground truth for it -- two genuine C-side bugs were fixed
# in Compression/SREP AFTER 5c2c6ce:
#
#   fb32a20  a heap overflow that intermittently produced corrupt archives
#            (SliceHash allocated filesize/L entries, one short for a file whose
#            size is not a multiple of L)
#   1f8f8a2  a block header copied out AFTER its buffer was released, so a block
#            could carry the hash of the block two positions later
#
# Building the oracle at the old pin reproduces both. It was measurably wrong:
# `-m3f -b16kb` on the `runs` corpus produced a block whose stored hash did not
# match its own contents, which the harness correctly reported as a divergence
# from a port that is right.
#
# So SREP pins separately, to the last revision on main that touched it. The
# shared pin is deliberately left alone: bumping it would redefine "correct" for
# every other harness at once, which is not what a SREP fix warrants.
DARC_SREP_REF_SHA="1f8f8a21ea9f986c241406484e5414e1fad32af2"

# Materialise Compression/SREP + srep/ at the SREP pin and echo the tree's path.
darc_srep_reference() {
  local root="$1"
  local sha="$DARC_SREP_REF_SHA"
  local ref="${TMPDIR:-/tmp}/darc-srep-ref-$sha"

  if [ ! -f "$ref/.extracted-ok" ]; then
    rm -rf "$ref"; mkdir -p "$ref" || return 1
    darc_fetch_pinned "$root" "$sha" "srep-reference" || { rm -rf "$ref"; return 1; }
    git -C "$root" archive "$sha" Compression/SREP srep | tar -x -C "$ref" || {
      echo "srep-reference: could not extract the SREP sources at $sha" >&2
      rm -rf "$ref"; return 1
    }
    mkdir -p "$ref/Tests"
    # Same guard as the shared reference: a partial extraction must not persist.
    [ -f "$ref/Compression/SREP/srep.cpp" ] && [ -f "$ref/srep/compile" ] || {
      echo "srep-reference: extraction at $sha looks incomplete" >&2
      rm -rf "$ref"; return 1
    }
    : > "$ref/.extracted-ok"
  fi
  echo "$ref"
}
