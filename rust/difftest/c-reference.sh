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

# Usage: darc_c_reference <repo-root>   → echoes the reference tree's path
darc_c_reference() {
  local root="$1"
  local sha="$DARC_C_REF_SHA"
  local cref="${TMPDIR:-/tmp}/darc-c-ref-$sha"

  # Rebuild the shim copy every time (cheap, and the shims are live source);
  # extract the pinned C only once.
  if [ ! -d "$cref/Compression" ]; then
    rm -rf "$cref"; mkdir -p "$cref"
    # CI checks out shallow (actions/checkout defaults to fetch-depth: 1), so
    # the pinned commit is usually absent. Fetch just that one commit rather
    # than making every job clone full history.
    if ! git -C "$root" rev-parse --verify --quiet "$sha^{commit}" >/dev/null; then
      git -C "$root" fetch --depth=1 --quiet origin "$sha" 2>/dev/null || true
    fi
    git -C "$root" rev-parse --verify --quiet "$sha^{commit}" >/dev/null || {
      echo "c-reference: pinned revision $sha is not available and could not be" >&2
      echo "fetched. In CI, give the checkout enough history (fetch-depth: 0)." >&2
      return 1; }
    git -C "$root" archive "$sha" Compression | tar -x -C "$cref" || {
      echo "c-reference: could not extract Compression/ at $sha" >&2
      return 1; }
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