#!/usr/bin/env bash
# GRZip's encoder stages, forward direction: C vs Rust, byte for byte.
#
# GRZip's encoder is being ported STAGE BY STAGE, and GRZip_CompressBlock cannot
# produce a comparable stream until every stage exists -- it is also recursive
# (the record filter splits a block into 2 or 4 parts that re-enter it). So each
# stage is gated on its own first, and this is the first of them.
#
# The reference input is PADDED with 64 zero bytes. The C reads up to
# MinMatchLen-1 bytes past the end at LZP.c:89 -- confirmed under ASan as a
# heap-buffer-overflow READ of size 4 -- so without padding this would compare
# against whatever malloc happened to leave there. The Rust port does not
# reproduce the overread: it reads a zero-padded view and stays in bounds, which
# is why the two agree. See the note on `lzp::encode` for why that is the right
# call rather than a divergence.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds GRZip: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags GRZip)" || exit 1
W="${TMPDIR:-/tmp}/grzip-lzp.$$"; mkdir -p "$W/in"
trap 'rm -rf "$W"' EXIT
( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 || { echo "cargo build failed"; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/grzip_ref.cpp" "$CREF/rust/difftest/grzip_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" grzip-stage "$W/in"

fail=0; total=0

# --- ST4 -------------------------------------------------------------------
# Both the transformed block AND the returned FBP matter: the driver stores FBP
# in the block header, so a port that produced the right bytes with the wrong
# index would still write an archive that decodes to garbage.
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  t < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" t < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_fbp=$(cat "$W/e.c"); r_fbp=$(cat "$W/e.rs")
  if [ "$c_fbp" != "$r_fbp" ]; then
    echo "  [st4] $(basename "$f"): FBP differs ($c_fbp vs $r_fbp)"; fail=$((fail+1)); continue
  fi
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [st4] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
st4_total=$total
echo "grzip ST4 encode: $((st4_total-fail))/$st4_total agree"
st4_fail=$fail

# --- MTF + arithmetic coder ------------------------------------------------
# The entropy stage. Its models adapt on every symbol, so a single divergent
# probability desynchronises everything after it -- there is no such thing as a
# small difference here, which makes byte equality the only useful bar.
fail=0; total=0; mtf_compressed=0
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  m < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" m < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_rc=$(cat "$W/e.c"); r_rc=$(cat "$W/e.rs")
  if [ "$c_rc" != "$r_rc" ]; then
    echo "  [mtf] $(basename "$f"): return differs ($c_rc vs $r_rc)"; fail=$((fail+1)); continue
  fi
  case "$c_rc" in rc=[1-9]*) mtf_compressed=$((mtf_compressed+1));; esac
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [mtf] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
mtf_total=$total; mtf_fail=$fail
echo "grzip MTF-Ari encode: $((mtf_total-mtf_fail))/$mtf_total agree"
# All-incompressible would mean the coder never actually ran.
[ "$mtf_compressed" -gt 0 ] || { echo "  no input was ever compressed -- the coder never ran"; mtf_fail=$((mtf_fail+1)); }

# --- BWT with fast/strong selection ----------------------------------------
# The returned FBP carries StrongBWT_Flag, so comparing it compares WHICH sort
# ran as well as what it produced. That matters: the fast sort aborts when its
# adaptive match limit runs out, and the limit only decrements after 32
# consecutive matching bytes -- so high-entropy inputs take the fast path and
# repetitive ones fall back. Both must be represented here or half the code is
# untested while everything stays green.
fail=0; total=0; fast_ran=0; fell_back=0
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  F < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" F < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_f=$(cat "$W/e.c"); r_f=$(cat "$W/e.rs")
  if [ "$c_f" != "$r_f" ]; then
    echo "  [bwt] $(basename "$f"): FBP differs ($c_f vs $r_f)"; fail=$((fail+1)); continue
  fi
  n=${c_f#fbp=}
  case "$n" in skip) ;; -*) ;; *) if [ "$n" -ge 1073741824 ] 2>/dev/null; then fell_back=$((fell_back+1)); else fast_ran=$((fast_ran+1)); fi;; esac
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [bwt] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
bwt_total=$total; bwt_fail=$fail
echo "grzip BWT(fast) encode: $((bwt_total-bwt_fail))/$bwt_total agree  [fast=$fast_ran fallback=$fell_back]"
[ "$fast_ran"  -gt 0 ] || { echo "  the fast sort never ran -- that half is untested"; bwt_fail=$((bwt_fail+1)); }
[ "$fell_back" -gt 0 ] || { echo "  the fallback never ran -- that path is untested"; bwt_fail=$((bwt_fail+1)); }

# --- strong BWT ------------------------------------------------------------
# The sort decides the entire transformed block, and the first-byte position
# decides where the inverse starts, so both are compared. This is the fallback
# path: the driver reaches it when the fast sort gives up, and whenever the mode
# word does not ask for fast sorting.
fail=0; total=0
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  B < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" B < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_f=$(cat "$W/e.c"); r_f=$(cat "$W/e.rs")
  if [ "$c_f" != "$r_f" ]; then
    echo "  [sbwt] $(basename "$f"): FBP differs ($c_f vs $r_f)"; fail=$((fail+1)); continue
  fi
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [sbwt] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
sbwt_total=$total; sbwt_fail=$fail
echo "grzip strong-BWT encode: $((sbwt_total-sbwt_fail))/$sbwt_total agree"

# --- WFC + arithmetic coder ------------------------------------------------
# Shares the range coder and every model with MTF; only the symbol list differs,
# so this is really a test of the list discipline -- the weights, the twelve
# look-back decrements, and the insertion walk that keeps `index` the inverse of
# `list`.
fail=0; total=0; wfc_compressed=0
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  w < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" w < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_rc=$(cat "$W/e.c"); r_rc=$(cat "$W/e.rs")
  if [ "$c_rc" != "$r_rc" ]; then
    echo "  [wfc] $(basename "$f"): return differs ($c_rc vs $r_rc)"; fail=$((fail+1)); continue
  fi
  case "$c_rc" in rc=[1-9]*) wfc_compressed=$((wfc_compressed+1));; esac
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [wfc] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
wfc_total=$total; wfc_fail=$fail
echo "grzip WFC-Ari encode: $((wfc_total-wfc_fail))/$wfc_total agree"
[ "$wfc_compressed" -gt 0 ] || { echo "  no input was ever compressed -- the coder never ran"; wfc_fail=$((wfc_fail+1)); }

# --- record filter ---------------------------------------------------------
# The MODE matters as much as the bytes: it is what makes GRZip_CompressBlock
# recurse, and it is chosen by a float entropy comparison plus an integer sum
# that overflows on purpose (see rec::test).
fail=0; total=0; modes_seen=""
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  r < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" r < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_m=$(cat "$W/e.c"); r_m=$(cat "$W/e.rs")
  if [ "$c_m" != "$r_m" ]; then
    echo "  [rec] $(basename "$f"): MODE differs ($c_m vs $r_m)"; fail=$((fail+1)); continue
  fi
  modes_seen="$modes_seen ${c_m#mode=}"
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [rec] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
rec_total=$total; rec_fail=$fail
echo "grzip Rec encode: $((rec_total-rec_fail))/$rec_total agree"
# A green run over inputs that all return mode 0 would test nothing at all.
for want in 1 2 3 4; do
  case " $modes_seen " in
    *" $want "*) ;;
    *) echo "  corpus never produced Rec mode $want -- that path is untested"; rec_fail=$((rec_fail+1));;
  esac
done

# --- LZP -------------------------------------------------------------------
fail=0; total=0
# mml 0 and 2 matter: the archiver reaches LZP with MinMatchLen == 0 for every
# mode word in the 0x100-0x106 family, and there C's early-match probe indexes
# `Ptr + mml - 4` -- four bytes BEFORE the block. Guarding that by declining to
# compress diverted the whole family down the LZP-declined path and produced
# wrong archives, while this sweep stayed green because it only tried mml >= 8.
for mml in 0 2 8 16 32 64; do
  for htb in 8 12 15; do
    for f in "$W"/in/*; do
      total=$((total+1))
      rc1=0; rc2=0
      "$W/c"  l $mml $htb < "$f" >| "$W/o.c"  2>"$W/e.c"  || rc1=1
      "$W/rs" l $mml $htb < "$f" >| "$W/o.rs" 2>"$W/e.rs" || rc2=1
      c_rc=$(cat "$W/e.c"); r_rc=$(cat "$W/e.rs")
      if [ "$c_rc" != "$r_rc" ]; then
        echo "  [mml=$mml ht=$htb] $(basename "$f"): return differs ($c_rc vs $r_rc)"; fail=$((fail+1)); continue
      fi
      cmp -s "$W/o.c" "$W/o.rs" || { echo "  [mml=$mml ht=$htb] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
    done
  done
done
echo "grzip LZP encode: $((total-fail))/$total agree"
[ $((fail+st4_fail+rec_fail+mtf_fail+wfc_fail+sbwt_fail+bwt_fail)) -eq 0 ] || exit 1
