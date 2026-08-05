#!/usr/bin/env bash
# Make the Haskell reference available at `Tests/arc-ghc`, however it can.
#
# Twenty of the twenty-one `arc-*-check.sh` harnesses compare the port against a
# build of the Haskell archiver and exit 2 without one. `9a127e6` is the last
# commit that can produce it, and that is a dependency with a clock on it: the
# day GHC 9.10.3 stops installing, roughly 500 archive comparisons stop running
# and only `arc-golden-check.sh`'s 93 recorded hashes remain.
#
# This resolves the binary from whichever source is available, so CI can run the
# full set instead of the backstop alone. Every harness already defaults to
# `$ROOT/Tests/arc-ghc`, so populating that path is all this has to do — no
# harness needed changing.
#
# ── Why a downloaded artifact is pinned and a local one is not ──────────────
#
# The binary is NOT bit-reproducible: two builds of `9a127e6` on this machine
# differ by SHA-256, because GHC embeds paths and build metadata. But they are
# BEHAVIOURALLY reproducible — measured, 8 varied cases (`-m0`, `-m4`, `-m9`,
# `-m5 -ma1`, `--groups`, `-lc8m`, `-s-`, `-mbsc`) byte-identical between two
# independent builds.
#
# So the two sources need different treatment, and conflating them would be
# wrong in both directions:
#
#   * A DOWNLOAD is verified against a pinned SHA-256. It comes over the
#     network and nothing else vouches for it; an oracle you cannot read is
#     only as trustworthy as its hash. An unpinned platform is refused rather
#     than accepted — "no hash recorded" must not degrade to "any bytes will
#     do".
#   * A LOCAL binary is trusted as-is. Whoever built it has the source, and
#     requiring it to match a published artifact's hash would reject a
#     perfectly good build for a difference that carries no meaning.
#
# Usage:
#   haskell-reference.sh            # ensure Tests/arc-ghc exists; print how
#   DARC_HASKELL_REF=/path/to/arc   # override entirely
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEST="$ROOT/Tests/arc-ghc"
PINS="$ROOT/rust/difftest/golden/reference.sha256"

# The commit the reference is built from. Same idea as `DARC_C_REF_SHA` in
# c-reference.sh: the oracle is pinned to history so it cannot drift.
DARC_HASKELL_REF_SHA="9a127e6"
RELEASE_TAG="oracle-$DARC_HASKELL_REF_SHA"

sha() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

# `darwin-arm64`, `linux-x86_64`, `linux-aarch64`.
platform() {
  local s m
  s=$(uname -s | tr '[:upper:]' '[:lower:]')
  m=$(uname -m)
  printf '%s-%s\n' "$s" "$m"
}

pinned_sha() {
  [ -f "$PINS" ] || return 1
  awk -v p="$1" '$1 == p { print $2 }' "$PINS" | grep -E '^[0-9a-f]{64}$' || return 1
}

# 1. An explicit override wins, and is not second-guessed.
if [ -n "${DARC_HASKELL_REF:-}" ]; then
  [ -x "$DARC_HASKELL_REF" ] || { echo "DARC_HASKELL_REF is not executable: $DARC_HASKELL_REF" >&2; exit 2; }
  echo "reference: \$DARC_HASKELL_REF -> $DARC_HASKELL_REF"
  exit 0
fi

# 2. Already present. Trusted without a hash check, for the reason above.
if [ -x "$DEST" ]; then
  echo "reference: already at $DEST ($(sha "$DEST" | cut -c1-12), local build, not hash-checked)"
  exit 0
fi

# 3. A worktree of the pinned commit, if someone has one.
for wt in "${DARC_REF_WORKTREE:-}" /tmp/darc-ref; do
  [ -n "$wt" ] || continue
  if [ -x "$wt/Tests/arc-ghc" ]; then
    mkdir -p "$(dirname "$DEST")"
    cp "$wt/Tests/arc-ghc" "$DEST"
    echo "reference: copied from $wt ($(sha "$DEST" | cut -c1-12))"
    exit 0
  fi
done

# 4. The published artifact for this platform.
plat="$(platform)"
want="$(pinned_sha "$plat" || true)"
if [ -z "$want" ]; then
  cat >&2 <<MSG
No reference binary, and no pinned SHA-256 for platform '$plat' in
  $PINS

Refusing to download an unverified oracle. Either:
  * build one:  git worktree add /tmp/darc-ref $DARC_HASKELL_REF_SHA
                (cd /tmp/darc-ref && ./compile-ghc-probe)   # -> Tests/arc-ghc
  * or publish an asset for this platform and record its hash in that file.
MSG
  exit 2
fi

url="https://github.com/DavidLee18/DArc/releases/download/$RELEASE_TAG/arc-ghc-$plat"
tmp="$(mktemp)"
echo "reference: fetching $url"
if ! curl -fsSL --retry 3 --retry-delay 2 -o "$tmp" "$url"; then
  rm -f "$tmp"
  echo "could not fetch $url" >&2
  # Falling through to a build rather than failing: this platform may simply
  # have no published asset yet. Only darwin-arm64 does; the Linux ones still
  # need producing on a Linux host.
  exec "$(dirname "$0")/haskell-reference-build.sh"
fi
got="$(sha "$tmp")"
if [ "$got" != "$want" ]; then
  rm -f "$tmp"
  echo "::error::reference hash mismatch for $plat: expected $want, got $got" >&2
  exit 1
fi
mkdir -p "$(dirname "$DEST")"
mv "$tmp" "$DEST"
chmod +x "$DEST"
echo "reference: fetched and verified ($got)"
