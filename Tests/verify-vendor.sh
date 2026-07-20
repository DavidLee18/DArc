#!/usr/bin/env bash
# Verify the vendored third-party libraries against vendor.lock.
#
#   Tests/verify-vendor.sh              local files vs recorded hashes (offline)
#   Tests/verify-vendor.sh --upstream   also re-download the pinned releases and
#                                       prove the tree is still byte-identical
#
# The offline mode answers "has anything in a vendored tree drifted since it was
# locked?" and is cheap enough to run in CI on every push. The --upstream mode
# answers the stronger question, "is what we ship still what upstream published?",
# and needs the network.
#
# Background: these libraries were vendored with no record of their origin, so a
# pristine copy was indistinguishable from a locally patched one. That mattered in
# practice -- LZMA and eight other codecs here *are* patched, and without a
# manifest the only way to tell was to diff every file against upstream by hand.
set -uo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
cd "$HERE"
LOCK=vendor.lock
UPSTREAM=0
[ "${1:-}" = "--upstream" ] && UPSTREAM=1

[ -f "$LOCK" ] || { echo "error: $LOCK not found" >&2; exit 2; }

if command -v sha256sum >/dev/null 2>&1; then
  hash_of () { sha256sum "$1" | cut -d' ' -f1; }
elif command -v shasum >/dev/null 2>&1; then
  hash_of () { shasum -a 256 "$1" | cut -d' ' -f1; }
else
  echo "error: neither sha256sum nor shasum found" >&2; exit 2
fi

checked=0; bad=0; missing=0

# ---- offline: every locked file still hashes to what the lock says -----------
while read -r kind a b _rest; do
  case "$kind" in
    file)
      if [ ! -f "$a" ]; then
        echo "  MISSING: $a"; missing=$((missing+1)); continue
      fi
      got=$(hash_of "$a")
      if [ "$got" != "$b" ]; then
        echo "  CHANGED: $a"
        echo "      locked $b"
        echo "      actual $got"
        bad=$((bad+1))
      fi
      checked=$((checked+1))
      ;;
  esac
done < "$LOCK"

echo "vendor.lock: $checked files checked, $bad changed, $missing missing"

# A lock nobody reads is not a check. If the manifest somehow contains no file
# entries, that is a failure, not a pass -- otherwise this script would report
# success on an empty lock.
if [ "$checked" -eq 0 ]; then
  echo "error: $LOCK contained no 'file' entries -- nothing was verified" >&2
  exit 1
fi

status=0
[ "$bad" -eq 0 ] && [ "$missing" -eq 0 ] || status=1

# ---- optional: prove the lock still matches the upstream release -------------
if [ "$UPSTREAM" = 1 ]; then
  echo
  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' EXIT
  while read -r kind name ver url asha ldir prefix; do
    [ "$kind" = "lib" ] || continue
    echo "--- $name $ver ---"
    if ! curl -fsSL "$url" -o "$tmp/$name.tgz"; then
      echo "  download failed: $url"; status=1; continue
    fi
    got=$(hash_of "$tmp/$name.tgz")
    if [ "$got" != "$asha" ]; then
      echo "  ARCHIVE CHECKSUM MISMATCH"
      echo "      locked $asha"
      echo "      actual $got"
      status=1; continue
    fi
    mkdir -p "$tmp/$name" && tar -xzf "$tmp/$name.tgz" -C "$tmp/$name"
    n=0; d=0
    while read -r k f h _r; do
      [ "$k" = "file" ] || continue
      case "$f" in "$ldir"/*) ;; *) continue ;; esac
      up="$tmp/$name/$prefix/${f#"$ldir"/}"
      if [ ! -f "$up" ]; then
        echo "  not in upstream: $f"; d=$((d+1)); continue
      fi
      [ "$(hash_of "$up")" = "$h" ] || { echo "  differs from upstream: $f"; d=$((d+1)); }
      n=$((n+1))
    done < "$LOCK"
    echo "  $n files compared, $d differ from upstream"
    [ "$d" -eq 0 ] || status=1
  done < "$LOCK"
fi

echo
if [ "$status" -eq 0 ]; then
  echo "vendored libraries verified"
else
  echo "vendored library verification FAILED" >&2
fi
exit "$status"
