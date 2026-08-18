# Resolve the Haskell reference binary, for the `arc-*-check.sh` harnesses.
#
# SOURCED, not run -- it sets REF in the caller, the way c-reference.sh sets the
# C reference up for the codec harnesses:
#
#   ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
#   REF="${1:-$ROOT/Tests/arc-ghc}"
#   . "$ROOT/rust/difftest/arc-reference.sh"
#
# It was sixteen identical copies, which is how the two faults below came to be
# in fifteen of them at once.

[ -x "$REF" ] || {
  echo "no reference binary at $REF.

The Haskell reference was deleted; build one from a commit that still has it:
  git worktree add /tmp/darc-ref 9a127e6 && (cd /tmp/darc-ref && ./compile-ghc-probe)
then pass /tmp/darc-ref/Tests/arc-ghc as \$1. For a gate that needs no
reference at all, use arc-golden-check.sh" >&2
  exit 2
}

# ── the reference must be an ABSOLUTE path ─────────────────────────────────
# Nearly every harness runs its comparisons from inside the work directory --
# `( cd "$W/corpus" && "$REF" a … )` -- so a RELATIVE $1 stops resolving after
# the first `cd`. And `Tests/arc-ghc` is exactly what a person types.
#
# The check above passes, because it runs before any cd. Then every case fails
# to produce an archive and is skipped, and the harness reports
#
#   arc t: 0 archives, 0 differing, 20 skipped
#   nothing was compared
#
# The `checked -gt 0` guard does catch it, so this was never a silently green
# run -- but the message reads as a broken reference rather than a mistyped
# path, and the fix belongs here rather than in the reader's head. Resolving
# beats refusing: "relative to where I typed it" is unambiguous at startup, and
# it is what the person meant.
#
# Resolved AFTER the -x check, so a path that does not exist is reported as
# itself rather than as a `cd` failure. Same idiom as arc-cli-check.sh, which
# has done this from the start -- as do the three harnesses that require an
# explicit reference. It is the ones with a DEFAULT that went without, because
# the default is already absolute and the bug only shows when an argument is
# passed.
REF="$(cd "$(dirname "$REF")" && pwd)/$(basename "$REF")"
