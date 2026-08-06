#!/usr/bin/env bash
# Watch a PR, merge it only on a verdict PINNED TO ITS HEAD COMMIT, then watch
# the post-merge run.
#
#   usage: watch-merge.sh <pr-number> <branch>
#
# ── Why the previous version merged a red PR ────────────────────────────────
#
# It polled `gh pr checks <n>` until nothing was pending, then treated "no
# failures" as green. Both of those are wrong the moment a push has just
# happened:
#
#   * `gh pr checks` reports whatever check set exists RIGHT NOW. Straight after
#     a push the new run has not registered, so it answers with the previous
#     commit's checks -- or none at all.
#   * "nothing pending" is therefore satisfied before the new run starts, and
#     the loop exits on a stale, all-green answer.
#
# That is how #140 was merged with `arc-harnesses` failing on both of its
# commits. The output parsing was fine; the timing was not.
#
# This version never asks "are there failures". It asks "what did the run FOR
# THIS EXACT COMMIT conclude", and refuses to answer until such a run exists and
# has finished.
set -uo pipefail

PR="${1:?usage: watch-merge.sh <pr-number> <branch>}"
BR="${2:?usage: watch-merge.sh <pr-number> <branch>}"
DEADLINE=$(( $(date +%s) + 3600 ))

runs_for() { # runs_for <sha> -- one "status conclusion name" per line
  gh run list --branch "$BR" --limit 20 \
    --json headSha,status,conclusion,name \
    --jq ".[]|select(.headSha==\"$1\")|\"\(.status) \(.conclusion // \"-\") \(.name)\""
}

# The PR's head SHA, reconciled against what is actually on the branch.
#
# `gh pr view` is as stale as `gh pr checks` was: queried straight after a push
# it can still report the PREVIOUS head. That happened on the first live use of
# this script -- it pinned to the old commit, read that commit's failed run, and
# refused for the wrong reason. Refusing was the right outcome by luck; pinning
# to a commit that is no longer the head is not.
#
# So: if the local checkout is on this branch, its HEAD is the truth, and this
# waits for GitHub to catch up rather than trusting the first answer.
WANT=""
if [ "$(git rev-parse --abbrev-ref HEAD 2>/dev/null)" = "$BR" ]; then
  WANT="$(git rev-parse HEAD)"
fi
while :; do
  HEAD="$(gh pr view "$PR" --json headRefOid --jq .headRefOid)"
  [ -n "$HEAD" ] || { echo "could not read PR $PR head sha"; exit 2; }
  [ -z "$WANT" ] && break
  [ "$HEAD" = "$WANT" ] && break
  echo "PR head is ${HEAD:0:7}, local is ${WANT:0:7} -- waiting for GitHub to catch up"
  [ "$(date +%s)" -gt "$DEADLINE" ] && { echo "PR head never became ${WANT:0:7}"; exit 2; }
  sleep 15
done
echo "PR #$PR head = ${HEAD:0:7}${WANT:+ (matches local)}"

# 1. A run must EXIST for this commit. This is the check the old watcher had no
#    equivalent of, and its absence is the whole bug.
until [ -n "$(runs_for "$HEAD")" ]; do
  [ "$(date +%s)" -gt "$DEADLINE" ] && { echo "no run ever appeared for ${HEAD:0:7}"; exit 2; }
  sleep 20
done
echo "run(s) registered for ${HEAD:0:7}"

# 2. Every run for this commit must finish.
while runs_for "$HEAD" | grep -qE '^(queued|in_progress|waiting|requested|pending)'; do
  if [ "$(date +%s)" -gt "$DEADLINE" ]; then
    echo "STALLED. Outstanding for ${HEAD:0:7}:"; runs_for "$HEAD" | grep -vE '^completed' | sed 's/^/  /'
    exit 2
  fi
  sleep 45
done

echo "verdict for ${HEAD:0:7}:"
runs_for "$HEAD" | sed 's/^/  /'

# 3. Merge only if every run for THIS commit concluded success or was skipped.
bad="$(runs_for "$HEAD" | awk '$2!="success" && $2!="skipped" {print}')"
if [ -n "$bad" ]; then
  echo "NOT MERGING -- not every run for ${HEAD:0:7} succeeded:"; echo "$bad" | sed 's/^/  /'
  exit 1
fi
# A commit with no Build CI run at all is not green, it is untested.
#
# EXCEPT for a docs-only change: `build.yml` has a `paths-ignore` for `**.md`,
# `docs/**` and `LICENSE`, so such a commit legitimately produces no run. That
# is why this refuses rather than assuming — it cannot tell "skipped by
# paths-ignore" from "never started" — and why the docs rule is to push those
# straight to main instead of opening a PR. If you do PR one, merge it by hand
# and say why.
runs_for "$HEAD" | grep -q 'Build CI' || {
  echo "NOT MERGING -- no 'Build CI' run for ${HEAD:0:7}."
  echo "  If this PR is docs-only, build.yml's paths-ignore skipped it on purpose;"
  echo "  merge by hand. Otherwise the run never started and the commit is untested."
  exit 1; }

gh pr merge "$PR" --squash --delete-branch >/dev/null 2>&1 || { echo "merge failed"; exit 1; }
git checkout -q main && git pull -q --ff-only origin main
SHA="$(git rev-parse HEAD)"; echo "merged; main = ${SHA:0:7}"

# 4. Post-merge, pinned the same way.
pruns() { gh run list --branch main --limit 20 --json headSha,status,conclusion,name \
  --jq ".[]|select(.headSha==\"$SHA\")|\"\(.status) \(.conclusion // \"-\") \(.name)\""; }
until [ -n "$(pruns)" ]; do
  [ "$(date +%s)" -gt "$DEADLINE" ] && { echo "no post-merge run appeared for ${SHA:0:7}"; exit 2; }
  sleep 20
done
while pruns | grep -qE '^(queued|in_progress|waiting|requested|pending)'; do
  if [ "$(date +%s)" -gt "$DEADLINE" ]; then
    echo "post-merge STALLED for ${SHA:0:7}:"; pruns | grep -vE '^completed' | sed 's/^/  /'; exit 2
  fi
  sleep 45
done
echo "post-merge ${SHA:0:7}:"; pruns | sed 's/^/  /'
pruns | awk '$2!="success" && $2!="skipped"' | grep -q . && { echo "POST-MERGE RED"; exit 1; }
echo "post-merge green"
