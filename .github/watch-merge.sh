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
#
# ── ...and then it reintroduced the same class in its own wait ──────────────
#
# `while runs_for "$HEAD" | grep -qE '^(queued|in_progress|…)'` stops when grep
# finds no pending line -- which is true when nothing is pending AND when
# `gh run list` answered with nothing at all. The API does transiently return an
# empty list, and the loop read that non-answer as "done".
#
# Live on PR #161: `run(s) registered`, then straight to
# `verdict: in_progress Build CI` and `NOT MERGING`. A re-run merged on the
# first attempt, so the PR was green throughout and the watcher was not.
#
# It failed SAFE, and by design rather than by luck: step 3 re-queries, and the
# `Build CI`-exists check refuses an empty answer outright, so a transient can
# never produce a merge. But it stops WAITING, which is the whole job.
#
# So the rule the header already states has to hold for the waiting too: a
# question whose answer can be "I don't know" must not have "no" as its default.
# `pending`/`ppending` below treat an empty or failed query as KEEP WAITING.
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
# 0 while a run is still going -- OR while the answer is unusable. An empty
# reply is NOT "nothing pending": `gh run list` can transiently return nothing,
# and `runs_for | grep -q` reads that as done and drops out of the wait. That is
# what happened on PR #161: the loop exited with Build CI still in_progress.
# Failing safe, because step 3 re-queries and the Build-CI-exists check refuses
# an empty answer -- but it stops WAITING, which is the whole job.
pending() {
  local out
  out="$(runs_for "$1")" || return 0
  [ -n "$out" ] || return 0
  printf '%s\n' "$out" | grep -qE '^(queued|in_progress|waiting|requested|pending)'
}

while pending "$HEAD"; do
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
# The post-merge wait has the same flaw and the same fix. It matters slightly
# less -- nothing is merged past this point -- but a watcher that stops watching
# still reports on a run it did not see finish.
ppending() {
  local out
  out="$(pruns)" || return 0
  [ -n "$out" ] || return 0
  printf '%s\n' "$out" | grep -qE '^(queued|in_progress|waiting|requested|pending)'
}
while ppending; do
  if [ "$(date +%s)" -gt "$DEADLINE" ]; then
    echo "post-merge STALLED for ${SHA:0:7}:"; pruns | grep -vE '^completed' | sed 's/^/  /'; exit 2
  fi
  sleep 45
done
echo "post-merge ${SHA:0:7}:"; pruns | sed 's/^/  /'
pruns | awk '$2!="success" && $2!="skipped"' | grep -q . && { echo "POST-MERGE RED"; exit 1; }
echo "post-merge green"
