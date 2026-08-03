#!/usr/bin/env sh
# Replace the SINGLE occurrence of a literal string in a file, or fail.
#
#   patch-once.sh <file> <from> <to>
#
# The sabotage harnesses use this to introduce one deliberate bug into a copy of
# a source tree and prove the difftest catches it. "Exactly one occurrence" is
# the whole contract: a pattern that matched twice would sabotage two places and
# a pattern that matched none would sabotage nothing, and BOTH would leave the
# harness reporting a pass for a build it never actually broke.
#
# This replaced an identical five-line python3 heredoc in crypto-sabotage.sh and
# tornado-encoder-sabotage.sh. Both strings are LITERAL, never patterns, which
# is why the count uses `grep -F` and the substitution quotes with `\Q…\E`;
# `sed` would read regex metacharacters in C source (`*`, `[`, `.`, `\`) as
# syntax, silently changing what gets edited.
set -eu

file=$1
from=$2
to=$3

# Counted over the WHOLE FILE, not line by line. Several of these patterns span
# two lines, and `grep` is line-based: given a two-line pattern it treats each
# line as a separate pattern and reports two matches for one occurrence, which
# reads as "ambiguous" and refuses a perfectly good edit. `-0777` slurps, and
# `quotemeta` keeps the string literal.
n=$(PATCH_FROM=$from perl -0777 -ne \
    'my $p = quotemeta $ENV{PATCH_FROM}; my $c = () = /$p/g; print $c' "$file")
if [ "$n" != 1 ]; then
    echo "patch-once: pattern occurs $n times in $file, need exactly 1" >&2
    exit 1
fi

# Both values go through the environment rather than the command line so that
# neither can be read as perl source.
PATCH_FROM=$from PATCH_TO=$to perl -0pi -e 's/\Q$ENV{PATCH_FROM}\E/$ENV{PATCH_TO}/' "$file"
