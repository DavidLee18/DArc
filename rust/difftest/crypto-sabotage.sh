#!/usr/bin/env bash
# Does crypto-check.sh actually exercise the encryption port?
#
# It went green on its first complete run, which is the situation in which a
# check has most often turned out here to be passing without reaching the code
# it names. So each path gets a deliberate one-line break and the harness has
# to notice. A sabotage that survives means the corpus does not reach it.
#
# The mutations are chosen to be independently reachable: one breaks CTR from
# the first block, one breaks it only past docrypt's 256 KB read chunk, one
# breaks CFB in the decrypt direction alone, one breaks the key derivation, and
# two break the production forwarding shim rather than the algorithms.
#
# Discipline this harness enforces on itself, learned the hard way:
#   * the baseline copy must exist before anything is edited, or a failed
#     restore silently corrupts the tree;
#   * every edit must be confirmed to have applied, or a no-op patch reports
#     "not caught" while nothing was ever broken;
#   * a build failure must be reported as INCONCLUSIVE, not as a catch -- the
#     check exits non-zero for that too.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"   # absolute: the cd below breaks $0
cd "$HERE/.." || exit 1   # rust/
CHECK="difftest/crypto-check.sh"
SRC=darc-crypto/src
# cipher.rs joined the list when the cipher table moved there; the sabotage
# below edits it, and a file that is not backed up here is not restored after.
FILES="ctr.rs cfb.rs lib.rs exports.rs cipher.rs"
BACKUP=$(mktemp -d)
FAILED=0

for f in $FILES; do
    cp "$SRC/$f" "$BACKUP/$f" || { echo "FATAL: cannot back up $f"; exit 1; }
    [ -s "$BACKUP/$f" ] || { echo "FATAL: backup of $f is empty"; exit 1; }
done

restore() {
    for f in $FILES; do
        cp "$BACKUP/$f" "$SRC/$f" || { echo "FATAL: restore of $f failed"; exit 1; }
        cmp -s "$BACKUP/$f" "$SRC/$f" || { echo "FATAL: $f differs after restore"; exit 1; }
    done
}
trap 'restore; rm -rf "$BACKUP"' EXIT

# sabotage <name> <file> <from> <to>
sabotage() {
    local name=$1 file=$2 from=$3 to=$4
    restore
    # Confirm the target text is present before editing, so a stale pattern
    # cannot masquerade as an uncaught sabotage.
    if ! grep -qF -- "$from" "$SRC/$file"; then
        echo "BROKEN HARNESS: [$name] pattern not found in $file:"
        echo "    $from"
        FAILED=1
        return
    fi
    sh "$HERE/patch-once.sh" "$SRC/$file" "$from" "$to"
    if [ $? -ne 0 ]; then
        echo "BROKEN HARNESS: [$name] edit did not apply cleanly"
        FAILED=1
        return
    fi
    if ! grep -qF -- "$to" "$SRC/$file"; then
        echo "BROKEN HARNESS: [$name] replacement text absent after edit"
        FAILED=1
        return
    fi

    local out rc
    out=$("$CHECK" 2>&1); rc=$?
    # A crate that no longer compiles also makes the check exit non-zero, so
    # that case is separated before the exit status is trusted.
    if echo "$out" | grep -q "cargo build failed"; then
        echo "INCONCLUSIVE: [$name] the crate did not compile (sabotage was not testable)"
        FAILED=1
    elif [ $rc -ne 0 ]; then
        echo "caught:     [$name]"
    else
        echo "SURVIVED:   [$name]  <-- crypto-check.sh does not reach this code"
        FAILED=1
    fi
}

echo "=== baseline ==="
if ! "$CHECK" >/dev/null 2>&1; then
    echo "FATAL: clean tree does not pass crypto-check.sh; nothing below is meaningful"
    exit 1
fi
echo "clean tree passes"

echo "=== sabotages ==="

# CTR's counter is incremented little-endian (CTR_COUNTER_LITTLE_ENDIAN in
# ctr_start). Carrying from the wrong end is the classic CTR porting mistake
# and changes the keystream from the second block onwards.
sabotage "ctr: big-endian counter" ctr.rs \
    "    for byte in counter.iter_mut() {" \
    "    for byte in counter.iter_mut().rev() {"

# docrypt reads in 256 KB chunks and the mode state has to survive the boundary.
# Clearing `started` per call makes the first block of every chunk after the
# first reuse the previous counter value -- invisible on any input under 256 KB.
sabotage "ctr: state reset at each read chunk" ctr.rs \
    "        for byte in data.iter_mut() {" \
    "        self.started = false; for byte in data.iter_mut() {"

# The CFB register accumulates ciphertext in BOTH directions. Feeding back the
# byte in hand instead is a no-op while encrypting (it has just been overwritten
# with the ciphertext) and wrong while decrypting -- so this one is caught only
# if the corpus decrypts as well as encrypts.
sabotage "cfb: plaintext fed back when decrypting" cfb.rs \
    "            self.feedback[self.pos] = cipher_byte;" \
    "            self.feedback[self.pos] = *byte;"

# cfb_start encrypts the IV before any data is processed, so the first keystream
# block is E(IV) rather than the IV itself.
sabotage "cfb: IV not pre-encrypted" cfb.rs \
    "        encrypt_in_place(cipher, &mut keystream);" \
    "        if false { encrypt_in_place(cipher, &mut keystream); }"

# The iteration count is recorded in the archive and must be honoured exactly;
# one extra round derives a key nothing can reproduce.
sabotage "pbkdf2: one extra iteration" lib.rs \
    "    pbkdf2::pbkdf2::<Hmac<Sha512>>(password, salt, iterations, out)" \
    "    pbkdf2::pbkdf2::<Hmac<Sha512>>(password, salt, iterations + 1, out)"

# Cipher ids are positions in LibTomCrypt's registration table, hard-coded on
# the Rust side. Nothing in the type system ties them together.
sabotage "shim: cipher id 2 dispatches to the wrong cipher" cipher.rs \
    "            2 => Some(Cipher::Serpent)," \
    "            2 => Some(Cipher::Twofish),"

# The direction flag crosses the FFI boundary as an int. CTR does not care;
# CFB does, so this is caught only by the decrypt half of the corpus.
sabotage "shim: direction flag ignored" exports.rs \
    "    let encrypting = do_encryption == ENCRYPT;" \
    "    let encrypting = true;"

restore
echo "=== restored ==="
for f in $FILES; do
    cmp -s "$BACKUP/$f" "$SRC/$f" || { echo "FATAL: $f not restored"; exit 1; }
done
echo "working tree is byte-identical to the baseline"

[ "$FAILED" -eq 0 ] || { echo "crypto-sabotage: some mutations were not caught"; exit 1; }
echo "crypto-sabotage: every mutation was caught"
