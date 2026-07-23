/* Translation unit bindgen parses to produce the C ABI declarations.
 *
 * Deliberately narrow: it pulls in Compression.h (CALLBACK_FUNC, the
 * FREEARC_ERRCODE_* constants) and Common.h (MemSize) and nothing else, so the
 * generated bindings stay small and reviewable rather than covering the whole
 * C++ surface of the codebase.
 */
#include "Common.h"
#include "Compression.h"
