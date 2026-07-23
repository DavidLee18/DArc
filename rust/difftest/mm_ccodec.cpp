/* The C MM codec for the differential harness.
 *
 * Unlike the TTA side, this needs no amalgamation: mm.cpp already includes
 * Compression.h (inside `extern "C"`) and pulls in mmdet.cpp itself, exactly as
 * C_MM.cpp compiles it. MM_LIBRARY suppresses mm.cpp's own main()/driver, which
 * wants <io.h>; mm.cpp defines MMD_LIBRARY for mmdet.cpp on its own.
 *
 * mm_compress/mm_decompress are defined outside that `extern "C"` block, so
 * they are C++-mangled here -- matching the declarations in mm_ref.cpp. (In the
 * real build C_MM.cpp declares mm_decompress inside `extern "C"` first, and the
 * definition inherits that linkage; that is what lets the Rust drop-in take
 * over the symbol.)
 */
#define MM_LIBRARY
#include "../../Compression/MM/mm.cpp"
