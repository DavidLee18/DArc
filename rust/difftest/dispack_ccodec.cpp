/* The C DisPack codec for the differential harness.
 *
 * C_DisPack.cpp includes Compression.h (inside extern "C") and DisPack.cpp
 * itself, exactly as the real build compiles it, so no amalgamation is needed.
 * DISPACK_METHOD::decompress and ::compress are the entry points; the driver
 * calls them through a tiny shim, since the class definition lives here.
 */
#include "../../Compression/DisPack/C_DisPack.cpp"

// Archiver plumbing C_DisPack.cpp drags in that the block path never touches.
// AddCompressionMethod registers the codec at static-init; the vtable needs the
// COMPRESSION_METHOD base's typeinfo and its doit(). Stubbed here rather than
// pulling in CompressionLibrary.cpp and every other codec.
int AddCompressionMethod (CM_PARSER) { return 0; }
int COMPRESSION_METHOD::doit (char *, int, void *, CALLBACK_FUNC *) { return -1; }

extern "C" {
int darc_dispack_decompress (unsigned block_size, CALLBACK_FUNC *cb, void *aux)
{ DISPACK_METHOD m; m.BlockSize = block_size; return m.decompress(cb, aux); }

int darc_dispack_compress (unsigned block_size, CALLBACK_FUNC *cb, void *aux)
{ DISPACK_METHOD m; m.BlockSize = block_size; return m.compress(cb, aux); }
}
