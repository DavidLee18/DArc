/* The C GRZip codec for the differential harness.
 *
 * C_GRZip.cpp includes its six component .c files itself, so this is a single
 * translation unit like the real build. The shim at the bottom exposes the two
 * block entry points, which are file-scope in C_GRZip.cpp and absent from
 * libGRZip.h -- the driver cannot declare them itself.
 *
 * The block level is deliberately where this harness cuts. grzip_decompress
 * (the stream wrapper) is not ported yet, but GRZip_DecompressBlock is, and it
 * is where every stage that has been ported actually runs.
 */
// C_GRZip.cpp registers itself as a COMPRESSION_METHOD at static-init time and
// asks the library how many threads to use. Neither matters at the block level,
// and pulling in CompressionLibrary.cpp would drag in every other codec, so the
// two symbols are stubbed. GetCompressionThreads only sizes a memory estimate.
#include "../../Compression/GRZip/C_GRZip.cpp"
// The real synchronization primitives, textually included rather than compiled
// standalone: on their own they fail on TRUE/FALSE, which C_GRZip.cpp's include
// chain has already defined by this point. Cheaper and less fragile than
// stubbing the whole worker-thread pool.
#include "../../Compression/LZMA/Windows/Synchronization.cpp"

// Stubs for the archiver-side plumbing C_GRZip.cpp pulls in. None of it is on
// the block path: AddCompressionMethod registers the method at static-init
// time, GetCompressionThreads only sizes a memory estimate, and LoadFromDLL /
// WaitForMultipleObjects belong to the stream-level multithreaded wrapper
// (grzip_compress / grzip_decompress), which this harness deliberately does not
// exercise. Linking the real ones would drag in CompressionLibrary.cpp and
// every other codec with it.
int AddCompressionMethod (CM_PARSER)   { return 0; }
extern "C" int GetCompressionThreads (void) { return 1; }
FARPROC LoadFromDLL (char *)           { return 0; }
// Signature per the linker: (count, handles, wait_all, timeout).
// COMPRESSION_METHOD::doit is only reachable through GRZIP_METHOD, which the
// block path never constructs; it exists here to satisfy the vtable.
int COMPRESSION_METHOD::doit (char *, int, void *, CALLBACK_FUNC *) { return -1; }

extern "C" {
int darc_grz_compress_block (unsigned char *in, int size, unsigned char *out, int mode)
{ return GRZip_CompressBlock (in, size, out, mode); }

int darc_grz_decompress_block (unsigned char *in, int size, unsigned char *out)
{ return GRZip_DecompressBlock (in, size, out); }
}
