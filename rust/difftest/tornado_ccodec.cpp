/* The C Tornado codec for the differential harness.
 *
 * Tornado.cpp includes Compression.h and its four component files itself, the
 * same way C_Tornado.cpp compiles it, so no amalgamation is needed here.
 * TORNADO_LIBRARY suppresses anything driver-shaped.
 *
 * tor_compress/tor_decompress are declared inside an `extern "C"` block in
 * Tornado.cpp itself (:27-31), so they have C linkage here as well as in the
 * real build -- which is also why the Rust drop-in needs the C definition
 * excluded rather than merely shadowed.
 */
#define TORNADO_LIBRARY
#include "../../Compression/Tornado/Tornado.cpp"

// Thin shim so the driver can select a preset without seeing PackMethod's
// definition (it lives inside Tornado.cpp, which only this unit includes).
int tor_compress_preset (int preset, CALLBACK_FUNC *callback, void *auxdata)
{
    return tor_compress (std_Tornado_method[preset], callback, auxdata);
}
