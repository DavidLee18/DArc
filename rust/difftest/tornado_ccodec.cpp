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
// `notables` clears find_tables, which is the only way to reach preset 3's
// match finder (MatchFinder2) while the data-table detector is still unported.
// Without it that finder would be exercised by nothing at all, and a preset
// list that silently skips it reads exactly like one that covers it.
int tor_compress_preset (int preset, int notables, CALLBACK_FUNC *callback, void *auxdata)
{
    PackMethod m = std_Tornado_method[preset];
    if (notables)  m.find_tables = false;
    return tor_compress (m, callback, auxdata);
}

#ifdef USE_RUST
// The Rust encoder takes PackMethod by value, exactly as tor_compress does.
// PackMethod is defined inside Tornado.cpp, which only this translation unit
// includes, so the shim has to live here rather than in the driver.
//
// The second argument is the C's `compress_all_at_once` global. A drop-in
// replacement for tor_compress could not see it -- it is a file-scope int in
// Common.cpp, not a parameter -- so the Rust side takes it explicitly and the
// harness passes the live value, keeping the two encoders on the same setting.
extern "C" int darc_rs_tor_compress (PackMethod m, int all_at_once,
                                     CALLBACK_FUNC *callback, void *auxdata);

extern "C" int rust_tor_compress_preset (int preset, int notables,
                                        CALLBACK_FUNC *callback, void *auxdata)
{
    PackMethod m = std_Tornado_method[preset];
    if (notables)  m.find_tables = false;
    return darc_rs_tor_compress (m, compress_all_at_once, callback, auxdata);
}
#endif
