/* Amalgamation of the C TTA codec for the differential harness.
 *
 * tta.cpp / entropy.cpp / filters.cpp carry no includes of their own -- they
 * are meant to be textually included after Compression.h, exactly as C_TTA.cpp
 * does it. This mirrors C_TTA.cpp's structure verbatim (minus the method-class
 * wrapper it does not need):
 *
 *   - Compression.h and mmdet.h go inside `extern "C"`, so tta.cpp's calls to
 *     the autodetection entry points take C linkage -- matching mmdet.cpp's own
 *     `extern "C"` definitions over in mmdet_ccodec.cpp. Getting this wrong is a
 *     link error about a missing C++-mangled `autodetect_*`.
 *   - entropy/filters/tta.cpp are included with C++ linkage, so tta_compress /
 *     tta_decompress are C++-mangled, matching the declarations in tta_ref.cpp.
 *
 * TTA_LIBRARY suppresses tta.cpp's own main()/driver (which needs <io.h>).
 * mmdet.cpp is a SEPARATE unit (mmdet_ccodec.cpp) because it too defines a
 * file-local `channels[]`, which would redefine tta.cpp's in one unit.
 */
#define TTA_LIBRARY
extern "C" {
#include "../../Compression/Compression.h"
#include "../../Compression/MM/mmdet.h"
}
#include "../../Compression/MM/entropy.cpp"
#include "../../Compression/MM/filters.cpp"
#include "../../Compression/MM/tta.cpp"
