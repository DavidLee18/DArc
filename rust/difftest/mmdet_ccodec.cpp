/* mmdet.cpp for the differential harness, as its own translation unit.
 *
 * tta_compress references mmdet's autodetection entry points, so they must link
 * even though the harness passes explicit parameters and never calls them.
 * mmdet.cpp has no includes of its own (mm.cpp includes it after Compression.h)
 * and defines a file-local `channels[]` that collides with tta.cpp's, so it is
 * compiled separately here -- mirroring the real C_MM.o / C_TTA.o split.
 * MMD_LIBRARY suppresses its benchmark driver (which needs <io.h>/<windows.h>).
 */
#define MMD_LIBRARY
#include "../../Compression/Compression.h"
#include "../../Compression/MM/mmdet.cpp"
