// C oracle for detect_datatype / detect_mm / detect_mm_header.
//
// Reads a buffer from stdin and prints one line:
//
//     <datatype> <is_mm-fast> <is_mm-thorough> <is_mm_header> <mm_bytes>
//
// All five on one line so a single run compares every entry point, and so a
// disagreement names which one differs rather than only that something did.
//
// `mmdet.cpp` is compiled by #include, the way `mm.cpp` and `tta.cpp` include
// it, because it has no header of its own beyond the four prototypes and its
// helpers are static.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MMD_LIBRARY 1        // suppress mmdet.cpp's standalone main()
#include "Compression/MM/mmdet.cpp"

int main(int argc, char **argv)
{
    // -1 asks for the list of recognizable types, which the Haskell reads to
    // decide whether autodetection is worth running at all.
    if (argc > 1 && strcmp(argv[1], "--types") == 0) {
        char types[1000];
        detect_datatype(NULL, 0, types);
        printf("%s\n", types);
        return 0;
    }

    static unsigned char buf[8 << 20];
    int n = (int) fread(buf, 1, sizeof(buf), stdin);

    char type[1000];
    detect_datatype(buf, n, type);

    int fast     = detect_mm(1, buf, n);
    int thorough = detect_mm(3, buf, n);
    int header   = detect_mm_header(1, buf, n);
    int bytes    = detect_mm_bytes(3, n);

    printf("%s %d %d %d %d\n", type, fast != 0, thorough != 0, header != 0, bytes);
    return 0;
}
