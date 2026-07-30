include ../../common.mak

# LZMA, LZMA2 and BCJ are Rust now (rust/darc-lzma, rust/darc-codecs/src/bcj.rs).
# All that is left here is the three DArc wrappers: method objects, parameter
# parsers and the FFI hand-off. No vendored 7-Zip SDK remains -- the .7z container
# reader compiles its OWN copy under Compression/7z/sdk and merges it into a single
# C_7z.o with the symbols localized, so it never depended on this directory's.
# See rust/darc-lzma/PROVENANCE.md.
ALL: $(TEMPDIR)/C_LZMA.o $(TEMPDIR)/C_LZMA2.o $(TEMPDIR)/C_BCJ.o

CODE_FLAGS = -Wall -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS  = -O2 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_LZMA.o: C_LZMA.cpp C_LZMA.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_LZMA2.o: C_LZMA2.cpp C_LZMA2.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_BCJ.o: C_BCJ.cpp C_BCJ.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

