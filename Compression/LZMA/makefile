include ../../common.mak

# LZMA, LZMA2 and BCJ are Rust now (rust/darc-lzma, rust/darc-codecs/src/bcj.rs).
# What is left here is the three DArc wrappers -- method objects, parameter parsers
# and the FFI hand-off -- plus the handful of 7-Zip SDK files that the .7z container
# reader still needs. The LZMA engine itself (LzmaEnc, LzmaDec, LzFind*, Lzma2*,
# MtCoder, MtDec, Threads) is gone; see rust/darc-lzma/PROVENANCE.md.
ALL: $(TEMPDIR)/C_LZMA.o $(TEMPDIR)/C_LZMA2.o $(TEMPDIR)/C_BCJ.o \
     $(TEMPDIR)/7zStream.o $(TEMPDIR)/CpuArch.o

CODE_FLAGS = -Wall -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS  = -O2 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES) -I7z24

$(TEMPDIR)/C_LZMA.o: C_LZMA.cpp C_LZMA.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_LZMA2.o: C_LZMA2.cpp C_LZMA2.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_BCJ.o: C_BCJ.cpp C_BCJ.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

# --- 7-Zip 24.09 C sources (compile as C, not C++) ---
CC_C      ?= $(firstword $(CC) cc)
C7Z_CFLAGS = -std=c11 -O2 -DNDEBUG -D_REENTRANT $(DEFINES)

$(TEMPDIR)/7zStream.o: 7z24/7zStream.c ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/CpuArch.o:  7z24/CpuArch.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
