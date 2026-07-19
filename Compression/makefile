include ../common.mak

ALL: $(TEMPDIR)/Common.o $(TEMPDIR)/CompressionLibrary.o

# See the top-level makefile for why -fno-strict-aliasing is required and why
# -ffast-math and the -Wno-sign-compare/-Wno-conversion suppressions are gone.
CODE_FLAGS = -fno-exceptions -fno-rtti -Wall -Wextra \
                -Wno-unknown-pragmas
OPT_FLAGS = -Os -fomit-frame-pointer \
                 -fno-strict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/Common.o: Common.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/CompressionLibrary.o: CompressionLibrary.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
