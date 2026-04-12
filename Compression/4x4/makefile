include ../../common.mak

ALL: $(TEMPDIR)/C_4x4.o

CODE_FLAGS  = -fno-exceptions -fno-rtti \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -fomit-frame-pointer -fstrict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_4x4.o: C_4x4.cpp C_4x4.h ../Compression.h ../Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
