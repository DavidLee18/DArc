include ../../common.mak

ALL: $(TEMPDIR)/C_Encryption.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fno-strict-aliasing \
              -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_Encryption.o: C_Encryption.cpp C_Encryption.h makefile
# No -Iheaders: C_Encryption.cpp includes only C_Encryption.h, which includes
# ../Compression.h. The vendored LibTomCrypt it used to need is gone -- the
# difftest oracle builds its reference from the pinned revision instead.
	$(GCC) -c $(CFLAGS) -o $*.o $<
