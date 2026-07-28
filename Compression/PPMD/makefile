include ../../common.mak

ALL: $(TEMPDIR)/C_PPMD.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
# -O1 and -fno-strict-aliasing were load-bearing while Model.cpp was compiled
# here: its (WORD&) type-punning made the compressed output depend on them.
# The model is Rust now and this file is a thin wrapper, so the flags are just
# flags again -- but rust/difftest/ppmd-check.sh still builds its C oracle from
# THIS list via darc_codec_cflags, so changing it changes what that harness
# proves. Read the note in Compression/PPMD/readme before touching it.
OPT_FLAGS   = -O1 \
              -fomit-frame-pointer -fno-strict-aliasing \
              -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_PPMD.o: C_PPMD.cpp C_PPMD.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
