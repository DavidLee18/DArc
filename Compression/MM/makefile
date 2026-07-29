include ../../common.mak

ALL: $(TEMPDIR)/C_MM.o $(TEMPDIR)/C_TTA.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fno-strict-aliasing \
              -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

# OPT_FLAGS above no longer decides any MM or TTA byte -- both codecs are Rust,
# and these two objects hold the method objects, the parsers and the detector.
# The flag list rust/difftest/c-reference.sh uses to build the MM and TTA
# oracles must therefore match the makefile at the PINNED revision, not this
# one; changing the flags here will not move the oracle.
#
# mmdet.cpp is still a real dependency: C_MM.cpp #includes it, because the
# Haskell FFI calls into it for $text/$exe/$compressed grouping.
$(TEMPDIR)/C_MM.o: C_MM.cpp C_MM.h mmdet.cpp mmdet.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/C_TTA.o: C_TTA.cpp C_TTA.h ttaenc.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
