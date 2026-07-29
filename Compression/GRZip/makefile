include ../../common.mak

ALL: $(TEMPDIR)/C_GRZip.o

CODE_FLAGS = -fno-exceptions -fno-rtti \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O2 \
              -fomit-frame-pointer \
 -fno-strict-aliasing -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

# OPT_FLAGS above no longer decides any GRZip byte: this file compiles the
# method object and the parser, and the codec itself is Rust. The flag list
# rust/difftest/c-reference.sh uses to build the GRZip oracle must therefore
# match the makefile at the PINNED revision, not this one -- changing the flags
# here will not move the oracle, and must not be assumed to.
$(TEMPDIR)/C_GRZip.o: C_GRZip.cpp C_GRZip.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
