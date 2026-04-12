include ../../common.mak

ALL: $(TEMPDIR)/C_BSC.o

# Match the rest of DArc: no RTTI / no exceptions. COMPRESSION_METHOD is compiled
# without typeinfo, so BSC_METHOD must be too, otherwise the linker fails.
CODE_FLAGS  = -fno-exceptions -fno-rtti \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion -Wno-unused-function -Wno-unused-variable
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fstrict-aliasing -funroll-loops
DEBUG_FLAGS = -g0
# libbsc/libsais use OpenMP for parallelism when available.
OMP_FLAGS   = -fopenmp
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(OMP_FLAGS) $(DEFINES)

$(TEMPDIR)/C_BSC.o: C_BSC.cpp C_BSC.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
