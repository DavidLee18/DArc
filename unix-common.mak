#For Unix:
DEFINES  = -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER
ifeq ($(shell getconf LONG_BIT 2>/dev/null),64)
DEFINES  += -DFREEARC_64BIT
endif
TEMPDIR  = /tmp/out/FreeArc
GCC      = clang++ -std=c++17
ifeq ($(shell pkg-config --exists libcurl 2>/dev/null && echo yes),yes)
EXTRA_CFLAGS = $(shell pkg-config --cflags libcurl 2>/dev/null)
else
DEFINES  += -DFREEARC_NOURL
EXTRA_CFLAGS =
endif

# Apple's clang ships without OpenMP, so -fopenmp is a hard error there.
# Nothing is lost by omitting it: libbsc only takes its OpenMP paths when
# LIBBSC_OPENMP_SUPPORT is defined as well, and this build never defines it,
# so the flag is inert everywhere. Kept on non-Darwin so that defining that
# macro later actually enables parallelism.
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
OMP_FLAGS =
DEFINES  += -DFREEARC_MACOS
else
OMP_FLAGS = -fopenmp
endif
