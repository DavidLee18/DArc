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
