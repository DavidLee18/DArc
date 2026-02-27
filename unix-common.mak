#For Unix:
DEFINES  = -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER
ifeq ($(shell uname -m),x86_64)
DEFINES  += -DFREEARC_64BIT
endif
TEMPDIR  = /tmp/out/FreeArc
GCC      = g++ -std=c++23
EXTRA_CFLAGS = $(shell pkg-config --cflags libcurl 2>/dev/null)
