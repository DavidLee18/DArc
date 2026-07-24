include ../../common.mak

# The Zstandard codec itself is the `zstd-safe` crate, fetched by cargo. The
# vendored libzstd tree this makefile used to compile (2.2 MB, 68 files) is
# gone, so all that is left is DArc's own wrapper: parameter parsing, memory
# estimation and method registration, forwarding to rust/darc-codecs.
ALL: $(TEMPDIR)/C_Zstd.o

CODE_FLAGS = -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion \
             -Wno-unused-function -Wno-unused-variable -Wno-unused-but-set-variable
OPT_FLAGS  = -O3 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops

$(TEMPDIR)/C_Zstd.o: C_Zstd.cpp C_Zstd.h makefile
	$(GCC) -c -fno-exceptions -fno-rtti $(CODE_FLAGS) $(OPT_FLAGS) $(DEFINES) -o $@ $<
