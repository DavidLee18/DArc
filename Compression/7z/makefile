include ../../common.mak

ALL: $(TEMPDIR)/C_7z.o

SDK_DIR    = sdk
SDK_INC    = -I$(SDK_DIR)
CODE_FLAGS = -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion \
             -Wno-unused-function -Wno-unused-variable -Wno-unused-but-set-variable \
             -Wno-pointer-sign -Wno-parentheses
OPT_FLAGS  = -O2 -fomit-frame-pointer
# _7ZIP_ST = single-threaded; we only need reader, no multi-threaded decode.
SDK_DEFS   = -D_7ZIP_ST -DZ7_ST

SDK_CSRC := $(SDK_DIR)/7zAlloc.c $(SDK_DIR)/7zArcIn.c $(SDK_DIR)/7zBuf.c \
            $(SDK_DIR)/7zCrc.c $(SDK_DIR)/7zCrcOpt.c $(SDK_DIR)/7zDec.c \
            $(SDK_DIR)/7zStream.c $(SDK_DIR)/7zFile.c \
            $(SDK_DIR)/LzmaDec.c $(SDK_DIR)/Lzma2Dec.c \
            $(SDK_DIR)/Ppmd7.c $(SDK_DIR)/Ppmd7Dec.c \
            $(SDK_DIR)/Delta.c $(SDK_DIR)/Bra.c $(SDK_DIR)/Bra86.c \
            $(SDK_DIR)/BraIA64.c $(SDK_DIR)/Bcj2.c \
            $(SDK_DIR)/Alloc.c $(SDK_DIR)/CpuArch.c
SDK_OBJS := $(patsubst $(SDK_DIR)/%.c,$(TEMPDIR)/sz_%.o,$(SDK_CSRC))

$(TEMPDIR)/sz_%.o: $(SDK_DIR)/%.c
	clang -c -std=c99 $(OPT_FLAGS) $(CODE_FLAGS) $(SDK_INC) $(SDK_DEFS) -o $@ $<

$(TEMPDIR)/C_7z_wrapper.o: C_7z.c makefile
	clang -c -std=c99 $(OPT_FLAGS) $(CODE_FLAGS) $(SDK_INC) $(SDK_DEFS) $(DEFINES) -o $@ $<

# Merge wrapper + all SDK TUs into a single C_7z.o; localize all symbols
# except the three exported entry points so we don't clash with LZMA/PPMD
# decoders already linked via the main LZMA codec.
$(TEMPDIR)/C_7z.o: $(TEMPDIR)/C_7z_wrapper.o $(SDK_OBJS)
	ld -r -o $@.tmp $^
	objcopy --localize-hidden \
	  --keep-global-symbol=darc_7z_list \
	  --keep-global-symbol=darc_7z_extract \
	  --keep-global-symbol=darc_7z_test \
	  $@.tmp $@
	rm -f $@.tmp
