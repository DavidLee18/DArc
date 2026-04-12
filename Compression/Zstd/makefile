include ../../common.mak

ALL: $(TEMPDIR)/C_Zstd.o

ZSTD_SRC   = libzstd
ZSTD_INC   = -I$(ZSTD_SRC) -I$(ZSTD_SRC)/common
CODE_FLAGS = -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion \
             -Wno-unused-function -Wno-unused-variable -Wno-unused-but-set-variable
OPT_FLAGS  = -O3 -fomit-frame-pointer -fstrict-aliasing -funroll-loops
ZSTD_DEFS  = -DZSTD_MULTITHREAD -DXXH_NAMESPACE=ZSTD_ -DZSTD_LEGACY_SUPPORT=0 -DZSTD_DISABLE_ASM

ZSTD_CSRC := $(wildcard $(ZSTD_SRC)/common/*.c) \
             $(wildcard $(ZSTD_SRC)/compress/*.c) \
             $(wildcard $(ZSTD_SRC)/decompress/*.c)
ZSTD_OBJS := $(patsubst %.c,$(TEMPDIR)/zstd_%.o,$(notdir $(ZSTD_CSRC)))

VPATH = $(ZSTD_SRC)/common:$(ZSTD_SRC)/compress:$(ZSTD_SRC)/decompress

$(TEMPDIR)/zstd_%.o: %.c
	clang -c -std=c99 $(OPT_FLAGS) $(CODE_FLAGS) $(ZSTD_INC) $(ZSTD_DEFS) -pthread -o $@ $<

$(TEMPDIR)/C_Zstd_wrapper.o: C_Zstd.cpp C_Zstd.h makefile
	$(GCC) -c -fno-exceptions -fno-rtti $(CODE_FLAGS) $(OPT_FLAGS) $(ZSTD_INC) $(ZSTD_DEFS) $(DEFINES) -o $@ $<

# Merge wrapper + all zstd TUs into a single C_Zstd.o so the top-level
# compile script only needs one object to link.
$(TEMPDIR)/C_Zstd.o: $(TEMPDIR)/C_Zstd_wrapper.o $(ZSTD_OBJS)
	ld -r -o $@ $^
