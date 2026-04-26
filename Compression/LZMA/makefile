include ../../common.mak

ALL: $(TEMPDIR)/C_LZMA.o $(TEMPDIR)/C_LZMA2.o $(TEMPDIR)/C_BCJ.o \
     $(TEMPDIR)/LzmaEnc.o $(TEMPDIR)/LzmaDec.o \
     $(TEMPDIR)/Lzma2Enc.o $(TEMPDIR)/Lzma2Dec.o \
     $(TEMPDIR)/MtCoder.o  $(TEMPDIR)/MtDec.o  $(TEMPDIR)/7zStream.o \
     $(TEMPDIR)/LzFind.o   $(TEMPDIR)/LzFindOpt.o \
     $(TEMPDIR)/CpuArch.o  $(TEMPDIR)/LzFindMt.o $(TEMPDIR)/Threads.o

CODE_FLAGS = -Wall -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS  = -O2 -fomit-frame-pointer -fstrict-aliasing -ffast-math -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES) -I7z24

$(TEMPDIR)/C_LZMA.o: C_LZMA.cpp C_LZMA.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_LZMA2.o: C_LZMA2.cpp C_LZMA2.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

$(TEMPDIR)/C_BCJ.o: C_BCJ.cpp C_BCJ.h makefile
	$(GCC) -c $(CFLAGS) -fno-rtti -o $*.o $<

# --- 7-Zip 24.09 C sources (compile as C, not C++) ---
CC_C      ?= $(firstword $(CC) cc)
C7Z_CFLAGS = -std=c11 -O2 -DNDEBUG -D_REENTRANT $(DEFINES)

$(TEMPDIR)/LzmaEnc.o:  7z24/LzmaEnc.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/Lzma2Enc.o: 7z24/Lzma2Enc.c ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/Lzma2Dec.o: 7z24/Lzma2Dec.c ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/MtCoder.o:  7z24/MtCoder.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/MtDec.o:    7z24/MtDec.c    ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/7zStream.o: 7z24/7zStream.c ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/LzmaDec.o:  7z24/LzmaDec.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/LzFind.o:   7z24/LzFind.c   ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/LzFindMt.o: 7z24/LzFindMt.c ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/Threads.o:  7z24/Threads.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/LzFindOpt.o:7z24/LzFindOpt.c; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
$(TEMPDIR)/CpuArch.o:  7z24/CpuArch.c  ; $(CC_C) -c $(C7Z_CFLAGS) -o $@ $<
