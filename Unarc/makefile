include ../common.mak

windows: unarc.exe arc.sfx freearc.sfx freearc-installer.sfx FreeArc.fmt
linux: unarc arc.linux.sfx

# DARC_RUST is not optional here. Most of the codecs Unarc links have had their
# C deleted -- C_Tornado.cpp, C_LZP.cpp, C_GRZip.cpp and the rest are now thin
# forwarders to rust/darc-codecs -- so the extractor gets its decoders from the
# same library the archiver does. Restoring the C decoders to feed the SFX
# modules instead would put two implementations of every ported codec back in
# the tree, which is the thing this port exists to remove.
Extractor_DEFINES = -DFREEARC_DECOMPRESS_ONLY -D_NO_EXCEPTIONS -DUNARC -DDARC_RUST
OBJDIR  = $(TEMPDIR)-unarc
# LZMA and LZMA2 decode in Rust now, out of libdarc_codecs.a, so no 7-Zip SDK
# object is linked here at all. The comment this replaces recorded a real bug --
# the SDK sources were split into Compression/LZMA/7z24 and this makefile was
# never updated, leaving every SFX module with an unresolved LzmaDec_* that no
# build noticed because no build ran. That whole class is gone with the C.
LINKOBJ_FAR_PLUGIN = $(OBJDIR)/Environment.o $(OBJDIR)/Common.o $(OBJDIR)/CompressionLibrary.o \
                     $(OBJDIR)/C_LZMA.o
LINKOBJ_TINY = $(LINKOBJ_FAR_PLUGIN) $(OBJDIR)/C_BCJ.o $(OBJDIR)/C_Dict.o $(OBJDIR)/C_Delta.o
LINKOBJ_MINI = $(LINKOBJ_TINY) $(OBJDIR)/C_REP.o $(OBJDIR)/C_LZP.o $(OBJDIR)/C_PPMD.o
# The full module gets every codec the archiver can put in a chain. Everything
# from C_DisPack onwards is new: the list had not been touched since those
# codecs were added, so `arc a -m4` produced archives naming methods no SFX
# module could parse -- and the failure named only the FIRST method of the
# chain, because MultiDecompress used to split the caller's string in place.
LINKOBJ = $(LINKOBJ_MINI) $(OBJDIR)/C_MM.o $(OBJDIR)/C_TTA.o \
          $(OBJDIR)/C_Tornado.o $(OBJDIR)/C_External.o $(OBJDIR)/C_GRZip.o \
          $(OBJDIR)/C_DisPack.o $(OBJDIR)/C_4x4.o $(OBJDIR)/C_BSC.o \
          $(OBJDIR)/C_LZ4.o $(OBJDIR)/C_Zstd.o \
          $(OBJDIR)/C_LZMA2.o \
          $(OBJDIR)/C_Encryption.o
HEADERS =  ../Compression/Compression.h ../Compression/Common.h

# The Rust codecs, built with the `dropin` feature so the C-ABI entry points are
# exported. It is a prerequisite of every link target, so `make linux` from a
# clean tree builds it rather than failing on undefined symbols.
#
# It goes AFTER the objects on every command line below: GNU ld resolves an
# archive only against the undefined symbols it has already seen, so a library
# placed first is silently dropped. macOS's linker does not care, which is
# exactly why this has to be got right here rather than noticed in CI.
RUSTLIB = ../rust/target/release/libdarc_codecs.a \
          ../rust/target/release/libdarc_crypto.a

#makefile
LINK_FLAGS = -Xlinker --large-address-aware -s

# -L$(LIBDIR) was here on both lines; LIBDIR is defined only in
# win32-common.mak, so on Unix it expanded to a bare `-L` that swallowed the
# following -lstdc++ as its argument. The C++ runtime comes from the compiler
# driver anyway, so both are simply gone.
#
# librt was folded into glibc long ago and does not exist on macOS at all;
# `-s` is obsolete there and only produces a warning.
ifeq ($(UNAME_S),Darwin)
UNIX_LINK_FLAGS = -lpthread
else
UNIX_LINK_FLAGS = -lrt -lpthread -s
endif


# This builds unarc and the SFX modules, which parse attacker-supplied archives
# as standalone binaries with no surrounding process to contain a fault. It is
# the most exposed code in the tree, so it gets the strictest settings:
# signedness and conversion diagnostics stay on, and aliasing is not assumed.
CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall -Wextra \
              -Wno-unknown-pragmas
OPT_FLAGS   = -fomit-frame-pointer -fno-strict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) -O2 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)
CFLAGS1= $(CODE_FLAGS) $(OPT_FLAGS) -O1 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)


# `dropin` is what exports the C-ABI entry points (tor_decompress, mm_decompress,
# tta_decompress and the rest) that the C wrappers here call by name. Without it
# the library builds and links nothing.
.PHONY: windows linux rustlib
rustlib:
	cd ../rust && cargo build --release -p darc-codecs -p darc-crypto --features darc-codecs/dropin


unarc.exe: $(OBJDIR)/unarc.o $(LINKOBJ) | rustlib
	$(GCC) $< $(LINKOBJ) $(RUSTLIB) $(LINK_FLAGS) -o unarc.exe

arc.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ) | rustlib
	$(GCC) $< $(LINKOBJ)      $(RUSTLIB) $(LINK_FLAGS) -o arc.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(RUSTLIB) $(LINK_FLAGS) -o arc-mini.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(RUSTLIB) $(LINK_FLAGS) -o arc-tiny.sfx
	upx.exe -9 --lzma --compress-icons=2 arc.sfx arc-mini.sfx arc-tiny.sfx

freearc.sfx: $(OBJDIR)/FreeArcSFX.o $(LINKOBJ) | rustlib
	$(WINDRES) -Igui gui\main.rc gui\res.o
	$(GCC) $< $(LINKOBJ)      $(RUSTLIB) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(RUSTLIB) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-mini.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(RUSTLIB) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-tiny.sfx
	upx.exe -9 --lzma --compress-icons=2 freearc.sfx freearc-mini.sfx freearc-tiny.sfx

freearc-installer.sfx: $(OBJDIR)/FreeArcInstaller.o $(LINKOBJ) | rustlib
	$(WINDRES) -Igui gui\main.rc gui\res.o
	$(GCC) $< $(LINKOBJ)      $(RUSTLIB) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-installer.sfx
	upx.exe -9 --lzma --compress-icons=2 freearc-installer.sfx

FreeArc.fmt: $(OBJDIR)/FarPlugin.o FarPlugin.def $(LINKOBJ_FAR_PLUGIN) | rustlib
	$(DLLWRAP) --driver-name g++ $< $(LINKOBJ_FAR_PLUGIN) $(RUSTLIB) -def FarPlugin.def -s -o FreeArc.fmt


unarc: $(OBJDIR)/unarc.o $(LINKOBJ) | rustlib
	$(GCC) $< $(LINKOBJ) $(RUSTLIB) $(UNIX_LINK_FLAGS) -o unarc

arc.linux.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ) | rustlib
	$(GCC) $< $(LINKOBJ)      $(RUSTLIB) $(UNIX_LINK_FLAGS) -o arc.linux.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(RUSTLIB) $(UNIX_LINK_FLAGS) -o arc-mini.linux.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(RUSTLIB) $(UNIX_LINK_FLAGS) -o arc-tiny.linux.sfx
#	upx -9 --lzma arc*.linux.sfx


$(OBJDIR)/FarPlugin.o: FarPlugin.cpp ArcStructure.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -IHeaders.c -o $*.o $<

$(OBJDIR)/unarc.o: unarc.cpp ArcStructure.h CUI.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/ArcSFX.o: unarc.cpp ArcStructure.h CUI.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -o $*.o $<

$(OBJDIR)/FreeArcSFX.o: unarc.cpp ArcStructure.h gui\gui.h gui\gui.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_GUI -Igui -o $*.o $<

$(OBJDIR)/FreeArcInstaller.o: unarc.cpp ArcStructure.h gui\gui.h gui\gui.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_INSTALLER -DFREEARC_GUI -Igui -o $*.o $<


$(OBJDIR)/Environment.o: ../Environment.cpp ../Environment.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/CompressionLibrary.o: ../Compression/CompressionLibrary.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/Common.o: ../Compression/Common.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<


$(OBJDIR)/C_BCJ.o: ../Compression/LZMA/C_BCJ.cpp ../Compression/LZMA/C_BCJ.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA.o: ../Compression/LZMA/C_LZMA.cpp ../Compression/LZMA/C_LZMA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Dict.o: ../Compression/Dict/C_Dict.cpp ../Compression/Dict/C_Dict.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Delta.o: ../Compression/Delta/C_Delta.cpp ../Compression/Delta/C_Delta.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_REP.o: ../Compression/REP/C_REP.cpp ../Compression/REP/C_REP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZP.o: ../Compression/LZP/C_LZP.cpp ../Compression/LZP/C_LZP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_PPMD.o: ../Compression/PPMD/C_PPMD.cpp ../Compression/PPMD/C_PPMD.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -O1 -o $*.o $<

$(OBJDIR)/C_MM.o: ../Compression/MM/C_MM.cpp ../Compression/MM/C_MM.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_TTA.o: ../Compression/MM/C_TTA.cpp ../Compression/MM/C_TTA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Tornado.o: ../Compression/Tornado/C_Tornado.cpp ../Compression/Tornado/C_Tornado.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_External.o: ../Compression/External/C_External.cpp ../Compression/External/C_External.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_GRZip.o: ../Compression/GRZip/C_GRZip.cpp ../Compression/GRZip/C_GRZip.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_DisPack.o: ../Compression/DisPack/C_DisPack.cpp ../Compression/DisPack/C_DisPack.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_4x4.o: ../Compression/4x4/C_4x4.cpp ../Compression/4x4/C_4x4.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_BSC.o: ../Compression/BSC/C_BSC.cpp ../Compression/BSC/C_BSC.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZ4.o: ../Compression/LZ4/C_LZ4.cpp ../Compression/LZ4/C_LZ4.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Zstd.o: ../Compression/Zstd/C_Zstd.cpp ../Compression/Zstd/C_Zstd.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA2.o: ../Compression/LZMA/C_LZMA2.cpp ../Compression/LZMA/C_LZMA2.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

# No -Iheaders any more: that existed because C_Encryption.cpp used to reach
# LibTomCrypt's headers by bare name. Under DARC_RUST it routes to darc-crypto and
# includes only C_Encryption.h, so the vendored headers were deleted from the
# working tree; the difftest oracle builds its reference from the pinned revision.
$(OBJDIR)/C_Encryption.o: ../Compression/_Encryption/C_Encryption.cpp ../Compression/_Encryption/C_Encryption.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

