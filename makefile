include common.mak

## Default target: build with MicroHs architecture
.PHONY: all
all: microhs

ALL: $(TEMPDIR)/Environment.o $(TEMPDIR)/GuiEnvironment.o $(TEMPDIR)/URL.o

# -Wno-unknown-pragmas is kept: the vendored codecs carry MSVC pragmas.
# -Wno-sign-compare and -Wno-conversion were dropped deliberately; they were
# hiding exactly the integer-width and signedness bugs worth knowing about.
CODE_FLAGS = -fno-exceptions -fno-rtti -Wall -Wextra \
                -Wno-unknown-pragmas
# -fno-strict-aliasing is REQUIRED, not a preference. The codecs read and write
# buffers through value32()/setvalue32()-style punning macros (4x4, DisPack,
# mmdet, Tornado/EntropyCoder, ...). Under -fstrict-aliasing at -O3 the compiler
# is entitled to assume those accesses never alias and reorder them, which is a
# miscompile waiting to happen rather than a theoretical concern.
# -ffast-math was removed: there is no float hot path worth it in an archiver,
# and it licenses transforms that break IEEE semantics.
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fno-strict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES) $(EXTRA_CFLAGS)

$(TEMPDIR)/Environment.o:  Environment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/GuiEnvironment.o:  GuiEnvironment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/URL.o:  URL.cpp URL.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

## MicroHs architecture targets (Linux/Unix)
.PHONY: microhs microhs-gui
microhs:
	./compile

microhs-gui:
	./compile -DFREEARC_GUI

clean:
	rm -rf $(TEMPDIR)-unarc/*.o
	rm -rf $(TEMPDIR)/*.o
	rm -rf $(TEMPDIR)/*.hi
	rm -rf $(TEMPDIR)-O2/*.o
	rm -rf $(TEMPDIR)-O2/*.hi
	rm -rf $(TEMPDIR)-DFREEARC_GUI/*.o
	rm -rf $(TEMPDIR)-DFREEARC_GUI/*.hi
	rm -rf $(TEMPDIR)-O2-DFREEARC_GUI/*.o
	rm -rf $(TEMPDIR)-O2-DFREEARC_GUI/*.hi

winclean:
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-unarc\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-DFREEARC_GUI\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-DFREEARC_GUI\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2-DFREEARC_GUI\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2-DFREEARC_GUI\*.hi
