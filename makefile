include common.mak

## Default target: build with GHC (full-featured binary with parallel RTS)
.PHONY: all
all: ghc

ALL: $(TEMPDIR)/Environment.o $(TEMPDIR)/GuiEnvironment.o $(TEMPDIR)/URL.o

CODE_FLAGS = -fno-exceptions -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES) $(EXTRA_CFLAGS)

$(TEMPDIR)/Environment.o:  Environment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/GuiEnvironment.o:  GuiEnvironment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/URL.o:  URL.cpp URL.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

## GHC-compiled targets (Linux/Unix): use GHC threaded runtime with -N (all cores)
.PHONY: ghc ghc-gui
ghc:
	./compile-ghc

ghc-gui:
	./compile-ghc -DFREEARC_GUI

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
