DEFINES  = -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DWIN32 -D_WINDOWS -D_UNICODE -DUNICODE
TEMPDIR  = c:/temp/out/FreeArc
GHCDIR   = C:\Base\Compiler\ghc
LIBDIR   = $(GHCDIR)\gcc-lib
INCDIR   = $(GHCDIR)\include\mingw
GCC      = $(GHCDIR)\gcc.exe -B$(LIBDIR) -I$(INCDIR) -std=c++17
DLLWRAP  = $(GHCDIR)\gcc-lib\dllwrap.exe -B$(LIBDIR)
WINDRES  = $(GHCDIR)\windres.exe
