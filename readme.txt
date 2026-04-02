It should be easy to compile DArc:

== ON WINDOWS ===========================================================================

Prerequisites:
1. Install MSYS2 (https://www.msys2.org/) with the UCRT64 environment
   - Add MSYS2 binaries to your PATH (sh.exe, clang, make, curl, tar must be available)
2. Install MicroHs (https://github.com/augustss/MicroHs)
   - Add %USERPROFILE%\.mcabal\bin to your PATH
   - MicroHs is the Haskell compiler used; no GHC installation is needed

Building:
1. For compiling console version (Arc.exe):
     compile-O2.cmd
2. For compiling GUI version (FreeArc.exe):
     compile-GUI-O2.cmd
3. When compilation finishes, you will find Arc.exe/FreeArc.exe in Tests subdirectory
4. For compiling SFX modules and Unarc (optional):
     cd Unarc
     make windows


== ON UNIX (Linux/macOS) ================================================================

Prerequisites:
1. Install MicroHs (https://github.com/augustss/MicroHs)
   - MicroHs is the Haskell compiler used; no GHC installation is needed
2. Install clang, make, and required development libraries:
   - Required: liblua5.1-dev, libncurses-dev (or ncurses on macOS via Homebrew)
   - Optional: libcurl-dev (or curl on macOS) for URL/network archive support

Building:
1. Make compile scripts executable (if needed):
     chmod +x compile*
2. For compiling console version (arc):
     ./compile-O2
3. For compiling GUI version (freearc):
     ./compile-GUI-O2
4. When compilation finishes, you will find arc/freearc executables in Tests subdirectory
5. For compiling SFX modules and Unarc (optional):
     cd Unarc
     make linux

See README.md for detailed instructions and troubleshooting.


