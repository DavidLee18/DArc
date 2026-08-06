SFX modules and the standalone extractor for DArc

Linux
-----
arc.linux.sfx  - the only module. It is a copy of the Rust extractor
                 (rust/darc-unarc), which contains every codec, and it is the
                 same binary shipped as `unarc`.

There were three tiers here -- arc.sfx (everything), arc-mini.sfx (without
mm/tta/grzip/tornado, for archives made with options like -m5 -mm-) and
arc-tiny.sfx (also without rep/lzp/ppmd, for -m5x -mm-). Each linked a
different subset of the C decoders, so the smaller ones really were smaller.
One Rust binary carries all of them, so the three could only be identical
copies under three names, claiming a size saving that no longer exists.
`make oracle` still builds the tiered C++ ones as arc-{,mini-,tiny-}c.linux.sfx
if the comparison is ever wanted; they are not shipped.

Windows
-------
Not ported yet, and still built from unarc.cpp by `make windows`:

arc.sfx                - console
freearc.sfx            - GUI
freearc-installer.sfx  - GUI, extracts to a tempdir and runs setup.exe
FreeArc.fmt            - FAR Manager MultiArc plugin (FarPlugin.cpp)

These are why unarc.cpp, ArcStructure.h and CUI.h are still in the tree. Note
that ArcStructure.h is a SECOND implementation of the archive reader and has a
known defect: it reads the per-file time field as 4 bytes where the writer
stores a fixed 64-bit CTime, so everything after it -- directory flags, CRCs --
comes out of the wrong offset. The Rust extractor owns no format knowledge at
all; it calls the same reader the archiver uses.
