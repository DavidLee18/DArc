/* The C PPMd var.H codec for the differential harness.
 *
 * C_PPMD.cpp includes Model.cpp, which includes SubAlloc.hpp and Coder.hpp, so
 * this is a single translation unit like the real build.
 *
 * Unlike every other codec harness here, this one CANNOT cut below the whole
 * stream. PPMd's output depends on its memory suballocator:
 *
 *   Model.cpp:245  GetUsedMemory() < (SubAllocatorSize >> 1)  decides restart
 *   Model.cpp:416  if (pText >= UnitsStart) goto RESTART_MODEL
 *   Model.cpp:418  if ((BYTE*) FSuccessor < UnitsStart)
 *
 * so the model's behaviour is a function of allocator LAYOUT, not just of the
 * input. There is no stage boundary at which a partial port could be compared:
 * the coder, the model and the allocator are one system. The stream is the
 * only honest cut.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

// C_PPMD.cpp registers itself as a COMPRESSION_METHOD at static-init time.
// Linking the real AddCompressionMethod would drag in CompressionLibrary.cpp
// and every other codec, so it is stubbed, as in the other harnesses here.
int AddCompressionMethod (CM_PARSER) { return 0; }
int COMPRESSION_METHOD::doit (char *, int, void *, CALLBACK_FUNC *) { return -1; }

// The rest of the archiver plumbing C_PPMD.cpp references. None of it is on the
// codec path: parse_PPMD/ShowCompressionMethod are the method-string parser and
// its inverse, and LoadFromDLL is the Windows external-DLL hook. The harness
// calls ppmd_compress/ppmd_decompress directly with explicit parameters.
MemSize parseInt (char *, int *)      { return 0; }
MemSize parseMem (char *, int *)      { return 0; }
void    showMem  (MemSize, char *r)   { if (r) r[0] = 0; }
FARPROC LoadFromDLL (char *)          { return 0; }

// The Rust crate will export ppmd_compress/ppmd_decompress unconditionally once
// the port lands, and GNU ld rejects a duplicate C-linkage definition (macOS ld
// silently keeps one -- the failure mode that passed locally and broke CI for
// GRZip). Rename the reference's copies: this harness is the only place both
// implementations exist at once.
#define ppmd_compress   ppmd_compress_PINNED_REFERENCE
#define ppmd_decompress ppmd_decompress_PINNED_REFERENCE
#include "../../Compression/PPMD/C_PPMD.cpp"
#undef ppmd_compress
#undef ppmd_decompress

/* Allocator-level entry points.
 *
 * The header comment says the STREAM is the only honest cut, and that is true
 * of comparing compressed output -- the coder, model and allocator have no seam
 * between them. But the allocator on its own does have a testable interface:
 * drive both implementations through the same operation sequence and compare
 * the offsets they hand back plus GetUsedMemory(). That is a real oracle for
 * the part this codec is most sensitive to, and it does not have to wait for
 * the model to be ported.
 *
 * Offsets rather than pointers, so the two sides are comparable at all: the
 * Rust heap is a Vec<u8> and the C's is a malloc'ed block at an arbitrary
 * address. Everything is reported relative to HeapStart, which is exactly what
 * the C's own BLKREF/CTX_REF already do.
 *
 * C_PPMD.cpp includes Model.cpp TWICE, inside `namespace PPMD_compression` and
 * again inside `namespace PPMD_decompression`, so the allocator exists twice
 * with completely independent state -- the encoder and the decoder never share
 * a heap. These wrappers drive the COMPRESSION side; the two are the same code,
 * and the port keeps them as two instances for the same reason.
 */
using namespace PPMD_compression;
extern "C" {
long darc_ppmd_sa_start (unsigned t)  { return StartSubAllocator(t) ? 1 : 0; }
void darc_ppmd_sa_init  (void)        { InitSubAllocator(); }
void darc_ppmd_sa_stop  (void)        { StopSubAllocator(); }
unsigned darc_ppmd_sa_used (void)     { return (unsigned) GetUsedMemory(); }

long darc_ppmd_sa_alloc_units (unsigned nu)
{ void *p = AllocUnits(nu); return p ? (long)((BYTE*)p - HeapStart) : -1; }

long darc_ppmd_sa_alloc_context (void)
{ void *p = AllocContext(); return p ? (long)((BYTE*)p - HeapStart) : -1; }

void darc_ppmd_sa_free_units (long off, unsigned nu)
{ FreeUnits(HeapStart + off, nu); }

long darc_ppmd_sa_expand_units (long off, unsigned nu)
{ void *p = ExpandUnits(HeapStart + off, nu); return p ? (long)((BYTE*)p - HeapStart) : -1; }

long darc_ppmd_sa_shrink_units (long off, unsigned nu, unsigned newnu)
{ void *p = ShrinkUnits(HeapStart + off, nu, newnu); return p ? (long)((BYTE*)p - HeapStart) : -1; }

void darc_ppmd_sa_special_free (long off)
{ SpecialFreeUnit(HeapStart + off); }

/* The four layout cursors the model branches on. */
long darc_ppmd_sa_ptext       (void) { return (long)(pText      - HeapStart); }
long darc_ppmd_sa_units_start (void) { return (long)(UnitsStart - HeapStart); }
long darc_ppmd_sa_lo_unit     (void) { return (long)(LoUnit     - HeapStart); }
long darc_ppmd_sa_hi_unit     (void) { return (long)(HiUnit     - HeapStart); }
}

extern "C" {
int darc_ppmd_stream_compress (int order, MemSize mem, int mrmethod,
                               CALLBACK_FUNC *cb, void *aux)
{ return ppmd_compress_PINNED_REFERENCE (order, mem, mrmethod, cb, aux); }

int darc_ppmd_stream_decompress (int order, MemSize mem, int mrmethod,
                                 CALLBACK_FUNC *cb, void *aux)
{ return ppmd_decompress_PINNED_REFERENCE (order, mem, mrmethod, cb, aux); }
}
