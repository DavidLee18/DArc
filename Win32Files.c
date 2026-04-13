/* External (non-inline) wrappers for helpers used by Win32Files.hs.
 *
 * The Haskell FFI imports expect real external symbols. Two groups of
 * wrappers are needed:
 *
 *   1. __w_* helpers declared as INLINE in Win32Files.h (`static inline`
 *      in HsBase.h → no external symbol).
 *   2. __hscore_seek_* accessors from HsBase.h, same reason.
 *   3. Thin wrappers over the MS CRT *_i64 functions so the Haskell
 *      imports see plain symbols regardless of whether the underlying
 *      CRT (msvcrt vs ucrt) exports them with a leading underscore etc.
 */
#include <HsBase.h>
#include <io.h>
#include <wchar.h>
#include <sys/stat.h>
#include <stdio.h>
#include <setjmp.h>

/* Group 1: __w_* helpers */
HsInt    __w_find_sizeof       ( void )                          { return sizeof(struct _wfinddatai64_t); }
unsigned __w_find_attrib       ( struct _wfinddatai64_t* st )    { return st->attrib;      }
time_t   __w_find_time_create  ( struct _wfinddatai64_t* st )    { return st->time_create; }
time_t   __w_find_time_access  ( struct _wfinddatai64_t* st )    { return st->time_access; }
time_t   __w_find_time_write   ( struct _wfinddatai64_t* st )    { return st->time_write;  }
__int64  __w_find_size         ( struct _wfinddatai64_t* st )    { return st->size;        }
wchar_t* __w_find_name         ( struct _wfinddatai64_t* st )    { return st->name;        }

HsInt          __w_stat_sizeof ( void )                          { return sizeof(struct _stati64); }
unsigned short __w_stat_mode   ( struct _stati64* st )           { return st->st_mode;  }
time_t         __w_stat_ctime  ( struct _stati64* st )           { return st->st_ctime; }
time_t         __w_stat_atime  ( struct _stati64* st )           { return st->st_atime; }
time_t         __w_stat_mtime  ( struct _stati64* st )           { return st->st_mtime; }
__int64        __w_stat_size   ( struct _stati64* st )           { return st->st_size;  }

/* Group 2: SEEK_* accessors (static inline in HsBase.h means no external symbol) */
int __hscore_seek_cur(void) { return SEEK_CUR; }
int __hscore_seek_set(void) { return SEEK_SET; }
int __hscore_seek_end(void) { return SEEK_END; }

/* Group 3: UCRT-only aliases under the legacy names Win32Files.hs imports.
 *
 * The msvcrt-targeting MinGW (GHC 8.6 / DArc86) already exports the legacy
 * symbols _fstati64 / _wstati64 / _wfindfirsti64 / _wfindnexti64 directly,
 * so no wrappers are needed there. UCRT (GHC 9.4+) renamed them, so we
 * provide shims that forward to the UCRT names. */
#ifdef _UCRT

#undef _fstati64
#undef _wstati64
#undef _wfindfirsti64
#undef _wfindnexti64

/* UCRT names: _stati64 (32-bit time_t, 64-bit size) matches _stat32i64.
 * Do NOT forward to _wstat64 — that uses 64-bit time_t and a different
 * struct layout, which causes wstat to report success with garbage
 * data (e.g. nonexistent files appearing to exist). */
/* Return HsInt (int64 on Win64): Haskell FFI declares these as `IO Int`,
 * but the underlying CRT functions return `int` (32-bit). Without sign
 * extension, -1 becomes 0x00000000FFFFFFFF instead of -1, defeating
 * throwErrnoIfMinus1 and making nonexistent files appear to exist. */
HsInt _fstati64(int fd, struct _stati64* st) {
  return (HsInt)_fstat32i64(fd, (struct _stat32i64*)st);
}
HsInt _wstati64(const wchar_t* path, struct _stati64* st) {
  return (HsInt)_wstat32i64(path, (struct _stat32i64*)st);
}
/* Wine's ucrtbase.dll stubs _wfindfirst32i64/_wfindnext32i64 (real code
 * is at the tiny 0x2260 offset — they abort at runtime with "unimplemented").
 * Use _wfindfirst64/_wfindnext64 instead and convert 64-bit time_t fields
 * to 32-bit to match the _wfinddatai64_t struct layout Haskell expects. */
static void copy_find64_to_i64(struct _wfinddatai64_t* fd, const struct _wfinddata64_t* d64) {
  fd->attrib       = d64->attrib;
  fd->time_create  = (time_t)d64->time_create;
  fd->time_access  = (time_t)d64->time_access;
  fd->time_write   = (time_t)d64->time_write;
  fd->size         = d64->size;
  memcpy(fd->name, d64->name, sizeof(fd->name));
}
intptr_t _wfindfirsti64(const wchar_t* pat, struct _wfinddatai64_t* fd) {
  struct _wfinddata64_t d64;
  intptr_t h = _wfindfirst64(pat, &d64);
  if (h != (intptr_t)-1) copy_find64_to_i64(fd, &d64);
  return h;
}
HsInt _wfindnexti64(intptr_t h, struct _wfinddatai64_t* fd) {
  struct _wfinddata64_t d64;
  int r = _wfindnext64(h, &d64);
  if (r == 0) copy_find64_to_i64(fd, &d64);
  return (HsInt)r;
}

#endif /* _UCRT */
