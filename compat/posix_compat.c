/* Minimal POSIX stat + Handle seek compatibility helpers for MicroHs */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <termios.h>

/* stat helpers */
int mhs_sizeof_stat(void) { return (int)sizeof(struct stat); }
int mhs_stat(const char *path, struct stat *buf) { return stat(path, buf); }
unsigned int mhs_st_mode(struct stat *buf) { return (unsigned int)buf->st_mode; }
long long    mhs_st_size(struct stat *buf) { return (long long)buf->st_size; }
long long    mhs_st_mtime(struct stat *buf) { return (long long)buf->st_mtime; }
int          mhs_s_isdir(unsigned int m)  { return S_ISDIR(m); }
int          mhs_s_isreg(unsigned int m)  { return S_ISREG(m); }
int          mhs_chmod(const char *path, unsigned int mode) { return chmod(path, (mode_t)mode); }

/* GHC-compat __hscore_* aliases: the ghc-compat package's System.Posix.Internals
 * uses these GHC-specific names; we provide them here so linking succeeds. */
int          __hscore_sizeof_stat(void)              { return (int)sizeof(struct stat); }
int          __hscore_stat(const char *p, struct stat *b) { return stat(p, b); }
unsigned int __hscore_st_mode(struct stat *b)        { return (unsigned int)b->st_mode; }
long         __hscore_st_size(struct stat *b)        { return (long)b->st_size; }
long         __hscore_st_mtime(struct stat *b)       { return (long)b->st_mtime; }

/* MicroHs BFILE layout - must match bfile.c */
typedef struct MHS_BFILE {
  int    (*getb)(struct MHS_BFILE*);
  void   (*ungetb)(int, struct MHS_BFILE*);
  void   (*putb)(int, struct MHS_BFILE*);
  void   (*flushb)(struct MHS_BFILE*);
  void   (*closeb)(struct MHS_BFILE*);
  size_t (*readb)(uint8_t *, size_t, struct MHS_BFILE*);
  size_t (*writeb)(const uint8_t *, size_t, struct MHS_BFILE*);
} MHS_BFILE;

typedef struct { MHS_BFILE mets; FILE *file; } MHS_BFILE_file;

static FILE* bfile_to_file(MHS_BFILE *bp) {
  return ((MHS_BFILE_file*)bp)->file;
}

/* hTell: return current file position */
long long mhs_hTell(MHS_BFILE *bp) {
  FILE *f = bfile_to_file(bp);
  if (!f) return -1;
  fflush(f);
  return (long long)ftello(f);
}

/* hFileSize: return file size */
long long mhs_hFileSize(MHS_BFILE *bp) {
  FILE *f = bfile_to_file(bp);
  if (!f) return -1;
  fflush(f);
  struct stat st;
  if (fstat(fileno(f), &st) != 0) return -1;
  return (long long)st.st_size;
}

/* hSeek: seek to absolute offset */
int mhs_hSeek(MHS_BFILE *bp, long long offset) {
  FILE *f = bfile_to_file(bp);
  if (!f) return -1;
  fflush(f);
  return (int)fseeko(f, (off_t)offset, SEEK_SET);
}

/* Direct buffer read/write via BFILE methods — bypasses the broken hGetBuf FFI path
 * under MicroHs where the IO monad CPS encoding scrambles the readb argument order.
 * Called from Files.hs fReadBufSimple / fWriteBufSimple under __MHS__. */
size_t mhs_file_readbuf(MHS_BFILE *bp, void *buf, int size) {
  if (!bp || size <= 0) return 0;
  if (bp->readb)
    return bp->readb((uint8_t *)buf, (size_t)size, bp);
  /* fall back to byte-by-byte if no bulk read method */
  size_t n = 0;
  unsigned char *p = (unsigned char *)buf;
  while (n < (size_t)size) {
    int c = bp->getb(bp);
    if (c < 0) break;
    p[n++] = (unsigned char)c;
  }
  return n;
}

size_t mhs_file_writebuf(MHS_BFILE *bp, const void *buf, int size) {
  if (!bp || size <= 0) return 0;
  if (bp->writeb)
    return bp->writeb((const uint8_t *)buf, (size_t)size, bp);
  size_t n = 0;
  const unsigned char *p = (const unsigned char *)buf;
  while (n < (size_t)size) {
    bp->putb((int)p[n], bp);
    n++;
  }
  return n;
}

/* Terminal attribute helpers for System.Posix.Terminal shim */
int mhs_tcgetattr(int fd, struct termios *t) { return tcgetattr(fd, t); }
int mhs_tcsetattr(int fd, int action, struct termios *t) { return tcsetattr(fd, action, t); }

/* Stub compression callback for MicroHs (replaces foreign import ccall "wrapper").
 * Returns FREEARC_ERRCODE_NOT_IMPLEMENTED (-8) for any call.
 * Used when MicroHs cannot create Haskell-to-C callback trampolines. */
int mhs_stub_callback(const char *what, char *buf, int size, void *auxdata) {
  (void)what; (void)buf; (void)size; (void)auxdata;
  return -8; /* FREEARC_ERRCODE_NOT_IMPLEMENTED */
}

/* Stub Lua callbacks for MicroHs (replace foreign import ccall "wrapper").
 * Used when MicroHs cannot create Haskell-to-C callback trampolines. */
/* LuaWriter stub: signals no bytes written (returns non-zero = error) */
int mhs_stub_lua_writer(void *L, const char *p, size_t sz, void *ud) {
  (void)L; (void)p; (void)sz; (void)ud;
  return 1; /* non-zero = error */
}
/* LuaReader stub: signals end of input */
const char *mhs_stub_lua_reader(void *L, void *ud, size_t *sz) {
  (void)L; (void)ud;
  if (sz) *sz = 0;
  return (const char *)0;
}
/* LuaCFunction stub: returns 0 results */
int mhs_stub_lua_cfunction(void *L) {
  (void)L;
  return 0;
}
