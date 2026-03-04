/* Minimal POSIX stat + Handle seek compatibility helpers for MicroHs */
#include <sys/types.h>
#include <sys/stat.h>
#include <utime.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <termios.h>
#include <time.h>
#include <limits.h>

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

/* SetFileDateTime: sets mtime of `path` to `mtime` (Unix time), preserving atime */
int mhs_SetFileDateTime(const char *path, long long mtime) {
  struct stat st;
  struct utimbuf times;
  if (stat(path, &st) != 0) return -1;
  times.actime  = st.st_atime;
  times.modtime = (time_t)mtime;
  return utime(path, &times);
}

/* RunFile: execute `filename` in directory `curdir`; if wait_finish!=0, wait for exit.
 * Note: mirrors the original Environment.cpp RunFile which also uses system(). */
#include <stdlib.h>
#include <string.h>
int mhs_RunFile(const char *filename, const char *curdir, int wait_finish) {
  size_t fname_len = strlen(filename);
  size_t suffix_len = wait_finish ? 0 : 2; /* " &" */
  size_t cmd_len = 2 + fname_len + suffix_len + 1; /* "./" + name + suffix + NUL */
  char *cmd = (char*)malloc(cmd_len);
  if (!cmd) return -1;
  char *olddir = (char*)malloc(4096);
  if (!olddir) { free(cmd); return -1; }
  if (!getcwd(olddir, 4096)) { olddir[0] = '.'; olddir[1] = '\0'; }
  chdir(curdir);
  snprintf(cmd, cmd_len, "./%s%s", filename, wait_finish ? "" : " &");
  int r = system(cmd);
  chdir(olddir);
  free(cmd);
  free(olddir);
  return r;
}

/* GetExeName: fill buf with path to current executable (reads /proc/self/exe).
 * Returns 0 on success, -1 on error. */
int mhs_GetExeName(char *buf, int bufsize) {
  ssize_t n = readlink("/proc/self/exe", buf, (size_t)(bufsize - 1));
  if (n < 0) return -1;
  buf[n] = '\0';
  return 0;
}

/* System information stubs using sysinfo(2) */
#include <sys/sysinfo.h>
unsigned int mhs_GetPhysicalMemory(void) {
  struct sysinfo si;
  if (sysinfo(&si) != 0) return 0;
  return (unsigned int)(si.totalram * (unsigned long)si.mem_unit);
}

unsigned int mhs_GetAvailablePhysicalMemory(void) {
  struct sysinfo si;
  if (sysinfo(&si) != 0) return 0;
  return (unsigned int)(si.freeram * (unsigned long)si.mem_unit);
}

unsigned int mhs_GetMaxMemToAlloc(void) {
  return UINT_MAX;
}

int mhs_GetProcessorsCount(void) {
  return get_nprocs();
}

/* Console title (Unix): ANSI escape sequence to set terminal title */
void mhs_SetConsoleTitle(const char *title) {
  fprintf(stderr, "\033]0;%s\a", title);
}

void mhs_ResetConsoleTitle(void) {
  /* no-op: no saved title to restore */
}

/* Read random bytes from /dev/urandom; returns number of bytes read */
int mhs_read_urandom(void *buf, int size) {
  FILE *f = fopen("/dev/urandom", "rb");
  if (!f) return 0;
  int n = (int)fread(buf, 1, (size_t)size, f);
  fclose(f);
  return n;
}

/* Thread ID: Linux-specific gettid() syscall (returns lightweight thread ID,
 * different from pthread_self() which returns the pthread handle).
 * Not portable beyond Linux; returns 0 on other systems. */
#include <sys/syscall.h>
int32_t mhs_gettid(void) {
  return (int32_t)syscall(SYS_gettid);
}

/* URL stubs (FREEARC_NOURL equivalent for MicroHs) --------------------------------- */
void  mhs_url_setup_proxy       (const char *proxy)   { (void)proxy; }
void  mhs_url_setup_bypass_list (const char *bypass)  { (void)bypass; }
void *mhs_url_open              (const char *url)     { (void)url; return (void*)0; }
int64_t mhs_url_pos  (void *url) { (void)url; return 0; }
int64_t mhs_url_size (void *url) { (void)url; return 0; }
void    mhs_url_seek (void *url, int64_t pos) { (void)url; (void)pos; }
int     mhs_url_read (void *url, char *buf, int size) { (void)url; (void)buf; (void)size; return -1; }
void    mhs_url_close(void *url) { (void)url; }

/* MM detection stubs for MicroHs: always report "not MM data" ------------------- */
void mhs_detect_datatype(const char *buf, int bufsize, char *type) {
  /* Return empty string: type unknown / not detected */
  if (type) type[0] = '\0';
  (void)buf; (void)bufsize;
}

int mhs_detect_mm_bytes(int mode, int filesize) {
  /* 0 bytes to check — skip MM detection entirely */
  (void)mode; (void)filesize;
  return 0;
}

int mhs_detect_mm(int mode, const char *buf, int bufsize) {
  /* Not MM data */
  (void)mode; (void)buf; (void)bufsize;
  return 0;
}

int mhs_detect_mm_header(int mode, const char *buf, int bufsize) {
  /* No MM header */
  (void)mode; (void)buf; (void)bufsize;
  return 0;
}

/* =========================================================================
 * CompressionLib stubs for MicroHs
 * These replace the C++ functions from Compression/CompressionLibrary.cpp
 * when building without the full compression library.
 * All compression/decompression operations return FREEARC_ERRCODE_NOT_IMPLEMENTED.
 * ========================================================================= */
#define FREEARC_OK                       0
#define FREEARC_ERRCODE_NOT_IMPLEMENTED (-8)
#define FREEARC_ERRCODE_INVALID_COMPRESSOR (-2)

/* Global flag: whether to use fastest buffer-to-buffer compression */
int compress_all_at_once = 0;

/* Thread count */
static int mhs_CompressionThreads = 1;
int  mhs_GetCompressionThreads(void)          { return mhs_CompressionThreads; }
void mhs_SetCompressionThreads(int threads)   { mhs_CompressionThreads = (threads <= 0) ? 1 : threads; }

/* External compressors table */
void mhs_ClearExternalCompressorsTable(void)  { /* no-op */ }
int  mhs_AddExternalCompressor(const char *d) { (void)d; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }

/* aMAX_METHOD_STRLEN from CompressionLib.hs — keep in sync */
#define MHS_MAX_METHOD_STRLEN 2048

/* Canonize: just copy the method string as-is */
int mhs_CanonizeCompressionMethod(const char *method, char *canonical) {
  if (!method || !canonical) return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  size_t n = 0;
  /* Leave one byte for the NUL terminator */
  while (method[n] && n < MHS_MAX_METHOD_STRLEN - 1) { canonical[n] = method[n]; n++; }
  canonical[n] = '\0';
  return FREEARC_OK;
}

/* CompressionService: return NOT_IMPLEMENTED for everything */
int mhs_CompressionService(const char *method, const char *what,
                            int param, void *data, void *callback) {
  (void)method; (void)what; (void)param; (void)data; (void)callback;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

/* Cleanup: no-op */
void mhs_compressionLib_cleanup(void) { /* no-op */ }

/* Memory/dict/block size getters: return 0 (unknown) */
unsigned int mhs_GetCompressionMem  (const char *m) { (void)m; return 0; }
unsigned int mhs_GetDecompressionMem(const char *m) { (void)m; return 0; }
unsigned int mhs_GetDictionary      (const char *m) { (void)m; return 0; }
unsigned int mhs_GetBlockSize       (const char *m) { (void)m; return 0; }

/* Setters/Limiters: copy method unchanged, return OK */
int mhs_SetCompressionMem  (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_SetDecompressionMem(const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_SetDictionary      (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_SetBlockSize       (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_LimitCompressionMem  (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_LimitDecompressionMem(const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_LimitDictionary      (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}
int mhs_LimitBlockSize       (const char *in, unsigned int bytes, char *out) {
  (void)bytes; return mhs_CanonizeCompressionMethod(in, out);
}

/* Compress/Decompress: return NOT_IMPLEMENTED */
int mhs_Compress            (const char *m, void *rc, void *wc) { (void)m;(void)rc;(void)wc; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_Decompress          (const char *m, void *rc, void *wc) { (void)m;(void)rc;(void)wc; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_CompressWithHeader  (const char *m, void *rc, void *wc) { (void)m;(void)rc;(void)wc; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_DecompressWithHeader(         void *rc, void *wc)       {          (void)rc;(void)wc; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_CompressMem            (const char *m, void *i, int is, void *o, int os) { (void)m;(void)i;(void)is;(void)o;(void)os; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_DecompressMem          (const char *m, void *i, int is, void *o, int os) { (void)m;(void)i;(void)is;(void)o;(void)os; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_CompressMemWithHeader  (const char *m, void *i, int is, void *o, int os) { (void)m;(void)i;(void)is;(void)o;(void)os; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
int mhs_DecompressMemWithHeader(              void *i, int is, void *o, int os)  {          (void)i;(void)is;(void)o;(void)os; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }

/* =========================================================================
 * EncryptionLib stubs for MicroHs
 * Fortuna PRNG: redirect prng_read to /dev/urandom so random data still works.
 * Pbkdf2Hmac: stub that returns empty key (encryption will fail gracefully).
 * ========================================================================= */
#include <string.h>

/* Fortuna PRNG state size: 1 byte (minimal, we use /dev/urandom directly) */
int mhs_fortuna_size = 1;

int  mhs_fortuna_start      (void *prng)                            { (void)prng; return FREEARC_OK; }
int  mhs_fortuna_add_entropy(const void *in, unsigned long inlen, void *prng) {
  (void)in; (void)inlen; (void)prng; return FREEARC_OK;
}
int  mhs_fortuna_ready      (void *prng)                            { (void)prng; return FREEARC_OK; }
unsigned long mhs_fortuna_read(void *out, unsigned long outlen, void *prng) {
  /* Redirect to /dev/urandom for actual randomness.
   * Opening /dev/urandom each call is intentional: this is a stub for MicroHs
   * where the Fortuna PRNG C library is not linked; using a persistent fd would
   * add state that complicates fork/exec scenarios in this minimal build. */
  (void)prng;
  FILE *f = fopen("/dev/urandom", "rb");
  if (!f) { memset(out, 0, outlen); return 0; }
  unsigned long n = (unsigned long)fread(out, 1, outlen, f);
  fclose(f);
  return n;
}

/* PBKDF2-HMAC stub: fills key with zeros (encryption disabled under MicroHs) */
void mhs_Pbkdf2Hmac(const char *password, int pwdlen,
                    const char *salt,     int saltlen,
                    int iterations,
                    char *key,            int keylen) {
  (void)password; (void)pwdlen; (void)salt; (void)saltlen; (void)iterations;
  memset(key, 0, (size_t)keylen);
}
