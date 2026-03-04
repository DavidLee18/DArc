/* Declarations for MicroHs FFI callback stubs.
 * These are implemented in compat/posix_compat.c and used when
 * MicroHs cannot create Haskell-to-C callback trampolines
 * (foreign import ccall "wrapper" is unsupported in MicroHs). */
#ifndef MHS_STUBS_H
#define MHS_STUBS_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Compression callback stub: always returns FREEARC_ERRCODE_NOT_IMPLEMENTED (-8) */
int mhs_stub_callback(const char *what, char *buf, int size, void *auxdata);

/* Lua callback stubs */
int         mhs_stub_lua_writer    (void *L, const char *p, size_t sz, void *ud);
const char *mhs_stub_lua_reader    (void *L, void *ud, size_t *sz);
int         mhs_stub_lua_cfunction (void *L);

/* Direct BFILE buffer read/write — bypasses broken hGetBuf FFI under MicroHs */
size_t mhs_file_readbuf  (void *bfile, void *buf, int size);
size_t mhs_file_writebuf (void *bfile, const void *buf, int size);

/* File date/time: set mtime of path to the given Unix time */
int mhs_SetFileDateTime(const char *path, long long mtime);

/* Execute file in a directory, optionally waiting for completion */
int mhs_RunFile(const char *filename, const char *curdir, int wait_finish);

/* Get path of current executable into buf (NUL-terminated) */
int mhs_GetExeName(char *buf, int bufsize);

/* System information */
unsigned int mhs_GetPhysicalMemory(void);
unsigned int mhs_GetAvailablePhysicalMemory(void);
unsigned int mhs_GetMaxMemToAlloc(void);
int          mhs_GetProcessorsCount(void);

/* Console title */
void mhs_SetConsoleTitle(const char *title);
void mhs_ResetConsoleTitle(void);

/* Read random bytes from /dev/urandom */
int mhs_read_urandom(void *buf, int size);

/* Thread ID (Linux gettid) */
int32_t mhs_gettid(void);

/* URL stubs (FREEARC_NOURL equivalent for MicroHs) */
void    mhs_url_setup_proxy       (const char *proxy);
void    mhs_url_setup_bypass_list (const char *bypass);
void   *mhs_url_open              (const char *url);
int64_t mhs_url_pos  (void *url);
int64_t mhs_url_size (void *url);
void    mhs_url_seek (void *url, int64_t pos);
int     mhs_url_read (void *url, char *buf, int size);
void    mhs_url_close(void *url);

/* MM detection stubs (always report "not MM data") */
void mhs_detect_datatype  (const char *buf, int bufsize, char *type);
int  mhs_detect_mm_bytes  (int mode, int filesize);
int  mhs_detect_mm        (int mode, const char *buf, int bufsize);
int  mhs_detect_mm_header (int mode, const char *buf, int bufsize);

/* =========================================================================
 * CompressionLib stubs
 * ========================================================================= */
/* Global flag for buffer-to-buffer compression mode */
extern int compress_all_at_once;

int  mhs_GetCompressionThreads(void);
void mhs_SetCompressionThreads(int threads);
void mhs_ClearExternalCompressorsTable(void);
int  mhs_AddExternalCompressor(const char *description);
int  mhs_CanonizeCompressionMethod(const char *method, char *canonical_method);
int  mhs_CompressionService(const char *method, const char *what, int param, void *data, void *callback);
void mhs_compressionLib_cleanup(void);

unsigned int mhs_GetCompressionMem  (const char *method);
unsigned int mhs_GetDecompressionMem(const char *method);
unsigned int mhs_GetDictionary      (const char *method);
unsigned int mhs_GetBlockSize       (const char *method);

int mhs_SetCompressionMem  (const char *in_method, unsigned int bytes, char *out_method);
int mhs_SetDecompressionMem(const char *in_method, unsigned int bytes, char *out_method);
int mhs_SetDictionary      (const char *in_method, unsigned int bytes, char *out_method);
int mhs_SetBlockSize       (const char *in_method, unsigned int bytes, char *out_method);
int mhs_LimitCompressionMem  (const char *in_method, unsigned int bytes, char *out_method);
int mhs_LimitDecompressionMem(const char *in_method, unsigned int bytes, char *out_method);
int mhs_LimitDictionary      (const char *in_method, unsigned int bytes, char *out_method);
int mhs_LimitBlockSize       (const char *in_method, unsigned int bytes, char *out_method);

int mhs_Compress            (const char *method, void *read_cb, void *write_cb);
int mhs_Decompress          (const char *method, void *read_cb, void *write_cb);
int mhs_CompressWithHeader  (const char *method, void *read_cb, void *write_cb);
int mhs_DecompressWithHeader(void *read_cb, void *write_cb);
int mhs_CompressMem            (const char *method, void *input, int inputSize, void *output, int outputSize);
int mhs_DecompressMem          (const char *method, void *input, int inputSize, void *output, int outputSize);
int mhs_CompressMemWithHeader  (const char *method, void *input, int inputSize, void *output, int outputSize);
int mhs_DecompressMemWithHeader(void *input, int inputSize, void *output, int outputSize);

/* =========================================================================
 * EncryptionLib stubs
 * ========================================================================= */
extern int mhs_fortuna_size;
int           mhs_fortuna_start      (void *prng);
int           mhs_fortuna_add_entropy(const void *in, unsigned long inlen, void *prng);
int           mhs_fortuna_ready      (void *prng);
unsigned long mhs_fortuna_read       (void *out, unsigned long outlen, void *prng);
void          mhs_Pbkdf2Hmac         (const char *password, int pwdlen,
                                      const char *salt,     int saltlen,
                                      int iterations,
                                      char *key,            int keylen);

#ifdef __cplusplus
}
#endif

#endif /* MHS_STUBS_H */
