/*-------------------------------------------------------------------------*/
/*  Native .7z reader for DArc.                                            */
/*                                                                         */
/*  Read-only: list / extract / test, built on the 7zip C SDK (7zArcIn +   */
/*  7zDec). Source under Compression/7z/sdk/ (lzma SDK 26.00).             */
/*                                                                         */
/*  Exposes three C entry points: darc_7z_list / darc_7z_extract /         */
/*  darc_7z_test, each taking a filesystem path. UTF-16 names from the     */
/*  archive are converted to UTF-8 before being written to disk or printed */
/*  to stdout.                                                             */
/*-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <stdint.h>

#if defined(_WIN32) || defined(FREEARC_WIN)
#  include <direct.h>
#  define darc_mkdir(p) _mkdir(p)
#else
#  define darc_mkdir(p) mkdir((p), 0755)
#endif

#include "sdk/7z.h"
#include "sdk/7zAlloc.h"
#include "sdk/7zBuf.h"
#include "sdk/7zCrc.h"
#include "sdk/7zFile.h"
#include "sdk/7zTypes.h"

static int g_crc_ready = 0;
static void ensure_crc(void) {
  if (!g_crc_ready) { CrcGenerateTable(); g_crc_ready = 1; }
}

/* Convert a null-terminated UTF-16 (BMP+astral) string to UTF-8.
   Writes up to dst_cap-1 bytes + NUL. Returns bytes written (excl NUL). */
static size_t u16_to_u8(const UInt16 *s, char *dst, size_t dst_cap) {
  size_t di = 0;
  if (dst_cap == 0) return 0;
  for (size_t i = 0; s[i] != 0 && di + 4 < dst_cap; i++) {
    unsigned c = s[i];
    if (c >= 0xD800 && c <= 0xDBFF && s[i+1] >= 0xDC00 && s[i+1] <= 0xDFFF) {
      unsigned c2 = s[++i];
      c = 0x10000 + (((c - 0xD800) << 10) | (c2 - 0xDC00));
    }
    if (c < 0x80) {
      dst[di++] = (char)c;
    } else if (c < 0x800) {
      dst[di++] = (char)(0xC0 | (c >> 6));
      dst[di++] = (char)(0x80 | (c & 0x3F));
    } else if (c < 0x10000) {
      dst[di++] = (char)(0xE0 | (c >> 12));
      dst[di++] = (char)(0x80 | ((c >> 6) & 0x3F));
      dst[di++] = (char)(0x80 | (c & 0x3F));
    } else {
      dst[di++] = (char)(0xF0 | (c >> 18));
      dst[di++] = (char)(0x80 | ((c >> 12) & 0x3F));
      dst[di++] = (char)(0x80 | ((c >> 6) & 0x3F));
      dst[di++] = (char)(0x80 | (c & 0x3F));
    }
  }
  dst[di] = '\0';
  return di;
}

/* mkdir -p for a UTF-8 path. Modifies the string in place. */
static void mkdir_p(char *path) {
  for (char *p = path + 1; *p; p++) {
    if (*p == '/') {
      *p = 0;
      darc_mkdir(path);
      *p = '/';
    }
  }
}

/* Open archive + initialise decoder state. Caller must call close7z(). */
typedef struct {
  CFileInStream  archiveStream;
  CLookToRead2   lookStream;
  CSzArEx        db;
  ISzAlloc       alloc;
  ISzAlloc       allocTemp;
  UInt16        *tempName;
  size_t         tempCap;
  int            opened;
} Sz7zCtx;

static SRes open7z(Sz7zCtx *c, const char *path) {
  memset(c, 0, sizeof *c);
  c->alloc.Alloc = SzAlloc;      c->alloc.Free = SzFree;
  c->allocTemp.Alloc = SzAllocTemp; c->allocTemp.Free = SzFreeTemp;

  if (InFile_Open(&c->archiveStream.file, path) != 0) return SZ_ERROR_FAIL;
  FileInStream_CreateVTable(&c->archiveStream);
  c->archiveStream.wres = 0;
  LookToRead2_CreateVTable(&c->lookStream, False);
  c->lookStream.buf = (Byte *)ISzAlloc_Alloc(&c->alloc, 1 << 18);
  if (!c->lookStream.buf) { File_Close(&c->archiveStream.file); return SZ_ERROR_MEM; }
  c->lookStream.bufSize = 1 << 18;
  c->lookStream.realStream = &c->archiveStream.vt;
  LookToRead2_INIT(&c->lookStream)

  ensure_crc();
  SzArEx_Init(&c->db);
  SRes res = SzArEx_Open(&c->db, &c->lookStream.vt, &c->alloc, &c->allocTemp);
  if (res == SZ_OK) c->opened = 1;
  return res;
}

static void close7z(Sz7zCtx *c) {
  if (c->opened) SzArEx_Free(&c->db, &c->alloc);
  if (c->lookStream.buf) ISzAlloc_Free(&c->alloc, c->lookStream.buf);
  SzFree(NULL, c->tempName);
  File_Close(&c->archiveStream.file);
}

/* Ensure c->tempName has room for `len` UInt16s. */
static int ensure_temp(Sz7zCtx *c, size_t len) {
  if (len <= c->tempCap) return 0;
  UInt16 *t = (UInt16 *)SzAlloc(NULL, len * sizeof *t);
  if (!t) return -1;
  SzFree(NULL, c->tempName);
  c->tempName = t;
  c->tempCap = len;
  return 0;
}

/*-------------------------------------------------------------------------*/
/*  Public entry points                                                    */
/*-------------------------------------------------------------------------*/

int darc_7z_list(const char *path) {
  Sz7zCtx c;
  SRes res = open7z(&c, path);
  if (res != SZ_OK) { close7z(&c); return (int)res; }

  char name8[4096];
  printf("%-20s %10s  %s\n", "Date", "Size", "Name");
  for (UInt32 i = 0; i < c.db.NumFiles; i++) {
    size_t len = SzArEx_GetFileNameUtf16(&c.db, i, NULL);
    if (ensure_temp(&c, len) != 0) { res = SZ_ERROR_MEM; break; }
    SzArEx_GetFileNameUtf16(&c.db, i, c.tempName);
    u16_to_u8(c.tempName, name8, sizeof name8);

    UInt64 size = SzArEx_GetFileSize(&c.db, i);
    int isDir = SzArEx_IsDir(&c.db, i);
    printf("%-20s %10llu  %s%s\n",
           "                   -", (unsigned long long)size,
           name8, isDir ? "/" : "");
  }
  close7z(&c);
  return (int)res;
}

/* Shared work for extract/test. out_dir == NULL means test-only (no writes). */
static int extract_or_test(const char *path, const char *out_dir) {
  Sz7zCtx c;
  SRes res = open7z(&c, path);
  if (res != SZ_OK) { close7z(&c); return (int)res; }

  UInt32 blockIndex  = 0xFFFFFFFF;
  Byte  *outBuffer   = NULL;
  size_t outBufSize  = 0;
  char   name8[4096];
  char   fullPath[8192];

  for (UInt32 i = 0; i < c.db.NumFiles; i++) {
    size_t offset = 0, processed = 0;
    size_t len = SzArEx_GetFileNameUtf16(&c.db, i, NULL);
    if (ensure_temp(&c, len) != 0) { res = SZ_ERROR_MEM; break; }
    SzArEx_GetFileNameUtf16(&c.db, i, c.tempName);
    u16_to_u8(c.tempName, name8, sizeof name8);

    int isDir = SzArEx_IsDir(&c.db, i);
    if (isDir) {
      if (out_dir) {
        snprintf(fullPath, sizeof fullPath, "%s/%s", out_dir, name8);
        mkdir_p(fullPath);
        darc_mkdir(fullPath);
      }
      continue;
    }

    res = SzArEx_Extract(&c.db, &c.lookStream.vt, i,
                         &blockIndex, &outBuffer, &outBufSize,
                         &offset, &processed,
                         &c.alloc, &c.allocTemp);
    if (res != SZ_OK) break;

    if (out_dir) {
      snprintf(fullPath, sizeof fullPath, "%s/%s", out_dir, name8);
      mkdir_p(fullPath);
      FILE *f = fopen(fullPath, "wb");
      if (!f) {
        fprintf(stderr, "7z: cannot open %s for writing: %s\n",
                fullPath, strerror(errno));
        res = SZ_ERROR_FAIL; break;
      }
      if (processed > 0 && fwrite(outBuffer + offset, 1, processed, f) != processed) {
        fclose(f); res = SZ_ERROR_WRITE; break;
      }
      fclose(f);
      printf("- %s\n", name8);
    } else {
      printf("T %s\n", name8);
    }
  }

  if (outBuffer) ISzAlloc_Free(&c.alloc, outBuffer);
  close7z(&c);
  return (int)res;
}

int darc_7z_extract(const char *path, const char *out_dir) {
  return extract_or_test(path, out_dir ? out_dir : ".");
}

int darc_7z_test(const char *path) {
  return extract_or_test(path, NULL);
}
