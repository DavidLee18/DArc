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

/* Terminal attribute helpers for System.Posix.Terminal shim */
int mhs_tcgetattr(int fd, struct termios *t) { return tcgetattr(fd, t); }
int mhs_tcsetattr(int fd, int action, struct termios *t) { return tcsetattr(fd, action, t); }
