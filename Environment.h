#include <time.h>
#include "Compression/Common.h"
/* Common.h defines stat() as a 1-arg statistics macro; undefine it so the
   POSIX stat(2) syscall (2 args) remains accessible in this translation unit. */
#ifdef stat
#undef stat
#endif

#define PRESENT_INT32

#ifdef  __cplusplus
extern "C" {
#endif

#define INIT_CRC 0xffffffff

// Environment.cpp
void SetFileDateTime (const CFILENAME Filename, time_t t); // Set the file modification time/date
void RunProgram (const CFILENAME filename, const CFILENAME curdir, int wait_finish);  // Execute program `filename` in the directory `curdir` optionally waiting until it finished
void RunFile    (const CFILENAME filename, const CFILENAME curdir, int wait_finish);  // Execute file `filename` in the directory `curdir` optionally waiting until it finished
int long_path_size (void);                                 // Maximum file name length
void FormatDateTime (char *buf, int bufsize, time_t t);    // Format time/date for the listing command
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // Return the name of the program's executable file
unsigned GetPhysicalMemory (void);                         // Amount of physical memory in the computer
unsigned GetMaxMemToAlloc (void);                          // Max amount of memory we can allocate in our process's address space
unsigned GetAvailablePhysicalMemory (void);                // Amount of free physical memory in the computer
void TestMalloc (void);                                    // Prints free memory statistics
int GetProcessorsCount (void);                             // Total number of processors (more precisely, physical cores) in the system. Used to decide how many "heavy" computational threads it makes sense to run in the program
uint UpdateCRC (void *Addr, uint Size, uint StartCRC);     // Update the CRC with the contents of a data block
uint CalcCRC (void *Addr, uint Size);                      // Compute the CRC of a data block
void memxor (char *dest, char *src, uint size);            // XOR two data blocks together
int systemRandomData (char *rand_buf, int rand_size);
long darc_urandom_read (void *buf, long size);
void BuildPathTo (CFILENAME name);                         // Create the directories on the path to name

// FreeArc 0.67 --shutdown / -ioff: power off the machine after archive op.
void PowerOffComputer(void);

// ---------------------------------------------------------------------------
// Helper entry points called from MicroHs-generated C.
//
// These are defined with extern "C" in Environment.cpp but were never declared
// here, so every call site saw an implicit declaration. C89 lets that through
// by assuming `int f()`; C99 removed it, and newer clang rejects it outright,
// which is why the macOS build failed at link time while Linux only warned
// (and -w hid even that).
//
// It is also a real defect rather than a portability nuisance. An implicit
// declaration assumes a return type of int, so the eight functions below that
// return long had their results truncated to 32 bits at every call. That is
// almost certainly why the _w variants exist, writing their result through an
// out-parameter to sidestep the return value entirely. Declaring them properly
// removes the need for that workaround.
// ---------------------------------------------------------------------------
long darc_bfile_read (void *bf, void *buf, long size);
void darc_bfile_read_w (void *bf, void *buf, long size, long *out);
int darc_bfile_seek (void *bf, long offset, int whence);
long darc_bfile_size (void *bf);
void darc_bfile_size_w (void *bf, long *out);
long darc_bfile_tell (void *bf);
void darc_bfile_tell_w (void *bf, long *out);
int darc_bfile_truncate (void *bf, long size);
long darc_bfile_write (void *bf, const void *buf, long size);
void darc_bfile_write_w (void *bf, const void *buf, long size, long *out);
int darc_check_sigint (void);
void darc_clear_sigint (void);
void darc_fill_tm (int *out, int sec, int min_, int hour, int mday, int mon, int year, int wday, int yday, int isdst, int gmtoff_min);
int darc_get_nprocs (void);
void darc_gmtime (long secs, int *out);
void darc_install_sigint (void);
void darc_localtime (long secs, int *out);
long darc_mktime_tz (int year, int mon, int mday, int hour, int min, int sec, int gmtoff_min);
void darc_mktime_tz_w (int year, int mon, int mday, int hour, int min, int sec, int gmtoff_min, long *out);
int darc_realpath (const char *path, char *out);
int darc_sizeof_stat (void);
unsigned int darc_st_mode (struct stat *p);
long darc_st_mtime (struct stat *p);
void darc_st_mtime_w (struct stat *p, long *out);
long darc_st_size (struct stat *p);
void darc_st_size_w (struct stat *p, long *out);
int darc_strftime (char *buf, size_t size, const char *fmt, int *flat_tm);
long darc_time (void);
void darc_time_w (long *out);
void darc_urandom_read_w (void *buf, long size, long *out);
int darc_utimes (const char *path, long atime, long mtime);


#ifdef  __cplusplus
}
#endif
