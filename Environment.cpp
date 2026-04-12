#define _WIN32_WINNT 0x0500
#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdint.h>
#include "Environment.h"
#include "Compression/Compression.h"


// Изменим настройки RTS, включив compacting GC начиная с 40 mb:
char *ghc_rts_opts = "-c1 -M4000m";


/* ********************************************************************************************************
*  Find largest contiguous memory block available and dump information about all available memory blocks
***********************************************************************************************************/

void memstat(void);

struct LargestMemoryBlock
{
  void   *p;
  size_t size;
  LargestMemoryBlock();
  ~LargestMemoryBlock()         {free();}
  void alloc(size_t n);
  void free();
  void test();
};

LargestMemoryBlock::LargestMemoryBlock() : p(NULL)
{
  size_t a=0, b=UINT_MAX;
  while (b-a>1) {
    free();
    size_t c=(a+b)/2;
    alloc(c);
    if(p) a=c;  else b=c;
  }
}

void LargestMemoryBlock::test()
{
  if ((size>>20)>0) {
    printf("Allocated %4d mb, addr=%p\n", size>>20, p);
    LargestMemoryBlock next;
    next.test();
  } else {
    memstat();
  }
}


void TestMalloc (void)
{
  memstat();
  printf("\n");
  LargestMemoryBlock m;
  m.test();
}


#ifdef FREEARC_WIN

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <time.h>

// Provide VirtualAlloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p = VirtualAlloc (0, size=n, MEM_RESERVE, PAGE_READWRITE);};
void LargestMemoryBlock::free ()         {VirtualFree (p, 0, MEM_RELEASE); p=NULL;};


// Use to convert bytes to MB
#define DIV (1024*1024)

// Specify the width of the field in which to print the numbers.
// The asterisk in the format specifier "%*I64d" takes an integer
// argument and uses it to pad and right justify the number.
#define WIDTH 4

void memstat (void)
{
  MEMORYSTATUSEX statex;

  statex.dwLength = sizeof (statex);

  GlobalMemoryStatusEx (&statex);

  printf ("There is  %*ld percent of memory in use.\n",
          WIDTH, statex.dwMemoryLoad);
  printf ("There are %*I64d total Mbytes of physical memory.\n",
          WIDTH, statex.ullTotalPhys/DIV);
  printf ("There are %*I64d free Mbytes of physical memory.\n",
          WIDTH, statex.ullAvailPhys/DIV);
  printf ("There are %*I64d total Mbytes of paging file.\n",
          WIDTH, statex.ullTotalPageFile/DIV);
  printf ("There are %*I64d free Mbytes of paging file.\n",
          WIDTH, statex.ullAvailPageFile/DIV);
  printf ("There are %*I64d total Mbytes of virtual memory.\n",
          WIDTH, statex.ullTotalVirtual/DIV);
  printf ("There are %*I64d free Mbytes of virtual memory.\n",
          WIDTH, statex.ullAvailVirtual/DIV);

  // Show the amount of extended memory available.

  printf ("There are %*I64d free Mbytes of extended memory.\n",
          WIDTH, statex.ullAvailExtendedVirtual/DIV);
}

#else

// Provide malloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p=malloc(size=n);};
void LargestMemoryBlock::free ()         {::free(p); p=NULL;};

void memstat (void)
{
}

#endif


#ifdef FREEARC_WIN

/*
void SetDateTimeAttr(const char* Filename, time_t t)
{
    struct tm* t2 = gmtime(&t);

    SYSTEMTIME t3;
    t3.wYear         = t2->tm_year+1900;
    t3.wMonth        = t2->tm_mon+1;
    t3.wDay          = t2->tm_mday;
    t3.wHour         = t2->tm_hour;
    t3.wMinute       = t2->tm_min;
    t3.wSecond       = t2->tm_sec;
    t3.wMilliseconds = 0;

    FILETIME ft;
    SystemTimeToFileTime(&t3, &ft);

    HANDLE hndl=CreateFile(Filename,GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,0);
    SetFileTime(hndl,NULL,NULL,&ft);  //creation, last access, modification times
    CloseHandle(hndl);
    //SetFileAttributes(Filename,ai.attrib);
}
*/


CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  GetModuleFileNameA (NULL, buf, bufsize);
  return buf;
}

unsigned GetPhysicalMemory (void)
{
  MEMORYSTATUS buf;
    GlobalMemoryStatus (&buf);
  return buf.dwTotalPhys;
}

unsigned GetMaxMemToAlloc (void)
{
  LargestMemoryBlock block;
  return block.size - 5*mb;
}

unsigned GetAvailablePhysicalMemory (void)
{
  MEMORYSTATUS buf;
    GlobalMemoryStatus (&buf);
  return buf.dwAvailPhys;
}

int GetProcessorsCount (void)
{
  SYSTEM_INFO si;
    GetSystemInfo (&si);
  return si.dwNumberOfProcessors;
}

void SetFileDateTime (const CFILENAME Filename, time_t mtime)
{
  struct _stat st;
    _stat (Filename, &st);
  struct _utimbuf times;
    times.actime  = st.st_atime;
    times.modtime = mtime;
  _utime (Filename, &times);
}

// Execute program `filename` in the directory `curdir` optionally waiting until it finished
void RunProgram (const CFILENAME filename, const CFILENAME curdir, int wait_finish)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  ZeroMemory (&si, sizeof(si));
  si.cb = sizeof(si);
  ZeroMemory (&pi, sizeof(pi));
  BOOL process_created = CreateProcessA (filename, NULL, NULL, NULL, FALSE, 0, NULL, curdir, &si, &pi);

  if (process_created && wait_finish)
      WaitForSingleObject (pi.hProcess, INFINITE);
}

// Execute file `filename` in the directory `curdir` optionally waiting until it finished
void RunFile (const CFILENAME filename, const CFILENAME curdir, int wait_finish)
{
  SHELLEXECUTEINFO sei;
  ZeroMemory(&sei, sizeof(SHELLEXECUTEINFO));
  sei.cbSize = sizeof(SHELLEXECUTEINFO);
  sei.fMask = (wait_finish? SEE_MASK_NOCLOSEPROCESS : 0);
  sei.hwnd = GetActiveWindow();
  sei.lpFile = filename;
  sei.lpDirectory = curdir;
  sei.nShow = SW_SHOW;
  DWORD rc = ShellExecuteEx(&sei);
  if (rc && wait_finish)
    WaitForSingleObject(sei.hProcess, INFINITE);
}


#else // For Unix:


#include <unistd.h>
#include <sys/sysinfo.h>

unsigned GetPhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return si.totalram*si.mem_unit;
}

unsigned GetMaxMemToAlloc (void)
{
  //struct sysinfo si;
  //  sysinfo(&si);
  return UINT_MAX;
}

unsigned GetAvailablePhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return si.freeram*si.mem_unit;
}

int GetProcessorsCount (void)
{
  return get_nprocs();
}

void SetFileDateTime(const CFILENAME Filename, time_t mtime)
{
#undef stat
  struct stat st;
    stat (Filename, &st);
  struct utimbuf times;
    times.actime  = st.st_atime;
    times.modtime = mtime;
  utime (Filename, &times);
}

// Execute file `filename` in the directory `curdir` optionally waiting until it finished
void RunFile (const CFILENAME filename, const CFILENAME curdir, int wait_finish)
{
  char *olddir = (char*) malloc(MY_FILENAME_MAX*4),
       *cmd    = (char*) malloc(strlen(filename)+10);
  getcwd(olddir, MY_FILENAME_MAX*4);

  chdir(curdir);
  sprintf(cmd, "./%s%s", filename, wait_finish? "" : " &");
  system(cmd);

  chdir(olddir);
  free(cmd);
  free(olddir);
}

#endif // Windows/Unix


void FormatDateTime (char *buf, int bufsize, time_t t)
{
  struct tm *p;
  if (t==-1)  t=0;  // Иначе получим вылет :(
  p = localtime(&t);
  strftime( buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}

// Максимальная длина имени файла
int long_path_size (void)
{
  return MY_FILENAME_MAX;
}


/************************************************************************
 ************* CRC-32 subroutines ***************************************
 ************************************************************************/

uint CRCTab[256];
static uint CRCTab8[8][256];
static int crc_slice8_initialized = 0;

void InitCRC()
{
  for (int I=0;I<256;I++)
  {
    uint C=I;
    for (int J=0;J<8;J++)
      C=(C & 1) ? (C>>1)^0xEDB88320L : (C>>1);
    CRCTab[I]=C;
  }
}

// Build the 8 tables used by the slice-by-8 inner loop. Each CRCTab8[k][i]
// is the CRC of the one-byte value i followed by k zero bytes.
static void InitCRCSlice8()
{
  if (CRCTab[1]==0) InitCRC();
  for (int i=0; i<256; i++) CRCTab8[0][i] = CRCTab[i];
  for (int i=0; i<256; i++) {
    uint c = CRCTab8[0][i];
    for (int k=1; k<8; k++) {
      c = CRCTab8[0][c & 0xff] ^ (c >> 8);
      CRCTab8[k][i] = c;
    }
  }
  crc_slice8_initialized = 1;
}

// Slice-by-8 CRC-32 (polynomial 0xEDB88320, zlib/gzip compatible).
// Processes 8 input bytes per iteration with 8 parallel table lookups,
// replacing the previous sequential byte-at-a-time inner loop. ~3-5x faster
// on large buffers; binary-identical output.
uint UpdateCRC( void *Addr, uint Size, uint StartCRC)
{
  if (!crc_slice8_initialized)
    InitCRCSlice8();
  uint8 *Data = (uint8 *)Addr;
  uint crc = StartCRC;
#if defined(FREEARC_INTEL_BYTE_ORDER)
  while (Size >= 8) {
    uint32_t lo = crc ^ *(uint32_t *)Data;
    uint32_t hi =        *(uint32_t *)(Data + 4);
    crc = CRCTab8[7][ lo        & 0xff]
        ^ CRCTab8[6][(lo >>  8) & 0xff]
        ^ CRCTab8[5][(lo >> 16) & 0xff]
        ^ CRCTab8[4][ lo >> 24]
        ^ CRCTab8[3][ hi        & 0xff]
        ^ CRCTab8[2][(hi >>  8) & 0xff]
        ^ CRCTab8[1][(hi >> 16) & 0xff]
        ^ CRCTab8[0][ hi >> 24];
    Data += 8;
    Size -= 8;
  }
#endif
  for (uint I=0; I<Size; I++)
    crc = CRCTab[(uint8)(crc ^ Data[I])] ^ (crc >> 8);
  return crc;
}

// Вычислить CRC блока данных
uint CalcCRC( void *Addr, uint Size)
{
  return UpdateCRC (Addr, Size, INIT_CRC) ^ INIT_CRC;
}



// От-xor-ить два блока данных
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}

// Вернуть имя файла без имени каталога
FILENAME arc_basename (FILENAME fullname)
{
  char *p = fullname;
  for (char* q=fullname; *q; q++)
    if (in_set (*q, ALL_PATH_DELIMITERS))
      p = q+1;
  return p;
}

// Создать каталоги на пути к name
void BuildPathTo (CFILENAME name)
{
  CFILENAME path_ptr = NULL;
  for (CFILENAME p = _tcschr(name,0); --p >= name;)
    if (_tcschr (_T(DIRECTORY_DELIMITERS), *p))
      {path_ptr=p; break;}
  if (path_ptr==NULL)  return;

  TCHAR oldc = *path_ptr;
  *path_ptr = 0;

  if (! file_exists (name))
  {
    BuildPathTo (name);
    create_dir  (name);
  }
  *path_ptr = oldc;
}


/* ***************************************************************************
*                                                                            *
* Random system values collection routine from CryptLib by Peter Gutmann     *
* [ftp://ftp.franken.de/pub/crypt/cryptlib/cl331.zip]                        *
*                                                                            *
*****************************************************************************/

/* The size of the intermediate buffer used to accumulate polled data */
#define RANDOM_BUFSIZE	4096

// Handling random data buffer
#define initRandomData(rand_buf, rand_size)  \
                                 char *rand_ptr=(rand_buf), *rand_end=(rand_buf)+(rand_size)
#define addRandomData(ptr,size)  (memcpy (rand_ptr, (ptr), mymin((size),rand_end-rand_ptr)), rand_ptr+=mymin((size),rand_end-rand_ptr))
#define addRandomLong(value)     {long n=(value); addRandomData(&n, sizeof(long));}
#define addRandomValue(value)    addRandomLong((long) value)


/* Map a value that may be 32 or 64 bits depending on the platform to a long */
#if defined( _WIN64 ) || ( defined( _MSC_VER ) && ( _MSC_VER >= 1400 ) )
  #define addRandomHandle( handle ) \
		  addRandomLong( PtrToUlong( handle ) )
#else
  #define addRandomHandle	addRandomValue
#endif /* 32- vs. 64-bit VC++ */


// This routine fills buffer with system-generated pseudo-random data
// and returns number of bytes filled
int systemRandomData (char *rand_buf, int rand_size)
{
#ifdef FREEARC_WIN

	FILETIME  creationTime, exitTime, kernelTime, userTime;
	SIZE_T minimumWorkingSetSize, maximumWorkingSetSize;
	LARGE_INTEGER performanceCount;
	MEMORYSTATUS memoryStatus;
	HANDLE handle;
	POINT point;

	initRandomData (rand_buf, rand_size);

	/* Get various basic pieces of system information: Handle of active
	   window, handle of window with mouse capture, handle of clipboard owner
	   handle of start of clpboard viewer list, pseudohandle of current
	   process, current process ID, pseudohandle of current thread, current
	   thread ID, handle of desktop window, handle  of window with keyboard
	   focus, whether system queue has any events, cursor position for last
	   message, 1 ms time for last message, handle of window with clipboard
	   open, handle of process heap, handle of procs window station, types of
	   events in input queue, and milliseconds since Windows was started.
	   Since a HWND/HANDLE can be a 64-bit value on a 64-bit platform, we
	   have to use a mapping macro that discards the high 32 bits (which
	   presumably won't be of much interest anyway) */
	addRandomHandle( GetActiveWindow() );
	addRandomHandle( GetCapture() );
	addRandomHandle( GetClipboardOwner() );
	addRandomHandle( GetClipboardViewer() );
	addRandomHandle( GetCurrentProcess() );
	addRandomValue( GetCurrentProcessId() );
	addRandomHandle( GetCurrentThread() );
	addRandomValue( GetCurrentThreadId() );
	addRandomHandle( GetDesktopWindow() );
	addRandomHandle( GetFocus() );
	addRandomValue( GetInputState() );
	addRandomValue( GetMessagePos() );
	addRandomValue( GetMessageTime() );
	addRandomHandle( GetOpenClipboardWindow() );
	addRandomHandle( GetProcessHeap() );
	addRandomHandle( GetProcessWindowStation() );
	addRandomValue( GetTickCount() );

	/* Get multiword system information: Current caret position, current
	   mouse cursor position */
	GetCaretPos( &point );
	addRandomData( &point, sizeof( POINT ) );
	GetCursorPos( &point );
	addRandomData( &point, sizeof( POINT ) );

	/* Get percent of memory in use, bytes of physical memory, bytes of free
	   physical memory, bytes in paging file, free bytes in paging file, user
	   bytes of address space, and free user bytes */
	memoryStatus.dwLength = sizeof( MEMORYSTATUS );
	GlobalMemoryStatus( &memoryStatus );
	addRandomData( &memoryStatus, sizeof( MEMORYSTATUS ) );

	/* Get thread and process creation time, exit time, time in kernel mode,
	   and time in user mode in 100ns intervals */
	handle = GetCurrentThread();
	GetThreadTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );
	handle = GetCurrentProcess();
	GetProcessTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );

	/* Get the minimum and maximum working set size for the current process */
	GetProcessWorkingSetSize( handle, &minimumWorkingSetSize, &maximumWorkingSetSize );
	addRandomValue( minimumWorkingSetSize );
	addRandomValue( maximumWorkingSetSize );

	/* The following are fixed for the lifetime of the process */
       	/* Get name of desktop, console window title, new window position and
       	   size, window flags, and handles for stdin, stdout, and stderr */
       	STARTUPINFO startupInfo;
       	startupInfo.cb = sizeof( STARTUPINFO );
       	GetStartupInfo( &startupInfo );
       	addRandomData( &startupInfo, sizeof( STARTUPINFO ) );

	/* The performance of QPC varies depending on the architecture it's
	   running on and on the OS, the MS documentation is vague about the
	   details because it varies so much.  Under Win9x/ME it reads the
	   1.193180 MHz PIC timer.  Under NT/Win2K/XP it may or may not read the
	   64-bit TSC depending on the HAL and assorted other circumstances,
	   generally on machines with a uniprocessor HAL
	   KeQueryPerformanceCounter() uses a 3.579545MHz timer and on machines
	   with a multiprocessor or APIC HAL it uses the TSC (the exact time
	   source is controlled by the HalpUse8254 flag in the kernel).  That
	   choice of time sources is somewhat peculiar because on a
	   multiprocessor machine it's theoretically possible to get completely
	   different TSC readings depending on which CPU you're currently
	   running on, while for uniprocessor machines it's not a problem.
	   However, the kernel appears to synchronise the TSCs across CPUs at
	   boot time (it resets the TSC as part of its system init), so this
	   shouldn't really be a problem.  Under WinCE it's completely platform-
	   dependant, if there's no hardware performance counter available, it
	   uses the 1ms system timer.

	   Another feature of the TSC (although it doesn't really affect us here)
	   is that mobile CPUs will turn off the TSC when they idle, Pentiums
	   will change the rate of the counter when they clock-throttle (to
	   match the current CPU speed), and hyperthreading Pentiums will turn
	   it off when both threads are idle (this more or less makes sense,
	   since the CPU will be in the halted state and not executing any
	   instructions to count).

	   To make things unambiguous, we detect a CPU new enough to call RDTSC
	   directly by checking for CPUID capabilities, and fall back to QPC if
	   this isn't present */
       	if( QueryPerformanceCounter( &performanceCount ) )
       		addRandomData( &performanceCount,
       					   sizeof( LARGE_INTEGER ) );
       	else
       		/* Millisecond accuracy at best... */
       		addRandomValue( GetTickCount() );

        return rand_ptr-rand_buf;

#else // For Unix:

	FILE *f = fopen ("/dev/urandom", "rb");

	if (f == NULL)
	{
		perror ("Cannot open /dev/urandom");
		return 0;
	}

	if (file_read (f, rand_buf, rand_size) != rand_size)
	{
		perror ("Read from /dev/urandom failed");
		fclose (f);
		return 0;
	}

	fclose (f);
	return rand_size;

#endif // Windows/Unix

}

/****************************************************************************
*
*                                           Random system values collection *
*
****************************************************************************/

/****************************************************************************
*  SIGINT helpers for the System.Posix.Signals MicroHs shim                *
*  darc_install_sigint / darc_check_sigint / darc_clear_sigint             *
****************************************************************************/
#ifndef FREEARC_WIN
#include <signal.h>
#include <stdint.h>

static volatile int darc_sigint_fired = 0;

static void darc_sigint_handler(int) {
    darc_sigint_fired = 1;
    /* Reinstall so the next Ctrl-C also fires (mirrors CatchOnce behaviour
       managed from the Haskell side). */
    signal(SIGINT, darc_sigint_handler);
}

extern "C" void darc_install_sigint(void) {
    signal(SIGINT, darc_sigint_handler);
}

extern "C" int darc_check_sigint(void) {
    return darc_sigint_fired;
}

extern "C" void darc_clear_sigint(void) {
    darc_sigint_fired = 0;
}
#else  /* FREEARC_WIN: stub sigint handlers on Windows */
extern "C" void darc_install_sigint(void) {}
extern "C" int  darc_check_sigint(void) { return 0; }
extern "C" void darc_clear_sigint(void) {}
#endif // !FREEARC_WIN

/****************************************************************************
*  MicroHs compat helpers: stat accessors and processor count              *
****************************************************************************/
#ifndef FREEARC_WIN
#include <sys/stat.h>
#include <unistd.h>

extern "C" int darc_sizeof_stat(void) {
    return (int)sizeof(struct stat);
}

extern "C" unsigned int darc_st_mode(struct stat *p) {
    return (unsigned int)p->st_mode;
}

// realpath wrapper: returns 0 on success, -1 on failure
extern "C" int darc_realpath(const char *path, char *out) {
    char *r = realpath(path, out);
    return r ? 0 : -1;
}

extern "C" int darc_utimes(const char *path, long atime, long mtime) {
    struct utimbuf ut;
    ut.actime  = (time_t)atime;
    ut.modtime = (time_t)mtime;
    return utime(path, &ut);
}

extern "C" long darc_st_size(struct stat *p) {
    return (long)p->st_size;
}

extern "C" long darc_st_mtime(struct stat *p) {
    return (long)p->st_mtime;
}

/* MicroHs workaround: FFI return values are truncated to 32 bits.
   These _w variants write 64-bit results via pointer instead. */
extern "C" void darc_st_size_w(struct stat *p, long *out) {
    *out = (long)p->st_size;
}

extern "C" void darc_st_mtime_w(struct stat *p, long *out) {
    *out = (long)p->st_mtime;
}
#endif // !FREEARC_WIN (stat/realpath/utime POSIX block)

/****************************************************************************
*  Windows compat helpers for POSIX APIs used by the portable blocks below *
****************************************************************************/
#ifdef FREEARC_WIN
#include <windows.h>
#include <io.h>        /* _chsize_s, _fullpath */
#include <sys/stat.h>
#include <sys/utime.h>
#include <time.h>
#include <wincrypt.h>
#ifndef ftruncate
static inline int ftruncate(int fd, long long size) {
    return _chsize_s(fd, (__int64)size);
}
#endif
static inline int darc_win_nprocs(void) {
    SYSTEM_INFO si; GetSystemInfo(&si);
    return si.dwNumberOfProcessors > 0 ? (int)si.dwNumberOfProcessors : 1;
}
/* localtime_r / gmtime_r fallbacks for Windows (MSVC uses localtime_s; MinGW-w64
   has localtime_s too but not the POSIX _r variants in default headers). */
static inline struct tm* darc_localtime_r_win(const time_t *t, struct tm *out) {
    return localtime_s(out, t) == 0 ? out : NULL;
}
static inline struct tm* darc_gmtime_r_win(const time_t *t, struct tm *out) {
    return gmtime_s(out, t) == 0 ? out : NULL;
}
#define localtime_r darc_localtime_r_win
#define gmtime_r    darc_gmtime_r_win

extern "C" int darc_sizeof_stat(void) { return (int)sizeof(struct stat); }
extern "C" unsigned int darc_st_mode(struct stat *p) { return (unsigned int)p->st_mode; }
extern "C" int darc_realpath(const char *path, char *out) {
    return _fullpath(out, path, MAX_PATH) ? 0 : -1;
}
extern "C" int darc_utimes(const char *path, long atime, long mtime) {
    struct _utimbuf ut; ut.actime = (time_t)atime; ut.modtime = (time_t)mtime;
    return _utime(path, &ut);
}
extern "C" long darc_st_size(struct stat *p) { return (long)p->st_size; }
extern "C" long darc_st_mtime(struct stat *p) { return (long)p->st_mtime; }
extern "C" void darc_st_size_w(struct stat *p, long *out) { *out = (long)p->st_size; }
extern "C" void darc_st_mtime_w(struct stat *p, long *out) { *out = (long)p->st_mtime; }
#endif // FREEARC_WIN

/****************************************************************************
*  Handle IO helpers for MicroHs (hSeek, hTell, hFileSize, hSetFileSize)   *
*  BFILE_file layout: BFILE (7 fn ptrs = 56 bytes) + FILE* at offset 56    *
****************************************************************************/
#include <stdio.h>

static FILE* bfile_to_file(void *bf) {
    /* The FILE* is at offset 56 (sizeof(BFILE) = 7 * sizeof(void*)) */
    return *(FILE**)((char*)bf + 7 * sizeof(void*));
}

extern "C" int darc_bfile_seek(void *bf, long offset, int whence) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    return fseek(f, offset, whence);
}

extern "C" long darc_bfile_tell(void *bf) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    return ftell(f);
}

extern "C" long darc_bfile_size(void *bf) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    long pos = ftell(f);
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, pos, SEEK_SET);
    return size;
}

/* MicroHs workaround: write 64-bit results via pointer. */
extern "C" void darc_bfile_tell_w(void *bf, long *out) {
    FILE *f = bfile_to_file(bf);
    *out = f ? ftell(f) : -1;
}

extern "C" void darc_bfile_size_w(void *bf, long *out) {
    FILE *f = bfile_to_file(bf);
    if (!f) { *out = -1; return; }
    long pos = ftell(f);
    fseek(f, 0, SEEK_END);
    *out = ftell(f);
    fseek(f, pos, SEEK_SET);
}

extern "C" void darc_bfile_read_w(void *bf, void *buf, long size, long *out) {
    FILE *f = bfile_to_file(bf);
    *out = f ? (long)fread(buf, 1, (size_t)size, f) : -1;
}

extern "C" void darc_bfile_write_w(void *bf, const void *buf, long size, long *out) {
    FILE *f = bfile_to_file(bf);
    *out = f ? (long)fwrite(buf, 1, (size_t)size, f) : -1;
}

extern "C" int darc_bfile_truncate(void *bf, long size) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    fflush(f);
    int fd = fileno(f);
    return ftruncate(fd, (off_t)size);
}

extern "C" long darc_bfile_read(void *bf, void *buf, long size) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    return (long)fread(buf, 1, (size_t)size, f);
}

extern "C" long darc_bfile_write(void *bf, const void *buf, long size) {
    FILE *f = bfile_to_file(bf);
    if (!f) return -1;
    return (long)fwrite(buf, 1, (size_t)size, f);
}

extern "C" int darc_get_nprocs(void) {
#ifdef FREEARC_WIN
    return darc_win_nprocs();
#else
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return (n > 0) ? (int)n : 1;
#endif
}

/* Random bytes: /dev/urandom on POSIX, CryptGenRandom on Windows. */
extern "C" long darc_urandom_read(void *buf, long size) {
#ifdef FREEARC_WIN
    HCRYPTPROV h;
    if (!CryptAcquireContextA(&h, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT)) return -1;
    BOOL ok = CryptGenRandom(h, (DWORD)size, (BYTE*)buf);
    CryptReleaseContext(h, 0);
    return ok ? size : -1;
#else
    FILE *f = fopen("/dev/urandom", "rb");
    if (!f) return -1;
    long n = (long)fread(buf, 1, (size_t)size, f);
    fclose(f);
    return n;
#endif
}

extern "C" void darc_urandom_read_w(void *buf, long size, long *out) {
    *out = darc_urandom_read(buf, size);
}

/****************************************************************************
*  System.Time helpers for the MicroHs shim                                *
*  Uses a flat int[10] layout: sec,min,hour,mday,mon,year,wday,yday,isdst,gmtoff_min
****************************************************************************/
#include <time.h>

static void tm_to_flat(struct tm *t, int *out) {
    out[0] = t->tm_sec;
    out[1] = t->tm_min;
    out[2] = t->tm_hour;
    out[3] = t->tm_mday;
    out[4] = t->tm_mon;
    out[5] = t->tm_year;
    out[6] = t->tm_wday;
    out[7] = t->tm_yday;
    out[8] = t->tm_isdst;
#ifdef __linux__
    out[9] = (int)(t->tm_gmtoff / 60);
#else
    out[9] = 0;
#endif
}

static void flat_to_tm(int *in, struct tm *t) {
    t->tm_sec   = in[0];
    t->tm_min   = in[1];
    t->tm_hour  = in[2];
    t->tm_mday  = in[3];
    t->tm_mon   = in[4];
    t->tm_year  = in[5];
    t->tm_wday  = in[6];
    t->tm_yday  = in[7];
    t->tm_isdst = in[8];
}

extern "C" long darc_time(void) {
    return (long)time(NULL);
}

extern "C" void darc_time_w(long *out) {
    *out = (long)time(NULL);
}

extern "C" void darc_localtime(long secs, int *out) {
    time_t t = (time_t)secs;
    struct tm buf;
    struct tm *r = localtime_r(&t, &buf);
    if (r) tm_to_flat(r, out);
}

extern "C" void darc_gmtime(long secs, int *out) {
    time_t t = (time_t)secs;
    struct tm buf;
    struct tm *r = gmtime_r(&t, &buf);
    if (r) tm_to_flat(r, out);
}

extern "C" long darc_mktime_tz(int year, int mon, int mday, int hour, int min, int sec, int gmtoff_min) {
    struct tm t = {};
    t.tm_year  = year;
    t.tm_mon   = mon;
    t.tm_mday  = mday;
    t.tm_hour  = hour;
    t.tm_min   = min;
    t.tm_sec   = sec;
    t.tm_isdst = -1;
    /* Adjust for timezone offset */
    time_t r = mktime(&t);
    r -= (time_t)(gmtoff_min * 60);
    /* Add local UTC offset back */
    struct tm local_check;
    localtime_r(&r, &local_check);
#ifdef __linux__
    r += local_check.tm_gmtoff;
#endif
    return (long)r;
}

extern "C" void darc_mktime_tz_w(int year, int mon, int mday, int hour, int min, int sec, int gmtoff_min, long *out) {
    *out = darc_mktime_tz(year, mon, mday, hour, min, sec, gmtoff_min);
}

extern "C" void darc_fill_tm(int *out, int sec, int min_, int hour, int mday, int mon,
                              int year, int wday, int yday, int isdst, int gmtoff_min) {
    out[0] = sec; out[1] = min_; out[2] = hour; out[3] = mday;
    out[4] = mon; out[5] = year; out[6] = wday; out[7] = yday;
    out[8] = isdst; out[9] = gmtoff_min;
}

extern "C" int darc_strftime(char *buf, size_t size, const char *fmt, int *flat_tm) {
    struct tm t = {};
    flat_to_tm(flat_tm, &t);
    return (int)strftime(buf, size, fmt, &t);
}


// ============================================================
// MHS C-side compression/decompression pipeline
// Bypasses slow MHS Haskell pipe iteration for large data.
// Data is accumulated in a C-managed growing buffer, then
// compressed/decompressed using streaming Compress/Decompress.
// ============================================================
#ifdef __cplusplus
extern "C" {
#endif

// Growing buffer for pipeline input/output
static char *g_pipeline_buf = NULL;
static long  g_pipeline_size = 0;
static long  g_pipeline_cap = 0;

void darc_pipeline_init(long initial_cap) {
    free(g_pipeline_buf);
    if (initial_cap < 65536) initial_cap = 65536;
    g_pipeline_buf = (char *)malloc(initial_cap);
    g_pipeline_size = 0;
    g_pipeline_cap = initial_cap;
}

void darc_pipeline_append(const void *data, long len) {
    if (!g_pipeline_buf) return;
    if (g_pipeline_size + len > g_pipeline_cap) {
        while (g_pipeline_size + len > g_pipeline_cap)
            g_pipeline_cap *= 2;
        g_pipeline_buf = (char *)realloc(g_pipeline_buf, g_pipeline_cap);
    }
    memcpy(g_pipeline_buf + g_pipeline_size, data, len);
    g_pipeline_size += len;
}

void darc_pipeline_get_buf_w(void **out_buf, long *out_size) {
    *out_buf = g_pipeline_buf;
    *out_size = g_pipeline_size;
    g_pipeline_buf = NULL;
    g_pipeline_size = 0;
    g_pipeline_cap = 0;
}

void darc_pipeline_free(void) {
    free(g_pipeline_buf);
    g_pipeline_buf = NULL;
    g_pipeline_size = 0;
    g_pipeline_cap = 0;
}

// Streaming callback that reads from/writes to memory buffers.
// Used by darc_pipeline_compress_step / darc_pipeline_decompress_step.
typedef struct {
    const char *in_buf;
    long in_pos;
    long in_size;
    char *out_buf;
    long out_pos;
    long out_cap;
} PipelineCtx;

static int pipeline_callback(const char *what, void *buf, int size, void *auxdata) {
    PipelineCtx *ctx = (PipelineCtx *)auxdata;
    if (strequ(what, "read")) {
        long avail = ctx->in_size - ctx->in_pos;
        int to_read = (avail < (long)size) ? (int)avail : size;
        memcpy(buf, ctx->in_buf + ctx->in_pos, to_read);
        ctx->in_pos += to_read;
        return to_read;
    } else if (strequ(what, "write")) {
        while (ctx->out_pos + size > ctx->out_cap) {
            ctx->out_cap = ctx->out_cap * 2 + 65536;
            ctx->out_buf = (char *)realloc(ctx->out_buf, ctx->out_cap);
        }
        memcpy(ctx->out_buf + ctx->out_pos, buf, size);
        ctx->out_pos += size;
        return size;
    } else if (strequ(what, "time")) {
        return 0;
    }
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Compress the pipeline buffer in-place with a single method using streaming Compress().
// After success, pipeline buffer contains compressed data.
// Writes compressed size (>=0) or negative error code to *out_result.
void darc_pipeline_compress_step_w(const char *method, long *out_result) {
    if (!g_pipeline_buf || g_pipeline_size == 0) {
        *out_result = 0;
        return;
    }
    PipelineCtx ctx;
    ctx.in_buf = g_pipeline_buf;
    ctx.in_pos = 0;
    ctx.in_size = g_pipeline_size;
    ctx.out_cap = g_pipeline_size / 2 + 65536;
    ctx.out_buf = (char *)malloc(ctx.out_cap);
    ctx.out_pos = 0;

    int ret = Compress((char *)method, (CALLBACK_FUNC *)pipeline_callback, &ctx);

    free(g_pipeline_buf);
    if (ret >= 0) {
        g_pipeline_buf = ctx.out_buf;
        g_pipeline_size = ctx.out_pos;
        g_pipeline_cap = ctx.out_cap;
        *out_result = ctx.out_pos;
    } else {
        free(ctx.out_buf);
        g_pipeline_buf = NULL;
        g_pipeline_size = 0;
        g_pipeline_cap = 0;
        *out_result = (long)ret;
    }
}

// Decompress the pipeline buffer in-place with a single method using streaming Decompress().
// orig_size_hint is used as initial output buffer size (0 = auto-size).
// Writes decompressed size (>=0) or negative error code to *out_result.
void darc_pipeline_decompress_step_w(const char *method, long orig_size_hint, long *out_result) {
    if (!g_pipeline_buf || g_pipeline_size == 0) {
        *out_result = 0;
        return;
    }
    PipelineCtx ctx;
    ctx.in_buf = g_pipeline_buf;
    ctx.in_pos = 0;
    ctx.in_size = g_pipeline_size;
    ctx.out_cap = (orig_size_hint > 0) ? orig_size_hint + 65536 : g_pipeline_size * 4 + 65536;
    ctx.out_buf = (char *)malloc(ctx.out_cap);
    ctx.out_pos = 0;

    int ret = Decompress((char *)method, (CALLBACK_FUNC *)pipeline_callback, &ctx);

    free(g_pipeline_buf);
    if (ret >= 0) {
        g_pipeline_buf = ctx.out_buf;
        g_pipeline_size = ctx.out_pos;
        g_pipeline_cap = ctx.out_cap;
        *out_result = ctx.out_pos;
    } else {
        free(ctx.out_buf);
        g_pipeline_buf = NULL;
        g_pipeline_size = 0;
        g_pipeline_cap = 0;
        *out_result = (long)ret;
    }
}

// ============================================================
// Full solid-block compression: read files + CRC + compress + write
// Bypasses ALL Haskell iteration for the data-intensive hot path.
// ============================================================

// Read multiple files from disk into the pipeline buffer, computing CRC.
// Per-file CRCs are stored in out_crcs[] array.
// Returns 0 on success, -1 on file open error (sets out_failed_file_idx).
static int pipeline_read_files(
    const char **input_files, int num_files,
    unsigned int *out_crcs, long *out_orig_size,
    int *out_failed_file_idx)
{
    long total = 0;
    for (int i = 0; i < num_files; i++) {
        FILE *f = fopen(input_files[i], "rb");
        if (!f) {
            *out_failed_file_idx = i;
            return -1;
        }
        unsigned int crc = INIT_CRC;
        char readbuf[256*1024];  // 256KB read buffer
        for (;;) {
            size_t n = fread(readbuf, 1, sizeof(readbuf), f);
            if (n == 0) break;
            crc = UpdateCRC(readbuf, (uint)n, crc);
            darc_pipeline_append(readbuf, (long)n);
            total += (long)n;
        }
        fclose(f);
        out_crcs[i] = crc ^ INIT_CRC;  // finishCRC
    }
    *out_orig_size = total;
    return 0;
}

// --- Pipelined streaming solid-block compression --------------------------
// Three-stage pipeline inspired by FreeArc 0.67 MTCompressor:
//   reader thread  -> ring of filled buffers  -> Compress() (main thread)
//   Compress write -> ring of output buffers  -> writer thread (darc_bfile_write)
// Overlapping disk read / compression / disk write closes the gap vs. upstream.

#define STREAM_RING_SLOTS   6
#define STREAM_BUF_SIZE     (4 * 1024 * 1024)     // 4 MB per buffered block

typedef struct {
    char *data;
    int   size;      // bytes in buffer (0 marks EOF on a reader slot)
} StreamBuf;

typedef struct {
    // Ring of slots (simple bounded producer/consumer queue)
    StreamBuf      slots[STREAM_RING_SLOTS];
    int            head;
    int            tail;
    int            count;
    pthread_mutex_t mu;
    pthread_cond_t  cv_not_full;
    pthread_cond_t  cv_not_empty;
    int            closed;   // producer set this after final push
} StreamQueue;

typedef struct {
    // --- Input side (reader thread) ---
    const char **input_files;
    int          num_files;
    unsigned int *out_crcs;
    long         total_read;
    unsigned int block_crc;
    int          reader_err;

    // --- Output side (writer thread) ---
    void        *archive_bfile;
    long         total_written;
    int          writer_err;

    StreamQueue  in_q;        // reader -> compressor
    StreamQueue  out_q;       // compressor -> writer

    // --- Compressor-callback scratch (main thread only) ---
    StreamBuf    in_cur;      // currently-consumed input buffer
    int          in_cur_pos;
} PipelineCtx2;

static void sq_init(StreamQueue *q) {
    q->head = q->tail = q->count = 0;
    q->closed = 0;
    pthread_mutex_init(&q->mu, NULL);
    pthread_cond_init(&q->cv_not_full, NULL);
    pthread_cond_init(&q->cv_not_empty, NULL);
    for (int i = 0; i < STREAM_RING_SLOTS; i++) {
        q->slots[i].data = NULL;
        q->slots[i].size = 0;
    }
}
static void sq_destroy(StreamQueue *q) {
    pthread_mutex_destroy(&q->mu);
    pthread_cond_destroy(&q->cv_not_full);
    pthread_cond_destroy(&q->cv_not_empty);
}
static void sq_push(StreamQueue *q, StreamBuf buf) {
    pthread_mutex_lock(&q->mu);
    while (q->count == STREAM_RING_SLOTS)
        pthread_cond_wait(&q->cv_not_full, &q->mu);
    q->slots[q->tail] = buf;
    q->tail = (q->tail + 1) % STREAM_RING_SLOTS;
    q->count++;
    pthread_cond_signal(&q->cv_not_empty);
    pthread_mutex_unlock(&q->mu);
}
static void sq_close(StreamQueue *q) {
    pthread_mutex_lock(&q->mu);
    q->closed = 1;
    pthread_cond_broadcast(&q->cv_not_empty);
    pthread_mutex_unlock(&q->mu);
}
// Returns a buffer with size>0 on data, size==0 on clean EOF, size<0 on error code.
static StreamBuf sq_pop(StreamQueue *q) {
    pthread_mutex_lock(&q->mu);
    while (q->count == 0 && !q->closed)
        pthread_cond_wait(&q->cv_not_empty, &q->mu);
    StreamBuf b;
    if (q->count == 0) {
        b.data = NULL; b.size = 0;
    } else {
        b = q->slots[q->head];
        q->head = (q->head + 1) % STREAM_RING_SLOTS;
        q->count--;
        pthread_cond_signal(&q->cv_not_full);
    }
    pthread_mutex_unlock(&q->mu);
    return b;
}

// Reader thread: walks input_files, reads into STREAM_BUF_SIZE chunks,
// updates per-file CRCs, pushes filled buffers onto in_q.
// Block CRC is not computed: darc_compress_solid_block_w is only called for
// DATA blocks where the Haskell side discards it — computing it would double
// the CRC cost on the hot path.
static void *pipeline_reader_thread(void *arg) {
    PipelineCtx2 *ctx = (PipelineCtx2 *)arg;
    for (int i = 0; i < ctx->num_files; i++) {
        FILE *f = fopen(ctx->input_files[i], "rb");
        if (!f) {
            ctx->out_crcs[i] = 0;
            continue;
        }
        unsigned int file_crc = INIT_CRC;
        for (;;) {
            char *buf = (char *)malloc(STREAM_BUF_SIZE);
            if (!buf) { ctx->reader_err = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; fclose(f); goto done; }
            size_t n = fread(buf, 1, STREAM_BUF_SIZE, f);
            if (n == 0) { free(buf); break; }
            file_crc = UpdateCRC(buf, (uint)n, file_crc);
            ctx->total_read += (long)n;
            StreamBuf sb; sb.data = buf; sb.size = (int)n;
            sq_push(&ctx->in_q, sb);
        }
        ctx->out_crcs[i] = file_crc ^ INIT_CRC;
        fclose(f);
    }
done:
    ctx->block_crc = 0;  // unused on DATA block hot path
    sq_close(&ctx->in_q);
    return NULL;
}

// Writer thread: pops compressed buffers and flushes them to the archive BFILE.
static void *pipeline_writer_thread(void *arg) {
    PipelineCtx2 *ctx = (PipelineCtx2 *)arg;
    for (;;) {
        StreamBuf b = sq_pop(&ctx->out_q);
        if (!b.data) break;   // closed & drained
        long w = darc_bfile_write(ctx->archive_bfile, b.data, b.size);
        if (w < b.size) ctx->writer_err = FREEARC_ERRCODE_IO;
        else            ctx->total_written += w;
        free(b.data);
    }
    return NULL;
}

// Compress() callback (main thread) — pulls from in_q, pushes to out_q.
static int pipeline2_callback(const char *what, void *buf, int size, void *auxdata) {
    PipelineCtx2 *ctx = (PipelineCtx2 *)auxdata;
    if (strequ(what, "read")) {
        int total = 0;
        while (total < size) {
            if (!ctx->in_cur.data || ctx->in_cur_pos >= ctx->in_cur.size) {
                if (ctx->in_cur.data) { free(ctx->in_cur.data); ctx->in_cur.data = NULL; }
                StreamBuf b = sq_pop(&ctx->in_q);
                if (!b.data) break;           // EOF
                ctx->in_cur     = b;
                ctx->in_cur_pos = 0;
            }
            int avail = ctx->in_cur.size - ctx->in_cur_pos;
            int want  = size - total;
            int take  = avail < want ? avail : want;
            memcpy((char *)buf + total, ctx->in_cur.data + ctx->in_cur_pos, take);
            ctx->in_cur_pos += take;
            total           += take;
        }
        return total;
    } else if (strequ(what, "write")) {
        if (size <= 0) return size;
        char *copy = (char *)malloc(size);
        if (!copy) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
        memcpy(copy, buf, size);
        StreamBuf sb; sb.data = copy; sb.size = size;
        sq_push(&ctx->out_q, sb);
        return size;
    } else if (strequ(what, "time")) {
        return 0;
    }
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Storing drive (no Compress call): the main thread just forwards pulled
// buffers straight to the output queue, still keeping read/write overlap.
static int pipeline2_storing(PipelineCtx2 *ctx) {
    for (;;) {
        StreamBuf b = sq_pop(&ctx->in_q);
        if (!b.data) return 0;
        sq_push(&ctx->out_q, b);   // ownership transfers to writer
    }
}

// Compress a solid block: read files from disk, compress through method chain,
// write result to archive BFILE. Streams when possible; falls back to the
// buffered pipeline for multi-method chains.
void darc_compress_solid_block_w(
    const char **input_files,
    int          num_files,
    void        *archive_bfile,
    const char **methods,
    int          num_methods,
    long        *out_compressed_size,
    unsigned int*out_crcs,
    long        *out_orig_size,
    unsigned int*out_block_crc,
    int         *out_result,
    int         *out_failed_file_idx)
{
    *out_result = 0;
    *out_compressed_size = 0;
    *out_orig_size = 0;
    *out_block_crc = 0;
    *out_failed_file_idx = -1;

    // Pre-validate every file can be opened so the caller still gets a clean
    // out_failed_file_idx on missing sources (same contract as the buffered path).
    for (int i = 0; i < num_files; i++) {
        FILE *f = fopen(input_files[i], "rb");
        if (!f) {
            *out_failed_file_idx = i;
            *out_result = -1;
            return;
        }
        fclose(f);
    }

    // Pipelined fast path: 0 or 1 methods. Multi-method chains would require
    // per-stage thread+pipe infrastructure; fall back to the buffered path.
    if (num_methods <= 1) {
        PipelineCtx2 ctx;
        memset(&ctx, 0, sizeof(ctx));
        ctx.input_files   = input_files;
        ctx.num_files     = num_files;
        ctx.out_crcs      = out_crcs;
        ctx.archive_bfile = archive_bfile;
        ctx.block_crc     = INIT_CRC;
        sq_init(&ctx.in_q);
        sq_init(&ctx.out_q);

        pthread_t r_tid, w_tid;
        pthread_create(&r_tid, NULL, pipeline_reader_thread, &ctx);
        pthread_create(&w_tid, NULL, pipeline_writer_thread, &ctx);

        int ret;
        if (num_methods == 0) {
            ret = pipeline2_storing(&ctx);
        } else {
            ret = Compress((char *)methods[0],
                           (CALLBACK_FUNC *)pipeline2_callback, &ctx);
            if (ctx.in_cur.data) { free(ctx.in_cur.data); ctx.in_cur.data = NULL; }
        }
        sq_close(&ctx.out_q);

        pthread_join(r_tid, NULL);
        pthread_join(w_tid, NULL);

        // Drain any leftover input buffers (error path) to avoid leaks.
        for (;;) {
            pthread_mutex_lock(&ctx.in_q.mu);
            if (ctx.in_q.count == 0) { pthread_mutex_unlock(&ctx.in_q.mu); break; }
            StreamBuf b = ctx.in_q.slots[ctx.in_q.head];
            ctx.in_q.head = (ctx.in_q.head + 1) % STREAM_RING_SLOTS;
            ctx.in_q.count--;
            pthread_mutex_unlock(&ctx.in_q.mu);
            free(b.data);
        }
        sq_destroy(&ctx.in_q);
        sq_destroy(&ctx.out_q);

        *out_orig_size       = ctx.total_read;
        *out_compressed_size = ctx.total_written;
        *out_block_crc       = ctx.block_crc;  // always 0 on streaming path (DATA blocks only)
        if (ret < 0)            *out_result = ret;
        else if (ctx.reader_err) *out_result = ctx.reader_err;
        else if (ctx.writer_err) *out_result = ctx.writer_err;
        return;
    }

    // --- Buffered multi-method fallback (original path) -------------------
    darc_pipeline_init(64 * 1024 * 1024);
    int rc = pipeline_read_files(input_files, num_files,
                                  out_crcs, out_orig_size,
                                  out_failed_file_idx);
    if (rc < 0) {
        darc_pipeline_free();
        *out_result = -1;
        return;
    }
    if (g_pipeline_buf && g_pipeline_size > 0) {
        *out_block_crc = UpdateCRC(g_pipeline_buf, (uint)g_pipeline_size, INIT_CRC) ^ INIT_CRC;
    }
    for (int i = 0; i < num_methods; i++) {
        long step_result;
        darc_pipeline_compress_step_w(methods[i], &step_result);
        if (step_result < 0) {
            darc_pipeline_free();
            *out_result = (int)step_result;
            return;
        }
    }
    if (g_pipeline_buf && g_pipeline_size > 0) {
        long written = darc_bfile_write(archive_bfile, g_pipeline_buf, g_pipeline_size);
        *out_compressed_size = written;
    }
    darc_pipeline_free();
}

// Extract a solid block: read from archive, decompress, write files to disk.
//
// archive_bfile:  BFILE* for input archive
// block_comp_size: compressed size to read
// methods:        decompression method chain (already in correct order for decompress)
// num_methods:    number of methods
// output_files:   paths for output files
// file_offsets:   byte offset of each file within decompressed stream
// file_sizes:     size of each file
// num_files:      number of files to extract
// out_crcs:       computed CRC for each extracted file
// out_result:     0 = success, negative = error
void darc_extract_solid_block_w(
    void        *archive_bfile,
    long         block_comp_size,
    const char **methods,
    int          num_methods,
    const char **output_files,
    const long  *file_offsets,
    const long  *file_sizes,
    int          num_files,
    unsigned int*out_crcs,
    int         *out_result)
{
    *out_result = 0;

    // Phase 1: Read compressed data from archive into pipeline buffer
    darc_pipeline_init(block_comp_size + 65536);
    char *readbuf = (char *)malloc(block_comp_size > 0 ? block_comp_size : 1);
    long n = darc_bfile_read(archive_bfile, readbuf, block_comp_size);
    if (n > 0) {
        darc_pipeline_append(readbuf, n);
    }
    free(readbuf);

    // Phase 2: Decompress through method chain
    for (int i = 0; i < num_methods; i++) {
        long step_result;
        darc_pipeline_decompress_step_w(methods[i], 0, &step_result);
        if (step_result < 0) {
            *out_result = (int)step_result;
            return;
        }
    }

    // Phase 3: Write decompressed files to disk
    char *decomp_data = g_pipeline_buf;
    long  decomp_size = g_pipeline_size;

    for (int i = 0; i < num_files; i++) {
        long offset = file_offsets[i];
        long fsize  = file_sizes[i];

        // Validate bounds
        if (offset + fsize > decomp_size) {
            *out_result = -2;  // data truncated
            break;
        }

        // Compute CRC
        out_crcs[i] = UpdateCRC(decomp_data + offset, (uint)fsize, INIT_CRC) ^ INIT_CRC;

        // Write file
        if (output_files[i] && output_files[i][0]) {
            BuildPathTo((CFILENAME)output_files[i]);
            FILE *f = fopen(output_files[i], "wb");
            if (!f) {
                *out_result = -3;  // can't create file
                break;
            }
            if (fsize > 0) {
                size_t w = fwrite(decomp_data + offset, 1, (size_t)fsize, f);
                if ((long)w != fsize) {
                    fclose(f);
                    *out_result = -4;  // write error
                    break;
                }
            }
            fclose(f);
        }
    }
    darc_pipeline_free();
}

#ifdef __cplusplus
}
#endif


// ============================================================
// MicroHs callback-wrapper support
// Since MicroHs cannot create C function pointers from Haskell
// closures, we use a global slot mechanism. The Haskell side
// stores a "packed" read/write pair in these globals before
// calling the C compression function, which uses
// darc_cb_read / darc_cb_write as its callback functions.
// ============================================================
#ifdef __cplusplus
extern "C" {
#endif

// Slot-0 callback: stores a C function pointer + opaque data
// These are set by darc_set_callback and called by darc_invoke_callback.
typedef int (*DarcCallback)(const char *what, char *buf, int size, void *auxdata);
static DarcCallback g_darc_cb[16] = {};
static void        *g_darc_cb_data[16] = {};

void darc_set_callback(int slot, void *fn, void *data) {
    if (slot >= 0 && slot < 16) {
        g_darc_cb[slot]      = (DarcCallback)fn;
        g_darc_cb_data[slot] = data;
    }
}

int darc_invoke_callback(int slot, const char *what, char *buf, int size) {
    if (slot >= 0 && slot < 16 && g_darc_cb[slot])
        return g_darc_cb[slot](what, buf, size, g_darc_cb_data[slot]);
    return -1;
}

#ifdef __MHS__
// MicroHs: forward-declare the Haskell-exported callback function so that
// darc_get_haskell_callback_ptr can return its address at link time.
// darc_haskell_callback is generated by MicroHs from the 'foreign export ccall' declaration
// in CompressionLib.hs.  The exact C signature uses intptr_t because MicroHs maps CInt -> Int.
#include <stdint.h>
extern intptr_t darc_haskell_callback(void *cwhat, void *buf, intptr_t size, void *auxdata);

void *darc_get_haskell_callback_ptr(void) {
    return (void *)&darc_haskell_callback;
}
#endif

#ifdef __cplusplus
}
#endif
