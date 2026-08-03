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


// Adjust the RTS settings, enabling the compacting GC starting from 40 mb:
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
    printf("Allocated %4zu mb, addr=%p\n", (size_t)(size>>20), p);
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
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
// sys/sysinfo.h and get_nprocs() are glibc extensions. The BSDs expose the
// same facts through sysctl and sysconf.
#include <sys/sysctl.h>
#else
#include <sys/sysinfo.h>
#endif

// These return `unsigned`, which is 32 bits, so anything at or above 4 GB
// cannot be represented. Saturate rather than truncate: reporting a 16 GB
// machine as having 379 MB (the low bits of 16000000000) would make the
// memory-limit arithmetic downstream produce nonsense, whereas reporting
// 4 GB - 1 merely makes it conservative.
static unsigned saturate_to_unsigned (unsigned long long bytes)
{
  return bytes > (unsigned long long)UINT_MAX ? UINT_MAX : (unsigned)bytes;
}

unsigned GetPhysicalMemory (void)
{
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  uint64_t memsize = 0;
  size_t   len     = sizeof(memsize);
#if defined(__APPLE__)
  if (sysctlbyname("hw.memsize", &memsize, &len, NULL, 0) != 0)  return 0;
#else
  if (sysctlbyname("hw.physmem", &memsize, &len, NULL, 0) != 0)  return 0;
#endif
  return saturate_to_unsigned(memsize);
#else
  struct sysinfo si;
    if (sysinfo(&si) != 0)  return 0;
  return saturate_to_unsigned((unsigned long long)si.totalram * si.mem_unit);
#endif
}

unsigned GetMaxMemToAlloc (void)
{
  //struct sysinfo si;
  //  sysinfo(&si);
  return UINT_MAX;
}

unsigned GetAvailablePhysicalMemory (void)
{
#if defined(__APPLE__)
  // macOS does not define _SC_AVPHYS_PAGES. vm.page_free_count is the nearest
  // equivalent. It understates what is usable, because macOS keeps most of RAM
  // in reclaimable cache rather than "free", so the resulting memory limit is
  // conservative rather than optimistic -- the safe direction to be wrong in.
  unsigned free_pages = 0;
  size_t   len        = sizeof(free_pages);
  if (sysctlbyname("vm.page_free_count", &free_pages, &len, NULL, 0) != 0)  return 0;
  long pagesize = sysconf(_SC_PAGESIZE);
  if (pagesize <= 0)  return 0;
  return saturate_to_unsigned((unsigned long long)free_pages * (unsigned long long)pagesize);
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  long pages    = sysconf(_SC_AVPHYS_PAGES);
  long pagesize = sysconf(_SC_PAGESIZE);
  if (pages <= 0 || pagesize <= 0)  return 0;
  return saturate_to_unsigned((unsigned long long)pages * (unsigned long long)pagesize);
#else
  struct sysinfo si;
    if (sysinfo(&si) != 0)  return 0;
  return saturate_to_unsigned((unsigned long long)si.freeram * si.mem_unit);
#endif
}

int GetProcessorsCount (void)
{
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  return n > 0 ? (int)n : 1;
#else
  return get_nprocs();
#endif
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
  if (t==-1)  t=0;  // Otherwise we get a crash :(
  p = localtime(&t);
  strftime( buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}

// Maximum file name length
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

// Compute the CRC of a data block
uint CalcCRC( void *Addr, uint Size)
{
  return UpdateCRC (Addr, Size, INIT_CRC) ^ INIT_CRC;
}



// XOR two data blocks together
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}

// Return the file name without the directory name
FILENAME arc_basename (FILENAME fullname)
{
  char *p = fullname;
  for (char* q=fullname; *q; q++)
    if (in_set (*q, ALL_PATH_DELIMITERS))
      p = q+1;
  return p;
}

// Create the directories along the path to name
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

/* FreeArc 0.67 --shutdown / -ioff: power off the machine. */
extern "C" void PowerOffComputer(void) {
#ifdef FREEARC_WIN
    HANDLE hToken;
    TOKEN_PRIVILEGES tkp;
    if (!OpenProcessToken(GetCurrentProcess(),
            TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken)) return;
    LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);
    tkp.PrivilegeCount = 1;
    tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
    AdjustTokenPrivileges(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES)NULL, 0);
    ExitWindowsEx(EWX_POWEROFF | EWX_FORCE, 0);
#else
    int r = system("shutdown -h now");
    (void)r;
#endif
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

// The MicroHs compression pipeline, the volume split/join helpers, the
// callback-slot trampoline and darc_get_haskell_callback_ptr used to follow
// here -- about 900 lines whose only callers were CompressionLib.hs and
// Files.hs. They went with the Haskell layer: darc-arc drives the codecs
// through its own read/write callbacks (rust/darc-arc/src/codec_io.rs) and
// splits volumes itself, and Unarc never called any of it.
