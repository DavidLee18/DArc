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
void SetFileDateTime (const CFILENAME Filename, time_t t); // ���������� �����/���� ����������� �����
void RunProgram (const CFILENAME filename, const CFILENAME curdir, int wait_finish);  // Execute program `filename` in the directory `curdir` optionally waiting until it finished
void RunFile    (const CFILENAME filename, const CFILENAME curdir, int wait_finish);  // Execute file `filename` in the directory `curdir` optionally waiting until it finished
int long_path_size (void);                                 // ������������ ����� ����� �����
void FormatDateTime (char *buf, int bufsize, time_t t);    // ��������������� �����/���� ��� ������� ��������
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // ������� ��� ������������ ����� ���������
unsigned GetPhysicalMemory (void);                         // ����� ���������� ������ ����������
unsigned GetMaxMemToAlloc (void);                          // ����. ����� ������ ������� �� ����� �������� � �������� ������������ ������ ��������
unsigned GetAvailablePhysicalMemory (void);                // ����� ��������� ���������� ������ ����������
void TestMalloc (void);                                    // �������� ���������� ��������� ������
int GetProcessorsCount (void);                             // ����� ���������� ����������� (������, ���������� ����) � �������. ������������ ��� ����������� ����, ������� "������" �������������� ������� ������������� ��������� � ���������
uint UpdateCRC (void *Addr, uint Size, uint StartCRC);     // �������� CRC ���������� ����� ������
uint CalcCRC (void *Addr, uint Size);                      // ��������� CRC ����� ������
void memxor (char *dest, char *src, uint size);            // ��-xor-��� ��� ����� ������
int systemRandomData (char *rand_buf, int rand_size);
void BuildPathTo (CFILENAME name);                         // ������� �������� �� ���� � name

// GuiEnvironment.cpp
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename);                      // ���� ������������ ������� �������
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename);        // ���� ������������ ������� ����
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format);  // ���������� �����/���� ����� � ������ � ������������ � ����������� locale ��� ��������� ��������� ������� � ����

#ifdef __MHS__
// MicroHs callback trampoline: returns address of darc_haskell_callback for use as FunPtr CALLBACK_FUNC.
void *darc_get_haskell_callback_ptr(void);
#endif

#ifdef  __cplusplus
}
#endif
