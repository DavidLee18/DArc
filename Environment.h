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
long darc_urandom_read (void *buf, long size);
void BuildPathTo (CFILENAME name);                         // ������� �������� �� ���� � name

// GuiEnvironment.cpp
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename);                      // ���� ������������ ������� �������
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename);        // ���� ������������ ������� ����
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format);  // ���������� �����/���� ����� � ������ � ������������ � ����������� locale ��� ��������� ��������� ������� � ����

// MHS C-side compression/decompression pipeline
void darc_pipeline_init(long initial_cap);
void darc_pipeline_append(const void *data, long len);
void darc_pipeline_get_buf_w(void **out_buf, long *out_size);
void darc_pipeline_free(void);
void darc_pipeline_compress_step_w(const char *method, long *out_result);
void darc_pipeline_decompress_step_w(const char *method, long orig_size_hint, long *out_result);
// Full solid-block C hot path
void darc_compress_solid_block_w(
    const char **input_files, int num_files, void *archive_bfile,
    const char **methods, int num_methods,
    long *out_compressed_size, unsigned int *out_crcs,
    long *out_orig_size, unsigned int *out_block_crc,
    int *out_result, int *out_failed_file_idx);
void darc_extract_solid_block_w(
    void *archive_bfile, long block_comp_size,
    const char **methods, int num_methods,
    const char **output_files, const long *file_offsets, const long *file_sizes,
    int num_files, unsigned int *out_crcs, int *out_result);

#ifdef __MHS__
// MicroHs callback trampoline: returns address of darc_haskell_callback for use as FunPtr CALLBACK_FUNC.
void *darc_get_haskell_callback_ptr(void);
#endif

// FreeArc 0.67 --shutdown / -ioff: power off the machine after archive op.
void PowerOffComputer(void);

#ifdef  __cplusplus
}
#endif
