extern "C" {
#include "C_DisPack.h"
}

// Compatibility shims for macros that exist in FreeArc 0.67 but not in DArc.
#ifndef BIGALLOC
#define BIGALLOC(type, ptr, size)                                          \
{                                                                          \
    (ptr) = (type*) BigAlloc ((size) * sizeof(type));                      \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}
#endif

#ifndef READ_LEN
#define READ_LEN(len, buf, size)                                           \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) < 0) {      \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}
#endif

#ifndef BigFreeAndNil
#define BigFreeAndNil(p)         ((p) && (BigFree(p), (p)=NULL))
#endif

// Big-endian load/store helpers used by DisPack.cpp. Present in FreeArc 0.67
// Common.h but absent in DArc.
static inline uint16 value16b (void *p) {
  uint8 *m = (uint8 *)p; return (m[0] << 8) + m[1];
}
static inline uint32 value32b (void *p) {
  uint8 *m = (uint8 *)p;
  return (m[0] << 24) + (m[1] << 16) + (m[2] << 8) + m[3];
}
static inline void setvalue16b (void *p, uint32 x) {
  uint8 *m = (uint8 *)p; m[0] = x >> 8; m[1] = x;
}
static inline void setvalue32b (void *p, uint32 x) {
  uint8 *m = (uint8 *)p;
  m[0] = x >> 24; m[1] = x >> 16; m[2] = x >> 8; m[3] = x;
}

#define DISPACK_LIBRARY
#include "DisPack.cpp"

/*-------------------------------------------------*/
/* ���������� ������ DISPACK_METHOD                */
/*-------------------------------------------------*/

// �����������, ������������� ���������� ������ ������ �������� �� ���������
DISPACK_METHOD::DISPACK_METHOD()
{
    BlockSize      = 8*mb;
    ExtendedTables = 0;
}

enum {TAG_DATA = 0xC71B3AE1, TAG_EXE};
bool is_tag (unsigned x)  {return (x^TAG_DATA) < 0x10;}

// ������� ����������
int DISPACK_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // ��������� �� ������� � �������� ������, ��������������
    uint  BaseAddress = 1u<<30;
    int   CHUNK_SIZE, InBufferSize = BlockSize+BlockSize/4+1024;
    READ4_OR_EOF (CHUNK_SIZE);
    if (CHUNK_SIZE > BlockSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    BIGALLOC (BYTE, In,  InBufferSize+2);
    BIGALLOC (BYTE, Out, BlockSize+2);
    for(;;) {
        int tag;
        READ4_OR_EOF (tag);
        if (!is_tag(tag) || tag==TAG_DATA) {
            // ��������� ������������� ������, �� ��� 4 ����� �� ��� �������� ��������� ;)
            int done = 0, len;
            if (tag==TAG_DATA) {
              READ4 (len);
            } else {
              done = 4;
              len = CHUNK_SIZE;
              setvalue32 (In, tag);
            }
            READ  (In+done, len-done);
            WRITE (In, len);
            BaseAddress += len;
        } else if (tag==TAG_EXE) {
            int InSize, OutSize;     // ���������� ���� �� ������� � �������� ������, ��������������
            // ���������� ������������� � �������� ������ �������� ������
            READ4 (OutSize);
            READ4 (InSize);
            if (OutSize > BlockSize  ||  InSize > InBufferSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            READ (In, InSize);
            bool success = DisUnFilter (In, InSize, Out, OutSize, BaseAddress);
            if (!success)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            WRITE (Out, OutSize);
            BaseAddress += OutSize;
        } else {
            ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        }
        if (BaseAddress >= 3u<<30)  BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); BigFreeAndNil(Out);
    return errcode;
}

#ifndef FREEARC_DECOMPRESS_ONLY

enum EXETYPE {EXETYPE_UNKNOWN, EXETYPE_DATA, EXETYPE_EXE};

EXETYPE detect (BYTE *buf, int len)
{
  int e8=0, exe=0, obj=0;
  for (BYTE *p=buf; p+5<buf+len; p++)
  {
    if (*p == 0xE8)
    {
      e8++;
      if (p[4]==0xFF && p[5]!=0xFF)
        exe++;
      if (p[4]==0    && p[5]!=0)
        obj++;
    }
  }
  // printf("  e8 %d, exe %d, obj %d, len %d\n", e8, exe, obj, len);
  return double(e8)/len >= 0.002   &&   double(exe+obj)/e8 >= 0.20  &&   double(exe)/e8 >= 0.01?  EXETYPE_EXE : EXETYPE_DATA;
}

// ������� ��������
int DISPACK_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // ��������� �� ������� � �������� ������, ��������������
    int   InSize;  uint32 OutSize;  // ���������� ���� �� ������� � �������� ������, ��������������
    uint  BaseAddress = 1u<<30;
    const int CHUNK_SIZE = 16*kb;
    bool  first_time = TRUE;
    BIGALLOC (BYTE, In, BlockSize+2);
    for(;;)
    {
        // ������ ���� ������� �� 16 ��, ���� �� �������� ����������� ���
        BYTE *p = In;  int len;
        do {
            READ_LEN (len, p, CHUNK_SIZE);
            if (len==0) break;
            EXETYPE exe_type = detect (p, len);
            if (exe_type!=EXETYPE_EXE) break;
            p += len, len = 0;
        } while (p-In <= BlockSize-CHUNK_SIZE);

        InSize = p-In;
        if (InSize+len == 0)  break;
        if (first_time)   WRITE4 (CHUNK_SIZE);  first_time = FALSE;

        if (InSize)
        {
            // �������� ����������� ���
            Out = DisFilter(In, InSize, BaseAddress, OutSize);
            if (Out==NULL)  ReturnErrorCode(FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
            WRITE4 (TAG_EXE);
            WRITE4 (InSize);
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
            free (Out);
        }
        if (len)
        {
            // �������� ������ ������
            if (len!=CHUNK_SIZE  ||  is_tag(value32(p))) {
                WRITE4 (TAG_DATA);
                WRITE4 (len);
            }
            WRITE (p, len);
        }
        if ((BaseAddress += InSize+len)  >=  3u<<30)   BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); //BigFreeAndNil(Out);
    return errcode;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_DISPACK)
void DISPACK_METHOD::ShowCompressionMethod (char *buf)
{
    DISPACK_METHOD defaults; char BlockSizeStr[100]=":";
    showMem (BlockSize, BlockSizeStr+1);
    sprintf (buf, "dispack070%s%s", BlockSize!=defaults.BlockSize? BlockSizeStr:"", ExtendedTables? ":x":"");
}

// ������������ ������ ���� DISPACK_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_DISPACK (char** parameters)
{
  if (strcmp (parameters[0], "dispack") == 0
   || strcmp (parameters[0], "dispack070") == 0) {
    // ���� �������� ������ (������� ��������) - "dispack", �� ������� ��������� ���������

    DISPACK_METHOD *p = new DISPACK_METHOD;
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������

    // �������� ��� ��������� ������ (��� ������ ������ ��� ������������� ������ ��� ������� ���������� ���������)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strlen(param)==1) switch (*param) {    // ������������� ���������
        case 'x':  p->ExtendedTables = 1; continue;
      }
      switch (*param) {                    // ���������, ���������� ��������
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
      }
      // ���� �� ��������, ���� � ��������� �� ������� ��� ��������
      // ���� ���� �������� ������� ��������� ��� ����� ������,
      // �� �������� ��� �������� ���� BlockSize
      p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������
    return p;
  } else
    return NULL;   // ��� �� ����� DISPACK
}

static int DISPACK_x = AddCompressionMethod (parse_DISPACK);   // �������������� ������ ������ DISPACK

