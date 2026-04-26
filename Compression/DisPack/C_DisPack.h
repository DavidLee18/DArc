#include "../Compression.h"

#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class DISPACK_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize BlockSize;        // ������ ����� ������, �������������� �� ���� ���
  int     ExtendedTables;   // ������ ������� � �������� ��������, �������� �� 2/4

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  DISPACK_METHOD();

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return 3*BlockSize+BlockSize/4+1024;}
  virtual void    SetCompressionMem        (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/13*4,64*kb);}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/ 9*4,64*kb);}
  virtual void    ShowCompressionMethod    (char *buf);
#endif
  virtual MemSize GetDecompressionMem      (void)               {return 2*BlockSize+BlockSize/4+1024;}

  // DArc COMPRESSION_METHOD API fill-ins (pure in DArc, absent in 0.67).
  virtual MemSize GetDictionary            (void)               {return 0;}
  virtual MemSize GetBlockSize             (void)               {return BlockSize;}
  virtual void    SetDecompressionMem      (MemSize mem)        {SetMinDecompressionMem(mem);}
  virtual void    SetDictionary            (MemSize)            {}
  virtual void    SetBlockSize             (MemSize bs)         {if (bs>0) BlockSize = bs;}
};

// ��������� ������ ������ ������ DISPACK
COMPRESSION_METHOD* parse_DISPACK (char** parameters);

#endif  // __cplusplus
