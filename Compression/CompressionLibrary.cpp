#include "Compression.h"
#include "MultiThreading.h"
#include "LZMA/Windows/Synchronization.cpp"

// Decompress data with the given compression method and return the running time in seconds
int timed_decompress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  //SET_JMP_POINT( FREEARC_ERRCODE_GENERAL);
  double time0 = GetThreadCPUTime();
  int result = compressor->decompress (callback, auxdata);
  double time1 = GetThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Decompress data with the given compression method
int Decompress (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = timed_decompress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Read the compression method designation from the input stream and decompress the data with that method
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata)
{
  char method [MAX_METHOD_STRLEN];
  for (int i=0; i<MAX_METHOD_STRLEN; i++)
  {
    // Read the input data character by character until we read the end-of-line character
    callback ("read", &method[i], 1, auxdata);
    if (method[i]=='\0')
      return Decompress (method, callback, auxdata);
  }
  return FREEARC_ERRCODE_INVALID_COMPRESSOR;  // We get here if no '\0' character was found in the first MAX_METHOD_STRLEN characters of the input data
}

// Read/write callback function for (de)compression in memory
void *readPtr;    // current position of the data being read
int   readLeft;   // how many bytes are still left in the input buffer
void *writePtr;   // current position of the data being written
int   writeLeft;  // how many bytes are still left in the output buffer
int ReadWriteMem (const char *what, void *buf, int size, void *callback)
{
  if (strequ(what,"read")) {
    int read_bytes = readLeft<size ? readLeft : size;
    memcpy (buf, readPtr, read_bytes);
    readPtr   = (uint8*)readPtr+read_bytes;
    readLeft -= read_bytes;
    return read_bytes;
  } else if (strequ(what,"write")) {
    if (size>writeLeft)  return FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
    memcpy (writePtr, buf, size);
    writePtr   = (uint8*)writePtr+size;
    writeLeft -= size;
    return size;
  } else {
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
  }
}

// Decompress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int DecompressMem (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = Decompress (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Decompress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int DecompressMemWithHeader (void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = DecompressWithHeader (ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compress data with the given compression method and return the running time in seconds
int timed_compress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  //SET_JMP_POINT( FREEARC_ERRCODE_GENERAL);
  double time0 = GetThreadCPUTime();
  int result = compressor->compress (callback, auxdata);
  double time1 = GetThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Compress data with the given compression method
int Compress (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = timed_compress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Write the compression method designation to the output stream and compress the data with that method
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    char canonical_method [MAX_METHOD_STRLEN];
    compressor->ShowCompressionMethod (canonical_method);
    int result = callback ("write", canonical_method, strlen(canonical_method)+1, auxdata);
    if (result>=0) result = timed_compress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Compress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int CompressMem (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = Compress (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Compress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int CompressMemWithHeader (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = CompressWithHeader (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Output into canonical_method the canonical representation of the compression method in_method
int CanonizeCompressionMethod (char *method, char *canonical_method)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    compressor->ShowCompressionMethod (canonical_method);
    delete compressor;
    return FREEARC_OK;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}


#define Generate_Getter(GETTER)                                              \
  MemSize GETTER (char *method)                                              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);        \
    if (compressor){                                                         \
      MemSize bytes = compressor->GETTER();                                  \
      delete compressor;                                                     \
      return bytes;}                                                         \
    else                                                                     \
      return (MemSize)FREEARC_ERRCODE_INVALID_COMPRESSOR;                    \
  }                                                                          \

#define Generate_Setter(SETTER)                                              \
  int SETTER (char *in_method, MemSize bytes, char *out_method)              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (in_method);     \
    if (compressor){                                                         \
      compressor->SETTER (bytes);                                            \
      compressor->ShowCompressionMethod (out_method);                        \
      delete compressor;                                                     \
      return FREEARC_OK;}                                                    \
    else                                                                     \
      return FREEARC_ERRCODE_INVALID_COMPRESSOR;                             \
  }                                                                          \

// Information about the memory needed for compression/decompression, the dictionary size and the block size.
Generate_Getter(GetCompressionMem)
Generate_Getter(GetDecompressionMem)
Generate_Getter(GetDictionary)
Generate_Getter(GetBlockSize)

// Return in out_method a new compression method configured to use
// the corresponding amount of memory for compression/decompression, or dictionary/block size
Generate_Setter(SetCompressionMem)
Generate_Setter(SetDecompressionMem)
Generate_Setter(SetDictionary)
Generate_Setter(SetBlockSize)

// Return in out_method a new compression method, reducing if necessary
// the memory used by the algorithm / its dictionary / the block size
Generate_Setter(LimitCompressionMem)
Generate_Setter(LimitDecompressionMem)
Generate_Setter(LimitDictionary)
Generate_Setter(LimitBlockSize)

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Universal method. Parameters:
//   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
//   data: data for the operation, in a format depending on the specific operation performed
//   param&result: a simple numeric parameter, which is enough for many informational operations
// Unused parameters are set to NULL/0. result<0 means an error code
int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
       if (strequ (what, "encryption?"))           return 0;        // Is this an encryption algorithm?
  else if (strequ (what, "GetCompressionMem"))     return 0;        // Amount of memory needed for compression
  else if (strequ (what, "GetDecompressionMem"))   return 0;        // Amount of memory needed for decompression
  else                                             return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}


// ****************************************************************************************************************************
// Decompressing data compressed by a chain of methods                                                                        *
// ****************************************************************************************************************************

// Local data of a single method
struct Params
{
  CThread             thread;         // OS thread executing this (de)compression algorithm
  int                 thread_num;     // Number of method in chain (0..N-1)
  int                 threads_total;  // Total amount of methods in chain (N)
  CMETHOD             method;         // String denoting (de)compression method with its parameters
  CALLBACK_FUNC*      callback;       // Original callback (function that reads data in first method and write data in last one)
  void*               auxdata;        // Original callback parameter
  BYTE*               buf;            // Buffer that points to data sent from i'th thread to i+1'th
  int                 size;           // Amount of data in the buf
  CManualResetEvent*  done;           // Set when (de)compression is finished or error was found
  int*                retcode;        // Overall multi_decompress return code
  CCriticalSection*   retcode_cs;     // Ensure single-threaded access to retcode
  CSemaphore          read;
  CSemaphore          write;

  // Abort multi_decompress and set its exit code
  void SetExitCode (int code)
  {
    CCriticalSectionLock lock(*retcode_cs);
    if (*retcode == 0)  *retcode = code;   // Save into retcode first error code signalled (subsequent error codes may be sequels of the first one)
    done->Set();
  }
};
static DWORD WINAPI multi_decompress_thread (void *paramPtr);
static int multi_decompress_callback (const char *what, void *buf, int size, void *paramPtr);


// Decompress data compressed by a chain of methods
int MultiDecompress (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  // Split the compressor into individual algorithms and start a separate thread for each of them.
  //
  // On a COPY: split() overwrites each delimiter with '\0', so splitting the
  // caller's string in place truncates it to its first method permanently.
  // Unarc passes DIRECTORY_BLOCK::data_block[].compressor straight in, so a
  // failure reported afterwards named only the first method of the chain --
  // "unsupported compression method rep:60kb" when rep was fine and something
  // later in the chain was missing -- and a second decompression of the same
  // block would have used the truncated chain.
  char local_method[MAX_METHOD_STRLEN];
  strncopy (local_method, method, MAX_METHOD_STRLEN);
  CMETHOD cm[MAX_METHODS_IN_COMPRESSOR];
  Params  param[MAX_METHODS_IN_COMPRESSOR];
  int N = split (local_method, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);

  CManualResetEvent  done;           // Set when (de)compression is finished or error was found
  int                retcode = 0;    // multi_decompress return code
  CCriticalSection   retcode_cs;     // Ensure single-threaded access to retcode

  // Create semaphores for inter-thread communication
  for (int i=0; i<N; i++)
  {
    param[i].read .Create(0,1);
    param[i].write.Create(0,1);
  }
  // Start N threads
  for (int i=0; i<N; i++)
  {
    param[i].thread_num    = i;
    param[i].threads_total = N;
    param[i].method        = cm[N-1-i];    // when decompressing we run the movie backwards :D
    param[i].callback      = callback;
    param[i].auxdata       = auxdata;
    param[i].done          = &done;
    param[i].retcode       = &retcode;
    param[i].retcode_cs    = &retcode_cs;
    param[i].thread.Create (multi_decompress_thread, &param[i]);
  }

  done.Lock();    // wait for error or finish of last thread

  // Wait until all threads will be finished and return errcode or 0 at success
  for (int i=0; i<N; i++)
    param[i].thread.Wait();
    //printf("\nreleased %d    ", i);
  return retcode;
}


// A single decompression thread in multi_decompress
static DWORD WINAPI multi_decompress_thread (void *paramPtr)
{
  Params *param = (Params*) paramPtr;
  // Don't start this thread until the previous one has begun producing output (to save memory)
  if (param->thread_num > 0)
    param->read.Lock(),           // wait for permission to read (for data to appear in the buffer)
    param->read.Release();        // give the read permission back
  //printf("\nstarted %d    ", param->thread_num);
  int ret = Decompress (param->method, multi_decompress_callback, param);
  // Abort multi_decompress if decompress() returned error code
  if (ret<0)
    param->SetExitCode (ret);
  // Tell the previous thread that no more data required
  if (param->thread_num > 0)
    param[-1].size = -1,
    param[-1].write.Release();
  // Tell the next thread that no more data will be supplied to it
  if (param->thread_num < param->threads_total-1)
    param->size = -1,
    param[+1].read.Release();
  // If the last thread finished then no more data will be output, so we can finish multi_decompress
  if (param->thread_num == param->threads_total-1)
    param->SetExitCode(0);
  //printf("\nfinished %d    ", param->thread_num);
  return 0;
}


// Read/write callback function for multi_decompress_thread
static int multi_decompress_callback (const char *what, void *_buf, int size, void *paramPtr)
{
  Params *param = (Params*) paramPtr;
  BYTE *buf = (BYTE*)_buf;
  //printf("\n%s %d........  ", what, param->thread_num);

  // Writing data to the next thread
  if (strequ(what,"write")  &&  param->thread_num < param->threads_total-1)
  {
    param->buf  = buf;
    param->size = size;
    param[+1].read.Release();   // grant permission to read (data has appeared in the buffer)
    param->write.Lock();        // wait for permission to exit (after all the data has been read)
    //printf("\n%s %d -> %d  ", what, param->thread_num, param->size<0? -1 : size);
    return param->size<0? FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED : size;
  }

  // Reading data from the previous thread
  else if (strequ(what,"read")  &&  param->thread_num > 0)
  {
    int prev=0;
loop:
    //if (size==0)  return prev;
    param->read.Lock();             // wait for permission to read (for data to appear in the buffer)
    if (param[-1].size < 0)         // there will be no more data - the previous thread has finished
    {
      param->read.Release();        // give the read permission back
      //printf("\n%s %d -> %d  ", what, param->thread_num, prev);
      return prev;
    }
    else if (size <= param[-1].size)
    {
      memcpy (buf, param[-1].buf, size);
      param[-1].buf  += size;
      param[-1].size -= size;
      param->read.Release();        // give the read permission back
      //printf("\n%s %d -> %d  ", what, param->thread_num, prev+size);
      return prev+size;
    }
    else // param[-1].size < size
    {
      memcpy (buf, param[-1].buf, param[-1].size);
      buf  += param[-1].size;
      size -= param[-1].size;
      prev += param[-1].size;
      param[-1].write.Release();    // grant permission to leave the write (the buffer is empty)
      goto loop;
    }
  }

  // Reading in the first thread, writing in the last one,
  // as well as all requests unknown to science, are passed on to the original callback
  else
  {
    int n = param->callback (what, buf, size, param->auxdata);
    //printf("\n%s %d -> %d  ", what, param->thread_num, n);
    return n;
  }
}


// ****************************************************************************************************************************
// UTILITIES                                                                                                                  *
// ****************************************************************************************************************************

// Split a COMPRESSOR into individual compression/encryption algorithms
//void splitCompressor (COMPRESSOR c, ARRAY<CMETHOD> &cm)

// Request the service what from the compression method method
int CompressionService (char *method, char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = compressor->doit (what, param, data, callback);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Check whether the given compressor includes an encryption algorithm
int compressorIsEncrypted (COMPRESSOR c)
{
  // Split the compressor into individual algorithms and look for an encryption algorithm among them
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (c, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  for (CMETHOD *cm=arr; *cm; cm++)
    if (CompressionService (*cm, "encryption?") == 1)  return TRUE;
  return FALSE;
}

// Compute how much memory is needed to decompress data compressed by this compressor
MemSize compressorGetDecompressionMem (COMPRESSOR c)
{
  // Split the compressor into individual algorithms and sum up their memory requirements
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (c, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  MemSize sum=0;
  for (CMETHOD *cm=arr; *cm; cm++)
    sum += CompressionService (*cm, "GetDecompressionMem");
  return sum;
}


// Get/set number of threads used for (de)compression. 0 means "autodetect"
static int CompressionThreads = 1;
int  __cdecl GetCompressionThreads (void)         {return CompressionThreads;}
void __cdecl SetCompressionThreads (int threads)
{
  CompressionThreads = threads==0? 1 : threads;
#ifndef FREEARC_DLL
  static FARPROC f = LoadFromDLL ("SetCompressionThreads");
  // -Wcast-function-type-mismatch fires here and the diagnostic is correct:
  // calling through a function pointer of a different type is UB by the
  // letter of the standard. It is also unavoidable for dynamic symbol
  // lookup -- LoadFromDLL hands back a generic FARPROC and the real
  // signature is only known here. Routing the cast through void* would
  // silence the warning without removing the UB, which is worse than
  // leaving it visible. Left deliberately; every real ABI defines it.
  if (f)  ((void (__cdecl *)(int)) f) (threads);
#endif
}


// Load accelerated function from facompress.dll
FARPROC LoadFromDLL (char *funcname)
{
#ifdef FREEARC_WIN  // Non-Windows platforms aren't yet supported
  static bool loaded = FALSE;
  static HMODULE dll = NULL;

  if (!loaded)
  {
    loaded = TRUE;
    dll = LoadLibraryA("facompress.dll");
  }

  return GetProcAddress (dll, funcname);
#else
  return NULL;
#endif
}


// ****************************************************************************************************************************
// SUPPORT FOR THE TEMPORARY FILE LIST AND DELETING THEM ON ABNORMAL PROGRAM EXIT *********************************************
// ****************************************************************************************************************************

// Table of temporary files that should be deleted on ^Break
static int TemporaryFilesCount=0;
static struct {char *name; FILE* file;}  TemporaryFiles[10];

void registerTemporaryFile (char *name, FILE* file)
{
  unregisterTemporaryFile (name);  // First, delete all existing registrations of the same file
  TemporaryFiles[TemporaryFilesCount].name = name;
  TemporaryFiles[TemporaryFilesCount].file = file;
  TemporaryFilesCount++;
}

void unregisterTemporaryFile (char *name)
{
  iterate_var(i,TemporaryFilesCount)
    if (strequ (TemporaryFiles[i].name, name))
    {
      memmove (TemporaryFiles+i, TemporaryFiles+i+1, (TemporaryFilesCount-(i+1)) * sizeof(TemporaryFiles[i]));
      TemporaryFilesCount--;
      return;
    }
}

// This function cleans up the Compression Library
void compressionLib_cleanup (void)
{
  iterate_var(i,TemporaryFilesCount)
    TemporaryFiles[i].file!=NULL  &&  fclose (TemporaryFiles[i].file),
    remove (TemporaryFiles[i].name);
}


// ****************************************************************************************************************************
// SUPPORT FOR THE REGISTERED COMPRESSION METHOD PARSER TABLE AND LOOKUP OF A SPECIFIC METHOD IMPLEMENTATION IN IT            *
// ****************************************************************************************************************************

template <class PARSER>
struct Parser
{
  PARSER  parser;
  void*   data;
};


int cmCount = 0;                                       // Number of registered compression methods
Parser<CM_PARSER>  cmTable[MAX_COMPRESSION_METHODS];   // Table into which all registered compression method parsers are recorded

// Add a new method to the list of supported compression methods
int AddCompressionMethod (CM_PARSER parser)
{
  CHECK (cmCount < elements(cmTable), (s,"INTERNAL ERROR: Overflow of compression methods table"));
  cmTable[cmCount++].parser = parser;
  return 0;
}


int cmExternalCount = 0;                                       // Number of registered external compression methods
Parser<CM_PARSER2> cmExternalTable[MAX_COMPRESSION_METHODS];   // Table into which all registered external compression method parsers are recorded

// Clear the table of external compressors
void ClearExternalCompressorsTable (void)
{
  static int builtins = -1;  if (builtins<0)  builtins=cmExternalCount;
  cmExternalCount = builtins;  // Keep only the built-in descriptions of external compressors
}

// Add a method parser together with an extra parameter that should be passed to that parser
int AddExternalCompressionMethod (CM_PARSER2 parser, void *data)
{
  CHECK (cmExternalCount < elements(cmExternalTable), (s,"INTERNAL ERROR: Overflow of external compression methods table"));
  cmExternalTable[cmExternalCount].parser = parser;
  cmExternalTable[cmExternalCount].data   = data;
  cmExternalCount++;
  return 0;
}


// Construct a COMPRESSION_METHOD object implementing the method given as the string `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method)
{
  // Turn the compression method string into the string array `parameters` holding its name and parameters
  char* parameters [MAX_PARAMETERS];
  char  local_method [MAX_METHOD_STRLEN];
  strncopy (local_method, method, sizeof (local_method));
  split (local_method, COMPRESSION_METHOD_PARAMETERS_DELIMITER, parameters, MAX_PARAMETERS);

  // Iterate over all registered compression method parsers and find the one that can recognize `parameters`
  iterate_var (i, cmExternalCount)  {
     COMPRESSION_METHOD *m = (*cmExternalTable[i].parser) (parameters, cmExternalTable[i].data);
     if (m)  return m;
  }
  iterate_var (i, cmCount)  {
     COMPRESSION_METHOD *m = (*cmTable[i].parser) (parameters);
     if (m)  return m;
  }
  return NULL;   // The given compression method was not recognized by any of the parsers
}


// ***********************************************************************************************************************
// Implementation of the STORING class                                                                                   *
// ***********************************************************************************************************************

// The "(de)compression" function that copies data verbatim
int copy_data (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0) {
    if ((len = callback ("write", buf, len, auxdata)) < 0)  break;
  }
  return len;
}

// Decompression function
int STORING_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int STORING_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] the string describing the compression method (inverse of parse_STORING)
void STORING_METHOD::ShowCompressionMethod (char *buf)
{
  sprintf (buf, "storing");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an object of type STORING_METHOD or returns NULL if this is a different compression method
COMPRESSION_METHOD* parse_STORING (char** parameters)
{
  if (strcmp (parameters[0], "storing") == 0
      &&  parameters[1]==NULL )
    // If the method name is "storing" and it has no parameters, then this is our method
    return new STORING_METHOD;
  else
    return NULL;   // This is not the storing method
}

static int STORING_x = AddCompressionMethod (parse_STORING);   // Register the STORING_METHOD parser

