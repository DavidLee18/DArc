// DArc 4x4: multithreaded block compression
// Adapted from FreeArc 0.67's 4x4 module, reimplemented with pthread for portability.
//
// Wire format:
//   version        : int32  (currently 0)
//   per block:
//     orig_size   : int32  (number of original bytes in block; -1 = raw stored)
//     comp_size   : int32  (number of bytes that follow)
//     payload     : comp_size bytes (compressed, unless orig_size==-1)
//   stream terminates with orig_size==0 at EOF (never written; reader detects EOF via read returning 0)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdint.h>

#include "C_4x4.h"
#include "../Compression.h"

#define _4x4_VERSION      0
#define _4x4_HEADER_SIZE  8

// Join remaining null-terminated parameter strings with delimiter into buf (max len bytes)
static void join_params (char **params, char delim, char *buf, int len)
{
  int pos = 0;
  for (int i = 0; params[i]; i++) {
    if (i > 0 && pos + 1 < len) buf[pos++] = delim;
    for (char *p = params[i]; *p && pos + 1 < len; p++) buf[pos++] = *p;
  }
  if (pos < len) buf[pos] = 0; else buf[len-1] = 0;
}

// -----------------------------------------------------------------------------
// Thread-safe bounded queue of void*
// -----------------------------------------------------------------------------
struct PQueue
{
  void **slots;
  int cap, head, tail, count;
  int closed;
  pthread_mutex_t mu;
  pthread_cond_t  cv_not_full, cv_not_empty;

  void init(int capacity)
  {
    cap = capacity; head = tail = count = 0; closed = 0;
    slots = (void**) malloc (sizeof(void*) * cap);
    pthread_mutex_init(&mu, NULL);
    pthread_cond_init(&cv_not_full, NULL);
    pthread_cond_init(&cv_not_empty, NULL);
  }
  void destroy()
  {
    free(slots); slots = NULL;
    pthread_mutex_destroy(&mu);
    pthread_cond_destroy(&cv_not_full);
    pthread_cond_destroy(&cv_not_empty);
  }
  void put(void *v)
  {
    pthread_mutex_lock(&mu);
    while (count == cap && !closed) pthread_cond_wait(&cv_not_full, &mu);
    slots[tail] = v; tail = (tail+1) % cap; count++;
    pthread_cond_signal(&cv_not_empty);
    pthread_mutex_unlock(&mu);
  }
  void *get()
  {
    pthread_mutex_lock(&mu);
    while (count == 0 && !closed) pthread_cond_wait(&cv_not_empty, &mu);
    void *v = NULL;
    if (count > 0) {
      v = slots[head]; head = (head+1) % cap; count--;
      pthread_cond_signal(&cv_not_full);
    }
    pthread_mutex_unlock(&mu);
    return v;
  }
  void close()
  {
    pthread_mutex_lock(&mu);
    closed = 1;
    pthread_cond_broadcast(&cv_not_empty);
    pthread_cond_broadcast(&cv_not_full);
    pthread_mutex_unlock(&mu);
  }
};

// One-shot event (set once, wait many)
struct PEvent
{
  int fired;
  pthread_mutex_t mu;
  pthread_cond_t  cv;

  void init()  { fired = 0; pthread_mutex_init(&mu, NULL); pthread_cond_init(&cv, NULL); }
  void destroy() { pthread_mutex_destroy(&mu); pthread_cond_destroy(&cv); }
  void signal() { pthread_mutex_lock(&mu); fired = 1; pthread_cond_broadcast(&cv); pthread_mutex_unlock(&mu); }
  void reset()  { pthread_mutex_lock(&mu); fired = 0; pthread_mutex_unlock(&mu); }
  void wait()   { pthread_mutex_lock(&mu); while (!fired) pthread_cond_wait(&cv, &mu); pthread_mutex_unlock(&mu); }
};

// -----------------------------------------------------------------------------
// Compression job
// -----------------------------------------------------------------------------
struct _4x4Job
{
  uint8_t *in_buf;       // input block buffer (one per job)
  uint8_t *out_buf;      // output block buffer (one per job)
  int      in_size;      // bytes of data in in_buf (0 = EOF sentinel)
  int      out_size;     // bytes of data in out_buf
  int      out_cap;      // capacity of out_buf
  int      stored_raw;   // 1 if block couldn't compress, stored raw
  int      result;       // return code
  PEvent   ready;        // signaled when compression/decompression done
};

// -----------------------------------------------------------------------------
// Per-job memory callback state (thread-safe: lives on each worker's stack)
// -----------------------------------------------------------------------------
struct MemCB
{
  uint8_t *in_ptr; int in_left;
  uint8_t *out_ptr; int out_left;
};

static int mem_callback (const char *what, void *buf, int size, void *aux)
{
  MemCB *s = (MemCB*)aux;
  if (strequ(what, "read")) {
    int n = s->in_left < size ? s->in_left : size;
    if (n > 0) memcpy (buf, s->in_ptr, n);
    s->in_ptr += n; s->in_left -= n;
    return n;
  }
  if (strequ(what, "write")) {
    if (size > s->out_left) return FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
    if (size > 0) memcpy (s->out_ptr, buf, size);
    s->out_ptr += size; s->out_left -= size;
    return size;
  }
  if (strequ(what, "quasiwrite") || strequ(what, "time") ||
      strequ(what, "progress")   || strequ(what, "init") || strequ(what, "done")) {
    return 0;
  }
  return 0;
}

// -----------------------------------------------------------------------------
// Shared context between reader, workers, writer
// -----------------------------------------------------------------------------
struct _4x4Ctx
{
  int             direction;    // 0 = compress, 1 = decompress
  char           *method;       // inner method (e.g. "tor:3")
  int             block_size;   // input block size (compress); reference for output buf alloc
  int             out_cap;      // per-block output buffer capacity
  CALLBACK_FUNC  *outer_cb;
  void           *outer_aux;

  int             num_workers;
  int             num_jobs;      // total jobs in pool (workers + extra buffering)
  _4x4Job        *jobs;
  PQueue          free_q;        // jobs with no data, available to reader
  PQueue          work_q;        // jobs read, pending worker
  PQueue          writer_q;      // jobs in read-order for writer
  int             err;           // first error encountered (0 = ok)
  pthread_mutex_t err_mu;

  pthread_t      *worker_threads;
  pthread_t       writer_thread;

  void set_err(int e) {
    pthread_mutex_lock(&err_mu);
    if (e < 0 && err == 0) err = e;
    pthread_mutex_unlock(&err_mu);
  }
};

// -----------------------------------------------------------------------------
// Worker thread: pops jobs, (de)compresses block, signals ready
// -----------------------------------------------------------------------------
static void *worker_thread (void *arg)
{
  _4x4Ctx *c = (_4x4Ctx*)arg;
  for (;;) {
    _4x4Job *job = (_4x4Job*) c->work_q.get();
    if (job == NULL) break;  // pool closed -> exit

    if (c->direction == 0) {
      // Compress block
#ifndef FREEARC_DECOMPRESS_ONLY
      MemCB mc;
      mc.in_ptr   = job->in_buf;
      mc.in_left  = job->in_size;
      mc.out_ptr  = job->out_buf;
      mc.out_left = job->out_cap;
      int r = Compress (c->method, mem_callback, &mc);
      if (r >= 0) {
        job->out_size   = job->out_cap - mc.out_left;
        job->stored_raw = 0;
        job->result     = FREEARC_OK;
        // If compression didn't help, store raw
        if (job->out_size >= job->in_size) {
          memcpy (job->out_buf, job->in_buf, job->in_size);
          job->out_size   = job->in_size;
          job->stored_raw = 1;
        }
      } else if (r == FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL) {
        memcpy (job->out_buf, job->in_buf, job->in_size);
        job->out_size   = job->in_size;
        job->stored_raw = 1;
        job->result     = FREEARC_OK;
      } else {
        job->result = r;
        c->set_err(r);
      }
#else
      job->result = FREEARC_ERRCODE_ONLY_DECOMPRESS;
      c->set_err(job->result);
#endif
    } else {
      // Decompress block
      if (job->stored_raw) {
        // raw: out_buf already points to raw data; copy to in_buf as "output"... actually
        // for decompress we already stored raw bytes into out_buf directly in reader.
        // Nothing to do; writer just writes out_buf/out_size.
        job->result = FREEARC_OK;
      } else {
        MemCB mc;
        mc.in_ptr   = job->in_buf;
        mc.in_left  = job->in_size;
        mc.out_ptr  = job->out_buf;
        mc.out_left = job->out_cap;
        int r = Decompress (c->method, mem_callback, &mc);
        if (r >= 0) {
          job->out_size = job->out_cap - mc.out_left;
          job->result   = FREEARC_OK;
        } else {
          job->result = r;
          c->set_err(r);
        }
      }
    }
    job->ready.signal();
  }
  return NULL;
}

// -----------------------------------------------------------------------------
// Writer thread: pops jobs in read-order, waits ready, writes to outer_cb
// -----------------------------------------------------------------------------
static void *writer_thread_fn (void *arg)
{
  _4x4Ctx *c = (_4x4Ctx*)arg;
  CALLBACK_FUNC *cb   = c->outer_cb;
  void          *aux  = c->outer_aux;

  for (;;) {
    _4x4Job *job = (_4x4Job*) c->writer_q.get();
    if (job == NULL) break;  // EOF sentinel
    job->ready.wait();

    if (c->err) { job->ready.reset(); c->free_q.put(job); continue; }

    if (c->direction == 0) {
      // Write header: int32 orig_size (or -1 if raw), int32 comp_size
      uint8_t hdr[_4x4_HEADER_SIZE];
      int32_t orig = job->stored_raw ? -1 : (int32_t)job->in_size;
      setvalue32 (hdr,     (uint32_t)orig);
      setvalue32 (hdr + 4, (uint32_t)job->out_size);
      int w = cb ("write", hdr, _4x4_HEADER_SIZE, aux);
      if (w < 0) { c->set_err(w); job->ready.reset(); c->free_q.put(job); continue; }
      w = cb ("write", job->out_buf, job->out_size, aux);
      if (w < 0) { c->set_err(w); job->ready.reset(); c->free_q.put(job); continue; }
    } else {
      // Decompression: write decompressed payload (out_buf / out_size)
      if (job->out_size > 0) {
        int w = cb ("write", job->out_buf, job->out_size, aux);
        if (w < 0) { c->set_err(w); job->ready.reset(); c->free_q.put(job); continue; }
      }
    }

    job->ready.reset();
    c->free_q.put(job);
  }
  return NULL;
}

// -----------------------------------------------------------------------------
// Pool setup / teardown
// -----------------------------------------------------------------------------
static int pool_init (_4x4Ctx *c)
{
  c->err = 0;
  pthread_mutex_init(&c->err_mu, NULL);

  // Alloc jobs
  c->jobs = (_4x4Job*) calloc (c->num_jobs, sizeof(_4x4Job));
  if (!c->jobs) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  for (int i = 0; i < c->num_jobs; i++) {
    _4x4Job *j = &c->jobs[i];
    j->in_buf  = (uint8_t*) malloc (c->block_size);
    j->out_buf = (uint8_t*) malloc (c->out_cap);
    if (!j->in_buf || !j->out_buf) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    j->out_cap = c->out_cap;
    j->ready.init();
  }

  c->free_q  .init (c->num_jobs + 1);
  c->work_q  .init (c->num_jobs + 1);
  c->writer_q.init (c->num_jobs + 1);

  for (int i = 0; i < c->num_jobs; i++)
    c->free_q.put (&c->jobs[i]);

  // Spawn workers
  c->worker_threads = (pthread_t*) calloc (c->num_workers, sizeof(pthread_t));
  for (int i = 0; i < c->num_workers; i++)
    pthread_create (&c->worker_threads[i], NULL, worker_thread, c);
  pthread_create (&c->writer_thread, NULL, writer_thread_fn, c);
  return 0;
}

static void pool_finish (_4x4Ctx *c)
{
  // Signal workers to stop
  for (int i = 0; i < c->num_workers; i++) c->work_q.put (NULL);
  for (int i = 0; i < c->num_workers; i++) pthread_join (c->worker_threads[i], NULL);
  // Signal writer
  c->writer_q.put (NULL);
  pthread_join (c->writer_thread, NULL);

  // Release job buffers and events
  for (int i = 0; i < c->num_jobs; i++) {
    free (c->jobs[i].in_buf);
    free (c->jobs[i].out_buf);
    c->jobs[i].ready.destroy();
  }
  free (c->jobs);
  free (c->worker_threads);
  c->free_q  .destroy();
  c->work_q  .destroy();
  c->writer_q.destroy();
  pthread_mutex_destroy(&c->err_mu);
}

// -----------------------------------------------------------------------------
// Compression driver
// -----------------------------------------------------------------------------
#ifndef FREEARC_DECOMPRESS_ONLY
static int do_compress (_4x4_METHOD *self, CALLBACK_FUNC *callback, void *auxdata)
{
  // Determine block_size
  int bs = self->BlockSize;
  if (bs == 0) {
    MemSize dict = ::GetDictionary (self->Method);
    bs = dict > 0 ? dict : 8*mb;
  }
  if (bs < 64*kb) bs = 64*kb;

  _4x4Ctx c;
  memset(&c, 0, sizeof(c));
  c.direction    = 0;
  c.method       = self->Method;
  c.block_size   = bs;
  // Output buffer must fit worst-case expansion; use GetMaxCompressedSize if exposed, else 1.1x + 1KB
  c.out_cap      = bs + (bs >> 6) + 1024;
  c.outer_cb     = callback;
  c.outer_aux    = auxdata;
  c.num_workers  = self->get_num_threads();
  if (c.num_workers < 1) c.num_workers = 1;
  c.num_jobs     = c.num_workers + 2;

  // Write version header
  {
    uint8_t vb[4];
    setvalue32 (vb, (uint32_t)_4x4_VERSION);
    int w = callback ("write", vb, 4, auxdata);
    if (w < 0) return w;
  }

  int init_err = pool_init(&c);
  if (init_err < 0) { pool_finish(&c); return init_err; }

  // Reader loop (this thread): read block, submit to work_q + writer_q
  int errcode = FREEARC_OK;
  for (;;) {
    if (c.err) { errcode = c.err; break; }
    _4x4Job *job = (_4x4Job*) c.free_q.get();
    if (!job) { errcode = FREEARC_ERRCODE_GENERAL; break; }

    int r = callback ("read", job->in_buf, c.block_size, auxdata);
    if (r <= 0) {
      if (r < 0) { c.set_err(r); errcode = r; }
      // Return job to free_q (not submitted)
      c.free_q.put (job);
      break;
    }
    job->in_size = r;
    c.work_q  .put (job);
    c.writer_q.put (job);
  }

  pool_finish(&c);
  if (errcode == FREEARC_OK && c.err < 0) errcode = c.err;
  return errcode;
}
#endif

// -----------------------------------------------------------------------------
// Decompression driver
// -----------------------------------------------------------------------------
static int do_decompress (_4x4_METHOD *self, CALLBACK_FUNC *callback, void *auxdata)
{
  // Read version
  uint8_t vb[4];
  int r = callback ("read", vb, 4, auxdata);
  if (r != 4) return r < 0 ? r : FREEARC_ERRCODE_IO;
  uint32_t version = value32 (vb);
  if (version != _4x4_VERSION) return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;

  // Determine output buffer size: we need to know the maximum orig_size.
  // We read blocks on-the-fly; out_cap must fit the largest decompressed block.
  // Use BlockSize if set, else GetDictionary, else 64MB fallback.
  int out_cap = self->BlockSize;
  if (out_cap == 0) {
    MemSize dict = ::GetDictionary (self->Method);
    out_cap = dict > 0 ? dict : 8*mb;
  }
  if (out_cap < 64*kb) out_cap = 64*kb;

  _4x4Ctx c;
  memset(&c, 0, sizeof(c));
  c.direction    = 1;
  c.method       = self->Method;
  c.block_size   = out_cap + 4096;   // compressed input buffer; will grow if needed
  c.out_cap      = out_cap + 4096;   // decompressed output
  c.outer_cb     = callback;
  c.outer_aux    = auxdata;
  c.num_workers  = self->get_num_threads();
  if (c.num_workers < 1) c.num_workers = 1;
  c.num_jobs     = c.num_workers + 2;

  int init_err = pool_init(&c);
  if (init_err < 0) { pool_finish(&c); return init_err; }

  int errcode = FREEARC_OK;
  for (;;) {
    if (c.err) { errcode = c.err; break; }

    // Read per-block header (8 bytes)
    uint8_t hdr[_4x4_HEADER_SIZE];
    int r = callback ("read", hdr, _4x4_HEADER_SIZE, auxdata);
    if (r == 0) break;  // EOF
    if (r != _4x4_HEADER_SIZE) { errcode = r < 0 ? r : FREEARC_ERRCODE_IO; break; }

    int32_t orig_size = (int32_t) value32 (hdr);
    int32_t comp_size = (int32_t) value32 (hdr + 4);
    if (comp_size < 0) { errcode = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    _4x4Job *job = (_4x4Job*) c.free_q.get();
    if (!job) { errcode = FREEARC_ERRCODE_GENERAL; break; }

    // Grow buffers if needed
    int need = comp_size > (orig_size > 0 ? orig_size : 0) ? comp_size : (orig_size > 0 ? orig_size : 0);
    if (need > c.out_cap) {
      free(job->out_buf);
      job->out_buf = (uint8_t*) malloc (need + 4096);
      job->out_cap = need + 4096;
    }
    if (comp_size > c.block_size) {
      free(job->in_buf);
      job->in_buf = (uint8_t*) malloc (comp_size);
    }

    if (orig_size == -1) {
      // Stored raw: read straight into out_buf, short-circuit worker
      r = callback ("read", job->out_buf, comp_size, auxdata);
      if (r != comp_size) { errcode = r < 0 ? r : FREEARC_ERRCODE_IO; c.free_q.put(job); break; }
      job->stored_raw = 1;
      job->out_size   = comp_size;
      job->in_size    = comp_size;
      job->result     = FREEARC_OK;
      job->ready.signal();
      c.writer_q.put (job);
      continue;
    }

    // Compressed block: read comp_size bytes into in_buf, dispatch to worker
    r = callback ("read", job->in_buf, comp_size, auxdata);
    if (r != comp_size) { errcode = r < 0 ? r : FREEARC_ERRCODE_IO; c.free_q.put(job); break; }

    job->stored_raw = 0;
    job->in_size    = comp_size;
    job->out_size   = 0;
    c.work_q  .put (job);
    c.writer_q.put (job);
  }

  pool_finish(&c);
  if (errcode == FREEARC_OK && c.err < 0) errcode = c.err;
  return errcode;
}

// -----------------------------------------------------------------------------
// _4x4_METHOD class methods
// -----------------------------------------------------------------------------
_4x4_METHOD::_4x4_METHOD()
{
  strcpy (Method, "tor:3");
  NumThreads = 0;
  BlockSize  = 0;
}

int _4x4_METHOD::get_num_threads()
{
  if (NumThreads > 0) return NumThreads;
  int t = GetCompressionThreads();
  return t > 0 ? t : 1;
}

void _4x4_METHOD::get_inner_method (char *buf)
{
  strncopy (buf, Method, MAX_METHOD_STRLEN);
}

int _4x4_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  int saved = compress_all_at_once;
  compress_all_at_once = 1;
  int r = do_decompress (this, callback, auxdata);
  compress_all_at_once = saved;
  return r;
}

#ifndef FREEARC_DECOMPRESS_ONLY
int _4x4_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  int saved = compress_all_at_once;
  compress_all_at_once = 1;
  int r = do_compress (this, callback, auxdata);
  compress_all_at_once = saved;
  return r;
}

void _4x4_METHOD::ShowCompressionMethod (char *buf)
{
  char bsStr[64] = "";
  if (BlockSize) showMem (BlockSize, bsStr);
  char tStr[64] = "";
  if (NumThreads) sprintf (tStr, ":t%d", NumThreads);
  if (BlockSize && NumThreads)       sprintf (buf, "4x4%s:b%s:%s", tStr, bsStr, Method);
  else if (BlockSize)                sprintf (buf, "4x4:b%s:%s",   bsStr, Method);
  else if (NumThreads)               sprintf (buf, "4x4%s:%s",     tStr, Method);
  else                               sprintf (buf, "4x4:%s",       Method);
}

MemSize _4x4_METHOD::GetCompressionMem()
{
  int t  = get_num_threads();
  MemSize d = ::GetDictionary (Method);
  int bs = BlockSize ? BlockSize : (d > 0 ? d : 8*mb);
  MemSize inner = ::GetCompressionMem (Method);
  return t * inner + (t + 2) * 2 * bs;
}
MemSize _4x4_METHOD::GetDecompressionMem()
{
  int t  = get_num_threads();
  MemSize d = ::GetDictionary (Method);
  int bs = BlockSize ? BlockSize : (d > 0 ? d : 8*mb);
  MemSize inner = ::GetDecompressionMem (Method);
  return t * inner + (t + 2) * 2 * bs;
}

void _4x4_METHOD::SetCompressionMem (MemSize mem)
{
  // Best-effort: leave inner method untouched; adjust BlockSize if absurdly large
  (void)mem;
}
#endif

int _4x4_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  if (strequ (what, "has_progress?")) return 1;
  return COMPRESSION_METHOD::doit (what, param, data, callback);
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------
COMPRESSION_METHOD* parse_4x4 (char** parameters)
{
  if (strcmp (parameters[0], "4x4") != 0) return NULL;

  _4x4_METHOD *p = new _4x4_METHOD;
  int error = 0;

  while (!error && *++parameters) {
    char *param = *parameters;

    // If parameter is not a pure number / memsize, it's the inner method (+ its params joined)
    if (!(isdigit(param[0]) || isdigit(param[1]))) {
      join_params (parameters, COMPRESSION_METHOD_PARAMETERS_DELIMITER, p->Method, sizeof(p->Method));
#ifndef FREEARC_DECOMPRESS_ONLY
      COMPRESSION_METHOD *check = ParseCompressionMethod (p->Method);
      error = (check == NULL);
      delete check;
#endif
      break;
    }

    // Recognized prefixed params
    switch (*param) {
      case 'b':  p->BlockSize  = parseMem (param+1, &error); continue;
      case 't':  p->NumThreads = parseInt (param+1, &error); continue;
    }

    // Bare number = NumThreads, bare memsize = BlockSize
    int n = parseInt (param, &error);
    if (!error) p->NumThreads = n;
    else        error = 0, p->BlockSize = parseMem (param, &error);
  }

  if (error) { delete p; return NULL; }
  return p;
}

// Register the 4x4 method at library init
static int _4x4_x = AddCompressionMethod (parse_4x4);
