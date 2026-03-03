/* Declarations for MicroHs FFI callback stubs.
 * These are implemented in compat/posix_compat.c and used when
 * MicroHs cannot create Haskell-to-C callback trampolines
 * (foreign import ccall "wrapper" is unsupported in MicroHs). */
#ifndef MHS_STUBS_H
#define MHS_STUBS_H

#ifdef __cplusplus
extern "C" {
#endif

/* Compression callback stub: always returns FREEARC_ERRCODE_NOT_IMPLEMENTED (-8) */
int mhs_stub_callback(const char *what, char *buf, int size, void *auxdata);

/* Lua callback stubs */
int         mhs_stub_lua_writer    (void *L, const char *p, size_t sz, void *ud);
const char *mhs_stub_lua_reader    (void *L, void *ud, size_t *sz);
int         mhs_stub_lua_cfunction (void *L);

#ifdef __cplusplus
}
#endif

#endif /* MHS_STUBS_H */
