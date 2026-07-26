/* The C BSC coder for the differential harness.
 *
 * This cuts at the QLFC CODER level, not the whole codec: bsc_coder_encode_block
 * and bsc_coder_decode_block operate on one raw buffer with no BWT, ST, LZP or
 * block header involved. That is deliberate -- it lets the range coder, the
 * mixer, the 238 tuned constants, the 49k table entries, the model init and both
 * decode bodies be verified while the only suspects are that code, before more
 * is layered on top.
 *
 * libbsc.cpp brings in the whole library; C_BSC.cpp is not needed, so the
 * archiver's COMPRESSION_METHOD plumbing does not come with it.
 *
 * libsais.c is compiled as its OWN translation unit (see bsc-check.sh), not
 * included here: dropping it into this file redefines the INLINE macro that the
 * later libbsc headers rely on, and st.cpp stops compiling.
 */
#include "../../Compression/BSC/libbsc/libbsc/libbsc.cpp"
#include "../../Compression/BSC/libbsc/coder/coder.cpp"
#include "../../Compression/BSC/libbsc/coder/qlfc/qlfc.cpp"
#include "../../Compression/BSC/libbsc/coder/qlfc/qlfc_model.cpp"
#include "../../Compression/BSC/libbsc/platform/platform.cpp"
#include "../../Compression/BSC/libbsc/adler32/adler32.cpp"
#include "../../Compression/BSC/libbsc/lzp/lzp.cpp"
#include "../../Compression/BSC/libbsc/bwt/bwt.cpp"
#include "../../Compression/BSC/libbsc/st/st.cpp"
#include "../../Compression/BSC/libbsc/filters/detectors.cpp"
#include "../../Compression/BSC/libbsc/filters/preprocessing.cpp"

extern "C" {
int darc_bsc_coder_encode_block (const unsigned char *in, unsigned char *out, int inSize, int outSize, int coder)
{ return bsc_coder_encode_block(in, out, inSize, outSize, coder); }

int darc_bsc_coder_decode_block (const unsigned char *in, unsigned char *out, int coder)
{ return bsc_coder_decode_block(in, out, coder); }

int darc_bsc_init (int features) { return bsc_init(features); }

/* The coder layer above the three encode bodies: block splitting plus framing.
 * bsc_coder_compress is what bsc_compress actually calls. */
int darc_bsc_coder_compress (const unsigned char *in, unsigned char *out, int n, int coder, int features)
{ return bsc_coder_compress(in, out, n, coder, features); }

/* Forward BWT (with auxiliary indexes) and its inverse, for the inverse-BWT
 * differential harness. The encoder is the same libsais forward transform a
 * real archive was built with, so the index / num_indexes / transformed bytes
 * it produces are exactly the format the Rust inverse must read. */
int darc_bsc_bwt_encode (unsigned char *T, int n, unsigned char *num_indexes, int *indexes, int features)
{ return bsc_bwt_encode(T, n, num_indexes, indexes, features); }

int darc_bsc_bwt_decode (unsigned char *T, int n, int index, unsigned char num_indexes, int *indexes, int features)
{ return bsc_bwt_decode(T, n, index, num_indexes, indexes, features); }

/* Forward sort-transform (ST3..ST8) and its inverse, for the inverse-ST
 * differential harness. */
int darc_bsc_st_encode (unsigned char *T, int n, int k, int features)
{ return bsc_st_encode(T, n, k, features); }

int darc_bsc_st_decode (unsigned char *T, int n, int k, int index, int features)
{ return bsc_st_decode(T, n, k, index, features); }

/* The QLFC forward transform, for the transform differential harness. This is
 * the stage all three encode bodies share, and it is cut out on its own for the
 * same reason the QLFC decoders were: a mismatch here points at the MTF walk
 * rather than at the range coder wrapped around it.
 *
 * qlfc.cpp is included above, so bsc_qlfc_transform is already declared -- and
 * with C++ linkage, so re-declaring it inside this extern "C" block is an
 * error rather than a redundancy. */
int darc_bsc_qlfc_transform (const unsigned char *in, int n, unsigned char *buffer, unsigned char *mtf)
{ return (int)(bsc_qlfc_transform(in, buffer, n, mtf) - buffer); }

/* Forward LZP, for the LZP-ENCODER differential harness. This is the first
 * encoder-side cut into BSC: LZP is the compressor's first stage and depends on
 * neither the block sorter nor the entropy coder, so a mismatch here points at
 * the match finder alone. */
int darc_bsc_lzp_compress (const unsigned char *in, unsigned char *out, int n, int hashSize, int minLen, int features)
{ return bsc_lzp_compress(in, out, n, hashSize, minLen, features); }

/* The whole codec, for the end-to-end differential harness: bsc_compress builds
 * a real framed block (header + entropy + block-sort + optional LZP), and the C
 * and Rust dispatchers both invert it. */
int darc_bsc_compress (const unsigned char *in, unsigned char *out, int n, int lzpHashSize, int lzpMinLen, int blockSorter, int coder, int features)
{ return bsc_compress(in, out, n, lzpHashSize, lzpMinLen, blockSorter, coder, features); }

int darc_bsc_decompress (const unsigned char *in, int inSize, unsigned char *out, int outSize, int features)
{ return bsc_decompress(in, inSize, out, outSize, features); }
}
