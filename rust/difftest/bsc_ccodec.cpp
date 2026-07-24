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
}
