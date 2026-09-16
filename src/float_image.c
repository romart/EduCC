#include "float_image.h"

#include <assert.h>
#include <string.h>

// x87's 80-bit extended format and IEEE binary128, and the conversion between
// them. They agree about everything except the significand: fifteen exponent
// bits biased by 16383 in both, so a value normal in one is normal in the other
// and the exponent field copies straight across. What differs is that x87
// writes the leading bit of the significand and binary128 leaves it implicit,
// and that binary128 has 112 fraction bits where x87 has 63.
//
// So widening is exact and narrowing rounds, which is the whole of this file.

#define EXP_MASK 0x7fffu
#define X87_SIG_BITS 63
#define QUAD_FRAC_BITS 112

typedef union {
  uint8_t bytes[16];
  uint64_t qwords[2];
  long double ld;
} Image;

enum LongDoubleFormat hostLongDoubleFormat(void) {
  Image one;
  one.qwords[0] = one.qwords[1] = 0;
  one.ld = 1.0L;

  if (one.qwords[0] == 0x8000000000000000ULL && (one.qwords[1] & EXP_MASK) == 0x3fffu) {
    return LDF_X87_EXTENDED;
  }

  assert(one.qwords[0] == 0 && one.qwords[1] == 0x3fff000000000000ULL &&
         "the host's long double is neither x87 extended nor binary128");

  return LDF_BINARY128;
}

size_t hostLongDoubleWidth(void) {
  return hostLongDoubleFormat() == LDF_X87_EXTENDED ? 10 : 16;
}

// The 63 fraction bits of an x87 significand sit at the top of binary128's 112,
// so every conversion this way is a shift and nothing is lost.
static void widenToQuad(const Image *src, Image *dst) {
  uint64_t sig = src->qwords[0];
  uint32_t exp = (uint32_t)(src->qwords[1] & EXP_MASK);
  uint64_t sign = (src->qwords[1] >> 15) & 1u;

  uint64_t frac = sig & ~(1ULL << 63);

  dst->qwords[0] = dst->qwords[1] = 0;

  // One expression for all three cases. x87 writes the leading bit where
  // binary128 leaves it implicit, so dropping it lines the remaining 63 up with
  // the top of binary128's 112 - and a subnormal, whose leading bit is zero and
  // whose least exponent is the same in both formats, lands there too.
  dst->qwords[0] = frac << (QUAD_FRAC_BITS - X87_SIG_BITS);
  dst->qwords[1] = (uint64_t)exp << 48 | frac >> (64 - (QUAD_FRAC_BITS - X87_SIG_BITS));
  dst->qwords[1] |= sign << 63;
}

// Round a 128-bit significand right by 'shift', to nearest, ties to even. The
// answer can carry out of bit 63, which is the caller's to notice.
static uint64_t roundShiftRight(uint64_t lo, uint64_t hi, uint32_t shift, Boolean *carry) {
  uint64_t kept, dropped, half;

  assert(shift > 0 && shift < 64);

  kept = hi << (64 - shift) | lo >> shift;
  dropped = lo & ((1ULL << shift) - 1);
  half = 1ULL << (shift - 1);

  *carry = FALSE;

  if (dropped > half || (dropped == half && (kept & 1) != 0)) {
    kept += 1;
    if (kept == 0) *carry = TRUE;
  }

  return kept;
}

static void narrowToX87(const Image *src, Image *dst) {
  uint32_t exp = (uint32_t)((src->qwords[1] >> 48) & EXP_MASK);
  uint64_t sign = src->qwords[1] >> 63;
  uint64_t fracHi = src->qwords[1] & 0xffffffffffffULL;
  uint64_t fracLo = src->qwords[0];

  dst->qwords[0] = dst->qwords[1] = 0;

  if (exp == EXP_MASK) {
    uint64_t sig = 1ULL << 63 | fracHi << (X87_SIG_BITS - 48) |
                   fracLo >> (64 - (X87_SIG_BITS - 48));

    // Quieted, which is what a format conversion does to a signalling NaN -
    // and which also keeps a NaN whose whole payload was in the dropped bits
    // from coming out an infinity.
    if ((fracHi | fracLo) != 0) {
      sig |= 1ULL << 62;
    }

    dst->qwords[0] = sig;
    dst->qwords[1] = EXP_MASK;
  } else {
    // Both formats put their least exponent in the same place, so a subnormal
    // stays one and needs no normalizing: its leading bit is zero in both, and
    // the shift is the same 49 that a normal's fraction takes. Only the
    // leading bit differs, x87 writing the one binary128 leaves implicit.
    uint64_t hi = exp == 0 ? fracHi : fracHi | 1ULL << 48;
    int32_t e = (int32_t)exp;
    Boolean carry = FALSE;
    uint64_t sig = roundShiftRight(fracLo, hi, QUAD_FRAC_BITS - X87_SIG_BITS, &carry);

    if (carry) {
      // The rounding carried out of the significand: 1.111..1 became 10.0, so
      // the leading bit moves up one place and the exponent with it.
      sig = 1ULL << 63;
      e += 1;
    }

    // A subnormal that rounded up into the normal range. x87 reads exponent
    // fields of 0 and 1 as the same power, so this is only a spelling - but it
    // is the canonical one, and a leading bit under a zero exponent is not.
    if (e == 0 && (sig >> 63) != 0) {
      e = 1;
    }

    if (e >= (int32_t)EXP_MASK) {
      dst->qwords[0] = 1ULL << 63;  // overflowed the format: an infinity
      dst->qwords[1] = EXP_MASK;
    } else {
      dst->qwords[0] = sig;
      dst->qwords[1] = (uint64_t)e;
    }
  }

  dst->qwords[1] |= sign << 15;
}

void longDoubleImage(enum LongDoubleFormat format, long double v, uint8_t out[16]) {
  enum LongDoubleFormat host = hostLongDoubleFormat();
  Image src, dst;

  src.qwords[0] = src.qwords[1] = 0;
  src.ld = v;

  if (host == format) {
    // The host's own bytes, padding included - which is zero, the union having
    // been cleared before the value went in.
    memcpy(out, src.bytes, 16);
    return;
  }

  if (format == LDF_BINARY128) {
    widenToQuad(&src, &dst);
  } else {
    narrowToX87(&src, &dst);
  }

  memcpy(out, dst.bytes, 16);
}
