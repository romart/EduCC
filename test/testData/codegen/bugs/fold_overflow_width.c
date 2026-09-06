// The IR folder built its result constant out of the raw 64-bit arithmetic and
// left it at the operation's own type, so a fold that overflowed the type kept
// the bits that should have been dropped. Every wider use then read them:
// '(uint32_t)0x80000000u << 1' is zero, and mbedtls' bignum tests say so.

#include <stdint.h>

int main(void) {
  uint32_t x = 0x80000000u;
  unsigned long long wide = x << 1;
  if (wide != 0) return 1;
  if ((x << 1) != 0) return 2;

  uint32_t sum = 0xffffffffu;
  if ((unsigned long long)(sum + 1u) != 0) return 3;

  uint16_t small = 40000;
  if ((unsigned long long)(uint16_t)(small + small) != 14464) return 4;

  uint8_t byte = 200;
  if ((unsigned long long)(uint8_t)(byte + byte) != 144) return 5;

  uint32_t mul = 0x10000u;
  if ((unsigned long long)(mul * mul) != 0) return 6;

  uint16_t ones = (uint16_t)~(uint16_t)0;
  if ((unsigned long long)ones != 0xffff) return 7;

  uint32_t high = 0xfffffffeu;
  if ((unsigned long long)(high << 4) != 0xffffffe0u) return 8;

  return 0;
}
