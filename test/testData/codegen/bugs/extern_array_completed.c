// 6.2.7: an array of unknown size is compatible with one whose size is known,
// so a later definition completes 'extern T x[]'. Rejecting it as a
// redefinition stopped zlib's trees.c, which declares its tables this way.

extern const unsigned char lengths[];
extern int counts[];

const unsigned char lengths[4] = { 1, 2, 3, 4 };
int counts[3] = { 10, 20, 30 };

int main(void) {
  if (lengths[3] != 4) return 1;
  if (counts[2] != 30) return 2;
  if (sizeof(lengths) != 4) return 3;
  if (sizeof(counts) != 3 * sizeof(int)) return 4;
  return 0;
}
