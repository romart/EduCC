// In an array of deduced size, laying out a designated element walked the
// elements already placed and charged the running offset for each of them a
// second time. Every designator after the first landed one element too far
// along per element already there, and the ones pushed past the end vanished -
// which is how mbedtls' cipher_wrap.c lost half of the table it indexes by
// cipher name, and dereferenced a null base.

static const int one = 1, two = 2, three = 3;

const int *const bases[] = {
  [1] = &one,
  [3] = &two,
  [5] = &three,
};

int values[] = { [1] = 20, [3] = 40, [5] = 60 };
int mixed[] = { 1, 2, 3, [3] = 44 };
int backwards[] = { [3] = 4, [1] = 2 };
int leading[] = { [2] = 3, 4, 5 };

int main(void) {
  if (sizeof(bases) / sizeof(bases[0]) != 6) return 1;
  if (bases[0] != 0 || bases[2] != 0 || bases[4] != 0) return 2;
  if (*bases[1] != 1 || *bases[3] != 2 || *bases[5] != 3) return 3;

  if (sizeof(values) / sizeof(values[0]) != 6) return 4;
  if (values[1] != 20 || values[3] != 40 || values[5] != 60) return 5;
  if (values[0] || values[2] || values[4]) return 6;

  if (sizeof(mixed) / sizeof(mixed[0]) != 4) return 7;
  if (mixed[0] != 1 || mixed[1] != 2 || mixed[2] != 3 || mixed[3] != 44) return 8;

  if (sizeof(backwards) / sizeof(backwards[0]) != 4) return 9;
  if (backwards[1] != 2 || backwards[3] != 4) return 10;

  if (sizeof(leading) / sizeof(leading[0]) != 5) return 11;
  if (leading[2] != 3 || leading[3] != 4 || leading[4] != 5) return 12;

  return 0;
}
