// A parameter of array type is adjusted to a pointer, so void * - NULL among
// them - converts to it as it does to any object pointer. SQLite's
// utimes(path, NULL) was rejected as an incompatible type.

struct tv { long sec, usec; };

static int taken(const char *p, const struct tv times[2]) {
  if (p == 0) return -1;
  return times == 0 ? 1 : (int)times[1].sec;
}

int main(void) {
  struct tv two[2];

  two[0].sec = 5; two[0].usec = 0;
  two[1].sec = 7; two[1].usec = 0;

  if (taken("x", (void *)0) != 1) return 1;
  if (taken("x", 0) != 1) return 2;
  if (taken("x", two) != 7) return 3;
  return 0;
}
