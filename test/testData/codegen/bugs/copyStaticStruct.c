
struct Type {
  unsigned x, y, z, a, b;
};

static struct Type char_type;

static struct Type x;

int test() {
  char_type.b |= 0x10;

  x = char_type;

  return x.b;
}

int main() {
  if (test() != 0x10) return 1;
  return 0;
}
