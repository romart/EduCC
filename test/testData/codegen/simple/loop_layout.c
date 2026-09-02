// Block layout by loop structure (roadmap step 39), and the case that only it
// reaches: a conditional branch neither of whose successors is laid out next,
// so selectCondBranch has to emit a jcc *and* a jmp. That arm existed from the
// day the selector did and nothing had ever executed it - reverse postorder
// over ast2ir's successor order could not produce the shape. A switch inside a
// loop, with cases that leave the loop and cases that go round again, can.
//
// Everything here is checked for its answer rather than its code: what the
// layout does is a quality question, but the branches it forces the selector
// to invert and duplicate are a correctness one.

int scan(const char *p, int n) {
  int acc = 0;

  for (int i = 0; i < n; i++) {
    switch (p[i]) {
    case 'a':
      acc += 1;
      break;
    case 'b':
      acc += 10;
      break;
    case 'q':
      return acc;
    default:
      return -1;
    }
  }

  return acc;
}

// Nested loops with a break out of the inner one: the inner body has to stay
// contiguous with its header while the outer loop still closes around both.
int grid(int w, int h) {
  int total = 0;

  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      if (x == y) break;
      total += 1;
    }
    total += 100;
  }

  return total;
}

// A do-while, whose back edge already fell through, plus a continue, which
// gives the header two back edges and so must stay one loop and not two.
int skips(int n) {
  int s = 0;
  int i = 0;

  do {
    i++;
    if (i % 3 == 0) continue;
    s += i;
  } while (i < n);

  return s;
}

int main() {
  if (scan("aab", 3) != 12) return 1;
  if (scan("abq", 3) != 11) return 2;
  if (scan("abz", 3) != -1) return 3;
  if (scan("", 0) != 0) return 4;
  if (scan("aaaa", 4) != 4) return 5;

  if (grid(3, 3) != 303) return 6;
  if (grid(0, 4) != 400) return 7;
  if (grid(4, 0) != 0) return 8;

  if (skips(10) != 37) return 9;
  if (skips(1) != 1) return 10;

  return 0;
}
