// Stack arguments are stored into an area reserved once for the whole
// function rather than pushed at each call, since roadmap step 42
// (docs/ir-codegen-design.md section 6.8). One area serves every call site,
// which is where the ways to get this wrong live.
//
// Check 3 is the one that says the area is not written by something other than
// the call it belongs to: 'outer' passes the results of two calls that
// themselves have stack arguments, so all three use the same bytes. If a
// nested call's stores were emitted anywhere but immediately in front of that
// call, one of them would land on top of the other's arguments.
//
// Check 4 is the same question asked of a loop. The area is written over on
// every iteration, so an argument narrower than an eightbyte has to fill the
// whole of one: the bytes above it are whatever the previous iteration - or
// the previous call - left there, and a callee reading its argument wide would
// read those. The 'char' and 'short' arguments are what would show it.
//
// Check 5 is the dynamic allocation. A VLA carves the stack below the frame,
// and the argument area is always the bottom of the stack, so the block a VLA
// gets has to start above an area whose width is settled before any of it is
// selected. Getting that wrong hands the callee bytes of the VLA, or hands the
// VLA bytes the next call is about to write.
//
// Check 6 is recursion, where each frame reserves its own area, and check 7
// mixes classes: SSE arguments run out at eight and integer ones at six, so
// both spill into the same area from different directions.

int sum9(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
  return a + b + c + d + e + f + g + h + i;
}

// Narrow arguments, so the eightbyte above each one has to be written.
int narrow9(int a, int b, int c, int d, int e, int f, char g, short h, int i) {
  return a + b + c + d + e + f + (int)g + (int)h + i;
}

int outer(int x) {
  return sum9(sum9(x, 1, 2, 3, 4, 5, 6, 7, 8), narrow9(x, 1, 2, 3, 4, 5, 6, 7, 8),
              10, 20, 30, 40, 50, 60, 70);
}

int inLoop(int n) {
  int total = 0;

  for (int k = 0; k < n; ++k) {
    total += narrow9(k, 1, 2, 3, 4, 5, (char)(k & 7), (short)(k * 3), k + 1);
  }

  return total;
}

int withVla(int n) {
  int total = 0;

  for (int k = 0; k < 3; ++k) {
    int v[n];

    for (int j = 0; j < n; ++j) {
      v[j] = j + k;
    }

    total += sum9(v[0], v[1], v[2], n, k, 6, 7, 8, 9);

    // After the call, so that a call writing into the VLA's bytes shows up.
    for (int j = 0; j < n; ++j) {
      if (v[j] != j + k) {
        return -1;
      }
    }
  }

  return total;
}

int recurse(int depth, int acc) {
  if (depth == 0) {
    return acc;
  }

  return sum9(recurse(depth - 1, acc + 1), 0, 0, 0, 0, 0, 0, 0, 0);
}

// Six integers and eight doubles fit in registers; everything after each runs
// out goes on the stack, and the two run out at different points.
double mixed(int a, int b, int c, int d, int e, int f, int g, int h,
             double p, double q, double r, double s,
             double t, double u, double v, double w, double x, double y) {
  return (double)(a + b + c + d + e + f + g + h) + p + q + r + s + t + u + v + w + x + y;
}

int main(void) {
  // 1: the plain case, three arguments in the area.
  if (sum9(1, 2, 3, 4, 5, 6, 7, 8, 9) != 45) {
    return 1;
  }

  // 2: the same with narrow arguments.
  if (narrow9(1, 2, 3, 4, 5, 6, 7, 8, 9) != 45) {
    return 2;
  }

  // 3: calls whose arguments are calls with arguments of their own.
  if (outer(0) != 36 + 36 + 10 + 20 + 30 + 40 + 50 + 60 + 70) {
    return 3;
  }

  // 4: the area written over on every iteration.
  //    k=0: 0+1+2+3+4+5+0+0+1 = 16; k=1: 1+15+1+3+2 = 22; k=2: 2+15+2+6+3 = 28
  if (inLoop(3) != 16 + 22 + 28) {
    return 4;
  }

  // 5: a VLA below the area.
  if (withVla(3) != (0 + 1 + 2 + 3 + 0 + 30) + (1 + 2 + 3 + 3 + 1 + 30)
                        + (2 + 3 + 4 + 3 + 2 + 30)) {
    return 5;
  }

  // 6: one area per frame, four frames deep.
  if (recurse(4, 0) != 4) {
    return 6;
  }

  // 7: both register classes running out.
  if (mixed(1, 2, 3, 4, 5, 6, 7, 8,
            0.5, 0.25, 0.125, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0) != 36.0 + 127.875) {
    return 7;
  }

  return 0;
}
