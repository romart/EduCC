// Six doubles live across the whole inner body, well past the number of
// callee-saved xmm registers there are (which is none on this ABI).
#include <stdio.h>
#include <math.h>

#define BODIES 5
#define STEPS 2000000

static double px[BODIES], py[BODIES], pz[BODIES];
static double vx[BODIES], vy[BODIES], vz[BODIES];
static double mass[BODIES];

static void advance(double dt) {
  int i, j;

  for (i = 0; i < BODIES; ++i) {
    for (j = i + 1; j < BODIES; ++j) {
      double dx = px[i] - px[j];
      double dy = py[i] - py[j];
      double dz = pz[i] - pz[j];
      double d2 = dx * dx + dy * dy + dz * dz;
      double mag = dt / (d2 * sqrt(d2));
      double mi = mass[i] * mag;
      double mj = mass[j] * mag;

      vx[i] -= dx * mj; vy[i] -= dy * mj; vz[i] -= dz * mj;
      vx[j] += dx * mi; vy[j] += dy * mi; vz[j] += dz * mi;
    }
  }

  for (i = 0; i < BODIES; ++i) {
    px[i] += dt * vx[i];
    py[i] += dt * vy[i];
    pz[i] += dt * vz[i];
  }
}

static double energy() {
  double e = 0.0;
  int i, j;

  for (i = 0; i < BODIES; ++i) {
    e += 0.5 * mass[i] * (vx[i] * vx[i] + vy[i] * vy[i] + vz[i] * vz[i]);
    for (j = i + 1; j < BODIES; ++j) {
      double dx = px[i] - px[j];
      double dy = py[i] - py[j];
      double dz = pz[i] - pz[j];
      e -= mass[i] * mass[j] / sqrt(dx * dx + dy * dy + dz * dz);
    }
  }
  return e;
}

int main() {
  int i, s;

  for (i = 0; i < BODIES; ++i) {
    px[i] = (double)(i + 1);
    py[i] = (double)(i * 2 + 1) * 0.5;
    pz[i] = (double)(i * 3 + 2) * 0.25;
    vx[i] = 0.01 * (double)(i + 1);
    vy[i] = -0.02 * (double)(i + 1);
    vz[i] = 0.003 * (double)(i + 1);
    mass[i] = 1.0 + 0.25 * (double)i;
  }

  for (s = 0; s < STEPS; ++s) advance(0.001);

  printf("%.9f\n", energy());
  return 0;
}
