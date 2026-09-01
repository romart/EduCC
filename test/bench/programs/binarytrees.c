// Recursion and malloc/free: every call site is a point where the allocator
// has to get caller-saved registers off the way, which is the one thing a
// spill-everything allocator never has to think about.
#include <stdio.h>
#include <stdlib.h>

#define DEPTH 18
#define ROUNDS 15

typedef struct Node {
  struct Node *left;
  struct Node *right;
  int value;
} Node;

static Node *build(int depth, int value) {
  Node *n = (Node *)malloc(sizeof(Node));

  n->value = value;
  if (depth > 0) {
    n->left = build(depth - 1, value * 2);
    n->right = build(depth - 1, value * 2 + 1);
  } else {
    n->left = n->right = NULL;
  }
  return n;
}

static long check(Node *n) {
  if (n->left == NULL) return n->value;
  return n->value + check(n->left) - check(n->right);
}

static void release(Node *n) {
  if (n->left != NULL) {
    release(n->left);
    release(n->right);
  }
  free(n);
}

int main() {
  long total = 0;
  int round, d;

  for (round = 0; round < ROUNDS; ++round) {
    for (d = 4; d <= DEPTH; d += 2) {
      Node *t = build(d, d);
      total += check(t);
      release(t);
    }
  }

  printf("%ld\n", total);
  return 0;
}
