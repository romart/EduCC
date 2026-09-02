// The two shapes an IR_M_COPY is selected into, either side of the size where
// unrolling it stops paying (X86_UNROLLED_COPY_LIMIT in isel_x86_64.c).
//
// 'small' is a run of load/store pairs at increasing displacements, widest
// chunk first; 'large' is the count into rcx, the two addresses into rdi and
// rsi, and one 'rep movsb' that walks all three. The dumps are here because
// the second is the only instruction this backend emits whose every operand is
// implicit - nothing in the encoding names a register - so what says the
// allocator was told about them at all is the annotation on that line.

struct Small { long a[3]; };
struct Large { long a[64]; };

long small(struct Small *dst, struct Small *src) {
  *dst = *src;
  return dst->a[0];
}

long large(struct Large *dst, struct Large *src) {
  *dst = *src;
  return dst->a[0];
}
