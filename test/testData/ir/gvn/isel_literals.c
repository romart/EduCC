// String literals, which are the constant pool's only entry kind so far.
//
// A literal is the one constant whose value is an address of something that
// does not exist yet: there is no immediate form and no symbol to be relative
// to, so selection puts the bytes in the function's pool and emits a
// rip-relative 'lea' against an index into it, which emission resolves to a
// .rodata offset. That makes the pool visible in the dump as 'cp#N "..."',
// and these four functions are the cases that index has to get right.
//
//   'one'      the base case: one literal, one pool entry, one lea.
//   'twice'    the same text used twice in one function. The IR's own constant
//              cache collapses the two occurrences into one IR_DEF_CONST, so
//              this pins down that the pool is asked once and both uses read
//              cp#0 - a second entry here would mean the same bytes stored
//              twice in .rodata.
//   'distinct' two different literals, so two entries: this is what says the
//              index is per constant and not per function. The order they
//              appear in is the order selection first reached them.
//   'embedded' bytes a dump cannot print literally - a newline, a quote, a
//              backslash, a high byte - so the escaping in the dump is pinned
//              down too, and a literal whose length includes an interior NUL
//              is not truncated at it the way strlen would.
//
// The pointers are returned rather than dereferenced on purpose: what is being
// tested is the address, and a load would only add noise from the load rule.

const char *one(void) {
  return "hello";
}

int twice(void) {
  const char *a = "same";
  const char *b = "same";
  return a == b;
}

const char *distinct(int which) {
  return which ? "first" : "second";
}

const char *embedded(void) {
  return "a\n\"b\\c\x80\0d";
}
