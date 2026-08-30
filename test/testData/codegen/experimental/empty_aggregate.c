// Copying nothing.
//
// 'struct E { }' is a GNU extension this frontend accepts, and its size is
// zero - so an assignment of one builds an IR_M_COPY whose count is the
// constant 0. Selection used to ask for a count strictly greater than zero and
// refuse everything else as "a number of bytes not known until run time",
// which sent a function containing one back to the legacy backend for a copy
// that has no bytes in it at all. Zero is a count like any other; the right
// amount of code for it is none.
//
// It is the only input in the corpus that reaches that rule with a constant
// count and still fails it, which is why the refusal survived so long: it was
// written for a dynamically sized copy, and nothing builds one of those -
// generateCompositeCopy takes every count from computeTypeSize(). See section
// 6.21 of docs/ir-codegen-design.md.
//
// The empty members are placed between fields that carry values, so a copy
// that moved the wrong number of bytes shows up as a wrong number rather than
// as nothing at all.
//
// Note that the size is zero and not one. C++ gives an empty class size 1 so
// that two objects of it have distinct addresses; C has no empty struct at all
// and GCC's extension gives it size 0, which is what both backends here agree
// with. Distinct addresses are the linker's business instead, and are not
// checked below - gcc, and each of the two backends, place two zero-sized
// globals differently, and none of the three is wrong.
//
// The array checks are the other half of the same fact. An array of zero-sized
// elements has every element at one address, so an index scales to nothing:
// translateArrayAccess used to assert that an element was at least one byte
// wide and abort on the second line of the loop below.

struct E { };

struct Wrapped {
  int before;
  struct E gap;
  int after;
};

struct E globalEmpty;

int main(void) {
  struct E a, b;

  // The assignment itself, local and through a global, which are two different
  // addresses for selection to fold.
  a = b;
  globalEmpty = a;
  b = globalEmpty;

  // An empty member inside a struct that is not empty. The whole-struct copy
  // is eight bytes here and the gap contributes none of them, so a copy sized
  // from the wrong member would take the wrong number.
  struct Wrapped w, v;
  w.before = 11;
  w.after = 22;
  v = w;
  if (v.before != 11) return 1;
  if (v.after != 22) return 2;

  // Passing and returning one by value is a separate matter, and a broken one
  // in this backend - see codegen/bugs/empty_aggregate_by_value.c.

  // An array of them, which is a copy of zero bytes as many times as it has
  // elements - and every one of those copies is to the same address.
  struct E arr[4];
  for (int i = 0; i < 4; ++i) {
    arr[i] = a;
  }

  // The index scales by the element size, so with a zero-sized element every
  // subscript names arr[0]. A runtime index as well as a constant one, since
  // one is folded at translation and the other is not.
  int three = 3;
  if (&arr[3] != &arr[0]) return 3;
  if (&arr[three] != &arr[0]) return 4;

  // The same thing through a pointer rather than a subscript, which is the
  // other place the element size is spent.
  struct E *p = arr;
  if (p + 2 != p) return 5;

  // An array of them is itself empty, so the whole array is a copyable
  // zero-byte object too.
  struct E other[4];
  *other = *arr;

  if (sizeof(struct E) != 0) return 6;
  if (sizeof(arr) != 0) return 7;
  if (sizeof(struct Wrapped) != 8) return 8;

  return 0;
}
