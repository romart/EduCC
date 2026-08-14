// The address of an *array* stored in a static initializer, with more fields
// after it.
//
// fillReference() in src/codegen_common.c records the relocation and then
// reserves the bytes the linker will write into. It used to reserve
// computeTypeSize(expr->type) of them - the size of the thing being pointed
// at, not the size of a pointer. For '&someInt' or a function those happen to
// come out at or below eight and nothing downstream noticed; for an array they
// do not. 'int arr[16]' names a 64-byte type, so a slot holding its address
// reserved 64 bytes and pushed every later field of the initializer 56 bytes
// down the section, while the symbol's st_size went on describing the struct
// with its proper layout. Everything after the first array-valued field then
// read back garbage.
//
// This is why a self-hosted EduCC could not run its own '-experimental'
// pipeline: src/x86_64/target_x86_64.c's TargetDescriptor is a const struct
// holding the addresses of six arrays, so its tail - including the
// classifyParameters hook the IR backend calls before anything else - sat
// hundreds of bytes past where the rest of the compiler looked for it.
//
// Kept separate from static_init_extern_symbol.c on purpose: that one is about
// which symbol the relocation names, this one is about how much room is left
// for the linker to write it into, and the two were fixed in different files.
//
// The exit code is the number of the first check that failed. gcc returns 0.

extern int printf(const char *, ...);

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

static const int ints[16] = { 11, 22, 33 };
static const long longs[8] = { 44, 55 };
static const char *const names[4] = { "alpha", "beta", "gamma", "delta" };

// Every field after 'p' is what the old layout displaced. The trailing scalars
// matter as much as the pointers: they are read at a fixed offset from the
// symbol, which is exactly what the over-long slot invalidated.
struct Tables {
    const int *ip;
    const long *lp;
    const char *const *np;
    const char *name;
    int tag;
    long stamp;
};

const struct Tables tables = { ints, longs, names, "tables", 0x1234, 0x5678L };

// The same thing outside a struct, so the bug is not confused with anything
// aggregate-specific: two pointers to arrays with a scalar sitting after them.
const int *first = ints;
const long *second = longs;
int sentinel = 0x4321;

// A pointer to one element rather than to the whole array, which was always
// fine - '&ints[2]' has pointer type - kept as the control.
const int *third = &ints[2];

int main(void) {
    check(tables.ip == ints, 1);
    check(tables.lp == longs, 2);
    check(tables.np == names, 3);

    check(tables.ip[0] == 11 && tables.ip[2] == 33, 4);
    check(tables.lp[0] == 44 && tables.lp[1] == 55, 5);
    check(tables.np[0][0] == 'a' && tables.np[3][0] == 'd', 6);

    check(tables.name[0] == 't', 7);
    check(tables.tag == 0x1234, 8);
    check(tables.stamp == 0x5678L, 9);

    check(first == ints, 10);
    check(second == longs, 11);
    check(sentinel == 0x4321, 12);
    check(third == &ints[2] && *third == 33, 13);

    if (failures) printf("first failing check: %d\n", failures);

    return failures;
}
