// The address of an externally-defined symbol used in a static initializer.
//
// EduCC emits the relocation for these into the data section with symbol index
// 0 - the null symbol - so the linker resolves them to NULL and every pointer
// below reads back as 0. The relocation record itself is created correctly by
// fillReference() in src/codegen_common.c; what goes missing is the symbol
// table entry it should point at, which is why '.text' references to the same
// symbols (the printf calls here) come out fine.
//
// Scope of the bug, all covered below: it applies to functions and to objects
// alike, inside an aggregate initializer and at the top level of a scalar one,
// and 'const' makes no difference. What does make a difference is whether the
// symbol is defined in this translation unit - 'localFn' works, so this is
// specifically about undefined symbols, not about static initializers in
// general. The libc declarations are just a convenient way to name a symbol
// this file does not define while staying a single-file fixture.
//
// This is why a self-hosted EduCC segfaults on every '-experimental' compile:
// the TargetDescriptor tables in src/x86_64/target_x86_64.c initialize
// '.classifyParameters = &classifyParametersGeneric' statically, and that is
// the first place in the compiler's own source to take the address of a
// cross-TU symbol this way. Everything else fills its function pointers at
// runtime (initArchCodegen_x86_64), which is why it stayed latent so long.
//
// Muted - see the sibling .muted file. Fixing it belongs in the ELF symbol
// table emission, not here.
extern int printf(const char *, ...);
extern int abs(int);
extern int atoi(const char *);

typedef struct {
    int (*fp)(int);
    int *vp;
} S;

static int localFn(int x) { return x + 1; }

extern int externObject;
int externObject = 7;

// In an aggregate initializer, referring to a symbol defined elsewhere...
S externInStruct = { &abs, &externObject };
// ...the same, but const, so it lands in .rodata rather than .data...
const S constExternInStruct = { &abs, &externObject };
// ...at the top level of a scalar initializer...
int (*externScalar)(const char *) = &atoi;
// ...and the control: a symbol this file does define, which works today.
S localInStruct = { &localFn, &externObject };

int main(void) {
    int failed = 0;

    if (externInStruct.fp == 0) { printf("externInStruct.fp is NULL\n"); failed = 1; }
    if (externInStruct.vp == 0) { printf("externInStruct.vp is NULL\n"); failed = 1; }
    if (constExternInStruct.fp == 0) { printf("constExternInStruct.fp is NULL\n"); failed = 1; }
    if (externScalar == 0) { printf("externScalar is NULL\n"); failed = 1; }
    if (localInStruct.fp == 0) { printf("localInStruct.fp is NULL\n"); failed = 1; }

    if (failed) return 1;

    // Not just non-NULL but actually the right symbols, so that a fix which
    // emits some arbitrary relocation still gets caught here.
    if (externInStruct.fp(-5) != 5) return 2;
    if (*externInStruct.vp != 7) return 3;
    if (constExternInStruct.fp(-3) != 3) return 4;
    if (externScalar("42") != 42) return 5;
    if (localInStruct.fp(1) != 2) return 6;

    return 0;
}
