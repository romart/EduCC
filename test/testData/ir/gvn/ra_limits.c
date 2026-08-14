// The two places the trivial "spill everything" allocator stops being uniform.
// Nothing else in the corpus reaches either, and both were found by looking
// for them rather than by a test failing.
//
//   ra_byte_setcc     A setcc whose destination is one byte wide. Everywhere
//                     else the destination is an int, so selection puts a
//                     zeroing move in front to define the upper three bytes
//                     and the setcc is written as a read-modify-write of the
//                     whole register - which is what makes the allocator
//                     reload the zero before the setcc rather than store back
//                     three bytes of whatever the comparison left behind. When
//                     the destination really is a byte there is no zeroing
//                     move, so the def is total and the read is left off; a
//                     setcc claiming to read a register nothing has written
//                     would be a use before def.
//
//                     A C comparison is an int however it is written, so this
//                     needs something whose *result* is a _Bool. A conversion
//                     to _Bool is one: it is 'x != 0' rather than a narrowing
//                     (see selectBooleanConversion), and the register it writes
//                     is the one byte _Bool occupies.
//
//                     This was a va_arg loop until variadic functions were
//                     refused wholesale - va_arg's lowering compares the
//                     register-save-area offset against 48 and yields an
//                     IR_BOOL, which was then the only source of one. That
//                     refusal is a property of the IR rather than of this file
//                     (see param_va_area, and section 6.11 of
//                     docs/ir-codegen-design.md), and it took the case away
//                     entirely: a function turned away before selection runs
//                     has no setcc of any width in it.
//
//   ra_scratch_budget A single instruction naming more distinct registers than
//                     the target reserves as scratch, which makes the whole
//                     function decline allocation ('Registers: not allocated'
//                     in the dump, virtual registers left as selection had
//                     them). Only a placeholder can do this: nothing stage 1
//                     genuinely selects names more than two, so the limit is
//                     reached exclusively by MOP_UNSELECTED standing in for an
//                     IR instruction with many inputs, and the function it is
//                     in already carries hasUnselected - so nothing that could
//                     be emitted is being turned away.
//
//                     This was a four-argument call of ints until step 7 gave
//                     calls a selection rule, at which point it stopped
//                     reaching the limit: each argument became its own
//                     one-register move and the widest instruction in the
//                     function went back to naming two. 'long double' is what
//                     reaches it now, and durably - the four arguments and the
//                     result are five FP registers against a budget of three,
//                     and lowering x87 arithmetic is soft-float work that
//                     docs/ir-codegen-design.md section 11 puts outside step 7
//                     altogether. Should that ever land, check whether any C
//                     program can still reach this at all before rewriting the
//                     fixture again: the honest answer may be that the refusal
//                     has become defensive, and the place to say so is the
//                     allocator rather than a test that no longer tests it.
//
// Values, checked against both the legacy pipeline and gcc, for whoever turns
// these into executable fixtures at step 6: ra_byte_setcc(0, 0) == 0 and
// ra_byte_setcc(4, 0) == 1. ra_scratch_budget has no definition for its callee
// on purpose - it is never linked, only compiled.

_Bool ra_byte_setcc(int a, int b) {
    return (_Bool)(a | b);
}

long double ra_limits_callee(long double a, long double b, long double c, long double d);

long double ra_scratch_budget(long double a, long double b) {
    return ra_limits_callee(a, b, a + b, a - b);
}
