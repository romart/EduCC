// When a constant becomes an immediate and when it has to take a register.
//
// Constants are hoisted into the entry block, so one that gets a register holds
// it for the whole function. Stage 1 therefore decides per constant, not per
// use, and folds one only when *every* use can take it as an immediate - the
// alternative is a function carrying the same value in both forms. Each line
// below is one way that decision can go, and the dump is where they are told
// apart: a folded constant appears as a bare number inside an instruction and
// as no instruction of its own, a materialized one as a 'mov' in the entry
// block that everything afterwards names by register.
//
//   'small'    only ever added to. ALU immediates are 32 bits sign-extended,
//              this fits, and nothing else uses it: folded.
//   'huge'     0x1234567890 does not fit in those 32 bits, so it has to be
//              materialized even though its only use is an add that would
//              otherwise have taken it.
//   'shared'   100 is used twice: by an add, which could take an immediate, and
//              by a divide, which on x86 has no immediate form at all. One use
//              that cannot fold settles it for all of them - the constant gets
//              a register and the add reads it from there, rather than the
//              function keeping the same value in both forms.
//   'scaled'   a shift count is an 8-bit immediate rather than a 32-bit one, a
//              different limit and so its own case.
//   'left'     the constant is the *left* operand of a subtract. x86 encodes an
//              immediate as the source, so there is nowhere to put it: also a
//              register, despite being small enough.
//
// Everything is spelled 'long' deliberately, constants included. A narrower
// literal in a 64-bit expression makes the IR ask for an operation on operands
// narrower than its result, which is a real gap but a different one, and it
// would show up here as width-mismatched copies that have nothing to do with
// immediates (see docs/ir-codegen-design.md section 10).
long isel_immediates(long x, long d) {
    long small = x + 7L;
    long huge = x + 0x1234567890L;
    long shared = (x + 100L) + 100L / d;
    long scaled = x << 5L;
    long left = 3L - x;

    return small + huge + shared + scaled + left;
}
