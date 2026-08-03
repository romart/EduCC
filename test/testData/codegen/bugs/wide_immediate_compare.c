// A 64-bit value compared against a literal too wide for the sign-extended
// imm32 form compares against the low half of it instead, and the answer is
// whatever that comes out as. Found by test/testData/codegen/experimental/
// calls.c, which wanted a wide argument and a wide return value and could not
// check either without also comparing one.
//
// x86-64 has no 'cmp r64, imm64': the widest immediate any ALU instruction
// takes is 32 bits, sign-extended to the operand width. A value outside that
// range has to be materialized into a register first and compared register to
// register. The legacy backend does not - generateExpression hands the constant
// straight to emitArithConst, which emits the imm32 form with the low half of
// the value and drops the top - so every comparison below is against a
// different number from the one written.
//
// Reachable from either pipeline, which is why the test is written against a
// global rather than a parameter. The *new* backend gets this right on its own
// account: x86IsLegalImmediate refuses to fold a constant outside the imm32
// range into a compare, so it materializes one and compares registers, and the
// same code with the operands in registers passes under -experimental. But a
// global is read with a load, loads have no selection rule yet, so a function
// containing one falls back to the legacy backend and is wrong in both
// configurations. When loads do get a rule this fixture will start passing
// under -experimental only - at which point it wants rewriting to keep both
// halves honest, not deleting.
//
// Every expected value here is the ordinary meaning of the operator; gcc
// agrees with all of them.

long gWide = 0x0123456789abcdefL;
long gSmall = 5;
unsigned long gUnsigned = 0xfedcba9876543210UL;

int main(void) {
    int failures = 0;

    // Equality, both ways round. The low half of 0x0123456789abcdef is
    // 0x89abcdef, so the comparison is against a sign-extended version of that
    // and comes out unequal.
    if (gWide != 0x0123456789abcdefL) failures += 1;
    if (!(gWide == 0x0123456789abcdefL)) failures += 2;

    // Ordering, which is where the wrong immediate is not merely unequal but
    // on the wrong side.
    if (!(gSmall < 0x0123456789abcdefL)) failures += 4;
    if (gWide < 0x0123456789abcdefL) failures += 8;

    // Unsigned, whose immediate is sign-extended just the same.
    if (gUnsigned != 0xfedcba9876543210UL) failures += 16;

    // The boundary either side of it: 0x7fffffff is the largest immediate the
    // short form encodes and must keep working, and one more than that is the
    // smallest that does not.
    if (gSmall >= 2147483647L) failures += 32;
    if (gSmall >= 2147483648L) failures += 64;

    return failures;
}
