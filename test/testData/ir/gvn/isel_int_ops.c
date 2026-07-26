// The integer operations the rest of the corpus never happens to produce.
//
// The fixtures here grew out of optimization passes, so what they select into
// is whatever those passes left behind: adds, multiplies, signed compares. That
// leaves most of stage 1's dispatch table unexercised, and an arm nothing runs
// is an arm nobody notices is wrong. This function reaches the rest of it.
//
// What each line is for:
//
//   ~ and !          the two unary operations, which are not each other. '~x'
//                    is destructive and one instruction; '!x' has to produce a
//                    whole int from a setcc that writes one byte, so it also
//                    has to zero the destination first.
//   << by a variable the count has to be in cl, and nowhere else, so this is
//                    the one arm that emits a copy into a fixed register that
//                    the operand list does not otherwise mention.
//   >> unsigned      right shift is where signedness stops being cosmetic: an
//                    arithmetic shift keeps the sign bit and a logical one does
//                    not, so the two are different opcodes for the same IR.
//   / and % unsigned the divide sequence differs by more than the opcode - a
//                    signed divide sign-extends into rdx:rax and an unsigned
//                    one zeroes rdx - and both leave the quotient and the
//                    remainder in different registers, so both are read out.
//   u < and u >      an unsigned compare is 'below', not 'less'; getting this
//                    wrong is invisible until a value crosses 0x80000000.
//
// The '&', '|' and '^' at the end are the plain two-address arms, here so that
// every binary opcode in the table appears at least once.
//
// The two MOP_UNSELECTED(IR_E_BITCAST) in the baseline are the int-to-unsigned
// conversions on the '~' and '!' results. Casts are not part of the integer
// subset stage 1 covers, so they are expected there, not a symptom.
unsigned isel_int_ops(unsigned a, unsigned b, int s, int k) {
    unsigned acc = 0;

    acc = acc + (unsigned)~s;
    acc = acc + (unsigned)!k;

    acc = acc + (a << k);
    acc = acc + (a >> 3);

    acc = acc + a / b;
    acc = acc + a % b;

    if (a < b) {
        acc = acc + 1;
    }
    if (a > b) {
        acc = acc + 2;
    }

    return acc & (a | (b ^ 0x0f0f0f0f));
}
