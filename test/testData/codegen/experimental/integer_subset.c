// Everything stage 3 can emit today, run for its answer rather than diffed
// against a dump. The suite this lives in compiles it twice - once normally and
// once with -experimental (see codegen_experimental in CMakeLists.txt) - so a
// wrong encoding shows up as a wrong number either way round.
//
// The shape is deliberate. Every function below is free of calls, loads and
// stores, because those are the three things instruction selection has no rule
// for yet: a function containing one is left to the legacy backend and would
// test nothing here. main() is the exception and is meant to be - it is all
// calls, so it always falls back, and every result it checks therefore crosses
// the boundary between the two backends through the ordinary SysV ABI. That
// the two agree about argument and return registers is the thing this fixture
// checks that none of the golden-dump suites can.
//
// Values are the arithmetic's own, checked against gcc.

// Two-address arithmetic, which is where the copy the selector inserts to make
// 'dst <- lhs' explicit either survives allocation or does not.
int arith(int a, int b) {
    return a + b - (a - b) + a * b;
}

// Division names fixed registers - the dividend in rdx:rax, the quotient in rax
// and the remainder in rdx - so the allocator has to leave those alone and put
// the divisor somewhere else. Signed and unsigned differ in how the high half
// is set up: cdq sign-extends, a zeroing move does not.
int divmod_signed(int a, int b) {
    return a / b + a % b;
}

unsigned divmod_unsigned(unsigned a, unsigned b) {
    return a / b + a % b;
}

// A negative dividend is what tells idiv from div: -7/2 is -3 with the signed
// instruction and an enormous positive number with the unsigned one.
int divmod_negative(int a, int b) {
    return a / b * 100 + a % b;
}

// A variable shift count has to be in cl and a constant one is an immediate, so
// the two take different paths through selection and through emission.
int shifts(int v, int n) {
    return (v << n) + (v >> n) + (v << 3) + (v >> 2);
}

unsigned shifts_unsigned(unsigned v, unsigned n) {
    return (v >> n) + (v >> 5);
}

// Comparisons materialize a boolean through setcc, which writes one byte and
// leaves the other three as it found them. Six of them in one expression means
// six zeroing moves whose values have to survive being spilled.
int compares(int a, int b) {
    return (a == b) + (a != b) * 2 + (a < b) * 4 + (a <= b) * 8
         + (a > b) * 16 + (a >= b) * 32;
}

// Unsigned comparisons use a different condition code for the same operator,
// and get it wrong invisibly unless one of the operands has the high bit set.
int compares_unsigned(unsigned a, unsigned b) {
    return (a < b) + (a > b) * 2 + (a <= b) * 4 + (a >= b) * 8;
}

// A loop is a back edge, a phi, and the copies stage 0 leaves on the incoming
// edges to destroy it.
int loop_sum(int n) {
    int sum = 0;
    for (int i = 1; i <= n; ++i) {
        sum += i;
    }
    return sum;
}

// Two values swapping across a back edge is the case parallel copies exist for:
// sequentializing them in the wrong order loses one.
int fib(int n) {
    int a = 0, b = 1;
    while (n > 0) {
        int t = a + b;
        a = b;
        b = t;
        --n;
    }
    return a;
}

// Nested loops with a conditional inside, so the layout has more than one
// candidate for the fallthrough successor and some branch has to be inverted.
int nested(int n) {
    int acc = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if ((i + j) % 3 == 0) {
                acc += i * j;
            } else {
                acc -= 1;
            }
        }
    }
    return acc;
}

// '&&' and '||' become branches and a phi, not arithmetic, and the right-hand
// side must not be evaluated when the left decides the answer. Division by zero
// is how that gets noticed: if the guard is not honoured this traps.
int short_circuit(int a, int b) {
    if (a != 0 && b / a > 2) {
        return 1;
    }
    if (a == 0 || b / a < -2) {
        return 2;
    }
    return 3;
}

// A 64-bit constant does not fit the sign-extended imm32 form and has to be
// materialized with movabs. Getting this wrong silently keeps the low half,
// which is what happened before integer literals were typed by their value.
long big_constants(long i) {
    return (i & 0x7ff0000000000000L) + (i | 0x000fffffffffffffL);
}

// The boundaries of the immediate encoding: one below, one above, and the two
// values where a 32-bit immediate stops being sign-extendable.
long immediate_edges(long v) {
    return (v + 2147483647L) + (v + 2147483648L) + (v - 2147483648L)
         + (v & 4294967295L);
}

// Constant folding across a narrowing conversion. 'char' is signed here, so
// (char)300 is 44 and (char)200 is -56; a fold that forgets to narrow leaves
// 300 and 200 behind.
int narrowing_constants(void) {
    return (char)300 + (char)200 + (unsigned char)300 + (short)70000;
}

// The unsigned-to-wider case: (unsigned)-1 is 4294967295, and widening it to a
// long must not sign-extend on the way.
long widening_constants(void) {
    return (long)(unsigned)-1 + (long)(int)-1;
}

// Bitwise operations and their identities, with an operand wide enough that a
// missing REX.W would show.
long bitwise(long a, long b) {
    return (a & b) + (a | b) + (a ^ b) + (~a) + (-b);
}

int main(void) {
    int failures = 0;

    if (arith(7, 3) != 7 + 3 - (7 - 3) + 7 * 3) failures += 1;
    if (divmod_signed(17, 5) != 17 / 5 + 17 % 5) failures += 2;
    if (divmod_unsigned(4000000000u, 7u) != 4000000000u / 7u + 4000000000u % 7u) failures += 4;
    if (divmod_negative(-7, 2) != -7 / 2 * 100 + -7 % 2) failures += 8;
    if (shifts(9, 2) != (9 << 2) + (9 >> 2) + (9 << 3) + (9 >> 2)) failures += 16;
    if (shifts_unsigned(4000000000u, 3u) != (4000000000u >> 3) + (4000000000u >> 5)) failures += 32;
    if (compares(4, 4) != 1 + 8 + 32) failures += 64;
    if (compares(4, 9) != 2 + 4 + 8) failures += 128;
    if (compares_unsigned(1u, 4000000000u) != 1 + 4) failures += 256;
    if (loop_sum(10) != 55) failures += 512;
    if (fib(10) != 55) failures += 1024;
    if (nested(5) != 34) failures += 2048;
    if (short_circuit(0, 100) != 2) failures += 4096;
    if (short_circuit(10, 100) != 1) failures += 8192;
    if (short_circuit(10, 20) != 3) failures += 16384;
    if (big_constants(0x7ff8000000000001L) !=
        (0x7ff8000000000001L & 0x7ff0000000000000L) + (0x7ff8000000000001L | 0x000fffffffffffffL))
        failures += 32768;
    if (immediate_edges(1000L) !=
        (1000L + 2147483647L) + (1000L + 2147483648L) + (1000L - 2147483648L) + (1000L & 4294967295L))
        failures += 65536;
    if (narrowing_constants() != 44 + (-56) + 44 + 4464) failures += 131072;
    if (widening_constants() != 4294967295L + (-1L)) failures += 262144;
    if (bitwise(0x1234567890abcdefL, 0x0fedcba098765432L) !=
        (0x1234567890abcdefL & 0x0fedcba098765432L) + (0x1234567890abcdefL | 0x0fedcba098765432L)
        + (0x1234567890abcdefL ^ 0x0fedcba098765432L) + (~0x1234567890abcdefL)
        + (-0x0fedcba098765432L))
        failures += 524288;

    return failures;
}
