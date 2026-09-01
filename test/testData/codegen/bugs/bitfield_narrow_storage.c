// Assigning an enum-typed value to a bit field.
//
// encodeBitField (src/ir/ast2ir.c) builds the read-modify-write of the storage
// unit - '(old & ~mask) | ((value << offset) & mask)' - at the width of the
// unit the field was packed into, which for a handful of one-bit 'unsigned'
// fields is a single byte. The value arrives at its own width, and for every
// arithmetic type sema had already inserted the cast that makes the two agree.
// An enum is the one that slips through: it stays an int, so the shift ran on
// an I32 operand as if it were a U8 - three bytes wider than the operation
// reading it - and validateOperandWidths caught it.
//
// EduCC's own 'Boolean' is such an enum and 'pp.h' is full of one-bit fields,
// so 'd->isVararg = isVararg' in src/pp.c is where this turned up: the last
// two of its own source files that '-experimental' could not compile.
//
// The exit code is the number of the first check that failed. gcc returns 0.

typedef enum _Flag { OFF = 0, ON = 1 } Flag;
typedef enum _Level { LOW = 1, MID = 5, HIGH = 7 } Level;

// Four bits in total, so the whole thing is packed into one byte.
struct Flags {
    unsigned first : 1;
    unsigned level : 3;
};

int failures = 0;

static void check(int ok, int id) {
    if (!ok && failures == 0) failures = id;
}

static struct Flags global;

static void setGlobal(Flag v) { global.first = v; }
static void setThroughPointer(struct Flags *p, Level v) { p->level = v; }

int main(void) {
    struct Flags local;

    setGlobal(ON);
    check(global.first == 1, 1);

    global.level = HIGH;
    check(global.level == 7, 2);
    check(global.first == 1, 3);

    setGlobal(OFF);
    check(global.first == 0, 4);
    check(global.level == 7, 5);

    local.first = ON;
    setThroughPointer(&local, MID);
    check(local.first == 1, 6);
    check(local.level == 5, 7);

    // A value that does not fit truncates to the field's width and leaves its
    // neighbour alone.
    setThroughPointer(&local, (Level)0xFF);
    check(local.level == 7, 8);
    check(local.first == 1, 9);

    return failures;
}
