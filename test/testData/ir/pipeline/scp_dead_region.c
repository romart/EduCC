// A dead arm that is a whole region rather than one block: the 'if (0)' body
// holds its own diamond, so several blocks die at once and they feed each
// other - the inner join's phi reads values defined in the inner arms, and
// the trailing '+ 1' reads that phi. Removal therefore has to converge
// rather than succeed in one sweep, which is what the repeat-until-empty
// loop in unlinkAndEraseInstructions() is for: a block only becomes erasable
// once the blocks reading it are gone.
//
// Nothing of the region may survive, and the live 'a - b' computed before it
// must come through untouched.
int scp_dead_region(int a, int b, int c) {
    int x = a - b;
    if (0) {
        if (c > 0) {
            x = a + b;
        } else {
            x = a * b;
        }
        x = x + 1;
    }
    return x;
}
