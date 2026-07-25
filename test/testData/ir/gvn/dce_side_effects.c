// Three unused values, only two of which may be removed. The 'unused'
// product and its feeding chain are pure and read by nobody, so they go -
// and they go transitively, which is what the fixed point is for: the
// multiply dies first, and only then do its operands become unused.
// 'alsoUnused' is the same story one step further out. The call must survive
// untouched even though its result is read only by an instruction that is
// itself deleted; a rule keyed on "has no uses" without consulting side
// effects would delete it and change what the program does.
//
// Worth knowing when reading the baselines: this sweep has already happened
// by the time the gvn phase is dumped, because buildSSA runs
// cleanupDeadInstructions itself - the .gvn.txt and .dce.txt snapshots are
// identical here, and what they pin down is that the property holds and is
// preserved, not that the final dce pass is what established it. The
// superseded-instruction cleanup that dce does own is covered by every other
// fixture in this directory.
//
// The subtraction is the interesting survivor: 'a - b' appears both in the
// dead product and in the return, gets folded into one, and must then be
// kept alive because one of its two users remains.
extern int sink(int v);

int dce_side_effects(int a, int b) {
    int unused = (a + b) * (a - b);
    int kept = sink(a);
    int alsoUnused = kept * 2;
    return a - b;
}
