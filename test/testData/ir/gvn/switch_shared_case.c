// The only fixture that drives critical-edge splitting through an
// IR_TBRANCH. Grouped labels ('case 0:' falling into 'case 1:') give the
// shared bodies more than one predecessor while the switch block still has
// many successors, so those edges - and the default edge into the join - are
// critical and must be split before anything is inserted on them.
//
// Splitting a switch edge is the one path in updateTerminatorTarget() that
// rewrites a SwitchTable rather than a branch's taken/notTaken pair: each
// affected case entry, and the default target, has to be repointed at its
// new splitter block, one per split, without disturbing the entries already
// redirected by an earlier split. Getting that wrong silently sends a case
// to the wrong block, which no other fixture would catch. PRE then uses two
// of the new splitters to hold the clones of 'a + b' it needs at the join.
int switch_shared_case(int a, int b, int sel) {
    int r = 0;
    switch (sel) {
    case 0:
    case 1:
        r = a + b;
        break;
    case 2:
    case 3:
        r = a - b;
        break;
    }
    return r + (a + b);
}
