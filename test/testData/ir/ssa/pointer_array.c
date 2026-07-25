// KNOWN BUG: translateDeReference() (src/ir/ast2ir.c) takes the address of
// `p` itself instead of loading p's value first, so '*p = *p + 1' currently
// behaves like 'p = p + 1' - the baseline below captures that (wrong)
// behavior on purpose so a future fix shows up as an intentional diff here,
// not a silent baseline update. TODO: fix translateDeReference and re-run
// with --update-baselines once it's corrected.
int pointer_array(int *arr, int index) {
    int *p = arr + index;
    *p = *p + 1;
    return *p;
}
