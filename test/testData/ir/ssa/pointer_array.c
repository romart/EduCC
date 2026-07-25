// Regression test for translateDeReference() (src/ir/ast2ir.c): it used to
// take the address of `p` itself instead of loading p's value first, so
// '*p = *p + 1' behaved like 'p = p + 1'. Since every value reference is
// parsed as EU_DEREF(E_NAMEREF), an explicit '*p' is EU_DEREF(EU_DEREF(p))
// and the inner deref has to be evaluated as an rvalue (a load) to yield the
// address the outer one writes through. The baseline must therefore keep the
// LOAD/ADD/STORE of *p distinct from the arr+index computation.
int pointer_array(int *arr, int index) {
    int *p = arr + index;
    *p = *p + 1;
    return *p;
}
