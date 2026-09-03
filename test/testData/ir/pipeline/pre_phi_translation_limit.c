// Records a known limitation, so that lifting it is a deliberate change to
// this baseline rather than an accident. With a *variable* index the address
// of 'arr[i]' is a chain - scale the index, then add it to the base - and PRE
// folds only its first link: the index scaling is partially redundant, so it
// gets a clone on the else edge and a phi at the join. The IR_GET_ELEMENT_PTR
// built on top of that phi is then refused, because its input is now defined
// in the join block itself and cloning it into a predecessor would require
// translating that input back across the edge (see inputsStrictlyDominate()
// in gvn.c). Everything downstream - the GEP, the load, the addition - is
// therefore recomputed at the join.
//
// This is a soundness guard, not a bug: without phi translation the clone
// would read a value that does not exist in the predecessor. array_index_pre.c
// is the same program with a constant index, where no phi intervenes and the
// whole address does fold.
int pre_phi_translation_limit(int arr[], int i, int k, int flag) {
    int x = 0;
    if (flag > 0) {
        x = arr[i] + k;
    }
    return x + (arr[i] + k);
}
