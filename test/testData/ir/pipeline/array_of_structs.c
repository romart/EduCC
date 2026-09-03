// 'items[i].weight' is the deepest address chain in the suite: an IR_E_SHL
// scaling the index by the element size, an IR_GET_ELEMENT_PTR selecting the
// array element, and a second IR_GET_ELEMENT_PTR selecting the field. Value
// numbering has to fold the whole chain link by link - each step is only
// recognized as redundant because the step feeding it already was - so a
// single mis-hashed level (say a GEP keyed without its member, or a shift
// whose constant operand is not part of the expression) leaves the rest of
// the chain duplicated and shows up immediately in the baseline.
//
// Both loads survive, as always. Note the two distinct constants involved -
// the element-size shift and the field offset - which the expression keys
// must keep apart.
struct Item {
    int id;
    int weight;
};

int array_of_structs(struct Item *items, int i, int k) {
    int w1 = items[i].weight;
    int w2 = items[i].weight;
    return w1 + w2 + k;
}
