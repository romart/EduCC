// Calls are side-effecting, so two calls with identical arguments must
// each keep their own value number - GVN must never redirect the second
// call's uses to the first one's result (imagine opaque() is rand()).
// Both IR_CALLs must survive in the dump.
extern int opaque(int x);

int calls_not_merged(int x) {
    return opaque(x) + opaque(x);
}
