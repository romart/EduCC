int short_circuit(int a, int b, int c) {
    if (a > 0 && b > 0 || c > 0) {
        return 1;
    }
    return 0;
}
