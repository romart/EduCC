int switch_stmt(int x) {
    int result;
    switch (x) {
        case 0:
            result = 10;
            break;
        case 1:
            result = 20;
            break;
        default:
            result = -1;
            break;
    }
    return result;
}
