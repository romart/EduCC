



// int test_integral_arith(int a, int b) {
//     int result = (a + b) * (a + b) + (b + a) * (b + a);  // Same expression repeated, should be optimized
//     return result;
// }
//
// int test_pointer_arith(int *arr, int index) {
//     int *ptr1 = arr + index;
//     int *ptr2 = arr + index; // Same calculation, should be optimized
//     int result = *ptr1 + *ptr2; // Dereferencing same pointer twice, should be optimized
//     return result;
// }

int test_pointer_arith_conditional(int *arr, int index, int flag) {
    int *ptr1, *ptr2;
    if (flag > 0) {
        ptr1 = arr + index;  // Same pointer calculation as in the else branch
        ptr2 = arr + index;
    } else {
        ptr1 = arr + index;  // Same calculation, should be optimized
        ptr2 = arr + index;
    }
    int result = *ptr1 + *ptr2;  // Dereferencing same pointer twice
    return result;
}

// int test_combined_redundant_expressions(int n) {
//     int result = 0;
//     for (int i = 0; i < n; i++) {
//         int x = i * i;  // Redundant computation
//         int y = i * i;  // Same as above, should be optimized
//         result += x + y; // Redundant addition
//     }
//     return result;
// }
