// Frame layout for the arguments the caller had to leave on the stack.
//
// SysV hands the first six integer arguments over in registers, so 'g', 'h'
// and the by-value struct arrive in memory the caller wrote, above the frame
// pointer - past the saved frame pointer at +0 and the return address at +8,
// hence +16 onwards. Those are not slots this frame allocates; they are listed
// as frame objects only so that everything addressed through a frame index is
// addressed the same way.
//
// 'arr' is the contrast: address-taken, so mem2reg leaves it in memory and it
// gets a real slot below the frame pointer. One function therefore pins down
// both directions of the frame at once, and 's' pins down that an oversized
// argument advances the incoming cursor by its whole size rather than by a
// word - two stack arguments landing on the same offset is otherwise silent.
struct Big { long a, b, c; };

int frame_stack_params(int a, int b, int c, int d, int e, int f, int g, int h, struct Big s) {
    int arr[4];

    arr[0] = g;
    arr[1] = h;

    return arr[0] + arr[1] + (int)s.a;
}
