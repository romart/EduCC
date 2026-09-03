// An aggregate argument too large for a register, in the IR and the machine
// code that decides what to do with it.
//
// The IR names such an argument by a pointer - the address of the temporary
// translateCall copies it into - and so does a genuine pointer argument, which
// is why for a long time the call was refused rather than selected: nothing in
// the instruction said which of the two it was. What settles it is not a type
// but a claim the caller makes, IrInstruction.info.call's memArgs, and that is
// invisible in the IR dump on purpose. Read this fixture through the machine
// baselines: the IR ones show the copy into the temporary and an ordinary
// pointer going into the call, and the isel one shows the pointer never being
// passed at all.
//
// 'pass' is both halves at once - it receives a three-eightbyte struct and
// hands it on - so the .isel baseline has the callee's side (an address
// computed off the frame pointer, since a memory parameter is read where the
// caller left it) and the caller's (three loads and three pushes, highest
// eightbyte first, because the argument list is pushed backwards and the
// struct has to come out the right way up). The X86_SUB/X86_ADD pair around
// them is the padding: three eightbytes is an odd number, and rsp has to be
// 16-byte aligned when the call executes.
//
// 'scalars' is the classification claim. The struct consumes no argument
// register of either class, so 'a' and 'b' keep rdi and rsi rather than being
// pushed along - the opposite of what the hidden return-buffer pointer does in
// composite_return.c, which is an argument and does take one.
//
// 'small' is the other side of the eight-byte line and needs none of this: it
// fits in one register, so it is loaded as an eightbyte and passed like an
// integer. A baseline diff between the two is the whole of the distinction.

struct Big { long a, b, c; };
struct Small { int x, y; };

int takeBig(struct Big s);
int takeSmall(struct Small s);
int mixed(int a, struct Big s, int b);

int pass(struct Big s) {
  return takeBig(s);
}

int scalars(int a, struct Big s, int b) {
  return mixed(a, s, b);
}

int small(struct Small s) {
  return takeSmall(s);
}
