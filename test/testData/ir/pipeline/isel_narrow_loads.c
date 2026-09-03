// What a byte or halfword load leaves above the value, and who is allowed to
// believe it.
//
// A narrow load widens as it loads rather than writing part of a register
// (selectMemoryLoad), so the register holds four bytes of extended value where
// the IR says the value is one. Nothing used to record that, so the next use
// extended it a second time - 'movzx.4/1 [%v5 + %v7*1]' followed by
// 'movzx.4/1 %v8', which is the same extension applied to its own output. The
// note that stops it is VRegInfo.extendedTo (section 6.28).
//
// The other half of it is in the VRegs table rather than in the instructions:
// a note is only true while the register is wide enough to hold it, since a
// one-byte register is a one-byte spill slot. So the register widens - but
// only when a use actually takes the note, which is why some of the loads
// below are 'gp/4' and some are still 'gp/1'. Widening every one of them
// costs more than it saves: a four-byte register no longer coalesces with the
// one-byte value a byte operation defines.
//
// This is a dump fixture rather than a runtime one because the whole subject
// is invisible from an exit code - the second extension was correct, it was
// merely the first one repeated.
//
// What each function is for:
//
//   promoted     the plain shape. One 'movsx.4/1' for the load, and the cast
//                to int that follows it is a copy the coalescer deletes.
//   unpromoted   the same for an unsigned byte, which is the other extension.
//   halfword     two bytes rather than one; the note is still four.
//   mixedSource  a byte out of memory beside a byte out of a register, and a
//                second load nothing widens at all. The first load's note is
//                taken and its register is four bytes; the incoming parameter
//                has no note and is widened the old way; the second load's
//                note is never read and its register stays one byte. Three
//                outcomes, in one function.
//   toWide       used as a 64-bit index. The note says four and the use wants
//                eight, so it is not taken - the widening stays, it starts
//                from the value's own width, and the register is not widened
//                for a note nobody could use.
//   byteLoop     the roadmap's case: a load through an addressing mode inside
//                a loop, where the second extension was one instruction of two
//                in the body.
//   throughPhi   the load's register is also a phi copy's source, which is a
//                register stage 0 named before selection ran. Widening it
//                afterwards has to leave that copy reading the low bytes it
//                always read.

signed char promoted(const signed char *p) {
  int n = *p;
  return (signed char)(n + 1);
}

int unpromoted(const unsigned char *p) {
  return *p + 1;
}

int halfword(const short *p) {
  return *p + 1;
}

void mixedSource(const char *src, char *dst, char limit) {
  if (*src < limit) {
    *dst = *src;
  }
}

int toWide(const int *a, const signed char *p) {
  return a[*p];
}

int byteLoop(const unsigned char *p, int n) {
  int total = 0;

  for (int i = 0; i < n; ++i) {
    total += p[i];
  }

  return total;
}

int throughPhi(const signed char *p, int n) {
  int c = 0;

  if (n > 0) {
    c = *p;
  }

  return c + 1;
}
