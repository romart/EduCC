// An include guard whose '#endif' is the last line of the file with no newline
// after it - the shape of most real headers. Consuming the trailing newline
// then reaches the end of the file and pops the lexer state, releasing the
// condition frame that '#endif' was about to pop and release itself.
//
// Included twice so that the guard has to still work: the frame bookkeeping
// has to survive the fix, not just avoid the crash.
#include "endif_at_eof.h1"
#include "endif_at_eof.h1"

int betweenIncludes;

// The same shape in the main file, where the lexer state being popped has no
// parent to return to. This '#endif' also ends the file without a newline.
#ifdef ENDIF_AT_EOF_UNDEFINED
int notThis;
#else
int butThis;
#endif