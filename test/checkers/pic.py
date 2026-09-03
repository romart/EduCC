#!/usr/bin/env python3
"""Position-independence checker for '-fPIC' (docs/ir-codegen-design.md step 51).

    python3 test/checkers/pic.py build/bin/main test/testData/codegen

A shared object may not contain a PC-relative reference to a preemptible
symbol: another object may define the same name, so its address is not known
until load time and has to be read out of the GOT. '-fPIC' is the request for
that, and 'ld -shared' is the oracle for whether it happened - the linker
refuses the relocation by name and says which symbol:

    relocation R_X86_64_PC32 against symbol `counter' can not be used
    when making a shared object; recompile with -fPIC

So the corpus walk is: compile each fixture with '-fPIC -c', hand the object to
'ld -shared', and read a nonzero exit as a finding. That covers the whole class
without this file having to know which relocation types are position-independent
or which symbols are preemptible, both of which ld already knows.

The walk says the object *could* be a shared library. It does not say one
works, so the first check here is a round trip that does: a library and a
program built against it, run, and asked for the answer. It uses its own two
sources rather than a fixture, because what it needs is a global that both
halves can see and a static that only one can, which no fixture has a reason to
be shaped like.
"""
import os
import shutil
import subprocess
import sys
import tempfile

import corpus

# A global the program writes and the library reads, a static of the same name
# in each half, and a string literal - the three things whose addressing '-fPIC'
# changes. The static pair is the point of the exercise: 'hidden' must stay two
# separate objects, so a GOT load in place of every address would be as wrong as
# no GOT load at all.
LIBRARY = r"""
int shared = 40;
static int hidden = 1;
const char *libName = "library";

static int helper(int x) { return x + hidden; }

int libBump(int n) {
  hidden += 1;
  shared += n;
  return helper(shared);
}

const char *libWho(void) { return libName; }
int libHidden(void) { return hidden; }
"""

PROGRAM = r"""
extern int shared;
extern int libBump(int);
extern const char *libWho(void);
extern int libHidden(void);

static int hidden = 100;
static int strEq(const char *a, const char *b) {
  while (*a && *a == *b) { ++a; ++b; }
  return *a == *b;
}

int main(void) {
  if (libBump(2) != 44) return 1;    // 40 + 2, plus the library's hidden, now 2
  if (shared != 42) return 2;        // the program and the library share it
  if (libHidden() != 2) return 3;
  if (hidden != 100) return 4;       // and do not share this one
  if (!strEq(libWho(), "library")) return 5;
  return 0;
}
"""


def roundTrip(compiler, tmp):
    """Build a shared object and a program against it, run it. A finding or None."""
    lib, prog = os.path.join(tmp, "lib.c"), os.path.join(tmp, "prog.c")
    with open(lib, "w") as f:
        f.write(LIBRARY)
    with open(prog, "w") as f:
        f.write(PROGRAM)

    so, exe = os.path.join(tmp, "libround.so"), os.path.join(tmp, "round")
    for cmd, what in (([compiler] + corpus.flags + ["-fPIC", "-shared", "-o", so, lib],
                       "building the shared object"),
                      ([compiler] + corpus.flags + ["-o", exe, prog, "-L" + tmp,
                        "-lround", "-Wl,-rpath," + tmp], "linking against it")):
        r = subprocess.run(cmd, capture_output=True)
        if r.returncode != 0:
            return "%s failed: %s" % (what, r.stderr.decode(errors="replace").strip())

    r = subprocess.run([exe], capture_output=True)
    if r.returncode != 0:
        return ("the round trip returned %d; the checks it runs are numbered in "
                "PROGRAM at the top of this file" % r.returncode)
    return None


def checkSource(compiler, source, obj, so):
    """None if the file did not compile, else the findings for it."""
    r = subprocess.run([compiler] + corpus.flags + ["-oneline", "-fPIC", "-c",
                        "-o", obj, source], capture_output=True)
    if r.returncode != 0 or not os.path.exists(obj):
        return None

    # -z undefs: a fixture's object on its own names libc, and undefined
    # symbols are a shared object's normal condition rather than the question
    # being asked here.
    r = subprocess.run(["ld", "-shared", "-z", "undefs", "-o", so, obj],
                       capture_output=True)
    if r.returncode == 0:
        return []

    # ld reports one line per offending relocation and there may be many of
    # them for one symbol; the first two lines are enough to name it.
    lines = [l for l in r.stderr.decode(errors="replace").splitlines() if l.strip()]
    return ["%s: %s" % (source, " / ".join(lines[:2]))]


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    if not shutil.which("ld"):
        print("ld not found; this checker needs it as its oracle")
        return 2
    files = corpus.sources(roots)

    findings, failed = [], []
    with tempfile.TemporaryDirectory() as tmp:
        trip = roundTrip(compiler, tmp)
        if trip:
            findings.append("shared object round trip: " + trip)

        obj, so = os.path.join(tmp, "p.o"), os.path.join(tmp, "p.so")
        for f in files:
            fs = checkSource(compiler, f, obj, so)
            if fs is None:
                failed.append(f)
            else:
                findings += fs

    return corpus.report(len(files), failed, findings,
                         "round trip: " + ("failed" if trip else "ok"))


if __name__ == "__main__":
    sys.exit(main())
