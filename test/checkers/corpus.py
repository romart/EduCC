"""Shared plumbing for the structural checkers in this directory.

Every checker is the same walk: compile each fixture under the given roots,
ask for one of the compiler's dumps, and assert an invariant over it. They all
take the same two arguments so that none of them needs its own instructions:

    python3 test/checkers/<checker>.py build/bin/main test/testData/codegen

Anything beginning with '-' is passed through to the compiler on every
invocation, which is how one checker covers a second configuration rather than
being copied:

    python3 test/checkers/allocation.py build/bin/main -Xregalloc=trivial \
            test/testData/codegen

A checker reports findings on stdout and exits nonzero if it has any. A file
the compiler refuses outright is counted as uncompilable rather than as a
finding: a checker has nothing to say about a file that never reached the
stage it reads.
"""
import os
import subprocess
import sys

# See test/link/reader_test.py: a sanitized EduCC leaks its arenas at exit by
# design, and a checker that links has to be able to run against one.
os.environ.setdefault("ASAN_OPTIONS", "detect_leaks=0")


# Flags '--' passed through to every compiler invocation, filled in by
# parseArgs. A module global rather than a parameter threaded through each
# checker: every one of them invokes the compiler from two or three places,
# and none of them has any business inspecting the flags.
flags = []


def parseArgs(doc):
    global flags
    args = sys.argv[1:]
    if len(args) < 2:
        print(doc)
        sys.exit(2)
    compiler = args[0]
    flags = [a for a in args[1:] if a.startswith("-")]
    roots = [a for a in args[1:] if not a.startswith("-")]
    if not roots:
        print(doc)
        sys.exit(2)
    return compiler, roots


def targetName():
    """The machine the pass-through flags name, as a fixture marker spells it."""
    for i, flag in enumerate(flags):
        if flag.startswith("-march="):
            return flag[7:]
        if flag == "-march" and i + 1 < len(flags):
            return flags[i + 1]
    return "x86_64"


def sources(roots, backend="ir"):
    """The .c fixtures under `roots`, minus the ones that belong elsewhere.

    A '<name>.legacy' / '<name>.ir' sibling marks a fixture the other
    backend is not going to be taught to agree with (see test/testRunner.py);
    running a checker over the IR of a file the IR backend is not meant to
    compile only produces noise. A '<name>.x86_64' / '<name>.riscv64' sibling
    says the same about a target - long double is one machine's and not the
    other's - and is read the same way.
    """
    other = ".legacy" if backend == "ir" else ".ir"
    target = targetName()
    otherTargets = [t for t in (".x86_64", ".riscv64") if t != "." + target]

    files = []
    for root in roots:
        for dirpath, _, names in os.walk(root):
            for n in names:
                if not n.endswith(".c"):
                    continue
                markers = [other] + otherTargets
                if any(os.path.exists(os.path.join(dirpath, n[:-2] + m))
                       for m in markers):
                    continue
                files.append(os.path.join(dirpath, n))
    files.sort()
    return files


def dump(compiler, source, phase, out, obj):
    """Compile `source`, snapshotting the given pass into `out`. False if it did not compile."""
    r = subprocess.run([compiler] + flags + ["-oneline", "-c",
                        "-irDump:" + phase, out, "-o", obj, source],
                       capture_output=True)
    return r.returncode == 0 and os.path.exists(out)


def report(scanned, failed, findings, what):
    for f in findings:
        print("PROBLEM: " + f)
    print()
    print(f"files scanned: {scanned}, uncompilable: {len(failed)}, {what}")
    for f in failed:
        print(f"    failed: {f}")
    return 1 if findings else 0
