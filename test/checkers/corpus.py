"""Shared plumbing for the structural checkers in this directory.

Every checker is the same walk: compile each fixture under the given roots,
ask for one of the compiler's dumps, and assert an invariant over it. They all
take the same two arguments so that none of them needs its own instructions:

    python3 test/checkers/<checker>.py build/bin/main test/testData/codegen

A checker reports findings on stdout and exits nonzero if it has any. A file
the compiler refuses outright is counted as uncompilable rather than as a
finding: a checker has nothing to say about a file that never reached the
stage it reads.
"""
import os
import subprocess
import sys


def parseArgs(doc):
    if len(sys.argv) < 3:
        print(doc)
        sys.exit(2)
    return sys.argv[1], sys.argv[2:]


def sources(roots, backend="ir"):
    """The .c fixtures under `roots`, minus the ones that belong to the other backend.

    A '<name>.legacy' / '<name>.ir' sibling marks a fixture the other
    backend is not going to be taught to agree with (see test/testRunner.py);
    running a checker over the IR of a file the IR backend is not meant to
    compile only produces noise.
    """
    other = ".legacy" if backend == "ir" else ".ir"
    files = []
    for root in roots:
        for dirpath, _, names in os.walk(root):
            for n in names:
                if n.endswith(".c") and not os.path.exists(os.path.join(dirpath, n[:-2] + other)):
                    files.append(os.path.join(dirpath, n))
    files.sort()
    return files


def dump(compiler, source, phase, out, obj):
    """Compile `source`, snapshotting the given pass into `out`. False if it did not compile."""
    r = subprocess.run([compiler, "-oneline", "-c",
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
