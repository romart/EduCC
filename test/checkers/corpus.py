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
import re
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


# What the checkers have to know about each machine, which is very nearly the
# TargetDescriptor the compiler reads - spelled again here because a checker
# cannot ask the compiler for it, and kept in one table rather than one per
# checker so that a machine is described once.
#
# Until roadmap step 59's R6 these were x86-64's constants written into the
# checkers directly, which is why pointing any of them at riscv64 used to
# report findings in the thousands, none of them a bug. link.py keeps a table
# of its own: what it needs is what a *loader* reads, and it identifies the
# machine from the file it is inspecting rather than from a flag.
TARGETS = {
    "x86_64": {
        "name": "x86-64",

        # TargetDescriptor.incomingStackParamOffset: the saved rbp and the
        # pushed return address lie between the frame pointer and the first
        # argument the caller left on the stack.
        "incomingStackParamOffset": 16,

        # Every register that holds something before this function wrote it:
        # what the ABI hands over, and what the frame is addressed by.
        "entryRegs": frozenset(
            {"rdi", "rsi", "rdx", "rcx", "r8", "r9", "rsp", "rbp"} |
            {"xmm%d" % n for n in range(8)}),

        # Whether a target instruction reads its own destination. x86's do,
        # which is what the leading copy is for; RV64G is three-address and
        # none of its do, so the invariant has nothing to say there.
        "twoAddress": True,

        # How a block ends, as the dump spells the opcodes. 'indirect' reaches
        # its successors through a table rather than through an operand.
        "uncond": frozenset({"jmp", "ret", "jmp*"}),
        "indirect": "jmp*",
        "cond": re.compile(r"^j(?!mp)[a-z]+$"),

        # TargetDescriptor.spillRegisterWidth, read the way machineSpillWidth
        # reads it: a spill is max(this, the value's own width). Zero because
        # 'al' is a name the machine has, so the bytes above a narrow value
        # belong to nobody and a reload need not reproduce them.
        "spillRegisterWidth": 0,

        # TargetDescriptor.scratchRegs: what the trivial allocator may use, and
        # so what no value may be live in across a block boundary there.
        "scratchRegs": frozenset({"r10", "r11", "rbx", "xmm8", "xmm9", "xmm10"}),

        # The last IR phase before the machine backend. x87 has instructions
        # for its own long double, so nothing is lowered after 'dce'.
        "lastIrPhase": "dce",

        # Which backends this machine has, for the checkers that cover more
        # than one. riscv64 refuses '-legacy' by name.
        "backends": ("ir", "legacy"),

        # Whether '-fPIC' means anything here.
        "pic": True,

        # The stack pointer as '-S' prints it.
        "sp": "rsp",
        "objdump": "objdump",
    },

    "riscv64": {
        "name": "riscv64",

        # The return address is in ra and s0 points at the top of the frame,
        # so the first stack argument is at the frame pointer itself.
        "incomingStackParamOffset": 0,

        # x0 is hardwired to zero: it holds a value before anything wrote one
        # in the strongest possible sense, and reading it is not a defect.
        "entryRegs": frozenset(
            {"sp", "zero", "ra", "s0"} |
            {"a%d" % n for n in range(8)} |
            {"fa%d" % n for n in range(8)}),

        "twoAddress": False,

        # A conditional branch here is the comparison and the branch at once,
        # so these are six opcodes rather than a setcc/jcc pair.
        "uncond": frozenset({"j", "jr", "ret"}),
        "indirect": "jr",
        "cond": re.compile(r"^b(eq|ne|lt|ge|ltu|geu)$"),

        # No sub-registers: every instruction reads all 64 bits, so a narrow
        # value's register is canonically extended and a spill has to save the
        # whole of it - a reload of the low bytes alone could not put the
        # register back without a signedness the spiller does not have.
        "spillRegisterWidth": 8,

        "scratchRegs": frozenset({"t4", "t5", "t6", "ft9", "ft10", "ft11"}),

        # lowerLongDoubleCalls runs after 'dce' here, and it replaces every
        # operation on a long double with a call - so a value the 'dce' dump
        # names is not necessarily one the machine IR has.
        "lastIrPhase": "softfloat",

        "backends": ("ir",),
        "pic": False,

        "sp": "sp",
        "objdump": "riscv64-linux-gnu-objdump",
    },
}


def target():
    """The facts about the machine this run compiles for. See TARGETS."""
    name = targetName()
    if name not in TARGETS:
        print("no checker knows anything about '%s'" % name)
        sys.exit(2)
    return TARGETS[name]


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
