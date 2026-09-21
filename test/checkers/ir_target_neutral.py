#!/usr/bin/env python3
"""The IR before the machine backend is the same on both machines.

    python3 test/checkers/ir_target_neutral.py build/bin/main test/testData/ir

The claim the IR backend has been making since docs/ir-codegen-design.md
section 2 is that a target is *a descriptor and a selector*, not a second copy
of the code generator. Everything downstream of instruction selection is
obviously a target's own; everything upstream is supposed not to be. Nothing
tested that.

So: compile each fixture for x86-64 and for riscv64, dump the same pre-machine
phase from each, and require the two to be identical except where a physical
register is named. The ABI decides which register a parameter arrives in and
that reaches the initial translation, so '$rdi' against '$a0' is the one
difference allowed - and it has to be a *consistent* renaming, or the same
register has been reused for two different things on one side and not the
other.

This is what the riscv64 IR-dump baselines turned out to be. Measured over
every fixture and every phase, the whole difference was the argument
registers' names, so a second set of golden files would have recorded the
first set twice and a rename table. What it would not have recorded is the
claim itself, which a baseline cannot state and this can: a target hook added
to ast2ir shows up here as a shape that differs, not as a file that needs
regenerating.

Seven fixtures are the exception, and they are the ones whose *subject* is
the ABI - where the argument registers run out, what a large aggregate does,
what shape a va_list has. Their IR is supposed to differ, so each is named in
ABI_SPECIFIC below with what about it is the ABI's, and one that stops
differing is reported rather than quietly left in the list.

The machine phases are a different question and do have riscv64 baselines -
'mir', 'isel' and the three 'ra' ones, under '--baseline-tag riscv64'. Those
are where the seven fixtures' riscv64 answers are pinned.

Pointed at test/testData/ir, which is the corpus that exists for exactly this
question. Over test/testData/codegen the claim holds too, but twenty-eight of
those fixtures are ABI-shaped - structs by value, varargs, long double, nine
arguments - and each would need its own entry below saying so, which is a
list worth writing only if the wider net starts earning it.
"""
import os
import re
import subprocess
import sys
import tempfile

import corpus

PHASES = ("initial", "ssa", "scp", "gvn", "dce")
PREG = re.compile(r"\$[a-z0-9]+")

# Fixtures whose IR is *about* the ABI, and so differs by more than a register
# name. Not a list of known problems - each of these is a fixture doing its
# job, and the entry says which job.
# Keyed by the fixture's path, so a same-named file elsewhere is not excused
# by accident.
ABI_SPECIFIC = {
    "ir/pipeline/aggregate_argument":
        "LP64D passes a large aggregate by reference and SysV as bytes",
    "ir/pipeline/composite_return": "which eightbytes a small struct comes back in",
    "ir/pipeline/frame_stack_params":
        "how many arguments fit in registers before the stack",
    "ir/pipeline/param_abi_boundary":
        "the point where each argument register bank runs out",
    "ir/pipeline/param_ptr_vs_stack":
        "the argument registers themselves are the subject",
    "ir/pipeline/param_va_area": "SysV's two save areas against LP64D's one flat one",
    "ir/pipeline/ra_limits":
        "register pressure, counted against a different register file",
}


def fixtureKey(path):
    """The fixture's path under test/testData, without the '.c'."""
    parts = os.path.normpath(path).split(os.sep)
    if "testData" in parts:
        parts = parts[parts.index("testData") + 1:]
    return "/".join(parts)[:-2]


def dumpFor(compiler, source, phase, target, out, obj):
    march = ["-march", target]
    r = subprocess.run([compiler] + march + ["-oneline", "-c",
                        "-irDump:" + phase, out, "-o", obj, source],
                       capture_output=True)
    if r.returncode != 0 or not os.path.exists(out):
        return None
    with open(out) as f:
        return f.read().splitlines()


def compare(where, phase, left, right):
    """left is x86-64's dump, right riscv64's. The rename has to be a bijection."""
    findings = []

    if len(left) != len(right):
        return [f"{where} [{phase}]: {len(left)} lines for x86_64, "
                f"{len(right)} for riscv64"]

    renaming = {}

    for idx, (a, b) in enumerate(zip(left, right)):
        if a == b:
            continue

        # Strike the register names out of both and require what is left to be
        # the same line: anything else is a difference in the IR itself.
        if PREG.sub("$", a) != PREG.sub("$", b):
            findings.append(f"{where} [{phase}] line {idx + 1}:\n"
                            f"    x86_64:  {a}\n    riscv64: {b}")
            continue

        for x, r in zip(PREG.findall(a), PREG.findall(b)):
            if renaming.setdefault(x, r) != r:
                findings.append(f"{where} [{phase}] line {idx + 1}: {x} is "
                                f"{renaming[x]} elsewhere and {r} here")

    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed, compared, excused = [], [], 0, 0
    stale = set(ABI_SPECIFIC)

    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, "d.o")
        for f in files:
            name = fixtureKey(f)
            for phase in PHASES:
                left = dumpFor(compiler, f, phase, "x86_64",
                               os.path.join(tmp, "x.txt"), obj)
                right = dumpFor(compiler, f, phase, "riscv64",
                                os.path.join(tmp, "r.txt"), obj)
                if left is None or right is None:
                    failed.append(f"{f} [{phase}]")
                    continue
                found = compare(f, phase, left, right)

                if name in ABI_SPECIFIC:
                    excused += 1
                    if found:
                        stale.discard(name)
                    continue

                compared += 1
                findings += found

    # A listed fixture whose IR no longer differs is an exemption that has
    # stopped exempting anything, which is the same kind of silence a rotted
    # mutation is - so it is a finding rather than a note.
    for name in sorted(stale):
        findings.append(f"{name} is listed as ABI-specific "
                        f"({ABI_SPECIFIC[name]}) but its IR no longer differs "
                        f"from x86-64's - delete the entry")

    return corpus.report(len(files), failed, findings,
                         f"dumps compared: {compared}, "
                         f"ABI-specific and not compared: {excused}")


if __name__ == "__main__":
    sys.exit(main())
