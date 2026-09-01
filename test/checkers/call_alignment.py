#!/usr/bin/env python3
"""Stack-alignment checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/call_alignment.py build/bin/main test/testData/codegen

Simulates rsp through the '-S' disassembly of each emitted function, following
only the instructions that move it - push, pop, and add/sub of an immediate -
and asserts two things the exit-code tests cannot see:

  - rsp is 16-byte aligned at every call. At function entry rsp % 16 == 8
    (the caller's return address is on the stack), so the invariant at a call
    is delta % 16 == 8.
  - every call's stack arguments are given back, so by the epilogue rsp is
    where the prologue left it.

An unaligned call only crashes if the callee touches SSE, which is why running
the corpus does not catch it: deleting the odd-stack-argument padding shows up
here on exactly one function and nowhere else.
"""
import os
import re
import subprocess
import sys
import tempfile

import corpus

PUSH = re.compile(r"^push ")
POP = re.compile(r"^pop ")
ADDSUB = re.compile(r"^(add|sub) rsp, (0x[0-9a-fA-F]+|\d+)$")
INSN = re.compile(r"^<[0-9a-f]+>\t[0-9a-f]+\s+(.*)$")
FUNCTION = re.compile(r"^<<< (\w+) >>>$")


def checkFunction(where, name, body, stats):
    findings = []
    # delta = how far rsp has moved below its value at function entry.
    delta, frame = 0, None
    # A frameless function never subtracts, so the prologue's push of rbp is
    # all there is; seed 'frame' with it so the balance check has a reference.
    if not any(ADDSUB.match(i) and i.startswith("sub") for i in body):
        frame = 8

    for insn in body:
        if PUSH.match(insn):
            delta += 8
            continue
        if POP.match(insn):
            delta -= 8
            continue
        m = ADDSUB.match(insn)
        if m:
            v = int(m.group(2), 0)
            delta += v if m.group(1) == "sub" else -v
            # The prologue's own 'sub rsp, framesize' is the first one;
            # everything after it is a call's doing.
            if frame is None and m.group(1) == "sub":
                frame = delta
            continue
        if insn.startswith("call "):
            stats["calls"] += 1
            if delta % 16 != 8:
                findings.append(f"{where} [{name}]: rsp misaligned at call "
                                f"(delta={delta}, delta%16={delta % 16}, want 8)")
            continue
        if insn.startswith("leave"):
            # 'leave' restores rsp from rbp whatever rsp held, so nothing about
            # delta is checkable at that point - but an unbalanced call
            # sequence is, and it shows up as delta not being back where the
            # prologue left it.
            if frame is not None and delta != frame:
                findings.append(f"{where} [{name}]: stack arguments not popped "
                                f"(delta={delta}, after prologue={frame})")
            delta = frame if frame is not None else delta

    stats["functions"] += 1
    return findings


def checkSource(compiler, source, obj, stats):
    r = subprocess.run([compiler] + corpus.flags + ["-oneline", "-S", "-c", "-o", obj, source],
                       capture_output=True, text=True)
    if r.returncode != 0:
        return None

    findings, name, body = [], None, []
    for line in r.stdout.splitlines():
        m = FUNCTION.match(line)
        if m:
            name, body = m.group(1), []
            continue
        if line == "<<<>>>" and name:
            findings += checkFunction(source, name, body, stats)
            name = None
            continue
        if name is not None:
            m = INSN.match(line)
            if m:
                body.append(m.group(1).strip())
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed = [], []
    stats = {"functions": 0, "calls": 0}
    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, "d.o")
        for f in files:
            fs = checkSource(compiler, f, obj, stats)
            if fs is None:
                failed.append(f)
            else:
                findings += fs

    return corpus.report(len(files), failed, findings,
                         f"emitted functions: {stats['functions']}, calls: {stats['calls']}")


if __name__ == "__main__":
    sys.exit(main())
