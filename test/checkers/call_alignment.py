#!/usr/bin/env python3
"""Stack-alignment checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/call_alignment.py build/bin/main test/testData/codegen

Simulates the stack pointer through the '-S' disassembly of each emitted
function, following only the instructions that move it, and asserts three
things the exit-code tests cannot see:

  - it is 16-byte aligned at every call;
  - every call's stack arguments are given back, so by the epilogue it is
    where the prologue left it;
  - it stands still between the prologue and the epilogue. Since roadmap step
    42 a call's stack arguments are stored into an area the frame reserves
    once, so the prologue's own moves are the only constant-sized ones an
    emitted function makes. A dynamic allocation still moves it, by a register
    rather than by a constant, and is not one of these.

Both machines, and the arithmetic differs in what the caller left behind. On
x86-64 the call pushed a return address, so at function entry rsp % 16 == 8
and the invariant at a call is delta % 16 == 8; the moves are 'push', 'pop'
and 'add/sub rsp, imm'. On riscv64 the return address is in a register and
nothing was pushed, so entry is already aligned and the invariant is
delta % 16 == 0; the only constant-sized move is 'addi sp,sp,imm', and the
epilogue restores with 'mv sp,s0' rather than with 'leave'.

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

# riscv64: 'addi sp,sp,-16' is the whole prologue's move, signed rather than
# spelled by two mnemonics, and 'mv sp,s0' is the epilogue's restore.
RISCV_ADJUST = re.compile(r"^addi sp,sp,(-?\d+)$")
RISCV_RESTORE = "mv sp,s0"
RISCV_CALL = re.compile(r"^(jal ra,|jalr\b)")


def checkRiscv(where, name, body, stats):
    findings = []
    delta, frame, adjustments = 0, None, 0

    # A frameless function subtracts nothing, so there is no prologue move to
    # take as the reference and the balance check compares against zero.
    if not any(RISCV_ADJUST.match(i) for i in body):
        frame = 0

    for insn in body:
        m = RISCV_ADJUST.match(insn)
        if m:
            delta -= int(m.group(1))
            adjustments += 1
            if adjustments > 1 or int(m.group(1)) > 0:
                findings.append(f"{where} [{name}]: sp moved outside the prologue: {insn}")
            if frame is None:
                frame = delta
            continue
        if RISCV_CALL.match(insn):
            stats["calls"] += 1
            if delta % 16 != 0:
                findings.append(f"{where} [{name}]: sp misaligned at call "
                                f"(delta={delta}, delta%16={delta % 16}, want 0)")
            continue
        if insn == RISCV_RESTORE:
            # 'mv sp,s0' restores whatever sp held, so nothing about delta is
            # checkable past it - but an unbalanced call sequence is, and it
            # shows up as delta not being where the prologue left it.
            if frame is not None and delta != frame:
                findings.append(f"{where} [{name}]: stack arguments not given back "
                                f"(delta={delta}, after prologue={frame})")
            delta = frame if frame is not None else delta

    stats["functions"] += 1
    return findings


def checkFunction(where, name, body, stats):
    findings = []
    # delta = how far rsp has moved below its value at function entry.
    delta, frame = 0, None
    # A frameless function never subtracts, so the prologue's push of rbp is
    # all there is; seed 'frame' with it so the balance check has a reference.
    if not any(ADDSUB.match(i) and i.startswith("sub") for i in body):
        frame = 8

    # What the prologue is allowed: one push of rbp, and one 'sub rsp' when
    # the function has a frame. Anything beyond that is the stack pointer
    # moving where nothing should move it - see the third invariant above.
    pushes, adjustments = 0, 0

    for insn in body:
        if PUSH.match(insn):
            delta += 8
            pushes += 1
            if pushes > 1:
                findings.append(f"{where} [{name}]: rsp moved outside the prologue: {insn}")
            continue
        if POP.match(insn):
            delta -= 8
            findings.append(f"{where} [{name}]: rsp moved outside the prologue: {insn}")
            continue
        m = ADDSUB.match(insn)
        if m:
            v = int(m.group(2), 0)
            delta += v if m.group(1) == "sub" else -v
            adjustments += 1
            if adjustments > 1 or m.group(1) == "add":
                findings.append(f"{where} [{name}]: rsp moved outside the prologue: {insn}")
            # The prologue's own 'sub rsp, framesize' is the first one.
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


def checkSource(compiler, source, obj, stats, check):
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
            findings += check(source, name, body, stats)
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
    check = checkRiscv if corpus.targetName() == "riscv64" else checkFunction

    findings, failed = [], []
    stats = {"functions": 0, "calls": 0}
    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, "d.o")
        for f in files:
            fs = checkSource(compiler, f, obj, stats, check)
            if fs is None:
                failed.append(f)
            else:
                findings += fs

    return corpus.report(len(files), failed, findings,
                         f"emitted functions: {stats['functions']}, calls: {stats['calls']}")


if __name__ == "__main__":
    sys.exit(main())
