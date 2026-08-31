#!/usr/bin/env python3
"""Instruction-selection checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/selection.py build/bin/main test/testData/codegen

Three invariants over '-irDump:isel' that a correct stage 1 must satisfy and
that reading a dump does not reliably establish:

 1. Nothing is read before it is written. A backward liveness fixpoint over
    the machine CFG; the entry block's live-in must hold no virtual register
    at all, and no physical register but the ones the ABI hands the function.
    A misplaced phi copy, a block visited in the wrong order, or a two-address
    sequence emitted backwards all surface here.

 2. Two-address form. A target instruction whose first source operand is a
    plain register must name that same register as its destination - that is
    what the leading copy is for, and this is what notices when one goes
    missing. Instructions reading memory are not in this shape and are not
    checked: '%v5 = mov.4 [%v1 + %v4*4]' names two registers and is a load.

 3. Branches agree with layout. Per block, the successors reachable by what
    was emitted - explicit branch targets, plus the next block in layout order
    unless the block ends in an unconditional jump or a return - must be
    exactly the CFG successors in its header.
"""
import collections
import os
import re
import sys
import tempfile

import corpus

ARG_REGS = {"rdi", "rsi", "rdx", "rcx", "r8", "r9", "rsp", "rbp",
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"}
UNCOND = {"jmp", "ret", "jmp*"}
# An indirect jump - a switch's table dispatch, a computed goto - reaches every
# successor through the table rather than through an operand, so what it emits
# can only be asked to be a subset of the CFG's successors.
INDIRECT = "jmp*"
COND = re.compile(r"^j(?!mp)[a-z]+$")
REG = re.compile(r"(%v\d+|\$[a-z0-9]+)")
BARE_REG = re.compile(r"^(%v\d+|\$[a-z0-9]+)$")
# 'fi#3' is a frame index, not a block label.
TARGET = re.compile(r"(?<![\w#])#(\d+)")


class Instr:
    def __init__(self, opcode, defs, uses, targets, text):
        self.opcode, self.defs, self.uses = opcode, defs, uses
        self.targets, self.text = targets, text


class Block:
    def __init__(self, bid, succs):
        self.id, self.succs, self.instrs = bid, succs, []


def splitOperands(text):
    """Split on commas outside brackets, so '[%v1 + %v4*4], %v2' stays two operands."""
    out, depth, cur = [], 0, ""
    for c in text:
        if c == "[":
            depth += 1
        elif c == "]":
            depth -= 1
        if c == "," and depth == 0:
            out.append(cur.strip())
            cur = ""
        else:
            cur += c
    if cur.strip():
        out.append(cur.strip())
    return out


def parseInstr(line):
    text = line.strip()
    body, _, _origin = text.partition(" ; ")

    # A call carries '[implicit ...]' and then '<clobbers ...>'; the annotation
    # is not an operand and the bracket is no longer at the end of the line.
    body = re.sub(r"<[^>]*>", "", body).strip()
    implicit = []
    m = re.search(r"\[([^\]]*implicit[^\]]*)\]", body)
    if m:
        implicit = [p.strip() for p in m.group(1).split(",")]
        body = (body[:m.start()] + body[m.end():]).strip()

    lhs, eq, rhs = body.partition(" = ")
    if not eq:
        lhs, rhs = "", body

    parts = rhs.split(None, 1)
    opcode = parts[0].split(".")[0].split("(")[0]
    operandText = parts[1] if len(parts) > 1 else ""

    defs = REG.findall(lhs)
    uses = REG.findall(operandText)
    for item in implicit:
        (defs if item.startswith("implicit-def") else uses).extend(REG.findall(item))

    return Instr(opcode, defs, uses, [int(t) for t in TARGET.findall(operandText)], text), \
        splitOperands(operandText)


def parseFunctions(path):
    functions, cur, block = [], None, None
    for line in open(path):
        if line.startswith("MachineFunction"):
            cur = (re.search(r"'([^']*)'", line).group(1), [])
            functions.append(cur)
            block = None
        elif line.startswith("MBB #"):
            bid = int(re.match(r"MBB #(\d+)", line).group(1))
            m = re.search(r"-> ([^\n]*)", line)
            succs = [int(x) for x in re.findall(r"#(\d+)", m.group(1))] if m else []
            block = Block(bid, succs)
            cur[1].append(block)
        elif line.startswith("  ") and line.strip() and block is not None:
            instr, operands = parseInstr(line)
            instr.operands = operands
            block.instrs.append(instr)
        elif not line.strip():
            block = None
    return functions


def liveness(blocks):
    liveIn = {b.id: set() for b in blocks}
    changed = True
    while changed:
        changed = False
        for b in reversed(blocks):
            live = set()
            for s in b.succs:
                live |= liveIn.get(s, set())
            for ins in reversed(b.instrs):
                live -= set(ins.defs)
                live |= set(ins.uses)
            if live != liveIn[b.id]:
                liveIn[b.id] = live
                changed = True
    return liveIn


def checkDump(path, where, stats):
    findings = []
    for fname, blocks in parseFunctions(path):
        if not blocks:
            continue
        stats["functions"] += 1
        stats["instrs"] += sum(len(b.instrs) for b in blocks)

        for r in sorted(liveness(blocks)[blocks[0].id]):
            if r.startswith("%v") or r.lstrip("$") not in ARG_REGS:
                findings.append(f"{where} [{fname}]: {r} is live on entry (read before written)")

        for idx, b in enumerate(blocks):
            for ins in b.instrs:
                if ins.opcode.startswith("MOP_") or ins.targets:
                    continue
                if len(ins.defs) == 1 and len(ins.operands) >= 2 \
                        and BARE_REG.match(ins.operands[0]):
                    stats["two-address"] += 1
                    if ins.defs[0] != ins.operands[0]:
                        findings.append(
                            f"{where} [{fname}] MBB#{b.id}: not two-address: {ins.text}")

            emitted, terminal = set(), False
            for ins in b.instrs:
                if ins.opcode in UNCOND or COND.match(ins.opcode):
                    emitted |= set(ins.targets)
                    if ins.opcode in UNCOND:
                        terminal = True
            if not terminal and idx + 1 < len(blocks):
                emitted.add(blocks[idx + 1].id)
            # A block whose terminator has no selection rule yet emits no branch
            # at all, so its successors are unreachable by construction rather
            # than by defect.
            if any(i.opcode == "MOP_UNSELECTED" and not i.defs for i in b.instrs[-1:]):
                continue
            if any(i.opcode == INDIRECT for i in b.instrs):
                if not emitted <= set(b.succs):
                    findings.append(f"{where} [{fname}] MBB#{b.id}: emitted -> "
                                    f"{sorted(emitted)}, not all in {sorted(b.succs)}")
                continue
            if emitted != set(b.succs):
                findings.append(f"{where} [{fname}] MBB#{b.id}: emitted -> {sorted(emitted)} "
                                f"but CFG says {sorted(b.succs)}")
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed = [], []
    stats = collections.Counter()
    with tempfile.TemporaryDirectory() as tmp:
        isel, obj = os.path.join(tmp, "isel.txt"), os.path.join(tmp, "d.o")
        for f in files:
            if not corpus.dump(compiler, f, "isel", isel, obj):
                failed.append(f)
                continue
            findings += checkDump(isel, f, stats)

    return corpus.report(len(files), failed, findings,
                         f"functions: {stats['functions']}, instructions: {stats['instrs']}, "
                         f"two-address: {stats['two-address']}")


if __name__ == "__main__":
    sys.exit(main())
