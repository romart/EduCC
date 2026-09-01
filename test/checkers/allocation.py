#!/usr/bin/env python3
"""Register-allocation checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/allocation.py build/bin/main test/testData/codegen

Eight invariants over '-irDump:ra'. The one that does the work is the last: a
backward liveness fixpoint over physical registers and spill slots together,
asserting nothing is read before it is written and that no scratch register is
live across a block boundary - which is the property that makes allocating
everything to a spill slot sound without any liveness analysis in the
allocator itself. Dropped reloads, dropped spills and spills placed before
their definition instead of after all surface there.

Its companion, the forward width walk that catches a spill storing more bytes
than were ever written, is test/checkers/allocation_widths.py.

Spill slots enter the liveness lattice through MOP_SPILL and MOP_RELOAD only.
A frame index named inside an ordinary memory operand is deliberately left
out: 'lea.8 [fi#0]' takes an address without reading the object, and a local
whose address is taken before it is assigned is not a defect.
"""
import collections
import os
import re
import sys
import tempfile

import corpus

SCRATCH = {"r10", "r11", "rbx", "xmm8", "xmm9", "xmm10"}
ABI_IN = {"rdi", "rsi", "rdx", "rcx", "r8", "r9",
          "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
          "rsp", "rbp"}
TERMINATORS = ("jmp", "jmp*", "ret", "je", "jne", "jl", "jle", "jg", "jge",
               "jb", "jbe", "ja", "jae")
# Only these have to be a block's last instruction. A sparse switch is a chain
# of 'cmp; je #case' inside one block (section 6.13), so a conditional branch
# in the middle of a block is the shape of the dispatch, not a misplacement.
UNCONDITIONAL = ("jmp", "jmp*", "ret")


class Instr:
    def __init__(self, text):
        self.text = text

        body, _, _origin = text.partition(" ; ")
        # A partial def keeps the bytes it does not write, so it reads its own
        # register as well - taken here, before the annotations are stripped.
        partial = set(re.findall(r"\$(\w+)<partial-def>", body))
        # A call carries '[implicit ...]' and then a '<clobbers ...>' note; the
        # note is not an operand, and the bracket is not at the end of the line.
        body = re.sub(r"<[^>]*>", "", body)
        # A constant-pool operand carries the string's text, which is neither
        # an operand nor free of things that look like one ("$Id: ...").
        body = re.sub(r'"(?:[^"\\]|\\.)*"', "", body).strip()
        implicit = ""
        m = re.search(r"\[([^\]]*implicit[^\]]*)\]", body)
        if m:
            implicit = m.group(1)
            body = (body[:m.start()] + body[m.end():]).strip()

        lhs, sep, rhs = body.partition(" = ")
        if not sep:
            lhs, rhs = "", body

        parts = rhs.split(None, 1)
        self.opcode = parts[0]
        args = parts[1] if len(parts) > 1 else ""

        self.defs = set(re.findall(r"\$(\w+)", lhs))
        self.uses = set(re.findall(r"\$(\w+)", args)) | partial
        for piece in implicit.split(","):
            piece = piece.strip()
            if piece:
                target = self.defs if piece.startswith("implicit-def") else self.uses
                target.update(re.findall(r"\$(\w+)", piece))

        slots = set(re.findall(r"(fi#\d+)", args))
        if self.base() == "MOP_SPILL":
            self.defs |= slots
        elif self.base() == "MOP_RELOAD":
            self.uses |= slots

        self.virtual = "%v" in body

    def base(self):
        return self.opcode.split(".")[0]

    def width(self):
        """The '.8' or, for a widening move, the '.8/4' destination width."""
        if "." not in self.opcode:
            return None
        return int(self.opcode.split(".")[1].split("/")[0])


class Block:
    def __init__(self, header):
        self.id = int(re.match(r"MBB #(\d+)", header).group(1))
        self.instrs = []
        self.succs = []
        s = re.search(r"-> ([#\d ]+)$", header.strip())
        if s:
            self.succs = [int(x) for x in re.findall(r"#(\d+)", s.group(1))]


def parseFunctions(path):
    funcs, cur = [], None
    for line in open(path):
        line = line.rstrip("\n")
        if line.startswith("MachineFunction"):
            cur = {"name": re.search(r"'([^']*)'", line).group(1), "blocks": [],
                   "frame": [], "unalloc": False, "used": set(), "vregs": {}}
            funcs.append(cur)
        elif cur is None:
            continue
        elif line.startswith("Registers: not allocated"):
            cur["unalloc"] = True
        elif line.startswith("Physical registers used:"):
            cur["used"] = set(re.findall(r"\$(\w+)", line))
        elif re.match(r"\s+%v\d+ : ", line):
            m = re.match(r"\s+%v(\d+) : (\w+)/(\d+)", line)
            cur["vregs"][int(m.group(1))] = (m.group(2), int(m.group(3)))
        elif re.match(r"\s+fi#\d+ : ", line):
            cur["frame"].append(line.strip())
        elif line.startswith("MBB #"):
            cur["blocks"].append(Block(line))
        elif line.startswith("  ") and cur["blocks"]:
            cur["blocks"][-1].instrs.append(Instr(line.strip()))
    return funcs


def checkFunction(where, f, stats):
    name = f["name"]
    findings = []

    def bad(msg):
        findings.append(f"{where} [{name}]: {msg}")

    if not f["blocks"]:
        return findings
    order = [b.id for b in f["blocks"]]
    blocks = {b.id: b for b in f["blocks"]}

    # (1) nothing virtual survives allocation
    for b in f["blocks"]:
        for i in b.instrs:
            if i.virtual:
                bad(f"virtual register survived allocation: {i.text}")

    # (2) one slot per spilled vreg
    byVreg = collections.defaultdict(list)
    for line in f["frame"]:
        m = re.match(r"fi#(\d+) : spill \d+/\d+ @ -?\d+ %v(\d+)", line)
        if m:
            byVreg[int(m.group(2))].append(int(m.group(1)))
    for v, slots in byVreg.items():
        if len(slots) != 1:
            bad(f"%v{v} has {len(slots)} slots: {slots}")

    # (3) a spill or reload moves exactly the register's own width
    slotVreg = {f"fi#{s[0]}": v for v, s in byVreg.items() if len(s) == 1}
    for b in f["blocks"]:
        for i in b.instrs:
            if i.base() not in ("MOP_SPILL", "MOP_RELOAD"):
                continue
            slot = next(iter(l for l in (i.defs | i.uses) if l.startswith("fi#")), None)
            v = slotVreg.get(slot)
            if v is not None and f["vregs"].get(v) and f["vregs"][v][1] != i.width():
                bad(f"{i.text} uses width {i.width()} for %v{v}, which is "
                    f"{f['vregs'][v][1]} bytes")

    # (4) nothing lands after a block's unconditional terminator
    for b in f["blocks"]:
        for pos, i in enumerate(b.instrs):
            if i.base() in UNCONDITIONAL and pos != len(b.instrs) - 1:
                bad(f"MBB #{b.id} has {i.base()} at position {pos} of {len(b.instrs)}")

    # (5) and (6): the backward liveness fixpoint.
    liveIn = {bid: set() for bid in order}
    changed = True
    while changed:
        changed = False
        for bid in reversed(order):
            b = blocks[bid]
            live = set()
            for s in b.succs:
                live |= liveIn.get(s, set())
            for i in reversed(b.instrs):
                live -= i.defs
                live |= i.uses
            if live != liveIn[bid]:
                liveIn[bid] = live
                changed = True

    for loc in sorted(liveIn[order[0]]):
        if loc.startswith("fi#"):
            bad(f"spill slot {loc} is read before it is written")
        elif loc in SCRATCH:
            bad(f"scratch register ${loc} is read before it is written")
        elif loc not in ABI_IN:
            bad(f"${loc} is live into the entry block")

    for bid in order:
        for loc in liveIn[bid]:
            if loc in SCRATCH:
                bad(f"scratch ${loc} is live into MBB #{bid}")

    # (7) two-address form survived allocation
    for b in f["blocks"]:
        for i in b.instrs:
            if i.base().startswith("MOP_") or i.base() in TERMINATORS:
                continue
            m = re.match(r"\$(\w+) = \S+ \$(\w+), ", i.text)
            if m:
                stats["two-address"] += 1
                if m.group(1) != m.group(2):
                    bad(f"two-address broken: {i.text}")

    # (8) the reported physical-register set is the one actually named
    named = set()
    for b in f["blocks"]:
        for i in b.instrs:
            named |= {l for l in (i.defs | i.uses) if not l.startswith("fi#")}
    if named != f["used"]:
        bad(f"reported used registers {sorted(f['used'])} != named {sorted(named)}")

    stats["functions"] += 1
    stats["instrs"] += sum(len(b.instrs) for b in f["blocks"])
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed = [], []
    stats = collections.Counter()
    with tempfile.TemporaryDirectory() as tmp:
        ra, obj = os.path.join(tmp, "ra.txt"), os.path.join(tmp, "d.o")
        for f in files:
            if not corpus.dump(compiler, f, "ra", ra, obj):
                failed.append(f)
                continue
            for fn in parseFunctions(ra):
                if not fn["unalloc"]:
                    findings += checkFunction(f, fn, stats)

    return corpus.report(len(files), failed, findings,
                         f"functions: {stats['functions']}, instructions: {stats['instrs']}, "
                         f"two-address: {stats['two-address']}")


if __name__ == "__main__":
    sys.exit(main())
