#!/usr/bin/env python3
"""Phi-destruction checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/phi_destruction.py build/bin/main test/testData/codegen

For every IR phi and every incoming edge, symbolically execute the copy
sequence stage 0 actually put in that predecessor and check the phi's register
ends up holding the value that edge carried.

    BB #7:  %30 = IR_PHI ([%12, #4], [%21, #6])

Every vreg starts holding its own name; each MOP_COPY assigns what its source
holds. A phi whose register does not end up holding the incoming value's
register means the copies were sequentialized in an order that clobbered one
of them - which is what a naive in-order sequentialization does to a swap.

A block ending in a computed goto owes copies to every target at once, its
edges being the ones that cannot be split, and the sequence executed here is
therefore all of them. That is the point: each target's phi still has to come
out holding what its own edge carried.

Reads two dumps of the same function: '-irDump:dce' for the phis (the last IR
before the machine backend) and '-irDump:mir' for the copies stage 0 emitted.
"""
import os
import re
import sys
import tempfile

import corpus

PHASE_SPLIT = re.compile(r"(?m)^--- Phase: \w+ ---\n")


def splitFunctions(text):
    return [p for p in PHASE_SPLIT.split(text) if p.strip()]


def parseIr(text):
    """-> list of functions, each {blockId: [(phiId, [(valueId, predId), ...])]}"""
    funcs = []
    for body in splitFunctions(text):
        blocks, cur = {}, None
        for line in body.splitlines():
            m = re.match(r"^BB #(\d+),", line)
            if m:
                cur = int(m.group(1))
                blocks[cur] = []
                continue
            m = re.match(r"^\s+%(\d+) = IR_PHI \((.*?)\)", line)
            if m and cur is not None:
                inputs = re.findall(r"\[%(\d+), #(\d+)\]", m.group(2))
                blocks[cur].append((int(m.group(1)), [(int(a), int(b)) for a, b in inputs]))
        funcs.append(blocks)
    return funcs


def parseMir(text):
    """-> list of functions, each (irBlockId -> mbbId, mbbId -> [(dst, src)], irId -> vreg)"""
    funcs = []
    for body in splitFunctions(text):
        ir2mbb, copies, ir2vreg, cur = {}, {}, {}, None
        for line in body.splitlines():
            m = re.match(r"^\s+%v(\d+) : \w+/\d+(?: ; %(\d+))?$", line)
            if m:
                if m.group(2) is not None:
                    ir2vreg[int(m.group(2))] = int(m.group(1))
                continue
            m = re.match(r"^MBB #(\d+), .*?, ir #(\d+)", line)
            if m:
                cur = int(m.group(1))
                ir2mbb[int(m.group(2))] = cur
                copies[cur] = []
                continue
            m = re.match(r"^MBB #(\d+),", line)
            if m:
                cur = int(m.group(1))
                copies[cur] = []
                continue
            m = re.match(r"^\s+%v(\d+) = MOP_COPY\.\d+ %v(\d+)", line)
            if m and cur is not None:
                copies[cur].append((int(m.group(1)), int(m.group(2))))
                continue
            # Stage 0 is phi destruction and frame layout only: an instruction
            # that is not a copy means this dump came from a later stage, and
            # every conclusion below would be drawn from the wrong input.
            if re.match(r"^\s+\S", line) and "MOP_" in line:
                raise AssertionError("unexpected machine instr: " + line)
        funcs.append((ir2mbb, copies, ir2vreg))
    return funcs


def check(name, irText, mirText):
    irFuncs, mirFuncs = parseIr(irText), parseMir(mirText)
    if len(irFuncs) != len(mirFuncs):
        return 0, [f"{name}: {len(irFuncs)} ir vs {len(mirFuncs)} mir functions"]

    findings, checked = [], 0
    for blocks, (ir2mbb, copies, ir2vreg) in zip(irFuncs, mirFuncs):
        for blockId, phis in blocks.items():
            if not phis:
                continue
            preds = set(p for _, ins in phis for _, p in ins)
            for pred in preds:
                mbb = ir2mbb[pred]
                state = {}
                for dst, src in copies[mbb]:
                    state[dst] = state.get(src, ("init", src))
                for phiId, ins in phis:
                    want = [v for v, p in ins if p == pred]
                    if len(want) != 1:
                        findings.append(f"{name}: phi %{phiId} lists pred #{pred} {len(want)}x")
                        continue
                    dstv, srcv = ir2vreg.get(phiId), ir2vreg.get(want[0])
                    if dstv is None or srcv is None:
                        findings.append(f"{name}: phi %{phiId} or value %{want[0]} has no vreg")
                        continue
                    checked += 1
                    got = state.get(dstv, ("init", dstv))
                    if got != ("init", srcv):
                        findings.append(
                            f"{name}: BB#{blockId} phi %{phiId} via #{pred}: "
                            f"%v{dstv} holds {got}, expected init %v{srcv}")
    return checked, findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed, pairs = [], [], 0
    with tempfile.TemporaryDirectory() as tmp:
        ir, mir = os.path.join(tmp, "ir.txt"), os.path.join(tmp, "mir.txt")
        obj = os.path.join(tmp, "d.o")
        for f in files:
            if not (corpus.dump(compiler, f, "dce", ir, obj)
                    and corpus.dump(compiler, f, "mir", mir, obj)):
                failed.append(f)
                continue
            n, fs = check(f, open(ir).read(), open(mir).read())
            pairs += n
            findings += fs

    return corpus.report(len(files), failed, findings, f"phi/edge pairs checked: {pairs}")


if __name__ == "__main__":
    sys.exit(main())
