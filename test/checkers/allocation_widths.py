#!/usr/bin/env python3
"""Allocation checker (b) of docs/ir-codegen-design.md section 9.

    python3 test/checkers/allocation_widths.py build/bin/main test/testData/codegen


A forward walk over the post-allocation machine IR (-irDump:ra) tracking how
many bytes of each physical register have actually been written, asserting that
no later instruction - a spill above all - reads more than that.

    $r10 = MOP_COPY.4 $r11     ; four bytes written
    MOP_SPILL.8 fi#9, $r10     ; eight stored, four of them never written

Registers are not live across block boundaries under the spill-everything
allocator, so the state is cleared at each block; a register not yet written in
this block is assumed whole (it is an incoming ABI register or a reload).
"""
import os, re, subprocess, sys, tempfile

import corpus

FUNC = re.compile(r"^MachineFunction '([^']+)'")
BLOCK = re.compile(r"^MBB #")
INSTR = re.compile(
    r"^  (?:(.*?) = )?([A-Za-z_][A-Za-z0-9_]*)(?:\.(\d+)(?:/(\d+))?)?(?: (.*?))?(?: ; %\d+)?$")
REG = re.compile(r"\$([a-z0-9]+)")


def regs_in(text):
    return REG.findall(text or "")


def check_dump(path):
    findings = []
    func = "?"
    written, definedBy = {}, {}
    for line in open(path):
        line = line.rstrip("\n")
        m = FUNC.match(line)
        if m:
            func, written, definedBy = m.group(1), {}, {}
            continue
        if BLOCK.match(line):
            written, definedBy = {}, {}
            continue
        if not line.startswith("  ") or line.startswith("   "):
            continue
        body = re.sub(r" ; %\d+$", "", line)
        body = body.split(" <clobbers caller-saved>")[0]
        # The implicit operand list is ABI bookkeeping - an argument register a
        # call reads, the rax a call defines - and carries no width.
        implicit = ""
        if body.endswith("]") and " [implicit" in body:
            at = body.index(" [implicit")
            implicit, body = body[at:], body[:at]

        m = INSTR.match(body)
        if not m:
            continue
        defs, opcode, size, srcsize, uses = m.groups()
        # A partial def keeps the bytes it does not write, so what it leaves is
        # what was there plus its own width - the same as a two-address def,
        # which says the same thing by naming its register on both sides.
        partial = set(re.findall(r"\$([a-z0-9]+)<partial-def>", defs or ""))
        size = int(size) if size else 8
        srcsize = int(srcsize) if srcsize else size

        # Uses first: they read the state the defs are about to change.
        used = regs_in(uses)
        mem = regs_in(" ".join(re.findall(r"\[[^\]]*\]", uses or "")))
        # A variable shift count is cl, one byte however wide the shift is.
        count = used[-1] if opcode in ("shl", "shr", "sar") and len(used) > 1 else None
        for r in used:
            # A register named inside a memory operand is an address, read
            # whole whatever the instruction's own width is.
            want = 1 if r == count else 8 if r in mem else srcsize
            have = written.get(r, 8)
            if have < want:
                findings.append((func, opcode, want, have,
                                 "%s   <- %s" % (line.strip(), definedBy.get(r, "?"))))

        for r in regs_in(defs) + re.findall(r"implicit-def \$([a-z0-9]+)", implicit):
            # A read-modify-write def keeps whatever the previous one put in
            # the bytes it does not touch; any other def leaves them unknown.
            prev = written.get(r, 8) if (r in used or r in partial) else 0
            written[r] = max(prev, size)
            definedBy[r] = line.strip()
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = []
    for root in roots:
        if os.path.isfile(root):
            files.append(root)
            continue
        for dirpath, _dirs, names in os.walk(root):
            for n in sorted(names):
                if n.endswith(".c"):
                    files.append(os.path.join(dirpath, n))
    files.sort()

    perfile, failed, total = [], [], 0
    with tempfile.TemporaryDirectory() as tmp:
        dump = os.path.join(tmp, "d.txt")
        obj = os.path.join(tmp, "d.o")
        for f in files:
            if os.path.exists(f[:-2] + ".legacy"):
                continue
            r = subprocess.run([compiler] + corpus.flags + ["-oneline", "-c",
                                "-irDump:ra", dump, "-o", obj, f], capture_output=True)
            if r.returncode != 0 or not os.path.exists(dump):
                failed.append(f)
                continue
            fs = check_dump(dump)
            os.remove(dump)
            if fs:
                perfile.append((f, fs))
                total += len(fs)

    for f, fs in perfile:
        print(f"{f}: {len(fs)}")
        for fn, op, want, have, l in fs:
            print(f"    {fn}: reads {want} of {have} written: {l}")
    print()
    print(f"files scanned: {len(files)}, uncompilable: {len(failed)}, with findings: {len(perfile)}")
    for f in failed:
        print(f"    failed: {f}")
    print(f"findings: {total}")
    return 1 if total else 0


if __name__ == "__main__":
    sys.exit(main())
