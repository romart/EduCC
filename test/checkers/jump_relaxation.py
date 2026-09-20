#!/usr/bin/env python3
"""'-S' checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/jump_relaxation.py build/bin/main test/testData/codegen

Every jump costs no more than it has to. A forward jump reserves its
displacement before the target's address is known, so stage 3 emits the
function, measures where everything landed, and emits it again (section 8);
this asks whether the answer it settled on is the right one. A loop that
stopped one round early looks like nothing else.

The question is the same on both machines and the shape of the answer is not,
because what a jump can cost is not the same thing:

  x86-64 has two encodings, one byte of displacement and four. If the bytes
  spell the four-byte form, the distance must actually need four. A jump that
  fits in one byte and was written in four is not wrong, it is three bytes of
  nothing. The displacement is measured the way the assembler measures it,
  from the end of the two-byte form, since both encodings are relative to
  their own end.

  RV64G is fixed-width, so no jump is ever shorter or longer than four bytes
  and "as short as it can be" is not a question the machine has. Its
  relaxation is the other kind - a conditional branch out of B-type range
  becomes the opposite branch over an unconditional jump - and whether one of
  those was *needed* cannot be read out of the bytes: the ordinary
  two-successor block emits a branch over a jump too, and only the machine IR
  says which successor each was meant to reach.

  What the bytes do answer is that every displacement came out of the encoder
  intact. A B-type immediate is split across two fields and a J-type one
  across four, which is exactly the shape of the three latent assembler bugs
  section 8 lists; the disassembler decodes the split independently of the
  encoder that wrote it, so a target that is not four-byte aligned or not
  inside the function is one of them.

Nothing on either machine emits a nop, so one in the output is the same
failure seen from the other side: the assembler shortening a jump it had
already reserved four bytes for and padding the slack.

The legacy backend chooses at each jump site by hand and still pads, so this
reads the IR backend alone.
"""
import os
import re
import subprocess
import sys
import tempfile

import corpus

# <0000001c>\t4180f961            cmp r9b, 0x61
LINE = re.compile(r"^<([0-9a-f]+)>\s+([0-9a-fA-F]+)\s+(\S+)\s*(.*)$")
TARGET = re.compile(r"^0x[0-9A-Fa-f]+$")

# A riscv64 branch is 'b<cond> rs1,rs2,0x1c' or 'b<cond>z rs,0x1c'; matched
# against the mnemonic alone, which the parse has already split off.
RISCV_BRANCH = re.compile(r"^b(?:eq|ne|lt|ge|ltu|geu)z?$")
RISCV_BRANCH_RANGE = 4096  # the B-type displacement is 13 bits, signed


def instructions(text):
    """(function, address, encoded length, mnemonic, operands) for each line."""
    name = "?"
    for line in text.splitlines():
        if line.startswith("<<< "):
            name = line[4:-4].strip()
            continue

        m = LINE.match(line)
        if m is not None:
            yield (name, int(m.group(1), 16), len(m.group(2)) // 2,
                   m.group(3), m.group(4).strip())


def nops(where, body):
    return [f"{where} {name}: a nop at {addr:#x} - a jump was shortened after "
            f"its slack was reserved"
            for name, addr, _length, mnemonic, _ops in body if mnemonic == "nop"]


def x86Findings(where, body, stats):
    """A jump written in four bytes must need four."""
    findings = nops(where, body)

    for name, addr, length, mnemonic, operands in body:
        if not mnemonic.startswith("j") or not TARGET.match(operands):
            continue
        stats["examined"] += 1
        d = int(operands, 16) - (addr + 2)
        if length > 2 and -128 <= d <= 127:
            findings.append(f"{where} {name}: the jump at {addr:#x} reaches "
                            f"{int(operands, 16):#x} in one byte ({d}) and was "
                            f"written in {length}")
    return findings


def riscvFindings(where, body, stats):
    """Every branch and jump displacement decodes back to a real instruction."""
    findings = nops(where, body)

    # Where each function's own bytes end, so a target can be asked to be
    # inside them; -S numbers each function from zero.
    end = {}
    for name, addr, length, _m, _o in body:
        end[name] = max(end.get(name, 0), addr + length)

    for name, addr, _length, mnemonic, operands in body:
        branch = RISCV_BRANCH.match(mnemonic) is not None
        if not branch and mnemonic not in ("j", "jal"):
            continue

        target = operands.rsplit(",", 1)[-1]
        if not TARGET.match(target):
            continue  # 'jal ra,<symbol>' is a call, relocated rather than encoded

        stats["examined"] += 1
        value = int(target, 16)

        if value % 4 != 0:
            findings.append(f"{where} {name}: the {mnemonic} at {addr:#x} decodes "
                            f"to {value:#x}, which is not an instruction boundary")
        elif value >= end[name]:
            findings.append(f"{where} {name}: the {mnemonic} at {addr:#x} decodes "
                            f"to {value:#x}, past the end of the function "
                            f"({end[name]:#x})")
        elif branch and not -RISCV_BRANCH_RANGE <= value - addr < RISCV_BRANCH_RANGE:
            findings.append(f"{where} {name}: the branch at {addr:#x} reaches "
                            f"{value:#x}, which is out of B-type range")
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    riscv = corpus.targetName() == "riscv64"
    check = riscvFindings if riscv else x86Findings

    findings, failed, scanned = [], [], 0
    stats = {"examined": 0}
    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, "j.o")
        for f in corpus.sources(roots):
            scanned += 1
            r = subprocess.run([compiler] + corpus.flags
                               + ["-S", "-oneline", "-c", "-o", obj, f],
                               capture_output=True)
            if r.returncode != 0:
                failed.append(f)
                continue

            findings += check(f, list(instructions(r.stdout.decode("utf-8", "replace"))), stats)

    what = "branches and jumps" if riscv else "direct jumps"
    return corpus.report(scanned, failed, findings,
                         f"files disassembled: {scanned - len(failed)}, "
                         f"{what} examined: {stats['examined']}")


if __name__ == "__main__":
    sys.exit(main())
