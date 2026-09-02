#!/usr/bin/env python3
"""'-S' checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/jump_relaxation.py build/bin/main test/testData/codegen

Every jump is as short as it can be. A forward jump reserves its displacement
before the target's address is known, so stage 3 emits the function, measures
where everything landed, and emits it again (section 8); this asks whether the
answer it settled on is the right one. For each jump in the disassembly: if the
bytes spell the four-byte displacement, the distance must actually need four.
A jump that fits in one byte and was written in four is not wrong, it is three
bytes of nothing - and it is what the whole relaxation loop exists to remove,
so a loop that stopped one round early looks like nothing else.

The displacement is measured the way the assembler measures it, from the end of
the two-byte form, since both encodings are relative to their own end.

Nothing else emits a nop, so one in the output is the same failure seen from
the other side: the assembler shortening a jump it had already reserved four
bytes for and padding the slack, which is what it did before there was a
relaxation pass to make the reservation right in the first place.

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


def jumps(text):
    """(function, address, encoded length, target) for every direct jump."""
    name = "?"
    for line in text.splitlines():
        if line.startswith("<<< "):
            name = line[4:-4].strip()
            continue

        m = LINE.match(line)
        if m is None:
            continue

        mnemonic, operands = m.group(3), m.group(4).strip()
        if mnemonic == "nop":
            yield name, int(m.group(1), 16), len(m.group(2)) // 2, None
        elif mnemonic.startswith("j") and TARGET.match(operands):
            yield (name, int(m.group(1), 16), len(m.group(2)) // 2,
                   int(operands, 16))


def main():
    compiler, roots = corpus.parseArgs(__doc__)

    findings, failed, scanned = [], [], 0
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

            for name, addr, length, target in jumps(r.stdout.decode("utf-8", "replace")):
                if target is None:
                    findings.append(f"{f} {name}: a nop at {addr:#x} - a jump "
                                    f"was shortened after its slack was reserved")
                    continue
                d = target - (addr + 2)
                if length > 2 and -128 <= d <= 127:
                    findings.append(f"{f} {name}: the jump at {addr:#x} reaches "
                                    f"{target:#x} in one byte ({d}) and was "
                                    f"written in {length}")

    return corpus.report(scanned, failed, findings, f"files disassembled: {scanned - len(failed)}")


if __name__ == "__main__":
    sys.exit(main())
