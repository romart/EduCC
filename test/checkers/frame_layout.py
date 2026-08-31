#!/usr/bin/env python3
"""Frame-layout checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/frame_layout.py build/bin/main test/testData/codegen

Four invariants over the frame stage 0 laid out, read from '-irDump:mir':

    fi#3 : local 8/8 @ -24 'p' ; %11

  - every offset is aligned to the object's own alignment;
  - no two static objects overlap;
  - a local lies below the frame pointer and inside the reported frame size;
  - an incoming argument lies at +16 or above, past the saved rbp and the
    return address.

Objects of size zero are excluded from all four: an empty struct occupies no
bytes, so it neither overlaps anything nor has a side of the frame pointer to
be on. Dynamic objects (a VLA, alloca) have no static offset to check.
"""
import os
import re
import sys
import tempfile

import corpus

FUNCTION = re.compile(r"^MachineFunction '([^']+)'")
FRAME_SIZE = re.compile(r"^Frame: (\d+) bytes")
OBJECT = re.compile(r"^  fi#(\d+) : (\S+) (?:dynamic|(\d+)/(\d+) @ (-?\d+))")


def checkFunction(where, name, size, objects):
    findings = []
    static = [o for o in objects if o[1] and o[0] != "param"]

    for kind, span, align, off, line in objects:
        if not span:
            continue
        if off % align != 0:
            findings.append(f"{where} [{name}]: not aligned to {align}: {line}")
        if kind == "param":
            if off < 16:
                findings.append(f"{where} [{name}]: incoming argument below +16: {line}")
        else:
            if off >= 0:
                findings.append(f"{where} [{name}]: local not below the frame pointer: {line}")
            if -off > size:
                findings.append(f"{where} [{name}]: outside the {size}-byte frame: {line}")

    for i in range(len(static)):
        for j in range(i + 1, len(static)):
            a, b = static[i], static[j]
            if a[3] < b[3] + b[1] and b[3] < a[3] + a[1]:
                findings.append(f"{where} [{name}]: overlap: [{a[4]}] vs [{b[4]}]")
    return findings


def checkDump(path, where):
    findings, objs = [], 0
    name, size, objects = None, 0, []
    for line in open(path):
        m = FUNCTION.match(line)
        if m:
            if name:
                objs += len(objects)
                findings += checkFunction(where, name, size, objects)
            name, size, objects = m.group(1), 0, []
            continue
        m = FRAME_SIZE.match(line)
        if m:
            size = int(m.group(1))
            continue
        m = OBJECT.match(line)
        if m:
            if m.group(3) is None:
                objects.append((m.group(2), None, None, None, line.strip()))
            else:
                objects.append((m.group(2), int(m.group(3)), int(m.group(4)),
                                int(m.group(5)), line.strip()))
    if name:
        objs += len(objects)
        findings += checkFunction(where, name, size, objects)
    return objs, findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    files = corpus.sources(roots)

    findings, failed, objects = [], [], 0
    with tempfile.TemporaryDirectory() as tmp:
        mir, obj = os.path.join(tmp, "mir.txt"), os.path.join(tmp, "d.o")
        for f in files:
            if not corpus.dump(compiler, f, "mir", mir, obj):
                failed.append(f)
                continue
            n, fs = checkDump(mir, f)
            objects += n
            findings += fs

    return corpus.report(len(files), failed, findings, f"frame objects checked: {objects}")


if __name__ == "__main__":
    sys.exit(main())
