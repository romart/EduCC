#!/usr/bin/env python3
"""'-S' checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/disasm_stable.py build/bin/main test/testData/codegen

Disassembles every fixture under both backends, twice, and asserts that the
output is byte-identical across runs and that the compiler does not die
producing it. The emitted bytes are a pure function of the input, so any
difference between two runs of the same command is the debugging tool lying.

That is exactly how the two step-26 bugs showed up. udis86's decode_prefixes()
read an uninitialized local as a REX prefix, so the same bytes rendered as a
different instruction depending on where the stack landed - and since the
invented prefix changes an instruction's length, it ran the following ones
together. The IR backend measured its code as a pointer difference
across a realloc, so a function with a jump table disassembled tens of
kilobytes past its own end until it walked off the section. Neither is visible
to a test that only checks what the compiled program returns.

ASLR is the run-to-run difference that made the first one intermittent, so one
run of each pair is made with it turned off where 'setarch -R' works.
"""
import os
import shutil
import subprocess
import sys
import tempfile

import corpus


def disassemble(compiler, flags, source, obj, prefix):
    r = subprocess.run(prefix + [compiler] + corpus.flags + flags
                       + ["-S", "-oneline", "-c", "-o", obj, source],
                       capture_output=True)
    return r.returncode, r.stdout


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    noAslr = ["setarch", "-R"] if shutil.which("setarch") else []

    findings, failed, runs = [], [], 0
    with tempfile.TemporaryDirectory() as tmp:
        obj = os.path.join(tmp, "d.o")
        for backend in ("ir", "legacy"):
            flags = [] if backend == "ir" else ["-legacy"]
            for f in corpus.sources(roots, backend):
                rc, first = disassemble(compiler, flags, f, obj, [])
                runs += 1
                if rc < 0 or rc >= 128:
                    findings.append(f"{f} ({backend}): crashed, signal "
                                    f"{-rc if rc < 0 else rc - 128}")
                    continue
                if rc != 0:
                    failed.append(f"{f} ({backend})")
                    continue
                for prefix in ([], noAslr):
                    runs += 1
                    if disassemble(compiler, flags, f, obj, prefix)[1] != first:
                        findings.append(f"{f} ({backend}): disassembly differs between runs")
                        break

    return corpus.report(len(corpus.sources(roots)), failed, findings,
                         f"compiler runs: {runs}")


if __name__ == "__main__":
    sys.exit(main())
