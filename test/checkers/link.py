#!/usr/bin/env python3
"""Structural checker for what the in-house linker writes.

    python3 test/checkers/link.py build/bin/main test/testData/codegen

The codegen suites already run every fixture linked this way and assert its
exit code, which says the program works on this machine today. It does not say
the file is well formed: ld.so reads a dozen tables before the first
instruction runs and checks almost none of them, so a .got.plt one slot short,
a PT_PHDR outside every PT_LOAD or a hash chain that loops are all invisible
until the day something consults them.

So this is the same corpus walk the other checkers do, over the linked
executable rather than over a dump: link each fixture and assert what a loader
is entitled to assume. The invariants themselves live
in test/link/dynamic_test.py, which asserts them over two hand-built programs;
this runs the same ones over three hundred real ones.

Skips (exit 2) when the host has no C runtime to link against, the way
emission_objdump.py skips without objdump.
"""
import os
import struct
import subprocess
import sys
import tempfile

import corpus

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "link"))

from dynamic_test import (Dynamic, crtDirectory, INTERPRETER, PT_PHDR,
                          PT_INTERP, PT_DYNAMIC, PT_GNU_RELRO,
                          R_X86_64_JUMP_SLOT, DT_PLTGOT, DT_JMPREL,
                          DT_HASH, DT_STRTAB, DT_SYMTAB)
from linker_test import PT_LOAD, PF_W, PF_X, PAGE, PHDR_SIZE

EM_X86_64, EM_RISCV = 62, 243

# What differs between the two machines the linker writes for. Everything else
# a loader consults is the same table in the same place. Keyed by e_machine and
# read off the file itself, so the checker cannot be told one thing and given
# another.
TARGETS = {
    EM_X86_64: {
        "name": "x86-64",
        "interpreter": INTERPRETER,
        "gotPltReserved": 3,
        "plt0": 16,
        "pltEntry": 16,
        "jumpSlot": R_X86_64_JUMP_SLOT,
        "sysroot": "",
        "flags": 0,
    },
    EM_RISCV: {
        "name": "riscv64",
        "interpreter": "/lib/ld-linux-riscv64-lp64d.so.1",
        "gotPltReserved": 2,
        "plt0": 32,
        "pltEntry": 16,
        "jumpSlot": 5,  # R_RISCV_JUMP_SLOT
        "sysroot": "/usr/riscv64-linux-gnu",
        # EF_RISCV_FLOAT_ABI_DOUBLE, and not EF_RISCV_RVC: no compressed
        # instruction is emitted and the flag is a claim about the bytes.
        "flags": 0x4,
    },
}


def targetOf(flags):
    """Which machine this run compiles for, from the flags it passes through."""
    for i, flag in enumerate(flags):
        value = None
        if flag.startswith("-march="):
            value = flag[7:]
        elif flag == "-march" and i + 1 < len(flags):
            value = flags[i + 1]
        if value == "riscv64":
            return TARGETS[EM_RISCV]
    return TARGETS[EM_X86_64]


def check(findings, source, condition, message):
    if not condition:
        findings.append("%s: %s" % (source, message))


def inspect(path, source, findings, target):
    exe = Dynamic(path)
    loads = exe.loads()

    # A RISC-V file states its float ABI in e_flags, and the zero x86-64 leaves
    # there reads as soft-float - a claim about how this program passes
    # doubles, which ld reads and refuses to link against a double-float one.
    check(findings, source, exe.flags == target["flags"],
          "e_flags is 0x%x where %s's is 0x%x"
          % (exe.flags, target["name"], target["flags"]))

    for segment in loads:
        check(findings, source, segment["offset"] % PAGE ==
              segment["vaddr"] % PAGE,
              "a PT_LOAD at 0x%x is not congruent with its file offset"
              % segment["vaddr"])

    for i in range(len(loads) - 1):
        check(findings, source,
              loads[i]["vaddr"] + loads[i]["memsz"] <= loads[i + 1]["vaddr"],
              "two PT_LOAD segments overlap at 0x%x" % loads[i + 1]["vaddr"])
        check(findings, source,
              loads[i]["offset"] + loads[i]["filesz"] <= loads[i + 1]["offset"],
              "two PT_LOAD segments overlap in the file at 0x%x"
              % loads[i + 1]["offset"])

    check(findings, source,
          any(s["vaddr"] <= exe.entry < s["vaddr"] + s["filesz"]
              for s in loads if s["flags"] & PF_X),
          "the entry point 0x%x is not in an executable segment" % exe.entry)

    phdr = exe.segment(PT_PHDR)
    check(findings, source, phdr is not None, "there is no PT_PHDR")
    if phdr is not None:
        check(findings, source, phdr["offset"] == exe.phoff,
              "PT_PHDR does not point at e_phoff")
        check(findings, source,
              phdr["filesz"] == len(exe.segments) * PHDR_SIZE,
              "PT_PHDR does not cover every program header")
        check(findings, source,
              any(s["offset"] <= phdr["offset"] and
                  phdr["offset"] + phdr["filesz"] <= s["offset"] + s["filesz"]
                  for s in loads),
              "PT_PHDR is not inside any PT_LOAD")

    interp = exe.segment(PT_INTERP)
    check(findings, source, interp is not None, "there is no PT_INTERP")
    if interp is not None:
        text = exe.blob[interp["offset"]:interp["offset"] + interp["filesz"]]
        check(findings, source,
              text == target["interpreter"].encode() + b"\0",
              "PT_INTERP is not the loader's path")

    dynamic = exe.section(".dynamic")
    check(findings, source, dynamic is not None, "there is no .dynamic")
    if dynamic is None:
        return

    segment = exe.segment(PT_DYNAMIC)
    check(findings, source,
          segment is not None and segment["vaddr"] == dynamic["addr"],
          "PT_DYNAMIC and .dynamic disagree")

    for tag, name in ((DT_HASH, ".hash"), (DT_STRTAB, ".dynstr"),
                      (DT_SYMTAB, ".dynsym")):
        section = exe.section(name)
        check(findings, source,
              section is not None and exe.tag(tag) == section["addr"],
              "the dynamic entry for %s is not where %s is" % (name, name))

    gotPlt = exe.section(".got.plt")
    check(findings, source, gotPlt is not None, "there is no .got.plt")
    if gotPlt is None:
        return

    check(findings, source, exe.tag(DT_PLTGOT) == gotPlt["addr"],
          "DT_PLTGOT is not where .got.plt is")
    check(findings, source,
          struct.unpack("<Q", exe.bytesAt(gotPlt["addr"], 8))[0] ==
          dynamic["addr"],
          "the first .got.plt slot does not hold the address of .dynamic")

    plt = exe.section(".plt")
    relaPlt = exe.relocations(".rela.plt")

    reserved = target["gotPltReserved"]

    check(findings, source, gotPlt["size"] == (reserved + len(relaPlt)) * 8,
          ".got.plt holds %d slots for %d stubs"
          % (gotPlt["size"] // 8, len(relaPlt)))

    if relaPlt:
        check(findings, source, plt is not None and
              plt["size"] == target["plt0"] +
              len(relaPlt) * target["pltEntry"],
              ".plt is %d bytes for %d stubs"
              % (plt["size"] if plt else 0, len(relaPlt)))
        check(findings, source, exe.tag(DT_JMPREL) ==
              exe.section(".rela.plt")["addr"],
              "DT_JMPREL is not where .rela.plt is")

    for offset, symbol, type, _addend in relaPlt:
        check(findings, source, type == target["jumpSlot"],
              ".rela.plt holds a relocation of type %d" % type)
        check(findings, source,
              gotPlt["addr"] + reserved * 8 <= offset <
              gotPlt["addr"] + gotPlt["size"],
              "a JUMP_SLOT at 0x%x is not a .got.plt slot past the reserved "
              "%d" % (offset, reserved))

    writable = [s for s in loads if s["flags"] & PF_W]
    for offset, symbol, type, _addend in relaPlt + exe.relocations(".rela.dyn"):
        check(findings, source,
              any(s["vaddr"] <= offset < s["vaddr"] + s["memsz"]
                  for s in writable),
              "a dynamic relocation at 0x%x is not in a writable segment"
              % offset)
        check(findings, source, 0 < symbol < len(exe.dynsyms),
              "a dynamic relocation names symbol %d, which is not there"
              % symbol)

    relro = exe.segment(PT_GNU_RELRO)
    check(findings, source, relro is not None, "there is no PT_GNU_RELRO")
    if relro is not None:
        check(findings, source,
              any(s["vaddr"] <= relro["vaddr"] and
                  relro["vaddr"] + relro["memsz"] <= s["vaddr"] + s["memsz"]
                  for s in writable),
              "PT_GNU_RELRO is not inside the writable segment")
        # Lazy binding writes a .got.plt slot on every first call, so a stub
        # inside the sealed region is a fault the first time it is used.
        check(findings, source,
              gotPlt["addr"] >= relro["vaddr"] + relro["memsz"],
              ".got.plt is inside the read-only-after-relocation region")

    for i, entry in enumerate(exe.dynsyms):
        if i == 0:
            continue
        check(findings, source, exe.hashLookup(entry["name"]) == i,
              "'%s' cannot be found through .hash" % entry["name"])


def main():
    compiler, roots = corpus.parseArgs(__doc__)

    target = targetOf(corpus.flags)

    if target["sysroot"]:
        runtime = os.path.join(target["sysroot"], "usr", "lib", "crt1.o")
        loader = target["sysroot"] + target["interpreter"]
    else:
        runtime = crtDirectory()
        loader = target["interpreter"]

    if runtime is None or not os.path.exists(runtime) or \
            not os.path.exists(loader):
        print("no %s C runtime to link against; skipping" % target["name"])
        return 2

    findings = []
    failed = []
    scanned = 0

    with tempfile.TemporaryDirectory(prefix="educc-linkcheck-") as tmp:
        output = os.path.join(tmp, "program")

        for source in corpus.sources(roots):
            if os.path.exists(output):
                os.unlink(output)

            result = subprocess.run(
                [compiler] + corpus.flags +
                ["-oneline", "-o", output, source,
                 "-lm"],  # what test/testRunner.py links every fixture with
                capture_output=True)

            if result.returncode != 0 or not os.path.exists(output):
                failed.append(source)
                continue

            scanned += 1
            inspect(output, source, findings, target)

    return corpus.report(scanned, failed, findings,
                         "linked executables inspected")


if __name__ == "__main__":
    sys.exit(main())
