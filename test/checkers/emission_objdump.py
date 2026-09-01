#!/usr/bin/env python3
"""Differential emission checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/emission_objdump.py build/bin/main test/testData/codegen

Does what objdump reads out of the emitted bytes say the same thing the
machine IR said? The '-irDump:ra' dump is what selection and allocation
decided; the object file is what stage 3 actually wrote. Anything that goes
wrong strictly inside the assembler - a missing REX bit, a digit in the wrong
field - changes only the second, and is invisible to every check that reads a
dump alone. This is what caught all three of the REX bugs in section 8.

GNU objdump is the oracle rather than EduCC's own disassembler wrapper, so
that a decoding mistake cannot cancel out an encoding one.

The two streams are compared as multisets of (mnemonic, register set) rather
than positionally: emission interposes a prologue and an epilogue the machine
IR does not contain, and a copy between equal registers emits nothing at all.
Only one direction is an error - something the machine IR asked for that the
bytes do not have.
"""
import collections
import os
import re
import shutil
import subprocess
import sys
import tempfile

import corpus

# MIR opcode -> the mnemonics objdump may print for it. Several are one-to-many
# because the width suffix or the immediate form changes the spelling.
MNEMONIC = {
    "add": {"add"}, "sub": {"sub"}, "imul": {"imul"}, "and": {"and"},
    "or": {"or"}, "xor": {"xor"}, "shl": {"shl"}, "shr": {"shr"},
    "sar": {"sar"}, "neg": {"neg"}, "not": {"not"}, "cmp": {"cmp"},
    "test": {"test"}, "mov": {"mov", "movabs"}, "jmp": {"jmp"},
    "ret": {"ret", "leave"},
    "MOP_COPY": {"mov", "movsd", "movss", "movq", "movd", "movaps"},
    "MOP_SPILL": {"mov", "movsd", "movss", "movq", "movd"},
    "MOP_RELOAD": {"mov", "movsd", "movss", "movq", "movd"},
    # cltd/cqto name no operand at all: the register pair is the whole point of
    # the instruction and is implicit in the opcode.
    "cdq": {"cltd", "cqto", "cwtd"},
    "idiv": {"idiv"}, "div": {"div"},
    "call": {"call"}, "lea": {"lea"}, "push": {"push"}, "pop": {"pop"},
    # AT&T spells the operand widths into the mnemonic, so one MIR opcode
    # answers to a family of them.
    "movsx": {"movsbl", "movsbw", "movsbq", "movswl", "movswq", "movslq", "movsxd"},
    # 'movzx.8/4' is a plain 32-bit mov: writing a 32-bit register zeroes the
    # upper half, so that is the whole zero-extension.
    "movzx": {"movzbl", "movzbw", "movzbq", "movzwl", "movzwq", "mov"},
    "movd": {"movd", "movq"},
    "ucomis": {"ucomiss", "ucomisd"}, "comis": {"comiss", "comisd"},
    "cvtf2f": {"cvtss2sd", "cvtsd2ss"},
    "cvtf2si": {"cvttss2si", "cvttsd2si", "cvtss2si", "cvtsd2si"},
    "cvtsi2f": {"cvtsi2ss", "cvtsi2sd", "cvtsi2ssl", "cvtsi2sdl",
                "cvtsi2ssq", "cvtsi2sdq"},
    "fld": {"fld", "flds", "fldl", "fldt"},
    "fstp": {"fstp", "fstps", "fstpl", "fstpt"},
    "fild": {"fild", "filds", "fildl", "fildll"},
    "fistp": {"fistp", "fistps", "fistpl", "fistpll"},
    # Both files' worth: the x87 memory forms and the SSE register forms share
    # one MIR opcode each. Only the x87 half was here, which went unnoticed
    # while every SSE arithmetic instruction had a move of the same two
    # registers beside it for the count to be satisfied by.
    "fadd": {"fadd", "fadds", "faddl", "addss", "addsd"}, "faddp": {"faddp"},
    "fsub": {"fsub", "fsubs", "fsubl", "fsubr", "fsubrs", "fsubrl",
             "subss", "subsd"},
    "fsubp": {"fsubp", "fsubrp"},
    "fmul": {"fmul", "fmuls", "fmull", "mulss", "mulsd"}, "fmulp": {"fmulp"},
    "fdiv": {"fdiv", "fdivs", "fdivl", "fdivr", "fdivrs", "fdivrl",
             "divss", "divsd"},
    "fdivp": {"fdivp", "fdivrp"},
    "fldcw": {"fldcw"}, "fnstcw": {"fnstcw"},
    "fcomip": {"fcomip"}, "fucomip": {"fucomip"},
    "setp": {"setp"}, "setnp": {"setnp"},
}
SSE_MOVES = frozenset({"movsd", "movss", "movq", "movd", "movaps", "movapd"})
for cc in ("e", "ne", "l", "le", "g", "ge", "b", "be", "a", "ae"):
    MNEMONIC["set" + cc] = {"set" + cc}
    MNEMONIC["j" + cc] = {"j" + cc}

# objdump prints the 32-bit and 8-bit names; map every spelling of a register
# back to its 64-bit name, which is what the MIR uses.
REGS = {}
for wide, parts in {
    "rax": "eax ax al", "rcx": "ecx cx cl", "rdx": "edx dx dl", "rbx": "ebx bx bl",
    "rsp": "esp sp spl", "rbp": "ebp bp bpl", "rsi": "esi si sil", "rdi": "edi di dil",
}.items():
    REGS[wide] = wide
    for p in parts.split():
        REGS[p] = wide
for n in range(8, 16):
    for suffix in ("", "d", "w", "b"):
        REGS["r%d%s" % (n, suffix)] = "r%d" % n
for n in range(16):
    REGS["xmm%d" % n] = "xmm%d" % n


def mirFunctions(path):
    """(name, [(opcode, regs), ...]) for each fully selected and allocated function.

    A function that still has an unselected instruction or that allocation
    declined is skipped: its bytes have no relation to this machine IR.
    """
    out, cur, skipped = [], None, set()
    for line in open(path):
        line = line.rstrip("\n")
        if line.startswith("MachineFunction"):
            cur = (re.search(r"'([^']*)'", line).group(1), [])
            out.append(cur)
        elif cur is None:
            continue
        elif line.startswith("Registers: not allocated") or "MOP_UNSELECTED" in line:
            skipped.add(cur[0])
        elif not line.startswith("  "):
            continue
        elif line.startswith("  %") or line.startswith("  fi#") or line.startswith("  jt#"):
            continue  # the vreg, frame and jump tables
        else:
            # Implicit operands - the dividend halves, both division results -
            # constrain allocation but encode to nothing, so they are not in
            # the bytes and must not be looked for there.
            body = line.split(" ; ")[0].split("[")[0].strip()
            m = re.match(r"(?:\S+ = )?(\w+)(?:\.\d+(?:/\d+)?)?(.*)$", body)
            if m and m.group(1) not in ("Frame:",):
                regs = [REGS[r] for r in re.findall(r"\$(\w+)", body) if r in REGS]
                out[-1][1].append((m.group(1), regs))
    return [f for f in out if f[0] not in skipped]


def objdumpFunctions(path):
    txt = subprocess.run(["objdump", "-d", "--no-show-raw-insn", path],
                         capture_output=True, text=True, check=True).stdout
    out, cur = [], None
    for line in txt.splitlines():
        m = re.match(r"[0-9a-f]+ <(\w+)>:", line)
        if m:
            cur = (m.group(1), [])
            out.append(cur)
            continue
        m = re.match(r"\s+[0-9a-f]+:\s+(\S+)\s*(.*)$", line)
        if m and cur is not None:
            mnem, args = m.group(1), m.group(2).split("#")[0]
            regs = [REGS[r] for r in re.findall(r"%(\w+)", args) if r in REGS]
            cur[1].append((mnem, regs))
    return out


def checkSource(compiler, source, obj, dump, stats):
    r = subprocess.run([compiler, "-oneline", "-c", "-o", obj,
                        "-irDump:ra", dump, source], capture_output=True)
    if r.returncode != 0 or not os.path.exists(obj):
        return None

    mir = dict(mirFunctions(dump))
    findings = []

    for name, actual in objdumpFunctions(obj):
        if name not in mir:
            continue
        stats["functions"] += 1

        want = collections.Counter()
        for opcode, regs in mir[name]:
            names = MNEMONIC.get(opcode)
            if names is None:
                if opcode not in stats["unknown"]:
                    stats["unknown"].add(opcode)
                continue
            stats["instrs"] += 1
            if opcode == "cdq":
                regs = []
            # A move naming an SSE register is spelled by its element type -
            # 'mov.8 $xmm8' is a movsd - which the MIR opcode does not record.
            if any(r.startswith("xmm") for r in regs):
                names = names | SSE_MOVES
            want[(frozenset(names), tuple(sorted(set(regs))))] += 1

        have = collections.Counter()
        for mnem, regs in actual:
            have[(mnem, tuple(sorted(set(regs))))] += 1

        for (names, regs), count in want.items():
            found = sum(c for (m, rs), c in have.items()
                        if m in names and set(regs) <= set(rs))
            if found < count:
                nonEmpty = regs and not set(regs) <= {"rbp", "rsp"}
                if found == 0 and not nonEmpty:
                    continue  # a register-free instruction folded away
                findings.append(f"{source} [{name}]: machine IR has {count} x "
                                f"({'/'.join(sorted(names))} {list(regs)}), bytes have {found}")
    return findings


def main():
    compiler, roots = corpus.parseArgs(__doc__)
    if not shutil.which("objdump"):
        print("objdump not found; this checker needs it as its oracle")
        return 2
    files = corpus.sources(roots)

    findings, failed = [], []
    stats = {"functions": 0, "instrs": 0, "unknown": set()}
    with tempfile.TemporaryDirectory() as tmp:
        obj, dump = os.path.join(tmp, "d.o"), os.path.join(tmp, "ra.txt")
        for f in files:
            fs = checkSource(compiler, f, obj, dump, stats)
            if fs is None:
                failed.append(f)
            else:
                findings += fs

    if stats["unknown"]:
        print("opcodes with no oracle (not compared): "
              + ", ".join(sorted(stats["unknown"])))
    return corpus.report(len(files), failed, findings,
                         f"functions: {stats['functions']}, instructions compared: "
                         f"{stats['instrs']}")


if __name__ == "__main__":
    sys.exit(main())
