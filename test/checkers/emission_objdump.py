#!/usr/bin/env python3
"""Differential emission checker of docs/ir-codegen-design.md section 9.

    python3 test/checkers/emission_objdump.py build/bin/main test/testData/codegen

Does what objdump reads out of the emitted bytes say the same thing the
machine IR said? The '-irDump:ra' dump is what selection and allocation
decided; the object file is what stage 3 actually wrote. Anything that goes
wrong strictly inside the assembler - a missing REX bit, a digit in the wrong
field - changes only the second, and is invisible to every check that reads a
dump alone. This is what caught all three of the REX bugs in section 8.

GNU objdump is the oracle rather than EduCC's own disassembler, so that a
decoding mistake cannot cancel out an encoding one. Each machine brings its
own objdump, its own register names and its own table of what a MIR opcode
may be spelled as in the bytes; everything else about the walk is the same.

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
X86_MNEMONIC = {
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
    # 'rep movsb', whose first word is all the parse below keeps. Its three
    # registers are implicit on both sides - the machine IR puts them in the
    # bracketed annotation the parse drops, and objdump prints two of the three
    # as string operands - so the mnemonic is the whole of the comparison.
    "rep": {"rep"},
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
    X86_MNEMONIC["set" + cc] = {"set" + cc}
    X86_MNEMONIC["j" + cc] = {"j" + cc}

# objdump prints the 32-bit and 8-bit names; map every spelling of a register
# back to its 64-bit name, which is what the MIR uses.
X86_REGS = {}
for wide, parts in {
    "rax": "eax ax al", "rcx": "ecx cx cl", "rdx": "edx dx dl", "rbx": "ebx bx bl",
    "rsp": "esp sp spl", "rbp": "ebp bp bpl", "rsi": "esi si sil", "rdi": "edi di dil",
}.items():
    X86_REGS[wide] = wide
    for p in parts.split():
        X86_REGS[p] = wide
for n in range(8, 16):
    for suffix in ("", "d", "w", "b"):
        X86_REGS["r%d%s" % (n, suffix)] = "r%d" % n
for n in range(16):
    X86_REGS["xmm%d" % n] = "xmm%d" % n

# riscv64. Shorter and flatter than x86's: one width per instruction rather
# than a suffix on the mnemonic, and the MIR already spells a register exactly
# as objdump does. What makes an entry one-to-many here is the immediate form
# ('add' is also 'addi' and, with a zero immediate, 'mv') and the
# pseudo-instructions objdump prints in place of a canonical encoding.
RISCV_MNEMONIC = {
    # A constant takes as many instructions as it takes, and an address under
    # medlow is a lui/addi pair - so both of these are one MIR instruction and
    # several in the bytes. Only one direction is checked, so that is sound.
    "li": {"li", "lui", "addi", "addiw", "slli", "add", "c.li"},
    # 'add' because a frame offset too wide for a twelve-bit immediate is
    # materialized into t0 and added, rather than folded into an addi.
    "la": {"la", "lui", "auipc", "addi", "li", "mv", "add"},

    "l": {"lb", "lh", "lw", "ld"},
    "lu": {"lbu", "lhu", "lwu", "ld"},
    "s": {"sb", "sh", "sw", "sd"},

    # A widening is a shift pair, or one instruction where RV64 has one.
    "sext": {"sext.w", "addiw", "slli", "srai", "mv"},
    "zext": {"zext.b", "andi", "slli", "srli", "mv"},

    "add": {"add", "addi", "mv", "nop"},
    "addw": {"addw", "addiw", "sext.w"},
    "sub": {"sub", "neg", "addi"},
    "subw": {"subw", "negw", "addiw", "sext.w"},
    "mul": {"mul"}, "mulw": {"mulw"},
    "div": {"div"}, "divu": {"divu"}, "divw": {"divw"}, "divuw": {"divuw"},
    "rem": {"rem"}, "remu": {"remu"}, "remw": {"remw"}, "remuw": {"remuw"},
    "and": {"and", "andi", "zext.b"},
    "or": {"or", "ori"},
    "xor": {"xor", "xori", "not"},
    "sll": {"sll", "slli"}, "srl": {"srl", "srli"}, "sra": {"sra", "srai"},
    "sllw": {"sllw", "slliw"}, "srlw": {"srlw", "srliw"}, "sraw": {"sraw", "sraiw"},
    "slt": {"slt", "slti", "sltz", "sgtz"},
    "sltu": {"sltu", "sltiu", "seqz", "snez"},
    "neg": {"neg", "negw", "sub"},
    "not": {"not", "xori"},

    "fadd": {"fadd.s", "fadd.d"}, "fsub": {"fsub.s", "fsub.d"},
    "fmul": {"fmul.s", "fmul.d"}, "fdiv": {"fdiv.s", "fdiv.d"},
    "fneg": {"fneg.s", "fneg.d", "fsgnjn.s", "fsgnjn.d"},
    "fabs": {"fabs.s", "fabs.d", "fsgnjx.s", "fsgnjx.d"},
    "fl": {"flw", "fld"}, "fs": {"fsw", "fsd"},
    "feq": {"feq.s", "feq.d"}, "flt": {"flt.s", "flt.d"}, "fle": {"fle.s", "fle.d"},
    "fcvt.f2f": {"fcvt.s.d", "fcvt.d.s"},
    "fcvt.i2f": {"fcvt.s.w", "fcvt.s.l", "fcvt.d.w", "fcvt.d.l"},
    "fcvt.u2f": {"fcvt.s.wu", "fcvt.s.lu", "fcvt.d.wu", "fcvt.d.lu"},
    "fcvt.f2i": {"fcvt.w.s", "fcvt.l.s", "fcvt.w.d", "fcvt.l.d"},
    "fcvt.f2u": {"fcvt.wu.s", "fcvt.lu.s", "fcvt.wu.d", "fcvt.lu.d"},
    "fmv.x": {"fmv.x.w", "fmv.x.d"},
    "fmv.f": {"fmv.w.x", "fmv.d.x"},

    "j": {"j", "jal"},
    "jr": {"jr", "jalr"},
    # auipc+jalr for a far call; the epilogue is what 'ret' expands into.
    "call": {"jal", "jalr", "call", "auipc"},
    "ret": {"ret", "jalr", "ld", "addi", "mv"},

    # A B-type branch reaches only +/-4KiB, and one that outgrew that is
    # emitted as the *opposite* branch over a 'j' - so each of these has to
    # accept its own inverse. What the relaxation did with the condition is
    # jump_relaxation.py's question; this one is about the registers.
    "beq": {"beq", "beqz", "bne", "bnez"},
    "bne": {"bne", "bnez", "beq", "beqz"},
    "blt": {"blt", "bltz", "bgtz", "bgt", "bge", "bgez", "blez", "ble"},
    "bge": {"bge", "bgez", "blez", "ble", "blt", "bltz", "bgtz", "bgt"},
    "bltu": {"bltu", "bgtu", "bgeu", "bleu"},
    "bgeu": {"bgeu", "bleu", "bltu", "bgtu"},

    "MOP_COPY": {"mv", "addi", "fmv.s", "fmv.d", "fsgnj.s", "fsgnj.d"},
    "MOP_SPILL": {"sb", "sh", "sw", "sd", "fsw", "fsd"},
    "MOP_RELOAD": {"lb", "lh", "lw", "ld", "flw", "fld"},
}

# The MIR names a register exactly as objdump does here, there being no
# sub-registers to map back to a wide name.
RISCV_REGS = {}
for _names in (("zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1"),
               tuple("a%d" % n for n in range(8)),
               tuple("s%d" % n for n in range(2, 12)),
               tuple("t%d" % n for n in range(3, 7)),
               tuple("ft%d" % n for n in range(8)),
               ("fs0", "fs1"),
               tuple("fa%d" % n for n in range(8)),
               tuple("fs%d" % n for n in range(2, 12)),
               tuple("ft%d" % n for n in range(8, 12))):
    for _r in _names:
        RISCV_REGS[_r] = _r

# AT&T marks a register with '%'; riscv64's assembly syntax does not, so a
# register there is any bare word the table above knows.
#
# The fourth entry is the registers that are not operands for this comparison.
# x0 is hardwired to zero, so naming it is a spelling choice rather than a
# fact about allocation: objdump writes 'bnez a0' where the MIR writes
# 'bne $a0, $zero', and 'snez t1,t2' where it writes 'sltu $t1, $zero, $t2'.
TABLES = {
    "x86_64": (X86_MNEMONIC, X86_REGS, re.compile(r"%(\w+)"), frozenset()),
    "riscv64": (RISCV_MNEMONIC, RISCV_REGS, re.compile(r"\b([a-z][a-z0-9]*)\b"),
                frozenset({"zero"})),
}


def mirFunctions(path, regTable, elided):
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
            # The opcode is the first token with its width suffix taken off -
            # '.8', or '.8/4' where a conversion names both. An opcode may
            # itself hold dots ('fcvt.f2f'), so this strips the suffix rather
            # than splitting on the first one.
            m = re.match(r"(?:\S+ = )?(\S+?)(?:\.\d+(?:/\d+)?)?(?:\s|$)", body)
            if m and m.group(1) not in ("Frame:",):
                regs = [regTable[r] for r in re.findall(r"\$(\w+)", body)
                        if r in regTable and r not in elided]
                out[-1][1].append((m.group(1), regs))
    return [f for f in out if f[0] not in skipped]


def objdumpFunctions(path, tool, regTable, regExpr, elided):
    txt = subprocess.run([tool, "-d", "--no-show-raw-insn", path],
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
            regs = [regTable[r] for r in regExpr.findall(args)
                    if r in regTable and r not in elided]
            cur[1].append((mnem, regs))
    return out


def checkSource(compiler, source, obj, dump, stats, tool, tables):
    r = subprocess.run([compiler] + corpus.flags + ["-oneline", "-c", "-o", obj,
                        "-irDump:ra", dump, source], capture_output=True)
    if r.returncode != 0 or not os.path.exists(obj):
        return None

    mnemonics, regTable, regExpr, elided = tables
    mir = dict(mirFunctions(dump, regTable, elided))
    findings = []

    for name, actual in objdumpFunctions(obj, tool, regTable, regExpr, elided):
        if name not in mir:
            continue
        stats["functions"] += 1

        want = collections.Counter()
        for opcode, regs in mir[name]:
            names = mnemonics.get(opcode)
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
    target = corpus.target()
    tool = target["objdump"]
    if not shutil.which(tool):
        print("%s not found; this checker needs it as its oracle" % tool)
        return 2
    tables = TABLES[corpus.targetName()]
    files = corpus.sources(roots)

    findings, failed = [], []
    stats = {"functions": 0, "instrs": 0, "unknown": set()}
    with tempfile.TemporaryDirectory() as tmp:
        obj, dump = os.path.join(tmp, "d.o"), os.path.join(tmp, "ra.txt")
        for f in files:
            fs = checkSource(compiler, f, obj, dump, stats, tool, tables)
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
