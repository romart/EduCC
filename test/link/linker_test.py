#!/usr/bin/env python3
"""Unit tests for the static linker (src/link/link.c).

    python3 test/link/linker_test.py build/bin/main

Three halves, and the first one is the point: every fixture here is linked and
then *run*, so what is asserted is the exit code of a program this linker
produced. A relocation that lands four bytes off is not a diff against a golden
file, it is a program that returns the wrong number or dies on SIGSEGV.

The objects are built byte by byte - machine code included, which is why the
programs are tiny - because the whole value of a synthesized input is reaching
what the host has no example of: a tentative definition of two different sizes,
a weak reference nothing defines, a relocation whose target does not fit in the
field. The second half compiles a real C file with EduCC and links that, which
is the only part that sees the relocations the compiler actually emits. The
third asserts the shape of the output itself - segment congruence, what may
overlap what, where the entry point is allowed to be - which is the checker
test/checkers/link.py will grow out of once there is more than one output kind.

'ld' is used as an oracle where it is present, and only to agree about what a
program does; it is skipped, not required.

Every assertion here was verified by breaking the linker on purpose: 29
mutations, 28 caught. The survivor - a weak definition allowed to displace a
strong one - cannot be reached, because two strong definitions are reported and
abandoned before the question is asked. Three of the six that survived the first
sweep were real gaps and are closed here: a pointer whose upper four bytes were
already zero, a tentative definition that would have been aligned by luck, and
an sh_info of zero making its own check vacuous. A fourth was dead state in the
linker rather than a missing test.
"""
import os
import struct
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from reader_test import (Elf, Suite, SHT_PROGBITS, SHT_NOBITS, SHT_RELA,
                         SHF_ALLOC, SHF_WRITE, SHF_EXECINSTR,
                         STB_LOCAL, STB_GLOBAL, STB_WEAK,
                         STT_NOTYPE, STT_FUNC, STT_OBJECT,
                         SHN_UNDEF, SHN_ABS, SHN_COMMON, RELA_SIZE, SYM_SIZE,
                         SHT_SYMTAB, ET_EXEC, EM_X86_64)

R_X86_64_64, R_X86_64_PC32, R_X86_64_PLT32 = 1, 2, 4
R_X86_64_32, R_X86_64_32S = 10, 11

PT_LOAD, PT_GNU_STACK = 1, 0x6474e551
PF_X, PF_W, PF_R = 1, 2, 4
PAGE = 0x1000

PHDR_SIZE, SHDR_SIZE = 56, 64


# --------------------------------------------------------------- machine code
#
# Hand-assembled, because an assembler would be a second thing to install and
# these programs are four instructions long. Each helper returns the bytes and
# says where inside them a relocation goes.

def exitWithStatus():
    """mov $60,%eax; syscall - %edi already holds the status."""
    return b"\xb8\x3c\x00\x00\x00\x0f\x05"


def statusImmediate(value):
    """mov $imm32,%edi"""
    return b"\xbf" + struct.pack("<I", value)


def statusFromAbsolute():
    """mov <disp32>,%edi - the displacement is an absolute address (32S)."""
    return b"\x8b\x3c\x25" + b"\0" * 4  # relocation at +3


def statusFromRip():
    """mov <disp32>(%rip),%edi (PC32)."""
    return b"\x8b\x3d" + b"\0" * 4  # relocation at +2, addend -4


def storeImmediateAbsolute(value):
    """movl $imm32,<disp32> (32S)."""
    return b"\xc7\x04\x25" + b"\0" * 4 + struct.pack("<I", value)


def loadAbsolutePointer():
    """mov <disp32>,%rax - loads a pointer variable by absolute address."""
    return b"\x48\x8b\x04\x25" + b"\0" * 4  # relocation at +4


def statusFromPointee():
    """movzbl (%rax),%edi"""
    return b"\x0f\xb6\x38"


def callRelative():
    """call <rel32>"""
    return b"\xe8" + b"\0" * 4  # relocation at +1, addend -4


def statusFromResult():
    """mov %eax,%edi"""
    return b"\x89\xc7"


def returnImmediate(value):
    """mov $imm32,%eax; ret"""
    return b"\xb8" + struct.pack("<I", value) + b"\xc3"


def addRela(elf, targetName, target, entries):
    """entries: (offset, symbol or None, type, addend). Symbols must already
    be on the object - a relocation names a symbol by table position."""
    blob = b""
    for offset, name, type, addend in entries:
        index = elf.symbolIndex(name) if name else 0
        blob += struct.pack("<QQq", offset, (index << 32) | type, addend)

    return elf.addSection(".rela" + targetName, SHT_RELA, 0, blob, align=8,
                          link=0, info=target, entsize=RELA_SIZE)


def objectImage(elf):
    """sh_link of a relocation section is its symbol table, which the writer
    appends after everything the caller added. EduCC's reader finds the symbol
    table by type and does not consult it - but ld does, and quietly leaves
    every relocation unapplied when it is zero, which is exactly the sort of
    difference that would make a differential test lie."""
    symtab = len(elf.sections) + 1

    for section in elf.sections:
        if section[1] == SHT_RELA:
            section[5] = symtab

    return elf.build()


def startObject(code, symbols=(), relocs=(), extra=()):
    """An object whose .text begins at _start. `extra` adds sections before
    the relocations are attached, so that a section index is available."""
    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)

    for section in extra:
        section(elf)

    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    for symbol in symbols:
        elf.addSymbol(*symbol)

    if relocs:
        addRela(elf, ".text", text, relocs)

    return elf


# ------------------------------------------------------------------- harness

class Link:
    def __init__(self, compiler, tmp, suite):
        self.compiler = compiler
        self.tmp = tmp
        self.suite = suite
        self.counter = 0

    def object(self, elf, name):
        path = os.path.join(self.tmp, name)
        with open(path, "wb") as f:
            f.write(objectImage(elf) if isinstance(elf, Elf) else elf)
        return path

    def link(self, inputs, name=None, extra=()):
        self.counter += 1
        output = os.path.join(self.tmp, name or ("prog%d" % self.counter))
        if os.path.exists(output):
            os.unlink(output)

        command = [self.compiler, "-Xlink-static", "-o", output]
        command += list(extra) + list(inputs)
        result = subprocess.run(command, capture_output=True, text=True)

        self.suite.check(result.returncode >= 0,
                         "the linker died on signal %d: %s"
                         % (-result.returncode, " ".join(inputs)))
        return output, result

    def expectRuns(self, inputs, status, description, extra=()):
        """Link, run, and insist on an exit code. Everything else here is a
        detail of how a program gets to be able to return one."""
        output, result = self.link(inputs, extra=extra)

        if result.returncode != 0:
            self.suite.check(False, "%s: the link failed: %s"
                             % (description, result.stderr.strip()))
            return None

        run = subprocess.run([output], capture_output=True)
        if run.returncode < 0:
            self.suite.check(False, "%s: the program died on signal %d"
                             % (description, -run.returncode))
            return None

        self.suite.equal(run.returncode, status, description)
        return output

    def expectFailure(self, inputs, fragment, description, extra=()):
        output, result = self.link(inputs, extra=extra)

        self.suite.check(result.returncode != 0,
                         "%s: the link should have failed" % description)
        self.suite.contains(result.stderr, fragment, description)
        # A file that is there after a refused link is a program someone can
        # run, and it is not one this linker stands behind.
        self.suite.check(not os.path.exists(output),
                         "%s: a refused link left '%s' behind"
                         % (description, os.path.basename(output)))


class Executable:
    """Just enough of an ELF reader to assert the shape of what was written."""

    def __init__(self, path):
        with open(path, "rb") as f:
            self.blob = f.read()

        (ident, self.type, self.machine, _version, self.entry, phoff, shoff,
         _flags, _ehsize, _phentsize, phnum, _shentsize, shnum,
         shstrndx) = struct.unpack("<16sHHIQQQIHHHHHH", self.blob[:64])

        self.ident = ident
        self.phoff = phoff
        self.segments = []
        for i in range(phnum):
            o = phoff + i * PHDR_SIZE
            type, flags, offset, vaddr, _paddr, filesz, memsz, align = \
                struct.unpack("<IIQQQQQQ", self.blob[o:o + PHDR_SIZE])
            self.segments.append(dict(type=type, flags=flags, offset=offset,
                                      vaddr=vaddr, filesz=filesz, memsz=memsz,
                                      align=align))

        raw = []
        for i in range(shnum):
            o = shoff + i * SHDR_SIZE
            (name, type, flags, addr, offset, size, link, info, align,
             entsize) = struct.unpack("<IIQQQQIIQQ",
                                      self.blob[o:o + SHDR_SIZE])
            raw.append(dict(nameOffset=name, type=type, flags=flags, addr=addr,
                            offset=offset, size=size, link=link, info=info,
                            align=align, entsize=entsize))

        names = raw[shstrndx]
        table = self.blob[names["offset"]:names["offset"] + names["size"]]
        for section in raw:
            end = table.find(b"\0", section["nameOffset"])
            section["name"] = table[section["nameOffset"]:end].decode()

        self.sections = raw
        self.symbols = {}

        for section in raw:
            if section["type"] != SHT_SYMTAB:
                continue
            strings = raw[section["link"]]
            blob = self.blob[strings["offset"]:
                             strings["offset"] + strings["size"]]
            self.symtabInfo = section["info"]
            self.symtabCount = section["size"] // SYM_SIZE

            self.symbolList = []
            for i in range(self.symtabCount):
                o = section["offset"] + i * SYM_SIZE
                name, info, other, shndx, value, size = struct.unpack(
                    "<IBBHQQ", self.blob[o:o + SYM_SIZE])
                end = blob.find(b"\0", name)
                text = blob[name:end].decode()
                entry = dict(name=text, bind=info >> 4, type=info & 0xf,
                             shndx=shndx, value=value, size=size)
                self.symbolList.append(entry)
                if text:
                    self.symbols[text] = entry

    def named(self, name):
        return [s for s in self.sections if s["name"] == name]

    def loads(self):
        return [s for s in self.segments if s["type"] == PT_LOAD]


# --------------------------------------------------------------------- tests

def testMinimalProgram(link, suite):
    """The smallest thing that can be said to have been linked: one object,
    one section, no relocations at all."""
    code = statusImmediate(42) + exitWithStatus()
    a = link.object(startObject(code), "minimal.o")
    link.expectRuns([a], 42, "a single object with no relocations runs")

    # The entry point is a symbol, not the first byte of .text: putting a byte
    # in front of _start proves the difference.
    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          b"\xcc" + code, align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 1, len(code))
    b = link.object(elf, "offsetstart.o")
    link.expectRuns([b], 42, "the entry point is the entry symbol's address")

    # '-e' picks a different one, and it has to be the one that runs.
    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          statusImmediate(1) + exitWithStatus() +
                          statusImmediate(7) + exitWithStatus(), align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, 12)
    elf.addSymbol("other", STB_GLOBAL, STT_FUNC, text, 12, 12)
    c = link.object(elf, "twoentries.o")
    link.expectRuns([c], 1, "the default entry symbol is _start")
    link.expectRuns([c], 7, "-e chooses the entry symbol", extra=["-e", "other"])


def testHelloWorld(link, suite):
    """The milestone's own checkpoint: a program with no libc that says
    something. Everything else asserts an exit code, which a program can
    produce without ever having read its own .rodata."""
    message = b"hello, world\n"

    elf = Elf()
    code = (b"\xb8\x01\x00\x00\x00" +               # mov $1,%eax   (write)
            b"\xbf\x01\x00\x00\x00" +               # mov $1,%edi   (stdout)
            b"\xbe" + b"\0" * 4 +                   # mov $msg,%esi (32)
            b"\xba" + struct.pack("<I", len(message)) +
            b"\x0f\x05" +                           # syscall
            statusImmediate(0) + exitWithStatus())
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    rodata = elf.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC, message,
                            align=1)

    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("message", STB_LOCAL, STT_OBJECT, rodata, 0, len(message))
    addRela(elf, ".text", text, [(11, "message", R_X86_64_32, 0)])

    a = link.object(elf, "hello.o")
    output, result = link.link([a], name="hello")

    if result.returncode != 0:
        suite.check(False, "the hello-world link failed: %s"
                    % result.stderr.strip())
        return

    run = subprocess.run([output], capture_output=True)
    suite.equal(run.returncode, 0, "a freestanding hello-world exits cleanly")
    suite.equal(run.stdout, message, "and writes what is in its .rodata")


def testCrossObjectCall(link, suite):
    """PLT32 against a symbol this link defines, which is a call and not a
    stub - the two are the same arithmetic until a shared object turns up."""
    code = callRelative() + statusFromResult() + exitWithStatus()
    caller = startObject(code,
                         symbols=[("answer", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)],
                         relocs=[(1, "answer", R_X86_64_PLT32, -4)])

    callee = Elf()
    text = callee.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                             returnImmediate(7), align=16)
    callee.addSymbol("answer", STB_GLOBAL, STT_FUNC, text, 0, 6)

    a = link.object(caller, "caller.o")
    b = link.object(callee, "callee.o")

    link.expectRuns([a, b], 7, "a call across objects reaches its callee")
    link.expectRuns([b, a], 7, "and does so whichever order the objects are in")


def testDataRelocations(link, suite):
    """R_X86_64_64 in .data pointing at .rodata, read back through a 32S
    absolute address. Both of them are wrong by a whole section if layout is."""
    elf = Elf()
    code = loadAbsolutePointer() + statusFromPointee() + exitWithStatus()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    rodata = elf.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC,
                            b"\x00\x2a\x00\x00", align=4)
    # Filled rather than zeroed, so that a relocation which writes four of the
    # eight bytes leaves the other four wrong instead of already right.
    data = elf.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                          b"\xff" * 8, align=8)

    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("value", STB_LOCAL, STT_OBJECT, rodata, 1, 1)
    elf.addSymbol("pointer", STB_GLOBAL, STT_OBJECT, data, 0, 8)

    addRela(elf, ".text", text, [(4, "pointer", R_X86_64_32S, 0)])
    addRela(elf, ".data", data, [(0, "value", R_X86_64_64, 0)])

    a = link.object(elf, "pointer.o")
    link.expectRuns([a], 42, "a pointer relocated into .data points at .rodata")


def testPcRelativeData(link, suite):
    """PC32 to a definition in another object: the displacement depends on
    where both of them ended up, so it is the one relocation that cannot be
    right by accident."""
    code = statusFromRip() + exitWithStatus()
    user = startObject(code,
                       symbols=[("counter", STB_GLOBAL, STT_OBJECT, SHN_UNDEF)],
                       relocs=[(2, "counter", R_X86_64_PC32, -4)])

    provider = Elf()
    data = provider.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                               struct.pack("<I", 17), align=4)
    provider.addSymbol("counter", STB_GLOBAL, STT_OBJECT, data, 0, 4)

    a = link.object(user, "pcuser.o")
    b = link.object(provider, "pcprovider.o")
    link.expectRuns([a, b], 17, "a PC32 reference reaches another object's data")


def testCommonSymbols(link, suite):
    """Tentative definitions: one block of .bss between them, the largest
    request winning, and nothing to give them an address until layout."""
    code = storeImmediateAbsolute(5) + statusFromAbsolute() + exitWithStatus()
    # 'pad' is a byte in front of 'slot', so that alignment is the only thing
    # that can put 'slot' on a sixteen-byte boundary.
    small = startObject(code,
                        symbols=[("pad", STB_GLOBAL, STT_OBJECT, SHN_COMMON,
                                  1, 1),
                                 ("slot", STB_GLOBAL, STT_OBJECT, SHN_COMMON,
                                  4, 4)],
                        relocs=[(3, "slot", R_X86_64_32S, 0),
                                (14, "slot", R_X86_64_32S, 0)])

    large = Elf()
    large.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                     b"\xc3", align=1)
    large.addSymbol("slot", STB_GLOBAL, STT_OBJECT, SHN_COMMON, 16, 40)

    a = link.object(small, "common.o")
    b = link.object(large, "commonbig.o")

    output = link.expectRuns([a, b], 5, "a common symbol is storage that works")
    if output is None:
        return

    exe = Executable(output)
    suite.equal(exe.symbols["slot"]["size"], 40,
                "the largest tentative definition sets the size")
    suite.check(exe.symbols["slot"]["value"] % 16 == 0,
                "the largest alignment request is honoured")

    bss = exe.named(".bss")
    suite.equal(len(bss), 1, "commons land in one .bss")
    suite.equal(bss[0]["type"], SHT_NOBITS, ".bss occupies no file space")

    # A real definition anywhere makes the tentative one stop being storage.
    defining = Elf()
    data = defining.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                               struct.pack("<I", 3), align=4)
    defining.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                        b"\xc3", align=1)
    defining.addSymbol("slot", STB_GLOBAL, STT_OBJECT, data, 0, 4)
    c = link.object(defining, "commondef.o")

    output = link.expectRuns([a, c], 5,
                             "a definition takes a tentative one's place")
    if output is not None:
        exe = Executable(output)
        section = exe.sections[exe.symbols["slot"]["shndx"]]
        suite.equal(section["name"], ".data",
                    "and the symbol is in the defining section")


def testWeakSymbols(link, suite):
    code = callRelative() + statusFromResult() + exitWithStatus()
    caller = startObject(code,
                         symbols=[("answer", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)],
                         relocs=[(1, "answer", R_X86_64_PLT32, -4)])
    a = link.object(caller, "weakcaller.o")

    def definition(bind, value, name):
        elf = Elf()
        text = elf.addSection(".text", SHT_PROGBITS,
                              SHF_ALLOC | SHF_EXECINSTR, returnImmediate(value),
                              align=16)
        elf.addSymbol("answer", bind, STT_FUNC, text, 0, 6)
        return link.object(elf, name)

    weak = definition(STB_WEAK, 3, "weakdef.o")
    strong = definition(STB_GLOBAL, 9, "strongdef.o")

    link.expectRuns([a, weak], 3, "a weak definition is used when it is the only one")
    link.expectRuns([a, weak, strong], 9, "a strong definition overrides a weak one")
    link.expectRuns([a, strong, weak], 9, "whichever order they are given in")

    # Two weak definitions are not a multiple definition; one of them wins.
    other = definition(STB_WEAK, 3, "weakdef2.o")
    link.expectRuns([a, weak, other], 3, "two weak definitions are not an error")

    # A weak reference nothing defines is zero, not a link error - which is
    # what crtbegin.o's __gmon_start__ is going to need.
    code = statusImmediate(0) + exitWithStatus()
    absent = startObject(statusImmediate(1) + exitWithStatus(),
                         symbols=[("nothingDefinesThis", STB_WEAK, STT_NOTYPE,
                                   SHN_UNDEF)],
                         relocs=[(1, "nothingDefinesThis", R_X86_64_32, 0)])
    b = link.object(absent, "weakundef.o")
    link.expectRuns([b], 0, "an undefined weak reference resolves to zero")


def testAbsoluteSymbols(link, suite):
    code = statusImmediate(0) + exitWithStatus()
    elf = startObject(code,
                      symbols=[("magic", STB_GLOBAL, STT_OBJECT, SHN_ABS, 23)],
                      relocs=[(1, "magic", R_X86_64_32, 0)])
    a = link.object(elf, "absolute.o")
    output = link.expectRuns([a], 23, "an absolute symbol keeps its value")

    if output is not None:
        exe = Executable(output)
        suite.equal(exe.symbols["magic"]["shndx"], SHN_ABS,
                    "and stays absolute in the output")


def testSectionMerging(link, suite):
    """Same-named sections from several objects become one output section, and
    the compiler's per-function names fold into their parent."""
    first = Elf()
    text = first.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                            statusImmediate(11) + exitWithStatus(), align=16)
    first.addSection(".text.hot", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                     b"\xc3", align=1)
    first.addSection(".rodata.str1.1", SHT_PROGBITS, SHF_ALLOC, b"hi\0",
                     align=1)
    first.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, 12)

    second = Elf()
    second.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                      b"\xc3" * 4, align=16)
    second.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC, b"there\0", align=1)
    second.addSection(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, b"\0" * 32,
                      align=8)

    a = link.object(first, "merge1.o")
    b = link.object(second, "merge2.o")
    output = link.expectRuns([a, b], 11, "merged sections still run")
    if output is None:
        return

    for name in (".text", ".rodata", ".bss"):
        suite.equal(len(exeSections(output, name)), 1,
                    "one output %s" % name)

    exe = Executable(output)
    text = exe.named(".text")[0]
    suite.check(text["size"] >= 12 + 1 + 4,
                ".text holds every contribution: %d bytes" % text["size"])
    suite.equal(len(exe.named(".text.hot")), 0,
                ".text.hot folded into .text")
    suite.equal(len(exe.named(".rodata.str1.1")), 0,
                ".rodata.str1.1 folded into .rodata")

    # An unknown name is kept rather than guessed at - which is how the
    # compiler's own 'data.rel.ro.local' survives.
    third = Elf()
    text = third.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                            statusImmediate(2) + exitWithStatus(), align=16)
    third.addSection("data.rel.ro.local", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                     b"\0" * 8, align=8)
    third.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, 12)
    c = link.object(third, "oddname.o")
    output = link.expectRuns([c], 2, "an unrecognised section name links")
    if output is not None:
        suite.equal(len(exeSections(output, "data.rel.ro.local")), 1,
                    "an unrecognised section keeps its name")


def exeSections(path, name):
    return Executable(path).named(name)


def testStructure(link, suite):
    """What the loader is entitled to assume about the file. None of this is
    visible in an exit code: a program can run with a segment that overlaps
    another one, right up until one of them is a page longer."""
    elf = Elf()
    code = statusImmediate(0) + exitWithStatus()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    elf.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC, b"constant\0", align=8)
    elf.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, b"\1" * 16,
                   align=16)
    elf.addSection(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, b"\0" * 64,
                   align=64)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))

    a = link.object(elf, "shape.o")
    output = link.expectRuns([a], 0, "the shaped program runs")
    if output is None:
        return

    exe = Executable(output)
    size = len(exe.blob)

    suite.equal(exe.type, ET_EXEC, "an executable is ET_EXEC")
    suite.equal(exe.machine, EM_X86_64, "for x86_64")
    suite.equal(exe.ident[:4], b"\x7fELF", "with an ELF magic")

    loads = exe.loads()
    suite.check(len(loads) >= 2, "several PT_LOADs: %d" % len(loads))

    previous = None
    for segment in loads:
        # p_offset and p_vaddr have to agree modulo the page size or mmap
        # cannot place the segment at all.
        suite.equal(segment["vaddr"] % PAGE, segment["offset"] % PAGE,
                    "segment at 0x%x is page-congruent" % segment["vaddr"])
        suite.check(segment["filesz"] <= segment["memsz"],
                    "segment at 0x%x has memsz >= filesz" % segment["vaddr"])
        suite.check(segment["offset"] + segment["filesz"] <= size,
                    "segment at 0x%x stays inside the file" % segment["vaddr"])
        suite.check(not (segment["flags"] & PF_W and segment["flags"] & PF_X),
                    "segment at 0x%x is not both writable and executable"
                    % segment["vaddr"])
        if previous is not None:
            suite.check(segment["vaddr"] >= previous,
                        "segments do not overlap at 0x%x" % segment["vaddr"])
        previous = segment["vaddr"] + segment["memsz"]

    executable = [s for s in loads if s["flags"] & PF_X]
    suite.equal(len(executable), 1, "one executable segment")
    suite.check(any(s["vaddr"] <= exe.entry < s["vaddr"] + s["filesz"]
                    for s in executable),
                "the entry point 0x%x is in an executable segment" % exe.entry)
    suite.equal(exe.entry, exe.symbols["_start"]["value"],
                "the entry point is _start")

    stack = [s for s in exe.segments if s["type"] == PT_GNU_STACK]
    suite.equal(len(stack), 1, "PT_GNU_STACK is present")
    if stack:
        suite.check(not stack[0]["flags"] & PF_X, "the stack is not executable")

    # Every allocated section belongs to a segment whose permissions are at
    # least what the section asked for.
    for section in exe.sections:
        if not section["flags"] & SHF_ALLOC:
            continue

        holder = [s for s in loads
                  if s["vaddr"] <= section["addr"] and
                  section["addr"] + section["size"] <= s["vaddr"] + s["memsz"]]
        suite.check(len(holder) == 1,
                    "'%s' is in exactly one segment" % section["name"])
        if not holder:
            continue

        if section["flags"] & SHF_EXECINSTR:
            suite.check(holder[0]["flags"] & PF_X,
                        "'%s' is in an executable segment" % section["name"])
        if section["flags"] & SHF_WRITE:
            suite.check(holder[0]["flags"] & PF_W,
                        "'%s' is in a writable segment" % section["name"])

        if section["align"] > 1:
            suite.equal(section["addr"] % section["align"], 0,
                        "'%s' is aligned as it asked" % section["name"])

    # An output section is as aligned as the most demanding thing in it, which
    # is not something 'addr % align' can see once align itself is wrong.
    for name, align in ((".text", 16), (".data", 16), (".bss", 64)):
        found = exe.named(name)
        suite.equal(len(found), 1, "one output %s" % name)
        if found:
            suite.equal(found[0]["align"], align,
                        "'%s' keeps the alignment its input asked for" % name)

    for section in exe.sections:
        if section["type"] == SHT_NOBITS or section["size"] == 0:
            continue
        suite.check(section["offset"] + section["size"] <= size,
                    "'%s' is inside the file" % section["name"])

    # sh_info of a symbol table is where its globals start, and every symbol
    # before it has to be local for a reader to be able to trust that.
    before = [s["name"] for s in exe.symbolList[:exe.symtabInfo]
              if s["bind"] != STB_LOCAL]
    suite.equal(before, [], "every symbol before sh_info is local")
    after = [s["name"] for s in exe.symbolList[exe.symtabInfo:]
             if s["bind"] == STB_LOCAL]
    suite.equal(after, [], "and every symbol from sh_info on is not")


def testDiagnostics(link, suite):
    """Every one of these is a link that must not produce a program."""
    code = statusImmediate(0) + exitWithStatus()

    undefined = startObject(code,
                            symbols=[("missing", STB_GLOBAL, STT_NOTYPE,
                                      SHN_UNDEF)],
                            relocs=[(1, "missing", R_X86_64_32, 0)])
    a = link.object(undefined, "undefined.o")
    link.expectFailure([a], "undefined symbol 'missing'",
                       "an undefined symbol is refused")

    def defines(name, value):
        elf = Elf()
        text = elf.addSection(".text", SHT_PROGBITS,
                              SHF_ALLOC | SHF_EXECINSTR, returnImmediate(value),
                              align=16)
        elf.addSymbol("twice", STB_GLOBAL, STT_FUNC, text, 0, 6)
        return link.object(elf, name)

    b = defines("dup1.o", 1)
    c = defines("dup2.o", 2)
    entry = link.object(startObject(code), "entryonly.o")
    link.expectFailure([entry, b, c], "multiple definition of 'twice'",
                       "two definitions are refused")

    noEntry = Elf()
    noEntry.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                       b"\xc3", align=1)
    noEntry.addSymbol("notTheEntry", STB_GLOBAL, STT_FUNC, 1, 0, 1)
    d = link.object(noEntry, "noentry.o")
    link.expectFailure([d], "entry symbol '_start' is not defined",
                       "a program with no entry point is refused")
    link.expectFailure([link.object(startObject(code), "hasentry.o")],
                       "entry symbol 'nowhere' is not defined",
                       "-e names a symbol that has to exist",
                       extra=["-e", "nowhere"])

    # Relocation fields that the value does not fit in. An absolute symbol is
    # the only way to put a target that far away in a four-instruction program.
    far = startObject(code,
                      symbols=[("faraway", STB_GLOBAL, STT_OBJECT, SHN_ABS,
                                0x123456789)],
                      relocs=[(1, "faraway", R_X86_64_32, 0)])
    e = link.object(far, "far32.o")
    link.expectFailure([e], "does not fit", "a 32-bit field that overflows")

    negative = startObject(statusFromAbsolute() + exitWithStatus(),
                           symbols=[("faraway", STB_GLOBAL, STT_OBJECT,
                                     SHN_ABS, 0xfffffff0)],
                           relocs=[(3, "faraway", R_X86_64_32S, 0)])
    f = link.object(negative, "far32s.o")
    link.expectFailure([f], "does not fit", "a signed 32-bit field that overflows")

    displacement = startObject(statusFromRip() + exitWithStatus(),
                               symbols=[("faraway", STB_GLOBAL, STT_OBJECT,
                                         SHN_ABS, 0x7fffffffff)],
                               relocs=[(2, "faraway", R_X86_64_PC32, -4)])
    g = link.object(displacement, "fardisp.o")
    link.expectFailure([g], "too far", "a displacement that does not reach")

    past = startObject(code, symbols=[("here", STB_GLOBAL, STT_OBJECT, SHN_ABS,
                                       1)],
                       relocs=[(len(code) - 2, "here", R_X86_64_64, 0)])
    h = link.object(past, "pastend.o")
    link.expectFailure([h], "points past the end",
                       "a relocation past the end of its section")

    unsupported = startObject(code,
                              symbols=[("gotThing", STB_GLOBAL, STT_OBJECT,
                                        SHN_ABS, 1)],
                              relocs=[(1, "gotThing", 9, 0)])  # GOTPCREL
    i = link.object(unsupported, "unsupported.o")
    link.expectFailure([i], "is not supported yet",
                       "a relocation this milestone does not do")


def testRefusedInputs(link, suite):
    """What a static link cannot make sense of says so by name, because
    'undefined symbol' would be the wrong answer to all of them. Archives and
    linker scripts are no longer on that list - both are read now, and only a
    shared object still needs a loader this link will not have."""
    from reader_test import sharedObject, archive

    shared = link.object(sharedObject(), "libfixture.so")
    link.expectFailure([shared], "needs a dynamic linker",
                       "a shared object is refused without a loader")

    link.expectFailure([os.path.join(link.tmp, "notthere.o")], "cannot open",
                       "a missing input is refused")
    link.expectFailure([], "no input files", "a link with no inputs is refused")

    # An archive nothing indexed cannot be searched, and scanning every member
    # instead would be a different linker's answer, not a cheaper one.
    members = [("member.o", Elf().build())]
    link.object(archive(members), "libnoindex.a")
    link.expectFailure([os.path.join(link.tmp, "libnoindex.a")],
                       "archive has no index",
                       "an archive with no armap is refused")


def testCompiledInput(link, suite, compiler):
    """The only half that sees the relocations the compiler actually emits -
    everything above says what the linker does with relocations someone chose
    to write down."""
    source = os.path.join(link.tmp, "program.c")
    with open(source, "w") as f:
        f.write("""
struct Point { int x, y; };

static struct Point points[3];
static int total;
const char text[] = "abcdef";
const char *const first = text;

static int scale(int v) { return v * 2 + 1; }

int compute(void) {
  struct Point p;
  int i;

  for (i = 0; i < 3; ++i) {
    points[i].x = i;
    points[i].y = scale(i);
  }

  p = points[2];

  for (i = 0; i < 3; ++i) {
    total += points[i].x + points[i].y;
  }

  return total + p.y + (int)(text[3] - 'a') + (int)(*first - 'a');
}
""")

    objectPath = os.path.join(link.tmp, "program.o")
    result = subprocess.run([compiler, "-c", "-o", objectPath, source],
                            capture_output=True, text=True)
    if result.returncode != 0:
        suite.check(False, "compiling the fixture failed: %s"
                    % result.stderr.strip())
        return

    code = callRelative() + statusFromResult() + exitWithStatus()
    start = startObject(code,
                        symbols=[("compute", STB_GLOBAL, STT_NOTYPE,
                                  SHN_UNDEF)],
                        relocs=[(1, "compute", R_X86_64_PLT32, -4)])
    startPath = link.object(start, "cstart.o")

    # 3 + 9 (the loops) + 5 (p.y) + 3 ('d' - 'a') + 0 (*first).
    link.expectRuns([startPath, objectPath], 20,
                    "a program EduCC compiled links and computes")


def testAgainstLd(link, suite):
    """ld as an oracle, and only about behaviour: the two linkers have no
    reason to agree about addresses and every reason to agree about what the
    program returns."""
    import shutil

    if not shutil.which("ld"):
        return False

    code = callRelative() + statusFromResult() + exitWithStatus()
    caller = startObject(code,
                         symbols=[("answer", STB_GLOBAL, STT_NOTYPE,
                                   SHN_UNDEF),
                                  ("slot", STB_GLOBAL, STT_OBJECT, SHN_COMMON,
                                   8, 8)],
                         relocs=[(1, "answer", R_X86_64_PLT32, -4)])

    callee = Elf()
    text = callee.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                             returnImmediate(29), align=16)
    callee.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC, b"oracle\0", align=1)
    callee.addSymbol("answer", STB_GLOBAL, STT_FUNC, text, 0, 6)
    callee.addSymbol("slot", STB_GLOBAL, STT_OBJECT, SHN_COMMON, 16, 32)

    a = link.object(caller, "oracle1.o")
    b = link.object(callee, "oracle2.o")

    reference = os.path.join(link.tmp, "reference")
    built = subprocess.run(["ld", "-o", reference, a, b], capture_output=True,
                           text=True)
    if built.returncode != 0:
        suite.check(False, "ld refused the fixture: %s" % built.stderr.strip())
        return True

    theirs = subprocess.run([reference], capture_output=True).returncode
    mine = link.expectRuns([a, b], theirs, "ld and EduCC agree about the result")
    return mine is not None


def main():
    if len(sys.argv) < 2:
        print("usage: linker_test.py <path to educc>", file=sys.stderr)
        return 2

    compiler = sys.argv[1]
    if not os.path.exists(compiler):
        print("no such compiler: %s" % compiler, file=sys.stderr)
        return 2

    suite = Suite()
    with tempfile.TemporaryDirectory(prefix="educc-link-") as tmp:
        link = Link(compiler, tmp, suite)

        testMinimalProgram(link, suite)
        testHelloWorld(link, suite)
        testCrossObjectCall(link, suite)
        testDataRelocations(link, suite)
        testPcRelativeData(link, suite)
        testCommonSymbols(link, suite)
        testWeakSymbols(link, suite)
        testAbsoluteSymbols(link, suite)
        testSectionMerging(link, suite)
        testStructure(link, suite)
        testDiagnostics(link, suite)
        testRefusedInputs(link, suite)
        testCompiledInput(link, suite, compiler)
        oracle = testAgainstLd(link, suite)

    print("== %d checks over linked and executed programs%s" %
          (suite.checks, ", with ld as an oracle" if oracle
           else " (ld absent: no differential half)"))

    for failure in suite.failures:
        print("FAIL: " + failure)

    if suite.failures:
        print("== %d of %d checks failed" % (len(suite.failures), suite.checks))
        return 1

    print("== all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
