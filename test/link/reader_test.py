#!/usr/bin/env python3
"""Unit tests for the linker's ELF reader (src/link/elfread.c).

    python3 test/link/reader_test.py build/bin/main

Two halves, and they answer different questions.

The first builds its inputs byte by byte - an ELF64 object, a shared object, a
GNU archive - and asserts what '-Xlink-read' reports about each. Synthesizing
them rather than checking in binaries is what makes this suite say anything at
all: the host's crt1.o is whatever the distro ships this month, so a golden
dump of one pins the distro and not the reader. It is also the only way to
reach the cases the host has no example of - a COMMON symbol, a /SYM64/ armap,
the e_shnum escape through section 0 - and every malformed input, which has to
produce a diagnostic rather than a signal.

The second cross-checks the reader against binutils over whatever real inputs
the machine has. No baselines, so nothing here rots when glibc moves; it skips
when nm/ar are missing rather than failing.

Every assertion here was verified by breaking the reader on purpose and
watching it fail: 21 mutations, 20 caught. The survivor is readArchive's own
'!<arch>' check, which nothing can reach today because classifyImage dispatches
on that same magic first - it is there for the linker calling readArchive
directly, and it will become reachable then.
"""
import os
import struct
import subprocess
import sys
import tempfile

# ELF constants. Spelled out rather than imported so this file states what it
# is building and does not depend on the host's headers agreeing.
ET_REL, ET_DYN, ET_EXEC = 1, 3, 2
EM_X86_64 = 62
SHT_NULL, SHT_PROGBITS, SHT_SYMTAB, SHT_STRTAB = 0, 1, 2, 3
SHT_RELA, SHT_DYNAMIC, SHT_NOBITS, SHT_REL, SHT_DYNSYM = 4, 6, 8, 9, 11
SHF_WRITE, SHF_ALLOC, SHF_EXECINSTR = 0x1, 0x2, 0x4
STB_LOCAL, STB_GLOBAL, STB_WEAK = 0, 1, 2
STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_SECTION = 0, 1, 2, 3
SHN_UNDEF, SHN_ABS, SHN_COMMON, SHN_XINDEX = 0, 0xfff1, 0xfff2, 0xffff
DT_NULL, DT_SONAME, DT_STRTAB, DT_STRSZ = 0, 14, 5, 10
R_X86_64_PC32 = 2

EHDR_SIZE, SHDR_SIZE, SYM_SIZE, RELA_SIZE, DYN_SIZE = 64, 64, 24, 24, 16


class StringTable:
    def __init__(self):
        self.blob = b"\0"

    def add(self, name):
        if not name:
            return 0
        offset = len(self.blob)
        self.blob += name.encode() + b"\0"
        return offset


class Elf:
    """A hand-built ELF64 object. Sections are numbered in the order added,
    starting at 1; section 0 is the null section this writes itself."""

    def __init__(self, etype=ET_REL, machine=EM_X86_64, elfClass=2, endian=1):
        self.etype = etype
        self.machine = machine
        self.elfClass = elfClass
        self.endian = endian
        self.sections = []            # (name, type, flags, data, align, link, info, entsize)
        self.symbols = []             # (name, bind, type, shndx, value, size)
        self.shnumOverride = None     # e_shnum, when not len(sections) + 1
        self.shstrndxOverride = None
        self.shoffOverride = None
        self.section0 = None          # (sh_size, sh_link) for the e_shnum escape
        self.symtabInfoOverride = None
        self.symtabLinkOverride = None
        self.truncateTo = None
        self.badNameOffset = None     # a symbol whose st_name points past .strtab
        self.oversizeSection = None   # a section claiming more bytes than exist
        self.unterminateStrtab = False  # drop .strtab's final NUL
        self.strtabType = SHT_STRTAB  # SHT_NOBITS makes it contentless

    def addSection(self, name, type, flags=0, data=b"", align=1, link=0,
                   info=0, entsize=0):
        self.sections.append([name, type, flags, data, align, link, info, entsize])
        return len(self.sections)  # its index, section 0 being the null one

    def addSymbol(self, name, bind, type, shndx, value=0, size=0):
        self.symbols.append((name, bind, type, shndx, value, size))

    def _ordered(self):
        # Locals first, which is the ordering .symtab requires and what sh_info
        # is the boundary of.
        return ([s for s in self.symbols if s[1] == STB_LOCAL] +
                [s for s in self.symbols if s[1] != STB_LOCAL])

    def symbolIndex(self, name):
        """What a relocation has to put in r_info, which is a position in the
        table rather than anything the caller chose."""
        for index, symbol in enumerate(self._ordered()):
            if symbol[0] == name:
                return index + 1  # the null symbol is 0
        raise KeyError(name)

    def _buildSymtab(self, strtab):
        ordered = self._ordered()
        firstGlobal = sum(1 for s in ordered if s[1] == STB_LOCAL) + 1

        blob = struct.pack("<IBBHQQ", 0, 0, 0, SHN_UNDEF, 0, 0)
        for index, (name, bind, type, shndx, value, size) in enumerate(ordered):
            nameOffset = strtab.add(name)
            if self.badNameOffset is not None and name == self.badNameOffset:
                nameOffset = 0x7fffffff
            blob += struct.pack("<IBBHQQ", nameOffset, (bind << 4) | type, 0,
                                shndx, value, size)
        return blob, firstGlobal

    def build(self):
        strtab = StringTable()
        symtabBlob, firstGlobal = self._buildSymtab(strtab)

        sections = [list(s) for s in self.sections]
        if self.symbols:
            symtabIndex = len(sections) + 1
            strtabIndex = symtabIndex + 1
            sections.append([".symtab", SHT_SYMTAB, 0, symtabBlob, 8,
                             strtabIndex, firstGlobal, SYM_SIZE])
            sections.append([".strtab", self.strtabType, 0, None, 1, 0, 0, 0])
            if self.symtabInfoOverride is not None:
                sections[-2][6] = self.symtabInfoOverride
            if self.symtabLinkOverride is not None:
                sections[-2][5] = self.symtabLinkOverride

        shstrtabIndex = len(sections) + 1
        sections.append([".shstrtab", SHT_STRTAB, 0, None, 1, 0, 0, 0])

        shstrtab = StringTable()
        nameOffsets = [shstrtab.add(s[0]) for s in sections]

        # .strtab's contents are only final once every symbol name is in it.
        for s in sections:
            if s[0] == ".strtab":
                s[3] = strtab.blob[:-1] if self.unterminateStrtab else strtab.blob
        sections[-1][3] = shstrtab.blob

        # Lay the section contents out after the header, then the section
        # header table after those.
        offset = EHDR_SIZE
        blob = b""
        placed = []
        for name, type, flags, data, align, link, info, entsize in sections:
            data = data or b""
            if align > 1 and offset % align:
                pad = align - (offset % align)
                blob += b"\0" * pad
                offset += pad
            fileOffset = offset
            size = len(data)
            if type == SHT_NOBITS:
                # Occupies no file space; sh_size is the memory size.
                size = len(data)
                data = b""
            else:
                blob += data
                offset += len(data)
            placed.append((name, type, flags, fileOffset, size, align, link,
                           info, entsize))

        shoff = offset
        shnum = len(sections) + 1

        headers = struct.pack("<IIQQQQIIQQ", 0, SHT_NULL, 0, 0, 0,
                              self.section0[0] if self.section0 else 0,
                              self.section0[1] if self.section0 else 0,
                              0, 0, 0)
        for i, (name, type, flags, fileOffset, size, align, link, info,
                entsize) in enumerate(placed):
            if name == self.oversizeSection:
                size = 0x7fffffff
            headers += struct.pack("<IIQQQQIIQQ", nameOffsets[i], type, flags,
                                   0, fileOffset, size, link, info, align,
                                   entsize)

        ident = bytes([0x7f]) + b"ELF" + bytes([self.elfClass, self.endian, 1,
                                                0, 0]) + b"\0" * 7
        header = struct.pack("<16sHHIQQQIHHHHHH", ident, self.etype,
                             self.machine, 1, 0, 0,
                             shoff if self.shoffOverride is None else self.shoffOverride,
                             0, EHDR_SIZE, 0, 0, SHDR_SIZE,
                             shnum if self.shnumOverride is None else self.shnumOverride,
                             shstrtabIndex if self.shstrndxOverride is None
                             else self.shstrndxOverride)

        image = header + blob + headers
        if self.truncateTo is not None:
            image = image[:self.truncateTo]
        return image


def archive(members, index=None, wide=False, longNameThreshold=15,
            unterminatedNames=False):
    """A GNU archive. `members` is a list of (name, bytes); `index` a list of
    (symbol, memberIndex) to put in the armap, or None for no armap."""
    magic = b"!<arch>\n"

    def header(name, size):
        return ("%-16s%-12s%-6s%-6s%-8s%-10d`\n" %
                (name, "0", "0", "0", "644", size)).encode()

    longNames = b""
    names = []
    for name, _ in members:
        if len(name) > longNameThreshold:
            names.append("/%d" % len(longNames))
            longNames += name.encode() + b"/\n"
        else:
            names.append(name + "/")

    body = b""
    if longNames:
        body += header("//", len(longNames)) + longNames
        if len(longNames) % 2:
            body += b"\n"

    # The armap has to be written before the members but names their offsets,
    # so its size is computed first and the offsets derived from it.
    armap = b""
    if index is not None:
        entry = 8 if wide else 4
        symbolNames = b"".join(s.encode() + b"\0" for s, _ in index)
        if unterminatedNames:
            symbolNames = symbolNames[:-1]
        armapSize = entry + entry * len(index) + len(symbolNames)
        armapName = "/SYM64/" if wide else "/"
        armapTotal = len(header(armapName, armapSize)) + armapSize
        if armapSize % 2:
            armapTotal += 1

        base = len(magic) + len(body) + armapTotal
        offsets = {}
        cursor = base
        for i, (name, data) in enumerate(members):
            offsets[i] = cursor
            cursor += len(header(names[i], len(data))) + len(data)
            if len(data) % 2:
                cursor += 1

        def pack(value):
            return (struct.pack(">Q", value) if wide else struct.pack(">I", value))

        armap = pack(len(index))
        for _, member in index:
            armap += pack(offsets[member])
        armap += symbolNames
        body += header(armapName, armapSize) + armap
        if armapSize % 2:
            body += b"\0"

    for i, (name, data) in enumerate(members):
        body += header(names[i], len(data)) + data
        if len(data) % 2:
            body += b"\n"

    return magic + body


class Reader:
    """Runs '-Xlink-read' and parses the dump into something assertable."""

    def __init__(self, compiler, tmp, suite):
        self.compiler = compiler
        self.tmp = tmp
        self.suite = suite
        self.counter = 0

    def run(self, image, name=None):
        self.counter += 1
        path = os.path.join(self.tmp, name or ("input%d.bin" % self.counter))
        with open(path, "wb") as f:
            f.write(image)

        r = subprocess.run([self.compiler, "-Xlink-read", path],
                           capture_output=True, text=True)
        dump = Dump(path, r.returncode, r.stdout, r.stderr)
        self.suite.check(not dump.malformed,
                         "%s: dump lines the reader could not have meant: %r"
                         % (name or path, dump.malformed[:3]))
        # A signal is never an answer, however the input got there.
        self.suite.check(r.returncode >= 0,
                         "%s: the reader died on signal %d" % (name or path,
                                                               -r.returncode))
        return dump


class Dump:
    def __init__(self, path, code, out, err):
        self.path = path
        self.code = code
        self.out = out
        self.err = err
        self.headline = out.splitlines()[0] if out else ""
        self.sections = {}
        self.symbols = {}
        self.members = []
        self.defines = {}

        # A line that does not have the shape its keyword promises is kept as
        # the malformed thing it is rather than skipped or crashed on: a reader
        # bug that truncates a line should read as a failed assertion, not as a
        # traceback here and not as a silently absent entry.
        self.malformed = []

        for line in out.splitlines():
            parts = line.split()
            if not parts:
                continue
            keyword = parts[0]
            if keyword not in ("section", "symbol", "member", "defines"):
                continue

            # Validated by shape, not just by length: a dropped column shifts
            # every field after it, and a length check alone would then read
            # one field as another and raise rather than report.
            shape = {
                "section": (10, {2: "type", 4: "flags", 6: "size", 8: "align"}),
                "symbol": (4, {}),
                "member": (4, {2: "size"}),
                "defines": (4, {2: "in"}),
            }
            minimum, keywords = shape[keyword]
            if len(parts) < minimum or any(parts[i] != word
                                           for i, word in keywords.items()):
                self.malformed.append(line)
                continue

            if keyword == "section":
                entry = {
                    "type": int(parts[3]),
                    "flags": int(parts[5], 16),
                    "size": int(parts[7]),
                    "align": int(parts[9]),
                    "relocs": 0,
                    "contents": True,
                }
                # Trailing tokens are optional and order-independent: a
                # 'relocs N' pair and a 'nocontents' flag.
                rest = parts[10:]
                while rest:
                    if rest[0] == "relocs" and len(rest) >= 2:
                        entry["relocs"] = int(rest[1])
                        rest = rest[2:]
                    elif rest[0] == "nocontents":
                        entry["contents"] = False
                        rest = rest[1:]
                    else:
                        self.malformed.append(line)
                        break
                self.sections[parts[1]] = entry
            elif keyword == "symbol":
                self.symbols[parts[1]] = {
                    "bind": parts[2],
                    "type": parts[3],
                    "where": " ".join(parts[4:]),
                }
            elif keyword == "member":
                self.members.append(parts[1])
            else:
                self.defines[parts[1]] = parts[3]


class Suite:
    def __init__(self):
        self.failures = []
        self.checks = 0

    def check(self, condition, message):
        self.checks += 1
        if not condition:
            self.failures.append(message)

    def equal(self, actual, expected, message):
        self.check(actual == expected,
                   "%s: expected %r, got %r" % (message, expected, actual))

    def contains(self, haystack, needle, message):
        self.check(needle in haystack,
                   "%s: %r not found in %r" % (message, needle, haystack))


def simpleObject():
    """One object carrying a section of each kind that matters, relocations
    against .text, and a symbol of every binding and type."""
    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          b"\x90" * 32, align=16)
    data = elf.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                          b"\x01" * 8, align=8)
    elf.addSection(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, b"\0" * 64,
                   align=32)
    elf.addSection(".rodata", SHT_PROGBITS, SHF_ALLOC, b"hello\0", align=1)

    relocs = b"".join(struct.pack("<QQq", off, (2 << 32) | R_X86_64_PC32, -4)
                      for off in (4, 20))
    elf.addSection(".rela.text", SHT_RELA, 0x40, relocs, align=8, link=0,
                   info=text, entsize=RELA_SIZE)

    elf.addSymbol("localFunc", STB_LOCAL, STT_FUNC, text, 0, 16)
    elf.addSymbol("globalFunc", STB_GLOBAL, STT_FUNC, text, 16, 16)
    elf.addSymbol("weakData", STB_WEAK, STT_OBJECT, data, 0, 8)
    elf.addSymbol("undefinedThing", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)
    elf.addSymbol("absoluteThing", STB_GLOBAL, STT_OBJECT, SHN_ABS, 0x1234)
    elf.addSymbol("commonThing", STB_GLOBAL, STT_OBJECT, SHN_COMMON, 16, 40)
    return elf


def sharedObject(soname="libfixture.so.1"):
    elf = Elf(etype=ET_DYN)
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          b"\x90" * 16, align=16)

    # A shared object's names come from .dynstr, which DT_STRTAB names and the
    # dynamic section links to.
    dynstr = b"\0" + (soname.encode() + b"\0" if soname else b"")
    dynstrIndex = elf.addSection(".dynstr", SHT_STRTAB, SHF_ALLOC, dynstr)

    dynsym = struct.pack("<IBBHQQ", 0, 0, 0, SHN_UNDEF, 0, 0)
    dynsym += struct.pack("<IBBHQQ", 0, (STB_GLOBAL << 4) | STT_FUNC, 0, text,
                          0, 16)
    elf.addSection(".dynsym", SHT_DYNSYM, SHF_ALLOC, dynsym, align=8,
                   link=dynstrIndex, info=1, entsize=SYM_SIZE)

    entries = b""
    if soname:
        entries += struct.pack("<qQ", DT_SONAME, 1)
    entries += struct.pack("<qQ", DT_NULL, 0)
    elf.addSection(".dynamic", SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE, entries,
                   align=8, link=dynstrIndex, entsize=DYN_SIZE)
    return elf


def testObjects(reader, suite):
    dump = reader.run(simpleObject().build(), "simple.o")
    suite.equal(dump.code, 0, "a well-formed object is read")
    suite.contains(dump.headline, "relocatable", "an ET_REL is called relocatable")

    suite.equal(dump.sections[".text"]["flags"], SHF_ALLOC | SHF_EXECINSTR,
                ".text flags")
    suite.equal(dump.sections[".text"]["align"], 16, ".text alignment")
    suite.equal(dump.sections[".text"]["size"], 32, ".text size")
    suite.equal(dump.sections[".bss"]["type"], SHT_NOBITS, ".bss stays NOBITS")
    suite.equal(dump.sections[".bss"]["size"], 64,
                ".bss keeps its memory size with no file bytes")
    # sh_offset of a NOBITS section names nothing. Reading bytes there would be
    # reading whichever section the file happens to hold at that offset, and
    # the result would be a .bss full of someone else's data.
    suite.equal(dump.sections[".bss"]["contents"], False,
                "a NOBITS section has no contents to point at")
    suite.equal(dump.sections[".text"]["contents"], True,
                "and a PROGBITS section does")

    # The point of the reader attaching relocations to what they relocate: the
    # count belongs to .text, and .rela.text is not relocated by anything.
    suite.equal(dump.sections[".text"]["relocs"], 2,
                "relocations attach to the section they apply to")
    suite.equal(dump.sections[".rela.text"]["relocs"], 0,
                "a relocation section is not itself relocated")

    suite.equal(dump.symbols["localFunc"]["bind"], "LOCAL", "local binding")
    suite.equal(dump.symbols["globalFunc"]["bind"], "GLOBAL", "global binding")
    suite.equal(dump.symbols["weakData"]["bind"], "WEAK", "weak binding")
    suite.equal(dump.symbols["weakData"]["type"], "OBJECT", "object type")
    suite.equal(dump.symbols["undefinedThing"]["where"], "undefined",
                "an undefined symbol")
    suite.equal(dump.symbols["absoluteThing"]["where"], "absolute 0x1234",
                "an absolute symbol keeps its value")
    suite.equal(dump.symbols["commonThing"]["where"], "common size 40",
                "a COMMON symbol reports the size it needs, not its value")
    suite.contains(dump.headline, "first global 2",
                   "sh_info is where the locals stop")

    # An object with no symbol table at all is legal - crtn.o is nearly one -
    # and must read rather than be refused.
    bare = Elf()
    bare.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, b"\xc3")
    dump = reader.run(bare.build(), "bare.o")
    suite.equal(dump.code, 0, "an object with no symbols is read")
    suite.contains(dump.headline, "symbols 0", "and reports no symbols")

    # e_shnum and e_shstrndx escape through section 0 when they do not fit in
    # the header's 16 bits. Reached here with a small count, because the escape
    # is a code path and not a size.
    escape = simpleObject()
    built = escape.build()
    sectionCount = struct.unpack_from("<H", built, 60)[0]
    shstrndx = struct.unpack_from("<H", built, 62)[0]
    escape.shnumOverride = 0
    escape.shstrndxOverride = SHN_XINDEX
    escape.section0 = (sectionCount, shstrndx)
    dump = reader.run(escape.build(), "escape.o")
    suite.equal(dump.code, 0, "the e_shnum escape through section 0 is read")
    suite.check(".text" in dump.sections,
                "section names still resolve through an SHN_XINDEX shstrndx")

    # A name offset past the end of its string table is a corrupt file that
    # must not be read out of bounds; the symbol loses its name and the rest of
    # the object survives.
    named = simpleObject()
    named.badNameOffset = "globalFunc"
    dump = reader.run(named.build(), "badname.o")
    suite.equal(dump.code, 0, "an out-of-range symbol name does not abort")
    suite.check("globalFunc" not in dump.symbols,
                "an out-of-range name reads as empty rather than as memory")
    suite.check("localFunc" in dump.symbols,
                "and the other symbols are unaffected")

    # A string table whose last byte is not NUL cannot terminate the name that
    # ends at it, so no name in it can be trusted - reading one would run off
    # the end of the table and into the next section.
    ragged = simpleObject()
    ragged.unterminateStrtab = True
    dump = reader.run(ragged.build(), "raggedstr.o")
    suite.equal(dump.code, 0, "an unterminated string table does not abort")
    suite.equal(dump.symbols, {},
                "no name is read out of a string table that cannot end one")


def testSharedObjects(reader, suite):
    dump = reader.run(sharedObject().build(), "libfixture.so")
    suite.equal(dump.code, 0, "a shared object is read")
    suite.contains(dump.headline, "soname libfixture.so.1",
                   "DT_SONAME is what a DT_NEEDED would record")

    # Without DT_SONAME the filename stands in - which is why the reader keeps
    # the two apart rather than assuming they match.
    dump = reader.run(sharedObject(soname=None).build(), "libnoname.so.7")
    suite.equal(dump.code, 0, "a shared object with no SONAME is read")
    suite.contains(dump.headline, "soname libnoname.so.7",
                   "the basename stands in for a missing SONAME")


def testArchives(reader, suite):
    members = [("short.o", simpleObject().build()),
               ("a_considerably_longer_name.o", simpleObject().build())]
    index = [("globalFunc", 0), ("otherThing", 1)]

    dump = reader.run(archive(members, index), "libnames.a")
    suite.equal(dump.code, 0, "an archive is read")
    suite.equal(dump.members, ["short.o", "a_considerably_longer_name.o"],
                "short and long member names both resolve")
    suite.contains(dump.headline, "indexed", "the armap is found")
    suite.equal(dump.defines.get("globalFunc"), "short.o",
                "the armap maps a symbol to its member")
    suite.equal(dump.defines.get("otherThing"), "a_considerably_longer_name.o",
                "including one in a long-named member")

    # A member of odd length is padded to an even offset, and getting that
    # wrong desynchronizes every member after it.
    odd = [("odd.o", b"12345"), ("after.o", simpleObject().build())]
    dump = reader.run(archive(odd, index=[("globalFunc", 1)]), "libodd.a")
    suite.equal(dump.code, 0, "an archive with an odd-sized member is read")
    suite.equal(dump.members, ["odd.o", "after.o"],
                "an odd-sized member is padded, so the next one is still found")
    suite.equal(dump.defines.get("globalFunc"), "after.o",
                "and the armap still points at the right member")

    dump = reader.run(archive(members, index, wide=True), "libsym64.a")
    suite.equal(dump.code, 0, "a /SYM64/ archive is read")
    suite.equal(dump.defines.get("globalFunc"), "short.o",
                "a 64-bit armap resolves the same way")

    dump = reader.run(archive(members, index=None), "libnoindex.a")
    suite.equal(dump.code, 0, "an archive with no armap is read")
    suite.contains(dump.headline, "no index",
                   "an archive with no armap says so")
    suite.equal(dump.members, ["short.o", "a_considerably_longer_name.o"],
                "and its members are still listed")


def testMalformed(reader, suite):
    def expectError(image, name, fragment, description):
        dump = reader.run(image, name)
        suite.equal(dump.code, 1, description + " is rejected")
        suite.contains(dump.err, fragment, description + " says why")

    truncated = simpleObject()
    truncated.truncateTo = 200
    expectError(truncated.build(), "truncated.o", "out of bounds",
                "an object cut short of its section headers")

    past = simpleObject()
    past.shoffOverride = 0x7fffffff
    expectError(past.build(), "farshoff.o", "out of bounds",
                "a section header table past the end of the file")

    # A section claiming more bytes than the file holds. This is the check that
    # stands between the reader and every later stage copying section contents
    # out of whatever follows the mapping.
    oversize = simpleObject()
    oversize.oversizeSection = ".text"
    expectError(oversize.build(), "oversize.o", "past the end of the file",
                "a section extending past the end of the file")

    badLink = simpleObject()
    badLink.symtabLinkOverride = 99
    expectError(badLink.build(), "badlink.o", "does not exist",
                "a symbol table naming a string table that is not there")

    rel = Elf()
    text = rel.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          b"\x90" * 8)
    rel.addSection(".rel.text", SHT_REL, 0x40, b"\0" * 16, align=8, info=text,
                   entsize=16)
    expectError(rel.build(), "rel.o", "REL relocations",
                "an object using REL rather than RELA")

    ragged = Elf()
    text = ragged.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                             b"\x90" * 8)
    ragged.addSection(".rela.text", SHT_RELA, 0x40, b"\0" * 30, align=8,
                      info=text, entsize=RELA_SIZE)
    expectError(ragged.build(), "ragged.o", "whole number of entries",
                "a relocation section that is not a whole number of entries")

    orphan = Elf()
    orphan.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, b"\x90")
    orphan.addSection(".rela.text", SHT_RELA, 0x40, b"\0" * RELA_SIZE, align=8,
                      info=42, entsize=RELA_SIZE)
    expectError(orphan.build(), "orphan.o", "does not exist",
                "a relocation section naming a section that is not there")

    # A symbol table whose string table has no bytes in the file. Pathological
    # rather than seen in the wild, and the reason the reader asks rather than
    # subtracting a NULL pointer from the image to get an offset.
    contentless = simpleObject()
    contentless.strtabType = SHT_NOBITS
    expectError(contentless.build(), "contentless.o", "no contents",
                "a symbol table whose string table has no contents")

    members = [("m.o", simpleObject().build())]
    expectError(archive(members, index=[("globalFunc", 0)],
                        unterminatedNames=True),
                "libbadnames.a", "unterminated name",
                "an armap whose last symbol name is not terminated")

    noSections = simpleObject()
    noSections.shoffOverride = 0
    expectError(noSections.build(), "nosections.o", "no section header table",
                "an object with no section header table")

    wrongMachine = simpleObject()
    wrongMachine.machine = 40  # EM_ARM
    expectError(wrongMachine.build(), "arm.o", "not an x86-64 object",
                "an object for another architecture")

    elf32 = simpleObject()
    elf32.elfClass = 1
    expectError(elf32.build(), "elf32.o", "not a 64-bit object",
                "a 32-bit object")

    bigEndian = simpleObject()
    bigEndian.endian = 2
    expectError(bigEndian.build(), "be.o", "not little-endian",
                "a big-endian object")

    executable = simpleObject()
    executable.etype = ET_EXEC
    dump = reader.run(executable.build(), "exec.bin")
    suite.equal(dump.code, 1, "an executable is rejected as a link input")

    # Text is neither ELF nor an archive, and /usr/lib/libc.so is exactly this:
    # a GNU ld script reached by '-lc' on every link. Recognized, not refused.
    dump = reader.run(b"/* GNU ld script */\nGROUP ( /usr/lib/libc.so.6 )\n",
                      "libscript.so")
    suite.equal(dump.code, 0, "a linker script is not an error")
    suite.contains(dump.out, "linker script", "a linker script is recognized")


def hostInputs(compiler):
    """Real objects, archives and shared libraries on this machine. Whatever is
    present - the point is to be a second opinion, not a fixed list."""
    import glob

    candidates = ["/usr/lib/crt1.o", "/usr/lib/crti.o", "/usr/lib/crtn.o",
                  "/usr/lib/x86_64-linux-gnu/crt1.o",
                  "/usr/lib64/crt1.o"]
    candidates += glob.glob("/usr/lib/gcc/*/*/crtbegin.o")
    candidates += glob.glob("/usr/lib/gcc/*/*/libgcc.a")
    candidates += glob.glob("build*/libzydis.a")
    candidates += glob.glob("build/CMakeFiles/main.dir/src/*.o")[:5]
    return [c for c in candidates if os.path.exists(c)]


def testAgainstBinutils(compiler, suite):
    """Differential: nm and ar as a second opinion over real inputs. No golden
    files, so nothing here rots when the distro's crt1.o changes."""
    import shutil

    if not shutil.which("nm") or not shutil.which("ar"):
        return False

    inputs = hostInputs(compiler)
    if not inputs:
        return False

    for path in inputs:
        r = subprocess.run([compiler, "-Xlink-read", path],
                           capture_output=True, text=True)
        if r.returncode != 0:
            suite.check(False, "reading %s failed: %s" % (path, r.stderr.strip()))
            continue

        dump = Dump(path, r.returncode, r.stdout, r.stderr)

        if path.endswith(".a"):
            listed = subprocess.run(["ar", "t", path], capture_output=True,
                                    text=True)
            expected = [n for n in listed.stdout.split() if n]
            suite.equal(dump.members, expected, "ar t agrees for %s" % path)
            continue

        # nm's view of what the object defines and needs, which is the only
        # part of a symbol table that resolution will ever consult.
        defined = subprocess.run(["nm", "--defined-only", "-g", path],
                                 capture_output=True, text=True)
        undefined = subprocess.run(["nm", "-u", path], capture_output=True,
                                   text=True)
        expected = set()
        for line in (defined.stdout + undefined.stdout).splitlines():
            parts = line.split()
            if parts:
                expected.add(parts[-1])

        mine = set(name for name, info in dump.symbols.items()
                   if info["bind"] in ("GLOBAL", "WEAK"))
        suite.equal(mine, expected, "nm agrees about %s" % path)

    return True


def main():
    if len(sys.argv) < 2:
        print("usage: reader_test.py <path to educc>", file=sys.stderr)
        return 2

    compiler = sys.argv[1]
    if not os.path.exists(compiler):
        print("no such compiler: %s" % compiler, file=sys.stderr)
        return 2

    suite = Suite()
    with tempfile.TemporaryDirectory(prefix="educc-link-") as tmp:
        reader = Reader(compiler, tmp, suite)
        testObjects(reader, suite)
        testSharedObjects(reader, suite)
        testArchives(reader, suite)
        testMalformed(reader, suite)
        differential = testAgainstBinutils(compiler, suite)

    print("== %d checks over synthesized inputs%s" %
          (suite.checks, ", plus binutils as a second opinion" if differential
           else " (binutils absent: no differential half)"))

    for failure in suite.failures:
        print("FAIL: " + failure)

    if suite.failures:
        print("== %d of %d checks failed" % (len(suite.failures), suite.checks))
        return 1

    print("== all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
