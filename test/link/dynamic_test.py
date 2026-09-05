#!/usr/bin/env python3
"""Unit tests for the dynamic half of the linker (src/link/link.c).

    python3 test/link/dynamic_test.py build/bin/main

linker_test.py asks whether the linker can place bytes and patch them; this
asks whether what it writes is something ld.so will load. The difference is
that the oracle here is the system's own dynamic loader: a PLT stub whose
displacement is six bytes out, a .hash table that cannot find a name, a
PT_GNU_RELRO that covers a word ld.so has to write - none of those are a diff
against a golden file, they are a program that does not start.

The inputs are the host's real libc, because there is no way to synthesize a
useful one: a shared object that means anything has a .dynsym, a .hash, a
.dynamic and a loader willing to map it, which is a second linker's worth of
Python. What is synthesized is everything on this side of the boundary - the
objects, their machine code and their relocations - so that a fixture is four
instructions and an exit code rather than a compiler's opinion.

The structural half asserts what the loader is entitled to assume and never
checks: that PT_PHDR is inside a PT_LOAD, that every .rela.plt entry names a
slot in .got.plt past the three the loader reserved, that .got.plt is *not*
inside the region ld.so makes read-only, that every dynamic symbol can be
found through the hash table that indexes it.

Exits 2 (skip) when the host has no C runtime to link against.
"""
import os
import shutil
import struct
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from reader_test import (Elf, Suite, SHT_PROGBITS, SHT_NOBITS,
                         SHF_ALLOC, SHF_WRITE, SHF_EXECINSTR,
                         STB_GLOBAL, STT_FUNC, STT_NOTYPE, SHN_UNDEF)
from linker_test import (Link, Executable, addRela, objectImage, runProgram,
                         exitWithStatus, statusFromResult, callRelative,
                         loadAbsolutePointer, R_X86_64_PLT32, R_X86_64_32S,
                         PT_LOAD, PF_W, PF_X, PAGE, PHDR_SIZE)

PT_PHDR, PT_INTERP, PT_DYNAMIC = 6, 3, 2
PT_GNU_RELRO = 0x6474e552

ET_EXEC, ET_DYN = 2, 3

R_X86_64_64, R_X86_64_GLOB_DAT, R_X86_64_JUMP_SLOT = 1, 6, 7
R_X86_64_COPY, R_X86_64_RELATIVE = 5, 8

DT_NEEDED, DT_HASH, DT_STRTAB, DT_SYMTAB = 1, 4, 5, 6
DT_STRSZ, DT_SYMENT, DT_PLTGOT, DT_PLTRELSZ = 10, 11, 3, 2
DT_JMPREL, DT_RELA, DT_RELASZ, DT_DEBUG, DT_NULL = 23, 7, 8, 21, 0
DT_SONAME, DT_RUNPATH, DT_RELACOUNT = 14, 29, 0x6ffffff9

INTERPRETER = "/lib64/ld-linux-x86-64.so.2"


def elfHash(name):
    h = 0
    for byte in name.encode():
        h = ((h << 4) + byte) & 0xffffffff
        high = h & 0xf0000000
        if high:
            h ^= high >> 24
        h &= ~high & 0xffffffff
    return h


# Where a library is on this machine. The driver has this list built in, but
# '-Xlink-dynamic' searches only what it is given, so a hardcoded '/usr/lib'
# here is a suite that finds libc on one distribution and not the next.
LIBRARY_DIRECTORIES = ("/usr/lib/x86_64-linux-gnu", "/usr/lib64", "/lib64",
                       "/usr/lib", "/lib")


def libraryPath():
    return ["-L" + d for d in LIBRARY_DIRECTORIES if os.path.isdir(d)]


def crtDirectory():
    for candidate in LIBRARY_DIRECTORIES:
        if os.path.exists(os.path.join(candidate, "crt1.o")):
            return candidate
    return None


# ------------------------------------------------------------- machine code

def callWithArgument(value):
    """mov $imm32,%edi; call <rel32> - the relocation is at +6."""
    return b"\xbf" + struct.pack("<i", value) + callRelative()


def statusIfNotNull():
    """test %rax,%rax; setne %dil; movzbl %dil,%edi"""
    return b"\x48\x85\xc0" + b"\x40\x0f\x95\xc7" + b"\x40\x0f\xb6\xff"


# ----------------------------------------------------------------- reading

class Dynamic(Executable):
    """An Executable that also reads the tables a loader would."""

    def __init__(self, path):
        Executable.__init__(self, path)

        self.dynamic = []
        self.dynsyms = []
        self.dynstr = b""

        section = self.section(".dynamic")
        if section is None:
            return

        blob = self.blob[section["offset"]:section["offset"] + section["size"]]
        for i in range(0, len(blob), 16):
            tag, value = struct.unpack("<qQ", blob[i:i + 16])
            self.dynamic.append((tag, value))
            if tag == DT_NULL:
                break

        strtab = self.section(".dynstr")
        self.dynstr = self.blob[strtab["offset"]:
                                strtab["offset"] + strtab["size"]]

        symtab = self.section(".dynsym")
        for i in range(symtab["size"] // 24):
            o = symtab["offset"] + i * 24
            name, info, other, shndx, value, size = struct.unpack(
                "<IBBHQQ", self.blob[o:o + 24])
            self.dynsyms.append(dict(name=self.string(name), bind=info >> 4,
                                     type=info & 0xf, shndx=shndx,
                                     value=value, size=size, other=other))

    def section(self, name):
        found = self.named(name)
        return found[0] if found else None

    def string(self, offset):
        end = self.dynstr.find(b"\0", offset)
        return self.dynstr[offset:end].decode()

    def bytesAt(self, address, length):
        """Read from the image by virtual address, which is how everything a
        loader consults is addressed."""
        for segment in self.loads():
            start = segment["vaddr"]
            if start <= address < start + segment["filesz"]:
                at = segment["offset"] + (address - start)
                return self.blob[at:at + length]
        return None

    def relocations(self, name):
        section = self.section(name)
        if section is None:
            return []

        blob = self.blob[section["offset"]:section["offset"] + section["size"]]
        entries = []
        for i in range(0, len(blob), 24):
            offset, info, addend = struct.unpack("<QQq", blob[i:i + 24])
            entries.append((offset, info >> 32, info & 0xffffffff, addend))
        return entries

    def tag(self, wanted):
        for tag, value in self.dynamic:
            if tag == wanted:
                return value
        return None

    def tags(self, wanted):
        return [value for tag, value in self.dynamic if tag == wanted]

    def segment(self, type):
        found = [s for s in self.segments if s["type"] == type]
        return found[0] if found else None

    def hashLookup(self, name):
        """The lookup ld.so does, so that a table it cannot search shows up
        here rather than as a program that will not start."""
        section = self.section(".hash")
        blob = self.blob[section["offset"]:section["offset"] + section["size"]]
        buckets, chains = struct.unpack("<II", blob[:8])

        words = struct.unpack("<%dI" % (len(blob) // 4), blob)
        index = words[2 + elfHash(name) % buckets]

        while index != 0:
            if index >= len(self.dynsyms):
                return None
            if self.dynsyms[index]["name"] == name:
                return index
            index = words[2 + buckets + index]

        return None


class DynamicLink(Link):
    def link(self, inputs, name=None, extra=()):
        self.counter += 1
        output = os.path.join(self.tmp, name or ("dyn%d" % self.counter))
        if os.path.exists(output):
            os.unlink(output)

        command = [self.compiler, "-Xlink-dynamic", "-o", output]
        command += list(extra) + list(inputs)
        result = subprocess.run(command, capture_output=True, text=True)

        self.suite.check(result.returncode >= 0,
                         "the linker died on signal %d" % -result.returncode)
        return output, result


# ------------------------------------------------------------------- tests

def testCallThroughPlt(link, suite):
    """A call into libc, and nothing else. Everything a dynamic link exists
    for has to be right for this to return a number: the stub, its .got.plt
    slot, the JUMP_SLOT relocation naming that slot, the .dynsym entry the
    relocation names, the .hash bucket that finds it and the DT_NEEDED that
    made ld.so open the library at all.

    'abs' rather than something that prints: a raw _start has no thread
    pointer, and a libc function that could report an error would reach for
    one. This one is three instructions and touches nothing."""
    code = callWithArgument(-5) + statusFromResult() + exitWithStatus()

    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("abs", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)
    addRela(elf, ".text", text, [(6, "abs", R_X86_64_PLT32, -4)])

    a = link.object(elf, "callsAbs.o")
    output = link.expectRuns([a, "-lc"], 5, "a call into libc runs",
                             extra=libraryPath())
    if output is None:
        return None

    exe = Dynamic(output)
    suite.equal([exe.string(o) for o in exe.tags(DT_NEEDED)], ["libc.so.6"],
                "libc is the one library recorded as needed")

    plt = exe.relocations(".rela.plt")
    suite.equal(len(plt), 1, "one PLT relocation for one called function")
    if plt:
        offset, symbol, type, addend = plt[0]
        suite.equal(type, R_X86_64_JUMP_SLOT, "the PLT relocation is a JUMP_SLOT")
        suite.equal(exe.dynsyms[symbol]["name"], "abs",
                    "the JUMP_SLOT names the function that was called")
        suite.equal(exe.dynsyms[symbol]["shndx"], SHN_UNDEF,
                    "a function the library owns is undefined here")

    return output


def testCopyRelocation(link, suite):
    """A data object the library owns is copied into this executable's .bss,
    so that every use site is an ordinary absolute reference. 'stdout' is the
    one that makes this load-bearing rather than optional - EduCC's own
    sources reach for it, and there is no other way for a non-PIE to."""
    code = loadAbsolutePointer() + statusIfNotNull() + exitWithStatus()

    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("stdout", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)
    addRela(elf, ".text", text, [(4, "stdout", R_X86_64_32S, 0)])

    a = link.object(elf, "readsStdout.o")
    # The copy holds what the library's own initializer put there, so a
    # non-NULL FILE* is proof ld.so wrote it rather than proof it was zeroed.
    output = link.expectRuns([a, "-lc"], 1, "a copied FILE* arrives non-NULL",
                             extra=libraryPath())
    if output is None:
        return

    exe = Dynamic(output)
    copies = [r for r in exe.relocations(".rela.dyn")
              if r[2] == R_X86_64_COPY]
    suite.equal(len(copies), 1, "one COPY relocation for one copied object")
    if not copies:
        return

    offset, symbol, _type, _addend = copies[0]
    entry = exe.dynsyms[symbol]

    suite.equal(entry["name"], "stdout", "the COPY names the object copied")
    suite.equal(entry["size"], 8, "the copy is the size the library gave it")
    suite.equal(entry["value"], offset,
                "the copied symbol is defined where the COPY writes")

    bss = exe.section(".bss")
    suite.check(bss is not None and
                bss["addr"] <= offset < bss["addr"] + bss["size"],
                "the copy lands in .bss")

    # The library binds its own references to this address, which only works
    # because the symbol is advertised as defined here.
    suite.check(entry["shndx"] != SHN_UNDEF,
                "a copied symbol is exported as defined, not undefined")

    writable = [s for s in exe.loads() if s["flags"] & PF_W]
    suite.check(any(s["vaddr"] <= offset < s["vaddr"] + s["memsz"]
                    for s in writable),
                "a dynamic relocation targets a writable segment")


def testStructure(link, suite, output):
    """What ld.so is entitled to assume. A program can run with any of this
    wrong right up until the day the table it did not check is consulted."""
    exe = Dynamic(output)

    phdr = exe.segment(PT_PHDR)
    suite.check(phdr is not None, "the program headers describe themselves")
    if phdr:
        suite.equal(phdr["offset"], exe.phoff, "PT_PHDR points at e_phoff")
        suite.equal(phdr["filesz"], len(exe.segments) * PHDR_SIZE,
                    "PT_PHDR covers every program header")
        suite.check(any(s["offset"] <= phdr["offset"] and
                        phdr["offset"] + phdr["filesz"] <=
                        s["offset"] + s["filesz"] for s in exe.loads()),
                    "PT_PHDR lies inside a loaded segment")
        suite.equal(exe.segments.index(phdr), 0, "PT_PHDR comes first")

    interp = exe.segment(PT_INTERP)
    suite.check(interp is not None, "an interpreter is named")
    if interp:
        text = exe.blob[interp["offset"]:
                        interp["offset"] + interp["filesz"]]
        suite.equal(text, INTERPRETER.encode() + b"\0",
                    "PT_INTERP is the loader's path, NUL-terminated")
        suite.equal(interp["vaddr"], exe.section(".interp")["addr"],
                    "PT_INTERP and .interp describe the same bytes")

    dynamic = exe.segment(PT_DYNAMIC)
    suite.check(dynamic is not None, "PT_DYNAMIC exists")
    if dynamic:
        suite.equal(dynamic["vaddr"], exe.section(".dynamic")["addr"],
                    "PT_DYNAMIC and .dynamic describe the same bytes")

    for wanted, name in ((DT_HASH, ".hash"), (DT_STRTAB, ".dynstr"),
                         (DT_SYMTAB, ".dynsym")):
        suite.equal(exe.tag(wanted), exe.section(name)["addr"],
                    "the dynamic entry for %s is where %s is" % (name, name))

    suite.equal(exe.tag(DT_STRSZ), exe.section(".dynstr")["size"],
                "DT_STRSZ is the size of .dynstr")

    # '-lc' is named twice on every link, so one soname reaching DT_NEEDED
    # twice is a library loaded twice rather than a harmless repetition.
    needed = [exe.string(o) for o in exe.tags(DT_NEEDED)]
    suite.equal(sorted(needed), sorted(set(needed)),
                "no library is recorded as needed twice: %s" % needed)
    suite.equal(exe.tag(DT_SYMENT), 24, "DT_SYMENT is the size of an entry")
    suite.check(exe.tag(DT_DEBUG) is not None,
                "DT_DEBUG is there for the loader to fill in")

    gotPlt = exe.section(".got.plt")
    plt = exe.section(".plt")
    relaPlt = exe.relocations(".rela.plt")

    suite.equal(exe.tag(DT_PLTGOT), gotPlt["addr"],
                "DT_PLTGOT is where .got.plt is")
    suite.equal(struct.unpack("<Q", exe.bytesAt(gotPlt["addr"], 8))[0],
                exe.section(".dynamic")["addr"],
                "the first .got.plt slot holds the address of .dynamic")
    suite.equal(gotPlt["size"], (3 + len(relaPlt)) * 8,
                "at .got.plt: three reserved slots and one per stub")
    suite.equal(plt["size"], (1 + len(relaPlt)) * 16,
                "at .plt: one resolver stub and one per function")
    suite.equal(exe.tag(DT_PLTRELSZ), len(relaPlt) * 24,
                "DT_PLTRELSZ is the size of .rela.plt")
    suite.equal(exe.tag(DT_JMPREL), exe.section(".rela.plt")["addr"],
                "DT_JMPREL is where .rela.plt is")

    for offset, symbol, type, _addend in relaPlt:
        # The first three slots are the loader's own; a JUMP_SLOT aimed at one
        # of them would overwrite the resolver it is supposed to reach.
        suite.check(gotPlt["addr"] + 24 <= offset <
                    gotPlt["addr"] + gotPlt["size"],
                    "a JUMP_SLOT names a slot past the reserved three")
        suite.equal((offset - gotPlt["addr"]) % 8, 0,
                    "a JUMP_SLOT is aligned to a slot")

    # Every stub's unresolved .got.plt slot points back into its own entry, so
    # that the first call goes through the resolver and the rest do not.
    for i in range(len(relaPlt)):
        slot = struct.unpack("<Q", exe.bytesAt(gotPlt["addr"] + 24 + i * 8,
                                               8))[0]
        entry = plt["addr"] + 16 * (i + 1)
        suite.equal(slot, entry + 6,
                    "an unresolved slot returns to its own stub's push")

    relro = exe.segment(PT_GNU_RELRO)
    suite.check(relro is not None, "PT_GNU_RELRO exists")
    if relro:
        writable = [s for s in exe.loads() if s["flags"] & PF_W]
        suite.check(any(s["vaddr"] <= relro["vaddr"] and
                        relro["vaddr"] + relro["memsz"] <=
                        s["vaddr"] + s["memsz"] for s in writable),
                    "PT_GNU_RELRO lies inside the writable segment")
        # ld.so protects whole pages, rounding the end down - but .got.plt is
        # written on every first call, so it must not be in the region at all.
        suite.check(gotPlt["addr"] >= relro["vaddr"] + relro["memsz"],
                    ".got.plt is outside the read-only-after-relocation region")
        suite.check(exe.section(".dynamic")["addr"] >= relro["vaddr"],
                    ".dynamic is inside it")

    for entry in exe.dynsyms[1:]:
        suite.check(exe.hashLookup(entry["name"]) is not None,
                    "'%s' can be found through .hash" % entry["name"])

    for value in exe.tags(DT_NEEDED):
        suite.check(0 < value < len(exe.dynstr),
                    "a DT_NEEDED names a string inside .dynstr")

    loads = exe.loads()
    for segment in loads:
        suite.equal(segment["offset"] % PAGE, segment["vaddr"] % PAGE,
                    "a loaded segment is congruent modulo the page size")
    for i in range(len(loads) - 1):
        suite.check(loads[i]["vaddr"] + loads[i]["memsz"] <=
                    loads[i + 1]["vaddr"], "loaded segments do not overlap")

    entryPoint = exe.entry
    suite.check(any(s["vaddr"] <= entryPoint < s["vaddr"] + s["filesz"]
                    for s in loads if s["flags"] & PF_X),
                "the entry point is inside an executable segment")


PROGRAM = """
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int order[8];
static int at = 0;

static void first(void) { order[at++] = 1; }
static void second(void) { order[at++] = 2; }

struct Node { struct Node *next; int value; };

int main(int argc, char **argv) {
  atexit(first);
  atexit(second);

  struct Node *head = NULL;
  int i;
  for (i = 0; i < 6; ++i) {
    struct Node *node = malloc(sizeof *node);
    node->value = i * i;
    node->next = head;
    head = node;
  }

  int total = 0;
  while (head) {
    struct Node *next = head->next;
    total += head->value;
    free(head);
    head = next;
  }

  // Not argv[0] itself: the two links have to be written to different files,
  // and a program that prints its own name would disagree about that alone.
  fprintf(stdout, "total=%d argc=%d named=%d\\n", total, argc,
          strrchr(argv[0], '/') != NULL);
  fprintf(stderr, "done\\n");

  return total % 7;
}
"""


def testDriver(compiler, tmp, suite):
    """Nothing below is about ELF: it is about a program that allocates, prints
    and unwinds its atexit handlers arriving at the right answer. ld used to be
    the oracle - the same source linked both ways had to behave the same - and
    what replaces it is the answer itself, written down here."""
    source = os.path.join(tmp, "program.c")
    with open(source, "w") as f:
        f.write(PROGRAM)

    path = os.path.join(tmp, "prog")
    result = subprocess.run([compiler, "-oneline", "-o", path, source],
                            capture_output=True, text=True)
    if result.returncode != 0:
        suite.check(False, "the link failed: %s" % result.stderr.strip())
        return None

    run = runProgram([path])
    if run is None:
        suite.check(False, "the program never finished")
        return path

    # 0 + 1 + 4 + 9 + 16 + 25, and the exit status is that modulo seven.
    suite.equal(run.returncode, 6, "the program's exit status")
    suite.equal(run.stdout, b"total=55 argc=1 named=1\n", "and its stdout")
    suite.equal(run.stderr, b"done\n", "and its stderr")

    return path


LIBRARY = """
int libraryValue(void) { return 11; }
int libraryData = 31;
char libraryBuffer[24] = "shared";
"""

CONSUMER = """
extern int libraryValue(void);
extern int libraryData;
extern char libraryBuffer[24];

int main(void) {
  if (libraryBuffer[0] != 's') return 2;
  libraryData += 1;                       // the copy is ours to write
  return libraryValue() + libraryData == 43 ? 0 : 3;
}
"""


def testSharedLibrary(compiler, tmp, suite):
    """A library that is not libc, found the way a user's would be. Everything
    the C runtime exercises is glibc's shape; this one is EduCC's own, and its
    sizes are known here rather than read off the host - which is what this
    asserts about the copies, where the libc round trip can only assert that
    whatever glibc chose was copied."""
    tag = "roundtrip"
    source = os.path.join(tmp, "roundtrip.c")
    consumer = os.path.join(tmp, "consumer.c")
    directory = os.path.join(tmp, "lib-" + tag)
    library = os.path.join(directory, "libroundtrip.so")
    program = os.path.join(tmp, "consumer-" + tag)

    os.makedirs(directory, exist_ok=True)
    with open(source, "w") as f:
        f.write(LIBRARY)
    with open(consumer, "w") as f:
        f.write(CONSUMER)

    built = subprocess.run([compiler, "-oneline", "-fPIC", "-shared",
                            "-o", library, source],
                           capture_output=True, text=True)
    if built.returncode != 0:
        suite.check(False, "%s: the shared object could not be built: %s"
                    % (tag, built.stderr.strip()))
        return None

    linked = subprocess.run([compiler, "-oneline",
                             "-o", program, consumer, "-L" + directory,
                             "-lroundtrip", "-Wl,-rpath," + directory],
                            capture_output=True, text=True)
    if linked.returncode != 0:
        suite.check(False, "%s: linking against the shared object failed: %s"
                    % (tag, linked.stderr.strip()))
        return None

    run = runProgram([program])
    if run is None:
        suite.check(False,
                    "%s: the program against its own library never finished"
                    % tag)
        return None

    suite.equal(run.returncode, 0,
                "%s: a program against its own library runs" % tag)

    exe = Dynamic(program)

    needed = [exe.string(o) for o in exe.tags(DT_NEEDED)]
    suite.check("libroundtrip.so" in needed,
                "%s: the library is recorded as needed: %s" % (tag, needed))
    suite.check("libc.so.6" in needed, "%s: so is libc" % tag)

    runpath = exe.tag(DT_RUNPATH)
    suite.check(runpath is not None and exe.string(runpath) == directory,
                "%s: '-Wl,-rpath' becomes DT_RUNPATH" % tag)

    copies = {exe.dynsyms[r[1]]["name"]: exe.dynsyms[r[1]]
              for r in exe.relocations(".rela.dyn") if r[2] == R_X86_64_COPY}

    # Sizes this test chose, rather than whatever the host's libc happens to
    # use - which is the whole reason for having a library of its own.
    suite.equal(sorted(copies), ["libraryBuffer", "libraryData"],
                "%s: both data objects are copied and the function is not" % tag)
    if "libraryData" in copies:
        suite.equal(copies["libraryData"]["size"], 4,
                    "%s: an int is copied as four bytes" % tag)
    if "libraryBuffer" in copies:
        suite.equal(copies["libraryBuffer"]["size"], 24,
                    "%s: an array is copied whole" % tag)

    plt = [exe.dynsyms[r[1]]["name"] for r in exe.relocations(".rela.plt")]
    suite.check("libraryValue" in plt,
                "%s: the function is reached through a stub" % tag)

    return program


# A library whose own call to 'reading' has to be answerable by somebody else:
# that is the whole of what preemption is, and it is the one thing a shared
# object cannot get right by binding everything it can see locally.
INTERPOSED = """
int counter = 7;
static int internal = 3;
const char *label = "inside";

int reading(void) { return 1; }
int viaGlobal(void) { return reading() + internal + (counter - 7); }
const char *labelOf(void) { return label; }
"""

INTERPOSER = """
extern int counter;
extern int reading(void);
extern int viaGlobal(void);
extern const char *labelOf(void);

int reading(void) { return 100; }      // the library must see this one

static int strEq(const char *a, const char *b) {
  while (*a && *a == *b) { ++a; ++b; }
  return *a == *b;
}

int main(void) {
  if (viaGlobal() != 103) return 1;    // 100 from here, 3 from its own static
  if (counter != 7) return 2;
  counter = 9;
  if (viaGlobal() != 105) return 3;    // and the library reads the same copy
  if (!strEq(labelOf(), "inside")) return 4;
  return 0;
}
"""


def testSharedObject(compiler, tmp, suite):
    """The producing side on its own: what '-shared' writes, and whether the
    loader does what it says. The program deliberately defines a name the
    library also defines and calls itself through - so a library that bound
    that call to its own definition passes every structural check here and
    still returns 1."""
    directory = os.path.join(tmp, "interpose")
    os.makedirs(directory, exist_ok=True)

    source = os.path.join(directory, "inside.c")
    consumer = os.path.join(directory, "outside.c")
    library = os.path.join(directory, "libinside.so")
    program = os.path.join(directory, "outside")

    with open(source, "w") as f:
        f.write(INTERPOSED)
    with open(consumer, "w") as f:
        f.write(INTERPOSER)

    built = subprocess.run([compiler, "-oneline", "-fPIC",
                            "-shared", "-Wl,-soname,libinside.so.1",
                            "-o", library, source],
                           capture_output=True, text=True)
    if built.returncode != 0:
        suite.check(False, "'-shared' failed: %s" % built.stderr.strip())
        return None

    so = Dynamic(library)

    suite.equal(so.type, ET_DYN, "'-shared' writes ET_DYN")
    suite.equal(so.entry, 0, "and no entry point")
    suite.check(so.segment(PT_INTERP) is None,
                "a shared object names no interpreter")
    suite.check(so.segment(PT_DYNAMIC) is not None, "PT_DYNAMIC is there")

    soname = so.tag(DT_SONAME)
    suite.check(soname is not None and so.string(soname) == "libinside.so.1",
                "'-Wl,-soname' becomes DT_SONAME")

    relocs = so.relocations(".rela.dyn")
    kinds = [r[2] for r in relocs]

    relative = so.tag(DT_RELACOUNT) or 0
    suite.check(relative > 0, "there are RELATIVE relocations: %d" % relative)
    suite.equal(kinds[:relative], [R_X86_64_RELATIVE] * relative,
                "DT_RELACOUNT counts a run that really is at the front")
    suite.equal(kinds.count(R_X86_64_RELATIVE), relative,
                "and there are no others behind it")

    # An addend the loader adds its own load address to, so it has to be an
    # offset into this object rather than an address anywhere else.
    span = max(s["vaddr"] + s["memsz"] for s in so.loads())
    strays = [hex(r[3]) for r in relocs[:relative] if not 0 < r[3] < span]
    suite.check(not strays, "every RELATIVE addend is inside the object: %s"
                % strays[:3])

    byName = {so.dynsyms[r[1]]["name"]: r for r in relocs
              if r[2] == R_X86_64_GLOB_DAT}
    suite.check("counter" in byName or "label" in byName,
                "a global reached through the GOT gets GLOB_DAT: %s"
                % sorted(byName))

    exported = {s["name"]: s for s in so.dynsyms if s["shndx"] != SHN_UNDEF}
    for name in ("counter", "reading", "viaGlobal", "labelOf"):
        suite.check(name in exported, "'%s' is exported" % name)
        suite.check(so.hashLookup(name) is not None,
                    "and '%s' is findable through .hash" % name)

    suite.check("internal" not in exported, "a static is not exported")

    # The call the library makes to its own global. It must be a stub, because
    # the program below defines the same name.
    stubs = [so.dynsyms[r[1]]["name"] for r in so.relocations(".rela.plt")]
    suite.check("reading" in stubs,
                "a call to a preemptible name goes through a stub: %s" % stubs)

    # What DT_NEEDED will say, so it has to be a name that resolves.
    versioned = os.path.join(directory, "libinside.so.1")
    if not os.path.exists(versioned):
        os.symlink("libinside.so", versioned)

    linked = subprocess.run([compiler, "-oneline",                              "-o", program, consumer, "-L" + directory,
                             "-linside", "-Wl,-rpath," + directory],
                            capture_output=True, text=True)
    if linked.returncode != 0:
        suite.check(False, "linking against it failed: %s"
                    % linked.stderr.strip())
        return None

    # DT_SONAME wins over the name the library was found under, which is the
    # only reason a versioned filename works at all.
    needed = [Dynamic(program).string(o) for o in Dynamic(program).tags(DT_NEEDED)]
    suite.check("libinside.so.1" in needed,
                "the soname is what gets recorded as needed: %s" % needed)

    run = runProgram([program])
    if run is None:
        suite.check(False, "the interposing program never finished")
        return None

    suite.equal(run.returncode, 0,
                "the program's definition wins inside the library too")
    return library


def testAgainstReadelf(suite, outputs):
    """binutils as a second opinion about the file itself. The reader above and
    the linker are both this project's understanding of ELF and could share a
    misunderstanding; readelf cannot share it, and it complains on stderr when
    a table does not add up rather than only when the file will not load."""
    if shutil.which("readelf") is None:
        return False

    for path in outputs:
        result = subprocess.run(["readelf", "--all", path],
                                capture_output=True, text=True)
        suite.equal(result.returncode, 0,
                    "readelf reads %s" % os.path.basename(path))

        complaints = [line for line in result.stderr.splitlines()
                      if line.strip()]
        suite.check(not complaints, "readelf has nothing to say about %s: %s"
                    % (os.path.basename(path), complaints[:3]))

        # It also parses the two tables this file writes by hand, so its own
        # count of them is an independent reading of the same bytes. Asked one
        # table at a time: '--all' shows the PLT twice, once as relocations and
        # once as the loader's view of them.
        exe = Dynamic(path)

        relocations = subprocess.run(["readelf", "--relocs", path],
                                     capture_output=True, text=True).stdout
        theirs = len([line for line in relocations.splitlines()
                      if "R_X86_64_JUMP_SLO" in line])
        suite.equal(theirs, len(exe.relocations(".rela.plt")),
                    "readelf and this reader count the same JUMP_SLOTs")

        theirs = len([line for line in relocations.splitlines()
                      if "R_X86_64_COPY" in line])
        suite.equal(theirs, len([r for r in exe.relocations(".rela.dyn")
                                 if r[2] == R_X86_64_COPY]),
                    "readelf and this reader count the same COPYs")

        dynamic = subprocess.run(["readelf", "--dynamic", path],
                                 capture_output=True, text=True).stdout
        theirs = len([line for line in dynamic.splitlines()
                      if "Shared library: [" in line])
        suite.equal(theirs, len(exe.tags(DT_NEEDED)),
                    "readelf and this reader count the same DT_NEEDEDs")

    return True


def testArchiveBehindALibrary(link, suite):
    """A shared object that answers for a name settles it: the archive listed
    after it must keep its member. Getting this wrong is silent - the program
    links and runs, and calls the wrong function - so what the fixture does is
    make the two answers different numbers."""
    from reader_test import archive
    from linker_test import memberObject

    # 'abs' is in libc, and this archive would like to be asked instead. It
    # ignores its argument, so which one answered is the exit code.
    members = [("shadow.o", memberObject("abs", 99))]
    link.object(archive(members, [("abs", 0)]), "libshadow.a")

    code = callWithArgument(-5) + statusFromResult() + exitWithStatus()
    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("abs", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)
    addRela(elf, ".text", text, [(6, "abs", R_X86_64_PLT32, -4)])

    caller = link.object(elf, "callsAbsAgain.o")
    output = link.expectRuns([caller, "-lc", "-lshadow"], 5,
                             "libc answers for 'abs', not the archive behind it",
                             extra=["-L" + link.tmp] + libraryPath())

    if output is None:
        return

    exe = Dynamic(output)
    plt = [exe.dynsyms[r[1]]["name"] for r in exe.relocations(".rela.plt")]
    # A stub exists only for a name this link did not define, so this says the
    # member stayed in the archive as much as the exit code does.
    suite.check("abs" in plt, "'abs' is reached through a stub into libc")


SHT_GNU_VERSYM = 0x6fffffff
DT_SONAME, DT_NULL_TAG = 14, 0


def versionedLibrary():
    """A shared object exporting one name twice, the way glibc exports
    '__libc_start_main': an old version nothing unversioned may bind to, and
    the default one. The sizes differ so that which of them was chosen is
    visible in what gets copied."""
    from reader_test import (Elf, ET_DYN, SHT_DYNSYM, SHT_STRTAB, SHT_DYNAMIC,
                             SYM_SIZE, DYN_SIZE, STT_OBJECT)

    elf = Elf(etype=ET_DYN)
    data = elf.addSection(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                          b"\0" * 128, align=8)

    soname = b"libversioned.so.1"
    name = b"versioned"
    dynstr = b"\0" + soname + b"\0" + name + b"\0"
    sonameAt, nameAt = 1, 1 + len(soname) + 1
    dynstrIndex = elf.addSection(".dynstr", SHT_STRTAB, SHF_ALLOC, dynstr)

    info = (STB_GLOBAL << 4) | STT_OBJECT
    dynsym = struct.pack("<IBBHQQ", 0, 0, 0, SHN_UNDEF, 0, 0)
    # The old one first, so that first-wins would pick exactly the wrong entry.
    dynsym += struct.pack("<IBBHQQ", nameAt, info, 0, data, 0, 4)
    dynsym += struct.pack("<IBBHQQ", nameAt, info, 0, data, 64, 64)
    elf.addSection(".dynsym", SHT_DYNSYM, SHF_ALLOC, dynsym, align=8,
                   link=dynstrIndex, info=1, entsize=SYM_SIZE)

    # Parallel to .dynsym: 0x8000 marks the definition an unversioned
    # reference may not bind to.
    elf.addSection(".gnu.version", SHT_GNU_VERSYM, SHF_ALLOC,
                   struct.pack("<HHH", 0, 0x8002, 0x0002), align=2,
                   link=dynstrIndex, entsize=2)

    entries = struct.pack("<qQ", DT_SONAME, sonameAt)
    entries += struct.pack("<qQ", DT_NULL_TAG, 0)
    elf.addSection(".dynamic", SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE, entries,
                   align=8, link=dynstrIndex, entsize=DYN_SIZE)

    return elf


def testDefaultVersionWins(link, suite):
    """Which of two same-named definitions a library meant. ld.so binds an
    unversioned reference to the default one, so a linker that picked the
    other would size its copy from a symbol nothing will ever write - and the
    program would not crash, it would read four bytes of a sixty-four byte
    object and carry on.

    Not run, only inspected: the library is synthesized and no loader would
    have it. What the executable says about the copy is the whole answer."""
    code = loadAbsolutePointer() + statusIfNotNull() + exitWithStatus()

    elf = Elf()
    text = elf.addSection(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                          code, align=16)
    elf.addSymbol("_start", STB_GLOBAL, STT_FUNC, text, 0, len(code))
    elf.addSymbol("versioned", STB_GLOBAL, STT_NOTYPE, SHN_UNDEF)
    addRela(elf, ".text", text, [(4, "versioned", R_X86_64_32S, 0)])

    a = link.object(elf, "usesVersioned.o")
    link.object(versionedLibrary(), "libversioned.so")

    output, result = link.link([a, "-lversioned"],
                               extra=["-L" + link.tmp])
    if result.returncode != 0:
        suite.check(False, "linking against a versioned library failed: %s"
                    % result.stderr.strip())
        return

    exe = Dynamic(output)
    copies = [exe.dynsyms[r[1]] for r in exe.relocations(".rela.dyn")
              if r[2] == R_X86_64_COPY]

    suite.equal(len(copies), 1, "the two definitions are one symbol")
    if copies:
        suite.equal(copies[0]["size"], 64,
                    "the copy is sized from the default version, not the old one")


def testAsNeeded(compiler, tmp, suite, directory):
    """A library named on the command line and never referenced. ld records it
    all the same - '--no-as-needed' is its default - and so must this: what a
    program looks up out of one at run time is invisible to the link, which is
    how tcc's '-run' finds 'sin' after nothing in tcc itself called it."""
    source = os.path.join(tmp, "unused.c")
    with open(source, "w") as f:
        f.write("int main(void) { return 0; }\n")

    for flags, wanted in (([], True), (["-Wl,--as-needed"], False)):
        label = "--as-needed" if flags else "default"
        program = os.path.join(tmp, "unused-" + label.strip("-"))
        linked = subprocess.run([compiler, "-oneline", "-o", program,
                                 source, "-L" + directory, "-lroundtrip",
                                 "-Wl,-rpath," + directory] + flags,
                                capture_output=True, text=True)
        if linked.returncode != 0:
            suite.check(False, "%s: the link failed: %s"
                        % (label, linked.stderr.strip()))
            continue

        needed = [Dynamic(program).string(o)
                  for o in Dynamic(program).tags(DT_NEEDED)]
        suite.equal("libroundtrip.so" in needed, wanted,
                    "%s: an unused library is%s recorded as needed: %s"
                    % (label, "" if wanted else " not", needed))
        suite.check("libc.so.6" in needed,
                    "%s: libc is needed either way" % label)


def testRefused(compiler, tmp, suite):
    """A flag the in-house linker cannot honour is named, not ignored: a
    silently dropped '-Wl,' is a link that succeeds and does the wrong thing."""
    source = os.path.join(tmp, "tiny.c")
    with open(source, "w") as f:
        f.write("int main(void) { return 0; }\n")

    result = subprocess.run([compiler, "-oneline",                              "-Wl,--gc-sections", "-o",
                             os.path.join(tmp, "tiny"), source],
                            capture_output=True, text=True)
    suite.check(result.returncode != 0, "an unknown ld flag is refused")
    suite.contains(result.stderr, "--gc-sections",
                   "the refusal names the flag")

    # A PC-relative reference to a name somebody else may define is what
    # '-fPIC' exists to avoid, and a shared object built without it is one ld
    # refuses too. Saying which symbol is the whole value of the message.
    source = os.path.join(tmp, "notpic.c")
    with open(source, "w") as f:
        f.write("int counter = 1;\nint bump(void) { return ++counter; }\n")

    result = subprocess.run([compiler, "-oneline",                              "-shared", "-o", os.path.join(tmp, "notpic.so"),
                             source], capture_output=True, text=True)
    suite.check(result.returncode != 0,
                "'-shared' without '-fPIC' is refused")
    suite.contains(result.stderr, "counter",
                   "and the refusal names the symbol")


def main():
    if len(sys.argv) < 2:
        print("usage: dynamic_test.py <path to educc>", file=sys.stderr)
        return 2

    compiler = sys.argv[1]
    if not os.path.exists(compiler):
        print("no such compiler: %s" % compiler, file=sys.stderr)
        return 2

    if crtDirectory() is None or not os.path.exists(INTERPRETER):
        print("== skipped: no C runtime to link against", file=sys.stderr)
        return 2

    suite = Suite()
    with tempfile.TemporaryDirectory(prefix="educc-dyn-") as tmp:
        link = DynamicLink(compiler, tmp, suite)

        called = testCallThroughPlt(link, suite)
        testCopyRelocation(link, suite)
        testArchiveBehindALibrary(link, suite)
        testDefaultVersionWins(link, suite)

        compiled = testDriver(compiler, tmp, suite)

        consumers = [c for c in [testSharedLibrary(compiler, tmp, suite)]
                     if c is not None]
        library = testSharedObject(compiler, tmp, suite)
        testAsNeeded(compiler, tmp, suite, os.path.join(tmp, "lib-roundtrip"))
        testRefused(compiler, tmp, suite)

        outputs = [o for o in [called, compiled] + consumers if o is not None]
        for output in outputs:
            testStructure(link, suite, output)

        # The shared object is read by readelf but not by testStructure: half
        # of what that asserts is about how a program is started, which is the
        # one thing a library is not.
        oracle = testAgainstReadelf(suite,
                                    outputs + ([library] if library else []))

    print("== %d checks over dynamically linked and executed programs%s"
          % (suite.checks,
             ", with readelf as an oracle" if oracle
             else " (readelf absent: no second reading)"))

    for failure in suite.failures:
        print("FAIL: " + failure)

    if suite.failures:
        print("== %d of %d checks failed" % (len(suite.failures), suite.checks))
        return 1

    print("== all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
