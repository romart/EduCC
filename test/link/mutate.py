#!/usr/bin/env python3
"""Break the linker on purpose and see whether its tests notice.

    python3 test/link/mutate.py build-mut              # everything, ~45 min
    python3 test/link/mutate.py build-mut --only PLT   # one group
    python3 test/link/mutate.py build-mut --list       # just name them
    python3 test/link/mutate.py build-mut --restore    # after a killed run

A green test suite says nothing about whether the suite would go red. These
are the mutations that answer that: each one replaces a line of src/link/link.c
with a plausible wrong version, rebuilds, and runs reader_test.py,
linker_test.py, dynamic_test.py and test/checkers/link.py. A mutation nothing
catches is a hole in the tests, not a bug in the linker - and every hole this
found has been closed, which is the only reason the numbers below are worth
printing.

It has earned its keep twice. The first sweep over the static half left six
survivors: three were real gaps (a pointer whose upper four bytes were already
zero, a tentative definition that would have been aligned by luck, an sh_info
of zero making its own check vacuous), one was dead state in the linker rather
than a missing test, one was an unusable pattern, and one is unreachable by
construction. The second, over the dynamic half, found three more: an armap
whose order let a single pass resolve a chain that was supposed to need two,
an archive shadowing a library that nothing checked, and no test at all for
which of two versions of a name a library meant.

As of the last full run: 63 mutations, 61 caught, 2 equivalent, 0 missed.

**Not a CTest entry, and deliberately so.** It rebuilds the compiler once per
mutation and edits a tracked source file to do it, which is not something to
have running beside other jobs - and nothing else, this session included,
should edit src/link/link.c while it runs: it rewrites the file from its own
copy on every mutation, so an edit made underneath it is simply lost.

It restores the file in a `finally`, which a Ctrl-C or a SIGTERM now reaches
too, and checks that it did. Only a SIGKILL gets past that, and then the
original is left in src/link/link.c.mutate-backup and the source on disk may
still carry a mutation: the next run refuses to start until that is dealt
with, and `--restore` deals with it.

The mutations are literal text, so they rot the moment the lines they name are
edited - a mutation whose pattern no longer matches is reported as unusable
rather than skipped quietly, because an unusable mutation is a check that
silently stopped running. Repair it or delete it; do not leave it.
"""
import os
import shutil
import signal
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__))))
SOURCE = os.path.join(ROOT, "src/link/link.c")


# Each entry is (name, text to find, text to put there). The text has to occur
# exactly once, so that what a mutation changes is never in question.
MUTATIONS = [
    # ------------------------------------------------ placement and addresses
    ("alignment dropped",
     "  return (value + align - 1) & ~(align - 1);",
     "  return value;"),

    ("segments are not page-aligned in the file",
     "    segment->offset = alignUp(offset, LINK_PAGE_SIZE);",
     "    segment->offset = offset;"),

    ("segments overlap in memory",
     "    segment->address = alignUp(base, LINK_PAGE_SIZE) + segment->offset;",
     "    segment->address = alignUp(base, LINK_PAGE_SIZE);"),

    ("a segment's memory size is its file size",
     "    segment->memSize = memCursor - segment->offset;",
     "    segment->memSize = cursor - segment->offset;"),

    ("section alignment is not carried to the output",
     "  if (piece->align > output->align) output->align = piece->align;",
     "  output->align = 1;"),

    ("sections are not folded into a parent",
     "    if (strncmp(name, prefix, length) == 0 && name[length] == '.') {",
     "    if (0) {"),

    ("every section name is folded",
     "    if (strcmp(name, prefix) == 0) return prefix;",
     "    if (strncmp(name, prefix, 5) == 0) return prefix;"),

    ("the entry point is the start of the segment",
     "  linker.entry = entry->address;",
     "  linker.entry = options->imageBase;"),

    ("the stack is executable",
     "  phdrs[count].p_flags = PF_R | PF_W;\n  phdrs[count].p_align = 0x10;",
     "  phdrs[count].p_flags = PF_R | PF_W | PF_X;\n  phdrs[count].p_align = 0x10;"),

    ("sh_info of the symbol table is zero",
     "    shdrs[symtabIndex].sh_info = (uint32_t)symtab.firstGlobal;",
     "    shdrs[symtabIndex].sh_info = 0;"),

    # ------------------------------------------------------------ relocations
    ("PC32 forgets the program counter",
     "      int64_t value = (int64_t)(S + (uint64_t)A) - (int64_t)P;",
     "      int64_t value = (int64_t)(S + (uint64_t)A);"),

    ("PC32 range check dropped",
     "      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {\n"
     "        linkError(l, \"%s: '%s' is too far from '%s' for a 32-bit displacement\",",
     "      if (0) {\n"
     "        linkError(l, \"%s: '%s' is too far from '%s' for a 32-bit displacement\","),

    ("32-bit range check dropped",
     "      if (value > (uint64_t)RELOC_U32_MAX) {",
     "      if (0) {"),

    ("signed 32-bit range check dropped",
     "      int64_t value = (int64_t)(S + (uint64_t)A);\n"
     "      if (value < RELOC_S32_MIN || value > RELOC_S32_MAX) {",
     "      int64_t value = (int64_t)(S + (uint64_t)A);\n"
     "      if (0) {"),

    ("a 64-bit relocation writes 32 bits",
     "      write64(where, S + (uint64_t)A);",
     "      write32(where, (uint32_t)(S + (uint64_t)A));"),

    ("the addend is ignored",
     "  int64_t A = rela->r_addend;",
     "  int64_t A = 0;"),

    ("relocation offset bounds check dropped",
     "  if (rela->r_offset > piece->size || piece->size - rela->r_offset < width) {",
     "  if (0) {"),

    ("PLT32 is not a call",
     "    case R_X86_64_PC32:\n    case R_X86_64_PLT32: {",
     "    case R_X86_64_PC32: {"),

    ("an absolute symbol is an address",
     "      if (definition->shndx == SHN_ABS) {\n        symbol->address = definition->value;",
     "      if (definition->shndx == SHN_ABS) {\n        symbol->address = definition->value + 1;"),

    ("a local symbol's value is ignored",
     "  *result = piece->address + symbol->value;",
     "  *result = piece->address;"),

    ("a GOT relocation names the symbol, not its slot",
     "      int64_t value = (int64_t)(G + (uint64_t)A) - (int64_t)P;",
     "      int64_t value = (int64_t)(S + (uint64_t)A) - (int64_t)P;"),

    # ------------------------------------------------------------- resolution
    ("the smallest tentative definition wins",
     "      if (symbol->size > global->commonSize) global->commonSize = symbol->size;",
     "      if (global->commonSize == 0) global->commonSize = symbol->size;"),

    ("tentative definitions ignore alignment",
     "      uint64_t align = symbol->value != 0 ? symbol->value : 1;",
     "      uint64_t align = 1;"),

    ("commons are packed without alignment",
     "    size = alignUp(size, symbol->commonAlign);",
     "    size = size;"),

    ("two definitions are allowed",
     "      if (global->definition != NULL && !global->weakDefinition && !weak) {",
     "      if (0) {"),

    ("a strong definition does not override a weak one",
     "      if (global->definition == NULL || (global->weakDefinition && !weak)) {",
     "      if (global->definition == NULL) {"),

    # Unreachable by construction: two strong definitions are reported and the
    # branch abandoned above, so a weak one never reaches this test with a
    # strong one already in place. Kept as the record of why it survives.
    ("a weak definition overrides a strong one",
     "      if (global->definition == NULL || (global->weakDefinition && !weak)) {",
     "      if (global->definition == NULL || !weak) {"),

    ("undefined symbols are not reported",
     "    if (symbol->strongReference) {\n      linkError(l, \"undefined symbol '%s'\", symbol->name);",
     "    if (0) {\n      linkError(l, \"undefined symbol '%s'\", symbol->name);"),

    ("a weak reference is as good as a strong one",
     "    if (symbol->bind != STB_WEAK) global->strongReference = TRUE;",
     "    global->strongReference = TRUE;"),

    ("a shared object needs no loader",
     "  if (kind == INPUT_DYN && !l->dynamic) {",
     "  if (0) {"),

    ("a non-default version is as good as the default one",
     "    if (!symbol->defaultVersion) continue;",
     "    if (FALSE) continue;"),

    # ------------------------------------------------------ linker-defined
    ("'_end' stops where the file's bytes do",
     "  if (strcmp(name, \"end\") == 0 || strcmp(name, \"_end\") == 0) return memoryEnd;",
     "  if (strcmp(name, \"end\") == 0 || strcmp(name, \"_end\") == 0) return dataEnd;"),

    ("'__bss_start' is the end of the last written section",
     "  if (strcmp(name, \"__bss_start\") == 0) return bssStart;",
     "  if (strcmp(name, \"__bss_start\") == 0) return dataEnd;"),

    ("a linker-defined name beats the program's own",
     "    if (symbol->definition != NULL || symbol->isCommon) continue;\n\n"
     "    symbol->linkerDefined = TRUE;",
     "    if (symbol->isCommon) continue;\n\n"
     "    symbol->linkerDefined = TRUE;"),

    # -------------------------------------------------- archives and scripts
    ("an archive is searched once",
     "  Boolean progress = TRUE;\n\n  while (progress) {\n    progress = FALSE;",
     "  Boolean progress = TRUE;\n\n  while (progress) {\n    progress = FALSE;\n"
     "    if (archive->members[0].loaded != NULL) break;"),

    ("an archive gives up every member",
     "      if (!wantsArchiveSymbol(l, entry->name)) continue;",
     "      if (FALSE) continue;"),

    ("an archive answers for what a library already defines",
     "  if (symbol->shared != NULL) return FALSE;",
     "  if (FALSE) return FALSE;"),

    ("a linker script's inputs are not read",
     "    if (!inputContext[depth]) continue;",
     "    if (depth == 0) continue;"),

    # ------------------------------------------------------ stubs and the GOT
    ("PLT0 pushes the wrong .got.plt word",
     "  write32(plt + 2, (uint32_t)(gotPlt + 8 - (pltAddress + 6)));",
     "  write32(plt + 2, (uint32_t)(gotPlt - (pltAddress + 6)));"),

    ("PLT0 jumps through the wrong .got.plt word",
     "  write32(plt + 8, (uint32_t)(gotPlt + 16 - (pltAddress + 12)));",
     "  write32(plt + 8, (uint32_t)(gotPlt + 8 - (pltAddress + 12)));"),

    ("a PLT stub jumps through the slot before its own",
     "    write32(entry + 2, (uint32_t)(slot - (address + 6)));",
     "    write32(entry + 2, (uint32_t)(slot - 8 - (address + 6)));"),

    ("a PLT stub's tail jump misses PLT0",
     "    write32(entry + 12, (uint32_t)(pltAddress - (address + 16)));",
     "    write32(entry + 12, (uint32_t)(pltAddress - (address + 12)));"),

    ("an unresolved slot points at the stub's jump, not its push",
     "    write64(gotPltData + GOT_ENTRY_SIZE * (GOT_PLT_RESERVED + i), address + 6);",
     "    write64(gotPltData + GOT_ENTRY_SIZE * (GOT_PLT_RESERVED + i), address);"),

    ("the first .got.plt slot does not name .dynamic",
     "    write64((uint8_t *)l->gotPltPiece->data, l->dynamicPiece->address);",
     "    write64((uint8_t *)l->gotPltPiece->data, 0);"),

    ("the loader's reserved slots are not reserved",
     "#define GOT_PLT_RESERVED 3",
     "#define GOT_PLT_RESERVED 1"),

    ("a JUMP_SLOT names a reserved slot",
     "      entries[i].r_offset = l->gotPltPiece->address +\n"
     "                            GOT_ENTRY_SIZE * (GOT_PLT_RESERVED + i);",
     "      entries[i].r_offset = l->gotPltPiece->address +\n"
     "                            GOT_ENTRY_SIZE * i;"),

    ("a stub's address is not the symbol's address",
     "      symbol->address = l->pltSection->address +\n"
     "                        (uint64_t)(symbol->pltIndex + 1) * PLT_ENTRY_SIZE;",
     "      symbol->address = l->pltSection->address;"),

    # ------------------------------------------------------------ copied data
    ("a COPY relocation writes into the library",
     "      entries[i].r_offset = symbol->address;\n"
     "      entries[i].r_info = ELF64_R_INFO(symbol->dynIndex, R_X86_64_COPY);",
     "      entries[i].r_offset = symbol->shared->value;\n"
     "      entries[i].r_info = ELF64_R_INFO(symbol->dynIndex, R_X86_64_COPY);"),

    ("a copied symbol is left undefined",
     "      entry->st_shndx = (uint16_t)l->copyPiece->output->index;",
     "      entry->st_shndx = SHN_UNDEF;"),

    ("a copy is one byte long",
     "  symbol->copySize = shared->size;",
     "  symbol->copySize = 1;"),

    ("a library's data is reached through a stub",
     "  if (shared->type == STT_FUNC || shared->type == STT_GNU_IFUNC ||\n"
     "      (shared->type == STT_NOTYPE && shared->size == 0)) {",
     "  if (1) {"),

    ("a library's code is copied instead of called",
     "    needPlt(l, symbol);\n  } else {\n    needCopy(l, symbol);",
     "    needCopy(l, symbol);\n  } else {\n    needCopy(l, symbol);"),

    # --------------------------------------------------- the loader's tables
    ("the hash function drops its high nibble fold",
     "    if (high != 0) hash ^= high >> 24;",
     "    if (high != 0) hash ^= 0;"),

    ("hash chains are not linked",
     "    chains[symbol->dynIndex] = buckets[bucket];\n"
     "    buckets[bucket] = symbol->dynIndex;",
     "    buckets[bucket] = symbol->dynIndex;\n"
     "    (void)chains;"),

    ("the bucket count is written as the chain count",
     "  hash[0] = bucketCount;\n  hash[1] = chainCount;",
     "  hash[0] = chainCount;\n  hash[1] = chainCount;"),

    ("dynamic symbol indices are off by one",
     "  symbol->dynIndex = (unsigned)l->dynsyms.size + 1;",
     "  symbol->dynIndex = (unsigned)l->dynsyms.size + 2;"),

    ("nothing is recorded as needed",
     "  if (symbol->sharedOwner != NULL) symbol->sharedOwner->needed = TRUE;",
     "  if (symbol->sharedOwner == NULL) symbol->sharedOwner->needed = TRUE;"),

    ("DT_PLTGOT names another section",
     "  DYN(DT_PLTGOT, l->gotPltPiece->address);",
     "  DYN(DT_PLTGOT, l->hashPiece->address);"),

    ("DT_STRSZ is the number of names",
     "  DYN(DT_STRSZ, l->dynstr.size);",
     "  DYN(DT_STRSZ, l->dynsyms.size);"),

    ("sh_info of .dynsym claims every entry is local",
     "  l->dynsymSection->info = 1; // only the null entry is local",
     "  l->dynsymSection->info = 0;"),

    ("PT_INTERP describes .dynamic",
     "    phdrs[count].p_offset = l->interpPiece->offset;\n"
     "    phdrs[count].p_vaddr = l->interpPiece->address;",
     "    phdrs[count].p_offset = l->dynamicPiece->offset;\n"
     "    phdrs[count].p_vaddr = l->dynamicPiece->address;"),

    ("PT_PHDR points at the ELF header",
     "    phdrs[count].p_offset = sizeof(Elf64_Ehdr);\n"
     "    phdrs[count].p_vaddr = first->address + sizeof(Elf64_Ehdr);",
     "    phdrs[count].p_offset = 0;\n"
     "    phdrs[count].p_vaddr = first->address;"),

    ("PT_GNU_RELRO seals .got.plt too",
     "      if (class == CLASS_DATA && output->rank < RANK_GOT_PLT) {",
     "      if (class == CLASS_DATA && output->rank <= RANK_GOT_PLT) {"),
]


# Mutations that cannot change what the linker does, and why. They are still
# run: if a change ever makes one of them reachable it should turn up as a
# mutation that suddenly gets caught, rather than as a line nobody reads.
EQUIVALENT = {
    "a weak definition overrides a strong one":
        "two strong definitions are reported and the branch abandoned above, "
        "so a weak one never meets a strong one at this test",
    "a linker-defined name beats the program's own":
        "resolveSymbolAddresses asks about a real definition before it asks "
        "about this flag, so a name the program defines never reads it",
}


SUITES = ("test/link/reader_test.py", "test/link/linker_test.py",
          "test/link/dynamic_test.py", "test/checkers/link.py")


def configure(build):
    if os.path.exists(os.path.join(ROOT, build, "CMakeCache.txt")):
        return True
    result = subprocess.run(["cmake", "-B", build, "-S", ".",
                             "-DEDUCC_RUN_CHECKERS=OFF"],
                            cwd=ROOT, capture_output=True, text=True)
    return result.returncode == 0


# A mutation can make the linker write a program that never returns - a stub
# whose GOT slot points back at its own jump is a two-instruction loop - so
# every wait here is bounded. Without this the sweep has no upper bound on its
# own running time, and the first such mutation stops it for good.
BUILD_TIMEOUT = 600
SUITE_TIMEOUT = 900


def rebuild(build):
    jobs = str(os.cpu_count() or 4)
    try:
        result = subprocess.run(["cmake", "--build", build, "-j" + jobs],
                                cwd=ROOT, capture_output=True, text=True,
                                timeout=BUILD_TIMEOUT)
    except subprocess.TimeoutExpired:
        return False, "the build did not finish in %d seconds" % BUILD_TIMEOUT

    return result.returncode == 0, result.stderr


def runSuites(build):
    """Caught as soon as one suite says so, and it says which - a mutation only
    the corpus walk sees is a different fact from one the unit tests see."""
    binary = os.path.join(build, "bin/main")

    for suite in SUITES:
        command = [sys.executable, suite, binary]
        if suite.startswith("test/checkers/"):
            command.append("test/testData/codegen")

        try:
            result = subprocess.run(command, cwd=ROOT, capture_output=True,
                                    text=True, timeout=SUITE_TIMEOUT)
        except subprocess.TimeoutExpired:
            # A suite that hangs has still noticed: the mutation produced
            # something that does not finish, which is a way of being wrong.
            return False, os.path.basename(suite) + " (timed out)"

        # 2 is a skip - the host has no oracle for that suite, which is not an
        # answer either way and must not be read as one.
        if result.returncode not in (0, 2):
            return False, os.path.basename(suite)

    return True, None


def main():
    # A sweep takes tens of minutes and its whole value while it runs is the
    # line per mutation; block-buffered into a file, none of them appear until
    # it is over and there is nothing left to watch.
    try:
        sys.stdout.reconfigure(line_buffering=True)
    except AttributeError:
        pass

    args = sys.argv[1:]
    if not args or args[0].startswith("-"):
        print(__doc__)
        return 2

    build = args[0]
    only = None
    listOnly = False

    rest = args[1:]
    while rest:
        if rest[0] == "--only" and len(rest) > 1:
            only = rest[1]
            rest = rest[2:]
        elif rest[0] == "--list":
            listOnly = True
            rest = rest[1:]
        elif rest[0] == "--restore":
            # After a run that was killed outright: put the source back and
            # take the backup away, which is all the finally below would have
            # done had it been reached.
            backup = SOURCE + ".mutate-backup"
            if not os.path.exists(backup):
                print("nothing to restore: there is no %s" % backup)
                return 2
            shutil.copy(backup, SOURCE)
            os.unlink(backup)
            print("restored %s" % SOURCE)
            return 0
        else:
            print(__doc__)
            return 2

    selected = [m for m in MUTATIONS if only is None or only in m[0]]

    if listOnly:
        for name, _, _ in selected:
            print(name)
        return 0

    if not selected:
        print("no mutation matches '%s'" % only)
        return 2

    backup = SOURCE + ".mutate-backup"
    if os.path.exists(backup):
        # An earlier run did not get to put the file back, which means the
        # source on disk may still carry a mutation. Saying which file to copy
        # where beats guessing on the user's behalf: they may have edited it
        # since, and this cannot tell.
        print("a backup from an earlier run is still at\n    %s\n"
              "which means %s may still be mutated. Restore it with\n"
              "    cp %s %s\n"
              "or, if you know the source is the one you want, delete the "
              "backup." % (backup, SOURCE, backup, SOURCE))
        return 2

    shutil.copy(SOURCE, backup)

    # SIGKILL cannot be caught and is what leaves the backup behind for the
    # message above. Everything else can, and turning it into an exception is
    # what gets the restore below to run.
    def terminate(number, frame):
        raise KeyboardInterrupt("signal %d" % number)

    for number in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(number, terminate)

    with open(backup) as f:
        original = f.read()

    caught, missed, unusable, equivalent = [], [], [], []

    try:
        if not configure(build):
            print("could not configure %s" % build)
            return 2

        ok, err = rebuild(build)
        if not ok:
            print("the unmutated build already fails:\n" + err[-2000:])
            return 2

        ok, who = runSuites(build)
        if not ok:
            print("the unmutated tests already fail: " + str(who))
            return 2

        for name, before, after in selected:
            hits = original.count(before)
            if hits != 1:
                unusable.append("%s (pattern matches %d sites)" % (name, hits))
                print("UNUSABLE  %s" % name)
                continue

            with open(SOURCE, "w") as f:
                f.write(original.replace(before, after))

            ok, err = rebuild(build)
            if not ok:
                unusable.append("%s (does not compile)" % name)
                print("UNUSABLE  %s" % name)
                continue

            ok, who = runSuites(build)
            if ok and name in EQUIVALENT:
                equivalent.append(name)
                print("equivalent %s" % name)
            elif ok:
                missed.append(name)
                print("MISSED    %s" % name)
            else:
                caught.append(name)
                print("caught    %-56s by %s" % (name, who))
    except KeyboardInterrupt:
        print("\ninterrupted; putting %s back" % os.path.basename(SOURCE))
    finally:
        shutil.copy(backup, SOURCE)
        with open(SOURCE) as f:
            restored = f.read() == original

        if restored:
            os.unlink(backup)
        else:
            print("\n*** %s was NOT restored; the original is at %s"
                  % (SOURCE, backup))

        rebuild(build)

    print("\n== %d caught, %d missed, %d equivalent, %d unusable, of %d"
          % (len(caught), len(missed), len(equivalent), len(unusable),
             len(selected)))
    for name in missed:
        print("   MISSED: " + name)
    for name in equivalent:
        print("   equivalent: %s\n       %s" % (name, EQUIVALENT[name]))
    for name in unusable:
        print("   unusable: " + name)

    # A mutation listed as equivalent that a test now catches is good news, and
    # worth saying so that the list does not outlive its reason.
    for name in caught:
        if name in EQUIVALENT:
            print("   no longer equivalent, and now caught: " + name)

    # An unusable mutation is a check that silently stopped running, so it
    # fails the sweep as loudly as one nothing caught.
    return 1 if missed or unusable else 0


if __name__ == "__main__":
    sys.exit(main())
