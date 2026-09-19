cmake_minimum_required(VERSION 3.16)
project(EduCC C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON) # gnu99, not c99

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
set(CMAKE_C_FLAGS_DEBUG "-g -O0")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# A separate build directory with -fsanitize=address. Worth its own option
# rather than hand-passed flags because the compiler is an arena allocator over
# a memory-mapped source buffer: nothing it gets wrong about lifetimes shows up
# as a crash where the mistake is, and several of the bugs found in this tree
# were silent until a sanitized binary ran the same input. Off by default -
# EduCC cannot compile its own sanitized self, so a bootstrap needs it off.
option(EDUCC_SANITIZE "Build with AddressSanitizer (host compiler only)" OFF)
if(EDUCC_SANITIZE)
  if(NOT CMAKE_C_COMPILER_ID)
    message(FATAL_ERROR "EDUCC_SANITIZE needs a real host compiler; CMAKE_C_COMPILER "
                        "('${CMAKE_C_COMPILER}') is not one.")
  endif()
  add_compile_options(-fsanitize=address -fno-omit-frame-pointer -g)
  add_link_options(-fsanitize=address)
endif()

# Used by the test suite (test/testRunner.py).
find_package(Python3 REQUIRED COMPONENTS Interpreter)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Zydis)

set(SOURCES
  src/main.c
  src/tokens.c
  src/utils.c
  src/sema.c
  src/treeDump.c
  src/parser.c
  src/tree.c
  src/memory.c
  src/diagnostics.c
  src/evaluate.c
  src/cannonization.c
  src/elf.c
  src/lexer.c
  src/pp.c
  src/codegen_common.c
  src/float_image.c
  src/x86_64/instructions_x86_64.c
  src/x86_64/codegen_x86_64.c
  src/x86_64/target_x86_64.c
  src/x86_64/isel_x86_64.c
  src/riscv64/instructions_riscv64.c
  src/riscv64/target_riscv64.c
  src/riscv64/isel_riscv64.c
  src/ir/ir.c
  src/ir/target.c
  src/ir/irdump.c
  src/ir/ast2ir.c
  src/ir/dominators.c
  src/ir/ssa.c
  src/ir/dce.c
  src/ir/cp.c
  src/ir/evaluator.c
  src/ir/gvn.c
  src/ir/softfloat.c
  src/ir/codegen/machine.c
  src/ir/codegen/mirdump.c
  src/ir/codegen/prepare.c
  src/ir/codegen/isel.c
  src/ir/codegen/loops.c
  src/ir/codegen/liveness.c
  src/ir/codegen/spill.c
  src/ir/codegen/regalloc.c
  src/ir/codegen/regalloc_linear.c
  src/ir/codegen/regalloc_colour.c
  src/x86_64/emit_x86_64.c
  src/riscv64/emit_riscv64.c
  src/link/elfread.c
  src/link/link.c
  src/link/target_link.c
)

add_executable(main ${SOURCES})
target_include_directories(main PRIVATE include)
target_link_libraries(main PRIVATE zydis m)

# Where a built-but-not-installed binary finds its own stddef.h/stdarg.h
# shims. sdkIncludePath() in src/main.c probes the installed layout relative
# to the binary first and only falls back to this, so an installed copy never
# reads it. Absolute, because it has to be right for a build directory at any
# depth or location - selfhost.sh builds two levels down, and this same define
# is what lets a previous stage's EduCC compile the next one.
target_compile_definitions(main PRIVATE
                           EDUCC_BUILD_SDK_DIR="${CMAKE_SOURCE_DIR}/sdk/include")

# `cmake --install build --prefix <dir>` produces a distributable tree: the
# binary as `educc`, and its headers where sdkIncludePath() looks first.
# Nothing absolute is recorded in it, so the result can be moved or tarred up
# and still resolves. A literal `lib` rather than CMAKE_INSTALL_LIBDIR - these
# are the compiler's private resource files, not system libraries, and a fixed
# name keeps the probe list in src/main.c short.
include(GNUInstallDirs)
install(PROGRAMS "$<TARGET_FILE:main>"
        DESTINATION "${CMAKE_INSTALL_BINDIR}"
        RENAME educc)
install(DIRECTORY sdk/include/
        DESTINATION lib/educc/include)

# `ctest` in the build directory is the single "run everything" entrypoint:
# it runs all three suites, applies a per-suite timeout (a hung test binary
# no longer wedges the whole run indefinitely), and can emit CI-friendly
# reports via `ctest --output-junit results.xml`. Pass extra flags to
# testRunner.py itself (e.g. --update-baselines) with `ctest -- <flags>`
# won't work here since CTest doesn't forward trailing args to the test
# command - re-run test/testRunner.py directly for that instead.
include(CTest)
if(BUILD_TESTING)
  set(EDUCC_TEST_OUTPUT_DIR "${CMAKE_BINARY_DIR}/test-output")

  # Neither qemu nor a riscv64 sysroot is a build dependency; what needs them
  # says so and is left unregistered otherwise. On a cross build the toolchain
  # file has already found qemu and insisted on it.
  find_program(QEMU_RISCV64 qemu-riscv64)

  # What the suites are handed as "the compiler". Normally the binary itself;
  # on a cross build it cannot be exec'd here, and every harness in this tree
  # takes a compiler *path* rather than a command - so the emulator goes into a
  # wrapper script and that is the path. This is what lets a riscv64-hosted
  # EduCC run the whole suite (selfhost.sh --target riscv64): the fixtures it
  # compiles are x86-64 and run natively, which is the self-host question asked
  # from the other side.
  if(CMAKE_CROSSCOMPILING AND CMAKE_CROSSCOMPILING_EMULATOR)
    set(EDUCC_TEST_COMPILER "${CMAKE_BINARY_DIR}/bin/run-main")
    string(REPLACE ";" " " EDUCC_EMULATOR_COMMAND "${CMAKE_CROSSCOMPILING_EMULATOR}")
    file(WRITE "${EDUCC_TEST_COMPILER}"
         "#!/bin/sh\nexec ${EDUCC_EMULATOR_COMMAND} \"${CMAKE_BINARY_DIR}/bin/main\" \"$@\"\n")
    file(CHMOD "${EDUCC_TEST_COMPILER}" PERMISSIONS
         OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE
         WORLD_READ WORLD_EXECUTE)
  else()
    set(EDUCC_TEST_COMPILER "$<TARGET_FILE:main>")
  endif()

  # And on a cross build every suite is the *target's*, not this machine's.
  # qemu's '-L' prefix redirects the filesystem as well as the loader, so a
  # riscv64-hosted compiler reads the riscv64 sysroot's headers whatever it is
  # asked to target - '#error unsupported ABI' out of bits/wordsize.h is the
  # first thing a fixture including anything hits. The honest run is therefore
  # the riscv64 configuration throughout: compiled for riscv64, run under qemu.
  #
  # Timeouts go up with it. Every compile is emulated, and the corpus that
  # takes 40 seconds natively takes several hundred.
  if(CMAKE_CROSSCOMPILING AND CMAKE_SYSTEM_PROCESSOR STREQUAL "riscv64")
    set(EDUCC_CROSS_RISCV64 ON)
    set(EDUCC_SUITE_TARGET
        --compiler-flag=-march --compiler-flag=riscv64
        --run-prefix=${QEMU_RISCV64}
        --run-prefix=-L --run-prefix=${EDUCC_RISCV64_SYSROOT})
    set(EDUCC_CHECKER_TARGET "-march=riscv64")
    set(EDUCC_SUITE_TIMEOUT 1800)
  else()
    set(EDUCC_SUITE_TARGET "")
    set(EDUCC_CHECKER_TARGET "")
    set(EDUCC_SUITE_TIMEOUT 300)
  endif()

  function(educc_add_test name testDataSubdir mode timeoutSeconds)
    # Optional 5th argument: the IR phase an 'ir'-mode suite snapshots
    # (forwarded as testRunner.py's --ir-phase; defaults to 'ssa' there).
    # Anything after it is passed to the runner verbatim, which is how a suite
    # asks for a non-default compiler configuration - see codegen_experimental.
    set(extraArgs "")
    if(ARGC GREATER 4)
      set(extraArgs --ir-phase ${ARGV4})
    endif()
    if(ARGC GREATER 5)
      list(SUBLIST ARGV 5 -1 passThrough)
      list(APPEND extraArgs ${passThrough})
    endif()
    # -p is deliberately relative (WORKING_DIRECTORY is the repo root,
    # matching how the fixtures' baselines were generated by hand): the
    # AST/AstCanon dumps embed the test file path exactly as passed on the
    # command line, so an absolute -p here would make every "FILE ..." line
    # mismatch its baseline.
    add_test(NAME ${name}
      COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/testRunner.py"
              -c "${EDUCC_TEST_COMPILER}"
              -wd "${EDUCC_TEST_OUTPUT_DIR}/${name}"
              -p "test/testData/${testDataSubdir}"
              -m ${mode}
              ${extraArgs}
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    )
    # A muted test (see '<name>.muted' in test/testRunner.py) does not count
    # towards the runner's exit code, which is deliberately still "the number
    # of failed tests" for anything scripting around it. That leaves one gap:
    # a muted test that starts passing prints its notice and, since the run
    # succeeded, ctest shows nothing - so the stale marker would sit there
    # forever, which defeats the point of keeping the fixture. Failing the
    # ctest entry on that one line closes it without giving the runner a
    # second, ambiguous failure mode; --output-on-failure then prints exactly
    # which marker file to delete.
    set_tests_properties(${name} PROPERTIES
      TIMEOUT ${timeoutSeconds}
      FAIL_REGULAR_EXPRESSION "MUTED TESTS THAT NOW PASS")
  endfunction()

  educc_add_test(parser parser parser 60)
  educc_add_test(preprocessor pp preprocessor 60)
  # Compiles, links, and runs every fixture, so it gets more headroom. No flag:
  # the compiler's default backend is the IR pipeline since step 30 of
  # docs/ir-codegen-design.md, and its default register allocator the graph
  # colouring of stage 2C since step 37, so this is the one that has to stay
  # green.
  educc_add_test(codegen codegen codegen ${EDUCC_SUITE_TIMEOUT} ssa ${EDUCC_SUITE_TARGET})
  # Every one of those fixtures a second time, through the old AST walker. It
  # was the other way round until step 30 - the IR backend was the second
  # configuration and had to ask for itself - and before step 16 it was the
  # 'experimental' subdirectory alone, the fixtures written to stay inside what
  # instruction selection covered.
  #
  # Kept because the legacy backend is still what a bootstrap of an unfinished
  # IR change falls back to by hand, and because two independent code
  # generators disagreeing is how several bugs in this tree were found. It is
  # not the reference: where the two disagree the IR backend has been right
  # more often than not, and three fixtures are muted here and nowhere else.
  #
  # The 'ssa' argument is there only because the pass-through slot comes after
  # the --ir-phase one; it is the runner's default and a codegen-mode run
  # ignores it. --compiler-flag is spelled with '=' because argparse will not
  # take a value beginning with '-' as a separate word.
  #
  # riscv64 has no legacy backend, so a cross build leaves this one out rather
  # than registering a suite whose every fixture is refused by name.
  if(NOT EDUCC_CROSS_RISCV64)
    educc_add_test(codegen_legacy codegen codegen 300 ssa --compiler-flag=-legacy)
  endif()
  # And a third time through stage 2A, the spill-everything allocator, and a
  # fourth through stage 2B, the linear scan that replaced it as the default in
  # step 34 and was itself replaced by the colouring allocator in step 37. This
  # is what makes them the differential oracles section 7 keeps them for rather
  # than dead code behind a flag: three allocators over one set of fixtures have
  # to produce programs with identical observable behaviour, however differently
  # they arrive at it, so the whole suite becomes a test of whichever one is
  # being changed.
  educc_add_test(codegen_regalloc_trivial codegen codegen ${EDUCC_SUITE_TIMEOUT} ssa
                 --compiler-flag=-Xregalloc=trivial ${EDUCC_SUITE_TARGET})
  educc_add_test(codegen_regalloc_linear codegen codegen ${EDUCC_SUITE_TIMEOUT} ssa
                 --compiler-flag=-Xregalloc=linear ${EDUCC_SUITE_TARGET})
  # And a fifth time position-independent (step 51). '-fPIC' changes how every
  # global is reached, which is one selection rule and every fixture that has a
  # global - so the cheapest honest test of it is the corpus it already has,
  # compiled the other way and run. checker_pic asks whether the objects could
  # be a shared library; this asks whether they still compute the right answers.
  # '-fPIC' is refused by name on riscv64, so a cross build leaves it out too.
  if(NOT EDUCC_CROSS_RISCV64)
    educc_add_test(codegen_pic codegen codegen 300 ssa --compiler-flag=-fPIC)
  endif()

  # And a sixth time for the *other target*. This is the only suite that says
  # riscv64 code computes the right answers rather than merely assembling, and
  # it is the same corpus for the same reason -fPIC reuses it: a target is a
  # selector and a descriptor, and the cheapest honest test of one is every
  # fixture there already is, compiled for it and run.
  #
  # Registered only when qemu and a riscv64 sysroot are installed, since
  # neither is a build dependency: qemu to run what comes out, the sysroot to
  # link against. EduCC links it itself - the cross driver was a crutch until
  # roadmap step 59's R5, and is gone. A fixture that is about one target and
  # no other carries a '<name>.x86_64' or '<name>.riscv64' marker and is
  # skipped here or there, the way '.ir' and '.legacy' already scope one to a
  # backend.
  #
  # A cross build has no use for it: 'codegen' is already that suite there, and
  # registering this would run the same 173 fixtures a second time under qemu.
  if(EDUCC_CROSS_RISCV64)
    # nothing to add - 'codegen' above is the riscv64 suite
  elseif(QEMU_RISCV64 AND EXISTS /usr/riscv64-linux-gnu/usr/lib/crt1.o)
    educc_add_test(codegen_riscv64 codegen codegen 600 ssa
                   --compiler-flag=-march --compiler-flag=riscv64
                   --run-prefix=${QEMU_RISCV64}
                   --run-prefix=-L --run-prefix=/usr/riscv64-linux-gnu)
  else()
    message(STATUS "riscv64 codegen suite not registered: need qemu-riscv64 and a riscv64 sysroot")
  endif()
  # The two backends inside one link: each fixture is a pair of files compiled
  # with a different backend each, linked and run, then swapped over. Every
  # other suite runs one backend at a time, so their ABI agreement is otherwise
  # only assumed - it was tested by accident until step 18 removed the per
  # function fallback that arranged it (docs/ir-codegen-design.md section 9).
  #
  # This and every IR-dump suite below it are x86-64's, and a cross build
  # leaves the lot out. crossabi's premise is two backends meeting in one link
  # and riscv64 has one, the way roadmap step 59's R4 always said. The dumps
  # are a different reason and a debt rather than a fact: every one of them,
  # not just the three phases R6 predicted, records a target's answers, so they
  # need baselines of their own under '--baseline-tag riscv64' before stage 2
  # can be asked about them.
  if(NOT EDUCC_CROSS_RISCV64)
  educc_add_test(crossabi crossabi crossabi 120)
  # Compares '-irDump:ssa' output against golden dumps, isolating buildSSA
  # from scp/gvn/dce (see test/testRunner.py's runIrTest).
  educc_add_test(ir_ssa ir/ssa ir 60 ssa)
  # The same fixtures one pass earlier: '-irDump:scp' is the IR the gvn pass
  # is handed, so this suite both covers sparse constant propagation (which
  # had no fixtures of its own) and keeps the gvn baselines honest about what
  # they inherited rather than folded themselves.
  educc_add_test(ir_scp ir/pipeline ir 60 scp)
  # Compares '-irDump:gvn' output (IR right after the gvn pass, before dce
  # sweeps the replaced instructions) against golden dumps.
  educc_add_test(ir_gvn ir/pipeline ir 60 gvn)
  # The same fixtures one pass later: '-irDump:dce' is what survives once dce
  # sweeps the instructions gvn superseded, so the two suites read as a
  # before/after pair over one shared set of .c files - ir_gvn pins down what
  # gvn rewrote, ir_dce that the leftovers actually go away.
  educc_add_test(ir_dce ir/pipeline ir 60 dce)
  # Not an IR phase but the machine IR built from what ir_dce leaves behind:
  # '-irDump:mir' dumps the MachineFunction, so this suite pins down the CFG
  # the backend is handed and, as selection lands, what it selects into it.
  educc_add_test(ir_mir ir/pipeline ir 60 mir)
  # The same MachineFunction one stage later: '-irDump:isel' is what stage 1
  # selected into those blocks. Split from ir_mir rather than folded into it so
  # that the two read as a before/after pair - ir_mir pins down the CFG, the
  # frame and the phi copies, ir_isel the instructions - and so a change to
  # selection cannot silently rewrite the baselines for stage 0.
  educc_add_test(ir_isel ir/pipeline ir 60 isel)
  # And one stage later again: '-irDump:ra' is the same function with every
  # virtual register replaced by a real one, which is what stage 3 will be
  # handed. Same reason for the split - stage 2 rewrites operands throughout,
  # so folding it into ir_isel would let an allocation change quietly rewrite
  # the baselines that pin down selection.
  #
  # One entry per register allocator, each against its own '.ra.<alloc>.txt'
  # baselines (--baseline-tag). Two allocators over one set of fixtures do not
  # produce the same dump and are not meant to, so a shared baseline could only
  # ever record whichever one happened to be the default - and a change of
  # default would silently rewrite the file rather than fail. Every allocator
  # names itself on both sides: '-Xregalloc=' on the command line and the
  # 'Allocator:' line in the dump.
  educc_add_test(ir_ra_chaitin ir/pipeline ir 60 ra
                 --compiler-flag=-Xregalloc=chaitin --baseline-tag chaitin)
  educc_add_test(ir_ra_linear ir/pipeline ir 60 ra
                 --compiler-flag=-Xregalloc=linear --baseline-tag linear)
  educc_add_test(ir_ra_trivial ir/pipeline ir 60 ra
                 --compiler-flag=-Xregalloc=trivial --baseline-tag trivial)
  endif() # NOT EDUCC_CROSS_RISCV64

  # The three linker unit suites synthesize x86-64 inputs byte by byte - an
  # object, an archive, machine code - so what they assert is this linker's
  # reading of *that* machine, whoever compiled the linker. Teaching all three
  # to synthesize riscv64 is a second target's worth of hand-assembled code and
  # is deliberately not done (roadmap step 59's R5): the riscv64 cover is
  # checker_link over real executables, which is a better oracle anyway.
  if(NOT EDUCC_CROSS_RISCV64)

  # Unit tests for the linker's ELF reader. Every input is synthesized byte by
  # byte inside the test rather than checked in or taken from the host: a
  # golden dump of the distro's crt1.o would pin the distro and not the reader,
  # which is the mistake test/testData/pp/stdlib/emptyDef.c had to be rescued
  # from. Synthesizing is also the only way to reach a COMMON symbol, a
  # /SYM64/ armap, and every malformed input - which has to produce a
  # diagnostic rather than a signal. A second half cross-checks the reader
  # against nm and ar over whatever real objects the machine has, and says so
  # when they are absent instead of failing.
  add_test(NAME link_reader
    COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/link/reader_test.py"
            "${EDUCC_TEST_COMPILER}"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )
  set_tests_properties(link_reader PROPERTIES TIMEOUT 120 SKIP_RETURN_CODE 2)

  # Unit tests for the static linker. Same synthesized inputs, machine code
  # included, and every fixture is linked and then *run*: what is asserted is
  # the exit code of a program this linker produced, so a relocation that lands
  # four bytes off is a wrong answer rather than a diff. One half compiles a
  # real file with EduCC, which is the only part that sees the relocations the
  # compiler actually emits; another asserts the shape of the output, and is
  # what test/checkers/link.py will grow out of.
  add_test(NAME link_static
    COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/link/linker_test.py"
            "${EDUCC_TEST_COMPILER}"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )
  set_tests_properties(link_static PROPERTIES TIMEOUT 120 SKIP_RETURN_CODE 2)

  # And the dynamic half, whose oracle is the system's own loader: a PLT stub
  # six bytes out or a hash chain that cannot find a name is not a diff, it is
  # a program that does not start. The inputs are still synthesized, but the
  # library they link against is the host's real libc - there is no way to
  # synthesize a useful one - so it skips where there is no C runtime.
  add_test(NAME link_dynamic
    COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/link/dynamic_test.py"
            "${EDUCC_TEST_COMPILER}"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )
  set_tests_properties(link_dynamic PROPERTIES TIMEOUT 120 SKIP_RETURN_CODE 2)

  endif() # NOT EDUCC_CROSS_RISCV64

  # riscv64's ABI against a third party. Every other suite compares EduCC to
  # EduCC - crossabi compares its two backends, and riscv64 has one - so a rule
  # both halves of a call get wrong is a rule nothing notices. This builds each
  # fixture pair three ways, one of them all-gcc, and diffs what they print.
  # It is what the plan's Risk 4 asks for, and it caught a sixteen-byte struct
  # holding a long double being passed by reference where LP64D passes it in a
  # register pair. Skips without the cross toolchain, which is not a build
  # dependency.
  add_test(NAME abi_riscv64
    COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/abi/cross_toolchain_test.py"
            "${EDUCC_TEST_COMPILER}"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )
  set_tests_properties(abi_riscv64 PROPERTIES TIMEOUT 300 SKIP_RETURN_CODE 2)

  # The structural checkers (test/checkers/, docs/ir-codegen-design.md section
  # 9). A golden baseline says the output has not changed; these say it is
  # right, which is a different question and the only one that catches a bug
  # that has always been there. They are one CTest entry each, labelled
  # 'checkers', so they can be run alone with `ctest -L checkers` or left out
  # with `ctest -LE checkers`.
  #
  # On by default because the whole set costs about ten seconds - they walk the
  # corpus once per phase they read, which is a few hundred compiler runs, not
  # the hours the note in section 9 used to assume. -DEDUCC_RUN_CHECKERS=OFF
  # takes them out entirely.
  #
  # SKIP_RETURN_CODE catches a checker that cannot answer rather than one that
  # found something: emission_objdump needs GNU objdump as its oracle and exits
  # 2 without it, which is a skip, not a failure.
  #
  # Registered once per register allocator: a checker takes the compiler's
  # flags after the compiler path, and what stage 2 produces is exactly what
  # these invariants are about. Without the second round the allocator that is
  # not the default is checked by nothing structural - and allocation.py's
  # scratch-register invariants, which belong to stage 2A alone, would never
  # run at all.
  option(EDUCC_RUN_CHECKERS "Register test/checkers/ as CTest tests" ON)
  if(EDUCC_RUN_CHECKERS)
    set(EDUCC_CHECKERS
        phi_destruction frame_layout selection allocation allocation_widths
        call_alignment emission_objdump disasm_stable jump_relaxation pic)

    # Every one of them is x86-64's, and a cross build registers none. link.py
    # below is the exception and the model: R5 gave it a target dimension, and
    # it is the only one that has one. What the other ten each need is a debt
    # recorded in roadmap step 59's R6, and it is specific rather than general:
    #
    #   frame_layout      asserts incoming arguments at +16, which is x86's
    #                     saved rbp plus return address. riscv64 keeps the
    #                     return address in 'ra' and the offset is 0 - which is
    #                     TargetDescriptor.incomingStackParamOffset, already
    #                     there to be read.
    #   selection         "nothing read before written" counts $a0..$a7 and the
    #                     hardwired $zero and $sp as reads of undefined
    #                     registers; it needs the target's argument registers
    #                     and its fixed ones.
    #   allocation,       assert a spill stores no more than was written, which
    #   allocation_widths is x86's answer. RV64 has no sub-registers, so a
    #                     spill saves the whole register by design
    #                     (TargetDescriptor.spillRegisterWidth).
    #   phi_destruction   one fixture, one finding: a phi whose value has no
    #                     vreg. Unlike the others this is not obviously the
    #                     checker's assumption and wants looking at.
    #   call_alignment,   read '-S', which riscv64 has no disassembler for yet.
    #   disasm_stable,    call_alignment is the worst of the four because it
    #   jump_relaxation   passes: with nothing to read it reports zero calls.
    #   emission_objdump  wants riscv64-linux-gnu-objdump as its oracle.
    #   pic               is '-fPIC', refused by name on riscv64.
    if(EDUCC_CROSS_RISCV64)
      set(EDUCC_CHECKERS "")
      message(STATUS "riscv64: only checker_link registered; the other ten "
                     "carry x86-64's invariants (roadmap step 59, R6)")
    endif()

    foreach(alloc chaitin linear trivial)
      foreach(checker ${EDUCC_CHECKERS})
        add_test(NAME checker_${checker}_${alloc}
          COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/checkers/${checker}.py"
                  "${EDUCC_TEST_COMPILER}" "-Xregalloc=${alloc}"
                  ${EDUCC_CHECKER_TARGET} "test/testData/codegen"
          WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
        )
        set_tests_properties(checker_${checker}_${alloc} PROPERTIES
          TIMEOUT ${EDUCC_SUITE_TIMEOUT}
          LABELS checkers
          SKIP_RETURN_CODE 2)
      endforeach()
    endforeach()

    # link.py is the exception to the round-per-allocator: it reads the linked
    # executable, and which allocator filled the .text in changes nothing about
    # the tables a loader consults. Registered once, with the same label.
    add_test(NAME checker_link
      COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/checkers/link.py"
              "${EDUCC_TEST_COMPILER}" ${EDUCC_CHECKER_TARGET} "test/testData/codegen"
      WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    )
    set_tests_properties(checker_link PROPERTIES
      TIMEOUT ${EDUCC_SUITE_TIMEOUT}
      LABELS checkers
      SKIP_RETURN_CODE 2)

    # And once more for the other machine. What it asserts is not that riscv64
    # code works - codegen_riscv64 runs it - but that the tables ld.so reads
    # before any of it runs add up, which is a different half of the linker and
    # the only one with a second target's answers in it. A cross build needs no
    # second round: checker_link above is already that one.
    if(NOT EDUCC_CROSS_RISCV64)
      add_test(NAME checker_link_riscv64
        COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/test/checkers/link.py"
                "${EDUCC_TEST_COMPILER}" "-march=riscv64" "test/testData/codegen"
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
      )
      set_tests_properties(checker_link_riscv64 PROPERTIES
        TIMEOUT 300
        LABELS checkers
        SKIP_RETURN_CODE 2)
    endif()
  endif()
endif()
