# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

EduCC is an educational, from-scratch C compiler written in C (gnu99), targeting `x86_64` and (in progress) `riscv64`. It implements its own preprocessor, lexer, parser, semantic analysis, and native code generation, and can drive the system linker (`ld`) directly to produce ELF executables — no LLVM/GCC backend is used. The compiler is even able to compile itself (see `bootstrap.sh`).

## Build

CMake is authoritative (`CMakeLists.txt` + `cmake/`); there is no more Makefile.

```sh
cmake -B build -S .                                  # configure
cmake --build build -j$(nproc)                        # builds build/bin/main using the default host compiler
cmake -B build -S . -DCMAKE_C_COMPILER=clang           # build with a different host compiler
cmake -B build-asan -S . -DEDUCC_SANITIZE=ON            # a second, AddressSanitizer build (see below)
rm -rf build                                           # "clean" (no incremental clean target is defined)
```

Build artifacts go under `build/` (git-ignored), with the final binary at `build/bin/main`. Third-party sources are downloaded and unpacked into `.deps/` (also git-ignored) rather than into the build directory, so that `bootstrap.sh` throwing `build/` away between stages does not re-fetch them five times.

Linking requires Zydis (disassembler, used by the x86_64 backend's `-S`) and a real glibc/gcc toolchain on the host (`linkCrtPath`/`linkGccPath` in `src/link/link.c` locate `crt1.o`/`crtbegin.o` etc. under common Linux paths), plus `ld` itself for `-fuse-ld=external`, which is no longer the default. SDK headers used when compiling test/user programs live under `sdk/include`.

### The Zydis dependency (`cmake/Zydis.cmake`)

`disassemble()` in `src/x86_64/codegen_x86_64.c` renders `-S` output through Zydis. `cmake/Zydis.cmake` fetches the project's **amalgamated** release artifact (`zydis-amalgamated.tar.gz`, hash-pinned to v4.1.0) — one `Zydis.c` and one `Zydis.h` — and compiles it into a static `libzydis.a` with a hand-written rule rather than Zydis' own CMakeLists, for the same reason the build never hands third-party code to `CMAKE_C_COMPILER`: see Bootstrapping. Nothing is generated and nothing is patched.

Do not pass `ZYDIS_DISABLE_ENCODER` or the other `ZYDIS_DISABLE_*` switches: the amalgamated header honours them but the amalgamated `Zydis.c` still carries the bodies whose declarations they remove, so turning one off is a build error rather than a saving.

This replaced udis86, which was abandoned upstream in 2013, needed a Python-2 opcode-table generator and two local source patches to build at all, and — the reason it went — decoded correct bytes wrongly about one run in twenty, because `decode_prefixes()` read an uninitialized local as a REX prefix (roadmap step 26, `docs/ir-codegen-design.md` §10).

### The AddressSanitizer build (`-DEDUCC_SANITIZE=ON`)

`cmake -B build-asan -S . -DEDUCC_SANITIZE=ON && cmake --build build-asan -j$(nproc)` gives a second binary at `build-asan/bin/main` built with `-fsanitize=address`. Keep it working and reach for it early: the compiler is an arena allocator over a memory-mapped source buffer, so nothing it gets wrong about a lifetime shows up as a crash anywhere near the mistake. Several bugs in this tree were completely silent until a sanitized binary ran the same input — a use-after-free in the preprocessor's `#endif` handling that only aborted on one input in the whole corpus, a stale stack pointer kept as a hash-map key in the float constant cache, a dead 31-byte `memcpy` reading past a two-byte allocation.

`test/asan_sweep.sh` is the sweep, and should stay clean — it compiles every fixture and every one of EduCC's own sources (the deepest input it has) through both backends and, within the IR one, both allocators that keep state of their own, about 1100 compiles in 23 seconds, and exits nonzero on any sanitizer report:

```sh
test/asan_sweep.sh                      # defaults to build-asan/bin/main
test/asan_sweep.sh path/to/other/main
```

It prints a count of rejected inputs alongside the findings, and that count is part of the result: the parser corpus is full of inputs meant to be rejected, so "nothing compiled at all" and "nothing went wrong" would otherwise read the same.

The sweep only compiles, so it never reaches the linker; point the four linker suites at the same binary to cover that half (`python3 test/link/dynamic_test.py build-asan/bin/main`, and the same for `reader_test.py`, `linker_test.py` and `test/checkers/link.py`). They set `ASAN_OPTIONS=detect_leaks=0` for themselves, as `asan_sweep.sh` does and for the same reason: EduCC is an arena allocator that frees nothing it does not have to, so what LeakSanitizer reports at exit is the front end's arenas. Nothing reported this before `-fuse-ld=internal` existed only because every link `exec`ed `ld` and the process was gone before exit.

The option refuses to configure unless `CMAKE_C_COMPILER` is a real host compiler; EduCC cannot compile a sanitized version of itself, so a bootstrap needs it off (it is off by default).

### Bootstrapping

Since roadmap step 30 a bootstrap goes through the **IR backend**, and since step 37 through its graph-colouring register allocator — the script passes no flag either time and did not have to change. The compiler it produces is now 1.26 MB, against 1.41 MB from `-Xregalloc=linear` and 1.34 MB from `-legacy`, the first time the self-compiled binary has been the smallest of the three. (Step 41's jump relaxation brought the first two down from 1.29 MB and 1.45 MB; `-legacy` chooses its own jump displacements and is unchanged.) Given the same workload it runs 17% faster than the `-legacy`-built one and 6% faster than the linear-scan-built one, where before step 34 it was 1.6× *slower* than legacy. Five stages still take under 40 seconds. `-DCMAKE_C_FLAGS=-legacy` or `-DCMAKE_C_FLAGS=-Xregalloc=linear` bootstraps one of the others. Those three sizes were `ld`'s; since step 53 the in-house linker writes them and the default one is 1.29 MB, EduCC compiling *and* linking itself to a fixed point at stage 2.

`./bootstrap.sh` performs a multi-stage self-host build: configures+builds `main` with the host compiler, then repeatedly reconfigures with `-DCMAKE_C_COMPILER=<path to the previous stage's binary>` and rebuilds, diffing `sha1sum` of the resulting binaries to verify a fixed point. Use this to sanity-check changes that could affect self-compilation. It reconfigures `build/` with the host compiler again before it exits — CMake caches `CMAKE_C_COMPILER`, so without that a later plain `cmake -B build -S .` silently keeps whichever EduCC stage ran last, and every build and test run after it is self-compiled without saying so.

`./selfhost.sh` asks the other half of the question — not "does it reach a fixed point" but "is the compiler it produces any good". It builds stage 0 with the host compiler, stages 1 and 2 with EduCC, checks the two EduCC stages are byte-identical, and then runs the whole `ctest` suite *with stage 2*, which is the combination that found seven of this tree's shipped bugs and that nothing automated did before roadmap step 31. It builds under `build-selfhost/` and never touches `build/`. `--no-tests` stops after the fixed-point check; `EDUCC_SELFHOST_FLAGS=-legacy` asks the same of the other backend, and `EDUCC_SELFHOST_FLAGS=-fuse-ld=external` asks it of `ld` — EduCC linking itself, against an archive built by a different compiler, is the hardest link in the repo, is what M2 and M3 of the linker were accepted on, and is now what a plain run does.

One build-system-level workaround exists solely to keep this working, in `cmake/Zydis.cmake`: the vendored Zydis (above) is always built with a real compiler, never with the in-progress EduCC binary — EduCC can't yet parse arbitrary third-party C against real system headers well enough to compile it. (It *can* parse `Zydis.h`, which is what lets `-S` survive a bootstrap; the library's own 55k-line `Zydis.c` is a different question and has never been asked.)

There used to be a second one. `src/main.c` added a default include path `"sdk/include"` that EduCC resolved relative to its *current working directory*, which held for `make` (always invoked from the repo root) but not for CMake, which runs compile recipes from the build directory — so `CMakeLists.txt` bolted an absolute `-I` onto the `main` target whenever `CMAKE_C_COMPILER_ID` was empty. That is gone: the path is now resolved from the binary's own location, and the `EDUCC_BUILD_SDK_DIR` define below covers the build tree for every stage at once.

## Installing

`cmake --install build --prefix <dir>` gives a distributable tree — the binary as `<prefix>/bin/educc`, its headers as `<prefix>/lib/educc/include`. There is no `sudo make install` step in any workflow here; the build tree is what the tests and scripts use.

`sdkIncludePath()` in `src/main.c` is how a binary finds those headers, and it is the reason the compiler works from any directory rather than only from the repo root. It resolves, first hit wins:

1. `$EDUCC_SDK_DIR`, if set — the override, and how a test harness pins a specific copy.
2. `<prefix>/lib/educc/include` (also `lib64`, `share`), derived from `/proc/self/exe` by stripping the binary and `bin/`. Nothing absolute is recorded at install time, so an installed tree can be moved, tarred up or unpacked elsewhere and still resolves.
3. `EDUCC_BUILD_SDK_DIR` — the absolute source location, baked in by `CMakeLists.txt` for a built-but-never-installed binary. It has to be absolute: a relative hop up from `bin/` would be wrong for `selfhost.sh`, which builds two levels down. An installed copy never reaches this tier, because (2) hits first.

`-print-sdk-dir` reports which one won, which is the only way to tell an installed binary from a build one without an `strace`.

`install(PROGRAMS ... RENAME educc)` rather than renaming the target: `build/bin/main` stays `build/bin/main`, so `bootstrap.sh`, `selfhost.sh`, `ctest`, the checkers and every launch config keep working untouched. The install destination is a literal `lib`, not `CMAKE_INSTALL_LIBDIR` — these are the compiler's private resource files rather than system libraries, and a fixed name keeps the probe list in `sdkIncludePath()` short.

## Running the compiler

The CLI intentionally mimics a subset of GCC flags (`-o`, `-c`, `-I`, `-L`, `-l`, `-D`, `-E`, `-S`, `-march`, `-std`, `-O*`, `-W*` are accepted, many as no-ops).

Three of them reach the linker rather than being consumed: `-s`, `-Xlinker <arg>` and `-Wl,a,b,c` are collected and passed through by `runLinker` — or, by default, read by `linkInternal`, which understands `-s`, `-rpath`, `-soname`, `-e`, `--as-needed` and `-z`, and refuses anything else by name. Three are **refused by name** — `-static`, `-nostdlib`, `-pie` — because each decides which CRT objects and dynamic linker the link needs and `runLinker` spells those out for one layout only; forwarding one would produce a binary that links and does not run, and anything unrecognised is treated as an input file, so the error is the honest answer.

`-shared` was the fourth until roadmap step 51 and now has a layout of its own: no `crt1.o` and no `-dynamic-linker`, and the `S` variants of `crtbegin`/`crtend`. `-fPIC` (also `-fpic`, `-fPIE`, `-fpie`; `-fno-pic` and friends turn it back off) is what makes the code inside one legal — a global's address is loaded out of the GOT rather than computed, since another object may define the same name. Only the IR backend implements it; `-fPIC` together with `-legacy` is an error rather than a no-op, because the legacy backend reaches a global through a rip-relative memory operand built at the use site and has no register to put the GOT load's result in. `-std` is read for exactly one bit: a strict `-std=c...` (not `gnu...`) predefines `__STRICT_ANSI__`, which is what makes glibc's headers hide `readlink`, `strdup` and `usleep`; the default dialect is gnu99 and does not.

EduCC-specific flags (see `src/main.c`):

- `-legacy` — compile through the old direct-from-AST code generator instead of the AST→IR pipeline (see Architecture). The IR pipeline is the **default** since roadmap step 30; `-legacy` is how the other one is still reachable, and the whole file goes to whichever is chosen. There is no per-function fallback either way, so a construct the IR backend cannot build aborts the compiler rather than being handed to the legacy one.
- `-experimental` — accepted and does nothing. It used to select the IR pipeline, which is now the default; it is kept because it is in every script, launch configuration and shell history this compiler has.
- `-march x86_64|riscv64` — select target (default `x86_64`). x86_64 is the only one either backend generates working code for: the legacy backend's riscv64 half is unfinished and is not going to be finished, a second target being the IR backend's job. `-march riscv64` still reaches that unfinished code so it stays exercisable, and asserts on anything real.
- `-astDump <file>` / `-astCanonDump <file>` — dump the AST before/after canonicalization.
- `-irDump <file>` — dump the IR; also emits `cfg.dot` for the CFG. Optionally `-irDump:phase[,phase...] <file>` (phases: `initial`, `ssa`, `scp`, `gvn`, `dce`, `mir`, `isel`, `ra`) snapshots the IR immediately after each named pass in `translateFunction()` (`src/ir/ast2ir.c`) instead of only the fully-processed result — e.g. `-irDump:ssa` captures IR right after `buildSSA`, unaffected by whatever `scp`/`gvn`/`dce` do afterwards. The last three are the odd ones out: they dump the `MachineFunction` rather than the IR, in machine-IR form — `mir` as stage 0 leaves it (CFG, frame, phi copies), `isel` once instruction selection has filled the blocks in, `ra` once register allocation has replaced every virtual register.
- `-Xregalloc=chaitin|linear|trivial` — which register allocator stage 2 runs. `chaitin` is the default since roadmap step 37 (stage 2C, `src/ir/codegen/regalloc_colour.c`): Chaitin-Briggs graph colouring with coalescing, which produces the best code of the three and takes the longest to do it. `linear` is the linear scan that was the default from step 34 to step 37 (stage 2B, `src/ir/codegen/regalloc_linear.c`); `trivial` is the original spill-everything allocator (stage 2A, `src/ir/codegen/regalloc.c`). The two that are not the default are kept as differential oracles — three allocators over one corpus have to produce programs with identical observable behaviour, which is what the `codegen_regalloc_linear` and `codegen_regalloc_trivial` CTest entries check. The `-irDump:ra` dump names which one ran on an `Allocator:` line, because a checker reading it cannot otherwise tell. Every allocator also gets its own golden `ra` baselines and its own run of the structural checkers — see Tests.
- `-skipCodegen` — stop after parsing/sema (used heavily by parser tests).
- `-oneline` — non-verbose output (used by the test runner).
- `-fuse-ld=external` — `exec` the system `ld` instead of linking in this process with `src/link/` (see Architecture). The in-house linker is the **default** since roadmap step 53, having passed the parity sweep; `external`, `bfd`, `lld` and every other spelling all mean the `ld` that `runLinker` builds a command line for, and `-fuse-ld=internal` still says the default out loud. What the default produces is a dynamic non-PIE executable against the distribution's own libc, or a shared object under `-shared`, and it is the same objects either way, which is what keeps the whole codegen corpus a differential test of both (`codegen_external_ld`). It refuses a `-Wl,` flag it cannot honour by name rather than dropping it. `ld` is kept for one cycle as that oracle and then goes, along with `runLinker`'s argv arithmetic and the `mkdtemp` temp-object dance.
- `-Xlink-read <file>...` / `-Xlink-static <arg>...` / `-Xlink-dynamic <arg>...` — the in-house linker driven directly, with none of the CRT objects a compile puts around it. The first reads each input — relocatable object, shared object or archive — and prints what it made of it. The other two link exactly what they are named, statically and against shared libraries respectively; both take `-o`, `-e`, `-s`, `-L`, `-l`, `-rpath`, `-shared` and `-soname`, and `-dynamic-linker` names the interpreter. All three are modes of their own, taken before the flag parser runs, because their arguments are paths of shapes the driver otherwise rejects — and because linking a bare object that carries its own `_start` is what a test can assert the exit code of.
- `-logtokens`, `-memstat` — debug tracing / arena memory statistics.
- `-trace` — the IR passes' running commentary on stdout (`ast2ir`, `ssa`, `cp`, `dce`, `dominators`, and the vector helper in `utils.c`). Off by default, because stdout is also where `-E` writes preprocessed source and `-S` the disassembly. Add new tracing through `trace()` (`include/utils.h`), not `printf`.

Example (matches `.dbg.config.json` / `.vimspector.json`):
```sh
build/bin/main -irDump hw.ir.txt ./test/testData/codegen/simple/gvn.c
```

## Tests

Tests are plain data-driven fixtures under `test/testData/{parser,pp,codegen,crossabi}` run via `test/testRunner.py` against a built `build/bin/main`. `cmake --build build && cd build && ctest --output-on-failure` runs the six passes over the codegen corpus, the cross-backend ABI suite, the IR-dump suites and the structural checkers in one shot (each wired up as its own CTest test with a timeout — see `CMakeLists.txt`'s `educc_add_test`, and Structural checkers below for the `checkers` label); `ctest --output-junit results.xml` produces a CI-friendly report. For scoping to a subdirectory or passing extra flags, invoke the runner directly, pointing `--compiler` at the built binary and `--working-dir` at a scratch directory for outputs:

```sh
# Parser/AST-dump tests (compares -astDump/-astCanonDump/stderr against *.txt/*.canon.txt/*.err)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/parser -m parser

# Preprocessor tests (compares -E output against *.expect)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/pp -m preprocessor

# Codegen tests (compiles+links+*runs* the binary, args optionally from a sibling *.args file)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/codegen -m codegen

# The same fixtures compiled through the old AST walker instead
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/codegen -m codegen --compiler-flag=-legacy

# Cross-backend ABI tests (each fixture is two files, one compiled per backend, linked and run)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/crossabi -m crossabi

# IR-dump tests: one pass, against <name>.<phase>.txt. The 'ra' phase is the
# one that differs per register allocator, so it names the allocator on both
# sides - the flag it compiles with, and the baseline suffix it compares to.
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/ir/pipeline -m ir --ir-phase gvn
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/ir/pipeline -m ir --ir-phase ra \
        --compiler-flag=-Xregalloc=trivial --baseline-tag trivial
```

Notes on the runner's behavior:
- `-p/--test-path` can be repeated and can point at a single subdirectory (e.g. `test/testData/codegen/tinyc`) to scope to one test group.
- If an expected file (`*.txt`, `*.err`, `*.canon.txt`, `*.expect`) doesn't exist yet, the test **fails** rather than silently passing — pass `--update-baselines` to (re)write every baseline from current actual output instead of comparing, then review with `git diff` before committing. There's no silent auto-baselining anymore.
- Codegen tests actually execute the compiled binary and check its exit code; a `<name>.args` file (one arg-string per line) runs the binary once per line. A nonzero compiler exit code fails the test; a zero exit with warnings on stderr does not (see the exit-code contract note in Architecture below).
- Nonzero process exit at the runner level is the failed-test count; on failure it also lists every failed test's path. Directory walks are sorted, so run order (and failure order) is deterministic across machines.
- `--compiler-flag` (repeatable) prepends a flag to every compiler invocation, which is how one set of fixtures is run against a second configuration rather than being copied. The `codegen_legacy` CTest entry uses it for `-legacy`, `codegen_pic` for `-fPIC`, and `codegen_external_ld` for `-fuse-ld=external`. It has to be spelled with `=` (`--compiler-flag=-legacy`), since argparse will not take a value beginning with `-` as a separate word.
- `--baseline-tag <tag>` qualifies an `ir`-mode baseline's name, giving `<name>.<phase>.<tag>.txt`. Use it whenever the same fixtures go through a second configuration whose dump legitimately differs — which today means the `ra` phase, since two register allocators do not produce the same code and are not meant to. `ir_ra_chaitin`, `ir_ra_linear` and `ir_ra_trivial` are the same 57 fixtures against `*.ra.chaitin.txt`, `*.ra.linear.txt` and `*.ra.trivial.txt`. A single shared baseline could only ever record whichever allocator happened to be the default, and a change of default would silently rewrite the file rather than fail.
- A test can be **muted** by placing a `<name>.muted` file next to its `<name>.c`, with the reason as the file's contents (printed whenever the test runs). This is for known-broken fixtures kept in the repo so a bug stays reproducible: the test still runs and reports, but its failures don't count towards the exit code. If a muted test passes every check, the summary flags it under `MUTED TESTS THAT NOW PASS` so the stale marker gets deleted — loudly, but without failing the run. `--update-baselines` deliberately skips muted tests rather than baking their known-wrong output into a golden file. A `<name>.muted.legacy` / `<name>.muted.ir` sibling mutes in that one configuration only, for a bug that belongs to one backend and not the other (`codegen/bugs/float_to_bool.c`) — without it the fixture is reported as a muted test that now passes on every run of the configuration that gets it right.
- `crossabi` mode is the odd one out: a fixture there is a **pair**, `<name>.c` and `<name>.partner.c`, compiled with a *different* backend each, linked into one binary and run — then swapped over and run again. The partner half is skipped by the directory walk rather than run as a fixture of its own, and `--compiler-flag` is deliberately ignored in this mode, since it names both backends itself. Every other suite runs one backend at a time, so this is the only place their ABI agreement is tested rather than assumed; it was tested by accident until roadmap step 18 removed the per-function fallback that arranged it. Since roadmap step 49 the two backends deliberately **disagree** about SysV's aggregate classification: the IR backend splits an aggregate of at most sixteen bytes into eightbytes and passes each in a register of its own class, the way the ABI says and gcc does, and the legacy backend passes every composite wider than a word on the stack. Both crossabi aggregate fixtures — `sse_aggregate.c` and `aggregates.c` — are muted for it and fail in both directions, with the reason in their `.muted` files; the runner flags either the day the legacy side catches up. What that mute costs is the cross-backend cover `aggregates.c` gave for hidden return pointers and large structs, since its small shapes fail first. Note what this suite cannot say either way: both backends agreeing proves nothing about whether they agree with *the platform*, which is what tinycc's `abitest` is for (see the benchmarks below).
- A `<name>.ir` or `<name>.legacy` sibling (reason as its contents) marks a fixture that belongs to that backend alone, the other one not being going to be taught to agree — a VLA in a loop, whose storage the legacy backend never gives back (`codegen/experimental/vla_in_loop.c`, whose directory keeps its historical name), or a fixture reading one local through a pointer to the next, which the IR backend is right to disagree with and which gcc fails too (`codegen/my/adjacent_locals.c`). Such a test is **skipped** in every other configuration and listed under `Skipped (belong to the other backend)`; it is not muted, because muting is for a bug someone intends to fix and a muted test is flagged the day it starts passing.

### Linker unit tests (`test/link/`)

The only tests here that are not fixtures, because the linker's inputs are not source files. `reader_test.py` (CTest entry `link_reader`), `linker_test.py` (`link_static`) and `dynamic_test.py` (`link_dynamic`) build their inputs **byte by byte** — an ELF object, a GNU archive, machine code — rather than checking one in or taking the host's: a golden dump of the distro's `crt1.o` would pin the distro and not the reader, which is the mistake `test/testData/pp/stdlib/emptyDef.c` had to be rescued from. Synthesizing is also the only way to reach what the host has no example of: a `/SYM64/` armap, a tentative definition of two different sizes, a relocation whose target does not fit in its field.

```sh
python3 test/link/reader_test.py build/bin/main
python3 test/link/linker_test.py build/bin/main
python3 test/link/dynamic_test.py build/bin/main
```

`linker_test.py` links every fixture and then **runs** it, so what it asserts is the exit code of a program the linker produced; one group compiles a real file with EduCC first, which is the only part that sees the relocations the compiler actually emits. `nm`/`ar`/`ld` are used as second opinions where present and skipped where not.

`dynamic_test.py` asks the other question — not whether the bytes are in the right place but whether ld.so will load them — so its oracle is the system's own loader, and its library is the host's real libc, plus (since step 52) shared objects this linker wrote itself. There is no way to synthesize a useful one: a shared object that means anything has a `.dynsym`, a `.hash`, a `.dynamic` and a loader willing to map it, which is a second linker's worth of Python. Everything on this side of the boundary is still synthesized, so a fixture is four instructions and an exit code. It skips (exit 2) where there is no C runtime to link against.

Four of its groups reach past libc. One builds a shared object out of EduCC's own `-fPIC` objects and links a program against it with `-L`/`-l`/`-Wl,-rpath`, which is the only place the sizes being copied are ones the test chose rather than ones glibc happens to use; it runs twice, once with the library built in-house and once with `ld`, so the same executable tables have to come out of either producer. The second is the `-shared` group, and what it asserts that no structural check can is **interposition**: the program deliberately defines a name the library also defines and calls itself through, so a library that bound that call to its own definition satisfies every check about `ET_DYN`, `DT_SONAME`, `DT_RELACOUNT` and the export table and still returns 1. The third links against that library and never calls it, which is the whole of `--no-as-needed`: `DT_NEEDED` has to name it anyway, and `-Wl,--as-needed` has to make it disappear, with `ld` asked the same two questions so the claim is that the linkers agree rather than that one of them is self-consistent. The fourth runs `readelf` over everything linked: the Python reader and the linker are both this project's understanding of ELF and could share a misunderstanding, and binutils cannot share it — it complains when a table does not add up rather than only when the file will not load.

#### Breaking it on purpose (`test/link/mutate.py`)

A green suite says nothing about whether it would go red. `mutate.py` answers that: it replaces one line of `src/link/link.c` with a plausible wrong version, rebuilds, runs all four linker suites, and reports any mutation nothing caught as a hole in the tests.

```sh
python3 test/link/mutate.py build-mut              # everything, ~15 min
python3 test/link/mutate.py build-mut --only PLT   # one group
python3 test/link/mutate.py build-mut --list       # just name them
```

**Not a CTest entry, and deliberately so**: it rebuilds the compiler once per mutation and edits a tracked source file to do it, and a full sweep takes most of an hour. It restores the file in a `finally` — which a `SIGTERM` or a Ctrl-C now reaches too — verifies that it did, and says so loudly if it ever could not. Only a `SIGKILL` can get past that, and then `src/link/link.c.mutate-backup` is left behind holding the original: the next run refuses to start until it is dealt with, and `--restore` deals with it. **Do not edit `link.c` while a sweep is running** — the sweep rewrites the file from its own copy on every mutation, so an edit made underneath it is lost, and a killed run can leave the mutation in place. It is also the reason to trust the suites at all: **75 mutations, 73 caught, 2 equivalent, 0 missed** as of the last full run, and getting there took seven real gaps out of the tests — a pointer whose upper four bytes were already zero, a tentative definition that would have been aligned by luck, an `sh_info` of zero making its own check vacuous, an armap whose order let one pass resolve a chain meant to need two, an archive shadowing a library that nothing checked, no test at all for which of two versions of a name a library meant, and nothing noticing a library named twice on one command line — which `-lc` is on every link — becoming two `DT_NEEDED` entries. The two survivors are unreachable by construction and are declared as such in an `EQUIVALENT` table with the reason, so they do not fail the sweep — and if a later test does catch one, it says so rather than letting the entry outlive its justification. The mutations are literal text, so they **rot** the moment the lines they name are edited; one whose pattern no longer matches is reported as unusable and fails the sweep, because an unusable mutation is a check that silently stopped running.

### Structural checkers (`test/checkers/`)

A golden baseline says the output has not changed, not that it was ever right. The checkers say it is right: each walks the corpus, asks the compiler for one of its dumps, and asserts an invariant over it. They all take the same two arguments and exit nonzero on a finding:

```sh
python3 test/checkers/<checker>.py build/bin/main test/testData/codegen
```

Anything beginning with `-` in between is passed through to the compiler on every invocation, which is how one checker covers a second configuration rather than being copied — and which is how these run once per register allocator:

```sh
python3 test/checkers/allocation.py build/bin/main -Xregalloc=trivial test/testData/codegen
```

| checker | what it asserts |
| --- | --- |
| `phi_destruction.py` | symbolically execute each edge's copy sequence; the phi's register ends up holding that edge's value |
| `frame_layout.py` | no two frame objects overlap, every offset is aligned to its own alignment, locals inside the reported size, incoming arguments at +16 |
| `selection.py` | over `-irDump:isel`: nothing read before written, two-address form intact, emitted branches agree with the CFG's successors |
| `allocation.py` | over `-irDump:ra`: backward liveness over physical registers and spill slots, no scratch register live across a block boundary, spill widths, and five more |
| `allocation_widths.py` | forward walk over `-irDump:ra` tracking how many bytes of each register were actually written; a spill may not store more than that |
| `call_alignment.py` | simulate `rsp` through `-S`; 16-byte aligned at every call, restored by the epilogue, and moved by a constant only in the prologue |
| `emission_objdump.py` | differential: compare the machine IR against GNU `objdump` as multisets of (mnemonic, register set). The only check that can see stage 3 at all |
| `disasm_stable.py` | `-S` output is byte-identical across runs of the same command, under both backends, with and without ASLR |
| `jump_relaxation.py` | over `-S`: a jump written with a four-byte displacement really needs four, and no nop pads one that was shortened |
| `pic.py` | compile with `-fPIC -c` and hand the object to `ld -shared`, whose refusal names the symbol; plus a round trip that builds a shared object and a program against it and runs it |
| `link.py` | link each fixture with `-fuse-ld=internal` and assert what ld.so is entitled to assume: PT_PHDR inside a PT_LOAD, PT_INTERP's path, `.plt`/`.rela.plt`/`.got.plt` sizes agreeing, no JUMP_SLOT aimed at the loader's reserved slots, `.got.plt` outside PT_GNU_RELRO, every dynamic symbol findable through `.hash` |

`link.py` is the one exception to what a checker reads: it walks the corpus the same way but inspects the linked executable rather than a dump, and it is registered once rather than per allocator, since which allocator filled the `.text` in changes nothing about the tables a loader consults. Its invariants live in `test/link/dynamic_test.py`, which asserts them over two hand-built programs; the checker runs the same ones over every fixture.

`pic.py`'s three rounds now build the shared object and the program that loads it in-house, which is the one place `-shared` output meets the system loader over a corpus's worth of objects rather than one hand-built library; it is registered a fourth time as `checker_pic_external`, with `-fuse-ld=external` passed through, as the control that says what `ld` makes of the same objects. The walk part asks `ld` either way and is the same answer.

They share `test/checkers/corpus.py` (argument parsing, the corpus walk, the dump invocation, the report), and each is a CTest entry per register allocator — `checker_allocation_linear`, `checker_allocation_trivial` — labelled `checkers`. Without that second round the allocator that is not the default is checked by nothing structural, and `allocation.py`'s scratch-register invariants, which belong to stage 2A alone, would never run at all:

```sh
ctest --test-dir build                # the fixture suites, the IR dumps, and the checkers
ctest --test-dir build -L checkers    # just the checkers, once per allocator (~50s)
ctest --test-dir build -LE checkers   # everything but (~12s)
cmake -B build -S . -DEDUCC_RUN_CHECKERS=OFF   # do not register them at all
```

A checker that cannot answer skips rather than fails (`SKIP_RETURN_CODE 2`) — `emission_objdump.py` needs GNU `objdump` as its oracle and exits 2 without it, and `pic.py` needs `ld`. **They rot.** Every one of them was written against the dumps of its day and had to be repaired when it was checked in — a `<clobbers ...>` annotation moved a bracket, index registers made a load look two-address, `movsx.8/4` gave widths a second number, a sparse switch put a conditional branch mid-block. Read a finding as a question about the checker first.

### CI (`.github/workflows/ci.yml`)

Three jobs, each of which is a command that already existed and that somebody had to remember to type:

| job | what it runs |
| --- | --- |
| `test (gcc)`, `test (clang)` | `cmake` + `ctest`, once per host compiler |
| `selfhost` | `./selfhost.sh` — the compiler builds itself twice, the two stages must be byte-identical, and the whole suite is then run by the self-hosted binary |
| `asan` | a `-DEDUCC_SANITIZE=ON` build, then `test/asan_sweep.sh` |

The runner has to stay x86_64 — that is the only target either backend generates working code for, and the codegen fixtures link and *run* what they compile. `.deps/` is cached on a key derived from `cmake/Zydis.cmake`, so the pinned Zydis tarball is fetched once rather than on every job, and an upstream outage does not take the workflow with it.

## Benchmarks (`test/bench/`)

The test suite says the code is correct. It says nothing about whether it is any good, which is the entire question a register allocator exists to answer. `test/bench/bench.py` asks it:

```sh
python3 test/bench/bench.py                                  # everything, ~3 min
python3 test/bench/bench.py --runs 5 --json before.json      # save a run
python3 test/bench/bench.py --compare before.json            # and diff against it
python3 test/bench/bench.py --filter nbody,sort --no-static  # scope it down
```

`test/bench/programs/*.c` are nine self-contained programs picked for what they do to an allocator, not for coverage: `nbody` and `matmul` keep more doubles live than there are volatile xmm registers, `crc` and `sieve` are tight integer loops, `binarytrees` is recursion over `malloc` so every call site is a place the allocator has to get caller-saved registers out of the way, `interp` is switch dispatch with everything live across every arm, `strings` is byte-at-a-time pointer walks, `sort` is quicksort's recursion and heapsort's sift-down. `blockcopy` is the exception that is about instruction selection rather than allocation — struct assignment at four sizes either side of `X86_UNROLLED_COPY_LIMIT`, which is what decides whether a copy is unrolled into load/store pairs or handed to `rep movsb`. Each prints a checksum, and each is compiled by every configuration — `-legacy`, `-Xregalloc=trivial`, `-Xregalloc=linear`, `-Xregalloc=chaitin`, plus the host `cc` at `-O0` and `-O2` — so the reference doubles as an oracle: the driver reports any configuration whose output disagrees with the others, which is how `codegen/bugs/narrow_store_result.c` was found.

Three things get measured, because they are three different questions: how fast the *generated* code runs, how fast EduCC itself compiles (over its own front-end and IR sources), and how big `.text` comes out. `--no-static` turns off a fourth — spill slots and machine instructions counted straight out of `-irDump:ra`, which says *what the allocator did* rather than how long it took.

Nothing here is wired into `ctest`: it measures wall-clock time, it takes minutes, and a loaded machine will lie to it. Run it deliberately, before and after a backend change, with `--compare`. As of step 36 — run time and `.text` totalled over the eight programs, fastest of three runs; the static counts over those plus EduCC's own sources:

| | legacy | trivial | linear | chaitin | cc -O0 | cc -O2 |
| --- | --- | --- | --- | --- | --- | --- |
| run time | 3.94 | 6.58 | 2.25 | **2.16** | 3.10 | 1.40 |
| `.text` | 12.0K | 23.5K | 11.1K | **8.5K** | 8.8K | 12.4K |
| spill slots | — | 14098 | 1844 | **1473** | — | — |
| instructions | — | 89272 | 53510 | **38913** | — | — |

The linear scan is 3× the trivial allocator and beats the host compiler at `-O0`; the colouring allocator emits a quarter fewer instructions and a quarter less code again. Neither is free, and they cost in the same place: compiling EduCC's own sources, `chaitin` takes 0.49s and `linear` 0.39s against `trivial`'s 0.34s and `-legacy`'s 0.20s, which is the allocator doing work the other two do not. One benchmark (`strings`) is still slower under `chaitin` than under `linear` despite executing fewer instructions — the one known cost of making it the default; `docs/ir-codegen-design.md` §10 has how far that has been chased.

### tinycc (`test/bench/bench_tcc.py`)

The eight programs above were written for this compiler. `bench_tcc.py` asks the same questions of one that was not: it clones [tinycc](https://github.com/TinyCC/tinycc) (`dev`), builds it once per configuration, runs **tinycc's own test suite** against each resulting `tcc`, and then measures that `tcc` compiling tinycc and compiling EduCC.

```sh
python3 test/bench/bench_tcc.py                  # clone, build, test, bench (~8 min)
python3 test/bench/bench_tcc.py --no-tests       # skip tinycc's suite
python3 test/bench/bench_tcc.py --only chaitin   # one configuration
python3 test/bench/bench_tcc.py --keep           # reuse the checkout and builds
```

Everything lives under `build-tcc/` (git-ignored) — the checkout included, so nothing is written outside the repo. Not wired into `ctest`: it clones from the network and times things.

Two of its columns are controls rather than measurements. **tinycc compiled by tinycc** separates "EduCC got this wrong" from "this is what building tinycc with a compiler that is not gcc looks like" — `config.h`'s `GCC_MAJOR` is 0 for anything configure does not recognise, and it gates a block of `tcctest.c`'s expected output. And because **tcc is deterministic**, the object files two of these tccs emit for one input have to be byte-identical; a mismatch is a miscompilation and it names the file. That is what found `tccgen.c` — the isel bug in §10 of the design doc — before any test did.

It is also the parity sweep the in-house linker was accepted on (M4), and the `external` column is the third control: same compiler, same objects, `ld` back in `src/link/`'s place. Fifteen sections of somebody else's test suite is the widest thing this repo links — a shared object in `dlltest`, and `tcc -run` resolving symbols out of what the link recorded as needed, which is what `--no-as-needed` turned out to be load-bearing for.

Nothing here is an oracle EduCC's own suite already has: `crossabi` compares EduCC to EduCC, so a classifier both backends share is invisible to it, where tinycc's `abitest` compares EduCC to a third party. It has found six bugs nothing else did — an x87 conversion that stored its operand before defining it, SysV's aggregate classification, two front-end bugs that give a file-scope object more than one address (which only a single-translation-unit build, `memtest` with `ONE_SOURCE=1`, can expose), no `-fPIC`, and a poison value in relocated fields that only a linker generous enough to read them could see. `dlltest` is also the only place anything links an EduCC object with something other than `ld`, which is what made that last one visible at all.

As of the last full run all three IR configurations pass **15 of tinycc's 15** test sections, the same as gcc at every level and as tcc itself — and so does `external`, so the in-house link and `ld`'s agree over the whole suite, on identical objects and identical `.text`; the `tcc` the in-house one writes is 332K against `ld`'s 336K. `-legacy` passes 13: `abitest` is the deliberate aggregate-ABI disagreement above, and `dlltest` needs `-fPIC`, which the legacy backend refuses.

## Architecture

Pipeline, driven from `src/main.c` → `compileFile()` in `src/parser.c`:

1. **Preprocessing** (`src/pp.c`, `src/lexer.c`) — full macro expansion, `#include`, conditionals, `#pragma once`. `-E` stops here.
2. **Parsing** (`src/parser.c`, ~3.6k lines) builds an AST (`src/tree.c`, `include/tree.h`) while interleaving **semantic analysis** (`src/sema.c`) — types, scopes, symbol resolution happen during parsing, not as a separate pass. `AstFile` / `AstTranslationUnit` is the top-level unit.
3. Diagnostics (`src/diagnostics.c`, `include/diagnostics.h`) accumulate through parsing/sema; the diagnostic catalog itself is data-driven via `include/diagnosticList.h` (`DIAGNOSTIC_DEF(severity, category, ID, format)` X-macro consumed with `#define DIAGNOSTIC_DEF ... #include "diagnosticList.h"`). Add new diagnostics there, not ad hoc. Exit-code contract: `Configuration.hadError` (set in `compileFile()` whenever `printDiagnostics()` reports an error) makes `main()` return `1`; a clean compile with only warnings still returns `0` — don't conflate the two when scripting around the compiler.
4. After a clean parse, compilation forks into **two independent backend pipelines** selected by `-legacy`:
   - **Legacy pipeline** (`-legacy`): `cannonizeAstFile()` (`src/cannonization.c`) lowers/normalizes the AST (e.g. desugaring composite ops), then `generateCodeForFile()` (`src/codegen_common.c`) walks the canonicalized AST directly to machine code via an arch-specific vtable (`ArchCodegen{generateFunction, generateVaribale}`, see `include/codegen.h`), implemented per-arch in `src/x86_64/codegen_x86_64.c` + `instructions_x86_64.c` and `src/riscv64/codegen_riscv64.c` + `instructions_riscv64.c`. Output is assembled straight into an in-memory ELF (`src/elf.c`, `include/_elf.h`) — there's no external assembler.
   - **IR pipeline** (default since roadmap step 30): `translateAstToIr()` (`src/ir/ast2ir.c`) lowers AST to a CFG-based SSA-capable IR (`include/ir/ir.h`, `include/ir/instructionList.h`). Passes: `buildSSA` (`src/ir/ssa.c`), `buildDominatorInfo` (`src/ir/dominators.c`), `gvn` — global value numbering (`src/ir/gvn.c`), `scp`/`cp` — (sparse) constant propagation (`src/ir/cp.c`, `src/ir/evaluator.c`), `dce` — dead code elimination (`src/ir/dce.c`). `IrFunction.phases` bitflags (`initalIr`/`ssa`/`cp_1`/`gvn`) track which passes a function has been through. The optimized IR then goes through a four-stage machine backend under `src/ir/codegen/`: `prepareMachineFunction` (stage 0 — critical edges, phi destruction, frame layout, `src/ir/codegen/prepare.c`), `selectInstructions` (stage 1, `isel.c` + `src/x86_64/isel_x86_64.c`), `allocateRegisters` (stage 2 — Chaitin-Briggs graph colouring in `regalloc_colour.c` over the liveness in `liveness.c`, with the linear scan of `regalloc_linear.c` and the spill-everything allocator of `regalloc.c` reachable by `-Xregalloc=`; the liveness and the spiller in `spill.c` belong to none of the three, the colouring allocator reading the same dataflow as live sets where the linear scan reads it as intervals) and emission (stage 3, `src/x86_64/emit_x86_64.c`), producing the same `GeneratedFunction` the legacy backend does.

     **The backend is picked for the whole file, not per function.** It used to be per function — the IR backend took what it could and `generateFunction` got the rest — which is what let the pipeline be exercised before it was finished; it now covers the codegen corpus, and that machinery is gone, so anything it cannot build aborts where the gap is instead of silently producing the other backend's code. `docs/ir-codegen-design.md` is the design document (§6.21 for the removal) and tracks what is done and what is not.
5. `linkInternal()` in `src/main.c` hands the input list to `src/link/` and writes the final executable in this process; `runLinker()` beside it shells out to the system `ld` with the same list instead when `-fuse-ld=external` is given. The CRT and gcc directories both need are probed once by `linkCrtPath()`/`linkGccPath()` in `src/link/link.c`, so the two paths cannot disagree about where `crt1.o` is.

### The in-house linker (`src/link/`)

Takes step 5 away from `ld`, and has been what runs by default since roadmap step 53 — `-fuse-ld=external` is how `ld` is still reachable. Two halves, and they are separate models on purpose: `elfread.c` reads a mapped file and owns none of it, bounds-checking every offset the file states about itself; `link.c` decides what every name means and writes the executable. `src/elf.c`, the object *writer*, is a third model and stays that way: converging it with either would mean a section that is both a growable buffer and a window onto someone else's bytes.

`link.c` runs one pass over a fixed set of inputs. It reads them in command-line order — relocatable objects, archives (a member is taken only when it resolves something still undefined, repeating until a pass takes nothing), shared objects, and GNU `ld` scripts, which is what `-lc` actually finds on a glibc system. Then it resolves every global name, groups the input sections into output ones, invents the sections a loader needs that nobody supplied, gives all of them addresses in `PT_LOAD` segments, and patches the bytes. Nothing is decided before layout that layout could change: the addresses relocations need do not exist until then.

Three decisions about **executables** are worth knowing about, because each of them is a fork the ABI leaves open:

- **A library's function is reached through a stub this link wrote, and a library's data is copied into this executable's `.bss`.** Both give the symbol an address the executable owns, which is the only thing a non-PIE can do with an address it has to write into an instruction. The copy is a `COPY` relocation, and it is not optional — EduCC's own sources reference `stdout` and `stderr`.
- **Every GOT slot is filled at link time and carries no dynamic relocation.** In a non-PIE nothing moves, so a slot for a library function holds its stub — the same address the program uses for that function everywhere else — and one for a local symbol holds the symbol. `GLOB_DAT` never arises here; only `JUMP_SLOT` and `COPY` do.
- **A shared object's `.gnu.version` is read**, so an unversioned reference binds to the default definition rather than to whichever entry came first. glibc exports `__libc_start_main` twice, and the two are not the same function.

#### `-shared` (roadmap step 52)

`ET_DYN` output turns every one of those the other way up, because nothing in a shared object has a final address and any name it defines may be answered for by somebody else. The image base is zero and each address is really an offset; there is no `PT_INTERP` and no `PT_PHDR`, and `e_entry` is left at zero.

- **A name with default visibility is preemptible, and is treated as such even where this object defines it.** A call to one goes through a PLT stub rather than straight at the local definition, a GOT slot for one gets `GLOB_DAT` naming it, and an absolute reference gets a dynamic `R_X86_64_64`. Everything else — a local, a hidden definition, a linker-defined name — gets `RELATIVE` and the answer the link already knew as its addend. `RELATIVE` entries are written first and counted by `DT_RELACOUNT`, which is only a saving if they are.
- **An executable advertises the names a shared object it loaded also knows**, whether the library references them or defines them itself, and that is the other half of the same thing: `ld.so` searches the executable first, so a program's own definition of a name the library calls only wins by being in a table the loader looks at. This is why `.dynsym` in an executable is not just its imports.
- **A relocation that cannot be made position-independent is refused by name**, the way `ld` refuses it: a PC-relative reference to a preemptible symbol, a 32-bit absolute reference to anything but `SHN_ABS`, or an address the loader would have to write into a read-only section. `DT_TEXTREL` is not emitted, so the honest answer is the error and `-fPIC`.

Symbol versioning beyond reading it (`.gnu.version_r` in the output), `.gnu.hash`, `.eh_frame_hdr`/`PT_GNU_EH_FRAME`, TLS and `-static` are all deliberately absent — see the plan's frontier table. Non-allocated sections are dropped rather than copied through, so an object compiled with `-g` by another compiler loses its `.debug_*` on the way; nothing EduCC emits is affected, since `-g` is a no-op here.

#### What a library is needed for (roadmap step 53)

One decision was left open since M2, and the parity sweep is what settled it. `ld`'s default is `--no-as-needed`: a shared object *named on the command line* gets a `DT_NEEDED` entry whether or not the link took anything from it, because what a program looks up out of one at run time is invisible here — tinycc's `-run` resolves `sin` through `dlsym` after nothing in `tcc` itself called it, and a link that dropped `libm` as unused turns that into `undefined symbol 'sin'`. So a shared object is recorded when it is read, `--as-needed`/`--no-as-needed` toggle that positionally in the input list, and `AS_NEEDED (...)` inside a linker script means it for exactly that group — which is what keeps `ld-linux-x86-64.so.2` out of `DT_NEEDED` when `-lc` resolves to a script naming it. The driver spells `-lgcc_s` the way `runLinker` does, wrapped in the pair. `-lc` is named twice, so one soname is loaded once: without that the entry appears twice.

What is not there yet: dropping `ld`. The `execvp` path stays one cycle as the differential oracle (`codegen_external_ld`, `checker_pic_external`, `bench_tcc.py`'s `external` column) before it, `runLinker`'s argv arithmetic and the `mkdtemp` temp-object dance go.

### Supporting infrastructure

- **Memory**: everything uses a custom arena/heap allocator (`src/memory.c`, `include/mem.h`) — `heapAllocate`/`releaseHeap` for general allocation, `createArena`/`areanAllocate`/`releaseArena` for phase-scoped bulk allocation (e.g. one arena per generated function/codegen context). Prefer arena allocation matching the existing context struct (`ParserContext`, `GenerationContext`, `IrContext`) over raw `malloc` when adding IR/codegen state.
- **Constant evaluation**: `src/evaluate.c` (AST-level, e.g. for static initializers/`#if`) vs `src/ir/evaluator.c` (IR-level, used by the `cp`/`scp` pass) are separate.
- Tree/IR dumping utilities (`src/treeDump.c`, `src/ir/irdump.c`) are the primary debugging tool for both pipelines — reach for `-astDump`/`-astCanonDump`/`-irDump` before adding printf debugging.

## Code comments
Keep comments brief — one short line max. Only comment on non-obvious logic;
skip comments that just restate what the code does. Do not add docstrings/
header comments unless asked.
