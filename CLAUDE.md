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

Linking requires Zydis (disassembler, used by the x86_64 backend's `-S`) and a real `ld`/glibc/gcc toolchain on the host (`main.c`'s `runLinker`/`gccLibPath` locate `crt1.o`/`crtbegin.o` etc. under common Linux paths). SDK headers used when compiling test/user programs live under `sdk/include`.

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

The option refuses to configure unless `CMAKE_C_COMPILER` is a real host compiler; EduCC cannot compile a sanitized version of itself, so a bootstrap needs it off (it is off by default).

### Bootstrapping

Since roadmap step 30 a bootstrap goes through the **IR backend**, that being the compiler's default — the script itself passes no backend flag and did not have to change. Since step 34 replaced the spill-everything allocator with a linear scan, the compiler it produces is 1.42 MB against a `-legacy`-built 1.31 MB (it was 1.9 MB) and runs about 20% *faster* than the `-legacy`-built one over the same sources, where it used to be 1.6× slower. Five stages still take under 40 seconds. Use `-DCMAKE_C_FLAGS=-legacy` to bootstrap the other one.

`./bootstrap.sh` performs a multi-stage self-host build: configures+builds `main` with the host compiler, then repeatedly reconfigures with `-DCMAKE_C_COMPILER=<path to the previous stage's binary>` and rebuilds, diffing `sha1sum` of the resulting binaries to verify a fixed point. Use this to sanity-check changes that could affect self-compilation. It reconfigures `build/` with the host compiler again before it exits — CMake caches `CMAKE_C_COMPILER`, so without that a later plain `cmake -B build -S .` silently keeps whichever EduCC stage ran last, and every build and test run after it is self-compiled without saying so.

`./selfhost.sh` asks the other half of the question — not "does it reach a fixed point" but "is the compiler it produces any good". It builds stage 0 with the host compiler, stages 1 and 2 with EduCC, checks the two EduCC stages are byte-identical, and then runs the whole `ctest` suite *with stage 2*, which is the combination that found seven of this tree's shipped bugs and that nothing automated did before roadmap step 31. It builds under `build-selfhost/` and never touches `build/`. `--no-tests` stops after the fixed-point check; `EDUCC_SELFHOST_FLAGS=-legacy` asks the same of the other backend.

Two build-system-level workarounds exist solely to keep this working, both in `CMakeLists.txt`/`cmake/Zydis.cmake`:
- The vendored Zydis (above) is always built with a real compiler, never with the in-progress EduCC binary — EduCC can't yet parse arbitrary third-party C against real system headers well enough to compile it. (It *can* parse `Zydis.h`, which is what lets `-S` survive a bootstrap; the library's own 55k-line `Zydis.c` is a different question and has never been asked.)
- `src/main.c` adds a default include path `"sdk/include"` that EduCC resolves relative to its *current working directory* at invocation time. That assumption holds for `make` (always invoked from the repo root) but not CMake, which always runs compile recipes from the build directory. When `CMAKE_C_COMPILER_ID` is empty (i.e. the compiler is an unrecognized EduCC binary, not gcc/clang), `CMakeLists.txt` adds an explicit absolute `-I<repo>/sdk/include` to the `main` target so EduCC can still find its `stddef.h`/`stdarg.h` shims. This is inert for normal gcc/clang builds.

## Running the compiler

The CLI intentionally mimics a subset of GCC flags (`-o`, `-c`, `-I`, `-L`, `-l`, `-D`, `-E`, `-S`, `-march`, `-std`, `-O*`, `-W*` are accepted, many as no-ops). EduCC-specific flags (see `src/main.c`):

- `-legacy` — compile through the old direct-from-AST code generator instead of the AST→IR pipeline (see Architecture). The IR pipeline is the **default** since roadmap step 30; `-legacy` is how the other one is still reachable, and the whole file goes to whichever is chosen. There is no per-function fallback either way, so a construct the IR backend cannot build aborts the compiler rather than being handed to the legacy one.
- `-experimental` — accepted and does nothing. It used to select the IR pipeline, which is now the default; it is kept because it is in every script, launch configuration and shell history this compiler has.
- `-march x86_64|riscv64` — select target (default `x86_64`). x86_64 is the only one either backend generates working code for: the legacy backend's riscv64 half is unfinished and is not going to be finished, a second target being the IR backend's job. `-march riscv64` still reaches that unfinished code so it stays exercisable, and asserts on anything real.
- `-astDump <file>` / `-astCanonDump <file>` — dump the AST before/after canonicalization.
- `-irDump <file>` — dump the IR; also emits `cfg.dot` for the CFG. Optionally `-irDump:phase[,phase...] <file>` (phases: `initial`, `ssa`, `scp`, `gvn`, `dce`, `mir`, `isel`, `ra`) snapshots the IR immediately after each named pass in `translateFunction()` (`src/ir/ast2ir.c`) instead of only the fully-processed result — e.g. `-irDump:ssa` captures IR right after `buildSSA`, unaffected by whatever `scp`/`gvn`/`dce` do afterwards. The last three are the odd ones out: they dump the `MachineFunction` rather than the IR, in machine-IR form — `mir` as stage 0 leaves it (CFG, frame, phi copies), `isel` once instruction selection has filled the blocks in, `ra` once register allocation has replaced every virtual register.
- `-Xregalloc=linear|trivial|chaitin` — which register allocator stage 2 runs. `linear` is the default (stage 2B, `src/ir/codegen/regalloc_linear.c`); `trivial` is the original spill-everything allocator (stage 2A, `src/ir/codegen/regalloc.c`); `chaitin` is Chaitin-Briggs graph colouring with coalescing (stage 2C, `src/ir/codegen/regalloc_colour.c`), which produces the best code of the three and takes the longest to do it. The two that are not the default are kept as differential oracles — three allocators over one corpus have to produce programs with identical observable behaviour, which is what the `codegen_regalloc_trivial` and `codegen_regalloc_chaitin` CTest entries check. The `-irDump:ra` dump names which one ran on an `Allocator:` line, because a checker reading it cannot otherwise tell. Every allocator also gets its own golden `ra` baselines and its own run of the structural checkers — see Tests.
- `-skipCodegen` — stop after parsing/sema (used heavily by parser tests).
- `-oneline` — non-verbose output (used by the test runner).
- `-logtokens`, `-memstat` — debug tracing / arena memory statistics.
- `-trace` — the IR passes' running commentary on stdout (`ast2ir`, `ssa`, `cp`, `dce`, `dominators`, and the vector helper in `utils.c`). Off by default, because stdout is also where `-E` writes preprocessed source and `-S` the disassembly. Add new tracing through `trace()` (`include/utils.h`), not `printf`.

Example (matches `.dbg.config.json` / `.vimspector.json`):
```sh
build/bin/main -irDump hw.ir.txt ./test/testData/codegen/simple/gvn.c
```

## Tests

Tests are plain data-driven fixtures under `test/testData/{parser,pp,codegen,crossabi}` run via `test/testRunner.py` against a built `build/bin/main`. `cmake --build build && cd build && ctest --output-on-failure` runs all five suites, the IR-dump suites and the structural checkers in one shot (each wired up as its own CTest test with a timeout — see `CMakeLists.txt`'s `educc_add_test`, and Structural checkers below for the `checkers` label); `ctest --output-junit results.xml` produces a CI-friendly report. For scoping to a subdirectory or passing extra flags, invoke the runner directly, pointing `--compiler` at the built binary and `--working-dir` at a scratch directory for outputs:

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
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/ir/gvn -m ir --ir-phase gvn
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/ir/gvn -m ir --ir-phase ra \
        --compiler-flag=-Xregalloc=trivial --baseline-tag trivial
```

Notes on the runner's behavior:
- `-p/--test-path` can be repeated and can point at a single subdirectory (e.g. `test/testData/codegen/tinyc`) to scope to one test group.
- If an expected file (`*.txt`, `*.err`, `*.canon.txt`, `*.expect`) doesn't exist yet, the test **fails** rather than silently passing — pass `--update-baselines` to (re)write every baseline from current actual output instead of comparing, then review with `git diff` before committing. There's no silent auto-baselining anymore.
- Codegen tests actually execute the compiled binary and check its exit code; a `<name>.args` file (one arg-string per line) runs the binary once per line. A nonzero compiler exit code fails the test; a zero exit with warnings on stderr does not (see the exit-code contract note in Architecture below).
- Nonzero process exit at the runner level is the failed-test count; on failure it also lists every failed test's path. Directory walks are sorted, so run order (and failure order) is deterministic across machines.
- `--compiler-flag` (repeatable) prepends a flag to every compiler invocation, which is how one set of fixtures is run against a second configuration rather than being copied. The `codegen_legacy` CTest entry uses it for `-legacy`. It has to be spelled with `=` (`--compiler-flag=-legacy`), since argparse will not take a value beginning with `-` as a separate word.
- `--baseline-tag <tag>` qualifies an `ir`-mode baseline's name, giving `<name>.<phase>.<tag>.txt`. Use it whenever the same fixtures go through a second configuration whose dump legitimately differs — which today means the `ra` phase, since two register allocators do not produce the same code and are not meant to. `ir_ra_linear`, `ir_ra_trivial` and `ir_ra_chaitin` are the same 55 fixtures against `*.ra.linear.txt`, `*.ra.trivial.txt` and `*.ra.chaitin.txt`. A single shared baseline could only ever record whichever allocator happened to be the default, and a change of default would silently rewrite the file rather than fail.
- A test can be **muted** by placing a `<name>.muted` file next to its `<name>.c`, with the reason as the file's contents (printed whenever the test runs). This is for known-broken fixtures kept in the repo so a bug stays reproducible: the test still runs and reports, but its failures don't count towards the exit code. If a muted test passes every check, the summary flags it under `MUTED TESTS THAT NOW PASS` so the stale marker gets deleted — loudly, but without failing the run. `--update-baselines` deliberately skips muted tests rather than baking their known-wrong output into a golden file. A `<name>.muted.legacy` / `<name>.muted.ir` sibling mutes in that one configuration only, for a bug that belongs to one backend and not the other (`codegen/bugs/float_to_bool.c`) — without it the fixture is reported as a muted test that now passes on every run of the configuration that gets it right.
- `crossabi` mode is the odd one out: a fixture there is a **pair**, `<name>.c` and `<name>.partner.c`, compiled with a *different* backend each, linked into one binary and run — then swapped over and run again. The partner half is skipped by the directory walk rather than run as a fixture of its own, and `--compiler-flag` is deliberately ignored in this mode, since it names both backends itself. Every other suite runs one backend at a time, so this is the only place their ABI agreement is tested rather than assumed; it was tested by accident until roadmap step 18 removed the per-function fallback that arranged it.
- A `<name>.ir` or `<name>.legacy` sibling (reason as its contents) marks a fixture that belongs to that backend alone, the other one not being going to be taught to agree — a VLA in a loop, whose storage the legacy backend never gives back (`codegen/experimental/vla_in_loop.c`, whose directory keeps its historical name), or a fixture reading one local through a pointer to the next, which the IR backend is right to disagree with and which gcc fails too (`codegen/my/adjacent_locals.c`). Such a test is **skipped** in every other configuration and listed under `Skipped (belong to the other backend)`; it is not muted, because muting is for a bug someone intends to fix and a muted test is flagged the day it starts passing.

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
| `call_alignment.py` | simulate `rsp` through `-S`; 16-byte aligned at every call, restored by the epilogue |
| `emission_objdump.py` | differential: compare the machine IR against GNU `objdump` as multisets of (mnemonic, register set). The only check that can see stage 3 at all |
| `disasm_stable.py` | `-S` output is byte-identical across runs of the same command, under both backends, with and without ASLR |

They share `test/checkers/corpus.py` (argument parsing, the corpus walk, the dump invocation, the report), and each is a CTest entry per register allocator — `checker_allocation_linear`, `checker_allocation_trivial` — labelled `checkers`. Without that second round the allocator that is not the default is checked by nothing structural, and `allocation.py`'s scratch-register invariants, which belong to stage 2A alone, would never run at all:

```sh
ctest --test-dir build                # the five suites, the IR dumps, and the checkers
ctest --test-dir build -L checkers    # just the checkers, once per allocator (~35s)
ctest --test-dir build -LE checkers   # everything but (~9s)
cmake -B build -S . -DEDUCC_RUN_CHECKERS=OFF   # do not register them at all
```

A checker that cannot answer skips rather than fails (`SKIP_RETURN_CODE 2`) — `emission_objdump.py` needs GNU `objdump` as its oracle and exits 2 without it. **They rot.** Every one of them was written against the dumps of its day and had to be repaired when it was checked in — a `<clobbers ...>` annotation moved a bracket, index registers made a load look two-address, `movsx.8/4` gave widths a second number, a sparse switch put a conditional branch mid-block. Read a finding as a question about the checker first.

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

`test/bench/programs/*.c` are eight self-contained programs picked for what they do to an allocator, not for coverage: `nbody` and `matmul` keep more doubles live than there are volatile xmm registers, `crc` and `sieve` are tight integer loops, `binarytrees` is recursion over `malloc` so every call site is a place the allocator has to get caller-saved registers out of the way, `interp` is switch dispatch with everything live across every arm, `strings` is byte-at-a-time pointer walks, `sort` is quicksort's recursion and heapsort's sift-down. Each prints a checksum, and each is compiled by every configuration — `-legacy`, `-Xregalloc=trivial`, `-Xregalloc=linear`, `-Xregalloc=chaitin`, plus the host `cc` at `-O0` and `-O2` — so the reference doubles as an oracle: the driver reports any configuration whose output disagrees with the others, which is how `codegen/bugs/narrow_store_result.c` was found.

Three things get measured, because they are three different questions: how fast the *generated* code runs, how fast EduCC itself compiles (over its own front-end and IR sources), and how big `.text` comes out. `--no-static` turns off a fourth — spill slots and machine instructions counted straight out of `-irDump:ra`, which says *what the allocator did* rather than how long it took.

Nothing here is wired into `ctest`: it measures wall-clock time, it takes minutes, and a loaded machine will lie to it. Run it deliberately, before and after a backend change, with `--compare`. As of step 35 — run time and `.text` totalled over the eight programs, fastest of three runs; the static counts over those plus EduCC's own sources:

| | legacy | trivial | linear | chaitin | cc -O0 | cc -O2 |
| --- | --- | --- | --- | --- | --- | --- |
| run time | 3.91 | 6.60 | 2.29 | **2.27** | 3.24 | 1.39 |
| `.text` | 12.0K | 23.5K | 11.1K | **8.5K** | 8.8K | 12.4K |
| spill slots | — | 14098 | 1844 | **1473** | — | — |
| instructions | — | 89272 | 53510 | **38913** | — | — |

The linear scan is 3× the trivial allocator and beats the host compiler at `-O0`; the colouring allocator emits a quarter fewer instructions and a quarter less code again, at the same run time. Neither is free, and they cost in the same place: compiling EduCC's own sources, `chaitin` takes 0.49s and `linear` 0.39s against `trivial`'s 0.34s and `-legacy`'s 0.20s, which is the allocator doing work the other two do not. Why the better code is not also faster is `docs/ir-codegen-design.md` §7 stage C, and the answer turned out to be a selection bug rather than an allocation one.

## Architecture

Pipeline, driven from `src/main.c` → `compileFile()` in `src/parser.c`:

1. **Preprocessing** (`src/pp.c`, `src/lexer.c`) — full macro expansion, `#include`, conditionals, `#pragma once`. `-E` stops here.
2. **Parsing** (`src/parser.c`, ~3.6k lines) builds an AST (`src/tree.c`, `include/tree.h`) while interleaving **semantic analysis** (`src/sema.c`) — types, scopes, symbol resolution happen during parsing, not as a separate pass. `AstFile` / `AstTranslationUnit` is the top-level unit.
3. Diagnostics (`src/diagnostics.c`, `include/diagnostics.h`) accumulate through parsing/sema; the diagnostic catalog itself is data-driven via `include/diagnosticList.h` (`DIAGNOSTIC_DEF(severity, category, ID, format)` X-macro consumed with `#define DIAGNOSTIC_DEF ... #include "diagnosticList.h"`). Add new diagnostics there, not ad hoc. Exit-code contract: `Configuration.hadError` (set in `compileFile()` whenever `printDiagnostics()` reports an error) makes `main()` return `1`; a clean compile with only warnings still returns `0` — don't conflate the two when scripting around the compiler.
4. After a clean parse, compilation forks into **two independent backend pipelines** selected by `-legacy`:
   - **Legacy pipeline** (`-legacy`): `cannonizeAstFile()` (`src/cannonization.c`) lowers/normalizes the AST (e.g. desugaring composite ops), then `generateCodeForFile()` (`src/codegen_common.c`) walks the canonicalized AST directly to machine code via an arch-specific vtable (`ArchCodegen{generateFunction, generateVaribale}`, see `include/codegen.h`), implemented per-arch in `src/x86_64/codegen_x86_64.c` + `instructions_x86_64.c` and `src/riscv64/codegen_riscv64.c` + `instructions_riscv64.c`. Output is assembled straight into an in-memory ELF (`src/elf.c`, `include/_elf.h`) — there's no external assembler.
   - **IR pipeline** (default since roadmap step 30): `translateAstToIr()` (`src/ir/ast2ir.c`) lowers AST to a CFG-based SSA-capable IR (`include/ir/ir.h`, `include/ir/instructionList.h`). Passes: `buildSSA` (`src/ir/ssa.c`), `buildDominatorInfo` (`src/ir/dominators.c`), `gvn` — global value numbering (`src/ir/gvn.c`), `scp`/`cp` — (sparse) constant propagation (`src/ir/cp.c`, `src/ir/evaluator.c`), `dce` — dead code elimination (`src/ir/dce.c`). `IrFunction.phases` bitflags (`initalIr`/`ssa`/`cp_1`/`gvn`) track which passes a function has been through. The optimized IR then goes through a four-stage machine backend under `src/ir/codegen/`: `prepareMachineFunction` (stage 0 — critical edges, phi destruction, frame layout, `src/ir/codegen/prepare.c`), `selectInstructions` (stage 1, `isel.c` + `src/x86_64/isel_x86_64.c`), `allocateRegisters` (stage 2 — a linear scan in `regalloc_linear.c` over the liveness in `liveness.c`, with the spill-everything allocator of `regalloc.c` and the Chaitin-Briggs colouring of `regalloc_colour.c` reachable by `-Xregalloc=`; the liveness and the spiller in `spill.c` belong to none of the three, the colouring allocator reading the same dataflow as live sets where the linear scan reads it as intervals) and emission (stage 3, `src/x86_64/emit_x86_64.c`), producing the same `GeneratedFunction` the legacy backend does.

     **The backend is picked for the whole file, not per function.** It used to be per function — the IR backend took what it could and `generateFunction` got the rest — which is what let the pipeline be exercised before it was finished; it now covers the codegen corpus, and that machinery is gone, so anything it cannot build aborts where the gap is instead of silently producing the other backend's code. `docs/ir-codegen-design.md` is the design document (§6.21 for the removal) and tracks what is done and what is not.
5. `runLinker()` in `src/main.c` shells out to the system `ld` (not the EduCC binary itself) to produce the final executable, locating CRT objects/libc/libgcc across common distro layouts.

### Supporting infrastructure

- **Memory**: everything uses a custom arena/heap allocator (`src/memory.c`, `include/mem.h`) — `heapAllocate`/`releaseHeap` for general allocation, `createArena`/`areanAllocate`/`releaseArena` for phase-scoped bulk allocation (e.g. one arena per generated function/codegen context). Prefer arena allocation matching the existing context struct (`ParserContext`, `GenerationContext`, `IrContext`) over raw `malloc` when adding IR/codegen state.
- **Constant evaluation**: `src/evaluate.c` (AST-level, e.g. for static initializers/`#if`) vs `src/ir/evaluator.c` (IR-level, used by the `cp`/`scp` pass) are separate.
- Tree/IR dumping utilities (`src/treeDump.c`, `src/ir/irdump.c`) are the primary debugging tool for both pipelines — reach for `-astDump`/`-astCanonDump`/`-irDump` before adding printf debugging.

## Code comments
Keep comments brief — one short line max. Only comment on non-obvious logic;
skip comments that just restate what the code does. Do not add docstrings/
header comments unless asked.
