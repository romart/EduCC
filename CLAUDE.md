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
rm -rf build                                           # "clean" (no incremental clean target is defined)
```

Build artifacts go under `build/` (git-ignored), with the final binary at `build/bin/main`.

Linking requires `-ludis86` (disassembler, used by the x86_64 backend) and a real `ld`/glibc/gcc toolchain on the host (`main.c`'s `runLinker`/`gccLibPath` locate `crt1.o`/`crtbegin.o` etc. under common Linux paths). SDK headers used when compiling test/user programs live under `sdk/include`.

### The udis86 dependency (`cmake/Udis86.cmake`)

udis86 has no official prebuilt binaries anywhere upstream (`vmt/udis86` only ever published source tags, never a GitHub Release). `cmake/Udis86.cmake` handles this in two tiers:
1. `find_path`/`find_library` for an already-installed system udis86 (e.g. via a distro package or AUR) — used as-is via an `IMPORTED` target if present.
2. Otherwise, it fetches the upstream `v1.7.2` source tarball via `FetchContent` and builds it as a static lib. Two things needed patching to make this work on a modern system, both handled automatically:
   - udis86's opcode-table generator (`scripts/ud_itab.py`/`ud_opcode.py`) is Python-2-only; `cmake/patch_udis86_py3.cmake` rewrites the two offending lines (integer-division `/` and `dict.keys().sort()`) to run under Python 3.
   - `udis86.c` calls `memset()` without including `<string.h>`, which GCC ≥ 14 now treats as a hard error by default in gnu99 mode; suppressed for this vendored target only.
   Crucially, this fallback build always compiles with a real host compiler (`gcc`/`clang`/`cc`, auto-detected), **never** with `CMAKE_C_COMPILER` directly — see Bootstrapping below for why.

### Bootstrapping

`./bootstrap.sh` performs a multi-stage self-host build: configures+builds `main` with the host compiler, then repeatedly reconfigures with `-DCMAKE_C_COMPILER=<path to the previous stage's binary>` and rebuilds, diffing `sha1sum` of the resulting binaries to verify a fixed point. Use this to sanity-check changes that could affect self-compilation.

Two build-system-level workarounds exist solely to keep this working, both in `CMakeLists.txt`/`cmake/Udis86.cmake`:
- The vendored udis86 fallback (above) is always built with a real compiler, never with the in-progress EduCC binary — EduCC can't yet parse arbitrary third-party C against real system headers well enough to compile it.
- `src/main.c` adds a default include path `"sdk/include"` that EduCC resolves relative to its *current working directory* at invocation time. That assumption holds for `make` (always invoked from the repo root) but not CMake, which always runs compile recipes from the build directory. When `CMAKE_C_COMPILER_ID` is empty (i.e. the compiler is an unrecognized EduCC binary, not gcc/clang), `CMakeLists.txt` adds an explicit absolute `-I<repo>/sdk/include` to the `main` target so EduCC can still find its `stddef.h`/`stdarg.h` shims. This is inert for normal gcc/clang builds.

## Running the compiler

The CLI intentionally mimics a subset of GCC flags (`-o`, `-c`, `-I`, `-L`, `-l`, `-D`, `-E`, `-S`, `-march`, `-std`, `-O*`, `-W*` are accepted, many as no-ops). EduCC-specific flags (see `src/main.c`):

- `-experimental` — route compilation through the new AST→IR pipeline (see Architecture) instead of the legacy direct-from-AST codegen.
- `-march x86_64|riscv64` — select backend (default `x86_64`).
- `-astDump <file>` / `-astCanonDump <file>` — dump the AST before/after canonicalization.
- `-irDump <file>` — dump the IR (experimental pipeline); also emits `cfg.dot` for the CFG. Optionally `-irDump:phase[,phase...] <file>` (phases: `initial`, `ssa`, `scp`, `gvn`, `dce`, `mir`) snapshots the IR immediately after each named pass in `translateFunction()` (`src/ir/ast2ir.c`) instead of only the fully-processed result — e.g. `-irDump:ssa` captures IR right after `buildSSA`, unaffected by whatever `scp`/`gvn`/`dce` do afterwards. `mir` is the odd one out: it dumps the `MachineFunction` built from the optimized IR (`src/ir/codegen/`), in machine-IR rather than IR form.
- `-skipCodegen` — stop after parsing/sema (used heavily by parser tests).
- `-oneline` — non-verbose output (used by the test runner).
- `-logtokens`, `-memstat` — debug tracing / arena memory statistics.

Example (matches `.dbg.config.json` / `.vimspector.json`):
```sh
build/bin/main -experimental -irDump hw.ir.txt ./test/testData/codegen/simple/gvn.c
```

## Tests

Tests are plain data-driven fixtures under `test/testData/{parser,pp,codegen}` run via `test/testRunner.py` against a built `build/bin/main`. `cmake --build build && cd build && ctest --output-on-failure` runs all three suites in one shot (each wired up as its own CTest test with a timeout — see `CMakeLists.txt`'s `educc_add_test`); `ctest --output-junit results.xml` produces a CI-friendly report. For scoping to a subdirectory or passing extra flags, invoke the runner directly, pointing `--compiler` at the built binary and `--working-dir` at a scratch directory for outputs:

```sh
# Parser/AST-dump tests (compares -astDump/-astCanonDump/stderr against *.txt/*.canon.txt/*.err)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/parser -m parser

# Preprocessor tests (compares -E output against *.expect)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/pp -m preprocessor

# Codegen tests (compiles+links+*runs* the binary, args optionally from a sibling *.args file)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/codegen -m codegen
```

Notes on the runner's behavior:
- `-p/--test-path` can be repeated and can point at a single subdirectory (e.g. `test/testData/codegen/tinyc`) to scope to one test group.
- If an expected file (`*.txt`, `*.err`, `*.canon.txt`, `*.expect`) doesn't exist yet, the test **fails** rather than silently passing — pass `--update-baselines` to (re)write every baseline from current actual output instead of comparing, then review with `git diff` before committing. There's no silent auto-baselining anymore.
- Codegen tests actually execute the compiled binary and check its exit code; a `<name>.args` file (one arg-string per line) runs the binary once per line. A nonzero compiler exit code fails the test; a zero exit with warnings on stderr does not (see the exit-code contract note in Architecture below).
- Nonzero process exit at the runner level is the failed-test count; on failure it also lists every failed test's path. Directory walks are sorted, so run order (and failure order) is deterministic across machines.
- A test can be **muted** by placing a `<name>.muted` file next to its `<name>.c`, with the reason as the file's contents (printed whenever the test runs). This is for known-broken fixtures kept in the repo so a bug stays reproducible: the test still runs and reports, but its failures don't count towards the exit code. If a muted test passes every check, the summary flags it under `MUTED TESTS THAT NOW PASS` so the stale marker gets deleted — loudly, but without failing the run. `--update-baselines` deliberately skips muted tests rather than baking their known-wrong output into a golden file.

## Architecture

Pipeline, driven from `src/main.c` → `compileFile()` in `src/parser.c`:

1. **Preprocessing** (`src/pp.c`, `src/lexer.c`) — full macro expansion, `#include`, conditionals, `#pragma once`. `-E` stops here.
2. **Parsing** (`src/parser.c`, ~3.6k lines) builds an AST (`src/tree.c`, `include/tree.h`) while interleaving **semantic analysis** (`src/sema.c`) — types, scopes, symbol resolution happen during parsing, not as a separate pass. `AstFile` / `AstTranslationUnit` is the top-level unit.
3. Diagnostics (`src/diagnostics.c`, `include/diagnostics.h`) accumulate through parsing/sema; the diagnostic catalog itself is data-driven via `include/diagnosticList.h` (`DIAGNOSTIC_DEF(severity, category, ID, format)` X-macro consumed with `#define DIAGNOSTIC_DEF ... #include "diagnosticList.h"`). Add new diagnostics there, not ad hoc. Exit-code contract: `Configuration.hadError` (set in `compileFile()` whenever `printDiagnostics()` reports an error) makes `main()` return `1`; a clean compile with only warnings still returns `0` — don't conflate the two when scripting around the compiler.
4. After a clean parse, compilation forks into **two independent backend pipelines** selected by `-experimental`:
   - **Legacy pipeline** (default): `cannonizeAstFile()` (`src/cannonization.c`) lowers/normalizes the AST (e.g. desugaring composite ops), then `generateCodeForFile()` (`src/codegen_common.c`) walks the canonicalized AST directly to machine code via an arch-specific vtable (`ArchCodegen{generateFunction, generateVaribale}`, see `include/codegen.h`), implemented per-arch in `src/x86_64/codegen_x86_64.c` + `instructions_x86_64.c` and `src/riscv64/codegen_riscv64.c` + `instructions_riscv64.c`. Output is assembled straight into an in-memory ELF (`src/elf.c`, `include/_elf.h`) — there's no external assembler.
   - **Experimental IR pipeline** (`-experimental`): `translateAstToIr()` (`src/ir/ast2ir.c`) lowers AST to a CFG-based SSA-capable IR (`include/ir/ir.h`, `include/ir/instructionList.h`). Passes: `buildSSA` (`src/ir/ssa.c`), `buildDominatorInfo` (`src/ir/dominators.c`), `gvn` — global value numbering (`src/ir/gvn.c`), `scp`/`cp` — (sparse) constant propagation (`src/ir/cp.c`, `src/ir/evaluator.c`), `dce` — dead code elimination (`src/ir/dce.c`). `IrFunction.phases` bitflags (`initalIr`/`ssa`/`cp_1`/`gvn`) track which passes a function has been through. This pipeline currently only lowers/optimizes and dumps (`src/ir/irdump.c`, `.dot` CFG output); it does not yet emit machine code — codegen for `-experimental` is a work in progress.
5. `runLinker()` in `src/main.c` shells out to the system `ld` (not the EduCC binary itself) to produce the final executable, locating CRT objects/libc/libgcc across common distro layouts.

### Supporting infrastructure

- **Memory**: everything uses a custom arena/heap allocator (`src/memory.c`, `include/mem.h`) — `heapAllocate`/`releaseHeap` for general allocation, `createArena`/`areanAllocate`/`releaseArena` for phase-scoped bulk allocation (e.g. one arena per generated function/codegen context). Prefer arena allocation matching the existing context struct (`ParserContext`, `GenerationContext`, `IrContext`) over raw `malloc` when adding IR/codegen state.
- **Constant evaluation**: `src/evaluate.c` (AST-level, e.g. for static initializers/`#if`) vs `src/ir/evaluator.c` (IR-level, used by the `cp`/`scp` pass) are separate.
- Tree/IR dumping utilities (`src/treeDump.c`, `src/ir/irdump.c`) are the primary debugging tool for both pipelines — reach for `-astDump`/`-astCanonDump`/`-irDump` before adding printf debugging.
