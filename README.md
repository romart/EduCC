# EduCC

[![CI](https://github.com/romart/EduCC/actions/workflows/ci.yml/badge.svg)](https://github.com/romart/EduCC/actions/workflows/ci.yml)

An educational, from-scratch C compiler targeting `x86_64` and (in progress) `riscv64`. It has its own preprocessor, lexer, parser, semantic analysis, and native code generator, and drives the system linker (`ld`) directly to produce ELF executables — no LLVM/GCC backend. It can compile itself (see [Bootstrapping](#bootstrapping)).

## Requirements

- CMake >= 3.16
- A host C compiler (gcc or clang)
- Python 3 (for the test runner and the structural checkers under `test/checkers/`)
- `ld` and a glibc/gcc toolchain on `PATH` (for linking compiled programs)
- Network access on the first `cmake` configure, which fetches [Zydis](https://github.com/zyantific/zydis) (used to disassemble `-S` output) into `.deps/`

## Build

```sh
cmake -B build -S .
cmake --build build -j$(nproc)
```

The compiler binary ends up at `build/bin/main`. To build with a different host compiler:

```sh
cmake -B build -S . -DCMAKE_C_COMPILER=clang
```

To rebuild from scratch, just delete the build directory: `rm -rf build`.

### Running the compiler

```sh
build/bin/main -o hello ./path/to/hello.c
./hello
```

It accepts a GCC-like subset of flags (`-o`, `-c`, `-I`, `-L`, `-l`, `-D`, `-E`, `-march x86_64|riscv64`, ...). See `src/main.c` for the full list, including EduCC-specific debugging flags like `-astDump` and `-irDump`, and `-legacy`, which compiles through the older direct-from-AST code generator instead of the IR pipeline the compiler uses by default.

## Editor support (clangd)

Generate a compilation database and point clangd at it:

```sh
cmake -B build -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
ln -sf build/compile_commands.json compile_commands.json
```

clangd (and most editor integrations) will pick up `compile_commands.json` from the project root automatically. Regenerate it whenever `CMakeLists.txt` or the source file list changes — re-running the `cmake -B build` command above is enough, no need to redo the symlink.

## Tests

Tests are data-driven fixtures under `test/testData/{parser,pp,codegen,crossabi}`. The easiest way to run all of them is `ctest`, from the build directory:

```sh
cmake -B build -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON  # only needed once, or after editing CMakeLists.txt
cmake --build build -j$(nproc)
cd build && ctest --output-on-failure
```

This runs every suite with a per-suite timeout, and can produce a CI-friendly report with `ctest --output-junit results.xml`. Use `ctest -R codegen` to run just one. There are more of them than there are fixture directories: the codegen fixtures are compiled once through each backend (`codegen`, `codegen_legacy`), the IR dumps are compared once per pipeline phase, and `test/checkers/` contributes one entry each (`ctest -L checkers`, or `-LE checkers` to leave them out).

The `crossabi` suite is the odd one out. Each of its fixtures is a *pair* of files — `<name>.c` and `<name>.partner.c` — compiled with a different backend each, linked into one binary and run, then swapped over and run again. Every other suite runs one backend at a time, so this is the only place the two code generators have to agree with each other about calling conventions.

To run a suite directly (e.g. to pass extra flags, or scope to a single subdirectory), use `test/testRunner.py`:

```sh
# Parser/AST-dump tests
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/parser -m parser

# Preprocessor tests
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/pp -m preprocessor

# Codegen tests (compiles, links, and runs each test binary)
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/codegen -m codegen
```

`-p` can be repeated or point at a single subdirectory (e.g. `test/testData/codegen/tinyc`) to scope a run. A nonzero exit code equals the number of failed tests, and any failures are listed by path at the end of the run.

A test whose expected/baseline file (`*.txt`, `*.err`, `*.canon.txt`, `*.expect`) doesn't exist yet **fails** rather than silently passing. After adding a new test, or after an intentional change to compiler output, regenerate baselines explicitly:

```sh
python3 test/testRunner.py -c build/bin/main -wd /tmp/eduwd -p test/testData/parser -m parser --update-baselines
```

Then review the result with `git diff` before committing — this only *writes* what the compiler currently outputs, it doesn't judge whether that output is correct.

## Bootstrapping

```sh
./bootstrap.sh
```

Builds EduCC with the host compiler, then repeatedly recompiles it with itself and `sha1sum`-compares the results to confirm the build reaches a fixed point. Useful as a sanity check for changes that could affect self-compilation.

```sh
./selfhost.sh
```

Asks the other half of the question: not whether the build reaches a fixed point but whether the compiler it produces is any good. Builds one stage with the host compiler and two with EduCC, checks the two EduCC stages are byte-identical, and then runs the whole test suite *with the self-hosted compiler* — which is by far the largest and least forgiving input this compiler has. It builds under `build-selfhost/` and leaves `build/` alone.

## Continuous integration

`.github/workflows/ci.yml` runs three things on every push: `ctest` under both gcc and clang, `./selfhost.sh`, and an AddressSanitizer build followed by `test/asan_sweep.sh`, which compiles every fixture and every one of EduCC's own sources through both backends and fails on any sanitizer report. All three are runnable locally with exactly the commands the workflow uses.
