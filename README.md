# EduCC

An educational, from-scratch C compiler targeting `x86_64` and (in progress) `riscv64`. It has its own preprocessor, lexer, parser, semantic analysis, and native code generator, and drives the system linker (`ld`) directly to produce ELF executables — no LLVM/GCC backend. It can compile itself (see [Bootstrapping](#bootstrapping)).

## Requirements

- CMake >= 3.16
- A host C compiler (gcc or clang)
- Python 3 (only needed to build the [udis86](https://github.com/vmt/udis86) dependency from source, if it isn't already installed as a system package)
- `ld` and a glibc/gcc toolchain on `PATH` (for linking compiled programs)

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

It accepts a GCC-like subset of flags (`-o`, `-c`, `-I`, `-L`, `-l`, `-D`, `-E`, `-march x86_64|riscv64`, ...). See `src/main.c` for the full list, including EduCC-specific debugging flags like `-astDump`, `-irDump`, and `-experimental`.

## Editor support (clangd)

Generate a compilation database and point clangd at it:

```sh
cmake -B build -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
ln -sf build/compile_commands.json compile_commands.json
```

clangd (and most editor integrations) will pick up `compile_commands.json` from the project root automatically. Regenerate it whenever `CMakeLists.txt` or the source file list changes — re-running the `cmake -B build` command above is enough, no need to redo the symlink.

## Tests

Tests are data-driven fixtures under `test/testData/{parser,pp,codegen}`. The easiest way to run all of them is `ctest`, from the build directory:

```sh
cmake -B build -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=ON  # only needed once, or after editing CMakeLists.txt
cmake --build build -j$(nproc)
cd build && ctest --output-on-failure
```

This runs all three suites (`parser`, `preprocessor`, `codegen`) with a per-suite timeout, and can produce a CI-friendly report with `ctest --output-junit results.xml`. Use `ctest -R codegen` to run just one suite.

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
