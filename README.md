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

## Installing

```sh
cmake --install build --prefix ~/.local
~/.local/bin/educc -o hello hello.c
```

That gives `<prefix>/bin/educc` and the compiler's own `stddef.h`/`stdarg.h` shims under `<prefix>/lib/educc/include`. The result is relocatable: nothing absolute is recorded in it, so the tree can be moved, tarred up, or unpacked somewhere else and still works.

EduCC finds those headers relative to its own binary (via `/proc/self/exe`), not relative to the current directory, so an installed copy compiles from anywhere. `EDUCC_SDK_DIR` overrides the lookup, and `-print-sdk-dir` reports which copy won:

```sh
$ ~/.local/bin/educc -print-sdk-dir
/home/you/.local/lib/educc/include
```

A binary built but never installed falls back to the source tree it was configured from, so `build/bin/main` keeps working without an install step. The install renames the binary to `educc` but leaves `build/bin/main` alone, so every script and launch config in the repo is unaffected.

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

## Benchmarks

The tests say the generated code is correct, not that it is any good. `test/bench/bench.py` measures how fast it runs, how fast EduCC compiles, how big `.text` is, and what the register allocator did — over eight self-contained programs in `test/bench/programs/`, compiled by every backend configuration and by the host `cc`. Each program prints a checksum, so the run doubles as a differential oracle across configurations.

```sh
python3 test/bench/bench.py                                  # everything, ~3 min
python3 test/bench/bench.py --runs 5 --json before.json      # save a run
python3 test/bench/bench.py --compare before.json            # and diff against it
python3 test/bench/bench.py --filter nbody,sort --no-static  # scope it down
```

Not wired into `ctest`: it is wall-clock timing, it takes minutes, and a loaded machine will lie to it. Run it deliberately, before and after a backend change, with `--compare`. `chaitin` is the default allocator; `-Xregalloc=linear|trivial` selects the other two, kept as differential oracles.

`--runs 3`, on an idle machine:

```
run time (s)      legacy     trivial      linear     chaitin      cc -O0      cc -O2
------------------------------------------------------------------------------------
binarytrees        0.412       0.381       0.392       0.389       0.369       0.365
crc                0.334       0.530       0.211       0.211       0.375       0.244
interp             0.295       0.870       0.219       0.146       0.408       0.158
matmul             0.450       0.997       0.189       0.112       0.420       0.049
nbody              0.775       1.289       0.401       0.261       0.384       0.121
sieve              0.521       0.640       0.157       0.166       0.377       0.069
sort               0.477       0.852       0.255       0.278       0.411       0.230
strings            0.618       0.842       0.236       0.177       0.377       0.139
------------------------------------------------------------------------------------
total              3.881       6.401       2.059       1.739       3.122       1.374

compile time (s)      legacy     trivial      linear     chaitin      cc -O0      cc -O2
----------------------------------------------------------------------------------------
binarytrees            0.022       0.022       0.022       0.023       0.035       0.092
crc                    0.024       0.023       0.023       0.023       0.033       0.042
interp                 0.020       0.021       0.019       0.022       0.032       0.043
matmul                 0.022       0.023       0.022       0.025       0.032       0.046
nbody                  0.024       0.025       0.025       0.027       0.041       0.068
sieve                  0.021       0.021       0.022       0.021       0.031       0.041
sort                   0.024       0.024       0.024       0.024       0.035       0.057
strings                0.020       0.022       0.019       0.020       0.031       0.054
----------------------------------------------------------------------------------------
total                  0.177       0.182       0.177       0.187       0.270       0.443

.text size        legacy     trivial      linear     chaitin      cc -O0      cc -O2
------------------------------------------------------------------------------------
binarytrees         0.9K        1.3K        0.9K        0.7K        0.7K        5.0K
crc                 0.8K        1.5K        0.7K        0.6K        0.6K        0.6K
interp              1.4K        3.0K        1.1K        0.9K        1.3K        0.8K
matmul              1.2K        2.8K        1.0K        0.8K        0.9K        1.1K
nbody               3.9K        7.9K        3.6K        2.4K        2.4K        1.6K
sieve               0.7K        1.2K        0.6K        0.5K        0.5K        0.6K
sort                1.8K        3.2K        1.4K        1.2K        1.4K        1.0K
strings             1.3K        2.5K        1.1K        0.8K        1.0K        1.7K
------------------------------------------------------------------------------------
total              12.0K       23.3K       10.3K        7.9K        8.8K       12.4K

EduCC's own sources      legacy     trivial      linear     chaitin      cc -O0      cc -O2
-------------------------------------------------------------------------------------------
compile time (s)          0.199       0.349       0.400       0.488       0.613       2.029

                  legacy     trivial      linear     chaitin      cc -O0      cc -O2
------------------------------------------------------------------------------------
.text size        202.0K      421.3K      220.0K      171.1K      157.0K      107.1K

what the allocator did      legacy     trivial      linear     chaitin      cc -O0      cc -O2
----------------------------------------------------------------------------------------------
spill slots                      -       14071        1758        1479           -           -
instructions                     -       89757       53388       39010           -           -
```

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
