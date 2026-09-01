#!/usr/bin/env bash
# Builds EduCC with itself and runs the whole test suite with the result.
#
# bootstrap.sh answers "does the compiler reach a fixed point building itself";
# this answers "is the compiler it produces any good", which is a different
# question and the one that found seven of this tree's shipped bugs - a
# self-compiled compiler is by far the largest and least forgiving input EduCC
# has, and every one of those bugs was invisible to a gcc-built binary running
# the same corpus. Two commands nobody was obliged to type until this was a
# script and a CI job (docs/ir-codegen-design.md sections 9 and 11 step 31).
#
#   ./selfhost.sh              stage 1, stage 2, ctest with stage 2, fixed point
#   ./selfhost.sh --no-tests   just the stages and the fixed-point check
#   EDUCC_SELFHOST_FLAGS=-legacy ./selfhost.sh    the same for the old backend
#
# Deliberately builds in build-selfhost/ and never touches build/: CMake caches
# CMAKE_C_COMPILER, and a build/ left pointed at an EduCC binary makes every
# later build and test run self-compiled without saying so.

set -euo pipefail

root=$(cd "$(dirname "$0")" && pwd)
out=$root/build-selfhost
jobs=$(nproc 2>/dev/null || echo 4)
runTests=1
flags=${EDUCC_SELFHOST_FLAGS:-}

for arg in "$@"; do
  case $arg in
    --no-tests) runTests=0 ;;
    *) echo "usage: $0 [--no-tests]" >&2; exit 2 ;;
  esac
done

# The stage the host compiler builds, then the two EduCC builds of itself. Two
# rather than one because the fixed point is what says the compiler compiles
# itself *correctly*: stage 1 is EduCC's own code as gcc renders it, stage 2 is
# the same code as stage 1 renders it, and byte-identical binaries mean the two
# renderings agree everywhere the compiler's own sources reach.
build() {
  local dir=$1 cc=$2
  rm -rf "$dir"
  if [ -n "$cc" ]; then
    cmake -B "$dir" -S "$root" -DCMAKE_C_COMPILER="$cc" ${flags:+-DCMAKE_C_FLAGS="$flags"} > /dev/null
  else
    cmake -B "$dir" -S "$root" > /dev/null
  fi
  cmake --build "$dir" -j"$jobs" > /dev/null
}

echo "== stage 0: building with the host compiler"
build "$out/stage0" ""

echo "== stage 1: building with stage 0's EduCC${flags:+ ($flags)}"
build "$out/stage1" "$out/stage0/bin/main"

echo "== stage 2: building with stage 1's EduCC${flags:+ ($flags)}"
build "$out/stage2" "$out/stage1/bin/main"

sha1() { sha1sum "$1" | cut -d' ' -f1; }
one=$(sha1 "$out/stage1/bin/main")
two=$(sha1 "$out/stage2/bin/main")

echo "== stage 1: $one"
echo "== stage 2: $two"
if [ "$one" != "$two" ]; then
  echo "FAIL: no fixed point - stage 2 differs from stage 1" >&2
  exit 1
fi
echo "== fixed point reached at stage 2"

if [ "$runTests" -eq 1 ]; then
  # Against stage 2 rather than stage 1: same binary either way if the fixed
  # point holds, and if it somehow does not, this is the one that was built by
  # a self-compiled compiler rather than by gcc.
  echo "== running the suite with the self-hosted compiler"
  ctest --test-dir "$out/stage2" --output-on-failure
fi
