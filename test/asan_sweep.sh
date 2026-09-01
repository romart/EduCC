#!/usr/bin/env bash
# Runs a sanitized compiler over every input this repository has and reports
# every AddressSanitizer finding.
#
#   cmake -B build-asan -S . -DEDUCC_SANITIZE=ON && cmake --build build-asan -j$(nproc)
#   test/asan_sweep.sh build-asan/bin/main
#
# Two corpora, because they find different things: the fixtures are small and
# cover the odd corners of the language, EduCC's own sources are 30k lines of
# real C and are by a wide margin the deepest input it has. Both backends, since
# the sanitized bug is as likely to be in one code generator as the other.
#
# Leak detection is off: the compiler is an arena allocator that deliberately
# never frees, so every run "leaks" and the report would be all noise.

set -uo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
compiler=${1:-$root/build-asan/bin/main}

if [ ! -x "$compiler" ]; then
  echo "no sanitized compiler at '$compiler'" >&2
  echo "build one with: cmake -B build-asan -S . -DEDUCC_SANITIZE=ON && cmake --build build-asan" >&2
  exit 2
fi

export ASAN_OPTIONS=detect_leaks=0

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

findings=0
compiled=0
rejected=0

# One compile. A nonzero exit on its own is the compiler rejecting the input -
# the parser corpus is full of inputs meant to be rejected - so only a sanitizer
# report is a finding. The rejections are still counted and printed at the end,
# because "nothing compiled at all" and "nothing went wrong" produce the same
# report otherwise.
sweep() {
  local label=$1; shift
  compiled=$((compiled + 1))
  if ! "$@" > "$tmp/out" 2>&1; then
    rejected=$((rejected + 1))
  fi
  if grep -q "AddressSanitizer" "$tmp/out"; then
    findings=$((findings + 1))
    echo "=== $label"
    echo "    $*"
    sed -n '1,25p' "$tmp/out"
  fi
}

cd "$root"

echo "== the fixture corpus, both backends"
while IFS= read -r fixture; do
  sweep "$fixture (ir)"     "$compiler" -oneline -c -o "$tmp/o.o" "$fixture"
  sweep "$fixture (legacy)" "$compiler" -oneline -legacy -c -o "$tmp/o.o" "$fixture"
done < <(find test/testData -name '*.c' | sort)

echo "== EduCC's own sources, both backends"
# The same include paths and defines CMakeLists.txt builds these with; Zydis'
# header is only reachable once it has been fetched, so a build directory that
# has been configured at least once is a precondition.
educcFlags=(-I include -I sdk/include -I .deps/zydis_src-src
            -DZYDIS_STATIC_BUILD -DZYCORE_STATIC_BUILD)
while IFS= read -r source; do
  sweep "$source (ir)"     "$compiler" -c "${educcFlags[@]}" -o "$tmp/o.o" "$source"
  sweep "$source (legacy)" "$compiler" -c -legacy "${educcFlags[@]}" -o "$tmp/o.o" "$source"
done < <(find src -name '*.c' | sort)

echo "== $compiled compiles, $rejected rejected inputs, $findings AddressSanitizer findings"
[ "$findings" -eq 0 ]
