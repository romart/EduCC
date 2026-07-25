# Run via `cmake -DSCRIPT_TARGET=<path> -P patch_udis86_py3.cmake` as
# udis86's FetchContent PATCH_COMMAND.
#
# udis86's itab.c/itab.h generator (scripts/ud_opcode.py) is Python-2-only:
# it uses '/' for (intended) integer division and calls list.sort() on the
# result of dict.keys(), both of which fail under Python 3. Rewrite the two
# offending lines so the generator runs under Python 3 -- python2 is no
# longer reasonably available on current systems.

file(READ "${SCRIPT_TARGET}" contents)
string(REPLACE "int(v) / 32" "int(v) // 32" contents "${contents}")
string(REPLACE "keys.sort()" "keys = sorted(keys)" contents "${contents}")
file(WRITE "${SCRIPT_TARGET}" "${contents}")
