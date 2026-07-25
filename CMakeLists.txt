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

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Udis86)

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
  src/x86_64/instructions_x86_64.c
  src/x86_64/codegen_x86_64.c
  src/riscv64/codegen_riscv64.c
  src/riscv64/instructions_riscv64.c
  src/ir/ir.c
  src/ir/irdump.c
  src/ir/ast2ir.c
  src/ir/dominators.c
  src/ir/ssa.c
  src/ir/dce.c
  src/ir/cp.c
  src/ir/evaluator.c
  src/ir/gvn.c
)

add_executable(main ${SOURCES})
target_include_directories(main PRIVATE include)
target_link_libraries(main PRIVATE udis86 m)

if(NOT CMAKE_C_COMPILER_ID)
  # Self-hosting: CMAKE_C_COMPILER is an EduCC binary from a previous stage.
  # Its own default include path list (src/main.c) has a "sdk/include"
  # entry that it resolves relative to its current working directory at
  # invocation time. That assumption holds for `make` (always run from the
  # repo root) but not for CMake, which always runs compile recipes from
  # the build directory. Supply the real, absolute location explicitly so
  # EduCC can still find its stddef.h/stdarg.h/etc. shims.
  target_include_directories(main PRIVATE "${CMAKE_SOURCE_DIR}/sdk/include")
endif()
