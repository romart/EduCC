# Building EduCC *for* riscv64, with EduCC. Pass it as
# -DCMAKE_TOOLCHAIN_FILE=cmake/riscv64.toolchain.cmake alongside a
# -DCMAKE_C_COMPILER naming an EduCC binary; selfhost.sh --target riscv64 is
# what does both.
#
# There is no cross *driver* here. EduCC is one binary that targets either
# machine, so what makes this a cross build is a flag and an emulator, not a
# second toolchain: -march=riscv64 chooses the backend, and EduCC's own linker
# does the rest against the sysroot it already knows about (LinkTarget.sysroot,
# src/link/target_link.c).

set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR riscv64)

set(EDUCC_RISCV64_SYSROOT "/usr/riscv64-linux-gnu" CACHE PATH
    "Where the riscv64 C runtime lives")

# The '=' spelling rather than '-march riscv64': CMake passes flags as one
# string per word through several layers that would split the pair.
string(APPEND CMAKE_C_FLAGS_INIT " -march=riscv64")

# How the build runs anything it just produced. CMake consults this for
# try_run and for custom commands; CMakeLists.txt also writes it into the
# wrapper the test suites are handed, since ctest cannot exec a riscv64 binary
# on an x86-64 machine and every harness here takes a compiler *path*.
find_program(QEMU_RISCV64 qemu-riscv64 REQUIRED)
set(CMAKE_CROSSCOMPILING_EMULATOR "${QEMU_RISCV64};-L;${EDUCC_RISCV64_SYSROOT}")

# The vendored Zydis is never built with the in-progress EduCC (cmake/Zydis.cmake
# says why), which on a cross build also means it is never built for the host's
# machine: an x86-64 libzydis.a in a riscv64 link is the first error anyone sees.
find_program(ZYDIS_HOST_CC riscv64-linux-gnu-gcc REQUIRED)
find_program(ZYDIS_AR riscv64-linux-gnu-ar REQUIRED)

# Look for libraries and headers under the target's sysroot, not the host's -
# and for programs on the host, since those are things the build runs.
set(CMAKE_FIND_ROOT_PATH "${EDUCC_RISCV64_SYSROOT}")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
