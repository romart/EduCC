# Fetches Zydis' amalgamated distribution (one Zydis.c, one Zydis.h) and
# compiles it as a static library exposed as the `zydis` target. Used by the
# x86_64 backend's disassemble() to render '-S' output.
#
# Zydis publishes 'zydis-amalgamated.tar.gz' as a release artifact, so unlike
# udis86 -- which this replaced -- there is a stable, hash-pinned URL to fetch
# and nothing to generate or patch: no Python opcode-table step, no Python-2
# rewrite, no missing-include workaround. Its own CMakeLists is deliberately
# not used; see the compiler note below for why a single translation unit
# matters here.

# FetchContent unpacks into ${CMAKE_BINARY_DIR}/_deps by default, and
# bootstrap.sh throws the build directory away between each of its five
# stages - so the same tarball would be fetched five times per bootstrap, and
# one network blip halfway through loses the whole run. Keep the download and
# the unpacked source outside the build tree so every stage shares them.
set(FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/.deps" CACHE PATH
    "Where third-party sources are downloaded and unpacked")

include(FetchContent)
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

FetchContent_Declare(zydis_src
  URL "https://github.com/zyantific/zydis/releases/download/v4.1.0/zydis-amalgamated.tar.gz"
  URL_HASH SHA256=aa9b82be3a37a2998bd8e16cf583bbf2b6c3d80e97dc20504169dc32ca1ced59
)
FetchContent_MakeAvailable(zydis_src)

# Deliberately NOT using add_library()/CMAKE_C_COMPILER here. When EduCC is
# bootstrapping itself, CMAKE_C_COMPILER is EduCC's own (still very
# incomplete) binary, which cannot parse real system headers well enough to
# compile arbitrary third-party C code such as this vendored library. The
# original Makefile-based bootstrap never recompiled the disassembler with the
# in-progress EduCC compiler either -- it only ever swapped compilers for
# EduCC's own sources and always linked the disassembler as an already-built
# library. Mirror that: always build this copy with a real host compiler,
# independent of whatever CMAKE_C_COMPILER the outer project is using.
if(CMAKE_C_COMPILER_ID)
  set(ZYDIS_HOST_CC "${CMAKE_C_COMPILER}" CACHE FILEPATH "Compiler used to build vendored Zydis")
else()
  find_program(ZYDIS_HOST_CC NAMES gcc cc clang)
  if(NOT ZYDIS_HOST_CC)
    message(FATAL_ERROR "Can't build vendored Zydis: no real host C compiler (gcc/clang/cc) found, "
                        "and CMAKE_C_COMPILER ('${CMAKE_C_COMPILER}') isn't recognized as one either "
                        "(this happens when bootstrapping with EduCC's own not-yet-complete binary). "
                        "Install gcc or clang.")
  endif()
endif()
find_program(ZYDIS_AR NAMES ar REQUIRED)

set(ZYDIS_OBJECT "${CMAKE_CURRENT_BINARY_DIR}/zydis_objs/Zydis.c.o")
set(ZYDIS_ARCHIVE "${CMAKE_CURRENT_BINARY_DIR}/libzydis.a")
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/zydis_objs")

add_custom_command(
  OUTPUT "${ZYDIS_OBJECT}"
  # No ZYDIS_DISABLE_* here: the amalgamated header honours those, but the
  # amalgamated Zydis.c still carries the bodies they cut the declarations
  # out from, so switching one off is a build error rather than a saving.
  COMMAND "${ZYDIS_HOST_CC}" -O2 -DZYDIS_STATIC_BUILD -DZYCORE_STATIC_BUILD
          -I"${zydis_src_SOURCE_DIR}"
          -c "${zydis_src_SOURCE_DIR}/Zydis.c" -o "${ZYDIS_OBJECT}"
  DEPENDS "${zydis_src_SOURCE_DIR}/Zydis.c"
  COMMENT "Building Zydis.c (with ${ZYDIS_HOST_CC})"
)
add_custom_command(
  OUTPUT "${ZYDIS_ARCHIVE}"
  COMMAND "${ZYDIS_AR}" rcs "${ZYDIS_ARCHIVE}" "${ZYDIS_OBJECT}"
  DEPENDS "${ZYDIS_OBJECT}"
  COMMENT "Archiving libzydis.a"
)
add_custom_target(zydis_build DEPENDS "${ZYDIS_ARCHIVE}")

add_library(zydis STATIC IMPORTED)
add_dependencies(zydis zydis_build)
set_target_properties(zydis PROPERTIES
  IMPORTED_LOCATION "${ZYDIS_ARCHIVE}"
  INTERFACE_INCLUDE_DIRECTORIES "${zydis_src_SOURCE_DIR}"
  # ZYDIS_EXPORT/ZYCORE_EXPORT resolve to an import declaration unless the
  # consumer agrees this is a static build; harmless on ELF, wrong on Windows.
  INTERFACE_COMPILE_DEFINITIONS "ZYDIS_STATIC_BUILD;ZYCORE_STATIC_BUILD"
)
