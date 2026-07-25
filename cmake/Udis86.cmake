# Locates a system-installed udis86 (libudis86.{a,so} + udis86.h) and exposes
# it as the `udis86` target. If it isn't installed, fetches the upstream
# source tarball and compiles it as a static library instead -- there is no
# officially published prebuilt binary for udis86 to download.

find_path(UDIS86_INCLUDE_DIR NAMES udis86.h)
find_library(UDIS86_LIBRARY NAMES udis86 ud86)

if(UDIS86_INCLUDE_DIR AND UDIS86_LIBRARY)
  message(STATUS "Using system udis86: ${UDIS86_LIBRARY}")

  add_library(udis86 UNKNOWN IMPORTED)
  set_target_properties(udis86 PROPERTIES
    IMPORTED_LOCATION "${UDIS86_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${UDIS86_INCLUDE_DIR}"
  )
else()
  message(STATUS "System udis86 not found, fetching upstream source and building it")

  include(FetchContent)
  if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
  endif()
  # udis86's itab.c/itab.h generator (scripts/ud_opcode.py) is Python-2-only
  # (uses '/' for integer division and list.sort() on dict.keys()). Patch it
  # to run under Python 3, since python2 is no longer reasonably available.
  FetchContent_Declare(udis86_src
    URL "https://github.com/vmt/udis86/archive/refs/tags/v1.7.2.tar.gz"
    URL_HASH SHA256=43567f7e12168943c5b5ffb3d3f5b7a33cb36328f8938a993458f3ded0ba5779
    PATCH_COMMAND ${CMAKE_COMMAND}
      "-DSCRIPT_TARGET=<SOURCE_DIR>/scripts/ud_opcode.py"
      -P "${CMAKE_CURRENT_LIST_DIR}/patch_udis86_py3.cmake"
  )
  FetchContent_MakeAvailable(udis86_src)

  find_package(Python3 REQUIRED COMPONENTS Interpreter)

  # udis86's opcode tables (itab.c/itab.h) aren't checked in - they're
  # generated from docs/x86/optable.xml by this script, same as its own
  # autotools build does.
  set(UDIS86_LIBDIR "${udis86_src_SOURCE_DIR}/libudis86")
  add_custom_command(
    OUTPUT "${UDIS86_LIBDIR}/itab.c" "${UDIS86_LIBDIR}/itab.h"
    COMMAND "${Python3_EXECUTABLE}" "${udis86_src_SOURCE_DIR}/scripts/ud_itab.py"
            "${udis86_src_SOURCE_DIR}/docs/x86/optable.xml" "${UDIS86_LIBDIR}"
    DEPENDS "${udis86_src_SOURCE_DIR}/docs/x86/optable.xml"
            "${udis86_src_SOURCE_DIR}/scripts/ud_itab.py"
            "${udis86_src_SOURCE_DIR}/scripts/ud_opcode.py"
    COMMENT "Generating udis86 opcode tables"
  )

  # Deliberately NOT using add_library()/CMAKE_C_COMPILER here. When EduCC
  # is bootstrapping itself, CMAKE_C_COMPILER is EduCC's own (still very
  # incomplete) binary, which cannot parse real system headers well enough
  # to compile arbitrary third-party C code such as this vendored library.
  # The original Makefile-based bootstrap never recompiled udis86 with the
  # in-progress EduCC compiler either -- it only ever swapped compilers for
  # EduCC's own sources and always linked udis86 as an already-built
  # library. Mirror that: always build this fallback copy with a real host
  # compiler, independent of whatever CMAKE_C_COMPILER the outer project is
  # using.
  if(CMAKE_C_COMPILER_ID)
    set(UDIS86_HOST_CC "${CMAKE_C_COMPILER}" CACHE FILEPATH "Compiler used to build vendored udis86 fallback")
  else()
    find_program(UDIS86_HOST_CC NAMES gcc cc clang)
    if(NOT UDIS86_HOST_CC)
      message(FATAL_ERROR "Can't build vendored udis86: no real host C compiler (gcc/clang/cc) found, "
                           "and CMAKE_C_COMPILER ('${CMAKE_C_COMPILER}') isn't recognized as one either "
                           "(this happens when bootstrapping with EduCC's own not-yet-complete binary). "
                           "Install udis86 as a system package, or install gcc/clang.")
    endif()
  endif()
  find_program(UDIS86_AR NAMES ar REQUIRED)

  set(UDIS86_SOURCE_FILES itab.c decode.c syn.c syn-intel.c syn-att.c udis86.c)
  file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/udis86_objs")
  set(UDIS86_OBJECTS "")
  foreach(src ${UDIS86_SOURCE_FILES})
    set(obj "${CMAKE_CURRENT_BINARY_DIR}/udis86_objs/${src}.o")
    add_custom_command(
      OUTPUT "${obj}"
      COMMAND "${UDIS86_HOST_CC}" -O2 -I"${udis86_src_SOURCE_DIR}" -I"${UDIS86_LIBDIR}"
              # Vendored 2013-era C89-ish code (e.g. udis86.c calls memset()
              # without including <string.h>); GCC >= 14 makes that a hard
              # error by default in gnu99 mode. Relax it for this
              # third-party code only -- EduCC's own sources stay strict.
              -Wno-implicit-function-declaration
              -c "${UDIS86_LIBDIR}/${src}" -o "${obj}"
      DEPENDS "${UDIS86_LIBDIR}/${src}" "${UDIS86_LIBDIR}/itab.h"
      COMMENT "Building udis86/${src} (with ${UDIS86_HOST_CC})"
    )
    list(APPEND UDIS86_OBJECTS "${obj}")
  endforeach()

  set(UDIS86_ARCHIVE "${CMAKE_CURRENT_BINARY_DIR}/libudis86.a")
  add_custom_command(
    OUTPUT "${UDIS86_ARCHIVE}"
    COMMAND "${UDIS86_AR}" rcs "${UDIS86_ARCHIVE}" ${UDIS86_OBJECTS}
    DEPENDS ${UDIS86_OBJECTS}
    COMMENT "Archiving libudis86.a"
  )
  add_custom_target(udis86_build DEPENDS "${UDIS86_ARCHIVE}")

  add_library(udis86 STATIC IMPORTED)
  add_dependencies(udis86 udis86_build)
  set_target_properties(udis86 PROPERTIES
    IMPORTED_LOCATION "${UDIS86_ARCHIVE}"
    INTERFACE_INCLUDE_DIRECTORIES "${udis86_src_SOURCE_DIR}"
  )
endif()
