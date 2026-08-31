#!/usr/bin/env bash
set -euo pipefail

rm -rf ./build
cmake -B build -S .
cmake --build build

rm -rf ./bootstrap
mkdir -p ./bootstrap

cp build/bin/main bootstrap/main_gcc
rm -rf build
cmake -B build -S . -DCMAKE_C_COMPILER="$(pwd)/bootstrap/main_gcc"
cmake --build build



cp build/bin/main bootstrap/main2
rm -rf build
cmake -B build -S . -DCMAKE_C_COMPILER="$(pwd)/bootstrap/main2"
cmake --build build



cp build/bin/main bootstrap/main3
rm -rf build
cmake -B build -S . -DCMAKE_C_COMPILER="$(pwd)/bootstrap/main3"
cmake --build build


cp build/bin/main bootstrap/main4
rm -rf build
cmake -B build -S . -DCMAKE_C_COMPILER="$(pwd)/bootstrap/main4"
cmake --build build

cp build/bin/main bootstrap/main5

# Leave build/ configured with the host compiler again. CMake caches
# CMAKE_C_COMPILER, so a plain `cmake -B build -S .` afterwards keeps whichever
# EduCC stage was last used - and every later build, test run and ctest is then
# silently self-compiled. That is invisible until a bug EduCC has in its own
# code changes what its rebuilt self does with a test, which is not where anyone
# looks first.
rm -rf ./build
cmake -B build -S .
cmake --build build

sha1sum ./bootstrap/*
