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

sha1sum ./bootstrap/*
