#!/usr/bin/env bash

make clean all

rm -rf ./bootstrap
mkdir -p ./bootstrap

cp build/bin/main bootstrap/main_gcc
rm -rf build
COMPILER=bootstrap/main_gcc make clean all



cp build/bin/main bootstrap/main2
rm -rf build
COMPILER=bootstrap/main2 make clean all



cp build/bin/main bootstrap/main3
rm -rf build
COMPILER=bootstrap/main3 make clean all


cp build/bin/main bootstrap/main4
rm -rf build
COMPILER=bootstrap/main4 make clean all

cp build/bin/main bootstrap/main5

sha1sum ./bootstrap/*
