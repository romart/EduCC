#!/usr/bin/env bash

make clean all


mkdir -p ./bootstrap

cp build/bin/main bootstrap/main

COMPILER=bootstrap/main make clean all
