#!/bin/bash

SAHA_BIN=./target/debug/saha_interpreter
TEST_BIN=./target/debug/saha_testbench
TEST_DIR=./tests/e2e/
COV_DIR=./target/coverage

KCOV=$(which kcov)

if [[ ! -f "$KCOV" ]]; then
    echo "kcov binary missing, please install kcov v35+ first"
    exit 1
fi

$TEST_BIN --command "kcov --include-pattern=saha/ $COV_DIR $SAHA_BIN" --test $TEST_DIR