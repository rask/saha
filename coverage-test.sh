#!/bin/bash
#
# Run end-to-end Saha interpreter tests with coverage collection enabled.
#
# First, install kcov (above version 35) to an accessable location. Then run
# this either by relying on your $PATH to find kcov:
#
#     $ ./coverage-test.sh
#
# Or supply a path to your kcov binary manually:
#
#     $ ./coverage-test.sh /path/to/kcov
#
# The script runs the tests and dumps coverage data to `./target/coverage` in
# kcov format (HTML and cobertura).
#
# This script has been tested with a Debian-like Linux, but might work
# on other systems as well.

KCOV_ARG=$1

SAHA_BIN=./target/debug/saha_interpreter
TEST_BIN=./target/debug/saha_testbench
TEST_DIR=./tests/e2e/
COV_DIR=./target/coverage

INCL=$(pwd)/saha

if [[ "" = "$KCOV_ARG" ]]; then
    KCOV_ARG="kcov"
fi

KCOV_BIN=$(which "$KCOV_ARG")

if [[ ! -f "$KCOV_BIN" ]]; then
    echo "kcov binary missing, please install kcov v35+ first"
    exit 1
fi

$TEST_BIN --command "$KCOV_BIN --verify --exclude-path=/lib --exclude-pattern=testbench --include-path=$INCL $COV_DIR $SAHA_BIN" --test $TEST_DIR