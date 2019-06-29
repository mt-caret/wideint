#!/usr/bin/env bash
set -euxo pipefail

PACKAGE_PATH="./lib/github.com/mt-caret/wideint"

futhark pyopencl --safe --library "$PACKAGE_PATH/u256_test_harness.fut"
python "$PACKAGE_PATH/test.py"
