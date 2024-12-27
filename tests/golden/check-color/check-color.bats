#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

# The CI variable must be always explicitly set in these tests because they are checking for an
# intended behavior regardless of where they are actually being run (e.g. "No color flag (not
# in CI)") may be running in CI).

@test "Color flag (not in CI)" {
  golden_file=$(realpath expected-color.gold)
  output_file="$TEST_TEMP_DIR/temp_file.test"
  CI=false xrefcheck -u -v --no-progress --color > $output_file
  assert_diff
}

@test "No color flag (not in CI)" {
  golden_file=$(realpath expected-no-color.gold)
  output_file="$TEST_TEMP_DIR/temp_file.test"
  CI=false xrefcheck -u -v --no-progress --no-color > $output_file
  assert_diff
}

@test "No color default when pipe (not in CI)" {
  golden_file=$(realpath expected-no-color.gold)
  output_file="$TEST_TEMP_DIR/temp_file.test"
  CI=false xrefcheck -u -v --no-progress > $output_file
  assert_diff
}

@test "Color default when CI" {
  golden_file=$(realpath expected-color.gold)
  output_file="$TEST_TEMP_DIR/temp_file.test"
  CI=true xrefcheck -u -v --no-progress > $output_file
  assert_diff
}

@test "No color flag in CI" {
  golden_file=$(realpath expected-no-color.gold)
  output_file="$TEST_TEMP_DIR/temp_file.test"
  CI=true xrefcheck -u -v --no-progress --no-color > $output_file
  assert_diff
}
