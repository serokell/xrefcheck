#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Dump config to stdout" {
  to_temp xrefcheck dump-config --stdout -t GitHub

  assert_diff ../../configs/github-config.yaml
}

@test "Dump config to existent default file error" {
  run xrefcheck dump-config -t GitHub

  assert_failure

  assert_output "Output file exists. Use --force to overwrite."
}

@test "Dump config to existent file error" {
  run xrefcheck dump-config -o .config.yaml -t GitHub

  assert_failure

  assert_output "Output file exists. Use --force to overwrite."
}

@test "Dump config to non existent default file" {
  cd $TEST_TEMP_DIR

  run xrefcheck dump-config -t GitHub

  assert_success

  assert_exists .xrefcheck.yaml
}

@test "Dump config to non existent file" {
  cd $TEST_TEMP_DIR

  run xrefcheck dump-config -o .config.yaml -t GitHub

  assert_success

  assert_exists .config.yaml
}

@test "Dump config to existent default file with force" {
  cp .xrefcheck.yaml $TEST_TEMP_DIR
  cd $TEST_TEMP_DIR

  run xrefcheck dump-config -t GitHub --force

  assert_success

  assert_exists .xrefcheck.yaml
}

@test "Dump config to existent file with force" {
  cp .config.yaml $TEST_TEMP_DIR
  cd $TEST_TEMP_DIR

  run xrefcheck dump-config -o .config.yaml -t GitHub --force

  assert_success

  assert_exists .config.yaml
}
