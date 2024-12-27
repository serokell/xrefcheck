#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "Ignore localhost" {
  run xrefcheck -u \
    -c config-check-disabled.yaml \
    -r .

  assert_output --partial "All repository links are valid."
}

@test "Ignore localhost, check errors" {
  golden_file=$(realpath expected.gold)

  to_temp xrefcheck -u \
    -c config-check-enabled.yaml \
    -r .

  assert_diff
}

@test "Ignore localhost, no config specified" {
  run xrefcheck -u

  assert_output --partial "All repository links are valid."
}
