#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "Ignore localhost" {
  run xrefcheck \
    -c config-check-disabled.yaml \
    -r .

  assert_output --partial "All repository links are valid."
}

@test "Ignore localhost, check errors" {
  to_temp xrefcheck \
    -c config-check-enabled.yaml \
    -r .

  assert_diff expected_linux.gold || assert_diff expected_windows.gold
}

@test "Ignore localhost, no config specified" {
  run xrefcheck

  assert_output --partial "All repository links are valid."
}
