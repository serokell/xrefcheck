#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Virtual files: all references should be valid" {
  run xrefcheck -c ./config-virtualFiles.yaml

  assert_output --partial "All repository links are valid."
}

@test "Virtual files: check failure" {
  to_temp xrefcheck

  assert_diff expected.gold
}
