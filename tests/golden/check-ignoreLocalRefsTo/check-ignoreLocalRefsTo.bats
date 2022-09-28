#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "IgnoreLocalRefsTo: all references should be valid" {
  run xrefcheck -c ./config-ignoreLocalRefsTo.yaml

  assert_output --partial "All repository links are valid."
}

@test "IgnoreLocalRefsTo: check failure" {
  to_temp xrefcheck

  assert_diff expected.gold
}
