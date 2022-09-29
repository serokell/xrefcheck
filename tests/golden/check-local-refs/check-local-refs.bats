#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking local references, root = \".\"" {
  to_temp xrefcheck

  assert_diff expected1.gold
}

@test "Checking local references, root = \"dir1\"" {
  to_temp xrefcheck -r dir1

  assert_diff expected2.gold
}
