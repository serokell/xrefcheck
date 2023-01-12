#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking that symlinks are not processed" {
  to_temp xrefcheck -v

  assert_diff expected.gold
}
