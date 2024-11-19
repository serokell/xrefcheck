#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking local references, root = \".\"" {
  golden_file=$(realpath expected1.gold)
  to_temp xrefcheck
  assert_diff
}

@test "Checking local references, root = \"dir1\"" {
  golden_file=$(realpath expected2.gold)
  to_temp xrefcheck -r dir1
  assert_diff
}

@test "Checking behavior when there are virtual files, root = \"dir1\"" {
  golden_file=$(realpath expected3.gold)
  to_temp xrefcheck -r dir1 -c config-with-virtual-files.yaml
  assert_diff
}
