#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "We report ambiguous anchor references" {
  golden_file=$(realpath expected1.gold)
  to_temp xrefcheck -r ambiguous-anchors
  assert_diff
}

@test "We report references to non-existing anchors, giving hints about similar ones" {
  golden_file=$(realpath expected2.gold)
  to_temp xrefcheck -r non-existing-anchors
  assert_diff
}
