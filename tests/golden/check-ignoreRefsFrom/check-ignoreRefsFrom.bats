#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "ignoreRefsFrom: full path" {
  run xrefcheck -c config-full-path.yaml

  assert_output --partial "All repository links are valid."
}

@test "ignoreRefsFrom: glob wildcard" {
  run xrefcheck -c config-wildcard.yaml

  assert_output --partial "All repository links are valid."
}

@test "ignoreRefsFrom: nested directories with glob wildcard" {
  run xrefcheck -c config-nested-directories.yaml

  assert_output --partial "All repository links are valid."
}

@test "ignoreRefsFrom: directory, check failure" {
  golden_file=$(realpath expected.gold)
  to_temp xrefcheck -c config-directory.yaml
  assert_diff
}
