#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "GitHub paths: case-sensitive" {
  golden_file=$(realpath expected.gold)
  to_temp xrefcheck -u -v -c config-github.yaml
  assert_diff
}

@test "GitLab paths: case-sensitive"  {
  golden_file=$(realpath expected.gold)
  to_temp xrefcheck -u -v -c config-gitlab.yaml
  assert_diff
}
