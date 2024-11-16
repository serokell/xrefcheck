#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "GitHub anchors: check, ambiguous and similar detection is case-insensitive" {
  golden_file=$(realpath expected1.gold)
  to_temp xrefcheck -c config-github.yaml
  assert_diff
}

@test "GitLab anchors: check and ambiguous detection is case-sensitive, but similar detection is not"  {
  golden_file=$(realpath expected2.gold)
  to_temp xrefcheck -c config-gitlab.yaml
  assert_diff
}
