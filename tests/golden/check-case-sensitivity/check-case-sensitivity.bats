#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "GitHub anchors: check, ambiguous and similar detection is case-insensitive" {
  to_temp xrefcheck -c config-github.yaml

  assert_diff expected1.gold
}

@test "GitLab anchors: check and ambiguous detection is case-sensitive, but similar detection is not"  {
  to_temp xrefcheck -c config-gitlab.yaml

  assert_diff expected2.gold
}
