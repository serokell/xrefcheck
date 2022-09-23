#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "We report broken links inside footnotes" {
  to_temp xrefcheck -r broken-link-in-footnote

  assert_diff expected.gold
}

@test  "We're not treating footnotes as 'shortcut reference links'"  {
# See: https://github.com/serokell/xrefcheck/issues/155
  run xrefcheck -r one-word-footnote

  assert_output --partial "All repository links are valid."
}
