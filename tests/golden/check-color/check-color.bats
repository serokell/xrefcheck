#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

# The CI variable must be always explicitly set in these tests because they are checking for an
# intended behavior regardless of where they are actually being run (e.g. "No color flag (not
# in CI)") may be running in CI).

@test "Color flag (not in CI)" {
  CI=false xrefcheck -v --no-progress --color | diff - expected-color.gold
}

@test "No color flag (not in CI)" {
  CI=false xrefcheck -v --no-progress --no-color | diff - expected-no-color.gold
}

@test "No color default when pipe (not in CI)" {
  CI=false xrefcheck -v --no-progress | diff - expected-no-color.gold
}

@test "Color default when CI" {
  CI=true xrefcheck -v --no-progress | diff - expected-color.gold
}

@test "No color flag in CI" {
  CI=true xrefcheck -v --no-progress --no-color | diff - expected-no-color.gold
}
