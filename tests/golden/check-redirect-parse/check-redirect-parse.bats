#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "No redirect rules" {
  golden_file=$(realpath expected1.gold)
  to_temp xrefcheck -c no-rules.yaml
  assert_diff
}

@test "Only outcome" {
  golden_file=$(realpath expected2.gold)
  to_temp xrefcheck -c only-outcome.yaml
  assert_diff
}

@test "Only outcome and to" {
  golden_file=$(realpath expected3.gold)
  to_temp xrefcheck -c only-outcome-to.yaml
  assert_diff
}

@test "Only outcome and on" {
  golden_file=$(realpath expected4.gold)
  to_temp xrefcheck -c only-outcome-to.yaml
  assert_diff
}

@test "Full rule" {
  golden_file=$(realpath expected5.gold)
  to_temp xrefcheck -c full-rule.yaml
  assert_diff
}

@test "Rules not an array error" {
  run xrefcheck -c bad-rules.yaml

  assert_output --partial "expected Array, but encountered String"
}

@test "Rule not an object error" {
  run xrefcheck -c bad-rule.yaml

  assert_output --partial "expected Object, but encountered String"
}

@test "Bad code error" {
  run xrefcheck -c bad-code.yaml

  assert_output --partial "expected a redirect (3XX) HTTP code or (permanent|temporary)"
}

@test "Bad on" {
  run xrefcheck -c bad-on.yaml

  assert_output --partial "expected a redirect (3XX) HTTP code or (permanent|temporary)"
}

@test "Bad to" {
  run xrefcheck -c bad-to.yaml

  assert_output --partial "expected String, but encountered Number"
}

@test "Bad outcome" {
  run xrefcheck -c bad-outcome.yaml

  assert_output --partial "expected (valid|invalid|follow)"
}

@test "No outcome error" {
  run xrefcheck -c no-outcome.yaml

  assert_output --partial "key \"outcome\" not found"
}
