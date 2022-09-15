#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "Ignore localhost" {
  run xrefcheck \
    -c config-check-disabled.yaml \
    -r .

  assert_output --partial "All repository links are valid."
}

@test "Ignore localhost, check errors" {
  xrefcheck \
    -c config-check-enabled.yaml \
    -r . \
    | prepare > /tmp/check-localhost.test || true

  diff /tmp/check-localhost.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-localhost.test
}

@test "Ignore localhost, no config specified" {
  run xrefcheck

  assert_output --partial "All repository links are valid."
}
