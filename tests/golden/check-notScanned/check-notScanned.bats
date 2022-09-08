#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'


@test "Not scanned: full path" {
  run xrefcheck -c config-full-path.yaml

  assert_output --partial "All repository links are valid."
}

@test "Not scanned: glob wildcard" {
  run xrefcheck -c config-wildcard.yaml

  assert_output --partial "All repository links are valid."
}

@test "Not scanned: nested directories with glob wildcard" {
  run xrefcheck -c config-nested-directories.yaml

  assert_output --partial "All repository links are valid."
}

@test "Not scanned: directory, check failure" {
  xrefcheck -c config-directory.yaml \
  | prepare > /tmp/check-notScanned.test || true

  diff /tmp/check-notScanned.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-notScanned.test
}
