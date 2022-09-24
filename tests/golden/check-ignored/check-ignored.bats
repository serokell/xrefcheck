#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Ignore file with broken xrefcheck annotation: full path" {
  run xrefcheck --ignored ./to-ignore/inner-directory/broken_annotation.md

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: glob wildcard" {
  run xrefcheck --ignored 'to-ignore/inner-directory/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: nested directories with glob wildcard" {
  run xrefcheck --ignored './**/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: config file" {
  run xrefcheck --config ./config-ignored.yaml

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: directory, check failure" {
  to_temp xrefcheck --ignored ./to-ignore/inner-directory/

  assert_diff expected.gold
}
