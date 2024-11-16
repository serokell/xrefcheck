#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Ignore file with broken xrefcheck annotation: full path" {
  run xrefcheck --ignore ./to-ignore/inner-directory/broken_annotation.md

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: glob wildcard" {
  run xrefcheck --ignore 'to-ignore/inner-directory/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: nested directories with glob wildcard" {
  run xrefcheck --ignore './**/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: config file" {
  run xrefcheck --config ./config-ignore.yaml

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: directory, check failure" {
  golden_file=$(realpath expected1.gold)
  to_temp xrefcheck --ignore ./to-ignore/inner-directory/
  assert_diff
}

@test "Ignore referenced file, check error" {
  golden_file=$(realpath expected2.gold)

  to_temp xrefcheck --ignore referenced-file.md

  assert_diff
}

@test "Config: Absolute fiepath in \"ignore\" error" {
  run xrefcheck --config ./config-ignore-bad-path-absolute.yaml

  assert_failure
  assert_output --partial "Expected a relative glob pattern, but got /to-ignore/inner-directory/broken_annotation.md"
}

@test "Config: Malformed glob in \"ignore\" error" {
  run xrefcheck --config ./config-ignore-malformed-glob.yaml

  assert_failure
  assert_output --partial "Glob pattern compilation failed."
}

@test "CLI: Absolute filepath in \"ignore\" yields to error" {
  run xrefcheck\
    --ignore "/to-ignore/*"
  assert_failure
  assert_output --partial "option --ignore: Expected a relative glob pattern, but got /to-ignore/*"
}

@test "CLI: Malformed glob in arg \"ignore\" yields to error" {
  run xrefcheck\
    --ignore "<to-ignore>"
  assert_failure
  assert_output --partial "option --ignore: Glob pattern compilation failed."
  assert_output --partial "compile :: bad <>, expected number followed by - in to-ignore"
}
