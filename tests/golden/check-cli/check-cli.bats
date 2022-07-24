#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'

@test "No redundant slashes" {
  run xrefcheck \
    --ignored to-ignore \
    --root .

  assert_output --partial "All repository links are valid."
}

@test "Redundant slashes in root and ignored" {
  run xrefcheck \
    --ignored ./././././././//to-ignore \
    --root ./

  assert_output --partial "All repository links are valid."
}

@test "Redundant slashes in root" {
  run xrefcheck \
    -c config-no-scan-ignored.yaml \
    --root ./

  assert_output --partial "All repository links are valid."
}

@test "Reduchant slashes in ignored" {
  run xrefcheck \
    --ignored ./././././././//to-ignore \
    --root .

  assert_output --partial "All repository links are valid."
}

@test "Basic root, check errors report" {
  xrefcheck \
    --root . \
    | prepare > /tmp/check-cli.test || true

  diff /tmp/check-cli.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-cli.test
}

@test "Root with redundant slashes, check errors report" {
  xrefcheck \
    --root ././///././././//./ \
    | prepare > /tmp/check-cli.test || true

  diff /tmp/check-cli.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-cli.test
}

@test "No root, check errors report" {
  xrefcheck \
    | prepare > /tmp/check-cli.test || true

  diff /tmp/check-cli.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-cli.test
}

@test "Single file as root" {
  run xrefcheck \
    --root single-file.md

  assert_failure
  assert_output --partial "Repository's root does not seem to be a directory: single-file.md"
}
