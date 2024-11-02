#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "Git: not a repo" {
  cd $TEST_TEMP_DIR

  export LANG=en_US
  run xrefcheck -u

  assert_output --partial "fatal: not a git repository"
}

@test "Git: bad file not tracked" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](/a.md)" >> "git.md"

  run xrefcheck -u

  assert_success

  assert_output --partial "All repository links are valid."

  # this is printed to stderr
  assert_output --partial "Those files are not added by Git, so we're not scanning them:"
  assert_output --partial "- git.md"
  assert_output --partial "Please run \"git add\" before running xrefcheck or enable --include-untracked CLI option to check these files."
}

@test "Git: bad file not tracked, --include-untracked enabled, check failure" {
  golden_file=$(realpath expected1.gold)

  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  to_temp xrefcheck -u --include-untracked

  assert_diff
}

@test "Git: bad file tracked, check failure" {
  golden_file=$(realpath expected2.gold)

  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  git add git.md

  to_temp xrefcheck -u

  assert_diff
}


@test "Git: link to untracked file, check failure" {
  golden_file=$(realpath expected3.gold)

  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  touch ./a.md

  git add git.md

  to_temp xrefcheck -u

  assert_diff
}

@test "Git: link to untracked file, --include-untracked enabled" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  touch ./a.md

  git add git.md

  run xrefcheck -u --include-untracked

  assert_success

  assert_output --partial "All repository links are valid."
}
