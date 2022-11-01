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

  run xrefcheck

  assert_output --partial "fatal: not a git repository"
}

@test "Git: file not tracked" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](/a.md)" >> "git.md"

  run xrefcheck

  assert_output --partial "All repository links are valid."
}

@test "Git: file tracked, check failure" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  git add git.md

  to_temp xrefcheck

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file git.md
     bad reference (relative) at src:1:1-11:
       - text: "a"
       - link: ./a.md
       - anchor: -

     ⛀  File does not exist:
        a.md

Invalid references dumped, 1 in total.
EOF
}
