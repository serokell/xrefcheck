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

@test "Git: bad file not tracked" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](/a.md)" >> "git.md"

  run xrefcheck

  assert_success

  assert_output --partial "All repository links are valid."

  # this is printed to stderr
  assert_output --partial "Those files are not added by Git, so we're not scanning them:"
  assert_output --partial "- git.md"
  assert_output --partial "Please run \"git add\" before running xrefcheck or enable --include-untracked CLI option to check these files."
}

@test "Git: bad file not tracked, --include-untracked enabled, check failure" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  to_temp xrefcheck --include-untracked

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file git.md
     bad reference (relative) at src:1:1-11:
       - text: "a"
       - link: ./a.md
       - anchor: -

     File does not exist:
       a.md

Invalid references dumped, 1 in total.
EOF
}

@test "Git: bad file tracked, check failure" {
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

     File does not exist:
       a.md

Invalid references dumped, 1 in total.
EOF
}


@test "Git: link to untracked file, check failure" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  touch ./a.md

  git add git.md

  to_temp xrefcheck

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file git.md
     bad reference (relative) at src:1:1-11:
       - text: "a"
       - link: ./a.md
       - anchor: -

     Link target is not tracked by Git:
       a.md
       Please run "git add" before running xrefcheck or enable --include-untracked CLI option.

Invalid references dumped, 1 in total.
EOF
}

@test "Git: link to untracked file, --include-untracked enabled" {
  cd $TEST_TEMP_DIR

  git init

  echo "[a](./a.md)" >> "git.md"

  touch ./a.md

  git add git.md

  run xrefcheck --include-untracked

  assert_success

  assert_output --partial "All repository links are valid."
}
