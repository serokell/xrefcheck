#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking that symlinks are not processed as md files" {
  golden_file=$(realpath expected1.gold)

  cp config-ignore.yaml $TEST_TEMP_DIR
  cp -R dir $TEST_TEMP_DIR

  cd $TEST_TEMP_DIR
  touch dir/a

  # Required for Git Bash on Windows to have symlinks properly working, which also requires either
  # to be running as an admin or having developer mode turned on.
  export MSYS=winsymlinks:nativestrict

  ln -s ../d.md outside.md
  ln -s dir/b.md ok.md
  ln -s dir/c.md broken.md

  git init
  git add ./*

  to_temp xrefcheck -v -c config-ignore.yaml

  assert_diff $golden_file
}

@test "Symlinks validation" {
  golden_file=$(realpath expected2.gold)

  cp -R dir $TEST_TEMP_DIR

  cd $TEST_TEMP_DIR
  touch dir/a

  # Required for Git Bash on Windows to have symlinks properly working, which also requires either
  # to be running as an admin or having developer mode turned on.
  export MSYS=winsymlinks:nativestrict

  ln -s ../d.md outside.md
  ln -s dir/b.md ok.md
  ln -s dir/c.md broken.md

  git init
  git add ./*

  to_temp xrefcheck -v

  assert_diff
}
