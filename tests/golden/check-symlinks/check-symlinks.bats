#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking that symlinks are not processed as md files" {
  cp config-ignore.yaml $TEST_TEMP_DIR
  cp expected1.gold $TEST_TEMP_DIR
  cp -R dir $TEST_TEMP_DIR

  cd $TEST_TEMP_DIR
  touch dir/a
  ln -s ../d.md outside.md
  ln -s dir/b.md ok.md
  ln -s dir/c.md broken.md

  git init
  git add ./*

  to_temp xrefcheck -v -c config-ignore.yaml

  assert_diff expected1.gold
}

@test "Symlinks validation" {
  cp expected2.gold $TEST_TEMP_DIR
  cp -R dir $TEST_TEMP_DIR

  cd $TEST_TEMP_DIR
  touch dir/a
  ln -s ../d.md outside.md
  ln -s dir/b.md ok.md
  ln -s dir/c.md broken.md

  git init
  git add ./*

  to_temp xrefcheck -v

  assert_diff expected2.gold
}
