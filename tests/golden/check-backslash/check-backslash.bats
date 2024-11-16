#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Checking files with backslash" {
  golden_file=$(realpath expected.gold)

  cp a.md $TEST_TEMP_DIR
  touch "$TEST_TEMP_DIR/a\a.md" || \
    return 0 # Cannot be tested on Windows

  cat <<EOF > "$TEST_TEMP_DIR/a\a.md"
# Header
[Reference to a](a.md)
[Reference to myself](a\a.md)
EOF

  cd $TEST_TEMP_DIR
  git init
  git add a.md
  git add "a\a.md"

  to_temp xrefcheck -v

  assert_diff
}
