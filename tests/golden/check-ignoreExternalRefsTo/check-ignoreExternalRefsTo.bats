#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "Ignore localhost" {
  run xrefcheck \
    -c config-check-disabled.yaml \
    -r .

  assert_output --partial "All repository links are valid."
}

@test "Ignore localhost, check errors" {
  uname_out=$(uname)
  case "${uname_out}" in
      Linux*)     platform_suffix=linux;;
      Darwin*)    platform_suffix=darwin;;
      CYGWIN*)    platform_suffix=windows;;
      MINGW*)     platform_suffix=windows;;
      MSYS_NT*)   platform_suffix=windows;;
      *)          machine="UNKNOWN:${unameOut}"
  esac
  echo "platform_suffix=${platform_suffix}"

  golden_file=$(realpath expected_${platform_suffix}.gold)

  to_temp xrefcheck \
    -c config-check-enabled.yaml \
    -r .

  assert_diff
}

@test "Ignore localhost, no config specified" {
  run xrefcheck

  assert_output --partial "All repository links are valid."
}
