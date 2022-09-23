#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers'


@test "Dump all errors along with broken links" {
  xrefcheck | prepare > /tmp/check-scan-errors.test || true

  diff /tmp/check-scan-errors.test expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-scan-errors.test
}
