#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Relative anchor, check error report" {
  to_temp xrefcheck

  assert_diff - <<EOF
=== Invalid references found ===

       ➥  In file check-relative-anchor.md
          bad reference (relative) at src:7:1-40:
            - text: "no-anchor"
            - link: no-anchor.md
            - anchor: invalid-anchor

          ⛀  Anchor 'invalid-anchor' is not present


Invalid references dumped, 1 in total.
EOF
}
