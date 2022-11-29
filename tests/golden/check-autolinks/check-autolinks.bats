#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "We're finding and checking autolinks" {
  to_temp xrefcheck -v
assert_diff - <<EOF
=== Repository data ===

  file-with-autolinks.md:
    - references:
        - reference (external) :
            - text: "https://www.google.com/doodles"
            - link: https://www.google.com/doodles
            - anchor: -
        - reference (external) at src:8:0-18:
            - text: "www.commonmark.org"
            - link: http://www.commonmark.org
            - anchor: -
    - anchors:
        none

All repository links are valid.
EOF
}
