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
        - reference (external) at src:8:0-18:
            - text: "www.commonmark.org"
            - link: http://www.commonmark.org
    - anchors:
        none

=== Invalid references found ===

  ➥  In file file-with-autolinks.md
     bad reference (external) at src:8:0-18:
       - text: "www.commonmark.org"
       - link: http://www.commonmark.org

     Permanent redirect found:
       -| http://www.commonmark.org
       -> https://commonmark.org
          ^-- stopped before this one

Invalid references dumped, 1 in total.
EOF
}
