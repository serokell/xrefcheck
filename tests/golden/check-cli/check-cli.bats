#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "No redundant slashes" {
  run xrefcheck \
    --ignore to-ignore/* \
    --root .

  assert_output --partial "All repository links are valid."
}

@test "Redundant slashes in root and ignore" {
  run xrefcheck \
    --ignore ./././././././//to-ignore/* \
    --root ./

  assert_output --partial "All repository links are valid."
}

@test "Redundant slashes in root" {
  run xrefcheck \
    -c config-no-scan-ignored.yaml \
    --root ./

  assert_output --partial "All repository links are valid."
}

@test "Redundant slashes in ignore" {
  run xrefcheck \
    --ignore ./././././././//to-ignore/* \
    --root .

  assert_output --partial "All repository links are valid."
}

@test "Basic root, check errors report" {
  to_temp xrefcheck --root .

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file to-ignore/broken-link.md
     bad reference (absolute) at src:7:1-25:
       - text: "my link"
       - link: /one/two/three
       - anchor: -

     File does not exist:
       one/two/three

Invalid references dumped, 1 in total.
EOF
}

@test "Root with redundant slashes, check errors report" {
  to_temp xrefcheck --root ././///././././//./

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file to-ignore/broken-link.md
     bad reference (absolute) at src:7:1-25:
       - text: "my link"
       - link: /one/two/three
       - anchor: -

     File does not exist:
       one/two/three

Invalid references dumped, 1 in total.
EOF
}

@test "No root, check errors report" {
  to_temp xrefcheck

  assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file to-ignore/broken-link.md
     bad reference (absolute) at src:7:1-25:
       - text: "my link"
       - link: /one/two/three
       - anchor: -

     File does not exist:
       one/two/three

Invalid references dumped, 1 in total.
EOF
}

@test "Single file as root" {
  run xrefcheck \
    --root single-file.md

  assert_failure
  assert_output --partial "Repository's root does not seem to be a directory: single-file.md"
}
