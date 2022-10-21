#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'


@test "Ignore file with broken xrefcheck annotation: full path" {
  run xrefcheck --ignored ./to-ignore/inner-directory/broken_annotation.md

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: glob wildcard" {
  run xrefcheck --ignored 'to-ignore/inner-directory/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: nested directories with glob wildcard" {
  run xrefcheck --ignored './**/*'

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: config file" {
  run xrefcheck --config ./config-ignored.yaml

  assert_output --partial "All repository links are valid."
}

@test "Ignore file with broken xrefcheck annotation: directory, check failure" {
  to_temp xrefcheck --ignored ./to-ignore/inner-directory/

  assert_diff - <<EOF
=== Scan errors found ===

  ➥  In file to-ignore/inner-directory/broken_annotation.md
  scan error at src:9:1-30:

  ⛀  Annotation "ignore file" must be at the top of markdown or right after comments at the top



Scan errors dumped, 1 in total.
EOF
}

@test "Ignore referenced file, check error" {
  to_temp xrefcheck --ignored referenced-file.md

  assert_diff - <<EOF
=== Scan errors found ===

  ➥  In file to-ignore/inner-directory/broken_annotation.md
     scan error at src:9:1-30:

     ⛀  Annotation "ignore file" must be at the top of markdown or right after comments at the top


Scan errors dumped, 1 in total.


=== Invalid references found ===

  ➥  In file check-ignored.md
     bad reference (absolute) at src:7:1-37:
       - text: "Good reference"
       - link: /referenced-file.md
       - anchor: -

     ⛀  File does not exist:
        ./referenced-file.md


Invalid references dumped, 1 in total.
EOF
}
