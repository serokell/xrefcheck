#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load '../helpers/bats-support/load'
load '../helpers/bats-assert/load'
load '../helpers/bats-file/load'
load '../helpers'

@test "We report ambiguous anchor references" {
  to_temp xrefcheck -r ambiguous-anchors
assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file a.md
     bad reference (file-local) at src:16:1-43:
       - text: "ambiguous anchor in this file"
       - anchor: some-text

     Ambiguous reference to anchor 'some-text'
       In file a.md
       It could refer to either:
         - some-text (header I) at src:6:1-11
         - some-text (header I) at src:8:1-15
         - some-text (header II) at src:12:1-12
       Use of ambiguous anchors is discouraged because the target
       can change silently while the document containing it evolves.

  ➥  In file b.md
     bad reference (relative) at src:7:1-48:
       - text: "ambiguous anchor in other file"
       - link: a.md
       - anchor: some-text

     Ambiguous reference to anchor 'some-text'
       In file a.md
       It could refer to either:
         - some-text (header I) at src:6:1-11
         - some-text (header I) at src:8:1-15
         - some-text (header II) at src:12:1-12
       Use of ambiguous anchors is discouraged because the target
       can change silently while the document containing it evolves.

Invalid references dumped, 2 in total.
EOF
}

@test "We report references to non-existing anchors, giving hints about similar ones" {
  to_temp xrefcheck -r non-existing-anchors
assert_diff - <<EOF
=== Invalid references found ===

  ➥  In file a.md
     bad reference (file-local) at src:12:1-13:
       - text: "broken"
       - anchor: h3

     Anchor 'h3' is not present, did you mean:
         - h1 (header I) at src:6:1-4
         - h2 (header II) at src:8:1-5

  ➥  In file a.md
     bad reference (file-local) at src:14:1-18:
       - text: "broken"
       - anchor: heading

     Anchor 'heading' is not present, did you mean:
         - the-heading (header I) at src:10:1-13

  ➥  In file a.md
     bad reference (file-local) at src:16:1-31:
       - text: "broken"
       - anchor: really-unique-anchor

     Anchor 'really-unique-anchor' is not present

Invalid references dumped, 3 in total.
EOF
}
