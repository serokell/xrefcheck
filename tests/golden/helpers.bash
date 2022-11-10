# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

setup () {
  # change working directory to the location of the running `bats` suite.
  cd "$( dirname "$BATS_TEST_FILENAME")"
  TEST_TEMP_DIR="$(temp_make)"
}

teardown() {
  temp_del "$TEST_TEMP_DIR"
}

# this function is used for:
# - delete all color characters
# - replace socket port with N
# - replace multiple connection retry errors with single one
#   (because at some machine there are one error and at others - two)
prepare () {
  sed -r "s/[[:cntrl:]]\[[0-9]{1,3}m//g" \
    | sed 's/socket: [0-9]*/socket: N/g' \
    | sed 's/Network.Socket.connect: <socket: N>: does not exist (Connection refused),//g'
}

# Create temporary file with program output, used with `assert_diff`.
to_temp() {
  output_file="$TEST_TEMP_DIR/temp_file.test"
  $@ | prepare > $output_file
}

# Uses `diff` to compare output file created by `to_temp` against expected output.
# Expected output could be given either:
# - in the form of a filepath, e.g. `assert_diff expected.gold`
# - via stdin when `-` is used, e.g. `assert_diff -`
# Usage examples:
# - filepath:
#
# @test "Ignore localhost, check errors" {
#   to_temp xrefcheck \
#     -c config-check-enabled.yaml \
#     -r .
#
#   assert_diff expected.gold
# }
#
# - stdin:
# @test "Relative anchor, check error report" {
#   to_temp xrefcheck
#
#   assert_diff - <<EOF
# === Invalid references found ===
#
#        ➥  In file check-relative-anchor.md
#           bad reference (relative) at src:7:1-40:
#             - text: "no-anchor"
#             - link: no-anchor.md
#             - anchor: invalid-anchor
#
#           ⛀  Anchor 'invalid-anchor' is not present
#
#
# Invalid references dumped, 1 in total.
# EOF
# }
assert_diff() {
  : "{output_file?}"

  diff $output_file $1 \
    --ignore-tab-expansion
}
