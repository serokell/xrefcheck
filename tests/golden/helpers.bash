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
# Expected output must be given by setting the `golden_file` variable to an
# absolute path.
#
# Usage example:
# - filepath:
#
# @test "Ignore localhost, check errors" {
#   golden_file=$(realpath expected.gold)
#
#   to_temp xrefcheck \
#     -c config-check-enabled.yaml \
#     -r .
#
#   assert_diff
# }
assert_diff() {
  : "{golden_file?}"
  : "{output_file?}"

  if [ "${BATS_ACCEPT}" = "1" ]; then
    cp $output_file $golden_file
  fi

  diff $output_file $golden_file \
    --ignore-tab-expansion \
    --strip-trailing-cr
}
